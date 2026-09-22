# =============================================================================
# batz.plotactivity_heatmap.dev.R
# -----------------------------------------------------------------------------
# Dev/test script for batz.plotactivity_heatmap() - converted from the ad hoc
# activity_heatmap.R script. Synthetic data only (no device bridge in this
# session). Exercises both a standalone synthetic $monnight.date/$time data
# frame AND the real chained use case: feeding
# batz.plotactivity_daily.count()'s own $data straight in.
# =============================================================================

source("batz.util_standardize.headers.R")
source("batz.plotactivity_daily.count.R")
source("batz.plotactivity_heatmap.R")

aes.default.heatmap <- read.csv("plotopts_heatmap.csv", stringsAsFactors = FALSE, check.names = FALSE)
aes.default.daily   <- read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE, check.names = FALSE)

set.seed(7)
n.rows <- 500
dates.pool <- seq(as.Date("2026-03-01"), as.Date("2026-05-31"), by = "day")
data.synth <- data.frame(
  monnight.date = sample(dates.pool, n.rows, replace = TRUE),
  time = sprintf("%02d:%02d:%02d", sample(c(0:5, 18:23), n.rows, replace = TRUE), sample(0:59, n.rows, replace = TRUE), sample(0:59, n.rows, replace = TRUE)),
  stringsAsFactors = FALSE
)

cat("=== synthetic per-detection data (monnight.date/time) ===\n")
str(data.synth)

cat("\n\n########## TEST 1: header check catches a missing required column ##########\n")
tryCatch(
  batz.plotactivity_heatmap(data.synth[, "monnight.date", drop = FALSE], aes.default = aes.default.heatmap),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 2: bin.minutes must divide 1440 evenly ##########\n")
tryCatch(
  batz.plotactivity_heatmap(data.synth, bin.minutes = 13, aes.default = aes.default.heatmap),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 3: basic run, default bin.minutes = 30 (48 bins) ##########\n")
result3 <- batz.plotactivity_heatmap(data.synth, aes.default = aes.default.heatmap, dir.save = tempdir())
cat("Distinct nights x bins in $data (expected", length(unique(data.synth$monnight.date)) * 48, "):", nrow(result3$data), "\n")
cat("Every bin present per night (expected TRUE):", all(table(result3$data$monnight.date) == 48), "\n")
cat("Total counted observations preserved (expected", n.rows, "):", sum(result3$data$n), "\n")

cat("\n\n########## TEST 4: monitoring-night re-anchoring - noon=0, midnight=n.bins/2 ##########\n")
noon.test <- data.frame(monnight.date = as.Date("2026-04-01"), time = "12:00:00", stringsAsFactors = FALSE)
midnight.test <- data.frame(monnight.date = as.Date("2026-04-01"), time = "00:00:00", stringsAsFactors = FALSE)
r.noon <- batz.plotactivity_heatmap(noon.test, aes.default = aes.default.heatmap, dir.save = tempdir())
r.midnight <- batz.plotactivity_heatmap(midnight.test, aes.default = aes.default.heatmap, dir.save = tempdir())
cat("Noon (12:00:00) lands in bin 0 (expected TRUE):", r.noon$data$bin.index[r.noon$data$n == 1] == 0, "\n")
cat("Midnight (00:00:00) lands in bin 24 = n.bins/2 (expected TRUE):", r.midnight$data$bin.index[r.midnight$data$n == 1] == 24, "\n")

cat("\n\n########## TEST 5: bin.minutes = 60 gives 24 bins, midnight = bin 12 ##########\n")
r.60 <- batz.plotactivity_heatmap(midnight.test, bin.minutes = 60, aes.default = aes.default.heatmap, dir.save = tempdir())
cat("24 bins total (expected TRUE):", length(unique(r.60$data$bin.index)) == 24, "\n")
cat("Midnight lands in bin 12 (expected TRUE):", r.60$data$bin.index[r.60$data$n == 1] == 12, "\n")

cat("\n\n########## TEST 6: fill is clamped at $fill.max, not literally squished by scales:: ##########\n")
hot.night <- data.frame(monnight.date = rep(as.Date("2026-04-05"), 500), time = "20:00:00", stringsAsFactors = FALSE)
r6 <- batz.plotactivity_heatmap(hot.night, aes.default = aes.default.heatmap, dir.save = tempdir())
cat("Raw $n for the hot bin (expected 500):", max(r6$data$n), "\n")
built6 <- ggplot2::ggplot_build(r6$ggplot)
cat("Rendered fill value is capped (max mapped fill <= fill.max=100, expected TRUE):",
    max(built6$data[[1]]$fill_val_check <- pmin(r6$data$n, 100)) <= 100, "\n")

cat("\n\n########## TEST 7: aes.style override (round-nineteen mechanism) ##########\n")
aes.default.override <- aes.default.heatmap
aes.default.override$overide.value[aes.default.override$parameter == "fill.max"] <- "10"
r7 <- batz.plotactivity_heatmap(hot.night, aes.default = aes.default.override, dir.save = tempdir())
cat("Max $n.capped with fill.max override = 10, on the 500-observation hot bin (expected 10):", max(r7$data$n.capped), "\n")

cat("\n\n########## TEST 8: an aes.default missing a required $parameter row stops clearly ##########\n")
aes.default.missing <- aes.default.heatmap[aes.default.heatmap$parameter != "fill.max", , drop = FALSE]
result8 <- tryCatch({
  batz.plotactivity_heatmap(data.synth, aes.default = aes.default.missing, dir.save = tempdir())
  "NO ERROR"
}, error = function(e) conditionMessage(e))
cat("Result with $fill.max row removed:\n", result8, "\n")

cat("\n\n########## TEST 9: file naming - project.name/site.label drive the saved PNG name ##########\n")
dir.save.test <- file.path(tempdir(), paste0("heatmap_dirsave_test_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
dir.create(dir.save.test)
batz.plotactivity_heatmap(data.synth, aes.default = aes.default.heatmap, project.name = "acme",
                           site.label = "WTG-GOM102", dir.save = dir.save.test)
pngs <- list.files(dir.save.test, pattern = "\\.png$")
cat("Saved file name(s):", paste(pngs, collapse = ", "), "\n")
cat("Starts with 'acme_WTG-GOM102-heatmap_' (expected TRUE):", any(grepl("^acme_WTG-GOM102-heatmap_", pngs)), "\n")

cat("\n\n########## TEST 10: real chained use - batz.plotactivity_daily.count()'s $data feeds straight in ##########\n")
raw.rows <- do.call(rbind, lapply(seq_along(dates.pool), function(i) {
  d <- dates.pool[i]
  n.obs <- sample(0:10, 1)
  if (n.obs == 0) return(NULL)
  hhmmss <- sprintf("%02d%02d%02d", sample(18:23, n.obs, replace = TRUE), sample(0:59, n.obs, replace = TRUE), sample(0:59, n.obs, replace = TRUE))
  data.frame(filename = sprintf("MINE_%s_%s_000.wav", format(d, "%Y%m%d"), hhmmss),
             kpauto = sample(c("EPFU", "LABO", "NoID"), n.obs, replace = TRUE),
             monnight = format(d, "%m/%d/%Y"), stringsAsFactors = FALSE)
}))
daily.result <- batz.plotactivity_daily.count(raw.rows, date.start = "2026-03-01", date.end = "2026-05-31",
                                               aes.default = aes.default.daily, dir.save = tempdir())
chained.result <- batz.plotactivity_heatmap(daily.result$data, aes.default = aes.default.heatmap, dir.save = tempdir())
cat("Chained heatmap built successfully, $data rows:", nrow(chained.result$data), "\n")
cat("Total counted observations match daily.count's own filtered row count (expected TRUE):",
    sum(chained.result$data$n) == nrow(daily.result$data), "\n")

cat("\n\n########## TEST 11: filename collision bugfix - same project.name/site.label, same second ##########\n")
dir.save.collision <- file.path(tempdir(), paste0("collision_test_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
dir.create(dir.save.collision)
daily.result.c <- batz.plotactivity_daily.count(raw.rows, date.start = "2026-03-01", date.end = "2026-05-31",
                                                 aes.default = aes.default.daily, project.name = "demo",
                                                 site.label = "TestSite", dir.save = dir.save.collision)
batz.plotactivity_heatmap(daily.result.c$data, aes.default = aes.default.heatmap, project.name = "demo",
                           site.label = "TestSite", dir.save = dir.save.collision)
pngs.collision <- list.files(dir.save.collision, pattern = "\\.png$")
cat("Saved file name(s) with identical project.name/site.label (expected 2 distinct files):\n ",
    paste(pngs.collision, collapse = "\n  "), "\n")
cat("Exactly two distinct PNGs, no overwrite (expected TRUE):", length(pngs.collision) == 2, "\n")
cat("One ends in '-dailycount_<ts>.png', the other in '-heatmap_<ts>.png' (expected TRUE):",
    any(grepl("-dailycount_", pngs.collision)) && any(grepl("-heatmap_", pngs.collision)), "\n")

cat("\n\nALL TESTS COMPLETED\n")

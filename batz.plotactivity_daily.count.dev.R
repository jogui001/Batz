# =============================================================================
# batz.plotactivity_daily.count.dev.R
# -----------------------------------------------------------------------------
# Dev/test script for batz.plotactivity_daily.count() - converted from the ad
# hoc mine_bat_plot.R script. Synthetic data only (no device bridge in this
# session) - mirrors the real mine_only.xlsx column shapes described in that
# script's own header comment (filename/kpauto/monnight), deliberately with
# mixed-case/spaced headers to exercise standardize.headers().
#
# 2026-09-28: TEST 3 revised and TESTs 3b/3c added, per Josh's request that
# trim.noid operate the same way as batz.generate_plotframe.bat() - see that
# function's own @details, and this function's own @details entry "trim.noid
# matched to batz.generate_plotframe.bat". trim.noid's default changed from
# TRUE to FALSE, so the synthetic data generator now also produces some
# blank spp.id values (not just literal "NoID"), to exercise the new
# blank-handling block.
# =============================================================================

source("batz.util_standardize.headers.R")
source("batz.plotactivity_daily.count.R")

make.aes.default <- function() {
  df <- read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE, check.names = FALSE)
  df
}
aes.default.synth <- make.aes.default()

set.seed(42)
n.days <- 40
dates <- seq(as.Date("2026-07-01"), by = "day", length.out = n.days)
build.synth <- function(include.blanks = FALSE) {
  rows <- do.call(rbind, lapply(seq_along(dates), function(i) {
    d <- dates[i]
    n.obs <- sample(0:15, 1)
    if (n.obs == 0) return(NULL)
    hhmmss <- sprintf("%02d%02d%02d", sample(18:23, n.obs, replace = TRUE),
                                        sample(0:59, n.obs, replace = TRUE),
                                        sample(0:59, n.obs, replace = TRUE))
    spp.vals <- if (include.blanks) {
      sample(c("EPFU", "LABO", "NoID", "MYLU", "", NA), n.obs, replace = TRUE,
             prob = c(0.25, 0.25, 0.15, 0.15, 0.1, 0.1))
    } else {
      sample(c("EPFU", "LABO", "NoID", "MYLU"), n.obs, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))
    }
    data.frame(
      Filename = sprintf("MINE_%s_%s_000.wav", format(d, "%Y%m%d"), hhmmss),
      `KP Auto` = spp.vals,
      MonNight = format(d, "%m/%d/%Y"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }))
  rows
}
data.synth <- build.synth(include.blanks = FALSE)
data.synth.blanks <- build.synth(include.blanks = TRUE)

cat("=== synthetic raw data (mixed-case headers, mimics mine_only.xlsx) ===\n")
str(data.synth)

cat("\n\n########## TEST 1: header check catches a missing required column ##########\n")
tryCatch(
  batz.plotactivity_daily.count(data.synth[, setdiff(names(data.synth), "MonNight")], aes.default = aes.default.synth),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 2: standardize.headers() lets mixed-case/spaced raw headers resolve ##########\n")
result2 <- batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename",
                                          groupby.date = "MonNight", aes.default = aes.default.synth,
                                          dir.save = tempdir())
cat("Rows returned in $data:", nrow(result2$data), "\n")
cat("$data has $time/$date.monitoringnight:", all(c("time", "date.monitoringnight") %in% names(result2$data)), "\n")

cat("\n\n########## TEST 3: trim.noid now defaults to FALSE - NoID rows are kept by default ##########\n")
n.noid.before <- sum(tolower(trimws(data.synth[["KP Auto"]])) == "noid")
cat("NoID rows in raw synthetic data:", n.noid.before, "\n")
cat("NoID rows surviving in $data with default trim.noid = FALSE (expected == raw count, i.e. kept):",
    sum(tolower(result2$data$kp_auto) == "noid"), "\n")

result3 <- batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename",
                                          groupby.date = "MonNight", trim.noid = TRUE,
                                          aes.default = aes.default.synth, dir.save = tempdir())
cat("With trim.noid = TRUE (spp.id = 'KP Auto', a non-default column name), NoID rows surviving (expected 0):",
    sum(tolower(result3$data$kp_auto) == "noid"), "\n")

cat("\n\n########## TEST 3b: trim.noid matched to batz.generate_plotframe.bat - blank handling at spp.id's own default ('kpauto') ##########\n")
n.blank.before <- sum(is.na(data.synth.blanks[["KP Auto"]]) | !nzchar(trimws(data.synth.blanks[["KP Auto"]])))
n.noid.before.blanks <- sum(tolower(trimws(data.synth.blanks[["KP Auto"]])) %in% "noid", na.rm = TRUE)
cat("Blank spp.id rows in raw synthetic data:", n.blank.before, "| literal NoID rows:", n.noid.before.blanks, "\n")

# standardize.headers("KP Auto") -> "kp_auto", NOT "kpauto" (this function's own
# default) - so passing spp.id = "KP Auto" explicitly is a NON-default call, and
# blanks get filled in as "NOID" first, then removed by the trim.noid = TRUE
# literal-match step below (same end result, different code path - see
# TEST 3c for the actual default-spp.id case, which skips the fill step and
# removes blank rows directly).
result3b.nondefault <- batz.plotactivity_daily.count(data.synth.blanks, spp.id = "KP Auto", filename.col = "Filename",
                                                      groupby.date = "MonNight", trim.noid = TRUE,
                                                      aes.default = aes.default.synth, dir.save = tempdir())
cat("Non-default spp.id ('KP Auto'/'kp_auto'), trim.noid = TRUE - blank/NoID rows surviving (expected 0):",
    sum(is.na(result3b.nondefault$data$kp_auto) | tolower(trimws(result3b.nondefault$data$kp_auto)) %in% c("", "noid")), "\n")

cat("\n\n########## TEST 3c: trim.noid at spp.id's OWN default ('kpauto') - blanks removed directly, not filled ##########\n")
# Build data whose $spp.id column is already standardized to "kpauto" (this
# function's own default column name) so the ported
# identical(spp.id, "kpauto") && trim.noid == TRUE branch actually fires.
data.synth.default.col <- data.synth.blanks
names(data.synth.default.col)[names(data.synth.default.col) == "KP Auto"] <- "kpauto"

result3c.default.notrim <- batz.plotactivity_daily.count(data.synth.default.col, filename.col = "Filename",
                                                           groupby.date = "MonNight", trim.noid = FALSE,
                                                           aes.default = aes.default.synth, dir.save = tempdir())
cat("Default spp.id ('kpauto'), trim.noid = FALSE - blanks filled to 'NOID', not dropped (expected > 0 'noid' rows kept):",
    sum(tolower(result3c.default.notrim$data$kpauto) == "noid"), "\n")

result3c.default.trim <- batz.plotactivity_daily.count(data.synth.default.col, filename.col = "Filename",
                                                         groupby.date = "MonNight", trim.noid = TRUE,
                                                         aes.default = aes.default.synth, dir.save = tempdir())
cat("Default spp.id ('kpauto'), trim.noid = TRUE - blank/NoID rows surviving (expected 0):",
    sum(is.na(result3c.default.trim$data$kpauto) | tolower(trimws(result3c.default.trim$data$kpauto)) %in% c("", "noid")), "\n")
cat("Rows dropped by trim.noid = TRUE at the default spp.id (blanks removed directly + literal NoID removed) vs FALSE:",
    nrow(result3c.default.notrim$data) - nrow(result3c.default.trim$data), "(expected > 0, i.e. some rows were actually dropped)\n")

cat("\n\n########## TEST 4: default date window is Jul 1 - Dec 31 of the data's own year ##########\n")
cat("min/max $date.monitoringnight in $data (expected within 2026-07-01..2026-08-09, the 40-day synthetic window):",
    format(min(result2$data$date.monitoringnight)), format(max(result2$data$date.monitoringnight)), "\n")

cat("\n\n########## TEST 5: explicit date.start/date.end are honored ##########\n")
result5 <- batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename",
                                          groupby.date = "MonNight", date.start = "2026-07-10", date.end = "2026-07-20",
                                          aes.default = aes.default.synth, dir.save = tempdir())
cat("min/max $date.monitoringnight with explicit window (expected 2026-07-10..2026-07-20):",
    format(min(result5$data$date.monitoringnight)), format(max(result5$data$date.monitoringnight)), "\n")

cat("\n\n########## TEST 6: a malformed filename is dropped, not silently mis-parsed ##########\n")
data.bad.fname <- data.synth
data.bad.fname$Filename[1] <- "not_a_recognizable_name.wav"
result6 <- batz.plotactivity_daily.count(data.bad.fname, spp.id = "KP Auto", filename.col = "Filename",
                                          groupby.date = "MonNight", aes.default = aes.default.synth, dir.save = tempdir())
cat("Rows in $data after one bad filename (expected one fewer than TEST 2's", nrow(result2$data), "):", nrow(result6$data), "\n")

cat("\n\n########## TEST 7: aes.style override (round-nineteen mechanism) ##########\n")
aes.default.override <- aes.default.synth
aes.default.override$overide.value[aes.default.override$parameter == "bar.fill"] <- "steelblue"
result7 <- batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename",
                                          groupby.date = "MonNight", aes.default = aes.default.override, dir.save = tempdir())
built.fill <- ggplot2::ggplot_build(result7$ggplot)$data[[1]]$fill[1]
cat("Bar fill with $overide.value = 'steelblue' (expected a steelblue hex):", built.fill, "\n")

cat("\n\n########## TEST 8: an aes.default missing a required $parameter row stops clearly ##########\n")
aes.default.missing <- aes.default.synth[aes.default.synth$parameter != "y.custom", , drop = FALSE]
result8 <- tryCatch({
  batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename",
                                 groupby.date = "MonNight", aes.default = aes.default.missing, dir.save = tempdir())
  "NO ERROR"
}, error = function(e) conditionMessage(e))
cat("Result with $y.custom row removed:\n", result8, "\n")

cat("\n\n########## TEST 9: file naming - project.name/site.label drive the saved PNG name ##########\n")
dir.save.test <- file.path(tempdir(), paste0("dailycount_dirsave_test_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
dir.create(dir.save.test)
batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename", groupby.date = "MonNight",
                               aes.default = aes.default.synth, project.name = "acme", site.label = "WTG-GOM102",
                               dir.save = dir.save.test)
pngs <- list.files(dir.save.test, pattern = "\\.png$")
cat("Saved file name(s):", paste(pngs, collapse = ", "), "\n")
cat("Starts with 'acme_WTG-GOM102-dailycount_' (expected TRUE):", any(grepl("^acme_WTG-GOM102-dailycount_", pngs)), "\n")

cat("\n\n########## TEST 10: write.csv.data = TRUE writes a CSV alongside the PNG ##########\n")
n.before <- length(list.files(dir.save.test))
batz.plotactivity_daily.count(data.synth, spp.id = "KP Auto", filename.col = "Filename", groupby.date = "MonNight",
                               aes.default = aes.default.synth, dir.save = dir.save.test, write.csv.data = TRUE)
csvs <- list.files(dir.save.test, pattern = "\\.csv$")
cat("CSV(s) written (expected >= 1):", length(csvs), "\n")

cat("\n\nALL TESTS COMPLETED\n")

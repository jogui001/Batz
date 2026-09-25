## Dev/test script for batz.plotsm4_heatmap()
##
## Built 2026-09-23, per Josh's request to model batz.plotsm4_heatmap() on
## batz.plotdetections_first.last() and match a target SM4Bat operational-time
## heatmap image, from two real attached files (a per-15-minute-interval
## activity summary and a suntimes file). This dev script follows the same
## synthetic-fixture-plus-numbered-TEST-suite pattern used by every other
## batz plotting function's own .dev.R (see e.g.
## batz.plotdetections_first.last.dev.R/batz.plotactivity_observations.dev.R).
##
## Real end-to-end verification against Josh's actual two attached files
## (WTG-GOM101, 2026-08-19 to 2026-09-10) was also done separately (see
## run.R in this same folder) - this script instead uses small, hand-built
## synthetic fixtures so each test isolates one specific behavior and runs
## fast, independent of the real files' size/shape.

source("R/batz.util_standardize.headers.R")
source("R/batz.plotsm4_heatmap.R")

cat("========== batz.plotsm4_heatmap() dev/test script ==========\n\n")

## ---------------------------------------------------------------------
## Synthetic fixtures
## ---------------------------------------------------------------------

## Two nights, 4 tiles/night at a real 15-minute cadence, with a few
## seconds of jitter baked into $mins2.noon.mon (mirrors the real file's
## own timestamp jitter) - specifically to exercise the bin.minutes
## per-night-gap bugfix (see @details/TEST 6 below).
data.synth <- data.frame(
  aru.name       = "WTG-GOM101",
  date.mon       = rep(c("2026-08-19", "2026-08-20"), each = 4),
  mins.oper      = c(15, 15, 15, 8,     1, 15, 15, 15),
  mins2.noon.mon = c(0.02, 15.03, 30.01, 45.05,
                     0.03, 15.01, 30.04, 45.02),
  stringsAsFactors = FALSE
)

## A second-ARU copy, used by the plot.set filtering test.
data.synth.gom102 <- data.synth
data.synth.gom102$aru.name <- "WTG-GOM102"

## $aru (not $aru.name) - the real attached file's own raw spelling - to
## exercise the compatibility shim.
data.synth.rawaru <- data.synth
names(data.synth.rawaru)[names(data.synth.rawaru) == "aru.name"] <- "aru"

suntimes.synth <- data.frame(
  aru.name        = "WTG-GOM101",
  date            = c("2026-08-19", "2026-08-20"),
  date.mon        = c("2026-08-19", "2026-08-20"),
  sunregion       = "test.region",
  time.zone       = "UTC",
  sunregion.type  = "test",
  schedual1       = "",
  schedual2       = "",
  suns            = c("2026-08-19 19:30:00", "2026-08-20 19:29:00"),
  suns.unix       = 0,
  sunr            = c("2026-08-19 06:00:00", "2026-08-20 06:01:00"),
  sunr.unix       = 0,
  sunr.mon        = c("2026-08-20 06:01:00", "2026-08-21 06:02:00"),
  sunr.mon.unix   = 0,
  stringsAsFactors = FALSE
)

aes.default.synth <- read.csv("plotopts_sm4heatmap.csv", stringsAsFactors = FALSE)

fig.list.synth <- data.frame(
  plot.type     = "sm4.heatmap",
  plot.name     = "Synthetic test heatmap",
  plot.set      = "WTG-GOM101",
  date.format   = "%b-%d/n%Y",
  date.start    = "2026-08-19",
  date.end      = "2026-08-20",
  xaxe.interval = "2",
  xaxe.title    = "Monitoring Night",
  yaxe.title    = "Hour of Monitoring",
  stringsAsFactors = FALSE
)

dir.save.test <- tempfile("sm4heatmap_test_")
dir.create(dir.save.test)

## ---------------------------------------------------------------------
## TEST 1: basic end-to-end run succeeds, produces 1 plot
## ---------------------------------------------------------------------
cat("########## TEST 1: basic end-to-end run ##########\n")
result1 <- batz.plotsm4_heatmap(data.synth, fig.list.synth, suntimes.synth,
                                  aes.default.synth, dir.save = dir.save.test)
cat("plots produced (expected 1):", length(result1$plots), "\n")
cat("ggplots produced (expected 1):", length(result1$ggplots), "\n")
cat("is a ggplot object (expected TRUE):", inherits(result1$ggplots[[1]], "ggplot"), "\n\n")

## ---------------------------------------------------------------------
## TEST 2: missing required $data column stops with a clear message
## ---------------------------------------------------------------------
cat("########## TEST 2: missing required data column ##########\n")
data.bad <- data.synth
data.bad$mins.oper <- NULL
result2 <- tryCatch({
  batz.plotsm4_heatmap(data.bad, fig.list.synth, suntimes.synth, aes.default.synth, dir.save = dir.save.test)
  "NO ERROR (unexpected)"
}, error = function(e) conditionMessage(e))
cat("error message:\n  ", result2, "\n\n")

## ---------------------------------------------------------------------
## TEST 3: missing required aes.default $parameter row stops with a clear message
## ---------------------------------------------------------------------
cat("########## TEST 3: missing aes.default parameter row ##########\n")
aes.default.bad <- aes.default.synth[aes.default.synth$parameter != "fill.max", ]
result3 <- tryCatch({
  batz.plotsm4_heatmap(data.synth, fig.list.synth, suntimes.synth, aes.default.bad, dir.save = dir.save.test)
  "NO ERROR (unexpected)"
}, error = function(e) conditionMessage(e))
cat("error message:\n  ", result3, "\n\n")

## ---------------------------------------------------------------------
## TEST 4: duplicate column name stops with a clear message
## ---------------------------------------------------------------------
cat("########## TEST 4: duplicate column name in data ##########\n")
data.dup <- data.synth
names(data.dup)[names(data.dup) == "mins.oper"] <- "date.mon"  # forces a duplicate "date.mon"
result4 <- tryCatch({
  batz.plotsm4_heatmap(data.dup, fig.list.synth, suntimes.synth, aes.default.synth, dir.save = dir.save.test)
  "NO ERROR (unexpected)"
}, error = function(e) conditionMessage(e))
cat("error message:\n  ", result4, "\n\n")

## ---------------------------------------------------------------------
## TEST 5: $aru -> $aru.name compatibility shim
## ---------------------------------------------------------------------
cat("########## TEST 5: $aru (raw) -> $aru.name compatibility shim ##########\n")
result5 <- batz.plotsm4_heatmap(data.synth.rawaru, fig.list.synth, suntimes.synth,
                                  aes.default.synth, dir.save = dir.save.test)
cat("plots produced from raw $aru input (expected 1):", length(result5$plots), "\n")
cat("tile count matches the aru.name-spelled version (expected TRUE):",
    identical(nrow(result5$plots[[1]]$pd), nrow(result1$plots[[1]]$pd)), "\n\n")

## ---------------------------------------------------------------------
## TEST 6 (2026-09-23 BUGFIX regression): bin.minutes is computed WITHIN
## each night, not across nights' jittered values pooled together - see
## @details for the real "bin width 0.02 minute(s)" bug this catches.
## ---------------------------------------------------------------------
cat("########## TEST 6 (BUGFIX regression): bin.minutes computed per-night, not across nights' jitter ##########\n")
cat("bin.minutes (expected 15, NOT a tiny jitter-sized fraction):", result1$plots[[1]]$bin.minutes, "\n\n")

## ---------------------------------------------------------------------
## TEST 7 (2026-09-23 BUGFIX regression): boundary tiles (mins2.noon.mon
## near 0, i.e. right at the Noon/y.start edge) are not dropped by
## ggplot2's default out-of-bounds censoring - see @details.
## ---------------------------------------------------------------------
cat("########## TEST 7 (BUGFIX regression): edge tiles are not silently dropped ##########\n")
g1 <- result1$ggplots[[1]]
b1 <- ggplot2::ggplot_build(g1)
tile.layer <- b1$data[[1]]
cat("tile rows built (expected 8, matching data.synth's 8 rows):", nrow(tile.layer), "\n")
cat("NA ymin count in built tile layer (expected 0):", sum(is.na(tile.layer$ymin)), "\n\n")

## ---------------------------------------------------------------------
## TEST 8: $plot.set filters to the requested ARU only
## ---------------------------------------------------------------------
cat("########## TEST 8: $plot.set filters to one ARU ##########\n")
data.two.arus <- rbind(data.synth, data.synth.gom102)
result8 <- batz.plotsm4_heatmap(data.two.arus, fig.list.synth, suntimes.synth,
                                  aes.default.synth, dir.save = dir.save.test)
cat("rows in prepared plot data (expected 8, GOM102's 8 rows excluded):", nrow(result8$plots[[1]]$pd), "\n")
cat("all prepared rows are the requested ARU (expected TRUE):",
    all(tolower(trimws(result8$plots[[1]]$pd$aru.name)) == "wtg-gom101"), "\n\n")

## ---------------------------------------------------------------------
## TEST 9: $date.start/$date.end filters rows outside the requested window
## ---------------------------------------------------------------------
cat("########## TEST 9: $date.start/$date.end narrows the plotted range ##########\n")
fig.list.narrow <- fig.list.synth
fig.list.narrow$date.start <- "2026-08-20"
fig.list.narrow$date.end   <- "2026-08-20"
result9 <- batz.plotsm4_heatmap(data.synth, fig.list.narrow, suntimes.synth,
                                  aes.default.synth, dir.save = dir.save.test)
cat("rows in prepared plot data (expected 4, only 2026-08-20's 4 rows):", nrow(result9$plots[[1]]$pd), "\n\n")

## ---------------------------------------------------------------------
## TEST 10: aes.style-driven settings resolution (overide.value beats
## default.value; a blank overide.value falls through to default.value)
## ---------------------------------------------------------------------
cat("########## TEST 10: aes.style-driven settings resolution ##########\n")
aes.default.override <- aes.default.synth
aes.default.override$overide.value[aes.default.override$parameter == "fill.max"] <- "30"
result10 <- batz.plotsm4_heatmap(data.synth, fig.list.synth, suntimes.synth,
                                   aes.default.override, dir.save = dir.save.test)
g10 <- result10$ggplots[[1]]
fill.scale.limits <- ggplot2::ggplot_build(g10)$plot$scales$get_scales("fill")$limits
cat("fill scale upper limit with $overide.value = 30 set (expected 30):", fill.scale.limits[2], "\n")
fill.scale.limits.default <- ggplot2::ggplot_build(g1)$plot$scales$get_scales("fill")$limits
cat("fill scale upper limit with no override, falling to $default.value (expected 15):", fill.scale.limits.default[2], "\n\n")

## ---------------------------------------------------------------------
## TEST 11: exact-duplicate fig.list rows are collapsed before plotting
## ---------------------------------------------------------------------
cat("########## TEST 11: exact-duplicate fig.list rows are collapsed ##########\n")
fig.list.dup <- rbind(fig.list.synth, fig.list.synth, fig.list.synth)
result11 <- batz.plotsm4_heatmap(data.synth, fig.list.dup, suntimes.synth,
                                   aes.default.synth, dir.save = dir.save.test)
cat("plots produced from 3 identical fig.list rows (expected 1, 2 duplicates removed):", length(result11$plots), "\n\n")

## ---------------------------------------------------------------------
## TEST 12: non-matching plot.type rows are skipped, not errored on
## ---------------------------------------------------------------------
cat("########## TEST 12: a fig.list row with a different $plot.type is skipped ##########\n")
fig.list.mixed <- rbind(fig.list.synth, fig.list.synth)
fig.list.mixed$plot.type[2] <- "bat.detection"
result12 <- batz.plotsm4_heatmap(data.synth, fig.list.mixed, suntimes.synth,
                                   aes.default.synth, dir.save = dir.save.test)
cat("plots produced (expected 1 - only the sm4.heatmap row):", length(result12$plots), "\n\n")

## ---------------------------------------------------------------------
## TEST 13: 0 matching data rows produces no plot and no error
## ---------------------------------------------------------------------
cat("########## TEST 13: 0 matching rows produces no plot, no error ##########\n")
fig.list.nomatch <- fig.list.synth
fig.list.nomatch$plot.set <- "NO-SUCH-ARU"
result13 <- batz.plotsm4_heatmap(data.synth, fig.list.nomatch, suntimes.synth,
                                   aes.default.synth, dir.save = dir.save.test)
cat("plots produced (expected 0):", length(result13$plots), "\n\n")

## ---------------------------------------------------------------------
## TEST 14 (2026-09-24 regression): Dawn/Dusk/Midnight lines are
## flat-extended to the true panel edges (date.start - 0.5 / date.end +
## 0.5), not just the real suntimes dates - see @details.
## ---------------------------------------------------------------------
cat("########## TEST 14 (regression): reference lines extend to the panel edges ##########\n")
sdb1 <- result1$plots[[1]]$sdb
cat("sdb rows (expected 4: 2 real + 2 synthetic boundary rows):", nrow(sdb1), "\n")
cat("first row's date is date.start - 0.5 (expected TRUE):",
    sdb1$date.parsed[1] == (as.Date("2026-08-19") - 0.5), "\n")
cat("last row's date is date.end + 0.5 (expected TRUE):",
    sdb1$date.parsed[nrow(sdb1)] == (as.Date("2026-08-20") + 0.5), "\n")
cat("boundary rows copy the nearest real row's dusk.time, unchanged (expected TRUE):",
    sdb1$dusk.time[1] == sdb1$dusk.time[2] &&
      sdb1$dusk.time[nrow(sdb1)] == sdb1$dusk.time[nrow(sdb1) - 1], "\n\n")

## ---------------------------------------------------------------------
## TEST 15 (2026-09-24 regression): the fill legend's colorbar orientation
## follows $legend.position - horizontal when "bottom", vertical otherwise.
## ---------------------------------------------------------------------
cat("########## TEST 15 (regression): colorbar direction follows $legend.position ##########\n")
fill.guide.bottom <- ggplot2::ggplot_build(result1$ggplots[[1]])$plot$scales$get_scales("fill")$guide
cat("colorbar direction with $legend.position = 'bottom' (expected horizontal):",
    fill.guide.bottom$direction, "\n")

aes.default.side <- aes.default.synth
aes.default.side$overide.value[aes.default.side$parameter == "legend.position"] <- "right"
result15 <- batz.plotsm4_heatmap(data.synth, fig.list.synth, suntimes.synth,
                                   aes.default.side, dir.save = dir.save.test)
fill.guide.side <- ggplot2::ggplot_build(result15$ggplots[[1]])$plot$scales$get_scales("fill")$guide
cat("colorbar direction with $legend.position = 'right' (expected vertical):",
    fill.guide.side$direction, "\n\n")

cat("========== all tests complete ==========\n")

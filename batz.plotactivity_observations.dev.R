# =============================================================================
# batz.plotactivity_observations() - development/test script
# =============================================================================
#
# Iteration 1 ("basic layout"), built 2026-08-28, copying the structure/steps
# of batz.plotdetections_first.last() and modifying for a count-based
# ($obs) Y axis instead of a time-of-night Y axis, per Josh's own spec.
#
# ASSUMPTIONS / KNOWN GAPS (flag if any of these are wrong):
#  - fig.list.csv does NOT yet have a real row with $plot.type =
#    "call.observations" (Josh's real file only has "bat.detection" rows so
#    far) - every test below builds its own synthetic fig.list row(s) for
#    this plot.type, several using Josh's real plfr.bats.csv as the actual
#    $data underneath so this is still a genuine real-data test, just with
#    a synthetic fig.list row wrapped around it (same approach
#    batz.plotdetections_first.last.dev.R used before Josh's real
#    plot.meta.csv had a usable row).
#  - suntimes is accepted/header-checked but not used by this function yet
#    (see @details) - a small synthetic data frame with just the required
#    headers is used everywhere below instead of loading the real (1.16MB)
#    gome_20250101to20300201_suntimes.csv, since its actual VALUES don't
#    affect anything this iteration - much faster to build/tear down per
#    test. Swap in the real file if/when suntimes actually gets used.
#  - plotopts_callobs.csv is a FRESH settings file built for this function
#    (not a copy of plotopts_first.last.csv/batactivity.plotoptions.csv) -
#    Josh's own on-disk plotopts_callobs.csv (in his test-data folder) was,
#    at the time this was built, an unmodified copy of plotopts_first.last.csv
#    (same file size, same dawn/dusk/midnight/crossbar rows) - i.e. a
#    placeholder Josh hadn't yet edited for this new function. The version
#    delivered alongside this function REPLACES that placeholder with a
#    parameter set actually built for this plot type (no dawn/dusk/midnight/
#    time.zone/layer.order rows; new Y-axis/bar-fill rows instead).
#  - Real data quirk found and left alone, not "fixed" (nothing to fix in
#    code - it's a legitimate data question for Josh): plfr.bats.csv has at
#    least one row with a BLANK $spp.id (e.g. WTG-GOM102, 6/21/2026, obs=55)
#    that's neither a named species nor "All Detections"/"40kMyo"/HiF/LoF/
#    etc - it doesn't match anything in batz.batusa_recode.names()'s
#    reference table, so it's silently excluded from every plot (same as
#    any other genuinely-unmatched value) and triggers that function's own
#    console WARNING. Flagging in case this blank-species bucket should
#    actually be folded into "All detections" or a specific category.
#
# FOLLOW-UP (2026-08-28), per Josh - $plot.group/$plot.sets/$pool added,
# built and tested using his two newly-attached files (see @details in the
# .R file for the full design and the flagged assumption around dodged,
# same-colored bars when $pool = FALSE):
#  - fig.list.csv (Josh's real file, 1 row) already uses the new column
#    names (plot.group = "aru.name", plot.sets = a triple-quoted 3-value
#    field, pool = FALSE) - used directly in TEST 17 below, unmodified.
#  - processed.csv (Josh's real file, 1963 data rows: spp.id/date/aru.name/
#    sunregion/obs/mins2.noon.min/mins2.noon.max/vetting.type) is used as
#    `data` in TEST 15-17 - it does NOT have a column called "aru.groupby"
#    at all, only "aru.name" - exactly the scenario $plot.group generalizes
#    for (this data was very likely never produced by
#    batz.generate_plotframe.bat() itself, e.g. it may be a hand-summarized
#    sheet - $plot.group = "aru.name" tells this function to filter/group on
#    that column directly, with no renaming needed).
#  - TEST 15-16 build their own small synthetic fig.list rows (like the rest
#    of this file) against processed.csv to isolate plot.group/plot.sets/
#    pool one at a time; TEST 17 uses Josh's real fig.list.csv row as-is,
#    end-to-end, against processed.csv.
#
# =============================================================================

setwd("/home/claude/plotactivity_work")
source("batz.plotactivity_observations.R")
source("/home/claude/plotdetections_work/batz.batusa_recode.names.R")  # dependency, unchanged, pulled from the sibling function's own work folder

plot.data.real <- read.csv("/mnt/user-data/uploads/4 Current  test data/plfr.bats.csv",
                            stringsAsFactors = FALSE, check.names = FALSE)
plot.data.real <- plot.data.real[, names(plot.data.real) != "", drop = FALSE]  # drop the row-number column read.csv picked up from the CSV's blank first header

aes.default.real <- read.csv("plotopts_callobs.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Minimal synthetic suntimes - only the required HEADERS matter this
# iteration (see header comment above). One dummy row is enough to pass
# check.headers()/check.duplicates().
suntimes.synth <- data.frame(
  aru = "WTG-GOM102", date = "2026-06-20", date.mon = "2026-06-21",
  sunregion = "penobscotbay", time.zone = "America/New_York", sunregion.type = "coastal",
  schedual1 = "", schedual2 = "", suns = "2026-06-20 20:30:00", suns.unix = 0,
  sunr = "2026-06-21 05:00:00", sunr.unix = 0, sunr.mon = "2026-06-21 05:00:00", sunr.mon.unix = 0,
  stringsAsFactors = FALSE
)

## plot.group/plot.sets/pool (2026-08-28 follow-up - see @details in the .R
## file): plot.data.real (plfr.bats.csv) still has its column literally
## named "aru.groupby" (predates the batz.generate_plotframe.bat() rename
## of that column to $group), so plot.group defaults to "aru.groupby" here
## to match it - see TEST 15+ below for plot.group actually generalized to
## a DIFFERENT column, using Josh's new processed.csv/fig.list.csv fixtures.
make.job <- function(plot.group = "aru.groupby", plot.sets = "WTG-GOM102", pool = FALSE,
                      date.start = "6/20/2026", date.end = "7/3/2026",
                      Yaxe.trans = "", y.scale = "", y.custom = "", ymax = "",
                      loglabels = "", plot.name = "University of  Maine WTG turbine - Call Observations") {
  data.frame(
    plot.type = "call.observations", plot.name = plot.name, facet = "sppid", facet.set = "NE",
    MYSO = FALSE, Alldect = TRUE, facet.panel = "", `40khzmyo` = TRUE, facet.label = "",
    plot.group = plot.group, plot.sets = plot.sets, pool = pool,
    date.format = "%b-%d/n%Y", date.start = date.start, date.end = date.end,
    xaxe.interval = 4, Yaxe.trans = Yaxe.trans, y.scale = y.scale, y.custom = y.custom, ymax = ymax,
    loglabels = loglabels,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

cat("\n\n########## TEST 1: header checks catch real problems ##########\n")
bad.data <- plot.data.real[, setdiff(names(plot.data.real), "obs"), drop = FALSE]
result <- tryCatch(
  batz.plotactivity_observations(bad.data, make.job(), suntimes.synth, aes.default.real),
  error = function(e) conditionMessage(e)
)
cat("Missing $obs column ->", result, "\n")

dup.fig.list <- make.job()
dup.fig.list$plot.type.2 <- dup.fig.list$plot.type
names(dup.fig.list)[names(dup.fig.list) == "plot.type.2"] <- "plot.type"
result <- tryCatch(
  batz.plotactivity_observations(plot.data.real, dup.fig.list, suntimes.synth, aes.default.real),
  error = function(e) conditionMessage(e)
)
cat("Duplicate column name in fig.list ->", result, "\n")

bad.aes <- aes.default.real[aes.default.real$parameter != "bar.width", , drop = FALSE]
result <- tryCatch(
  batz.plotactivity_observations(plot.data.real, make.job(), suntimes.synth, bad.aes),
  error = function(e) conditionMessage(e)
)
cat("aes.default missing a required $parameter row ->", result, "\n")

cat("\n\n########## TEST 2: real plfr.bats.csv, real ARU/date window, default (none) Y-axis trans ##########\n")
result2 <- batz.plotactivity_observations(
  data = plot.data.real, fig.list = make.job(), suntimes = suntimes.synth,
  aes.default = aes.default.real
)
cat("$plots entries (expected 1):", length(result2$plots), "\n")
cat("$ggplots entries (expected 1):", length(result2$ggplots), "\n")
if (length(result2$ggplots) > 0) {
  gb <- ggplot2::ggplot_build(result2$ggplots[[1]])
  cat("Total facet panel count, incl. empty ones via facet_wrap(drop=FALSE) (expected 9 - 8 species + All detections):",
       nrow(gb$layout$layout), "\n")
  cat("Panels with at least one bar this window (expected 3 - only 2 real named species (Big brown bat/Hoary bat) + All detections had data 6/20-6/25):",
       length(unique(gb$data[[1]]$PANEL)), "\n")
}

cat("\n\n########## TEST 3: Yaxe.trans none/log/log10 all produce a working plot; obs.plot values match the expected transform ##########\n")
for (tr in c("none", "log", "log10")) {
  r <- batz.plotactivity_observations(plot.data.real, make.job(Yaxe.trans = tr), suntimes.synth, aes.default.real)
  pd <- r$plots[[1]]$pd
  expected <- switch(tr, none = pd$obs, log = log1p(pd$obs), log10 = log10(pd$obs + 1))
  cat(sprintf("Yaxe.trans = '%s': obs.plot matches expected transform exactly: %s\n",
               tr, isTRUE(all.equal(pd$obs.plot, expected))))
}
result.badtrans <- batz.plotactivity_observations(plot.data.real, make.job(Yaxe.trans = "sqrt"), suntimes.synth, aes.default.real)
cat("Invalid Yaxe.trans falls back to 'none' (no crash):", length(result.badtrans$plots) == 1, "\n")

cat("\n\n########## TEST 4: y.scale regular/rounded/custom produce the expected break positions/labels ##########\n")
r.reg <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "regular", ymax = "70"), suntimes.synth, aes.default.real)
cat("regular, ymax=70 -> break labels (expected 0,17.5,35,52.5,70):", paste(r.reg$plots[[1]]$break.labels, collapse = ", "), "\n")

r.round <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "rounded", ymax = "70"), suntimes.synth, aes.default.real)
cat("rounded, ymax=70 -> break labels (expected 0,18,35,52,70 - R's round() is round-half-to-even, so round(52.5) = 52, not 53):",
     paste(r.round$plots[[1]]$break.labels, collapse = ", "), "\n")

r.round2 <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "rounded", ymax = "10"), suntimes.synth, aes.default.real)
cat("rounded, ymax=10 -> break labels (expected 0,2,5,8,10 - round-half-to-even: round(2.5)=2, round(7.5)=8):",
     paste(r.round2$plots[[1]]$break.labels, collapse = ", "), "\n")
r.reg2 <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "regular", ymax = "10"), suntimes.synth, aes.default.real)
cat("regular, ymax=10 -> break labels (expected exact fractions 0,2.5,5,7.5,10):",
     paste(r.reg2$plots[[1]]$break.labels, collapse = ", "), "\n")

r.custom <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "custom", y.custom = "0;2;8;25;70", ymax = "70"), suntimes.synth, aes.default.real)
cat("custom, y.custom='0;2;8;25;70' -> break labels (expected 0,2,8,25,70):",
     paste(r.custom$plots[[1]]$break.labels, collapse = ", "), "\n")

r.custom.badnums <- batz.plotactivity_observations(plot.data.real, make.job(y.scale = "custom", y.custom = "not,numbers"), suntimes.synth, aes.default.real)
cat("custom with unusable $y.custom falls back to regular (no crash):", length(r.custom.badnums$plots) == 1, "\n")

cat("\n\n########## TEST 5: loglabels TRUE/FALSE controls whether break labels show real counts or the transformed value ##########\n")
r.loglab.false <- batz.plotactivity_observations(plot.data.real, make.job(Yaxe.trans = "log", y.scale = "custom", y.custom = "0;2;8;25;70", loglabels = "FALSE"), suntimes.synth, aes.default.real)
cat("Yaxe.trans=log, loglabels=FALSE -> labels are real counts (expected 0,2,8,25,70):",
     paste(r.loglab.false$plots[[1]]$break.labels, collapse = ", "), "\n")
r.loglab.true <- batz.plotactivity_observations(plot.data.real, make.job(Yaxe.trans = "log", y.scale = "custom", y.custom = "0;2;8;25;70", loglabels = "TRUE"), suntimes.synth, aes.default.real)
cat("Yaxe.trans=log, loglabels=TRUE -> labels are log1p-transformed values (expected 0,1.1,2.2,3.26,4.26 approx):",
     paste(r.loglab.true$plots[[1]]$break.labels, collapse = ", "), "\n")

cat("\n\n########## TEST 6: $ymax blank/unusable auto-falls-back to max($obs) in that plot's own data, with a console NOTE ##########\n")
r.automax <- batz.plotactivity_observations(plot.data.real, make.job(ymax = ""), suntimes.synth, aes.default.real)
pd.auto <- r.automax$plots[[1]]$pd
cat("Auto ymax equals max(obs) in filtered data (expected TRUE):",
     max(r.automax$plots[[1]]$break.pos) >= 0 && isTRUE(all.equal(max(pd.auto$obs), max(pd.auto$obs))), "\n")
cat("max(obs) actually present in this job's filtered data:", max(pd.auto$obs), "\n")

r.badmax <- batz.plotactivity_observations(plot.data.real, make.job(ymax = "not.a.number"), suntimes.synth, aes.default.real)
cat("Unusable $ymax also falls back cleanly (no crash):", length(r.badmax$plots) == 1, "\n")

cat("\n\n########## TEST 7: 40kHzMyo bar overlay - real data uses '40kMyo' (not '40KHzMyo'), workaround must still catch it ##########\n")
result7 <- batz.plotactivity_observations(plot.data.real, make.job(), suntimes.synth, aes.default.real)
pd7 <- result7$plots[[1]]$pd
n.khz.rows <- sum(pd7$bar.type == "40kHzMyo")
cat("Rows recognized as 40kHzMyo bar.type (expected 2, matching the two real '40kMyo' rows on 6/20 and 6/23 in this window):", n.khz.rows, "\n")
if (length(result7$ggplots) > 0) {
  fill.scale <- ggplot2::ggplot_build(result7$ggplots[[1]])$plot$scales$get_scales("fill")
  cat("Fill legend breaks (expected '40kHzMyo' only):", paste(fill.scale$get_breaks(), collapse = ", "), "\n")
}
# Confirm batz.batusa_recode.names() ALONE (no workaround) would NOT have caught "40kMyo" -
# i.e. this is a real mismatch this function had to work around, not a non-issue.
direct.recode <- batz.batusa_recode.names("40kMyo", output.format = "common")
cat("batz.batusa_recode.names('40kMyo') alone (expected: passed through unchanged, i.e. still '40kMyo', proving the mismatch is real):", direct.recode, "\n")

cat("\n\n########## TEST 8: rows sharing the SAME $plot.name but otherwise DIFFERENT still each produce their own plot (job.key fix, carried over) ##########\n")
job.a <- make.job(date.start = "6/20/2026", date.end = "6/25/2026")
job.b <- make.job(date.start = "6/20/2026", date.end = "6/25/2026")
job.b$xaxe.interval <- 2
job.c <- make.job(date.start = "6/20/2026", date.end = "6/25/2026")
job.c$xaxe.interval <- 3
jobs.samename <- rbind(job.a, job.b, job.c)
result8 <- batz.plotactivity_observations(plot.data.real, jobs.samename, suntimes.synth, aes.default.real)
cat("$plots entries produced (expected 3, would silently collapse to 1 without the job.key fix):", length(result8$plots), "\n")

cat("\n\n########## TEST 9: exact full-row fig.list duplicates are removed before plotting (carried over dedup fix) ##########\n")
job.dup <- make.job(date.start = "6/20/2026", date.end = "6/25/2026")
jobs.dup <- rbind(job.dup, job.dup, job.dup)
result9 <- batz.plotactivity_observations(plot.data.real, jobs.dup, suntimes.synth, aes.default.real)
cat("$plots entries produced (expected 1 - 2 exact duplicates removed):", length(result9$plots), "\n")

cat("\n\n########## TEST 10: unrecognized fig.list $plot.type rows (e.g. 'bat.detection') are skipped with a NOTE, not an error ##########\n")
job.other <- make.job()
job.other$plot.type <- "bat.detection"
jobs.mixed <- rbind(job.other, make.job(date.start = "6/20/2026", date.end = "6/25/2026"))
result10 <- batz.plotactivity_observations(plot.data.real, jobs.mixed, suntimes.synth, aes.default.real)
cat("Mixed fig.list ('bat.detection' + 'call.observations') -> $plots entries (expected 1, only the matching row):", length(result10$plots), "\n")

cat("\n\n########## TEST 11: dir.save controls where the PNG is written ##########\n")
dir.create("test_dirsave_out", showWarnings = FALSE)
result11 <- batz.plotactivity_observations(plot.data.real, make.job(date.start = "6/20/2026", date.end = "6/25/2026"),
                                            suntimes.synth, aes.default.real, dir.save = "test_dirsave_out")
files.in.dirsave <- list.files("test_dirsave_out", pattern = "\\.png$")
files.in.wd <- list.files(".", pattern = "\\.png$")
cat("PNG written into dir.save (expected >=1):", length(files.in.dirsave), "\n")
unlink("test_dirsave_out", recursive = TRUE)

cat("\n\n########## TEST 12: project.name column override still resolves correctly (three-tier precedence, carried over from the sibling function) ##########\n")
aes.default.gome <- aes.default.real
aes.default.gome$gome <- ""
aes.default.gome$gome[aes.default.gome$parameter == "yaxe.title"] <- "Calls Detected (gome project)"
result12 <- batz.plotactivity_observations(plot.data.real, make.job(date.start = "6/20/2026", date.end = "6/25/2026"),
                                            suntimes.synth, aes.default.gome, project.name = "gome")
if (length(result12$ggplots) > 0) {
  y.title <- ggplot2::ggplot_build(result12$ggplots[[1]])$plot$scales$get_scales("y")$name
  cat("yaxe.title with project.name='gome' override (expected 'Calls Detected (gome project)'):",
       trimws(y.title), "\n")
}

cat("\n\n########## TEST 13: 40kHzMyo overlay is drawn as its own layer, on top, so it isn't hidden behind the all-detections bar (real bug found via visual render, fixed by splitting into two geom_col() layers) ##########\n")
result13 <- batz.plotactivity_observations(plot.data.real, make.job(date.start = "6/20/2026", date.end = "6/25/2026"), suntimes.synth, aes.default.real)
g13 <- result13$ggplots[[1]]
cat("Number of geom_col() layers (expected 2 - all-detections then 40kHzMyo, drawn in that order):", length(g13$layers), "\n")
layer1.types <- unique(g13$layers[[1]]$data$bar.type)
layer2.types <- unique(g13$layers[[2]]$data$bar.type)
cat("Layer 1 (bottom) is all-detections only (expected TRUE):", identical(layer1.types, "All detections"), "\n")
cat("Layer 2 (top) is 40kHzMyo only (expected TRUE):", identical(layer2.types, "40kHzMyo"), "\n")

cat("\n\n########## TEST 14: BUGFIX 2026-08-28 - Yaxe.trans='log10' + y.scale='regular' breaks are evenly SPACED ALONG THE AXIS, not clustered near the top (Josh's exact real scenario: Yaxe.trans=log10, y.scale=regular, ymax=230) ##########\n")
result14 <- batz.plotactivity_observations(
  plot.data.real,
  make.job(date.start = "6/20/2026", date.end = "6/25/2026", Yaxe.trans = "log10", y.scale = "regular", ymax = "230"),
  suntimes.synth, aes.default.real
)
p14 <- result14$plots[[1]]
break.pos.range <- max(p14$break.pos) - min(p14$break.pos)
break.pos.frac <- (p14$break.pos - min(p14$break.pos)) / break.pos.range
cat("Break positions as % of axis span (expected 0,25,50,75,100 - evenly spaced; OLD buggy code produced 0,74.8,87.3,94.7,100):",
    paste(round(break.pos.frac * 100, 1), collapse = ", "), "\n")
gaps <- diff(break.pos.frac * 100)
cat("Consecutive gaps between breaks, in % of axis span (expected all ~25, i.e. equal - this is the actual 'evenly spaced along the axis' check):",
    paste(round(gaps, 1), collapse = ", "), "\n")
cat("All gaps equal within rounding tolerance (expected TRUE):", all(abs(gaps - gaps[1]) < 0.01), "\n")
cat("Break labels show real (untransformed) counts, rounded to 1 decimal for readability (expected 0, 2.9, 14.2, 58.3, 230 - NOT 6-decimal raw values like 2.898549):",
    paste(trimws(p14$break.labels), collapse = ", "), "\n")

cat("\n\n########## TEST 15: $plot.group generalizes away from the hardcoded $aru.groupby column - Josh's new processed.csv only has $aru.name, no $aru.groupby at all ##########\n")
processed.real <- read.csv("josh_uploads/processed.csv", stringsAsFactors = FALSE, check.names = FALSE)
processed.real <- processed.real[, names(processed.real) != "", drop = FALSE]
cat("processed.csv has an $aru.name column but NOT an $aru.groupby column (confirms this is a genuine plot.group generalization test, not a renamed pass-through)?",
    "aru.name" %in% names(processed.real) && !("aru.groupby" %in% names(processed.real)), "\n")

job15 <- make.job(plot.group = "aru.name", plot.sets = "105059-NW3",
                   date.start = "7/7/2026", date.end = "8/18/2026")
result15 <- batz.plotactivity_observations(processed.real, job15, suntimes.synth, aes.default.real)
cat("$plots entries produced (expected 1):", length(result15$plots), "\n")
cat("every row of prepared data's $group.val is the selected plot.sets value ('105059-NW3')?",
    length(result15$plots) == 1 && all(result15$plots[[1]]$pd$group.val == "105059-NW3"), "\n")

job15b <- make.job(plot.group = "not.a.real.column", plot.sets = "105059-NW3",
                    date.start = "7/7/2026", date.end = "8/18/2026")
result15b <- batz.plotactivity_observations(processed.real, job15b, suntimes.synth, aes.default.real)
cat("$plot.group naming a column that doesn't exist in `data` is skipped, not an error (expected 0 plots):",
    length(result15b$plots), "\n")

job15c <- make.job(plot.group = "", plot.sets = "105059-NW3",
                    date.start = "7/7/2026", date.end = "8/18/2026")
result15c <- batz.plotactivity_observations(processed.real, job15c, suntimes.synth, aes.default.real)
cat("blank $plot.group is skipped, not an error (expected 0 plots):", length(result15c$plots), "\n")

cat("\n\n########## TEST 16: $plot.sets multi-value parsing + $pool TRUE/FALSE ##########\n")
job16.multi <- make.job(plot.group = "aru.name",
                         plot.sets = '"105059-NW3" "105059-SE3" "105059-SW3"',
                         date.start = "7/7/2026", date.end = "8/18/2026", pool = FALSE)
result16.multi <- batz.plotactivity_observations(processed.real, job16.multi, suntimes.synth, aes.default.real)
pd16 <- result16.multi$plots[[1]]$pd
cat("all 3 selected $plot.sets values present in the prepared (unpooled) data?",
    setequal(unique(pd16$group.val), c("105059-NW3", "105059-SE3", "105059-SW3")), "\n")
## row count unpooled should equal running each of the 3 detectors alone and
## summing their row counts (pool=FALSE just concatenates, never collapses) -
## NOT simply 3x a single detector's count, since different detectors don't
## necessarily have the same number of species/date combinations present
n.each <- sapply(c("105059-NW3", "105059-SE3", "105059-SW3"), function(v) {
  jv <- make.job(plot.group = "aru.name", plot.sets = v, date.start = "7/7/2026", date.end = "8/18/2026")
  nrow(batz.plotactivity_observations(processed.real, jv, suntimes.synth, aes.default.real)$plots[[1]]$pd)
})
cat("row count unpooled equals the sum of running each of the 3 detectors separately (nothing got summed/dropped)?",
    nrow(pd16) == sum(n.each), "\n")

job16.pool <- make.job(plot.group = "aru.name",
                        plot.sets = '"105059-NW3" "105059-SE3" "105059-SW3"',
                        date.start = "7/7/2026", date.end = "8/18/2026", pool = TRUE)
result16.pool <- batz.plotactivity_observations(processed.real, job16.pool, suntimes.synth, aes.default.real)
pd16.pool <- result16.pool$plots[[1]]$pd
cat("pooled data collapses to a single $group.val ('pooled')?",
    length(unique(pd16.pool$group.val)) == 1 && unique(pd16.pool$group.val) == "pooled", "\n")

## cross-check: pooled $obs for one date/panel should equal the sum of the
## 3 individual detectors' $obs for that same date/panel, unpooled
chk.panel <- pd16$facet.panel.value[1]
chk.date  <- pd16$date.parsed[1]
sum.unpooled <- sum(pd16$obs[pd16$facet.panel.value == chk.panel & pd16$date.parsed == chk.date])
sum.pooled   <- sum(pd16.pool$obs[pd16.pool$facet.panel.value == chk.panel & pd16.pool$date.parsed == chk.date])
cat("pooled $obs for one date/panel equals the sum of the 3 unpooled detectors' $obs for that same date/panel?",
    isTRUE(all.equal(sum.pooled, sum.unpooled)), "\n")

## a single selected $plot.sets value should behave identically pooled or not (nothing to sum/dodge)
job16.single.pool   <- make.job(plot.group = "aru.name", plot.sets = "105059-NW3",
                                 date.start = "7/7/2026", date.end = "8/18/2026", pool = TRUE)
result16.single.pool <- batz.plotactivity_observations(processed.real, job16.single.pool, suntimes.synth, aes.default.real)
cat("single-value $plot.sets: pooled total $obs equals unpooled total $obs (expected TRUE, nothing to pool)?",
    isTRUE(all.equal(sum(result16.single.pool$plots[[1]]$pd$obs), sum(result15$plots[[1]]$pd$obs))), "\n")

## dodge rendering check (ggplot2 layer, mirrors TEST 13's approach for the sibling overlay bug)
if (length(result16.multi$ggplots) > 0) {
  g16 <- result16.multi$ggplots[[1]]
  cat("pool=FALSE with 3 plot.sets values uses position_dodge2 (not identity) for the geom_col layers?",
       inherits(g16$layers[[1]]$position, "PositionDodge2"), "\n")
}
if (length(result16.pool$ggplots) > 0) {
  g16p <- result16.pool$ggplots[[1]]
  cat("pool=TRUE uses position 'identity' (one bar per date, nothing to dodge)?",
       inherits(g16p$layers[[1]]$position, "PositionIdentity"), "\n")
}

cat("\n\n########## TEST 17: Josh's real fig.list.csv row, end-to-end against his real processed.csv ##########\n")
fig.list.real <- read.csv("josh_uploads/fig.list.csv", stringsAsFactors = FALSE, check.names = FALSE)
cat("real fig.list.csv row's $plot.group/$plot.sets/$pool as read:\n")
print(fig.list.real[, c("plot.group", "plot.sets", "pool")])
result17 <- batz.plotactivity_observations(processed.real, fig.list.real, suntimes.synth, aes.default.real)
cat("$plots entries produced from Josh's real fig.list.csv row (expected 1):", length(result17$plots), "\n")
if (length(result17$plots) == 1) {
  pd17 <- result17$plots[[1]]$pd
  cat("all 3 of Josh's real $plot.sets detectors present (105059-NW3/SE3/SW3), pool=FALSE so not collapsed?",
       setequal(unique(pd17$group.val), c("105059-NW3", "105059-SE3", "105059-SW3")), "\n")
}
cat("$ggplots entries produced (expected 1):", length(result17$ggplots), "\n")

cat("\n\nEXIT: 0\n")

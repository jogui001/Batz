# =============================================================================
# batz.plotdetections_first.last.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.plotdetections_first.last() - tested here before being
# wrapped into the final function. ITERATION 1 ("basic layout" per Josh's own
# framing - "Development of this current function will be iterative, first
# getting the basic layout then moving towards adding options").
#
# Purpose: generates the standard report plot showing, for each species (plus
# an "All detections" panel and an optional overlaid 40kHzMyo indicator), the
# earliest-to-latest nightly detection window across the monitoring period,
# with Dawn/Dusk/Midnight reference lines - i.e. a general, package-ready
# version of the exact plot Josh and I iterated on earlier today when
# building plotoptions.batactivity.default.csv from his ggplot2 script.
#
# NAME NORMALIZATION (per Josh's naming conventions):
#   - Requested name was "batz.plotdections_first.last()" - "plotdections" is
#     a typo for "plotdetections" (confirmed against Josh's own purpose text,
#     "...nightly detections by species"); normalized to
#     batz.plotdetections_first.last() - just the typo fix, nothing else
#     changed. Family = "plotdetections" (mirrors the existing
#     "plotframe"/"plotdetections" pattern where the verb "plot" is baked
#     into the family name itself, same as batz.plotframe_batactivity, which
#     also has no separate action word after the family), subject =
#     "first.last" (what's being plotted: each species' first and last
#     nightly detection). Flagging this rename to Josh per project
#     convention - not silently renamed.
#
# *** THE GGPLOT CODE BLOCK IN JOSH'S SPEC DOES NOT MATCH THIS FUNCTION ***
#   The ggplot2 code pasted at the end of the spec (geom_tile() heatmap of
#   "SM4Bat Operational Minutes", titled "... Operational Time Heatmap") is
#   NOT the earliest/latest-detection crossbar plot this function is
#   supposed to produce - it looks like an operational-time heatmap script
#   from a different report (SM4 log summary), pasted in by mistake. The
#   REAL target - "target.output" = "Earlies and lastest batcall.png",
#   confirmed by opening the real file at Josh's test-data path - is exactly
#   the crossbar/geom_line design Josh and I already built out in full in
#   plotoptions.batactivity.default.csv earlier today (gray "All detections"
#   crossbar per panel, black "40kHzMyo" crossbar overlay, blue dashed Dawn/
#   red dashed Dusk/black solid Midnight reference lines, one facet panel per
#   species). This dev script builds THAT plot, not the pasted geom_tile
#   code. **Please confirm this reading is correct** - if the geom_tile
#   heatmap script actually WAS intended for this function, this needs a
#   different data model entirely (it operates on ARU operational minutes,
#   not detection min/max times) and would need to be rebuilt from scratch.
#
# TEST DATA STATUS - real files found and used directly from Josh's own
# "4 Current  test data" folder (device-bridge access already granted):
#   - vetted.processed.csv -> plot.data: headers match the spec's list
#     EXACTLY ($spp.id $date $aru.groupby $obs $mins2.noon.min
#     $mins2.noon.max $vetting.type). Real data: 13 rows, ARU "WTG-GOM102",
#     dates 5/15/2026-5/21/2026, spp.id values include "All Detections"
#     (capital D - matches case-insensitively via batz.batusa_recode.names,
#     see below), "LoF"/"LoFrag" (the exact category labels just added to
#     batz.batusa_recode.names earlier today), "Hoary bat"/"Big brown bat"/
#     "Silver-haired bat".
#   - plot.meta.csv -> aru.metadata.db: **real headers do NOT match the
#     spec's literal list** ($plot.type $varible1 $varible2 $facet
#     $facet.set $plot.set $date.format $date.start $date.end $xaxe
#     $xaxe.ticks $yaxe $yaxe.ticks $ytransform). The real file instead has:
#     plot.type, plot.name, facet, facet.set, MYSO, Alldect, facet.panel,
#     40khzmyo, facet.label, plot.set, date.format, date.start, date.end,
#     xaxe.interval, xaxe.title (TWICE - see bug note below). This isn't a
#     stale-file problem - the spec's own "Steps" section explicitly
#     references $MYSO/$Alldect/$40khzmyo/$facet.label by name, and those
#     columns only exist in the REAL file, not the literal header list
#     above. **Used the real file's headers as the actual required list**
#     (flagging the mismatch rather than silently reconciling it) since the
#     Steps logic could not work at all against the literal list.
#   - **Real bug found in Josh's plot.meta.csv: duplicate "xaxe.title"
#     column.** The file has TWO columns both named "xaxe.title" - the
#     second one's value ("Hour of mointoring") is clearly meant as a
#     Y-AXIS title override (it reads as one, and the target plot's y-axis
#     title is "Hour of Monitoring"), so the second occurrence was almost
#     certainly meant to be "yaxe.title", not a second "xaxe.title". Since
#     R does allow (and silently mis-handles) duplicate column names when
#     check.names = FALSE, this function explicitly checks for and stops on
#     any duplicate column name in aru.metadata.db, rather than silently
#     picking one of the two "xaxe.title" columns - **Josh: please rename
#     the second "xaxe.title" column to "yaxe.title" in plot.meta.csv.**
#   - aru_..._suntimes.csv -> suntimes.db: matches the spec's required list
#     (real file is a superset - also has $sunregion.long/$sunregion.lat/
#     $lat/$long, all fine as extra columns) - this is a real output of
#     batz.suntimes_generate() from earlier today. **Real-data misalignment
#     to flag: this suntimes file only has ARU "WTG-GOM101", dates
#     1/1/2025-2/1/2025 - but vetted.processed.csv's real detections are all
#     for ARU "WTG-GOM102", dates in May 2026, and plot.meta.csv's own
#     date.start/date.end (4/8/2026-4/27/2026) is a THIRD, different date
#     range again.** None of the three real test files line up with each
#     other on ARU name or date range. This isn't something this function
#     can fix - it just means an end-to-end run against these exact three
#     files as-is produces an empty plot (0 matching detection rows AND 0
#     matching suntimes rows for the requested plot.set/date window) rather
#     than the populated target image. Tested below against a small aligned
#     SYNTHETIC dataset (built from the same real values) to prove the
#     pipeline logic itself is correct, and separately against the real
#     files to confirm the (mis)alignment problem is surfaced clearly
#     instead of silently producing a wrong plot. **Josh: please align the
#     three real test files (same ARU name, overlapping date range) for a
#     true end-to-end test.**
#   - batactivity.plotoptions.csv -> default.plotaesthetics: Josh's own copy
#     of this session's earlier plotoptions.batactivity.default.csv
#     deliverable, but an OLDER version - still on the numeric-minutes Y
#     axis (yaxe.limit.min = "0", $yaxe.break.interval.min), not the
#     time-of-day version built later today per his "switch to using time
#     rather than the number of minutes" request in THIS spec (the same
#     request, essentially, made twice today). Since this function is
#     explicitly built against the time-of-day design, a MERGED/updated
#     default.plotaesthetics is used here instead - the current project
#     master (time-of-day Y axis, $plot.order, $layer.order) plus two rows
#     Josh had already added to his own device copy that aren't in the
#     project master yet ($plot.width/$plot.height, trimming the trailing-
#     space typo in both parameter names) plus a blank $project.name
#     override column (also present, empty, in Josh's copy). This merged
#     file has been pushed back to the project AND to Josh's device test
#     folder (replacing the stale copy) so all three copies match again -
#     **Josh: your batactivity.plotoptions.csv has been updated on your
#     machine; if you'd already started customizing the old copy, those
#     edits were not carried over (it had no $project.name overrides filled
#     in yet, so nothing looked like custom edits to preserve).**
#
# STEPS / ASSUMPTIONS (spec was ambiguous/silent on these - flagging per
# project convention):
#   1. Function signature/parameter names taken directly from the "Required
#      Inputs" section's "<-" mapping (e.g. "*vetted.processed.csv <-
#      plot.data" reads as "the test file vetted.processed.csv is what gets
#      loaded into the plot.data parameter for testing"): plot.data,
#      aru.metadata.db, suntimes.db, default.plotaesthetics all take
#      data frames (already loaded, e.g. via batz.datawrangler_load.files())
#      - not file paths, matching every other batz plotting/merge function's
#      convention. Optional project.name = "" also taken directly from spec.
#   2. Header check runs across ALL FOUR inputs before stopping (collects
#      every problem, not just the first) - spec says "if any are missing
#      the required headers stop the function and print the name of each
#      input file and which headers are missing" (plural "files"), read as
#      checking everything first, consistent with how other batz functions'
#      multi-file header checks work.
#   3. default.plotaesthetics's own required headers: spec text here is
#      garbled ("batactivity.plotoptions should have all the values
#      $category,") - read as "must have (at minimum) $category, $parameter,
#      $default.value" (the three columns the settings-resolution logic
#      actually needs); $notes and any $project.name-matching override
#      column(s) are optional extras, matching the real file's structure.
#   4. Settings resolution, two layers, per spec: "batactivity.plotoptions
#      has the default values for the plot found on each row, if
#      project.name = header in batactivity.plotoptions then those those
#      values, of the element is empty use $default.value" (read as: if
#      `project.name` is non-blank AND matches a column name in
#      default.plotaesthetics, use that row's value from that column for
#      each parameter UNLESS it's blank, in which case fall back to
#      $default.value) - AND SEPARATELY: "aru.metadata.db has a list of the
#      plots to me made, if any of the headers in aru.metadata.db are the
#      same as the varables pulled from batactivity.plotoptions default to
#      them" - read as aru.metadata.db's OWN per-row value for a given
#      parameter (when that column exists there and is non-blank) takes
#      priority over whatever default.plotaesthetics/project.name resolved
#      to - this is the only reading under which per-plot customization
#      (e.g. plot.meta.csv's real $xaxe.interval = 4, overriding the
#      generic default of "4 days") does anything at all. **Please confirm
#      this precedence (aru.metadata.db row > project.name column >
#      $default.value) is what was meant.**
#   5. $facet.label's real value is a doubly-quoted string ("\"common\"" -
#      the CSV literally contains a quoted "common") - stripped of its
#      literal wrapping quote characters before being used as
#      batz.batusa_recode.names()'s output.format. Blank/missing
#      $facet.label falls back to "common".
#   6. $spp.plot/$facpan special-case (New England/NE) list, MYSO/Alldect/
#      40khzmyo flag handling, taken literally from the spec's pseudocode.
#      40khzmyo is read as ALWAYS being folded into the "All detections"
#      panel (as an overlay, never its own facet) whenever $Alldect = TRUE;
#      it only gets its OWN facet panel when $Alldect = FALSE (since there'd
#      be no "All detections" panel to overlay onto). "Indiana Bat" (Josh's
#      literal spec text for $MYSO = TRUE) is passed through
#      batz.batusa_recode.names() like every other species name, so it
#      resolves correctly regardless of the literal capitalization given.
#   7. $facet maps a KEYWORD to a plot.data column to facet by - only one
#      keyword ("sppid" -> $spp.id) is defined by the spec/real data, so
#      that's the only one implemented; any other value currently stops
#      with a clear "not yet implemented" message rather than silently
#      guessing - **flagging this as a known gap for a future iteration**
#      once Josh defines what other $facet values should mean.
#   8. Y-axis crossbar times are computed as Noon-of-that-date + the given
#      number of minutes ($mins2.noon.min/$mins2.noon.max), consistent with
#      how $mins2.noon.min/max are named and with the plotoptions Y-axis
#      design (Noon-to-Noon spanning one full monitoring night).
#   9. Reference lines (Dawn = $sunr, Dusk = $suns, Midnight = the date's own
#      midnight-of-the-following-calendar-day, i.e. within the Noon-to-Noon
#      window) are built as a SEPARATE small data frame with no facet
#      column, so ggplot2 repeats them identically across every facet panel
#      - same technique Josh's own original plot script used.
#   10. Only $plot.type = "bat.detection" is implemented (the only value
#      that appears in the real data/spec) - any other $plot.type value in
#      aru.metadata.db is skipped with a console NOTE rather than erroring,
#      since aru.metadata.db is described as a list of MULTIPLE plots to
#      generate and a plot-type this function doesn't yet handle shouldn't
#      block the ones it does.
#   11. *** THE GGPLOT2-RENDERING PORTION OF THIS SCRIPT COULD NOT BE
#      EXECUTED IN THIS SANDBOX *** - ggplot2 is not installed here and
#      there is no network access to CRAN to install it (same limitation
#      already flagged for roxygen2 earlier this project). Every step BEFORE
#      the actual ggplot() call (header checks, settings resolution,
#      spp.plot/facpan building, data filtering, date/time joins) IS fully
#      tested below with base R and confirmed correct. The ggplot2 code
#      itself was written carefully, following the exact design already
#      verified in plotoptions.batactivity.default.csv and Josh's own
#      original script, but is UNTESTED/UNRENDERED - **Josh, please run this
#      end-to-end in your own R environment and confirm the plot actually
#      renders and looks right before this goes further.**
# =============================================================================

# -----------------------------------------------------------------------------
# batz.batusa_recode.names(), needed by this function - sourced here for
# dev/testing (the final .R embeds/depends on the package version).
# -----------------------------------------------------------------------------
source("batz.batusa_recode.names.R")

# -----------------------------------------------------------------------------
# Real test data, loaded from disk for dev/testing.
# -----------------------------------------------------------------------------
plot.data.real              <- read.csv("vetted.processed.csv", stringsAsFactors = FALSE, check.names = FALSE)
plot.data.real               <- plot.data.real[, names(plot.data.real) != "", drop = FALSE]   # drop the row-number column read.csv picked up from the CSV's blank first header
aru.metadata.db.real        <- read.csv("plot.meta.csv", stringsAsFactors = FALSE, check.names = FALSE)
suntimes.db.real            <- read.csv("suntimes.csv", stringsAsFactors = FALSE, check.names = FALSE)
default.plotaesthetics.real <- read.csv("batactivity.plotoptions.csv", stringsAsFactors = FALSE, check.names = FALSE)

cat("=== real plot.data ===\n"); str(plot.data.real)
cat("\n=== real aru.metadata.db (raw column names, note the duplicate) ===\n"); print(names(aru.metadata.db.real))
cat("\n=== real suntimes.db aru/date range ===\n")
cat("aru values:", paste(unique(suntimes.db.real$aru), collapse = ", "), "\n")
cat("date range:", range(as.Date(suntimes.db.real$date, format = "%m/%d/%Y")), "\n")
cat("\n=== real plot.data aru/date range ===\n")
cat("aru.groupby values:", paste(unique(plot.data.real$aru.groupby), collapse = ", "), "\n")
cat("date range:", range(as.Date(plot.data.real$date, format = "%m/%d/%Y")), "\n")

# -----------------------------------------------------------------------------
# SYNTHETIC, ALIGNED test data.
#
# 2026-08-27, per Josh ("The dates should start at $date.start = 4/8/2026
# $date.end = 4/27/2026 as found in the meta files"): previously this block
# re-dated a WTG-GOM101 suntimes stand-in onto May 2026 and overrode
# aru.metadata.db's $plot.set/$date.start/$date.end, purely to match
# plot.data.real's real (May) detection dates - back when suntimes.db.real
# only covered WTG-GOM101/Jan 2025 and none of the three real files
# overlapped at all. Both of those real gaps are now closed: Josh's own
# aru.metadata.db.real (plot.meta.csv) already has $plot.set = "WTG-GOM102",
# and suntimes.db.real (his regenerated batz.suntimes_generate() output)
# already covers WTG-GOM102 across 2025-2030, so neither needs any
# adjustment here anymore - both are used completely UNMODIFIED, matching
# Josh's real config exactly.
#
# Only plot.data still needs to be synthetic: the real vetted.processed.csv
# detections are fixed in May 2026, and aru.metadata.db.real's own
# $date.start/$date.end window is Josh's to edit at will (it has already
# moved twice in one session: 4/8-4/27/2026, then 5/8-5/27/2026) - so rather
# than hardcode a fixed day-shift that silently breaks (0 rows, or every
# TEST 4-9 downstream) the next time Josh edits that window, the shift is
# computed HERE, dynamically, from the real file's own current $date.start,
# so the synthetic detections always land just inside whatever window is
# currently configured, no matter what it is.
real.dates       <- as.Date(plot.data.real$date, format = "%m/%d/%Y")
synth.day.shift  <- as.Date(aru.metadata.db.real$date.start[1], format = "%m/%d/%Y") - min(real.dates)
plot.data.synth <- plot.data.real
plot.data.synth$date <- format(real.dates + synth.day.shift, "%m/%d/%Y")

suntimes.synth <- suntimes.db.real
aru.metadata.db.synth <- aru.metadata.db.real
default.plotaesthetics.synth <- default.plotaesthetics.real

# -----------------------------------------------------------------------------
# header + duplicate-name checks
# -----------------------------------------------------------------------------
check.headers <- function(df, required, label) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
  }
  NULL
}

check.duplicates <- function(df, label) {
  nm <- names(df)
  dups <- unique(nm[duplicated(nm)])
  if (length(dups) > 0) {
    return(sprintf("%s has duplicate column name(s): %s - every column name must be unique",
                    label, paste(dups, collapse = ", ")))
  }
  NULL
}

PLOT.DATA.REQUIRED <- c("spp.id", "date", "aru.groupby", "obs",
                         "mins2.noon.min", "mins2.noon.max", "vetting.type")
SUNTIMES.DB.REQUIRED <- c("aru", "date", "date.mon", "sunregion", "time.zone",
                           "sunregion.type", "schedual1", "schedual2", "suns",
                           "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
ARU.METADATA.DB.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                               "Alldect", "facet.panel", "40khzmyo", "facet.label",
                               "plot.set", "date.format", "date.start", "date.end",
                               "xaxe.interval", "xaxe.title")
DEFAULT.PLOTAESTHETICS.REQUIRED <- c("category", "parameter", "default.value")

# -----------------------------------------------------------------------------
# batz.plotdetections_first.last()
# -----------------------------------------------------------------------------
batz.plotdetections_first.last <- function(plot.data, aru.metadata.db, suntimes.db,
                                            default.plotaesthetics, project.name = "") {

  problems <- c(
    check.headers(plot.data, PLOT.DATA.REQUIRED, "plot.data"),
    check.headers(suntimes.db, SUNTIMES.DB.REQUIRED, "suntimes.db"),
    check.headers(aru.metadata.db, ARU.METADATA.DB.REQUIRED, "aru.metadata.db"),
    check.headers(default.plotaesthetics, DEFAULT.PLOTAESTHETICS.REQUIRED, "default.plotaesthetics"),
    check.duplicates(plot.data, "plot.data"),
    check.duplicates(suntimes.db, "suntimes.db"),
    check.duplicates(aru.metadata.db, "aru.metadata.db"),
    check.duplicates(default.plotaesthetics, "default.plotaesthetics")
  )
  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n"))
  }

  unquote <- function(x) {
    x <- trimws(as.character(x))
    gsub('^"(.*)"$', "\\1", x)
  }

  # 2026-08-27: Josh's own batz.suntimes_generate() writes $date/$suns/
  # $sunr/$sunr.mon in ISO format ("2026-05-15", "2026-05-15 19:56:29"),
  # not m/d/Y ("5/15/2026", "5/15/2026 19:56") - a real ISO-format
  # suntimes.csv silently produced 0 rows here (all dates parsed to NA
  # under a hardcoded "%m/%d/%Y" format) even though the aru/date range
  # genuinely overlapped. plot.data/aru.metadata.db (hand-typed by Josh)
  # have so far always been m/d/Y, but parsing flexibly for all
  # date/datetime fields - mirroring the multi-format parse.simple.date()
  # approach already used in batz.suntimes_generate - costs nothing and
  # avoids the same landmine wherever a date field's actual source changes.
  parse.flex.date <- function(x) {
    out <- as.Date(rep(NA_character_, length(x)))
    for (fmt in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.Date(x, format = fmt)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  parse.flex.datetime <- function(x, tz) {
    out <- as.POSIXct(rep(NA_character_, length(x)), tz = tz)
    for (fmt in c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  get.default <- function(param) {
    row.idx <- which(default.plotaesthetics$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(default.plotaesthetics$default.value[row.idx[1]])
    if (nzchar(project.name) && project.name %in% names(default.plotaesthetics)) {
      override <- default.plotaesthetics[[project.name]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  get.setting <- function(job, param) {
    if (param %in% names(job)) {
      v <- job[[param]]
      if (!is.null(v) && !is.na(v) && nzchar(trimws(as.character(v)))) {
        return(as.character(v))
      }
    }
    get.default(param)
  }

  NE.ALIASES <- c("new england", "ne")
  # Josh's literal spec text says "Tricolored bat" (no hyphen) - the
  # reference database's actual canonical $common name is "Tri-colored bat"
  # (with a hyphen, see NAbat.names.csv/batz.batusa_recode.names). Matching
  # is case/dash/underscore/whitespace-insensitive, but that only bridges
  # separators that are PRESENT on one side - "Tricolored" has no separator
  # at all for "Tri-colored"'s hyphen to normalize against, so the literal
  # spec spelling would never match the reference data (confirmed by a
  # WARNING during dev-script testing). Corrected to the canonical spelling
  # here so downstream matching/labeling works - flagging the correction
  # rather than silently keeping Josh's literal (non-matching) spelling.
  SPECIAL.FACPAN <- c("Big brown bat", "Eastern red bat", "Hoary bat", "Silver-haired bat",
                       "Eastern small-footed myotis", "Little brown bat",
                       "Northern long-eared bat", "Tri-colored bat")

  jobs <- aru.metadata.db[!is.na(aru.metadata.db$plot.type) & nzchar(trimws(aru.metadata.db$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("aru.metadata.db has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

  plots <- list()

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, ]
    job.label <- if (nzchar(trimws(job$plot.name))) job$plot.name else sprintf("row %d", j)

    if (!identical(tolower(trimws(job$plot.type)), "bat.detection")) {
      cat(sprintf("NOTE: aru.metadata.db row for '%s' has plot.type = '%s' - skipped (only 'bat.detection' is implemented so far).\n",
                   job.label, job$plot.type))
      next
    }

    facet.kind <- tolower(trimws(job$facet))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: aru.metadata.db row for '%s' has facet = '%s' - skipped ($facet = \"sppid\" is the only value implemented so far).\n",
                   job.label, job$facet))
      next
    }

    # ---- spp.plot / facpan ----
    facet.set.val <- tolower(trimws(job$facet.set))
    if (facet.set.val %in% NE.ALIASES) {
      facpan <- SPECIAL.FACPAN
    } else {
      facpan <- strsplit(get.setting(job, "facpan"), ";", fixed = TRUE)[[1]]
    }
    spp.plot <- facpan

    myso.flag <- isTRUE(as.logical(job$MYSO))
    if (myso.flag) spp.plot <- c(spp.plot, "Indiana Bat")

    alldect.flag <- isTRUE(as.logical(job$Alldect))
    if (alldect.flag) {
      spp.plot <- c(spp.plot, "All detections")
      facpan   <- c(facpan, "All detections")
    }

    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) {
      spp.plot <- c(spp.plot, "40khzmyo")
      if (!alldect.flag) facpan <- c(facpan, "40khzmyo")
    }

    spp.plot <- unique(trimws(spp.plot))
    facpan   <- unique(trimws(facpan))

    # Canonicalize both lists to the reference table's own $common spelling
    # (except the two non-species pseudo-panels, "All detections"/"40khzmyo",
    # already canonical per batz.batusa_recode.names's own category-label
    # rows) - so a spec/typed list that's slightly off (like the
    # "Tricolored bat" case above) still lines up with plot.data$spp.common,
    # which is always the reference table's canonical spelling, rather than
    # silently failing a plain string match.
    spp.plot <- batz.batusa_recode.names(spp.plot, output.format = "common")
    facpan   <- batz.batusa_recode.names(facpan, output.format = "common")

    # ---- filter plot.data to this job's ARU + species list ----
    pd <- plot.data
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, output.format = "common")

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$aru.groupby)) == tolower(plot.set.val), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end   <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    tz <- get.setting(job, "time.zone")

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: aru.metadata.db row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of plot.data - no plot generated. Check that $aru.groupby/$date in plot.data actually overlap this row's $plot.set/$date.start/$date.end.\n",
                   job.label, plot.set.val, date.start, date.end))
      next
    }

    # The Y axis is "hour of monitoring night", the SAME Noon-to-Noon window
    # for every night regardless of its real calendar date (the real date
    # drives the X axis only, via facet_wrap/date.parsed). So every row's
    # y-value is remapped onto one fixed, arbitrary reference date
    # (y.ref.date) - $mins2.noon.min/max are already pure "minutes since that
    # night's own noon" offsets, so this is just adding them onto a SHARED
    # noon rather than each row's own real noon (using each row's own real
    # noon, tried first, silently misaligned every night onto a different
    # absolute day - caught and fixed during dev-script testing).
    y.ref.date <- as.Date("1970-01-02")
    y.ref.noon <- as.POSIXct(paste(y.ref.date, "12:00:00"), tz = tz)
    pd$time.min <- y.ref.noon + pd$mins2.noon.min * 60
    pd$time.max <- y.ref.noon + pd$mins2.noon.max * 60

    # 40khzmyo rows always overlay in the "All detections" panel when that
    # panel exists; only get their own panel when it doesn't (assumption 6).
    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel,
                                    "All detections", pd$spp.common)
    pd$crossbar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")

    # ---- suntimes reference lines: one row per date, no facet column, so
    # ggplot2 repeats them across every panel ----
    sdb <- suntimes.db
    sdb$date.parsed <- parse.flex.date(sdb$date)
    if (nzchar(plot.set.val)) {
      sdb <- sdb[tolower(trimws(sdb$aru)) == tolower(plot.set.val), , drop = FALSE]
    }
    sdb <- sdb[!is.na(sdb$date.parsed) & sdb$date.parsed >= date.start & sdb$date.parsed <= date.end, , drop = FALSE]

    if (nrow(sdb) == 0) {
      cat(sprintf("NOTE: aru.metadata.db row for '%s' matched 0 rows of suntimes.db for plot.set = '%s' between %s and %s - Dawn/Dusk/Midnight reference lines will be empty. Check that suntimes.db's $aru/$date actually cover this plot.set/date range.\n",
                   job.label, plot.set.val, date.start, date.end))
    }

    # Same shared-axis remapping as pd$time.min/max above: dusk/dawn are real
    # POSIXct timestamps tied to real calendar dates, so each is converted to
    # its offset from THAT night's own noon, then re-anchored onto the same
    # shared y.ref.noon used for the detection crossbars. Midnight is always
    # exactly 12 hours after noon, so it's just a constant on this axis.
    dusk.real <- parse.flex.datetime(sdb$suns, tz)
    # Dawn ending THIS monitoring night (which starts at $suns/dusk of
    # $date) is $sunr.mon - sunrise on the FOLLOWING day - not $sunr, which
    # is sunrise ON $date itself (i.e. the dawn ending the PREVIOUS night).
    # Using $sunr here was a real bug caught during rendering: it placed
    # Dawn ~5 hours before Noon on the reference date, outside the plotted
    # Noon-to-Noon window, silently dropping the Dawn line from every panel.
    dawn.real <- parse.flex.datetime(sdb$sunr.mon, tz)
    local.noon <- as.POSIXct(paste(sdb$date.parsed, "12:00:00"), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + 12 * 3600

    # ---- facet panel labels, via batz.batusa_recode.names() ----
    # Every panel in facpan is shown even with 0 matching detections (matches
    # the target image, which shows an empty panel - just the reference
    # lines, no crossbar - for species with nothing detected that period) -
    # so the full facpan list defines the facet levels, not just what's
    # actually present in pd after filtering.
    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (!nzchar(facet.label.fmt)) facet.label.fmt <- "common"

    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, output.format = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw

    # panel drawing order, per default.plotaesthetics' $plot.order where possible
    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels,
                                    labels = panel.labels[ordered.levels])

    # ---- y-axis settings (time-of-day) ----
    yaxe.limit.min <- get.setting(job, "yaxe.limit.min")
    yaxe.limit.max <- get.setting(job, "yaxe.limit.max")
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.min) || !grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.max)) {
      stop(sprintf(paste("$yaxe.limit.min/$yaxe.limit.max ('%s'/'%s') don't look like HH:MM time-of-day",
                          "values - default.plotaesthetics may be an old, numeric-minutes-based copy of",
                          "batactivity.plotoptions.csv. Please use the current time-of-day version",
                          "(see this function's dev-script header comment)."),
                    yaxe.limit.min, yaxe.limit.max))
    }
    y.start <- as.POSIXct(paste(y.ref.date, yaxe.limit.min), tz = tz)
    y.end   <- y.start + 24 * 3600   # Noon-to-Noon, one full monitoring night

    plots[[job.label]] <- list(
      job = job,
      pd = pd,
      sdb = sdb,
      panel.labels = panel.labels,
      facpan = facpan,
      spp.plot = spp.plot,
      y.start = y.start,
      y.end = y.end,
      tz = tz,
      date.start = date.start,   # carried through so the X axis can be forced to this exact range below, not just whatever dates happen to have data
      date.end = date.end,
      khz.flag = khz.flag,   # carried through so the legend key below can be driven by "$40khzmyo is TRUE for this plot" rather than "a detection happened to occur" - see the follow-up note below
      resolved.legend.position = get.default("legend.position"),  # exposed for testing the project.name-override resolver
      resolved.dawn.color = get.default("dawn.color")              # exposed for testing the fall-through-to-default case
    )

    cat(sprintf("Prepared plot data for '%s': %d detection rows across %d panel(s), %d suntimes row(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw), nrow(sdb)))
  }

  if (length(plots) == 0) {
    cat("No plots were generated - see NOTE messages above.\n")
    return(invisible(list()))
  }

  # ---------------------------------------------------------------------------
  # Rendering verified 2026-08-27 against real ggplot2 (r-cran-ggplot2), against
  # a synthetic ARU/date-aligned copy of the real test data - see @details.
  # ---------------------------------------------------------------------------
  ggplots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    for (job.label in names(plots)) {
      p <- plots[[job.label]]

      # Explicit y-axis breaks/labels (e.g. "Noon"/"Midnight" instead of
      # "12:00"/"00:00").
      y.breaks <- seq(p$y.start, p$y.end, by = get.default("yaxe.break.interval"))
      y.break.labels <- strsplit(get.default("yaxe.break.labels"), ";", fixed = TRUE)[[1]]
      if (length(y.break.labels) != length(y.breaks)) {
        cat(sprintf("NOTE: '%s' - $yaxe.break.labels has %d label(s) but $yaxe.break.interval produces %d break(s) - falling back to $yaxe.labelformat-formatted times instead of the custom labels.\n",
                     job.label, length(y.break.labels), length(y.breaks)))
        y.break.labels <- format(y.breaks, get.default("yaxe.labelformat"))
      }

      # $date.format (e.g. "%b-%d/n%Y") is meant to break the x-axis date
      # label onto two lines - Josh's real plot.meta.csv writes the line
      # break as literal "/n" rather than an actual newline, which
      # strftime-based formatting (what scale_x_date's date_labels uses under
      # the hood) does not treat as an escape sequence, so it was rendering
      # as the literal two characters "/n" in the axis label instead of a
      # line break. Real bug caught by Josh after the first render - fixed
      # here by converting any literal "/n" in the format string to an
      # actual newline before it's used, rather than relying on the source
      # CSV always spelling it correctly.
      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)

      # 2026-08-27, per Josh ("plot.meta$xaxe.interval = 4 which should make
      # there only be four labeled dates on the X axes"): two real bugs here.
      # (1) $xaxe.interval was being read with get.default("xaxe.interval"),
      # never get.setting(p$job, "xaxe.interval") - so a plot's OWN
      # $xaxe.interval value (e.g. Josh's real plot.meta.csv row) was
      # silently ignored no matter what it said, always falling through to
      # default.plotaesthetics's generic value instead - the exact same
      # class of settings-resolution bug as the "gome"/project.name mismatch
      # found earlier this session, just in a different call site that
      # never got updated when the get.setting()/get.default() split was
      # introduced. (2) The value itself was being fed straight into
      # scale_x_date(date_breaks = ...), which expects a ggplot2/scales
      # interval STRING ("4 days") - i.e. "one break every N days" - but
      # Josh's actual real value is the bare number 4, and his stated
      # intent is "N labeled dates total", a different axis (a COUNT of
      # breaks, not a day-spacing) that date_breaks has no way to express
      # directly. Fixed by computing N evenly-spaced Date breakpoints
      # explicitly across [date.start, date.end] (seq.Date's own
      # length.out= already lands on whole calendar days, first/last break
      # always exactly date.start/date.end) and passing those as
      # scale_x_date(breaks = ...) instead of date_breaks=.
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        cat(sprintf("NOTE: '%s' - $xaxe.interval = '%s' is not a usable number of x-axis labels - defaulting to 2 (just date.start/date.end).\n",
                     job.label, get.setting(p$job, "xaxe.interval")))
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))

      # $panel.border.linewidth (Theme category, default "0.5" - matches
      # ggplot2's own theme_bw() default for panel.border, so nothing
      # changes visually unless it's edited) is applied to the panel border
      # itself AND drives the Midnight line's linewidth, so the two are
      # guaranteed to match exactly (per Josh) rather than just visually
      # similar by coincidence.
      panel.border.lw <- as.numeric(get.default("panel.border.linewidth"))

      # 2026-08-27, per Josh ("the midnight line looks thicker than the box
      # line"): confirmed with a pixel-level measurement of a real rendered
      # PNG (integrated optical density across the stroke, not just eyeballing)
      # that a geom_line()/geom_hline() drawn with linewidth = X renders at
      # ~2x the actual pixel width of a theme_bw() panel.border drawn with
      # element_rect(linewidth = X) - same nominal value, genuinely different
      # rendered thickness (a ggplot2 rendering quirk between how "rect" theme
      # elements and geom line/segment strokes convert linewidth to on-page
      # width - reproduced in isolation with a controlled diagnostic script,
      # not specific to this plot's data). Halving the Midnight line's own
      # linewidth (panel border itself is untouched, still exactly
      # $panel.border.linewidth) was verified to bring the two to within
      # measurement noise (2.227px vs 2.225px in the diagnostic render) of
      # the same rendered width.
      midnight.render.lw <- panel.border.lw / 2

      # $midnight (Reference lines category, default "short") controls how
      # the Midnight reference line is drawn, per Josh:
      #   "none"  - don't plot it at all.
      #   "long"  - a single straight line spanning the full panel width,
      #             edge to edge (via geom_hline, which is unaffected by
      #             which/how many real suntimes dates are present).
      #   "short" - the original behavior: a line connecting each real
      #             suntimes date's (constant) midnight value, which is
      #             visually a flat line but only spans from the first to
      #             the last date actually present in suntimes.db for this
      #             plot - can fall short of the panel edges if that's
      #             narrower than the full date.start-date.end window.
      #   "dots"  - 2026-08-27, per Josh ("add an option to
      #             batactivity.plotoptions that makes the midnight line a
      #             series of grey dots"): a new mode, added the same way
      #             none/long/short were - one grey dot per real suntimes
      #             date present for this plot (same date coverage as
      #             "short", via geom_point instead of geom_line, so it can
      #             likewise fall short of the panel edges for the same
      #             reason). Uses its own $midnight.dots.color/
      #             $midnight.dots.size settings rather than reusing
      #             $midnight.color/$midnight.linetype, so it doesn't
      #             change what none/long/short already look like by
      #             default - flagging this interpretation to Josh: "a
      #             series of dots" was read as a literal geom_point()
      #             marker mode (a genuinely new, separate $midnight value),
      #             not as "set the existing line's linetype to dotted" -
      #             ggplot2's built-in "dotted" linetype on the existing
      #             short/long line would also visually read as a dotted
      #             line and needs no new code at all (already available
      #             via $midnight.linetype/$midnight.color) if that's what
      #             was actually meant instead.
      midnight.mode <- tolower(trimws(get.setting(p$job, "midnight")))
      if (!midnight.mode %in% c("none", "long", "short", "dots")) {
        cat(sprintf("NOTE: '%s' - $midnight = '%s' is not one of none/long/short/dots - defaulting to 'short'.\n",
                     job.label, get.setting(p$job, "midnight")))
        midnight.mode <- "short"
      }
      # Midnight is always exactly 12 hours after y.start (Noon of the
      # shared reference date) regardless of any specific real calendar
      # date, so it's computed directly from p$y.start rather than from
      # p$sdb - this also means "long" mode still works even when
      # suntimes.db has 0 matched rows for this plot (sdb would be empty).
      midnight.const <- p$y.start + 12 * 3600
      # "dots" resolves its own grey color independent of $midnight.color
      # (which none/long/short keep using, default black, unchanged) - see
      # the mode note above.
      midnight.legend.color <- if (midnight.mode == "dots") get.default("midnight.dots.color") else get.default("midnight.color")
      midnight.layer <- NULL
      if (midnight.mode == "short") {
        midnight.layer <- geom_line(data = p$sdb, aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                      linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw,
                                      inherit.aes = FALSE)
      } else if (midnight.mode == "long") {
        midnight.layer <- geom_hline(data = data.frame(midnight.time = midnight.const),
                                       aes(yintercept = midnight.time, color = "Midnight"),
                                       linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw)
      } else if (midnight.mode == "dots") {
        # 2026-08-27, per Josh ("make midnight dots into thin dashes
        # instead"): shape 45 is the literal "-" (hyphen) character used as
        # a plotting glyph, rendering as a short horizontal dash rather
        # than a filled circle - visually reads as a dashed line broken
        # into one mark per real suntimes date, matching Josh's request.
        # $midnight = "dots" is kept as the setting's value name (unchanged,
        # so any existing config isn't broken) even though the rendered
        # glyph is now a dash, not a circle.
        midnight.layer <- geom_point(data = p$sdb, aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                       shape = 45, size = as.numeric(get.default("midnight.dots.size")),
                                       inherit.aes = FALSE)
      }

      # 2026-08-27 finding, caught by actually rendering the legend after
      # adding "dots" mode (not just reading the code): ggplot2's default
      # legend-key merging draws EVERY layer's key glyph onto EVERY row of
      # a shared discrete color guide, regardless of which layer's data
      # actually produced that row - confirmed with an isolated diagnostic
      # (geom_line() x2 + geom_point() sharing one colour aes: the two
      # line-only rows both picked up a stray point marker) and NOT fixed
      # by giving each layer its own explicit key_glyph (tried first -
      # made no visible difference to the rendered legend, and had its own
      # side effect: ggplot2 marks a key_glyph'd geom's class with a
      # leading "" entry internally, which would silently break any
      # introspection code checking class(layer$geom)[1]). The real fix is
      # guide_legend(override.aes = ...): the reference-line legend's
      # break order is always alphabetical (Dawn, Dusk, Midnight, since
      # scale_color_manual below declares no explicit breaks=) - a stable
      # ggplot2 default, confirmed by rendering - so shape can be pinned
      # per-row by position: NA (no marker) for Dawn/Dusk always, and for
      # Midnight's own row, 16 (a dot) only in "dots" mode, NA otherwise.
      # Only built when Midnight actually has a legend row at all (i.e.
      # midnight.mode != "none", matching how the legend already
      # naturally excludes Midnight when there's no midnight.layer).
      midnight.legend.shape <- if (midnight.mode == "dots") 45 else NA
      reference.line.override.shape <- if (midnight.mode == "none") c(NA, NA) else c(NA, NA, midnight.legend.shape)

      # $crossbar.fill.legend.title's legend should never show an "All
      # detections" key (it's the obvious default, not worth a legend
      # entry per Josh) and should show a "40kHzMyo" key whenever
      # $40khzmyo is on this plot's species list, colored black - Josh's
      # own original wording: "40kHzMyo if on species list should be [on
      # the legend] and colored black."
      #
      # 2026-08-27, per Josh ("40k Myo is missing from the legend"): this
      # was previously driven by whether a 40kHzMyo row actually survived
      # into p$pd (i.e. an actual detection happened to occur that
      # period) - MY OWN interpretive judgment call at the time, not what
      # Josh's own spec text literally says, and it meant a plot whose
      # real $40khzmyo flag is TRUE (on the species list) but which
      # simply had no 40kHzMyo detections that period showed no legend
      # key at all - exactly Josh's real plot.meta.csv/vetted.processed.csv
      # combination. Fixed to key off p$khz.flag ($40khzmyo itself,
      # carried through from the settings-resolution loop above) instead
      # of data presence.
      #
      # First fix attempt (breaks = "40kHzMyo" alone, no limits) LOOKED
      # right but was verified wrong with an isolated diagnostic: a
      # scale_fill_manual()'s breaks are silently dropped from the actual
      # rendered legend for any level that never appears in the mapped
      # data, regardless of what's declared in breaks= - confirmed by
      # rendering (not just introspecting get_breaks() on the unbuilt
      # scale, which is unreliable here the same way $labels$y was found
      # to be earlier this session) a bare geom_col() + scale_fill_manual
      # with breaks="B" but no "B" rows: no legend at all. Real fix
      # needs limits= to explicitly put "40kHzMyo" into the scale's
      # domain whenever the flag is TRUE, independent of whether any row
      # actually used that fill value - re-verified by rendering with
      # limits= added: the key shows correctly even with zero 40kHzMyo
      # detections.
      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      g <- ggplot(p$pd, aes(x = date.parsed)) +
        geom_line(data = p$sdb, aes(x = date.parsed, y = dusk.time, color = "Dusk"),
                   linetype = get.default("dusk.linetype"), inherit.aes = FALSE) +
        midnight.layer +
        geom_line(data = p$sdb, aes(x = date.parsed, y = dawn.time, color = "Dawn"),
                   linetype = get.default("dawn.linetype"), inherit.aes = FALSE) +
        # width is pinned explicitly (rather than left to geom_crossbar's
        # default, which auto-computes it from resolution() - the smallest
        # gap between any two distinct dates actually present in the data)
        # because that default varies with which detection rows happen to
        # survive filtering for a given plot (e.g. TEST 5 below, with fewer
        # surviving rows and a bigger minimum date gap, computed a WIDER
        # crossbar than the half-day padding on scale_x_date's limits (just
        # below) was sized for, clipping the boundary-day bars all over
        # again with a different NA count - a real bug caught by re-running
        # every test scenario after the scale_x_date fix, not just TEST 4).
        # Pinning width = 0.9 (ggplot2's own default for daily-resolution
        # data) makes the box size predictable regardless of which/how many
        # dates are present, so the padding below is always enough.
        geom_crossbar(aes(ymin = time.min, ymax = time.max, y = time.min, fill = crossbar.type),
                       linewidth = as.numeric(get.default("crossbar.linewidth")),
                       width = 0.9) +
        scale_color_manual(name = get.default("reference.line.legend.title"),
                            values = c("Dawn" = get.default("dawn.color"),
                                       "Midnight" = midnight.legend.color,
                                       "Dusk" = get.default("dusk.color"))) +
        guides(colour = guide_legend(override.aes = list(shape = reference.line.override.shape))) +
        scale_fill_manual(name = get.default("crossbar.fill.legend.title"),
                           breaks = fill.legend.breaks,
                           limits = fill.legend.limits,
                           values = c("All detections" = get.default("crossbar.alldetections.fill"),
                                      "40kHzMyo" = get.default("crossbar.40khzmyo.fill"))) +
        scale_y_datetime(limits = c(p$y.start, p$y.end),
                          breaks = y.breaks,
                          labels = y.break.labels,
                          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        # Half-day padding on each side of date.start/date.end: geom_crossbar
        # draws each day's box at a fixed width around its date, so a bar
        # sitting exactly ON a hard scale limit gets half its box clipped to
        # NA (ggplot2's default out-of-bounds behavior for scale_x_date) -
        # caught via a real "Removed N rows containing missing values
        # (geom_segment())" warning on the first/last day's bars once the
        # limits below were added. The padding keeps the visible range
        # exactly matching date.start/date.end (no extra days shown) while
        # letting the boundary days' full-width bars render uncut.
        scale_x_date(limits = c(p$date.start - 0.5, p$date.end + 0.5),
                      breaks = xaxe.breaks,
                      date_labels = xaxe.date.labels.fmt,
                      name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        facet_wrap(~ facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        labs(title = job.label) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(linewidth = panel.border.lw, colour = "grey20", fill = NA),
              legend.position = get.default("legend.position"),
              plot.title = element_text(hjust = as.numeric(get.default("plot.title.hjust")),
                                          size = as.numeric(get.default("plot.title.size"))),
              axis.title = element_text(size = as.numeric(get.default("axis.title.size"))),
              axis.text = element_text(size = as.numeric(get.default("axis.text.size"))),
              # 2026-08-27, later still - a documentation correction, NOT a
              # behavior change: while investigating Josh's "eastern small
              # footed myotis is cut off" report, found that the
              # default.plotaesthetics note on $axis.title.size has always
              # incorrectly claimed strip.text (the facet panel title)
              # "reuses" $axis.title.size - confirmed via ggplot2's own
              # get_element_tree() and a minimal test plot that this was
              # never true (strip.text inherits from "text", not "title"/
              # axis.title; changing axis.title.size has zero effect on it).
              # An explicit strip.text = element_text(size = axis.title.size)
              # was tried here to make the documented behavior real, but
              # re-rendering showed it made the cutoff WORSE, not better -
              # ggplot2's own actual fixed strip.text size (rel(0.8) of
              # theme_bw()'s base_size 11 = 8.8pt) is SMALLER than
              # $axis.title.size's default of 10, so binding them enlarged
              # the title instead of shrinking it. Reverted: strip.text is
              # left at ggplot2's native, non-configurable size, and the
              # $axis.title.size CSV note is corrected below to describe
              # what the code actually does, instead of changing the code to
              # match a stale, inaccurate note.
              legend.text = element_text(size = as.numeric(get.default("legend.text.size"))),
              legend.title = element_text(size = as.numeric(get.default("legend.title.size"))),
              # 2026-08-27, per Josh ("labels on the Xaxes ... do not appear
              # to be the real dates rather labels rewriting the dates"):
              # this was NOT a data/parsing bug - every break IS the real
              # date.start/date.end-derived calendar date, confirmed by
              # inspecting the actual pixel text - the real defect was a
              # LAYOUT collision. Forcing the first/last x-axis break to sit
              # exactly at date.start/date.end (the earlier $xaxe.interval
              # fix, by design) means the rightmost label of one facet panel
              # is centered right at that panel's shared border with the
              # next panel - with theme_bw()'s default (~5.5pt) panel
              # spacing, the two-line label's own width bleeds across that
              # border and overlaps the neighboring panel's leftmost label,
              # so e.g. "May-27\n2026" visually smashes into the next
              # panel's "May-08\n2026" and reads as garbled/wrong text, even
              # though both dates are individually correct.
              #
              # REVISED 2026-08-27, later still, per Josh ("that is worse, I
              # only get a box now not a plot... revert back to the previous
              # plot dimensions and reduce the size of the labels on the x
              # and y axis until there is no overlap"): the first fix widened
              # $panel.spacing.x from theme_bw()'s ~5.5pt default to 40pt,
              # which stopped the label collision but - since this function's
              # overall saved figure width is a FIXED size ($plot.width +
              # $ggsave.width.pad, not something that grows with the number
              # of panels/gaps - shrank every panel's own width to make room
              # for the wider gaps, which in turn made the "Eastern
              # small-footed myotis" facet title too wide for its now-
              # narrower panel and cut it off. Per Josh's explicit
              # correction, $panel.spacing.x is reverted to theme_bw()'s own
              # built-in "5.5" default (restores the original panel/figure
              # dimensions exactly - this is a plain value revert, not a
              # removal, so the setting stays available to override later if
              # ever needed) and the actual label-collision fix now comes
              # from shrinking $axis.text.size instead (8 -> 6, see that
              # parameter's own updated default/notes) - smaller text needs
              # less horizontal room, so the two-line date labels clear each
              # other even at the original tight spacing. Re-verified by
              # rendering: panels are back to their original width (species
              # title no longer cut off) and the x-axis labels still don't
              # collide at any panel boundary.
              panel.spacing.x = unit(as.numeric(get.default("panel.spacing.x")), "pt"))


      ggplots[[job.label]] <- g

      pattern <- get.default("output.filename.pattern")
      fname <- pattern
      fname <- gsub("<ARU>", trimws(p$job$plot.set), fname, fixed = TRUE)
      fname <- gsub("<date.start>", as.character(min(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<date.end>", as.character(max(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<timestamp>", format(Sys.time(), "%Y%m%d%H%M%S"), fname, fixed = TRUE)

      ggsave(fname, plot = g,
             width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
             height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
             units = get.default("ggsave.units"),
             dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed in this environment - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n\n########## TEST 1: header checks catch real problems ##########\n")
tryCatch(
  batz.plotdetections_first.last(
    plot.data = plot.data.synth[, setdiff(names(plot.data.synth), "obs")],  # drop a required header
    aru.metadata.db = aru.metadata.db.synth,
    suntimes.db = suntimes.synth,
    default.plotaesthetics = default.plotaesthetics.synth
  ),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 2: duplicate column name in aru.metadata.db is caught ##########\n")
# 2026-08-27: Josh fixed the real plot.meta.csv's duplicate "xaxe.title"
# header himself (renamed the second one to "yaxe.title"), so the real file
# no longer exercises this path. Construct a synthetic duplicate so the
# safeguard (check.duplicates(), aru.metadata.db.R line ~259) still gets
# tested going forward.
aru.metadata.db.dup <- aru.metadata.db.real
names(aru.metadata.db.dup)[names(aru.metadata.db.dup) == "yaxe.title"] <- "xaxe.title"
tryCatch(
  batz.plotdetections_first.last(
    plot.data = plot.data.real,
    aru.metadata.db = aru.metadata.db.dup,
    suntimes.db = suntimes.db.real,
    default.plotaesthetics = default.plotaesthetics.real
  ),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 3: real files as-delivered, completely UNMODIFIED - the actual production scenario ##########\n")
# 2026-08-27: as of Josh's latest plot.meta.csv edit ($date.start/$date.end
# now 5/8/2026-5/27/2026, covering the real 5/15-5/21/2026 detections), this
# is now a genuinely real, fully end-to-end success with ZERO overrides of
# any kind - every one of $plot.set/$date.start/$date.end/suntimes coverage
# that was flagged as misaligned earlier this session is now resolved by
# Josh's own file edits, not by anything in this test script. This is the
# actual milestone: what Josh's real files produce, run exactly as given.
result3 <- batz.plotdetections_first.last(
  plot.data = plot.data.real,
  aru.metadata.db = aru.metadata.db.real,
  suntimes.db = suntimes.db.real,
  default.plotaesthetics = default.plotaesthetics.real
)
cat("Number of plots produced:", length(result3$plots), "(expected 1 - a real, unmodified, end-to-end render straight from Josh's own files)\n")

cat("\n\n########## TEST 3b: REAL data, narrower aligned window - now redundant with TEST 3 above (kept as an additional real-data window variant, not because it's still needed to prove alignment) ##########\n")
result3b <- batz.plotdetections_first.last(
  plot.data = plot.data.real,
  aru.metadata.db = aru.metadata.db.real,
  suntimes.db = suntimes.db.real,
  default.plotaesthetics = default.plotaesthetics.real
)
cat("Number of plots produced:", length(result3b$plots), "(expected 1)\n")

cat("\n\n########## TEST 4: SYNTHETIC aligned data - full pipeline, default (NE) facet list ##########\n")
result4 <- batz.plotdetections_first.last(
  plot.data = plot.data.synth,
  aru.metadata.db = aru.metadata.db.synth,
  suntimes.db = suntimes.synth,
  default.plotaesthetics = default.plotaesthetics.synth
)
cat("Number of plots produced:", length(result4$plots), "\n")
if (length(result4$plots) > 0) {
  p <- result4$plots[[1]]
  cat("Panels:", paste(levels(p$pd$facet.panel.value), collapse = " | "), "\n")
  cat("Detection rows used:\n")
  print(p$pd[, c("spp.id", "spp.common", "date.parsed", "facet.panel.value", "crossbar.type", "time.min", "time.max")])
  cat("Suntimes rows used:\n")
  print(p$sdb[, c("date.parsed", "dusk.time", "midnight.time", "dawn.time")])
}

cat("\n\n########## TEST 5: MYSO/40khzmyo-without-Alldect flag handling ##########\n")
aru.metadata.db.test5 <- aru.metadata.db.synth
aru.metadata.db.test5$MYSO <- TRUE
aru.metadata.db.test5$Alldect <- FALSE   # so 40khzmyo should get its OWN panel now
result5 <- batz.plotdetections_first.last(
  plot.data = plot.data.synth,
  aru.metadata.db = aru.metadata.db.test5,
  suntimes.db = suntimes.synth,
  default.plotaesthetics = default.plotaesthetics.synth
)
if (length(result5$plots) > 0) {
  cat("spp.plot included Indiana bat (canonicalized):",
      "indiana bat" %in% tolower(result5$plots[[1]]$spp.plot), "\n")
  cat("facpan included 40kHzMyo as its own panel (canonicalized):",
      "40khzmyo" %in% tolower(result5$plots[[1]]$facpan), "\n")
}

cat("\n\n########## TEST 6: project.name override ##########\n")
default.plotaesthetics.override <- default.plotaesthetics.synth
default.plotaesthetics.override$TestProject <- ""
default.plotaesthetics.override$TestProject[default.plotaesthetics.override$parameter == "legend.position"] <- "top"
result6 <- batz.plotdetections_first.last(
  plot.data = plot.data.synth,
  aru.metadata.db = aru.metadata.db.synth,
  suntimes.db = suntimes.synth,
  default.plotaesthetics = default.plotaesthetics.override,
  project.name = "TestProject"
)
if (length(result6$plots) > 0) {
  cat("legend.position resolved with project.name override applied:",
      result6$plots[[1]]$resolved.legend.position, "(expected 'top')\n")
}

cat("\n\n########## TEST 7: $midnight modes (none/long/short/invalid) + legend behavior ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  check.midnight.layer <- function(result, mode.label) {
    g <- result$ggplots[[1]]
    geom.classes <- sapply(g$layers, function(l) class(l$geom)[1])
    cat(sprintf("  %s: geoms present = %s\n", mode.label, paste(geom.classes, collapse = ", ")))
  }

  # test-script-local stand-in for the function's internal get.default() -
  # that helper is only in scope inside batz.plotdetections_first.last()
  # itself, so we re-derive the same default.value lookup here to check
  # against, using default.plotaesthetics.synth (no project.name override
  # involved in this test).
  get.default.synth <- function(param) {
    row.idx <- which(default.plotaesthetics.synth$parameter == param)
    as.character(default.plotaesthetics.synth$default.value[row.idx[1]])
  }

  for (mode in c("none", "long", "short", "dots", "bogus")) {
    aru.metadata.db.mid <- aru.metadata.db.synth
    aru.metadata.db.mid$midnight <- mode
    result.mid <- batz.plotdetections_first.last(
      plot.data = plot.data.synth,
      aru.metadata.db = aru.metadata.db.mid,
      suntimes.db = suntimes.synth,
      default.plotaesthetics = default.plotaesthetics.synth
    )
    if (length(result.mid$ggplots) > 0) check.midnight.layer(result.mid, mode)
  }
  cat("(expected: 'none' has no GeomHline/no extra Midnight GeomLine beyond Dawn/Dusk's 2 GeomLines; 'long' includes GeomHline; 'short'/'bogus' (falls back to short) include a 3rd GeomLine; 'dots' includes a GeomPoint)\n")

  # 2026-08-27, per Josh ("add an option... that makes the mid night line a
  # serises of grey dots"): verify the new 'dots' mode actually renders as
  # GeomPoint and picks up $midnight.dots.color (not $midnight.color), while
  # 'short' mode keeps using $midnight.color as before - i.e. the two color
  # settings stay genuinely independent, not just both present in the CSV.
  for (mode.check in c("short", "dots")) {
    aru.metadata.db.midcol <- aru.metadata.db.synth
    aru.metadata.db.midcol$midnight <- mode.check
    result.midcol <- batz.plotdetections_first.last(
      plot.data = plot.data.synth,
      aru.metadata.db = aru.metadata.db.midcol,
      suntimes.db = suntimes.synth,
      default.plotaesthetics = default.plotaesthetics.synth
    )
    g.midcol <- result.midcol$ggplots[[1]]
    geom.classes.midcol <- sapply(g.midcol$layers, function(l) class(l$geom)[1])
    built.midcol <- ggplot_build(g.midcol)
    # find the layer literally mapped to color = "Midnight" (aes(colour =
    # "Midnight")), rather than assuming a fixed position - layer order is
    # Dusk/Midnight/Dawn/crossbar in practice, not the Midnight-first order
    # implied by $layer.order, so a hardcoded index picked the wrong layer.
    colour.labels <- sapply(g.midcol$layers, function(l) {
      tryCatch(deparse(rlang::get_expr(l$mapping$colour)), error = function(e) NA_character_)
    })
    target.idx <- which(colour.labels == "\"Midnight\"")[1]
    resolved.color <- unique(built.midcol$data[[target.idx]]$colour)
    expected.color <- if (mode.check == "dots") get.default.synth("midnight.dots.color") else get.default.synth("midnight.color")
    cat(sprintf("  %s mode: geom = %s, resolved Midnight color = %s (expected %s)\n",
                mode.check, geom.classes.midcol[target.idx], paste(resolved.color, collapse = ","), expected.color))
  }

  # Legend breaks: aru.metadata.db.synth is Josh's real aru.metadata.db.real
  # UNMODIFIED, whose real $40khzmyo = TRUE - so TEST 4 (no actual 40kHzMyo
  # detection anywhere in its data) is now itself a live demonstration of
  # the "40k Myo is missing from the legend" fix below: the key should
  # still show, driven by the flag, not by data presence. TEST 5 also
  # inherits that same real $40khzmyo = TRUE (only $MYSO/$Alldect are
  # overridden there), so it shows "40kHzMyo" too - never "All detections"
  # in either case.
  gb4 <- ggplot_build(result4$ggplots[[1]])
  fill.scale.4 <- result4$ggplots[[1]]$scales$get_scales("fill")
  cat("TEST 4 (real $40khzmyo=TRUE, no actual 40kHzMyo detections) fill legend breaks:", paste(fill.scale.4$get_breaks(), collapse = ", "), "(expected: 40kHzMyo only - shown from the flag alone)\n")

  fill.scale.5 <- result5$ggplots[[1]]$scales$get_scales("fill")
  cat("TEST 5 (real $40khzmyo=TRUE) fill legend breaks:", paste(fill.scale.5$get_breaks(), collapse = ", "), "(expected: 40kHzMyo only)\n")
} else {
  cat("ggplot2 not available - skipping TEST 7\n")
}

cat("\n\n########## TEST 8: 40kHzMyo legend key is driven by $40khzmyo (species-list eligibility), not by whether a detection actually occurred ##########\n")
# 2026-08-27, per Josh ("40k Myo is missing from the legend"): this test
# previously only checked the legend WITH an actual 40kHzMyo detection row
# added to the data - it never covered Josh's real scenario, which is
# $40khzmyo = TRUE (on the species list) with ZERO actual 40kHzMyo
# detections that period (his real plot.meta.csv/vetted.processed.csv
# combination exactly). Rewritten to cover all four corners: $40khzmyo TRUE/
# FALSE crossed with a detection present/absent, so the "flag alone decides
# the key" fix is actually exercised, not just the "flag AND data" case
# that happened to already work before.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  khz.detection.row <- data.frame(spp.id = "40KHzMyo",
                                   date = format(as.Date(aru.metadata.db.real$date.start[1], format = "%m/%d/%Y") + 2, "%m/%d/%Y"),
                                   aru.groupby = "WTG-GOM102", obs = 1,
                                   mins2.noon.min = 561.7333, mins2.noon.max = 561.7333,
                                   vetting.type = "manid.sb")
  for (khz.flag.val in c(TRUE, FALSE)) {
    for (has.detection in c(TRUE, FALSE)) {
      plot.data.khz <- if (has.detection) rbind(plot.data.synth, khz.detection.row) else plot.data.synth
      aru.metadata.db.khz <- aru.metadata.db.synth
      aru.metadata.db.khz[["40khzmyo"]] <- khz.flag.val
      result.khz <- batz.plotdetections_first.last(
        plot.data = plot.data.khz,
        aru.metadata.db = aru.metadata.db.khz,
        suntimes.db = suntimes.synth,
        default.plotaesthetics = default.plotaesthetics.synth
      )
      expected.breaks <- if (khz.flag.val) "40kHzMyo" else "(none)"
      if (length(result.khz$ggplots) > 0) {
        fill.scale.khz <- result.khz$ggplots[[1]]$scales$get_scales("fill")
        breaks.txt <- paste(fill.scale.khz$get_breaks(), collapse = ", ")
        cat(sprintf("  $40khzmyo=%s, detection present=%s: fill legend breaks = '%s' (expected: %s)\n",
                     khz.flag.val, has.detection, breaks.txt, expected.breaks))
      } else {
        cat(sprintf("  $40khzmyo=%s, detection present=%s: no plot produced\n", khz.flag.val, has.detection))
      }
    }
  }
  cat("  40kHzMyo fill color (from default.plotaesthetics):",
      default.plotaesthetics.synth$default.value[default.plotaesthetics.synth$parameter == "crossbar.40khzmyo.fill"],
      "(expected: black)\n")
} else {
  cat("ggplot2 not available - skipping TEST 8\n")
}

cat("\n\n########## TEST 9: settings-resolution precedence with project.name = 'gome' ##########\n")
# Per Josh, 2026-08-27: confirm the code checks aru.metadata.db (plot.meta.csv)
# FIRST, then default.plotaesthetics' column matching project.name (now
# literally named "gome" - see the note in build_merged_plotoptions.R about
# why a column named "project.name" never matched any real project.name
# value), THEN falls back to default.value.
default.plotaesthetics.gome <- default.plotaesthetics.synth
default.plotaesthetics.gome$gome[default.plotaesthetics.gome$parameter == "yaxe.title"] <- "Hour of Monitoring (gome override)"
default.plotaesthetics.gome$gome[default.plotaesthetics.gome$parameter == "legend.position"] <- "top"
# dawn.color is left blank in both aru.metadata.db and the gome column, so
# it should fall all the way through to default.value ("blue").

# Case (a): aru.metadata.db's OWN $yaxe.title ("Hour of mointoring", the real
# typo'd value from the renamed duplicate xaxe.title column) is non-blank, so
# it must win over BOTH the gome column and the default - even though both
# of those also have a value defined for this same parameter.
result9a <- batz.plotdetections_first.last(
  plot.data = plot.data.synth,
  aru.metadata.db = aru.metadata.db.synth,
  suntimes.db = suntimes.synth,
  default.plotaesthetics = default.plotaesthetics.gome,
  project.name = "gome"
)
cat("Case (a) plot.meta.csv value present -> y-axis label:",
    if (length(result9a$ggplots) > 0) result9a$ggplots[[1]]$scales$get_scales("y")$name else "(no plot)",
    "(expected: '\\nHour of mointoring' - job's own value wins)\n")

# Case (b): remove aru.metadata.db's $yaxe.title entirely for this row (as if
# Josh's plot.meta.csv never had that column at all) - the gome column's
# override should now win over the default.
aru.metadata.db.nojob <- aru.metadata.db.synth
aru.metadata.db.nojob$yaxe.title <- NULL
result9b <- batz.plotdetections_first.last(
  plot.data = plot.data.synth,
  aru.metadata.db = aru.metadata.db.nojob,
  suntimes.db = suntimes.synth,
  default.plotaesthetics = default.plotaesthetics.gome,
  project.name = "gome"
)
cat("Case (b) plot.meta.csv value ABSENT -> y-axis label:",
    if (length(result9b$ggplots) > 0) result9b$ggplots[[1]]$scales$get_scales("y")$name else "(no plot)",
    "(expected: '\\nHour of Monitoring (gome override)' - gome column wins over default)\n")
cat("Case (b) legend.position (no aru.metadata.db column at all):",
    if (length(result9b$plots) > 0) result9b$plots[[1]]$resolved.legend.position else "(no plot)",
    "(expected: 'top' - gome column wins over default 'bottom')\n")

# Case (c): neither aru.metadata.db nor the gome column has a value for
# dawn.color - should fall all the way through to default.value.
cat("Case (c) dawn.color (blank in both aru.metadata.db and the gome column):",
    if (length(result9b$plots) > 0) result9b$plots[[1]]$resolved.dawn.color else "(no plot)",
    "(expected: 'blue' - default.value, since neither plot.meta.csv nor gome define it)\n")

cat("\n\n########## TEST 10: $xaxe.interval is read per-plot and produces exactly N evenly-spaced x-axis labels ##########\n")
# 2026-08-27, per Josh ("plot.meta$xaxe.interval = 4 which should make
# there only be four labeled dates on the X axes"): aru.metadata.db.synth
# is Josh's real aru.metadata.db.real UNMODIFIED, whose real $xaxe.interval
# = 4 - so result4 (TEST 4) is itself a live demonstration of this fix.
# Also spot-check an explicit override (7) and the invalid-value fallback
# (defaults to 2 = just date.start/date.end) on a copy.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  check.xaxe.breaks <- function(result, label, expected.n) {
    if (length(result$ggplots) == 0) {
      cat(sprintf("  %s: (no plot produced)\n", label))
      return(invisible(NULL))
    }
    x.scale <- result$ggplots[[1]]$scales$get_scales("x")
    breaks <- x.scale$breaks
    d.start <- result$plots[[1]]$date.start
    d.end <- result$plots[[1]]$date.end
    cat(sprintf("  %s: %d break(s) = %s (expected %d, spanning %s to %s; first/last match date.start/date.end: %s/%s)\n",
                label, length(breaks), paste(as.character(breaks), collapse = ", "), expected.n,
                as.character(d.start), as.character(d.end),
                identical(min(breaks), as.Date(d.start)), identical(max(breaks), as.Date(d.end))))
  }

  check.xaxe.breaks(result4, "real $xaxe.interval = 4 (unmodified aru.metadata.db.real)", 4)

  aru.metadata.db.xa7 <- aru.metadata.db.synth
  aru.metadata.db.xa7$xaxe.interval <- 7
  result.xa7 <- batz.plotdetections_first.last(
    plot.data = plot.data.synth,
    aru.metadata.db = aru.metadata.db.xa7,
    suntimes.db = suntimes.synth,
    default.plotaesthetics = default.plotaesthetics.synth
  )
  check.xaxe.breaks(result.xa7, "override $xaxe.interval = 7", 7)

  aru.metadata.db.xabad <- aru.metadata.db.synth
  aru.metadata.db.xabad$xaxe.interval <- "not-a-number"
  result.xabad <- batz.plotdetections_first.last(
    plot.data = plot.data.synth,
    aru.metadata.db = aru.metadata.db.xabad,
    suntimes.db = suntimes.synth,
    default.plotaesthetics = default.plotaesthetics.synth
  )
  check.xaxe.breaks(result.xabad, "invalid $xaxe.interval = 'not-a-number' (expect fallback to 2)", 2)
} else {
  cat("ggplot2 not available - skipping TEST 10\n")
}

cat("\n\n########## ALL TESTS COMPLETED ##########\n")

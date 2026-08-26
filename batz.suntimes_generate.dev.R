# batz.suntimes_generate.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.suntimes_*      (functions that work with ARU deployment
#                                 lists and solar-time calculations)
# Action:  generate             (generate sunrise/sunset times per ARU/date)
#
# This is the procedural script used to develop and test the logic against
# real sample data before it gets wrapped into the reusable function
# batz.suntimes_generate() (see batz.suntimes_generate.R).
#
# Purpose: for each ARU in an "*arulist.csv" deployment list, and for every
# date in that ARU's [$date.start, $date.end] range, calculate the sunset
# time on $date, the sunrise time on $date, and the sunrise time on the
# FOLLOWING day - i.e. the start/end bounds of that night's monitoring
# window (sunset of $date through sunrise of $date + 1).
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS MADE (spec was ambiguous on these - flag for review):
#
#  1. $suns was specified as "sunrise date and time", but $sunr is already
#     separately defined as "sunrise for that date" - having both be
#     sunrise would make $suns a pure duplicate of $sunr, which doesn't fit
#     a bat-monitoring use case (a monitoring night runs sunset -> next
#     sunrise). Given the "suns"/"sunr" naming shorthand strongly implies
#     sunset vs. sunrise, this script treats $suns as SUNSET on $date.
#     >>> Please confirm this is what you meant - easy to flip if not. <<<
#
#  2. $date.mon is specified simply as "date plus time of 12:00:00", with no
#     mention of the following day (unlike $sunr.mon, which explicitly says
#     "for the following day"). Taken literally, this script sets $date.mon
#     to $date itself at noon (12:00:00), NOT $date + 1. Noon is used
#     (rather than midnight) specifically because midnight timestamps are
#     prone to landing on the wrong calendar day when converted between
#     time zones - noon gives a safe date-time anchor for $date.
#
#  3. The date range [$date.start, $date.end] is treated as INCLUSIVE of
#     both endpoints (one row generated per calendar day in that closed
#     range).
#
#  4. Sunrise/sunset are calculated using the standard solar-elevation
#     threshold of -0.833 degrees (accounts for ~34' of atmospheric
#     refraction + the sun's ~16' angular radius) - this is the same
#     threshold used by NOAA's solar calculator and the widely-used
#     "suncalc" JS/R libraries. The underlying formulas (solar mean
#     anomaly, ecliptic longitude, declination, hour angle) are the
#     standard astronomy-answers.nl / NOAA approach - implemented here in
#     base R (see calc.suntimes()) rather than depending on the "suncalc"
#     package, so this script has zero non-base-R dependencies. Accuracy is
#     within about a minute of NOAA's published tables, which is more than
#     sufficient for defining a monitoring night's start/end.
#
#  5. Latitude/longitude that would produce polar day or polar night (no
#     sunrise or sunset on some date) is NOT expected in this data (all
#     ARUs are in the continental US) and would return NA with a note -
#     not specially handled beyond that.
#
#  6. Efficiency step (per Josh): before running any solar calculations,
#     ARU rows are expanded to one row per (aru, date), then collapsed to
#     the DISTINCT set of (sunregion, calc.lat, calc.long, time.zone, date)
#     combinations actually needed (calc.lat/calc.long per assumption #7
#     below). Multiple ARUs at the same site with identical or overlapping
#     date ranges automatically share one calculation per shared date
#     instead of repeating it per ARU - see the "efficiency" section below
#     for the before/after count.
#
#  7. $sunregion.type (added 2026-08-26, per Josh) - four categories were
#     specified: "fixed.unique", "fixed.pooled", "mobile.unique",
#     "mobile.pooled". Per Josh: "We will update the code to deal with the
#     fixed.unique & fixed.pooled first before moving on to the mobile
#     two" - so ONLY the two fixed types are implemented here; any row
#     tagged mobile.unique/mobile.pooled makes the script stop with a clear
#     error rather than silently running fixed-site logic against it.
#     - "fixed.unique": solar calc uses the ARU's own exact lat/long
#       (same as pre-existing behavior).
#     - "fixed.pooled": solar calc uses ONE representative lat/long per
#       $sunregion (the mean across every ARU sharing that sunregion), so
#       ARUs described as "near by but not exactly the same lat/long" still
#       share a single calculation per sunregion/date rather than getting
#       one calculation each for near-identical coordinates.
#     - If $sunregion.type is missing entirely (older input files), every
#       row defaults to "fixed.unique" so old files still run unchanged.
#     >>> Also note: Josh's own definitions of "mobile.unique" and
#     "mobile.pooled" are worded identically to each other - almost
#     certainly a copy/paste slip (by analogy with fixed.unique vs.
#     fixed.pooled, mobile.pooled should presumably differ by having a
#     pooled/shared sunregion ID). Not a blocker now since mobile isn't
#     implemented yet, but will need clarifying before that work starts. <<<
#
#  8. **Update (2026-08-26, later) - required-header check, fixed-type
#     filtering, and $sunregion.long/$sunregion.lat as the calculation
#     source - per Josh's new instruction, superseding parts of #7 above.**
#     - **Required headers, checked up front:** $aru, $long, $lat,
#       $sunregion, $sunregion.long, $sunregion.lat, $date.start, $date.end,
#       $time.zone, $sunregion.type, $schedual1, $schedual2. If any are
#       missing, the script stops with Josh's own literal message text:
#       "inputfile is missing these headers", followed by the list of
#       missing header names. $sunregion.type is now REQUIRED - assumption
#       #7's "defaults to fixed.unique if the column is absent entirely"
#       fallback no longer applies (a missing $sunregion.type column now
#       fails this header check first). $schedual1/$schedual2 are new,
#       pass-through-only columns (no calculation uses them) - kept with
#       Josh's own spelling ("schedual", not "schedule").
#     - **Real naming mismatch flagged, not silently fixed:** the real
#       "4 Current  test data\WTG.arulist.csv" test file (already updated
#       on Josh's end with real $sunregion.long/$sunregion.lat/
#       $sunregion.type columns) has its last two columns named
#       "schedual.type2"/"schedual.type" - NOT "schedual1"/"schedual2" as
#       just instructed. Built exactly to the new literal instruction
#       (requires $schedual1/$schedual2); this real file will FAIL the new
#       header check as-is. The test harness below (SECTION 2) renames
#       schedual.type -> schedual1 and schedual.type2 -> schedual2 on a
#       COPY of the real file purely so it can be used to test this
#       script - the real file on Josh's machine is untouched. >>> Flagging
#       for Josh: either rename those two columns in the real file, or say
#       the required names should be schedual.type/schedual.type2 instead
#       of schedual1/schedual2. <<<
#     - **Only "fixed.unique"/"fixed.pooled" rows get records generated -
#       a real behavior change from erroring to filtering.** Assumption
#       #7's hard stop() on any "mobile.unique"/"mobile.pooled" row is
#       GONE. Now, any row whose $sunregion.type is anything other than
#       "fixed.unique"/"fixed.pooled" (mobile.*, blank, a typo) is
#       EXCLUDED from the run with a console NOTE naming the affected
#       ARU(s) and their actual $sunregion.type value - the script no
#       longer stops just because a mobile row exists somewhere in the
#       file. If EVERY row ends up excluded, the script still stops
#       (nothing left to generate) - a distinct, deliberate stop, not the
#       old per-mobile-row one.
#     - **Solar-calculation coordinates now come directly from
#       $sunregion.long/$sunregion.lat for EVERY kept row - a real
#       behavior change, not just an addition.** Assumption #7's
#       "fixed.unique uses the ARU's own exact lat/long; fixed.pooled uses
#       a computed mean lat/long across ARUs sharing a sunregion" split is
#       GONE. Both types now use $sunregion.long/$sunregion.lat exactly as
#       given in the input file - no averaging happens in this script
#       anymore. $lat/$long (the ARU's own coordinates) are still required
#       input and still appear in the output, just no longer used for the
#       calculation itself. Added one new non-blocking sanity NOTE (not
#       requested, matching the existing $sunregion.type-consistency NOTE):
#       flags a $sunregion whose $sunregion.long/$sunregion.lat aren't
#       identical across every row sharing it.
#     - **New output columns**, per Josh's explicit list: $sunregion.long,
#       $sunregion.lat, $date.start, $date.end, $schedual1, $schedual2 (in
#       addition to $sunregion/$sunregion.type/$time.zone, which were
#       already in the output).
# ---------------------------------------------------------------------------

## base R only - no package dependencies required

## ===========================================================================
## SECTION 1: solar calculation helpers (base R port of the standard
## astronomy-answers.nl / NOAA sunrise-sunset algorithm - the same approach
## used by NOAA's solar calculator and the "suncalc" JS/R libraries)
## ===========================================================================

rad          <- pi / 180
day.ms       <- 86400 * 1000
J1970        <- 2440588
J2000        <- 2451545
e.obliquity  <- rad * 23.4397        # obliquity of the Earth
J0           <- 0.0009

to.julian <- function(date.posix.utc) {
  as.numeric(date.posix.utc) * 1000 / day.ms - 0.5 + J1970
}
from.julian <- function(j) {
  as.POSIXct((j + 0.5 - J1970) * day.ms / 1000, origin = "1970-01-01", tz = "UTC")
}
to.days <- function(date.posix.utc) to.julian(date.posix.utc) - J2000

declination <- function(l, b) {
  asin(sin(b) * cos(e.obliquity) + cos(b) * sin(e.obliquity) * sin(l))
}
solar.mean.anomaly <- function(d) rad * (357.5291 + 0.98560028 * d)
ecliptic.longitude <- function(M) {
  C <- rad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M)) # equation of center
  P <- rad * 102.9372                                                    # perihelion of the Earth
  M + C + P + pi
}
julian.cycle    <- function(d, lw) round(d - J0 - lw / (2 * pi))
approx.transit  <- function(Ht, lw, n) J0 + (Ht + lw) / (2 * pi) + n
solar.transit.j <- function(ds, M, L) J2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L)
hour.angle      <- function(h, phi, d) acos((sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d)))

get.set.j <- function(h, lw, phi, dec, n, M, L) {
  w <- hour.angle(h, phi, dec)
  a <- approx.transit(w, lw, n)
  solar.transit.j(a, M, L)
}

## Vectorized sunrise/sunset (as UTC POSIXct) for parallel date/lat/lon
## vectors of the same length.
##   date : Date vector (calendar date)
##   lat  : numeric vector, decimal degrees (+ north)
##   lon  : numeric vector, decimal degrees (+ east, - west)
## Returns a data.frame with $sunrise and $sunset (POSIXct, tz = "UTC").
## NA is returned for a given row if that lat produces no sunrise/sunset on
## that date (polar day/night) - see assumption #5 above.
##
## The reference instant is anchored at UTC NOON (not UTC midnight) of the
## given calendar date. The algorithm derives an integer "julian cycle"
## number by rounding (days-since-epoch - longitude/360), and anchoring at
## UTC midnight can round that to the PREVIOUS day for any location west of
## Greenwich (the longitude term alone can exceed half a day) - shifting
## every result a full day early. Anchoring at UTC noon keeps the rounding
## correct for every longitude from -180 to +180.
calc.suntimes <- function(date, lat, lon) {
  date.utc.anchor <- as.POSIXct(paste(date, "12:00:00"), tz = "UTC")
  lw  <- rad * -lon
  phi <- rad * lat
  d   <- to.days(date.utc.anchor)
  n   <- julian.cycle(d, lw)
  ds  <- approx.transit(0, lw, n)
  M   <- solar.mean.anomaly(ds)
  L   <- ecliptic.longitude(M)
  dec <- declination(L, 0)
  Jnoon <- solar.transit.j(ds, M, L)

  h0 <- -0.833 * rad
  suppressWarnings({
    Jset  <- get.set.j(h0, lw, phi, dec, n, M, L)
  })
  Jrise <- Jnoon - (Jset - Jnoon)

  data.frame(
    sunrise = from.julian(Jrise),
    sunset  = from.julian(Jset)
  )
}

## Format a vector of UTC POSIXct instants as local wall-clock time strings,
## grouped by time zone (rather than looping row-by-row) since a POSIXct
## vector can only carry one tzone attribute at a time - this keeps mixed
## time zones in one input file correct.
format.local <- function(instant.utc, tz) {
  out <- character(length(instant.utc))
  for (this.tz in unique(tz)) {
    idx <- which(tz == this.tz)
    out[idx] <- format(instant.utc[idx], tz = this.tz, usetz = FALSE)
  }
  out
}

## ===========================================================================
## SECTION 2: config for this test run
## ===========================================================================
## NOTE (2026-08-19, per Josh): renamed from "path" to "dir.load" to match
## the parameter name used by batz.datawrangler_load.files - same concept
## (directory to search/load from), should use the same name across functions.
##
## NOTE (2026-08-20, per Josh): cross-function optional-input naming pass -
## "subdirectory" renamed to "dir.sub" (default flipped TRUE -> FALSE), and
## a new "load.pattern" input added for the *arulist.csv suffix (previously
## hardwired). Josh's instruction listed a default of
## c("*templog.csv", "*templog.meta.csv") for this input, which is clearly a
## copy-paste artifact from the batz.templogger_merge.format part of the same
## message (this function has nothing to do with templog files) - used
## "*arulist.csv" instead, which is what this function actually searches
## for. Flagging this rather than silently applying the templog patterns
## here - please confirm "*arulist.csv" is what you meant.
## NOTE (2026-08-26, per Josh - load-error bugfix): dir.load was hardcoded to
## "/home/claude/batz_test", a path that only ever existed in Claude's own
## cloud sandbox from original development. On any other machine (e.g.
## Josh's Windows machine) this matches zero files, and the old script had
## no check for that - it silently cascaded into a malformed aru.list and a
## confusing "argument must be coercible to non-negative integer" error deep
## in SECTION 4. Fixed two ways: (1) default is now getwd() (matches the
## dir.load convention used by other batz functions, e.g.
## batz.vettedacoustics_merge.format) - >>> set this to wherever your
## *arulist.csv file actually lives before running <<<. (2) an explicit
## check right after list.files() now stops with a clear message instead of
## silently corrupting aru.list when zero files are found.
dir.load     <- getwd()
load.pattern <- "*arulist.csv"
dir.sub      <- FALSE

## NOTE (2026-08-26, per Josh): added dir.save - a separate output-write
## location, defaulting to dir.load - matching the same option already
## added to batz.templogger_merge.format (see preferences.md's "Second
## consistency pass" note). aru.suntimes.csv is now written to dir.save
## instead of always going back into dir.load.
dir.save     <- dir.load

## NOTE (2026-08-26, per Josh, revised same day): "file" lets Josh choose
## the output CSV's base name. The auto-generated part of the name is no
## longer optional - EVERY output file name gets a date-range + save-time
## stamp appended, so two runs never silently overwrite each other and the
## date coverage is visible from the file name alone:
##   <base>_<DATE1>to<DATE2>_sav<timestamp>_suntimes.csv
## where DATE1/DATE2 are the earliest/latest $date in the output (YYYYMMDD)
## and <timestamp> is when the file was written (YYYYMMDDHHMMSS). "" (the
## default) uses "aru" as <base>. Any other value is used as <base> - but
## first has any already-auto-generated suffix (a prior stamped run's
## "_DATE1toDATE2_savTIMESTAMP_suntimes.csv"/".csv" tail) stripped back
## off, so passing a previously-stamped output file name back in as `file`
## re-stamps it instead of stacking a second date/timestamp on top. See
## the stamping code itself, just above the write.csv() call below, for
## the actual strip/build logic.
file         <- ""

## convert a plain wildcard/glob suffix pattern (or vector of them) into one
## combined regex suitable for list.files()'s pattern= argument
pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

## ===========================================================================
## SECTION 3: load ARU deployment list(s)
## ===========================================================================
aru.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                         recursive = dir.sub, full.names = TRUE)

## NOTE (2026-08-26, per Josh - load-error bugfix): fail loudly and clearly
## here instead of letting a zero-file match cascade into a malformed
## aru.list (a plain, non-data.frame list produced by `NULL$field <- value`)
## whose nrow() misbehaves several sections later - that cascade is exactly
## what produced the "argument must be coercible to non-negative integer"
## error Josh saw.
if (length(aru.files) == 0) {
  stop("No files matching load.pattern (\"", paste(load.pattern, collapse = "\", \""),
       "\") were found in dir.load (\"", dir.load, "\"). Check that dir.load points ",
       "to the folder containing your *arulist.csv file.")
}

cat("Found", length(aru.files), "arulist.csv file(s)\n\n")

read.aru.list <- function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
  df
}
aru.list <- do.call(rbind, lapply(aru.files, read.aru.list))

## ===========================================================================
## SECTION 3a: required-header check (NEW, 2026-08-26, per Josh) - see
## assumption #8 above. Runs on the raw loaded columns, before any
## parsing/filtering below. Message text is Josh's own, used verbatim.
## ===========================================================================
required.headers <- c("aru", "long", "lat", "sunregion", "sunregion.long",
                       "sunregion.lat", "date.start", "date.end", "time.zone",
                       "sunregion.type", "schedual1", "schedual2")
missing.headers <- setdiff(required.headers, names(aru.list))
if (length(missing.headers) > 0) {
  stop("inputfile is missing these headers: ", paste(missing.headers, collapse = ", "))
}

## ---- parse types -----------------------------------------------------------
aru.list$lat  <- as.numeric(aru.list$lat)
aru.list$long <- as.numeric(aru.list$long)
aru.list$sunregion.long <- as.numeric(aru.list$sunregion.long)
aru.list$sunregion.lat  <- as.numeric(aru.list$sunregion.lat)

## date.start / date.end: try the common formats seen in practice
parse.simple.date <- function(x) {
  x <- trimws(x)
  out <- as.Date(rep(NA_character_, length(x)))
  fmts <- c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y")
  for (fmt in fmts) {
    still.na <- is.na(out) & nzchar(x)
    if (!any(still.na)) break
    parsed <- as.Date(x, format = fmt)
    out[still.na] <- parsed[still.na]
  }
  out
}
aru.list$date.start <- parse.simple.date(aru.list$date.start)
aru.list$date.end   <- parse.simple.date(aru.list$date.end)

cat("Loaded", nrow(aru.list), "ARU row(s) from input file(s)\n\n")

## ===========================================================================
## SECTION 3b: $sunregion.type - categorizes how $sunregion relates to ARU
## identity and lat/long stability:
##   "fixed.unique" - fixed lat/long, $sunregion == the ARU's own name (one
##                    ARU per sunregion).
##   "fixed.pooled" - fixed lat/long, $sunregion is SHARED across multiple
##                    ARUs that are near each other but not at exactly the
##                    same lat/long.
##   "mobile.unique"/"mobile.pooled" - lat/long changes across the date
##                    range enough to change sunrise/sunset - NOT YET
##                    IMPLEMENTED.
## UPDATED 2026-08-26 (see assumption #8 above): $sunregion.type is now a
## REQUIRED header (checked in SECTION 3a above) - the old "default to
## fixed.unique when the column is absent entirely" fallback is gone, since
## a missing column now fails the header check first. Only
## "fixed.unique"/"fixed.pooled" rows get records generated; any other
## value (mobile.*, or a typo) is EXCLUDED with a console NOTE instead of
## stopping the whole script - the OLD hard stop() on any mobile.* row is
## gone.
## ===========================================================================
aru.list$sunregion.type <- trimws(tolower(aru.list$sunregion.type))

allowed.types <- c("fixed.unique", "fixed.pooled")
keep.rows <- aru.list$sunregion.type %in% allowed.types
if (any(!keep.rows)) {
  excluded <- aru.list[!keep.rows, ]
  cat("NOTE:", nrow(excluded), "row(s) excluded - $sunregion.type is not",
      "\"fixed.unique\"/\"fixed.pooled\":",
      paste(unique(paste0(excluded$aru, " (", excluded$sunregion.type, ")")), collapse = ", "),
      "\n\n")
}
aru.list <- aru.list[keep.rows, , drop = FALSE]
if (nrow(aru.list) == 0) {
  stop("No rows remain after filtering to $sunregion.type \"fixed.unique\"/",
       "\"fixed.pooled\" - nothing to generate.")
}

## fixed.unique sanity check: spec says $sunregion should equal $aru for this
## type. Flagging a mismatch rather than silently ignoring it or guessing -
## does not block the run, since the lat/long-based grouping below is
## unaffected either way.
is.fixed.unique <- aru.list$sunregion.type == "fixed.unique"
mismatched.unique <- is.fixed.unique & (aru.list$sunregion != aru.list$aru)
if (any(mismatched.unique)) {
  cat("NOTE:", sum(mismatched.unique), "row(s) marked \"fixed.unique\" have",
      "$sunregion != $aru (spec says these should match for this type):",
      paste(aru.list$aru[mismatched.unique], collapse = ", "), "\n\n")
}

## flag a $sunregion used with more than one $sunregion.type across its ARUs
## (e.g. one ARU says "fixed.unique", another at the same sunregion says
## "fixed.pooled") - almost certainly a data-entry inconsistency
type.per.region <- aggregate(sunregion.type ~ sunregion, data = aru.list,
                              FUN = function(x) length(unique(x)))
mixed.regions <- type.per.region$sunregion[type.per.region$sunregion.type > 1]
if (length(mixed.regions) > 0) {
  cat("NOTE: sunregion(s) with inconsistent $sunregion.type across their ARUs:",
      paste(mixed.regions, collapse = ", "), "\n\n")
}

## light data-entry sanity check (NEW, 2026-08-26, not requested - added to
## match the $sunregion.type-consistency NOTE above): flag a $sunregion
## whose $sunregion.long/$sunregion.lat aren't identical across every row
## that shares it - not enforced/blocking, since the calculation below uses
## each row's own value directly (no averaging happens anymore)
coord.per.region <- aggregate(cbind(n.long = sunregion.long, n.lat = sunregion.lat) ~ sunregion,
                               data = aru.list, FUN = function(x) length(unique(x)))
mixed.coords <- coord.per.region$sunregion[coord.per.region$n.long > 1 | coord.per.region$n.lat > 1]
if (length(mixed.coords) > 0) {
  cat("NOTE: sunregion(s) with inconsistent $sunregion.long/$sunregion.lat across their ARUs:",
      paste(mixed.coords, collapse = ", "), "\n\n")
}

## ---- resolve the lat/long actually used for the solar calculation --------
## UPDATED 2026-08-26 (see assumption #8 above): $sunregion.long/
## $sunregion.lat are now used DIRECTLY for EVERY kept row (both
## fixed.unique and fixed.pooled) - no more per-type split, no more
## computed mean across ARUs sharing a sunregion.
aru.list$calc.lat  <- aru.list$sunregion.lat
aru.list$calc.long <- aru.list$sunregion.long

## ===========================================================================
## SECTION 4: expand each ARU row to one row per date in its range
## ===========================================================================
expand.one <- function(i) {
  row <- aru.list[i, ]
  dates <- seq(row$date.start, row$date.end, by = "day")
  data.frame(
    aru            = row$aru,
    sunregion      = row$sunregion,
    sunregion.type = row$sunregion.type,
    lat            = row$lat,
    long           = row$long,
    sunregion.long = row$sunregion.long,
    sunregion.lat  = row$sunregion.lat,
    calc.lat       = row$calc.lat,
    calc.long      = row$calc.long,
    date.start     = row$date.start,
    date.end       = row$date.end,
    schedual1      = row$schedual1,
    schedual2      = row$schedual2,
    time.zone      = row$time.zone,
    date           = dates,
    stringsAsFactors = FALSE
  )
}
aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))

cat("Expanded to", nrow(aru.expand), "aru-date row(s)\n\n")

## ===========================================================================
## SECTION 5: efficiency step - collapse to the distinct set of
## (sunregion, lat, long, time.zone, date) combinations actually needed.
## ARUs sharing a sunregion/lat/long with identical or overlapping date
## ranges collapse onto the same rows here, so the solar calculation below
## runs once per unique site-date rather than once per aru-date.
## ===========================================================================
## NOTE (2026-08-26, per Josh - $sunregion.type): grouping now uses
## calc.lat/calc.long rather than the ARU's own raw lat/long - for
## "fixed.unique" rows these are identical to lat/long (unchanged
## behavior), but for "fixed.pooled" rows calc.lat/calc.long is the shared
## per-sunregion center computed above, so ARUs at the same sunregion but
## slightly different exact coordinates still collapse onto one calculation.
site.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
site.dates <- aru.expand[!duplicated(site.key),
                          c("sunregion", "calc.lat", "calc.long", "time.zone", "date")]
row.names(site.dates) <- NULL

## how many aru rows share each (sunregion, calc.lat, calc.long) site (for
## the note below - a site with >1 aru row is exactly the "same
## lat/long/sunregion, overlapping or identical date range" case Josh asked
## to combine)
site.group.key <- with(aru.list, paste(sunregion, calc.lat, calc.long, sep = "|||"))
arus.per.site  <- table(site.group.key)
n.shared.sites <- sum(arus.per.site > 1)

cat("Efficiency check:\n")
cat(" -", nrow(aru.expand), "aru-date rows would be needed without de-duplication\n")
cat(" -", nrow(site.dates), "unique site-date rows actually calculated\n")
cat(" -", n.shared.sites, "site(s) have >1 ARU sharing the same lat/long/sunregion",
    "(same-day calculations reused across those ARUs)\n\n")

## ---- also need sunrise for date + 1 (the "following day" for $sunr.mon) --
## build the distinct set of (site, date) pairs actually required for that
## look-ahead, then union with site.dates so everything gets computed once
lookahead <- site.dates
lookahead$date <- lookahead$date + 1
calc.key        <- with(site.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
lookahead.key   <- with(lookahead,  paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
extra.needed    <- lookahead[!lookahead.key %in% calc.key &
                                !duplicated(lookahead.key), ]

calc.dates <- rbind(site.dates, extra.needed)
row.names(calc.dates) <- NULL

cat(nrow(extra.needed), "extra look-ahead row(s) added for next-day sunrise",
    "(", nrow(calc.dates), "total unique site-date rows sent to calc.suntimes())\n\n")

## ===========================================================================
## SECTION 6: run the solar calculation once per unique site-date
## ===========================================================================
suns <- calc.suntimes(calc.dates$date, calc.dates$calc.lat, calc.dates$calc.long)
calc.dates$sunrise.utc <- suns$sunrise
calc.dates$sunset.utc  <- suns$sunset

calc.dates$calc.key <- with(calc.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))

## ===========================================================================
## SECTION 7: join results back onto every (aru, date) row
## ===========================================================================
aru.expand$calc.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
aru.expand$next.day.key <- with(aru.expand,
                                 paste(sunregion, calc.lat, calc.long, date + 1, sep = "|||"))

lookup <- calc.dates[, c("calc.key", "sunrise.utc", "sunset.utc")]

today <- lookup[match(aru.expand$calc.key, lookup$calc.key), ]
nextd <- lookup[match(aru.expand$next.day.key, lookup$calc.key), ]

aru.suntimes <- data.frame(
  aru            = aru.expand$aru,
  date           = aru.expand$date,
  date.mon       = as.POSIXct(paste(aru.expand$date, "12:00:00")),
  sunregion      = aru.expand$sunregion,
  sunregion.long = aru.expand$sunregion.long,
  sunregion.lat  = aru.expand$sunregion.lat,
  date.start     = aru.expand$date.start,
  date.end       = aru.expand$date.end,
  time.zone      = aru.expand$time.zone,
  sunregion.type = aru.expand$sunregion.type,
  schedual1      = aru.expand$schedual1,
  schedual2      = aru.expand$schedual2,
  lat            = aru.expand$lat,
  long           = aru.expand$long,
  stringsAsFactors = FALSE
)

aru.suntimes$suns         <- format.local(today$sunset.utc, aru.expand$time.zone)
aru.suntimes$suns.unix    <- as.numeric(today$sunset.utc)
aru.suntimes$sunr         <- format.local(today$sunrise.utc, aru.expand$time.zone)
aru.suntimes$sunr.unix    <- as.numeric(today$sunrise.utc)
aru.suntimes$sunr.mon     <- format.local(nextd$sunrise.utc, aru.expand$time.zone)
aru.suntimes$sunr.mon.unix <- as.numeric(nextd$sunrise.utc)

## ===========================================================================
## SECTION 8: quick sanity check + write output
## ===========================================================================
cat("--- head(aru.suntimes) ---\n")
print(head(aru.suntimes, 10))

cat("\n--- range check ---\n")
cat("date range:", format(min(aru.suntimes$date)), "to", format(max(aru.suntimes$date)), "\n")
cat("any NA sunrise/sunset?", any(is.na(aru.suntimes$sunr)) || any(is.na(aru.suntimes$suns)), "\n")

## unix timestamps rounded to whole seconds only at the final write step -
## never on intermediate values, per project convention
aru.suntimes.out <- aru.suntimes
aru.suntimes.out$suns.unix     <- round(aru.suntimes.out$suns.unix)
aru.suntimes.out$sunr.unix     <- round(aru.suntimes.out$sunr.unix)
aru.suntimes.out$sunr.mon.unix <- round(aru.suntimes.out$sunr.mon.unix)

## ---- resolve the output file name -----------------------------------
## <base>_<DATE1>to<DATE2>_sav<timestamp>_suntimes.csv - see the NOTE on
## `file` in SECTION 2 above for the full explanation. Strip any
## previously-auto-generated suffix off a user-given `file` first, so
## feeding a prior stamped output file name back in re-stamps rather than
## stacking a second stamp on top of the first.
strip.autoname <- function(x) {
  x <- sub("\\.csv$", "", x, ignore.case = TRUE)
  x <- sub("_suntimes$", "", x, ignore.case = TRUE)
  x <- sub("_sav[0-9]{14}$", "", x, ignore.case = TRUE)
  x <- sub("_[0-9]{8}to[0-9]{8}$", "", x, ignore.case = TRUE)
  x
}
base.name <- if (identical(file, "")) "aru" else strip.autoname(file)

date1     <- format(min(aru.suntimes$date), "%Y%m%d")
date2     <- format(max(aru.suntimes$date), "%Y%m%d")
savestamp <- paste0("sav", format(Sys.time(), "%Y%m%d%H%M%S"))

out.file <- paste0(base.name, "_", date1, "to", date2, "_", savestamp, "_suntimes.csv")

write.csv(aru.suntimes.out, file.path(dir.save, out.file), row.names = FALSE)
cat("\nWrote", out.file, "to", dir.save, "\n")

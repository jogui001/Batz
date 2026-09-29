# batz.generate_suntimes.arulist.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.suntimes_*      (functions that work with ARU deployment
#                                 lists and solar-time calculations)
# Action:  generate             (generate sunrise/sunset times per ARU/date)
#
# This is the procedural script used to develop and test the logic against
# real sample data before it gets wrapped into the reusable function
# batz.generate_suntimes.arulist() (see batz.generate_suntimes.arulist.R).
#
# Purpose: for each ARU in an "*arulist.csv" deployment list, and for every
# date in that ARU's [$date.start, $date.end] range, calculate the sunset
# time on $date, the sunrise time on $date, and the sunrise time on the
# FOLLOWING day - i.e. the start/end bounds of that night's monitoring
# window (sunset of $date through sunrise of $date + 1).
#
# NOTE ON STALENESS, flagged rather than silently fixed: this procedural
# dev script had already fallen behind several shipped-.R-only rounds
# before today (round twenty-two's file-naming reorder/"sav"-prefix drop,
# round twenty-five's $date.mon -> $date.monitoringnight rename) - it
# still uses the OLD "sav<timestamp>"-style output file name and $date.mon
# below, neither of which this round touches. Round twenty-six's, round
# twenty-eight's, and round twenty-nine's changes (below) are layered on
# top of this script's existing state rather than also catching up those
# unrelated older rounds, to keep each round's change small and
# reviewable - catching this script fully up to the shipped .R file's
# current state is flagged as a separate follow-up.
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
#     the DISTINCT set of (sunregion, calc.lat, calc.long, time_zone, date)
#     combinations actually needed (calc.lat/calc.long per assumption #7
#     below). Multiple ARUs at the same site with identical or overlapping
#     date ranges automatically share one calculation per shared date
#     instead of repeating it per ARU - see the "efficiency" section below
#     for the before/after count.
#
#  7. $sunregion_type (added 2026-08-26, per Josh) - four categories were
#     specified: "fixed.unique", "fixed.pooled", "mobile.unique",
#     "mobile.pooled". Per Josh: "We will update the code to deal with the
#     fixed.unique & fixed.pooled first before moving on to the mobile
#     two" - so ONLY the two fixed types are implemented here; any row
#     tagged mobile.unique/mobile.pooled makes the script stop with a clear
#     error rather than silently running fixed-site logic against it.
#
#  8. **Update (2026-08-26, later) - required-header check, fixed-type
#     filtering, and $sunregion_long/$sunregion_lat as the calculation
#     source - per Josh's new instruction, superseding parts of #7 above.**
#     (See the shipped .R file's own @details for the full history - not
#     repeated here in full.)
#
#  9. **Standardized (2026-08-29, per Josh)** - dir.save default ->
#     getwd(); "file" renamed to "project.name".
#
#  10. **Header standardization (per Josh, 2026-09-14 project preference).**
#      See the shipped .R file's own @details, "Header standardization",
#      for the full explanation - this script inlines the same
#      standardize.headers() helper below since it's standalone.
#
#  11. **Follow-up, 2026-09-22, per Josh** - $aru -> $aru.name (internal/
#      output rename only at the time; the raw-file requirement itself was
#      still bare "aru" - see #12 below for how round twenty-six changes
#      this).
#
#  12. **Round twenty-six, 2026-09-25, per Josh: "change these in and out
#      headers" - a real, real-file-affecting rename, layered on top of
#      everything above.** Full mapping: sunregion_lat -> sunregion_latitude,
#      sunregion_long -> sunregion_longitude, aru -> aru.name, long ->
#      longitude, lat -> latitude, suns -> sunset, sunr -> sunrise,
#      suns.unix -> sunset.unix, sunr.unix -> sunrise.unix, sunr.mon ->
#      sunrise.monitoringnight, sunr.mon.unix -> sunrise.monitoringnight.unix.
#      Unlike assumption #11's aru->aru.name rename (internal/output only,
#      raw file kept bare "aru"), THIS round's aru/long/lat/sunregion_long/
#      sunregion_lat renames DO change the required raw-file spelling: the
#      real *arulist.csv now needs columns standardizing to "aru_name"/
#      "longitude"/"latitude"/"sunregion_longitude"/"sunregion_latitude",
#      not the old "aru"/"long"/"lat"/"sunregion_long"/"sunregion_lat" -
#      see required.headers below, and see the shipped .R file's own
#      @details, "Round twenty-six", for the full explanation including
#      which internal/purely-local names (calc.lat/calc.long) were
#      deliberately left unchanged.
#
#  13. **Round twenty-eight, 2026-09-28, per Josh: "with the required
#      headers, treat \".\" the same as \"_\" when checking if the
#      required headers are there."** The required-header check below no
#      longer does a bare setdiff() against already-standardized names -
#      it now uses the same canonicalize.headers() helper the shipped .R
#      file calls (inlined here, since this script is standalone),
#      standardizing BOTH required.headers AND the loaded file's own
#      column names before comparing, so a dot or an underscore in either
#      one is treated as equivalent. This unblocks two renames that were
#      previously left pending (round twenty-seven's reference-workbook
#      review): required.headers now reads aru.name/date.start/date.end
#      instead of aru_name/date_start/date_end, and $date.start/$date.end
#      replace $date_start/$date_end everywhere below and in aru.suntimes'
#      own output - see the shipped .R file's own @details, "Follow-up,
#      2026-09-28", for the full explanation. No real-file changes are
#      needed: an existing underscore-spelled *arulist.csv still passes.
#
#  14. **Round twenty-nine, 2026-09-28 (later the same day), per Josh's
#      reference-workbook review: "Several of the $name.standard have
#      $Change.to values that have not been changed, make those changes
#      now or flag why they can not be made."** Applying the same
#      canonicalize.headers() dot/underscore equivalence fix from #13,
#      this round renames sunregion_longitude -> sunregion.longitude and
#      sunregion_latitude -> sunregion.latitude throughout (required.headers,
#      the coordinate sanity check, calc.lat/calc.long's source columns,
#      expand.one(), and aru.suntimes' own output) - no real-file changes
#      needed, same reasoning as #13. A third pending rename touching this
#      function, sunregion_type -> sunregion.type, was NOT applied here:
#      see the shipped .R file's own @details for why (it collides with an
#      already-existing, deliberately distinct sunregion.type identifier
#      used by other functions) - flagged back to Josh instead of applied.
#      **Superseded 2026-09-29** - see #15 below: Josh confirmed the merge
#      was wanted, so sunregion_type -> sunregion.type is now applied too.
#
#  15. **Round thirty, 2026-09-29, per Josh ("These are the same things" -
#      directly answering #14's flagged question): confirmed the two
#      sunregion.type identifiers ARE the same concept, not a real
#      collision.** sunregion_type -> sunregion.type is now applied, the
#      same way as #14 (required.headers, aru.list$sunregion.type, the
#      allowed-types filter, the aggregate() consistency check,
#      expand.one(), and aru.suntimes' own output) - no real-file changes
#      needed, same reasoning as #13/#14.
# ---------------------------------------------------------------------------

## base R only - no package dependencies required

## standardize.headers() / canonicalize.headers() - inlined here because
## this dev script is standalone (not part of the installed package); in
## the real package every function in R/ is loaded together, so
## batz.generate_suntimes.arulist() can call them directly without this.
## See batz.util_standardize.headers.R.
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

canonicalize.headers <- function(df, required) {
  std.have <- standardize.headers(names(df))
  std.want <- standardize.headers(required)
  idx <- match(std.want, std.have)
  found <- !is.na(idx)
  names(df)[idx[found]] <- required[found]
  list(df = df, missing = required[!found])
}

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
dir.load     <- getwd()
load.pattern <- "*arulist.csv"
dir.sub      <- FALSE
dir.save     <- getwd()
project.name <- ""

pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

## ===========================================================================
## SECTION 3: load ARU deployment list(s)
## ===========================================================================
aru.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                         recursive = dir.sub, full.names = TRUE)

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

## header standardization (per Josh, 2026-09-14 project preference): see
## assumption #10 above - standardize the raw file's own headers before the
## required-header check below runs.
names(aru.list) <- standardize.headers(names(aru.list))

## ===========================================================================
## SECTION 3a: required-header check - see assumption #12 above (round
## twenty-six, 2026-09-25, per Josh): the raw *arulist.csv itself must now
## have columns standardizing to "aru_name"/"longitude"/"latitude"/
## "sunregion_longitude"/"sunregion_latitude" - a real change from the old
## "aru"/"long"/"lat"/"sunregion_long"/"sunregion_lat" spellings.
##
## Round twenty-eight, 2026-09-28, per Josh (see assumption #13 above): the
## check now goes through canonicalize.headers(), which standardizes BOTH
## required.headers AND aru.list's own names before comparing - so a dot
## and an underscore are treated as equivalent on either side. This is what
## lets required.headers use aru.name/date.start/date.end (dot-style)
## directly below, while still accepting a real file spelled with
## underscores.
##
## Round twenty-nine, 2026-09-28 (later the same day, see assumption #14
## above): the same dot/underscore equivalence now also lets
## required.headers use sunregion.longitude/sunregion.latitude (dot-style)
## directly, in place of the previous sunregion_longitude/
## sunregion_latitude - again, no real-file changes needed.
## ===========================================================================
required.headers <- c("aru.name", "longitude", "latitude", "sunregion", "sunregion.longitude",
                       "sunregion.latitude", "date.start", "date.end", "time_zone",
                       "sunregion.type", "schedual1", "schedual2")
canon <- canonicalize.headers(aru.list, required.headers)
if (length(canon$missing) > 0) {
  stop("inputfile is missing these headers: ", paste(canon$missing, collapse = ", "))
}
aru.list <- canon$df

## ---- parse types -----------------------------------------------------------
aru.list$latitude  <- as.numeric(aru.list$latitude)
aru.list$longitude <- as.numeric(aru.list$longitude)
aru.list$sunregion.longitude <- as.numeric(aru.list$sunregion.longitude)
aru.list$sunregion.latitude  <- as.numeric(aru.list$sunregion.latitude)

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
## SECTION 3b: $sunregion.type
## ===========================================================================
aru.list$sunregion.type <- trimws(tolower(aru.list$sunregion.type))

allowed.types <- c("fixed.unique", "fixed.pooled")
keep.rows <- aru.list$sunregion.type %in% allowed.types
if (any(!keep.rows)) {
  excluded <- aru.list[!keep.rows, ]
  cat("NOTE:", nrow(excluded), "row(s) excluded - $sunregion.type is not",
      "\"fixed.unique\"/\"fixed.pooled\":",
      paste(unique(paste0(excluded$aru.name, " (", excluded$sunregion.type, ")")), collapse = ", "),
      "\n\n")
}
aru.list <- aru.list[keep.rows, , drop = FALSE]
if (nrow(aru.list) == 0) {
  stop("No rows remain after filtering to $sunregion.type \"fixed.unique\"/",
       "\"fixed.pooled\" - nothing to generate.")
}

is.fixed.unique <- aru.list$sunregion.type == "fixed.unique"
mismatched.unique <- is.fixed.unique & (aru.list$sunregion != aru.list$aru.name)
if (any(mismatched.unique)) {
  cat("NOTE:", sum(mismatched.unique), "row(s) marked \"fixed.unique\" have",
      "$sunregion != $aru.name (spec says these should match for this type):",
      paste(aru.list$aru.name[mismatched.unique], collapse = ", "), "\n\n")
}

type.per.region <- aggregate(sunregion.type ~ sunregion, data = aru.list,
                              FUN = function(x) length(unique(x)))
mixed.regions <- type.per.region$sunregion[type.per.region$sunregion.type > 1]
if (length(mixed.regions) > 0) {
  cat("NOTE: sunregion(s) with inconsistent $sunregion.type across their ARUs:",
      paste(mixed.regions, collapse = ", "), "\n\n")
}

## light data-entry sanity check - round twenty-six renamed the columns
## this reads (sunregion_long/sunregion_lat -> sunregion_longitude/
## sunregion_latitude); round twenty-nine renamed them again, to
## sunregion.longitude/sunregion.latitude (dot-style) - same logic
## otherwise.
coord.per.region <- aggregate(cbind(n.long = sunregion.longitude, n.lat = sunregion.latitude) ~ sunregion,
                               data = aru.list, FUN = function(x) length(unique(x)))
mixed.coords <- coord.per.region$sunregion[coord.per.region$n.long > 1 | coord.per.region$n.lat > 1]
if (length(mixed.coords) > 0) {
  cat("NOTE: sunregion(s) with inconsistent $sunregion.longitude/$sunregion.latitude across their ARUs:",
      paste(mixed.coords, collapse = ", "), "\n\n")
}

## ---- resolve the lat/long actually used for the solar calculation --------
## Purely-internal calc.lat/calc.long variable names left unchanged this
## round (see assumption #12 above) - only their SOURCE columns were
## renamed.
aru.list$calc.lat  <- aru.list$sunregion.latitude
aru.list$calc.long <- aru.list$sunregion.longitude

## ===========================================================================
## SECTION 4: expand each ARU row to one row per date in its range
## ===========================================================================
expand.one <- function(i) {
  row <- aru.list[i, ]
  dates <- seq(row$date.start, row$date.end, by = "day")
  data.frame(
    aru.name       = row$aru.name,
    sunregion      = row$sunregion,
    sunregion.type = row$sunregion.type,
    latitude             = row$latitude,
    longitude            = row$longitude,
    sunregion.longitude  = row$sunregion.longitude,
    sunregion.latitude   = row$sunregion.latitude,
    calc.lat       = row$calc.lat,
    calc.long      = row$calc.long,
    date.start     = row$date.start,
    date.end       = row$date.end,
    schedual1      = row$schedual1,
    schedual2      = row$schedual2,
    time_zone      = row$time_zone,
    date           = dates,
    stringsAsFactors = FALSE
  )
}
aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))

cat("Expanded to", nrow(aru.expand), "aru-date row(s)\n\n")

## ===========================================================================
## SECTION 5: efficiency step
## ===========================================================================
site.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
site.dates <- aru.expand[!duplicated(site.key),
                          c("sunregion", "calc.lat", "calc.long", "time_zone", "date")]
row.names(site.dates) <- NULL

site.group.key <- with(aru.list, paste(sunregion, calc.lat, calc.long, sep = "|||"))
arus.per.site  <- table(site.group.key)
n.shared.sites <- sum(arus.per.site > 1)

cat("Efficiency check:\n")
cat(" -", nrow(aru.expand), "aru-date rows would be needed without de-duplication\n")
cat(" -", nrow(site.dates), "unique site-date rows actually calculated\n")
cat(" -", n.shared.sites, "site(s) have >1 ARU sharing the same lat/long/sunregion",
    "(same-day calculations reused across those ARUs)\n\n")

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

## NOTE: this script's own $date.mon (below) has NOT been renamed to
## $date.monitoringnight (that was round twenty-five, applied to the
## shipped .R file but not yet caught up in this dev script - see the
## "NOTE ON STALENESS" comment at the top of this file). Round
## twenty-six's, round twenty-eight's, and round twenty-nine's own renames
## ARE applied below.
aru.suntimes <- data.frame(
  aru.name             = aru.expand$aru.name,
  date                 = aru.expand$date,
  date.mon             = as.POSIXct(paste(aru.expand$date, "12:00:00")),
  sunregion            = aru.expand$sunregion,
  sunregion.longitude  = aru.expand$sunregion.longitude,
  sunregion.latitude   = aru.expand$sunregion.latitude,
  date.start           = aru.expand$date.start,
  date.end             = aru.expand$date.end,
  time_zone            = aru.expand$time_zone,
  sunregion.type       = aru.expand$sunregion.type,
  schedual1            = aru.expand$schedual1,
  schedual2            = aru.expand$schedual2,
  latitude             = aru.expand$latitude,
  longitude            = aru.expand$longitude,
  stringsAsFactors = FALSE
)

aru.suntimes$sunset                       <- format.local(today$sunset.utc, aru.expand$time_zone)
aru.suntimes$sunset.unix                  <- as.numeric(today$sunset.utc)
aru.suntimes$sunrise                      <- format.local(today$sunrise.utc, aru.expand$time_zone)
aru.suntimes$sunrise.unix                 <- as.numeric(today$sunrise.utc)
aru.suntimes$sunrise.monitoringnight      <- format.local(nextd$sunrise.utc, aru.expand$time_zone)
aru.suntimes$sunrise.monitoringnight.unix <- as.numeric(nextd$sunrise.utc)

## ===========================================================================
## SECTION 8: quick sanity check + write output
## ===========================================================================
cat("--- head(aru.suntimes) ---\n")
print(head(aru.suntimes, 10))

cat("\n--- range check ---\n")
cat("date range:", format(min(aru.suntimes$date)), "to", format(max(aru.suntimes$date)), "\n")
cat("any NA sunrise/sunset?", any(is.na(aru.suntimes$sunrise)) || any(is.na(aru.suntimes$sunset)), "\n")

aru.suntimes.out <- aru.suntimes
aru.suntimes.out$sunset.unix                  <- round(aru.suntimes.out$sunset.unix)
aru.suntimes.out$sunrise.unix                 <- round(aru.suntimes.out$sunrise.unix)
aru.suntimes.out$sunrise.monitoringnight.unix <- round(aru.suntimes.out$sunrise.monitoringnight.unix)

strip.autoname <- function(x) {
  x <- sub("\\.csv$", "", x, ignore.case = TRUE)
  x <- sub("_suntimes$", "", x, ignore.case = TRUE)
  x <- sub("_sav[0-9]{14}$", "", x, ignore.case = TRUE)
  x <- sub("_[0-9]{8}to[0-9]{8}$", "", x, ignore.case = TRUE)
  x
}
base.name <- if (identical(project.name, "")) "aru" else strip.autoname(project.name)

date1     <- format(min(aru.suntimes$date), "%Y%m%d")
date2     <- format(max(aru.suntimes$date), "%Y%m%d")
savestamp <- paste0("sav", format(Sys.time(), "%Y%m%d%H%M%S"))

out.file <- paste0(base.name, "_", date1, "to", date2, "_", savestamp, "_suntimes.csv")

write.csv(aru.suntimes.out, file.path(dir.save, out.file), row.names = FALSE)
cat("\nWrote", out.file, "to", dir.save, "\n")

## ===========================================================================
## ROUND TWENTY-SIX TESTS - case-insensitivity is not part of this round;
## this is a rename verification pass exercising the required-header check
## and every renamed output column, using the same synthetic fixture
## pattern as SECTION 3 above but self-contained (writes/removes its own
## temp files rather than depending on real *arulist.csv test data).
## ===========================================================================
cat("\n========================================\n")
cat("ROUND TWENTY-SIX RENAME TESTS (2026-09-25)\n")
cat("========================================\n\n")

test.dir <- file.path(tempdir(), "suntimes_round26_test")
if (dir.exists(test.dir)) unlink(test.dir, recursive = TRUE)
dir.create(test.dir)

new.fixture <- data.frame(
  aru.name = "SITE1", longitude = -70.5, latitude = 44.5, sunregion = "SITE1",
  sunregion.longitude = -70.5, sunregion.latitude = 44.5,
  date.start = "7/1/2026", date.end = "7/2/2026", time.zone = "America/New_York",
  sunregion.type = "fixed.unique", schedual1 = "A", schedual2 = "B",
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(new.fixture, file.path(test.dir, "test_arulist.csv"), row.names = FALSE)

old.fixture <- data.frame(
  aru = "SITE1", long = -70.5, lat = 44.5, sunregion = "SITE1",
  sunregion.long = -70.5, sunregion.lat = 44.5,
  date.start = "7/1/2026", date.end = "7/2/2026", time.zone = "America/New_York",
  sunregion.type = "fixed.unique", schedual1 = "A", schedual2 = "B",
  check.names = FALSE, stringsAsFactors = FALSE
)
old.test.dir <- file.path(tempdir(), "suntimes_round26_test_old")
if (dir.exists(old.test.dir)) unlink(old.test.dir, recursive = TRUE)
dir.create(old.test.dir)
write.csv(old.fixture, file.path(old.test.dir, "test_arulist.csv"), row.names = FALSE)

## TEST 1: new-format file passes the required-header check (round twenty-
## eight: now via canonicalize.headers(), not a plain setdiff())
aru.list.t1 <- read.csv(file.path(test.dir, "test_arulist.csv"), stringsAsFactors = FALSE,
                         colClasses = "character")
names(aru.list.t1) <- standardize.headers(names(aru.list.t1))
canon.t1 <- canonicalize.headers(aru.list.t1, required.headers)
stopifnot(length(canon.t1$missing) == 0)
cat("TEST 1 PASS: new-format headers satisfy required.headers\n")

## TEST 2: old-format file fails the required-header check, naming the
## new-format spellings (required.headers' OWN spelling, dot-style where
## applicable - round twenty-eight changed "aru_name" to "aru.name" here,
## and round twenty-nine changed "sunregion_longitude"/"sunregion_latitude"
## to "sunregion.longitude"/"sunregion.latitude") as missing (not the old
## ones)
aru.list.t2 <- read.csv(file.path(old.test.dir, "test_arulist.csv"), stringsAsFactors = FALSE,
                         colClasses = "character")
names(aru.list.t2) <- standardize.headers(names(aru.list.t2))
canon.t2 <- canonicalize.headers(aru.list.t2, required.headers)
stopifnot(identical(sort(canon.t2$missing),
                     sort(c("aru.name", "longitude", "latitude",
                            "sunregion.longitude", "sunregion.latitude"))))
cat("TEST 2 PASS: old-format headers correctly fail, missing:",
    paste(canon.t2$missing, collapse = ", "), "\n")

unlink(test.dir, recursive = TRUE)
unlink(old.test.dir, recursive = TRUE)

cat("\nALL ROUND TWENTY-SIX TESTS PASSED\n")

## ===========================================================================
## ROUND TWENTY-EIGHT TESTS (2026-09-28) - explicit dot/underscore
## equivalence verification for the required-header check itself, per
## Josh: "treat \".\" the same as \"_\" when checking if the required
## headers are there." Exercises both directions: a real file whose own
## headers use underscores (old convention) against a dotted
## required.headers entry, and vice versa.
## ===========================================================================
cat("\n========================================\n")
cat("ROUND TWENTY-EIGHT DOT/UNDERSCORE-EQUIVALENCE TESTS (2026-09-28)\n")
cat("========================================\n\n")

## TEST 3: file's own header spelled with an UNDERSCORE ("date_start")
## still satisfies a DOTTED required.headers entry ("date.start")
df.underscore <- data.frame(date_start = "1/1/2026", stringsAsFactors = FALSE)
canon.t3 <- canonicalize.headers(df.underscore, c("date.start"))
stopifnot(length(canon.t3$missing) == 0)
stopifnot(identical(names(canon.t3$df), "date.start"))
cat("TEST 3 PASS: underscore-spelled file header (date_start) satisfies",
    "dotted required.headers entry (date.start), renamed to date.start\n")

## TEST 4: file's own header spelled with a DOT ("aru.name") still
## satisfies a DOTTED required.headers entry ("aru.name") - the ordinary
## case, included for completeness alongside TEST 3/5
df.dot <- data.frame(aru.name = "SITE9", stringsAsFactors = FALSE, check.names = FALSE)
canon.t4 <- canonicalize.headers(df.dot, c("aru.name"))
stopifnot(length(canon.t4$missing) == 0)
cat("TEST 4 PASS: dot-spelled file header (aru.name) satisfies dotted",
    "required.headers entry (aru.name)\n")

## TEST 5: a genuinely missing header is still reported missing (the
## equivalence fix must not mask a real gap)
df.missing <- data.frame(unrelated_column = "x", stringsAsFactors = FALSE)
canon.t5 <- canonicalize.headers(df.missing, c("date.end"))
stopifnot(identical(canon.t5$missing, "date.end"))
cat("TEST 5 PASS: a genuinely absent header (date.end) is still reported missing\n")

cat("\nALL ROUND TWENTY-EIGHT TESTS PASSED\n")

## ===========================================================================
## ROUND TWENTY-NINE TESTS (2026-09-28, later the same day) - explicit
## verification that sunregion_longitude/sunregion_latitude (underscore,
## the OLD raw-file spelling) still satisfy the now-dotted
## required.headers entries (sunregion.longitude/sunregion.latitude), per
## the same dot/underscore equivalence mechanism as round twenty-eight -
## see assumption #14 above.
## ===========================================================================
cat("\n========================================\n")
cat("ROUND TWENTY-NINE DOT/UNDERSCORE-EQUIVALENCE TESTS (2026-09-28, later)\n")
cat("========================================\n\n")

## TEST 6: file's own header spelled with an UNDERSCORE
## ("sunregion_longitude") still satisfies a DOTTED required.headers entry
## ("sunregion.longitude")
df.sunregion.underscore <- data.frame(sunregion_longitude = -70.5, stringsAsFactors = FALSE)
canon.t6 <- canonicalize.headers(df.sunregion.underscore, c("sunregion.longitude"))
stopifnot(length(canon.t6$missing) == 0)
stopifnot(identical(names(canon.t6$df), "sunregion.longitude"))
cat("TEST 6 PASS: underscore-spelled file header (sunregion_longitude) satisfies",
    "dotted required.headers entry (sunregion.longitude), renamed to sunregion.longitude\n")

## TEST 7: same check for sunregion_latitude -> sunregion.latitude
df.sunregion.underscore.lat <- data.frame(sunregion_latitude = 44.5, stringsAsFactors = FALSE)
canon.t7 <- canonicalize.headers(df.sunregion.underscore.lat, c("sunregion.latitude"))
stopifnot(length(canon.t7$missing) == 0)
stopifnot(identical(names(canon.t7$df), "sunregion.latitude"))
cat("TEST 7 PASS: underscore-spelled file header (sunregion_latitude) satisfies",
    "dotted required.headers entry (sunregion.latitude), renamed to sunregion.latitude\n")

cat("\nALL ROUND TWENTY-NINE TESTS PASSED\n")

## ===========================================================================
## ROUND THIRTY TESTS (2026-09-29) - sunregion_type -> sunregion.type, per
## assumption #15 above (Josh: "These are the same things", confirming the
## merge). Same dot/underscore equivalence mechanism as rounds 28/29.
## ===========================================================================
cat("\n========================================\n")
cat("ROUND THIRTY DOT/UNDERSCORE-EQUIVALENCE TEST (2026-09-29)\n")
cat("========================================\n\n")

## TEST 8: file's own header spelled with an UNDERSCORE ("sunregion_type")
## still satisfies the now-dotted required.headers entry ("sunregion.type")
df.sunregion.type.underscore <- data.frame(sunregion_type = "fixed.unique", stringsAsFactors = FALSE)
canon.t8 <- canonicalize.headers(df.sunregion.type.underscore, c("sunregion.type"))
stopifnot(length(canon.t8$missing) == 0)
stopifnot(identical(names(canon.t8$df), "sunregion.type"))
cat("TEST 8 PASS: underscore-spelled file header (sunregion_type) satisfies",
    "dotted required.headers entry (sunregion.type), renamed to sunregion.type\n")

cat("\nALL ROUND THIRTY TESTS PASSED\n")

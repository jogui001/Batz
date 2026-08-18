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
#     the DISTINCT set of (sunregion, lat, long, time.zone, date)
#     combinations actually needed. Multiple ARUs at the same sunregion/
#     lat/long with identical or overlapping date ranges automatically
#     share one calculation per shared date instead of repeating it per
#     ARU - see the "efficiency" section below for the before/after count.
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
path         <- "/home/claude/batz_test"
subdirectory <- TRUE

## ===========================================================================
## SECTION 3: load ARU deployment list(s)
## ===========================================================================
aru.files <- list.files(path, pattern = "arulist\\.csv$",
                         recursive = subdirectory, full.names = TRUE)

cat("Found", length(aru.files), "arulist.csv file(s)\n\n")

read.aru.list <- function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
  df
}
aru.list <- do.call(rbind, lapply(aru.files, read.aru.list))

## ---- parse types -----------------------------------------------------------
aru.list$lat  <- as.numeric(aru.list$lat)
aru.list$long <- as.numeric(aru.list$long)

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
## SECTION 4: expand each ARU row to one row per date in its range
## ===========================================================================
expand.one <- function(i) {
  row <- aru.list[i, ]
  dates <- seq(row$date.start, row$date.end, by = "day")
  data.frame(
    aru       = row$aru,
    sunregion = row$sunregion,
    lat       = row$lat,
    long      = row$long,
    time.zone = row$time.zone,
    date      = dates,
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
site.key <- with(aru.expand, paste(sunregion, lat, long, date, sep = "|||"))
site.dates <- aru.expand[!duplicated(site.key),
                          c("sunregion", "lat", "long", "time.zone", "date")]
row.names(site.dates) <- NULL

## how many aru rows share each (sunregion, lat, long) site (for the note
## below - a site with >1 aru row is exactly the "same lat/long/sunregion,
## overlapping or identical date range" case Josh asked to combine)
site.group.key <- with(aru.list, paste(sunregion, lat, long, sep = "|||"))
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
calc.key        <- with(site.dates, paste(sunregion, lat, long, date, sep = "|||"))
lookahead.key   <- with(lookahead,  paste(sunregion, lat, long, date, sep = "|||"))
extra.needed    <- lookahead[!lookahead.key %in% calc.key &
                                !duplicated(lookahead.key), ]

calc.dates <- rbind(site.dates, extra.needed)
row.names(calc.dates) <- NULL

cat(nrow(extra.needed), "extra look-ahead row(s) added for next-day sunrise",
    "(", nrow(calc.dates), "total unique site-date rows sent to calc.suntimes())\n\n")

## ===========================================================================
## SECTION 6: run the solar calculation once per unique site-date
## ===========================================================================
suns <- calc.suntimes(calc.dates$date, calc.dates$lat, calc.dates$long)
calc.dates$sunrise.utc <- suns$sunrise
calc.dates$sunset.utc  <- suns$sunset

calc.dates$calc.key <- with(calc.dates, paste(sunregion, lat, long, date, sep = "|||"))

## ===========================================================================
## SECTION 7: join results back onto every (aru, date) row
## ===========================================================================
aru.expand$calc.key <- with(aru.expand, paste(sunregion, lat, long, date, sep = "|||"))
aru.expand$next.day.key <- with(aru.expand,
                                 paste(sunregion, lat, long, date + 1, sep = "|||"))

lookup <- calc.dates[, c("calc.key", "sunrise.utc", "sunset.utc")]

today <- lookup[match(aru.expand$calc.key, lookup$calc.key), ]
nextd <- lookup[match(aru.expand$next.day.key, lookup$calc.key), ]

aru.suntimes <- data.frame(
  aru       = aru.expand$aru,
  date      = aru.expand$date,
  date.mon  = as.POSIXct(paste(aru.expand$date, "12:00:00")),
  sunregion = aru.expand$sunregion,
  lat       = aru.expand$lat,
  long      = aru.expand$long,
  time.zone = aru.expand$time.zone,
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

write.csv(aru.suntimes.out, file.path(path, "aru.suntimes.csv"), row.names = FALSE)
cat("\nWrote aru.suntimes.csv to", path, "\n")

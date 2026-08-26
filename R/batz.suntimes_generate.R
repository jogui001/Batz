#' Generate sunrise/sunset times for an ARU deployment list
#'
#' For every ARU in an \verb{*arulist.csv} deployment list, and for every
#' calendar date in that ARU's \code{[$date.start, $date.end]} range,
#' calculates sunset on \code{$date}, sunrise on \code{$date}, and sunrise on
#' the FOLLOWING day - i.e. the start/end bounds of that night's monitoring
#' window (sunset of \code{$date} through sunrise of \code{$date + 1}).
#'
#' Solar times are calculated in base R (no package dependencies) using the
#' standard astronomy-answers.nl / NOAA sunrise-sunset algorithm - the same
#' approach used by NOAA's solar calculator and the widely-used "suncalc"
#' JS/R libraries, accurate to within about a minute of NOAA's published
#' tables.
#'
#' Before any solar calculation runs, ARU rows are expanded to one row per
#' (aru, date) and then collapsed to the DISTINCT set of
#' (\code{sunregion}, \code{calc.lat}, \code{calc.long}, \code{time.zone},
#' \code{date}) combinations actually needed (\code{calc.lat}/\code{calc.long}
#' per the \code{$sunregion.type} handling below). ARUs that share a site
#' with identical or overlapping date ranges automatically reuse one
#' calculation per shared date instead of repeating it per ARU.
#'
#' \strong{$sunregion.type (added 2026-08-26, per Josh):} an optional input
#' column categorizing how \code{$sunregion} relates to ARU identity and
#' lat/long stability. Four categories were specified -
#' \code{"fixed.unique"}, \code{"fixed.pooled"}, \code{"mobile.unique"},
#' \code{"mobile.pooled"} - but per Josh's own instruction ("We will update
#' the code to deal with the fixed.unique & fixed.pooled first before
#' moving on to the mobile two"), only the two fixed types are implemented
#' here:
#' \itemize{
#'   \item \code{"fixed.unique"} - a fixed lat/long where \code{$sunregion}
#'     is the same as the ARU's own name. The solar calculation uses the
#'     ARU's own exact lat/long (unchanged from the original behavior).
#'   \item \code{"fixed.pooled"} - a fixed lat/long where \code{$sunregion}
#'     is SHARED across multiple ARUs that are near each other but not at
#'     exactly the same lat/long. The solar calculation uses ONE
#'     representative lat/long per \code{$sunregion} (the mean across every
#'     ARU sharing that sunregion), so these ARUs still share a single
#'     calculation per sunregion/date instead of getting one calculation
#'     each for near-identical coordinates. \code{$lat}/\code{$long} in the
#'     output remain each ARU's own actual coordinates - only the
#'     calculation itself uses the shared center.
#'   \item \code{"mobile.unique"}/\code{"mobile.pooled"} - NOT YET
#'     IMPLEMENTED (lat/long changing across the date range enough to
#'     change sunrise/sunset). Any row tagged with either of these makes
#'     the function stop with a clear error, rather than silently running
#'     fixed-site logic against a moving ARU (which would give wrong
#'     times).
#'   \item If \code{$sunregion.type} is absent from the input file entirely
#'     (older files), every row defaults to \code{"fixed.unique"} so old
#'     files still run unchanged.
#' }
#' A row marked \code{"fixed.unique"} whose \code{$sunregion} does not equal
#' its \code{$aru} is flagged with a console \code{NOTE} (per the spec's own
#' definition that the two should match for this type) but does not block
#' the run.
#'
#' \strong{Assumptions made (spec was ambiguous on these - flag for
#' review):}
#' \itemize{
#'   \item \code{$suns} is treated as SUNSET on \code{$date} (the shorthand
#'     "suns"/"sunr" strongly implies sunset vs. sunrise, and \code{$sunr}
#'     is separately and explicitly defined as sunrise - having both be
#'     sunrise would make \code{$suns} a pure duplicate).
#'   \item \code{$date.mon} is \code{$date} itself at noon (12:00:00), NOT
#'     \code{$date + 1} - taken literally from "date plus time of
#'     12:00:00", with no mention of the following day (unlike
#'     \code{$sunr.mon}, which explicitly says "for the following day").
#'     Noon (rather than midnight) is used so the timestamp can't drift to
#'     the wrong calendar day when read back in a different time zone.
#'   \item \code{[$date.start, $date.end]} is inclusive of both endpoints.
#'   \item Sunrise/sunset use the standard -0.833 degree solar-elevation
#'     threshold (atmospheric refraction + the sun's angular radius).
#'   \item Locations that would produce polar day/night (no sunrise or
#'     sunset on some date) are not expected in this data and are not
#'     specially handled beyond returning \code{NA}.
#' }
#'
#' See \code{batz.suntimes_generate.dev.R} in the package source repo for
#' the tested procedural version and the full assumptions list. Those
#' assumptions apply here unchanged and should be reviewed before relying
#' on this in production - in particular, please confirm the \code{$suns} =
#' sunset interpretation above is what was intended.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.suntimes_generate()}: family = "suntimes" (functions that work
#' with ARU deployment lists and solar-time calculations), action =
#' "generate".
#'
#' @param dir.load Directory to search for files matching \code{load.pattern}.
#'   Default: current working directory. Must actually contain the
#'   \verb{*arulist.csv} file(s) - if no matching file is found, the
#'   function stops with a clear error rather than proceeding on an empty
#'   deployment list.
#' @param load.pattern Character, default \code{"*arulist.csv"}: the
#'   file-name suffix pattern (plain wildcard/glob style - \code{"*"} as a
#'   leading wildcard, everything else literal) that identifies the ARU
#'   deployment-list file(s) to load.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   every subdirectory of \code{dir.load}.
#' @param write.output If \code{TRUE} (default), also write
#'   \code{aru.suntimes.csv} into \code{dir.load} (with the \verb{*.unix} columns
#'   rounded to whole seconds in the written CSV only).
#'
#' @return Invisibly, a list with:
#'   \describe{
#'     \item{aru.suntimes}{One row per (aru, date): \code{$aru}, \code{$date},
#'       \code{$date.mon}, \code{$sunregion}, \code{$sunregion.type},
#'       \code{$lat}, \code{$long}, \code{$time.zone}, \code{$suns},
#'       \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'       \code{$sunr.mon}, \code{$sunr.mon.unix}.}
#'     \item{efficiency}{One-row summary: \code{$aru.date.rows} (rows needed
#'       without de-duplication), \code{$site.date.rows} (unique site-date
#'       rows actually calculated), \code{$shared.sites} (count of sites
#'       with more than one ARU sharing the same calculation site).}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- batz.suntimes_generate(dir.load = "path/to/data")
#' result$aru.suntimes
#' result$efficiency
#' }
#'
#' @export
batz.suntimes_generate <- function(dir.load = getwd(),
                                    load.pattern = "*arulist.csv",
                                    dir.sub = FALSE,
                                    write.output = TRUE) {

  ## convert a plain wildcard/glob suffix pattern (or vector of them) into
  ## one combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  ## ---- internal helpers: solar calculation (base R port of the standard
  ## astronomy-answers.nl / NOAA sunrise-sunset algorithm) -------------------
  rad          <- pi / 180
  day.ms       <- 86400 * 1000
  J1970        <- 2440588
  J2000        <- 2451545
  e.obliquity  <- rad * 23.4397
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
    C <- rad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M))
    P <- rad * 102.9372
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

  ## Vectorized sunrise/sunset (UTC POSIXct) for parallel date/lat/lon
  ## vectors. Anchored at UTC NOON (not UTC midnight) of the given calendar
  ## date - anchoring at midnight can round the algorithm's internal
  ## "julian cycle" integer to the PREVIOUS day for any location west of
  ## Greenwich, shifting every result a full day early; noon keeps the
  ## rounding correct for every longitude from -180 to +180.
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
      Jset <- get.set.j(h0, lw, phi, dec, n, M, L)
    })
    Jrise <- Jnoon - (Jset - Jnoon)

    data.frame(sunrise = from.julian(Jrise), sunset = from.julian(Jset))
  }

  ## format a vector of UTC POSIXct instants as local wall-clock strings,
  ## grouped by time zone (a POSIXct vector can only carry one tzone
  ## attribute at a time, so this keeps mixed time zones correct)
  format.local <- function(instant.utc, tz) {
    out <- character(length(instant.utc))
    for (this.tz in unique(tz)) {
      idx <- which(tz == this.tz)
      out[idx] <- format(instant.utc[idx], tz = this.tz, usetz = FALSE)
    }
    out
  }

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

  ## ===========================================================================
  ## load ARU deployment list(s)
  ## ===========================================================================
  aru.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE)

  ## fail loudly and clearly here instead of letting a zero-file match
  ## cascade into a malformed aru.list (a plain, non-data.frame list
  ## produced by `NULL$field <- value`) whose nrow() misbehaves several
  ## steps later, several steps away from the real cause.
  if (length(aru.files) == 0) {
    stop("No files matching load.pattern (\"", paste(load.pattern, collapse = "\", \""),
         "\") were found in dir.load (\"", dir.load, "\"). Check that dir.load points ",
         "to the folder containing your *arulist.csv file.")
  }

  aru.list <- do.call(rbind, lapply(aru.files, read.csv,
                                     stringsAsFactors = FALSE,
                                     colClasses = "character"))

  aru.list$lat  <- as.numeric(aru.list$lat)
  aru.list$long <- as.numeric(aru.list$long)
  aru.list$date.start <- parse.simple.date(aru.list$date.start)
  aru.list$date.end   <- parse.simple.date(aru.list$date.end)

  ## ===========================================================================
  ## $sunregion.type - see @details above for the four categories and what's
  ## implemented. Only fixed.unique/fixed.pooled are supported; mobile.* stop
  ## with a clear error.
  ## ===========================================================================
  if (!"sunregion.type" %in% names(aru.list)) {
    aru.list$sunregion.type <- "fixed.unique"
  }
  aru.list$sunregion.type <- trimws(tolower(aru.list$sunregion.type))

  valid.types <- c("fixed.unique", "fixed.pooled", "mobile.unique", "mobile.pooled")
  bad.types <- setdiff(unique(aru.list$sunregion.type), valid.types)
  if (length(bad.types) > 0) {
    stop("Unrecognized $sunregion.type value(s): ", paste(bad.types, collapse = ", "),
         ". Expected one of: ", paste(valid.types, collapse = ", "), ".")
  }

  mobile.rows <- aru.list$sunregion.type %in% c("mobile.unique", "mobile.pooled")
  if (any(mobile.rows)) {
    stop(sum(mobile.rows), " row(s) have $sunregion.type \"mobile.unique\"/",
         "\"mobile.pooled\", which this function does not yet implement (mobile ",
         "logic - lat/long changing across the date range - is planned but not ",
         "built yet). Affected ARU(s): ",
         paste(unique(aru.list$aru[mobile.rows]), collapse = ", "),
         ". Remove/hold these rows out until mobile support is added.")
  }

  is.fixed.unique <- aru.list$sunregion.type == "fixed.unique"
  mismatched.unique <- is.fixed.unique & (aru.list$sunregion != aru.list$aru)
  if (any(mismatched.unique)) {
    cat("NOTE:", sum(mismatched.unique), "row(s) marked \"fixed.unique\" have",
        "$sunregion != $aru (spec says these should match for this type):",
        paste(aru.list$aru[mismatched.unique], collapse = ", "), "\n\n")
  }

  type.per.region <- aggregate(sunregion.type ~ sunregion, data = aru.list,
                                FUN = function(x) length(unique(x)))
  mixed.regions <- type.per.region$sunregion[type.per.region$sunregion.type > 1]
  if (length(mixed.regions) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion.type across their ARUs:",
        paste(mixed.regions, collapse = ", "), "\n\n")
  }

  ## resolve the lat/long actually used for the solar calculation:
  ## fixed.unique -> the ARU's own exact lat/long; fixed.pooled -> one
  ## representative (mean) lat/long per $sunregion
  aru.list$calc.lat  <- aru.list$lat
  aru.list$calc.long <- aru.list$long

  is.fixed.pooled <- aru.list$sunregion.type == "fixed.pooled"
  if (any(is.fixed.pooled)) {
    pooled.centers <- aggregate(cbind(calc.lat = lat, calc.long = long) ~ sunregion,
                                 data = aru.list[is.fixed.pooled, ], FUN = mean)
    idx <- match(aru.list$sunregion[is.fixed.pooled], pooled.centers$sunregion)
    aru.list$calc.lat[is.fixed.pooled]  <- pooled.centers$calc.lat[idx]
    aru.list$calc.long[is.fixed.pooled] <- pooled.centers$calc.long[idx]
  }

  ## ===========================================================================
  ## expand each ARU row to one row per date in its range
  ## ===========================================================================
  expand.one <- function(i) {
    row <- aru.list[i, ]
    dates <- seq(row$date.start, row$date.end, by = "day")
    data.frame(
      aru = row$aru, sunregion = row$sunregion,
      sunregion.type = row$sunregion.type,
      lat = row$lat, long = row$long,
      calc.lat = row$calc.lat, calc.long = row$calc.long,
      time.zone = row$time.zone, date = dates,
      stringsAsFactors = FALSE
    )
  }
  aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))

  ## ===========================================================================
  ## efficiency step - collapse to the distinct (sunregion, calc.lat,
  ## calc.long, time.zone, date) combinations actually needed. ARUs sharing
  ## a site with identical or overlapping date ranges collapse onto the
  ## same rows here, so the solar calculation runs once per unique
  ## site-date rather than once per aru-date.
  ## ===========================================================================
  site.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  site.dates <- aru.expand[!duplicated(site.key),
                            c("sunregion", "calc.lat", "calc.long", "time.zone", "date")]
  row.names(site.dates) <- NULL

  site.group.key <- with(aru.list, paste(sunregion, calc.lat, calc.long, sep = "|||"))
  arus.per.site  <- table(site.group.key)
  n.shared.sites <- sum(arus.per.site > 1)

  ## next-day look-ahead needed for $sunr.mon
  lookahead <- site.dates
  lookahead$date <- lookahead$date + 1
  calc.key      <- with(site.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  lookahead.key <- with(lookahead,  paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  extra.needed  <- lookahead[!lookahead.key %in% calc.key & !duplicated(lookahead.key), ]

  calc.dates <- rbind(site.dates, extra.needed)
  row.names(calc.dates) <- NULL

  ## ===========================================================================
  ## run the solar calculation once per unique site-date
  ## ===========================================================================
  suns <- calc.suntimes(calc.dates$date, calc.dates$calc.lat, calc.dates$calc.long)
  calc.dates$sunrise.utc <- suns$sunrise
  calc.dates$sunset.utc  <- suns$sunset
  calc.dates$calc.key <- with(calc.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))

  ## ===========================================================================
  ## join results back onto every (aru, date) row
  ## ===========================================================================
  aru.expand$calc.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  aru.expand$next.day.key <- with(aru.expand,
                                   paste(sunregion, calc.lat, calc.long, date + 1, sep = "|||"))

  lookup <- calc.dates[, c("calc.key", "sunrise.utc", "sunset.utc")]
  today  <- lookup[match(aru.expand$calc.key, lookup$calc.key), ]
  nextd  <- lookup[match(aru.expand$next.day.key, lookup$calc.key), ]

  aru.suntimes <- data.frame(
    aru            = aru.expand$aru,
    date           = aru.expand$date,
    date.mon       = as.POSIXct(paste(aru.expand$date, "12:00:00")),
    sunregion      = aru.expand$sunregion,
    sunregion.type = aru.expand$sunregion.type,
    lat            = aru.expand$lat,
    long           = aru.expand$long,
    time.zone      = aru.expand$time.zone,
    stringsAsFactors = FALSE
  )
  aru.suntimes$suns          <- format.local(today$sunset.utc, aru.expand$time.zone)
  aru.suntimes$suns.unix     <- as.numeric(today$sunset.utc)
  aru.suntimes$sunr          <- format.local(today$sunrise.utc, aru.expand$time.zone)
  aru.suntimes$sunr.unix     <- as.numeric(today$sunrise.utc)
  aru.suntimes$sunr.mon      <- format.local(nextd$sunrise.utc, aru.expand$time.zone)
  aru.suntimes$sunr.mon.unix <- as.numeric(nextd$sunrise.utc)

  efficiency <- data.frame(
    aru.date.rows  = nrow(aru.expand),
    site.date.rows = nrow(site.dates),
    shared.sites   = n.shared.sites
  )

  if (write.output) {
    # rounding is applied only here, at the final save step - never to
    # intermediate values or to the object returned to R (below)
    aru.suntimes.out <- aru.suntimes
    aru.suntimes.out$suns.unix     <- round(aru.suntimes.out$suns.unix)
    aru.suntimes.out$sunr.unix     <- round(aru.suntimes.out$sunr.unix)
    aru.suntimes.out$sunr.mon.unix <- round(aru.suntimes.out$sunr.mon.unix)
    write.csv(aru.suntimes.out, file.path(dir.load, "aru.suntimes.csv"), row.names = FALSE)
  }

  invisible(list(aru.suntimes = aru.suntimes, efficiency = efficiency))
}

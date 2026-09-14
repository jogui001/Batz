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
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real, documented output-schema change.} The raw \verb{*arulist.csv}'s
#' own headers are now run through the shared package helper
#' \code{standardize.headers()} (trim whitespace, collapse every run of
#' non-alphanumeric characters to a single underscore, lowercase) right
#' after loading, before the required-header check below runs. Unlike most
#' other \code{batz} functions, this one's \code{required.headers} list is
#' a literal, uninvented copy of the ARU list file's own real column text
#' (not a \code{batz}-invented shorthand) - so per this project's
#' header-standardization preference, both the raw incoming headers AND
#' \code{required.headers} are standardized together, and the change
#' cascades all the way through to this function's own returned
#' \code{aru.suntimes} column names, since those columns are pass-through
#' copies of the loaded file's fields, not an independently-invented output
#' schema. Concretely, every dot-separated required header becomes
#' underscore-separated: \code{sunregion.type} -> \code{sunregion_type},
#' \code{sunregion.long} -> \code{sunregion_long}, \code{sunregion.lat} ->
#' \code{sunregion_lat}, \code{date.start} -> \code{date_start},
#' \code{date.end} -> \code{date_end}, \code{time.zone} -> \code{time_zone}.
#' \code{aru}, \code{long}, \code{lat}, \code{sunregion}, \code{schedual1},
#' \code{schedual2} have no separators and are unaffected. This does NOT
#' touch this function's own INVENTED fields, which never came from the
#' loaded file - \code{date.mon}, \code{suns}, \code{suns.unix}, \code{sunr},
#' \code{sunr.unix}, \code{sunr.mon}, \code{sunr.mon.unix}, \code{calc.lat},
#' \code{calc.long} all keep their existing dot-separated names, per this
#' project's ordinary (unrelated to this preference) dot-separated output
#' convention. \strong{Anyone with a saved *arulist.csv using the old
#' dotted header spellings should re-save it with the new underscore
#' spellings (or simply let \code{read.csv()} load it as-is and rely on
#' \code{standardize.headers()} to convert it automatically - a header
#' spelled \code{"sunregion.long"} standardizes to \code{"sunregion_long"}
#' exactly the same as \code{"Sunregion Long"} would), and update any
#' downstream script that reads \code{aru.suntimes} by the old dotted
#' column names.}
#'
#' \strong{Required input headers (added 2026-08-26, per Josh) - checked
#' up front, before anything else runs.} \code{dir.load}'s
#' \verb{*arulist.csv} must have every one of: \code{aru}, \code{long},
#' \code{lat}, \code{sunregion}, \code{sunregion_long},
#' \code{sunregion_lat}, \code{date_start}, \code{date_end},
#' \code{time_zone}, \code{sunregion_type}, \code{schedual1},
#' \code{schedual2} (standardized spellings - see "Header standardization"
#' above). If any are missing, the function stops immediately
#' with \code{"inputfile is missing these headers"} followed by the list
#' of missing header names (Josh's own literal message text, used
#' verbatim). \code{$sunregion_type} is now REQUIRED - the previous
#' behavior of defaulting to \code{"fixed.unique"} when the column was
#' entirely absent no longer applies, since a missing
#' \code{$sunregion_type} column now fails this header check before
#' reaching that point. \code{$schedual1}/\code{$schedual2} are new,
#' pass-through-only columns (not used in any calculation, just carried
#' into the output - see below); note the spelling is Josh's own
#' ("schedual", not "schedule"), kept exactly as given.
#'
#' \strong{Only \code{"fixed.unique"}/\code{"fixed.pooled"} rows get
#' records generated (updated 2026-08-26, per Josh) - a real behavior
#' change from erroring to filtering.} Four \code{$sunregion_type}
#' categories exist - \code{"fixed.unique"}, \code{"fixed.pooled"},
#' \code{"mobile.unique"}, \code{"mobile.pooled"} - but only rows where
#' \code{$sunregion_type} is \code{"fixed.unique"} or \code{"fixed.pooled"}
#' get records generated. Any OTHER value (\code{"mobile.unique"}/
#' \code{"mobile.pooled"}, or anything else, including a typo) is
#' EXCLUDED from the run with a console \code{NOTE} listing the affected
#' ARU(s) and their actual \code{$sunregion_type} value, rather than
#' stopping the whole function. \strong{The PREVIOUS version of this
#' function hard-stopped the entire run if ANY row was
#' \code{"mobile.unique"}/\code{"mobile.pooled"}; that hard stop is gone
#' now} - those rows are simply left out of \code{aru.suntimes}. If every
#' row ends up excluded, the function still stops (nothing to generate).
#'
#' \strong{Solar-calculation coordinates now come directly from
#' \code{$sunregion_long}/\code{$sunregion_lat} (updated 2026-08-26, per
#' Josh) - a real behavior change.} The PREVIOUS version used the ARU's
#' own exact \code{$lat}/\code{$long} for \code{"fixed.unique"} rows, and
#' a COMPUTED MEAN of \code{$lat}/\code{$long} across every ARU sharing a
#' \code{$sunregion} for \code{"fixed.pooled"} rows. Now, for EVERY kept
#' row (both types), the solar calculation uses \code{$sunregion_long}/
#' \code{$sunregion_lat} exactly as given in the input file - no
#' averaging happens here anymore; the input file itself is now
#' responsible for carrying one consistent coordinate pair on every row
#' sharing a \code{$sunregion}. \code{$lat}/\code{$long} (the ARU's own
#' coordinates) are still required as input and still appear in the
#' output, just no longer used for the calculation itself. A console
#' \code{NOTE} (non-blocking) is printed if any \code{$sunregion} has
#' more than one distinct \code{$sunregion_long}/\code{$sunregion_lat}
#' pair across its rows - not explicitly requested, added as a light
#' data-entry sanity check matching the existing
#' \code{$sunregion_type}-consistency \code{NOTE} below.
#'
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
#'   \item \code{[$date_start, $date_end]} is inclusive of both endpoints.
#'   \item Sunrise/sunset use the standard -0.833 degree solar-elevation
#'     threshold (atmospheric refraction + the sun's angular radius).
#'   \item Locations that would produce polar day/night (no sunrise or
#'     sunset on some date) are not expected in this data and are not
#'     specially handled beyond returning \code{NA}.
#' }
#'
#' See \code{batz.generate_suntimes.arulist.dev.R} in the package source repo for
#' the tested procedural version and the full assumptions list. Those
#' assumptions apply here unchanged and should be reviewed before relying
#' on this in production - in particular, please confirm the \code{$suns} =
#' sunset interpretation above is what was intended.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.generate_suntimes.arulist()}: family = "suntimes" (functions
#' that work with ARU deployment lists and solar-time calculations), action =
#' "generate".
#'
#' \strong{Follow-up, 2026-08-30, per Josh: renamed from
#' \code{batz.suntimes_generate()} to \code{batz.generate_suntimes.arulist()}}
#' - this breaks the project's own \code{family_action.subject} convention
#' (the verb normally comes after the underscore, e.g. \code{suntimes_generate},
#' \code{batusa_recode.names}) since "generate" now comes first, but the user
#' was asked to confirm and explicitly chose this exact literal name over a
#' convention-conforming alternative, so it was used as given.
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
#'   \code{aru.suntimes.csv} into \code{dir.save} (with the \verb{*.unix}
#'   columns rounded to whole seconds in the written CSV only).
#' @param dir.save Directory to write the output CSV into when
#'   \code{write.output = TRUE}. Default: the current working directory
#'   (\code{getwd()}) - set this separately if the output should be
#'   written somewhere other than where the input \verb{*arulist.csv} was
#'   loaded from. (Standardized 2026-08-29, per Josh: previously defaulted
#'   to \code{dir.load}, which silently mirrored whatever \code{dir.load}
#'   was rather than defaulting to a sensible location on its own -
#'   flagged as a bug and fixed.)
#' @param project.name Character, default \code{""}: base name for the
#'   output CSV written when \code{write.output = TRUE}. Every output
#'   file name gets a date-range + save-time stamp appended automatically,
#'   so no two runs ever silently overwrite each other:
#'   \verb{<base>_<DATE1>to<DATE2>_sav<timestamp>_suntimes.csv}, where
#'   \code{DATE1}/\code{DATE2} are the earliest/latest \code{$date} in the
#'   output (\code{YYYYMMDD}) and \code{<timestamp>} is when the file was
#'   written (\code{YYYYMMDDHHMMSS}, e.g. \code{sav20260826114426}).
#'   \code{""} (default) uses \code{"aru"} as \verb{<base>}. Any other
#'   value is used as \verb{<base>} instead - e.g. \code{project.name =
#'   "projectname_suntimes.csv"} produces something like
#'   \code{"projectname_20250101to20250202_sav20260826113700_suntimes.csv"}.
#'   If the given value already ends in a previously-auto-generated suffix
#'   (e.g. you passed a prior run's output file name back in), that suffix
#'   is stripped back off first, so the file gets a fresh stamp instead of
#'   a second one stacked on top.
#'
#' @return Invisibly, a list with:
#'   \describe{
#'     \item{aru.suntimes}{One row per (aru, date), \code{"fixed.unique"}/
#'       \code{"fixed.pooled"} rows only: \code{$aru}, \code{$date},
#'       \code{$date.mon}, \code{$sunregion}, \code{$sunregion_long},
#'       \code{$sunregion_lat}, \code{$date_start}, \code{$date_end},
#'       \code{$time_zone}, \code{$sunregion_type}, \code{$schedual1},
#'       \code{$schedual2}, \code{$lat}, \code{$long}, \code{$suns},
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
#' result <- batz.generate_suntimes.arulist(dir.load = "path/to/data")
#' result$aru.suntimes
#' result$efficiency
#' }
#'
#' @export
batz.generate_suntimes.arulist <- function(dir.load = getwd(),
                                    load.pattern = "*arulist.csv",
                                    dir.sub = FALSE,
                                    write.output = TRUE,
                                    dir.save = getwd(),
                                    project.name = "") {

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

  ## header standardization (per Josh, 2026-09-14 project preference): the
  ## raw *arulist.csv's own headers are standardized (trim, collapse
  ## non-alphanumeric runs to a single underscore, lowercase) before the
  ## required-header check below runs - see @details "Header
  ## standardization" above for why this cascades into required.headers
  ## and this function's own returned column names.
  names(aru.list) <- standardize.headers(names(aru.list))

  ## ===========================================================================
  ## required-header check (added 2026-08-26, per Josh) - runs on the raw
  ## loaded columns, before any parsing/filtering below. Message text is
  ## Josh's own, used verbatim.
  ## ===========================================================================
  required.headers <- c("aru", "long", "lat", "sunregion", "sunregion_long",
                         "sunregion_lat", "date_start", "date_end", "time_zone",
                         "sunregion_type", "schedual1", "schedual2")
  missing.headers <- setdiff(required.headers, names(aru.list))
  if (length(missing.headers) > 0) {
    stop("inputfile is missing these headers: ", paste(missing.headers, collapse = ", "))
  }

  aru.list$lat  <- as.numeric(aru.list$lat)
  aru.list$long <- as.numeric(aru.list$long)
  aru.list$sunregion_long <- as.numeric(aru.list$sunregion_long)
  aru.list$sunregion_lat  <- as.numeric(aru.list$sunregion_lat)
  aru.list$date_start <- parse.simple.date(aru.list$date_start)
  aru.list$date_end   <- parse.simple.date(aru.list$date_end)

  ## ===========================================================================
  ## $sunregion_type - see @details above. $sunregion_type is now required
  ## (enforced by the header check above), so no more default-when-absent
  ## fallback. Only "fixed.unique"/"fixed.pooled" rows are kept; every other
  ## value (mobile.*, or a typo) is EXCLUDED with a NOTE instead of
  ## stopping the whole run - a real behavior change from the previous
  ## version, which hard-stopped on any mobile.* row.
  ## ===========================================================================
  aru.list$sunregion_type <- trimws(tolower(aru.list$sunregion_type))

  allowed.types <- c("fixed.unique", "fixed.pooled")
  keep.rows <- aru.list$sunregion_type %in% allowed.types
  if (any(!keep.rows)) {
    excluded <- aru.list[!keep.rows, ]
    cat("NOTE:", nrow(excluded), "row(s) excluded - $sunregion_type is not",
        "\"fixed.unique\"/\"fixed.pooled\":",
        paste(unique(paste0(excluded$aru, " (", excluded$sunregion_type, ")")), collapse = ", "),
        "\n\n")
  }
  aru.list <- aru.list[keep.rows, , drop = FALSE]
  if (nrow(aru.list) == 0) {
    stop("No rows remain after filtering to $sunregion_type \"fixed.unique\"/",
         "\"fixed.pooled\" - nothing to generate.")
  }

  is.fixed.unique <- aru.list$sunregion_type == "fixed.unique"
  mismatched.unique <- is.fixed.unique & (aru.list$sunregion != aru.list$aru)
  if (any(mismatched.unique)) {
    cat("NOTE:", sum(mismatched.unique), "row(s) marked \"fixed.unique\" have",
        "$sunregion != $aru (spec says these should match for this type):",
        paste(aru.list$aru[mismatched.unique], collapse = ", "), "\n\n")
  }

  type.per.region <- aggregate(sunregion_type ~ sunregion, data = aru.list,
                                FUN = function(x) length(unique(x)))
  mixed.regions <- type.per.region$sunregion[type.per.region$sunregion_type > 1]
  if (length(mixed.regions) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion_type across their ARUs:",
        paste(mixed.regions, collapse = ", "), "\n\n")
  }

  ## light data-entry sanity check (not requested, added to match the
  ## $sunregion_type-consistency NOTE above): flag a $sunregion whose
  ## $sunregion_long/$sunregion_lat aren't identical across every row that
  ## shares it - not enforced/blocking, since the calculation below uses
  ## each row's own value directly (no averaging happens anymore)
  coord.per.region <- aggregate(cbind(n.long = sunregion_long, n.lat = sunregion_lat) ~ sunregion,
                                 data = aru.list, FUN = function(x) length(unique(x)))
  mixed.coords <- coord.per.region$sunregion[coord.per.region$n.long > 1 | coord.per.region$n.lat > 1]
  if (length(mixed.coords) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion_long/$sunregion_lat across their ARUs:",
        paste(mixed.coords, collapse = ", "), "\n\n")
  }

  ## resolve the lat/long actually used for the solar calculation (updated
  ## 2026-08-26, per Josh): $sunregion_long/$sunregion_lat are now used
  ## DIRECTLY for every kept row (both fixed.unique and fixed.pooled) - see
  ## @details above for the behavior change from the previous
  ## exact-ARU-coords/computed-mean split.
  aru.list$calc.lat  <- aru.list$sunregion_lat
  aru.list$calc.long <- aru.list$sunregion_long

  ## ===========================================================================
  ## expand each ARU row to one row per date in its range
  ## ===========================================================================
  expand.one <- function(i) {
    row <- aru.list[i, ]
    dates <- seq(row$date_start, row$date_end, by = "day")
    data.frame(
      aru = row$aru, sunregion = row$sunregion,
      sunregion_type = row$sunregion_type,
      lat = row$lat, long = row$long,
      sunregion_long = row$sunregion_long, sunregion_lat = row$sunregion_lat,
      calc.lat = row$calc.lat, calc.long = row$calc.long,
      date_start = row$date_start, date_end = row$date_end,
      schedual1 = row$schedual1, schedual2 = row$schedual2,
      time_zone = row$time_zone, date = dates,
      stringsAsFactors = FALSE
    )
  }
  aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))

  ## ===========================================================================
  ## efficiency step - collapse to the distinct (sunregion, calc.lat,
  ## calc.long, time_zone, date) combinations actually needed. ARUs sharing
  ## a site with identical or overlapping date ranges collapse onto the
  ## same rows here, so the solar calculation runs once per unique
  ## site-date rather than once per aru-date.
  ## ===========================================================================
  site.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  site.dates <- aru.expand[!duplicated(site.key),
                            c("sunregion", "calc.lat", "calc.long", "time_zone", "date")]
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
    sunregion_long = aru.expand$sunregion_long,
    sunregion_lat  = aru.expand$sunregion_lat,
    date_start     = aru.expand$date_start,
    date_end       = aru.expand$date_end,
    time_zone      = aru.expand$time_zone,
    sunregion_type = aru.expand$sunregion_type,
    schedual1      = aru.expand$schedual1,
    schedual2      = aru.expand$schedual2,
    lat            = aru.expand$lat,
    long           = aru.expand$long,
    stringsAsFactors = FALSE
  )
  aru.suntimes$suns          <- format.local(today$sunset.utc, aru.expand$time_zone)
  aru.suntimes$suns.unix     <- as.numeric(today$sunset.utc)
  aru.suntimes$sunr          <- format.local(today$sunrise.utc, aru.expand$time_zone)
  aru.suntimes$sunr.unix     <- as.numeric(today$sunrise.utc)
  aru.suntimes$sunr.mon      <- format.local(nextd$sunrise.utc, aru.expand$time_zone)
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
    # ---- resolve the output file name ----------------------------------
    # <base>_<DATE1>to<DATE2>_sav<timestamp>_suntimes.csv - see @param
    # project.name above. Strip any previously-auto-generated suffix off a
    # user-given `project.name` first, so feeding a prior stamped output
    # file name back in re-stamps rather than stacking a second stamp on
    # top of the first.
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
  }

  invisible(list(aru.suntimes = aru.suntimes, efficiency = efficiency))
}

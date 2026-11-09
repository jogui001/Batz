#' Generate sunrise/sunset times for an ARU deployment list
#'
#' For every ARU in an \verb{*arulist.csv} deployment list and every date in
#' its \code{$sunregion_date.start} to \code{$sunregion_date.end} range
#' (inclusive; \code{$date.start} to \code{$date.end} when those are absent
#' or blank), calculates
#' sunset on that date, sunrise on that date, and sunrise on the following
#' day. Sunset on \code{$date} through sunrise on \code{$date + 1} is that
#' night's monitoring window.
#'
#' @details
#' \strong{Solar calculation.} Times are calculated in base R with the
#' standard NOAA / astronomy-answers sunrise-sunset algorithm (the same
#' approach as the "suncalc" libraries), using the -0.833 degree
#' solar-elevation threshold. Results are within about a minute of NOAA's
#' tables. Each date is anchored at noon UTC so the result stays on the
#' correct day for any longitude. Polar day/night (no sunrise or sunset)
#' gives \code{NA}.
#'
#' \strong{Loading.} Every file in \code{dir.load} matching
#' \code{load.pattern} is read as text with \code{special.read.csv()}
#' (UTF-8 with a Latin-1 fallback) and all files are stacked. With
#' \code{strip.special = TRUE}, accented letters and common symbols are
#' simplified (e.g. an accented e becomes \code{e}, curly quotes become
#' straight quotes) and other non-ASCII characters (e.g. a degree sign) are
#' removed from column names and data, so a latitude like
#' \code{"42.036373"} followed by a degree sign still converts to a number.
#' With \code{strip.special = FALSE} such a value becomes \code{NA}.
#'
#' \strong{Required \verb{*arulist.csv} headers.} Headers are matched with
#' \code{canonicalize.headers()}, which ignores case and treats \code{"."},
#' \code{"_"} and spaces as the same, and matched columns are renamed to:
#' \itemize{
#'   \item \code{$aru.name}: ARU identifier.
#'   \item \code{$longitude}, \code{$latitude}: the ARU's own coordinates
#'     (copied to the output, not used in the calculation).
#'   \item \code{$sunregion}: sun-region name. For \code{"fixed.unique"}
#'     rows it should equal \code{$aru.name}.
#'   \item \code{$sunregion.longitude}, \code{$sunregion.latitude}:
#'     coordinates used for the solar calculation of every row.
#'   \item \code{$date.start}, \code{$date.end}: deployment dates
#'     (m/d/Y, Y-m-d or m/d/y). Sun times are calculated for these dates
#'     unless \code{$sunregion_date.start}/\code{$sunregion_date.end} are
#'     given.
#'   \item \code{$time.zone}: time zone name (e.g.
#'     \code{"America/New_York"}) used for local times. A \code{time_zone}
#'     column is also accepted.
#' }
#' If any of these is missing, the function stops with
#' \code{"input file is missing these headers: ..."}.
#'
#' \strong{Optional headers} (e.g. an ARU list written by
#' \code{\link{batz.merge_sitevisit.acoustics}} with
#' \code{generate.arulist = TRUE}):
#' \itemize{
#'   \item \code{$sunregion.type}: \code{"fixed.unique"} or
#'     \code{"fixed.pooled"}. When the column is absent, or a cell is blank,
#'     it is \code{"fixed.unique"} where \code{$sunregion} equals
#'     \code{$aru.name} (case and spaces ignored), otherwise
#'     \code{"fixed.pooled"}.
#'   \item \code{$schedule1}, \code{$schedule2}: recording schedules, copied
#'     to the output only; blank when absent. The old spellings
#'     \code{schedual1}/\code{schedual2} are still accepted.
#'   \item \code{$sunregion_date.start}, \code{$sunregion_date.end}: the
#'     dates sun times are calculated for (m/d/Y, Y-m-d or m/d/y). A row with
#'     either one blank uses its \code{$date.start} to \code{$date.end}. The
#'     output's \code{$date.start}/\code{$date.end} stay the deployment
#'     dates.
#' }
#'
#' \strong{Sun region coordinates} (\code{sunregion.update = TRUE}). For each
#' \code{$sunregion}, the rows' \code{$sunregion.longitude} /
#' \code{$sunregion.latitude} pairs are compared (blank = either one blank):
#' \itemize{
#'   \item all blank, or every row a different pair: the central point (mean
#'     \code{$longitude} and \code{$latitude} of all its rows, 8 decimals)
#'     goes into every row;
#'   \item one pair given, or a pair shared by two or more rows: rows with a
#'     pair keep it, and blank rows get that pair (the most common one when
#'     there are several).
#' }
#' A NOTE lists each sun region changed and how. The ARU list as used is
#' returned as \code{$arulist}.
#'
#' \strong{Fixed sun regions} (\code{sunregion.list}). Each row moves ARU-list
#' rows whose \code{$aru.name} or \code{$sunregion} matches its
#' \code{$aru.name} (ignoring case, spaces, "." and "_") into its
#' \code{$sunregion}, for the dates from its \code{$date.start} to
#' \code{$date.end} (a blank date = no limit on that side; dates m/d/Y,
#' Y-m-d or m/d/y). Its \code{$sunregion.longitude} /
#' \code{$sunregion.latitude}, when given, take precedence over the ARU
#' list's. When they are blank, the moved rows take the coordinates of other
#' ARUs already in that sun region on the same date (the most common pair),
#' else the central point (mean \code{$longitude} / \code{$latitude}) of
#' the sun region's ARUs. \code{$sunregion.type} of a moved row is
#' \code{"fixed.unique"} when the sun region is the ARU itself, else
#' \code{"fixed.pooled"}. This is applied per date, after
#' \code{sunregion.update}; a NOTE lists the ARUs moved.
#'
#' \strong{Conflicts stop the function.} An \code{$aru.name} given more than
#' one \code{$sunregion} for overlapping dates (in the list itself, or an
#' ARU matched by two rows through its \code{$aru.name} and its
#' \code{$sunregion}) stops with a message naming the \code{$aru.name}, the
#' sun regions, their dates and the lines of the file to edit (line 1 is the
#' header).
#'
#' \strong{Checks.} \code{$sunregion.type} is trimmed and lower-cased. Rows
#' that are not \code{"fixed.unique"} or \code{"fixed.pooled"} (e.g.
#' \code{"mobile.unique"}, \code{"mobile.pooled"} or a typo) are dropped
#' with a console NOTE; if no rows remain the function stops. A NOTE (not
#' blocking) is also printed for \code{"fixed.unique"} rows whose
#' \code{$sunregion} differs from \code{$aru.name}, for sun regions with more
#' than one \code{$sunregion.type}, and for sun regions with more than one
#' \code{$sunregion.longitude}/\code{$sunregion.latitude} pair.
#'
#' \strong{Efficiency.} Rows are expanded to one per ARU and date, then
#' reduced to distinct sun region, coordinate and date combinations (plus
#' the next day where needed for \code{$sunrise.monitoringnight}). The
#' calculation runs once per combination, so ARUs sharing a sun region and
#' dates reuse one result.
#'
#' \strong{Output file.} With \code{write.output = TRUE}, a CSV named
#' \verb{<base>_suntimes_<DATE1>to<DATE2>_<timestamp>.csv} is written to
#' \code{dir.save}, where \code{DATE1}/\code{DATE2} are the first/last
#' \code{$date} (\code{YYYYMMDD}) and \verb{<timestamp>} is the save time
#' (\code{YYYYMMDDHHMMSS}). The \verb{*.unix} columns are rounded to whole
#' seconds in the CSV only.
#'
#' @param dir.load Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}. The function stops if no
#'   file is found.
#' @param load.pattern Character, default \code{"*arulist.csv"}. Wildcard
#'   (glob) file-name pattern of the ARU deployment list(s). All matching
#'   files are stacked.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also
#'   search subfolders of \code{dir.load}.
#' @param write.output Logical, default \code{TRUE}. If \code{TRUE}, write
#'   \code{aru.suntimes} to a CSV in \code{dir.save} (see Details).
#' @param dir.save Directory for the output CSV. Default \code{getwd()}.
#' @param project.name Character, default \code{""}. Base name of the
#'   output CSV; \code{""} uses \code{"aru"}. If the value ends with a
#'   suffix this function adds (\code{_suntimes}, date range, timestamp,
#'   \code{.csv}), e.g. a previous output file name, that suffix is removed
#'   first so the file gets a fresh stamp.
#' @param snake_case Logical, default \code{FALSE}. If \code{TRUE}, every
#'   \code{"."} in the \code{aru.suntimes} column names is replaced by
#'   \code{"_"} (\code{snake.headers()}), in both the returned data frame and
#'   the CSV (e.g. \code{$date.monitoringnight} becomes
#'   \code{$date_monitoringnight}). The \code{efficiency} table is not
#'   changed.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplify or remove non-ASCII characters in each loaded file (see
#'   Details). Files are always read with a UTF-8/Latin-1 fallback.
#' @param sunregion.update Logical, default \code{FALSE}. \code{TRUE} fills
#'   in or works out \code{$sunregion.longitude} / \code{$sunregion.latitude}
#'   for each \code{$sunregion} before the calculation (see Details, "Sun
#'   region coordinates"). \code{FALSE} uses them as given.
#' @param sunregion.list Optional, default \code{NULL}. Fixed sun regions: a
#'   data frame, or the path of a CSV (by convention
#'   \verb{<project.name>.fixedsunregions.csv}), with \code{$aru.name},
#'   \code{$sunregion}, \code{$sunregion.longitude},
#'   \code{$sunregion.latitude}, \code{$date.start} and \code{$date.end}
#'   (headers matched ignoring case and "." / "_" / spaces). See Details,
#'   "Fixed sun regions".
#'
#' @return Invisibly, a list with:
#'   \describe{
#'     \item{aru.suntimes}{Data frame, one row per ARU and date
#'       (\code{"fixed.unique"}/\code{"fixed.pooled"} rows only), with
#'       \code{$aru.name}, \code{$date} (Date), \code{$date.monitoringnight}
#'       (\code{$date} at 12:00:00, POSIXct, labels the night starting at
#'       sunset on \code{$date}), \code{$sunregion},
#'       \code{$sunregion.longitude}, \code{$sunregion.latitude},
#'       \code{$date.start}, \code{$date.end}, \code{$time.zone},
#'       \code{$sunregion.type}, \code{$schedule1}, \code{$schedule2},
#'       \code{$latitude}, \code{$longitude}, \code{$sunset} and
#'       \code{$sunrise} (local clock time on \code{$date}, text),
#'       \code{$sunrise.monitoringnight} (local sunrise on the following
#'       day, text), and \code{$sunset.unix}, \code{$sunrise.unix},
#'       \code{$sunrise.monitoringnight.unix} (seconds since 1970-01-01
#'       UTC). Snake_case names if \code{snake_case = TRUE}.}
#'     \item{arulist}{The ARU list as used (after \code{sunregion.update}),
#'       one row per ARU.}
#'     \item{efficiency}{One-row data frame: \code{$aru.date.rows} (ARU-date
#'       rows), \code{$site.date_rows.count} (unique site-date rows
#'       calculated), \code{$shared.sites_count} (sites shared by more than
#'       one ARU).}
#'   }
#'   With \code{write.output = TRUE}, \code{aru.suntimes} is also written to
#'   a CSV (see Details).
#'
#' @examples
#' \dontrun{
#' result <- batz.generate_suntimes.arulist(dir.load = "path/to/data")
#' result$aru.suntimes
#' result$efficiency
#'
#' # snake_case output headers
#' result <- batz.generate_suntimes.arulist(dir.load = "path/to/data",
#'                                          project.name = "myproject",
#'                                          snake_case = TRUE)
#' }
#'
#' @export
batz.generate_suntimes.arulist <- function(dir.load = getwd(),
                                    load.pattern = "*arulist.csv",
                                    dir.sub = FALSE,
                                    write.output = TRUE,
                                    dir.save = getwd(),
                                    project.name = "",
                                    snake_case = FALSE,
                                    strip.special = TRUE,
                                    sunregion.update = FALSE,
                                    sunregion.list = NULL) {
  fixed <- if (is.null(sunregion.list)) NULL else suntimes.read.sunregion.list(sunregion.list, strip.special)

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

  ## special characters (per Josh, 2026-09-30): each file is read with the
  ## shared UTF-8/Latin-1 fallback (special.read.csv) so a stray Latin-1
  ## degree byte can't stop it loading, then stripped of non-ASCII
  ## characters (special.strip) when strip.special = TRUE, immediately
  ## after loading and before any header check. Stripped columns are put
  ## back to character so every column still arrives as character, exactly
  ## as colClasses = "character" intends (numeric conversion happens below).
  aru.list <- do.call(rbind, lapply(aru.files, function(f) {
    df <- special.read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    s  <- special.strip(df, strip.special)
    df <- s$df
    for (h in s$headers) df[[h]] <- as.character(df[[h]])
    df
  }))

  ## header standardization (per Josh, 2026-09-14 project preference): the
  ## raw *arulist.csv's own headers are standardized (trim, collapse
  ## non-alphanumeric runs to a single underscore, lowercase) before the
  ## required-header check below runs - see @details "Header
  ## standardization" above for why this cascades into required.headers
  ## and this function's own returned column names.
  names(aru.list) <- standardize.headers(names(aru.list))

  ## ===========================================================================
  ## required-header check (added 2026-08-26, per Josh). Message text is
  ## Josh's own, used verbatim. Round twenty-six (2026-09-25, per Josh): the
  ## raw *arulist.csv itself must have columns that standardize to
  ## "aru_name"/"longitude"/"latitude"/"sunregion_longitude"/
  ## "sunregion_latitude" - a real change from the old "aru"/"long"/"lat"/
  ## "sunregion_long"/"sunregion_lat" spellings (unlike the 2026-09-22
  ## $aru->$aru.name follow-up, which was internal/output-only and left the
  ## raw file's own required spelling as bare "aru") - see @details, "Round
  ## twenty-six".
  ##
  ## Follow-up, 2026-09-28, per Josh ("treat \".\" the same as \"_\" when
  ## checking if the required headers are there"): this check now goes
  ## through the shared canonicalize.headers() helper
  ## (batz.util_standardize.headers.R) instead of a plain setdiff() against
  ## already-standardized names - canonicalize.headers() standardizes BOTH
  ## required.headers AND aru.list's own names before comparing (so a dot
  ## or an underscore in either one is treated as equivalent), then renames
  ## every matched column to required.headers' own spelling. This is what
  ## lets required.headers below use this function's normal dot-separated
  ## convention (aru.name/date.start/date.end/sunregion.longitude/
  ## sunregion.latitude) directly, without breaking real-file compatibility
  ## - see @details, "Follow-up, 2026-09-28" and "Reference-workbook
  ## Change.to pass".
  ## ===========================================================================
  required.headers <- c("aru.name", "longitude", "latitude", "sunregion", "sunregion.longitude",
                         "sunregion.latitude", "date.start", "date.end", "time.zone")
  canon <- canonicalize.headers(aru.list, required.headers)
  if (length(canon$missing) > 0) {
    stop("input file is missing these headers: ", paste(canon$missing, collapse = ", "))
  }
  aru.list <- canon$df
  ## optional headers (2026-11-06, per Josh): blank when absent
  optional.headers <- c("sunregion.type", "schedule1", "schedule2", "sunregion_date.start", "sunregion_date.end")
  aru.list <- canonicalize.headers(aru.list, optional.headers)$df
  for (h in optional.headers) {
    if (!h %in% names(aru.list)) aru.list[[h]] <- rep("", nrow(aru.list))
    v <- aru.list[[h]]; v[is.na(v)] <- ""; aru.list[[h]] <- v
  }
  ## $sunregion.type absent/blank: "fixed.unique" when $sunregion = $aru.name, else "fixed.pooled"
  rk <- function(x) gsub("\\s+", "", tolower(as.character(x)))
  no.type <- !nzchar(trimws(aru.list$sunregion.type))
  if (any(no.type)) {
    aru.list$sunregion.type[no.type] <- ifelse(rk(aru.list$sunregion[no.type]) == rk(aru.list$aru.name[no.type]),
                                               "fixed.unique", "fixed.pooled")
    cat("NOTE: $sunregion.type set from $sunregion for", sum(no.type), "row(s) (fixed.unique =",
        sum(aru.list$sunregion.type[no.type] == "fixed.unique"), ", fixed.pooled =",
        sum(aru.list$sunregion.type[no.type] == "fixed.pooled"), ").\n")
  }

  aru.list$latitude  <- as.numeric(aru.list$latitude)
  aru.list$longitude <- as.numeric(aru.list$longitude)
  aru.list$sunregion.longitude <- as.numeric(aru.list$sunregion.longitude)
  aru.list$sunregion.latitude  <- as.numeric(aru.list$sunregion.latitude)
  if (isTRUE(sunregion.update)) aru.list <- suntimes.sunregion.update(aru.list)
  no.xy <- is.na(aru.list$sunregion.longitude) | is.na(aru.list$sunregion.latitude)
  if (any(no.xy)) cat("NOTE:", sum(no.xy), "row(s) have no $sunregion.longitude/$sunregion.latitude - their sun times are blank",
                      "(sunregion.update = TRUE fills them in):", paste(aru.list$aru.name[no.xy], collapse = ", "), "\n\n")
  aru.list$date.start <- parse.simple.date(aru.list$date.start)
  aru.list$date.end   <- parse.simple.date(aru.list$date.end)
  ## dates sun times are calculated for: $sunregion_date.start/end, else $date.start/end
  rs <- parse.simple.date(aru.list$sunregion_date.start)
  re <- parse.simple.date(aru.list$sunregion_date.end)
  use.r <- !is.na(rs) & !is.na(re)
  aru.list$calc.start <- aru.list$date.start; aru.list$calc.end <- aru.list$date.end
  aru.list$calc.start[use.r] <- rs[use.r]; aru.list$calc.end[use.r] <- re[use.r]
  bad <- is.na(aru.list$calc.start) | is.na(aru.list$calc.end) | aru.list$calc.end < aru.list$calc.start
  if (any(bad)) {
    cat("NOTE:", sum(bad), "row(s) excluded - no usable date range:",
        paste(aru.list$aru.name[bad], collapse = ", "), "\n\n")
    aru.list <- aru.list[!bad, , drop = FALSE]
    if (nrow(aru.list) == 0) stop("No rows have a usable date range - nothing to generate.")
  }

  ## ===========================================================================
  ## $sunregion.type (renamed from $sunregion_type, 2026-09-29 - see @details,
  ## "Merge, 2026-09-29"). Required (enforced by the header check above), so
  ## no more default-when-absent fallback. Only "fixed.unique"/"fixed.pooled"
  ## rows are kept; every other value (mobile.*, or a typo) is EXCLUDED with a
  ## NOTE instead of stopping the whole run - a real behavior change from the
  ## previous version, which hard-stopped on any mobile.* row.
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

  ## light data-entry sanity check (not requested, added to match the
  ## $sunregion.type-consistency NOTE above): flag a $sunregion whose
  ## $sunregion.longitude/$sunregion.latitude aren't identical across every
  ## row that shares it - not enforced/blocking, since the calculation
  ## below uses each row's own value directly (no averaging happens
  ## anymore)
  coord.per.region <- aggregate(cbind(n.long = sunregion.longitude, n.lat = sunregion.latitude) ~ sunregion,
                                 data = aru.list, FUN = function(x) length(unique(x)))
  mixed.coords <- coord.per.region$sunregion[coord.per.region$n.long > 1 | coord.per.region$n.lat > 1]
  if (length(mixed.coords) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion.longitude/$sunregion.latitude across their ARUs:",
        paste(mixed.coords, collapse = ", "), "\n\n")
  }

  ## resolve the lat/long actually used for the solar calculation (updated
  ## 2026-08-26, per Josh): $sunregion.longitude/$sunregion.latitude are now
  ## used DIRECTLY for every kept row (both fixed.unique and fixed.pooled) -
  ## see @details above for the behavior change from the previous
  ## exact-ARU-coords/computed-mean split. (Purely-internal calc.lat/
  ## calc.long variable names left unchanged - see @details, "Round
  ## twenty-six".)
  aru.list.used <- aru.list[, setdiff(names(aru.list), c("calc.start", "calc.end")), drop = FALSE]
  aru.list$calc.lat  <- aru.list$sunregion.latitude
  aru.list$calc.long <- aru.list$sunregion.longitude

  ## ===========================================================================
  ## expand each ARU row to one row per date in its range
  ## ===========================================================================
  expand.one <- function(i) {
    row <- aru.list[i, ]
    dates <- seq(row$calc.start, row$calc.end, by = "day")
    data.frame(
      aru.name = row$aru.name, sunregion = row$sunregion,
      sunregion.type = row$sunregion.type,
      latitude = row$latitude, longitude = row$longitude,
      sunregion.longitude = row$sunregion.longitude, sunregion.latitude = row$sunregion.latitude,
      calc.lat = row$calc.lat, calc.long = row$calc.long,
      date.start = row$date.start, date.end = row$date.end,
      schedule1 = row$schedule1, schedule2 = row$schedule2,
      time.zone = row$time.zone, date = dates,
      stringsAsFactors = FALSE
    )
  }
  aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))
  if (!is.null(fixed)) aru.expand <- suntimes.apply.sunregion.list(aru.expand, fixed)

  ## ===========================================================================
  ## efficiency step - collapse to the distinct (sunregion, calc.lat,
  ## calc.long, time_zone, date) combinations actually needed. ARUs sharing
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

  ## next-day look-ahead needed for $sunrise.monitoringnight
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
    aru.name             = aru.expand$aru.name,
    date                 = aru.expand$date,
    date.monitoringnight = as.POSIXct(paste(aru.expand$date, "12:00:00")),
    sunregion            = aru.expand$sunregion,
    sunregion.longitude  = aru.expand$sunregion.longitude,
    sunregion.latitude   = aru.expand$sunregion.latitude,
    date.start           = aru.expand$date.start,
    date.end             = aru.expand$date.end,
    time.zone            = aru.expand$time.zone,
    sunregion.type       = aru.expand$sunregion.type,
    schedule1            = aru.expand$schedule1,
    schedule2            = aru.expand$schedule2,
    latitude             = aru.expand$latitude,
    longitude            = aru.expand$longitude,
    stringsAsFactors = FALSE
  )
  aru.suntimes$sunset                       <- format.local(today$sunset.utc, aru.expand$time.zone)
  aru.suntimes$sunset.unix                  <- as.numeric(today$sunset.utc)
  aru.suntimes$sunrise                      <- format.local(today$sunrise.utc, aru.expand$time.zone)
  aru.suntimes$sunrise.unix                 <- as.numeric(today$sunrise.utc)
  aru.suntimes$sunrise.monitoringnight      <- format.local(nextd$sunrise.utc, aru.expand$time.zone)
  aru.suntimes$sunrise.monitoringnight.unix <- as.numeric(nextd$sunrise.utc)

  efficiency <- data.frame(
    aru.date.rows      = nrow(aru.expand),
    site.date_rows.count = nrow(site.dates),
    shared.sites_count = n.shared.sites
  )

  if (write.output) {
    # rounding is applied only here, at the final save step - never to
    # intermediate values or to the object returned to R (below)
    aru.suntimes.out <- aru.suntimes
    aru.suntimes.out$sunset.unix                  <- round(aru.suntimes.out$sunset.unix)
    aru.suntimes.out$sunrise.unix                 <- round(aru.suntimes.out$sunrise.unix)
    aru.suntimes.out$sunrise.monitoringnight.unix <- round(aru.suntimes.out$sunrise.monitoringnight.unix)
    # ---- resolve the output file names ----------------------------------
    # <base>_suntimes_<DATE1>to<DATE2>_<timestamp>.csv - see @param
    # project.name above. Round twenty-two, 2026-09-24, per Josh: the
    # filetype token ("suntimes") moved to second position and the "sav"
    # prefix in front of the timestamp was dropped - see @details, "File
    # naming (round twenty-two)". Strip any previously-auto-generated
    # NEW-format suffix off a user-given `project.name` first, so feeding
    # a prior stamped output file name back in re-stamps rather than
    # stacking a second stamp on top of the first.
    strip.autoname <- function(x) {
      x <- sub("\\.csv$", "", x, ignore.case = TRUE)
      x <- sub("_[0-9]{14}$", "", x, ignore.case = TRUE)
      x <- sub("_[0-9]{8}to[0-9]{8}$", "", x, ignore.case = TRUE)
      x <- sub("_suntimes$", "", x, ignore.case = TRUE)
      x
    }
    base.name <- if (identical(project.name, "")) "aru" else strip.autoname(project.name)

    date1     <- format(min(aru.suntimes$date), "%Y%m%d")
    date2     <- format(max(aru.suntimes$date), "%Y%m%d")

    out.file <- paste0(base.name, "_suntimes_", date1, "to", date2, "_",
                        format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")

    ## snake_case (per Josh, 2026-09-22, project-wide audit/extension of the
    ## snake_case output option - see @details) is applied here, to a copy
    ## used only for the CSV write, AFTER the rounding above (which still
    ## references this function's own dot-separated $sunset.unix/
    ## $sunrise.unix/$sunrise.monitoringnight.unix names regardless of this
    ## parameter).
    if (snake_case) names(aru.suntimes.out) <- snake.headers(names(aru.suntimes.out))

    write.csv(aru.suntimes.out, file.path(dir.save, out.file), row.names = FALSE)
  }

  ## snake_case applied to the invisibly-returned aru.suntimes itself, as
  ## the very last step before it's returned (kept separate from the
  ## write.output copy above, since that copy is also rounded/renamed on
  ## its own timeline) - see @details, "Follow-up, 2026-09-22".
  if (snake_case) names(aru.suntimes) <- snake.headers(names(aru.suntimes))

  invisible(list(aru.suntimes = aru.suntimes, arulist = aru.list.used, efficiency = efficiency))
}


#' Fill in / work out sun-region coordinates (internal)
#'
#' Per $sunregion: all pairs blank, or every row a different pair -> the mean
#' $longitude / $latitude of the region's rows (8 decimals) for every row;
#' one pair, or a pair shared by 2+ rows -> rows keep theirs, blank rows get
#' the (most common) pair. Prints a NOTE per region changed.
#' @keywords internal
#' @noRd
suntimes.sunregion.update <- function(a) {
  lon <- suppressWarnings(as.numeric(a$sunregion.longitude)); lat <- suppressWarnings(as.numeric(a$sunregion.latitude))
  blank <- is.na(lon) | is.na(lat)
  rlon <- suppressWarnings(as.numeric(a$longitude)); rlat <- suppressWarnings(as.numeric(a$latitude))
  reg <- trimws(as.character(a$sunregion))
  for (r in unique(reg)) {
    i <- which(reg == r)
    pair <- ifelse(blank[i], NA, paste(lon[i], lat[i]))
    tab <- table(pair[!is.na(pair)])
    if (length(tab) == 0 || (length(tab) > 1 && all(tab == 1))) {
      cl <- round(mean(rlon[i], na.rm = TRUE), 8); ct <- round(mean(rlat[i], na.rm = TRUE), 8)
      if (is.nan(cl) || is.nan(ct)) { cat("NOTE: sunregion '", r, "' - no $longitude/$latitude to work out a central point; left as is.\n", sep = ""); next }
      lon[i] <- cl; lat[i] <- ct
      cat(sprintf("NOTE: sunregion '%s' - %s: central point of its %d ARU(s) (%s, %s) used for all.\n", r,
                  if (length(tab) == 0) "no coordinates" else "every ARU had different coordinates", length(i), cl, ct))
    } else {
      keep <- names(tab)[which.max(tab)]
      j <- i[blank[i]]
      if (!length(j)) next
      kp <- as.numeric(strsplit(keep, " ")[[1]])
      lon[j] <- kp[1]; lat[j] <- kp[2]
      cat(sprintf("NOTE: sunregion '%s' - %d ARU(s) with blank coordinates given the region's (%s, %s): %s.\n", r, length(j),
                  kp[1], kp[2], paste(a$aru.name[j], collapse = ", ")))
    }
  }
  a$sunregion.longitude <- lon; a$sunregion.latitude <- lat
  a
}


#' Read and check a fixed sun-region list (internal)
#'
#' Returns the list with $line (file line: row + 1), $k (aru.name key),
#' $d0 / $d1 (dates, -Inf / Inf when blank). Stops on overlapping
#' different sun regions for one aru.name.
#' @keywords internal
#' @noRd
suntimes.read.sunregion.list <- function(x, strip.special = TRUE) {
  FN <- "batz.generate_suntimes.arulist"
  src <- "sunregion.list"
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) stop(FN, ": sunregion.list file not found: ", x, call. = FALSE)
    src <- basename(x)
    x <- special.read.csv(x, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  }
  if (!is.data.frame(x)) stop(FN, ": sunregion.list must be a data frame or the path of a CSV.", call. = FALSE)
  x <- special.strip(x, strip.special)$df
  need <- c("aru.name", "sunregion", "sunregion.longitude", "sunregion.latitude", "date.start", "date.end")
  ch <- canonicalize.headers(x, need)
  if (length(ch$missing)) stop(FN, ": ", src, " is missing these headers: ", paste(ch$missing, collapse = ", "), call. = FALSE)
  x <- ch$df[, need]
  for (h in need) { v <- trimws(as.character(x[[h]])); v[is.na(v) | toupper(v) == "NA"] <- ""; x[[h]] <- v }
  x$line <- seq_len(nrow(x)) + 1L
  x <- x[nzchar(x$aru.name) & nzchar(x$sunregion), , drop = FALSE]
  if (!nrow(x)) stop(FN, ": ", src, " has no rows with both $aru.name and $sunregion.", call. = FALSE)
  key <- function(v) gsub("[^a-z0-9]", "", tolower(v))
  pd <- function(v) {
    out <- rep(as.Date(NA), length(v))
    for (fm in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y")) { todo <- is.na(out) & nzchar(v); if (any(todo)) out[todo] <- as.Date(v[todo], format = fm, optional = TRUE) }
    out
  }
  d0 <- pd(x$date.start); d1 <- pd(x$date.end)
  bad <- (nzchar(x$date.start) & is.na(d0)) | (nzchar(x$date.end) & is.na(d1))
  if (any(bad)) stop(FN, ": ", src, " - unreadable date(s) on line(s) ", paste(x$line[bad], collapse = ", "),
                     " (use m/d/Y, Y-m-d or m/d/y).", call. = FALSE)
  x$d0 <- ifelse(is.na(d0), -Inf, as.numeric(d0)); x$d1 <- ifelse(is.na(d1), Inf, as.numeric(d1))
  x$lon <- suppressWarnings(as.numeric(x$sunregion.longitude)); x$lat <- suppressWarnings(as.numeric(x$sunregion.latitude))
  x$k <- key(x$aru.name); x$src <- src
  ## conflicts: one aru.name, different sun regions, overlapping dates
  msg <- character(0)
  for (k in unique(x$k)) {
    i <- which(x$k == k)
    if (length(i) < 2) next
    for (a in i) for (b in i) if (a < b && key(x$sunregion[a]) != key(x$sunregion[b]) && x$d0[a] <= x$d1[b] && x$d0[b] <= x$d1[a])
      msg <- c(msg, sprintf("  aru.name '%s': sunregion '%s' (%s to %s, line %d) and '%s' (%s to %s, line %d)",
                            x$aru.name[a], x$sunregion[a], suntimes.dtxt(x$date.start[a]), suntimes.dtxt(x$date.end[a]), x$line[a],
                            x$sunregion[b], suntimes.dtxt(x$date.start[b]), suntimes.dtxt(x$date.end[b]), x$line[b]))
  }
  if (length(msg)) stop(FN, ": ", src, " gives an aru.name more than one sunregion for overlapping dates - edit these lines:\n",
                        paste(msg, collapse = "\n"), call. = FALSE)
  x
}

#' Blank date shown as "any date" (internal)
#' @keywords internal
#' @noRd
suntimes.dtxt <- function(v) ifelse(nzchar(v), v, "any date")


#' Apply a fixed sun-region list to the per-date ARU rows (internal)
#' @keywords internal
#' @noRd
suntimes.apply.sunregion.list <- function(e, fx) {
  FN <- "batz.generate_suntimes.arulist"
  key <- function(v) gsub("[^a-z0-9]", "", tolower(as.character(v)))
  dn <- as.numeric(e$date)
  hit <- rep(NA_integer_, nrow(e))
  msg <- character(0)
  for (r in seq_len(nrow(fx))) {
    i <- which((key(e$aru.name) == fx$k[r] | key(e$sunregion) == fx$k[r]) & dn >= fx$d0[r] & dn <= fx$d1[r])
    if (!length(i)) next
    clash <- i[!is.na(hit[i]) & key(fx$sunregion[hit[i]]) != key(fx$sunregion[r])]
    if (length(clash)) {
      for (q in unique(hit[clash])) {
        j <- clash[hit[clash] == q]
        msg <- c(msg, sprintf("  aru.name '%s' (sunregion '%s' in the ARU list), %s to %s: sunregion '%s' (line %d, aru.name '%s') and '%s' (line %d, aru.name '%s')",
                              e$aru.name[j[1]], e$sunregion[j[1]], format(min(e$date[j])), format(max(e$date[j])),
                              fx$sunregion[q], fx$line[q], fx$aru.name[q], fx$sunregion[r], fx$line[r], fx$aru.name[r]))
      }
      next
    }
    hit[i] <- r
  }
  if (length(msg)) stop(FN, ": ", fx$src[1], " gives an ARU more than one sunregion for the same dates (through its aru.name and its sunregion) - edit these lines:\n",
                        paste(unique(msg), collapse = "\n"), call. = FALSE)
  i <- which(!is.na(hit))
  if (!length(i)) { cat("NOTE: sunregion.list matched no ARU in the ARU list.\n"); return(e) }
  moved <- unique(paste0(e$aru.name[i], " -> ", fx$sunregion[hit[i]]))
  e$sunregion[i] <- fx$sunregion[hit[i]]
  e$sunregion.longitude[i] <- fx$lon[hit[i]]
  e$sunregion.latitude[i] <- fx$lat[hit[i]]
  ## blank coordinates: same sun region and date (most common pair), else the region's central point
  nb <- i[is.na(e$sunregion.longitude[i]) | is.na(e$sunregion.latitude[i])]
  if (length(nb)) {
    ok <- !is.na(e$sunregion.longitude) & !is.na(e$sunregion.latitude) & !seq_len(nrow(e)) %in% nb
    for (g in unique(paste(key(e$sunregion[nb]), dn[nb]))) {
      j <- nb[paste(key(e$sunregion[nb]), dn[nb]) == g]
      same <- which(ok & key(e$sunregion) == key(e$sunregion[j[1]]) & dn == dn[j[1]])
      if (length(same)) {
        pr <- table(paste(e$sunregion.longitude[same], e$sunregion.latitude[same]))
        pp <- as.numeric(strsplit(names(pr)[which.max(pr)], " ")[[1]])
      } else {
        reg <- which(key(e$sunregion) == key(e$sunregion[j[1]]))
        u <- unique(e[reg, c("aru.name", "longitude", "latitude")])
        pp <- c(round(mean(as.numeric(u$longitude), na.rm = TRUE), 8), round(mean(as.numeric(u$latitude), na.rm = TRUE), 8))
      }
      e$sunregion.longitude[j] <- pp[1]; e$sunregion.latitude[j] <- pp[2]
    }
  }
  e$calc.long[i] <- e$sunregion.longitude[i]; e$calc.lat[i] <- e$sunregion.latitude[i]
  e$sunregion.type[i] <- ifelse(key(e$sunregion[i]) == key(e$aru.name[i]), "fixed.unique", "fixed.pooled")
  cat("NOTE: sunregion.list - ", length(moved), " ARU sun region(s) set: ", paste(moved, collapse = "; "), "\n", sep = "")
  e
}

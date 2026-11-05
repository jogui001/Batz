#' Summarize vetted bat-acoustic detections into a plotting-ready frame
#'
#' Turns a data frame of vetted bat-acoustic detections (the output of
#' \code{\link{batz.merge_vetted.acoustics}}) into a plotting-ready summary,
#' \code{plfr.batsummary}. For each species, time pool (by default one
#' monitoring night) and group (by default each detector) it counts the
#' detections and gives the first and last call time as minutes from noon of
#' the monitoring night. It also lists the detectors involved and adds each
#' detector's \code{$sunregion}, looked up from an \verb{*arulist.csv} file.
#'
#' @param data A data frame of vetted detections with the required columns
#'   listed in Details. Headers may use "." or "_" (e.g.
#'   \code{date_monitoringnight}); they are matched with
#'   \code{canonicalize.headers()} and renamed to the dot spelling in the
#'   function's local copy only.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact duplicate
#'   rows from \code{data} before summarizing.
#' @param spp.id Character, default \code{"manid.sb"}. Name of the column
#'   holding the species ID to summarize by. Blank values (empty, \code{NA} or
#'   spaces only) become \code{"NOID"}. Exception: when \code{spp.id} is
#'   \code{"manid.sb"} and \code{trim.noid = TRUE}, rows with a blank value
#'   are removed from \code{data} instead.
#' @param groupby.date Character, default \code{"date.monitoringnight"}. Name
#'   of the date column to summarize by. If the name contains
#'   \code{"monitoringnight"}, each day runs noon to noon; otherwise midnight
#'   to midnight.
#' @param groupby Character, default \code{"aru.name"}. Name of the column to
#'   group by (e.g. \code{"aru.name"} for each detector, or
#'   \code{"deployment.type"}).
#' @param alldetections Logical, default \code{TRUE}. If \code{TRUE}, adds
#'   \code{"All Detections"} rows (species combined) to the per-species rows.
#'   If \code{FALSE}, returns the per-species rows only.
#' @param trim.noise Logical, default \code{TRUE}. When
#'   \code{alldetections = TRUE}, leave records whose species ID is
#'   \code{"noise"} (any case) out of the \code{"All Detections"} rows. They
#'   still appear in the per-species rows.
#' @param trim.noid Logical, default \code{FALSE}. When
#'   \code{alldetections = TRUE}, leave records whose species ID is
#'   \code{"NoID"} (any case) out of the \code{"All Detections"} rows. When
#'   \code{TRUE} and \code{spp.id} is \code{"manid.sb"}, records with a blank
#'   species ID are also removed from \code{data} entirely.
#' @param dir.load Character, default \code{getwd()}. Directory to search for
#'   the \verb{*arulist.csv} file(s) used to look up \code{$sunregion}.
#' @param load.pattern Character vector, default \code{c("*.arulist.csv")}.
#'   Wildcard (glob) pattern(s) for the arulist file(s), converted with
#'   \code{utils::glob2rx()}. Matching ignores case. All matching files are
#'   used.
#' @param dir.sub Logical, default \code{FALSE}. Also search subfolders of
#'   \code{dir.load}.
#' @param snake_case Logical, default \code{FALSE}. If \code{TRUE}, every "."
#'   in the output column names is replaced by "_" (e.g.
#'   \code{$mins2.noon.min} becomes \code{$mins2_noon_min}). Applied as the
#'   last step.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE}, accented
#'   letters and common symbols are simplified (accented letters to plain
#'   letters, the multiplication sign to \code{X}, curly quotes to straight
#'   quotes) and other non-ASCII characters (e.g. degree and micro signs) are
#'   removed from column names and values of \code{data} (local copy) and of
#'   each arulist file. A changed column is re-typed the way \code{read.csv()}
#'   would (a latitude saved as 42.5 plus a degree sign becomes \code{42.5}).
#'   Arulist files are always read as UTF-8 with a Latin-1 fallback, whatever
#'   this is set to.
#' @param pool.interval Character, default \code{"1day"}. Length of each pool
#'   that detections are counted over: a number plus a unit, with or without
#'   a space. Minutes (\code{"15 min"}, \code{"20min"}), hours
#'   (\code{"1 hour"}, \code{"2 hr"}), days (\code{"1day"}, \code{"2 day"}) or
#'   weeks (\code{"1 week"}). At least 1 minute; intervals under a day must be
#'   whole minutes, intervals over a day whole days.
#' @param pool.start Character, default \code{"observed"}. Used only when
#'   \code{pool.interval} is longer than one day: where the first pool starts.
#'   \code{"observed"} = the earliest date in the data; a date
#'   \code{"YYYY-MM-DD"}; a month (\code{"\%m = 5"}, \code{"may"},
#'   \code{"May"}) = the 1st of that month each year; or \code{"julian"} =
#'   January 1st each year.
#' @param detection.timing Character, default \code{"1 day"}. The window that
#'   \code{$mins2.noon.min}/\code{$mins2.noon.max} (first/last detection) are
#'   taken over. Same format as \code{pool.interval}. Must be no more than
#'   \code{"1 day"}, no less than \code{pool.interval}, and (if under a day) a
#'   whole multiple of \code{pool.interval}. Ignored when \code{pool.interval}
#'   is longer than a day.
#'
#' @details
#' \strong{Required columns in data.} \code{$filename},
#' \code{$date.monitoringnight}, \code{$manid}, \code{$autoid.kp},
#' \code{$autoid.sb}, \code{$lat}, \code{$aru.serial}, \code{$longitude},
#' \code{$aru.name}, \code{$date}, \code{$time} and \code{$call.datetime}.
#' The function stops and lists any that are missing. The columns named by
#' \code{spp.id}, \code{groupby.date} and \code{groupby} must also exist.
#' \code{$sunregion} is not needed in \code{data}; any existing value is
#' replaced by the arulist lookup.
#'
#' \strong{Serial numbers.} The old header \code{$serial} is still accepted:
#' \itemize{
#'   \item Both \code{$aru.serial} and \code{$serial} present: blank
#'     \code{$aru.serial} values are filled from \code{$serial}; where both
#'     have data and differ, \code{$aru.serial} is used. A WARNING gives the
#'     rows missing from each and/or the rows that did not match. \code{$serial}
#'     is then dropped.
#'   \item Only \code{$serial} present: it is used as \code{$aru.serial}, with
#'     a WARNING.
#'   \item Neither present: \code{$aru.serial} is added with the value
#'     \code{"missing"}, with a WARNING.
#' }
#' In every case a WARNING gives the number of rows with no serial number
#' (blank or \code{"missing"}).
#'
#' \strong{$sunregion lookup.} Files in \code{dir.load} matching
#' \code{load.pattern} are read with \code{special.read.csv()} and their
#' headers standardized with \code{standardize.headers()}. Each file needs an
#' ARU-name column (\code{aru}, \code{aru_name} or \code{aru.name}) and a
#' \code{sunregion} column; files without both (or that can't be read) are
#' skipped with a message. The usable files are stacked and
#' \code{data$aru.name} is matched to the arulist ARU name (first match wins).
#' \code{$aru.name} values with no match get \code{NA} and are listed in a
#' warning. The function stops if no file is found, or none has both columns.
#'
#' \strong{Steps.}
#' \enumerate{
#'   \item Drop exact duplicate rows (if \code{duplicates.remove = TRUE}) and
#'     set \code{$observations.count = 1} on every row.
#'   \item Calculate each call's minutes from noon of its \code{groupby.date}
#'     date. The date format is detected by
#'     \code{\link{batz.datawrangler_call.datetime}}. \code{$call.datetime} may
#'     be \code{Y-m-d H:M(:S)}, \code{m/d/Y H:M(:S)} or \code{m/d/y H:M(:S)};
#'     the first format that matches every non-blank value is used, otherwise
#'     the function stops.
#'   \item Put each record in a pool. A one-day pool is the
#'     \code{groupby.date} date. Shorter pools split each day into equal bins
#'     from the day start (\code{"2 hour"} on a monitoring night gives
#'     12:00-14:00, 14:00-16:00, ...). Longer pools are blocks of whole days
#'     counted from \code{pool.start}; with a month or \code{"julian"} start
#'     the count restarts each year and the last pool before the restart is
#'     cut short.
#'   \item Put each record in a \code{detection.timing} window (the whole day,
#'     equal bins from the day start, or the pool itself when pools are longer
#'     than a day; a NOTE is printed if \code{detection.timing} is then
#'     ignored).
#'   \item Build one row per species ID, pool and group. Only pools with at
#'     least one detection get a row. The function stops if one row has more
#'     than one distinct \code{$sunregion}.
#'   \item If \code{alldetections = TRUE}, drop \code{"noise"}/\code{"NoID"}
#'     records (per \code{trim.noise}/\code{trim.noid}) from a copy, summarize
#'     it again with the species ID set to \code{"All Detections"}, stack both
#'     tables and sort by \code{$group}, \code{$date}, \code{$pool.start} and
#'     \code{$spp.id}.
#' }
#'
#' \strong{Detectors.} \code{$aru.count} counts detectors by serial number; a
#' record with no usable serial (blank or \code{"missing"}) is counted by its
#' \code{$aru.name}.
#'
#' Nothing is written to disk and no object is assigned.
#'
#' @return A data frame, \code{plfr.batsummary}, with one row per species ID
#'   (plus \code{"All Detections"}), pool and group:
#'   \describe{
#'     \item{\code{$spp.id}}{Species ID from the \code{spp.id} column, or
#'       \code{"All Detections"}.}
#'     \item{\code{$date}}{The \code{groupby.date} value for pools of a day or
#'       less; the pool start date for longer pools.}
#'     \item{\code{$pool.interval}}{Pool length label, e.g. \code{"1 day"},
#'       \code{"2 hour"}, \code{"15 min"}.}
#'     \item{\code{$detection.timing}}{Label of the first/last-detection window
#'       (the pool label when pools are longer than a day).}
#'     \item{\code{$pool.start}, \code{$pool.end}}{Start and end of the pool as
#'       \code{"YYYY-MM-DD HH:MM:SS"} text (recorder clock time).}
#'     \item{\code{$group}}{Value of the \code{groupby} column, e.g. a detector
#'       name.}
#'     \item{\code{$groupedby}}{Name of the \code{groupby} column (same on every
#'       row).}
#'     \item{\code{$groupby.date}}{Name of the \code{groupby.date} column (same
#'       on every row).}
#'     \item{\code{$aru.name}, \code{$aru.serial}}{Every detector name/serial
#'       with a detection in the row, sorted and separated by \code{"; "}.}
#'     \item{\code{$aru.count}}{Number of unique detectors.}
#'     \item{\code{$sunregion}}{Sun region from the arulist (\code{NA} if not
#'       found).}
#'     \item{\code{$observations.count}}{Number of detections in the pool.}
#'     \item{\code{$mins2.noon.min}, \code{$mins2.noon.max}}{Earliest and latest
#'       call, in minutes from noon of the call's monitoring night, over the
#'       \code{detection.timing} window.}
#'     \item{\code{$vetting.type}}{Name of the \code{spp.id} column (same on
#'       every row).}
#'   }
#'   Column names use "_" instead of "." if \code{snake_case = TRUE}.
#'
#' @examples
#' \dontrun{
#' # looks for "*.arulist.csv" in the working directory to get $sunregion
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged)
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged, alldetections = FALSE)
#'
#' # arulist file in another folder
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged,
#'   dir.load = "C:/path/to/arulist/folder")
#'
#' # group by deployment type, 2-hour pools, first/last detection per night
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged,
#'   groupby = "deployment.type", pool.interval = "2 hour")
#'
#' # weekly pools starting May 1st each year
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged,
#'   pool.interval = "1 week", pool.start = "May")
#'
#' # snake_case output headers
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged, snake_case = TRUE)
#' }
#'
#' @seealso \code{\link{batz.merge_vetted.acoustics}},
#'   \code{\link{batz.generate_plotframe.bat.group}},
#'   \code{\link{batz.generate_suntimes.arulist}}
#'
#' @export
batz.generate_plotframe.bat <- function(data,
                                         duplicates.remove = TRUE,
                                         spp.id = "manid.sb",
                                         groupby.date = "date.monitoringnight",
                                         groupby = "aru.name",
                                         alldetections = TRUE,
                                         trim.noise = TRUE,
                                         trim.noid = FALSE,
                                         dir.load = getwd(),
                                         load.pattern = c("*.arulist.csv"),
                                         dir.sub = FALSE,
                                         snake_case = FALSE,
                                         strip.special = TRUE,
                                         pool.interval = "1day",
                                         pool.start = "observed",
                                         detection.timing = "1 day") {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  ## ---- pooling (2026-10-02, per Josh) - see @details "Pooling" ----------
  pool.mins <- plotframe.parse.interval(pool.interval, "pool.interval")
  pool.label <- plotframe.interval.label(pool.mins)

  ## ---- detection.timing (2026-10-07, per Josh) - see @details
  ## "detection.timing". The window $mins2.noon.min/$mins2.noon.max are
  ## taken over. Must be <= 1 day and >= pool.interval.
  timing.mins <- plotframe.parse.interval(detection.timing, "detection.timing")
  if (timing.mins > 1440) {
    stop("`detection.timing` = \"", detection.timing, "\" may not be greater than \"1 day\".")
  }
  if (pool.mins <= 1440) {
    if (timing.mins < pool.mins) {
      stop("`detection.timing` = \"", detection.timing, "\" may not be less than `pool.interval` (\"",
           pool.interval, "\").")
    }
    if (timing.mins < 1440 && timing.mins %% pool.mins != 0) {
      stop("`detection.timing` = \"", detection.timing, "\" must be a whole multiple of `pool.interval` (\"",
           pool.interval, "\") so each pool sits inside one detection.timing window.")
    }
    timing.label <- plotframe.interval.label(timing.mins)
  } else {
    ## pools longer than a day: one row covers several days, so first/last
    ## detection is taken over the whole pool (same as before this option)
    if (timing.mins != 1440) {
      cat("NOTE: pool.interval is longer than 1 day - detection.timing is ignored; ",
          "$mins2.noon.min/$mins2.noon.max cover the whole pool.\n", sep = "")
    }
    timing.label <- pool.label
  }
  ## monitoring-night dates start at noon, calendar dates at midnight
  pool.anchor.hour <- if (grepl("monitoringnight", groupby.date, ignore.case = TRUE)) 12 else 0

  ## special characters (per Josh, 2026-09-30): strip non-ASCII characters
  ## from `data` (this function's own local copy only) before any header
  ## check, matching or summarizing - see @details, "Follow-up, 2026-09-30".
  data <- special.strip(data, strip.special)$df

  ## ---- $aru.serial / old $serial header (2026-10-07, per Josh) - see
  ## @details "$aru.serial and the old $serial header" and
  ## plotframe.reconcile.serial() at the bottom of this file.
  data <- plotframe.reconcile.serial(data)

  ## $sunregion is NOT in this list (and no longer needs to already be a
  ## column of `data`) - it's now loaded from an *arulist.csv file and
  ## joined on below, replacing the old "join it in yourself first"
  ## requirement. See Details/Follow-up. These twelve names are `data`'s
  ## own already-established batz output schema (from
  ## batz.merge_vetted.acoustics()), not raw loaded headers.
  ##
  ## BUGFIX/NEW (2026-09-21, per Josh): matched via canonicalize.headers()
  ## rather than a plain setdiff() - both `data`'s real column names and
  ## required.headers below are standardized to snake_case purely to match
  ## them up (so a `data` whose headers came back snake_cased from some
  ## intervening save/reload step is still recognized), then every matched
  ## column of `data` is renamed, in this function's own local copy only,
  ## back to the exact dot-separated spelling below - everything after
  ## this check keeps referencing $date.monitoringnight/$aru.name/etc. exactly as
  ## before. See @details, "Header standardization"/"BUGFIX/NEW".
  required.headers <- c("filename", "date.monitoringnight", "manid", "autoid.kp",
                         "autoid.sb", "lat", "aru.serial", "longitude", "aru.name",
                         "date", "time", "call.datetime")
  data.canon <- canonicalize.headers(data, required.headers)
  if (length(data.canon$missing) > 0) {
    stop("`data` is missing required header(s): ",
         paste(data.canon$missing, collapse = ", "))
  }
  data <- data.canon$df

  for (colname in c(spp.id, groupby.date, groupby)) {
    if (!(colname %in% names(data))) {
      stop("`", colname, "` (from spp.id/groupby.date/groupby) is not ",
           "a column of `data`.")
    }
  }

  ## Follow-up (2026-09-22, per Josh: "spp.id = 'manid.sb' if value = ''
  ## then replace with 'NOID'"). A blank $spp.id value (empty string, or
  ## NA/whitespace-only after coercion to character) is standardized to
  ## the literal string "NOID" before anything downstream reads it - so a
  ## blank value participates as its own explicit category (matching the
  ## existing case-insensitive "NoID" recognized by trim.noid, and every
  ## per-species/$vetting.type breakdown) instead of silently blending
  ## into whatever key an empty string happens to produce. See @details,
  ## "Follow-up, 2026-09-22...blank $spp.id" below.
  spp.id.vals <- as.character(data[[spp.id]])
  is.blank.spp.id <- is.na(spp.id.vals) | !nzchar(trimws(spp.id.vals))

  ## Follow-up (2026-09-23, per Josh: "if spp.id = manid.sb and trim.noid =
  ## TRUE then remove any records that are blank"). A narrower, opt-in
  ## alternative to the blanket NOID fill-in just above: when `spp.id` is
  ## still specifically the default column name ("manid.sb") AND
  ## `trim.noid = TRUE`, a blank value in that column means the row is
  ## dropped from `data` entirely - before any summarizing - rather than
  ## folded into a "NOID" category. This is NOT the same thing as the
  ## pre-existing $trim.noise/$trim.noid mechanism further below (the
  ## `trimmed <- ...` block), which only excludes "noise"/"NoID" rows from
  ## the collapsed "All Detections" copy while leaving them in the
  ## per-species breakdown: this removal deletes the rows from `data`
  ## itself, so they disappear from BOTH the per-species table and the
  ## "All Detections" total. Every other combination of spp.id/trim.noid
  ## (a non-default spp.id column, or trim.noid = FALSE) keeps the
  ## unconditional NOID fill-in exactly as before. See @details,
  ## "Follow-up, 2026-09-23...remove blank $manid.sb records" below.
  if (identical(spp.id, "manid.sb") && isTRUE(trim.noid)) {
    data <- data[!is.blank.spp.id, , drop = FALSE]
    spp.id.vals <- spp.id.vals[!is.blank.spp.id]
  } else {
    spp.id.vals[is.blank.spp.id] <- "NOID"
  }
  data[[spp.id]] <- spp.id.vals

  ## --- load $sunregion from an *arulist.csv file and join it onto `data`
  ## by matching `data$aru.name` against the arulist file's own $aru.name
  ## column (always $aru.name specifically, regardless of what groupby
  ## points to - groupby can be overridden to an unrelated column like
  ## "serial", which wouldn't correspond to the arulist's $aru.name values
  ## at all) - see Details/Follow-up ----------------------------------------
  ## header standardization (per Josh, 2026-09-14 project preference): the
  ## arulist file's own headers are real loaded-file headers, so they're
  ## run through the shared package helper standardize.headers() instead
  ## of this function's prior local, more lenient ad hoc normalizer - see
  ## @details "Header standardization" above.
  ##
  ## Follow-up (2026-09-22, per Josh: "change all functions that have aru
  ## as an header to \code{"aru.name"}"): the loaded file itself is still
  ## checked for a column that standardizes to literal "aru" (unchanged -
  ## no real *arulist.csv file needs to be edited for this), but that
  ## column is renamed to $aru.name in this function's own local lookup
  ## table immediately after the check, so the join below reads
  ## $aru.name on both sides - see @details, "Follow-up, 2026-09-22...aru
  ## as an header".
  ##
  ## Follow-up (2026-09-25, round twenty-six second-order fix, per Josh's
  ## package-wide suntimes header rename - see @details, "Follow-up,
  ## 2026-09-25...raw *arulist.csv loader now accepts either $aru or
  ## $aru_name"): the raw file is now checked for EITHER a standardized
  ## "aru" OR "aru_name" column (not "aru" alone), since the real
  ## *arulist.csv test-data file(s) are separately being renamed to the
  ## same aru.name/aru_name spelling batz.generate_suntimes.arulist() now
  ## requires. Whichever one is present is used and renamed to $aru.name
  ## in this function's own local lookup table exactly as before.
  arulist.regex <- paste(utils::glob2rx(load.pattern), collapse = "|")
  arulist.files <- list.files(dir.load, pattern = arulist.regex, recursive = dir.sub,
                               full.names = TRUE, ignore.case = TRUE)
  if (length(arulist.files) == 0) {
    stop("No file matching `load.pattern` (\"", paste(load.pattern, collapse = ", "),
         "\") found in `dir.load` (\"", dir.load, "\", dir.sub = ", dir.sub, ") - ",
         "an arulist file is required to look up $sunregion.")
  }

  arulist <- data.frame(aru.name = character(0), sunregion = character(0), stringsAsFactors = FALSE)
  arulist.skipped <- character(0)
  for (f in arulist.files) {
    tmp <- tryCatch(special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { arulist.skipped <- c(arulist.skipped, paste0(f, " (could not read file)")); next }
    tmp <- special.strip(tmp, strip.special)$df
    names(tmp) <- standardize.headers(names(tmp))
    aru.col.candidates <- intersect(c("aru", "aru_name"), names(tmp))
    if (length(aru.col.candidates) == 0 || !("sunregion" %in% names(tmp))) {
      arulist.skipped <- c(arulist.skipped, paste0(f, " (missing $aru/$aru_name and/or $sunregion column)")); next
    }
    aru.col <- aru.col.candidates[1]
    tmp.sub <- tmp[, c(aru.col, "sunregion"), drop = FALSE]
    names(tmp.sub)[names(tmp.sub) == aru.col] <- "aru.name"
    arulist <- rbind(arulist, tmp.sub)
  }
  if (length(arulist.skipped) > 0) {
    message("batz.generate_plotframe.bat: skipped arulist file(s) that didn't have ",
            "both an $aru/$aru_name and $sunregion column: ", paste(arulist.skipped, collapse = "; "))
  }
  if (nrow(arulist) == 0) {
    stop("Found ", length(arulist.files), " file(s) matching `load.pattern` in `dir.load`, ",
         "but none had both an `$aru`/`$aru_name` and `$sunregion` column - cannot look up $sunregion.")
  }

  data$sunregion <- arulist$sunregion[match(data$aru.name, arulist$aru.name)]
  unmatched.arus <- unique(data$aru.name[is.na(data$sunregion)])
  if (length(unmatched.arus) > 0) {
    warning("$aru.name value(s) not found in the loaded arulist - $sunregion will be NA for: ",
            paste(unmatched.arus, collapse = ", "))
  }

  if (duplicates.remove) {
    data <- data[!duplicated(data), , drop = FALSE]
  }

  data$observations.count <- 1

  ## noon-of-monitoring-night anchor, reusing batz.datawrangler_call.datetime()'s
  ## own date-format auto-detection
  noon.anchor <- as.POSIXct(
    batz.datawrangler_call.datetime(date = as.character(data[[groupby.date]]),
                                     time = rep("120000", nrow(data))),
    tz = "UTC")

  ## $call.datetime format auto-detection - regex-guarded candidate list, same
  ## exact-full-string-match pattern already used in
  ## batz.datawrangler_call.datetime to avoid a silent partial-match
  call.datetime.candidates <- list(
    list(fmt = "%Y-%m-%d %H:%M:%S", rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%Y-%m-%d %H:%M",    rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}$")
  )
  parse.call.datetime <- function(x) {
    x <- trimws(as.character(x))
    present <- x[!is.na(x) & nzchar(x)]
    chosen <- NULL
    if (length(present) > 0) {
      for (cand in call.datetime.candidates) {
        if (!all(grepl(cand$rx, present))) next
        parsed <- as.POSIXct(present, format = cand$fmt, tz = "UTC")
        if (any(is.na(parsed))) next
        chosen <- cand$fmt
        break
      }
      if (is.null(chosen)) {
        stop("Could not auto-detect the format of $call.datetime - none of the ",
             "built-in candidate formats (",
             paste(vapply(call.datetime.candidates, function(c) c$fmt, character(1)), collapse = ", "),
             ") matched every non-blank value.")
      }
    }
    as.POSIXct(x, format = chosen, tz = "UTC")
  }
  call.dt <- parse.call.datetime(data$call.datetime)
  data$.mins2.noon <- as.numeric(difftime(call.dt, noon.anchor, units = "mins"))

  ## ---- pool each record (2026-10-02) ----------------------------------
  ## .pool.key   - grouping key (with spp.id and groupby)
  ## .pool.date  - value shown in the output $date column
  ## .pool.start / .pool.end - start/end of the pooled period (POSIXct, UTC
  ##               clock - i.e. the recorder's own local clock time)
  day.anchor <- noon.anchor - (12 - pool.anchor.hour) * 3600
  date.str <- as.character(data[[groupby.date]])
  if (pool.mins == 1440) {
    data$.pool.key   <- date.str
    data$.pool.date  <- date.str
    data$.pool.start <- day.anchor
    data$.pool.end   <- day.anchor + 86400
  } else if (pool.mins < 1440) {
    offset <- as.numeric(difftime(call.dt, day.anchor, units = "mins"))
    bin <- floor(offset / pool.mins)
    data$.pool.key   <- paste(date.str, bin)
    data$.pool.date  <- date.str
    data$.pool.start <- day.anchor + bin * pool.mins * 60
    data$.pool.end   <- data$.pool.start + pool.mins * 60
  } else {
    n.days <- pool.mins / 1440
    d <- as.Date(format(day.anchor, "%Y-%m-%d"))
    origin <- plotframe.pool.origin(pool.start, d)
    idx <- floor(as.numeric(d - origin$origin) / n.days)
    p.start <- origin$origin + idx * n.days
    p.end <- p.start + n.days
    if (!is.null(origin$cycle.end)) p.end <- pmin(p.end, origin$cycle.end)
    data$.pool.key   <- as.character(p.start)
    data$.pool.date  <- as.character(p.start)
    data$.pool.start <- as.POSIXct(paste(p.start, sprintf("%02d:00:00", pool.anchor.hour)), tz = "UTC")
    data$.pool.end   <- as.POSIXct(paste(p.end, sprintf("%02d:00:00", pool.anchor.hour)), tz = "UTC")
  }
  ## ---- detection.timing window each record falls in (2026-10-07) ----
  ## .timing.key - window $mins2.noon.min/$mins2.noon.max are taken over
  if (pool.mins >= 1440) {
    data$.timing.key <- data$.pool.key
  } else if (timing.mins == 1440) {
    data$.timing.key <- date.str
  } else {
    timing.offset <- as.numeric(difftime(call.dt, day.anchor, units = "mins"))
    data$.timing.key <- paste(date.str, floor(timing.offset / timing.mins))
  }

  fmt.dt <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

  safe.min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe.max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  ## $sunregion is a per-detector attribute (now joined onto `data` above,
  ## from the loaded arulist, by $aru.name), so every row within a single
  ## spp.id/groupby.date/groupby group should already agree on it
  ## whenever groupby == "aru.name" (the default). Collapsed with a
  ## consistency check
  ## rather than silently taking the first value, so a real data problem
  ## (e.g. groupby overridden to a column that doesn't line up 1:1
  ## with $sunregion) surfaces as a clear error instead of a silently
  ## arbitrary pick.
  safe.sunregion <- function(x, group.label) {
    present <- unique(x[!is.na(x) & nzchar(trimws(x))])
    if (length(present) == 0) return(NA_character_)
    if (length(present) > 1) {
      stop("`$sunregion` has more than one distinct value (",
           paste(present, collapse = ", "), ") within a single ",
           "spp.id/groupby.date/groupby group (", group.label, ") - ",
           "sunregion is expected to be constant per groupby.")
    }
    present[[1]]
  }

  build.summary <- function(df, spp.override = NULL) {
    spp.vals <- if (!is.null(spp.override)) rep(spp.override, nrow(df)) else as.character(df[[spp.id]])
    date.vals  <- as.character(df$.pool.key)
    group.vals <- as.character(df[[groupby]])
    sun.vals   <- as.character(df$sunregion)

    key <- paste(spp.vals, date.vals, group.vals, sep = "\r")
    first.of <- function(v) { v <- v[!duplicated(key)]; names(v) <- key[!duplicated(key)]; v }
    k.date  <- first.of(as.character(df$.pool.date))
    k.start <- first.of(fmt.dt(df$.pool.start))
    k.end   <- first.of(fmt.dt(df$.pool.end))
    ag.obs <- tapply(df$observations.count, key, sum)

    ## first/last detection over each record's detection.timing window
    ## (2026-10-07) - same spp.id/groupby, window from .timing.key
    tkey  <- paste(spp.vals, as.character(df$.timing.key), group.vals, sep = "\r")
    t.min <- tapply(df$.mins2.noon, tkey, safe.min)
    t.max <- tapply(df$.mins2.noon, tkey, safe.max)
    k.tkey <- first.of(tkey)

    ## detectors in each row (2026-10-07): every $aru.name / $aru.serial
    ## with a detection in that spp.id/pool/group, and how many detectors.
    ## A detector is counted by its serial; a record with no usable serial
    ## (blank or "missing") is counted by its $aru.name instead.
    serial.vals <- trimws(as.character(df$aru.serial))
    name.vals   <- trimws(as.character(df$aru.name))
    usable.serial <- !is.na(serial.vals) & nzchar(serial.vals) & tolower(serial.vals) != "missing"
    detector.id <- ifelse(usable.serial, paste0("serial:", serial.vals),
                          ifelse(!is.na(name.vals) & nzchar(name.vals), paste0("name:", name.vals), NA_character_))
    collapse.unique <- function(v) {
      v <- sort(unique(v[!is.na(v) & nzchar(v)]))
      if (length(v) == 0) NA_character_ else paste(v, collapse = "; ")
    }
    ag.serial <- tapply(serial.vals, key, collapse.unique)
    ag.name   <- tapply(name.vals, key, collapse.unique)
    ag.count  <- tapply(detector.id, key, function(v) length(unique(v[!is.na(v)])))

    keys  <- names(ag.obs)
    parts <- strsplit(keys, "\r", fixed = TRUE)

    ## computed per-key (not via tapply) so a stop() from safe.sunregion()
    ## names the actual offending spp.id/date/groupby combination in a
    ## human-readable form (the raw key itself is \r-joined and unreadable
    ## if ever printed)
    group.labels <- vapply(parts, function(p) sprintf("spp.id=%s, date=%s, groupby=%s", p[1], p[2], p[3]), character(1))
    names(group.labels) <- keys
    ag.sun <- vapply(keys, function(k) safe.sunregion(sun.vals[key == k], group.label = group.labels[[k]]),
                      character(1))

    out <- data.frame(
      spp.id         = vapply(parts, `[`, character(1), 1),
      date           = as.character(k.date[keys]),
      pool.interval  = pool.label,
      detection.timing = timing.label,
      pool.start     = as.character(k.start[keys]),
      pool.end       = as.character(k.end[keys]),
      group          = vapply(parts, `[`, character(1), 3),
      groupedby      = groupby,
      groupby.date   = groupby.date,
      aru.name       = as.character(ag.name[keys]),
      aru.serial     = as.character(ag.serial[keys]),
      aru.count      = as.integer(ag.count[keys]),
      sunregion      = as.character(ag.sun[keys]),
      observations.count = as.numeric(ag.obs[keys]),
      mins2.noon.min = as.numeric(t.min[k.tkey[keys]]),
      mins2.noon.max = as.numeric(t.max[k.tkey[keys]]),
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    out
  }

  if (!alldetections) {
    plfr.batsummary <- build.summary(data)
    plfr.batsummary$vetting.type <- spp.id
    if (snake_case) names(plfr.batsummary) <- snake.headers(names(plfr.batsummary))
    return(plfr.batsummary)
  }

  ## alldetections = TRUE: per-species breakdown (on all rows, pre-trim)
  ## + a collapsed "All Detections" summary on the noise/NoID-trimmed
  ## data, combined into one table - see Details
  species.summary <- build.summary(data)

  trimmed <- data
  if (trim.noise) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noise"), , drop = FALSE]
  }
  if (trim.noid) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noid"), , drop = FALSE]
  }

  all.summary <- build.summary(trimmed, spp.override = "All Detections")

  plfr.batsummary <- rbind(species.summary, all.summary)
  plfr.batsummary$vetting.type <- spp.id
  plfr.batsummary <- plfr.batsummary[order(plfr.batsummary$group,
                                            plfr.batsummary$date,
                                            plfr.batsummary$pool.start,
                                            plfr.batsummary$spp.id), ]
  rownames(plfr.batsummary) <- NULL

  if (snake_case) names(plfr.batsummary) <- snake.headers(names(plfr.batsummary))

  plfr.batsummary
}


#' Pooling helpers for batz.generate_plotframe.bat() (internal)
#'
#' \code{plotframe.parse.interval()} turns text such as \code{"1day"},
#' \code{"2 day"}, \code{"1 hour"}, \code{"2hr"}, \code{"15 min"},
#' \code{"20min"} or \code{"1 week"} into minutes, and stops on a value that
#' is not recognized, under 1 minute, or not whole minutes/days.
#' \code{plotframe.interval.label()} turns minutes back into a label
#' (\code{"1 day"}, \code{"2 hour"}, \code{"15 min"}).
#' \code{plotframe.pool.origin()} turns \code{pool.start} into the date
#' multi-day pools are counted from (and, for a month or \code{"julian"}, the
#' yearly restart date).
#' @keywords internal
#' @noRd
plotframe.parse.interval <- function(x, arg = "pool.interval") {
  if (!is.character(x) || length(x) != 1 || is.na(x)) stop("`", arg, "` must be one text value, e.g. \"1day\", \"2 hour\" or \"15 min\".")
  s <- tolower(gsub("\\s+", "", x))
  m <- regmatches(s, regexec("^([0-9]*\\.?[0-9]+)?(minutes|minute|mins|min|m|hours|hour|hrs|hr|h|days|day|d|weeks|week|wks|wk|w)$", s))[[1]]
  if (length(m) == 0) stop("`", arg, "` = \"", x, "\" not recognized - use a number and a unit, e.g. \"1day\", \"2 day\", \"1 hour\", \"15 min\" or \"1 week\".")
  n <- if (nzchar(m[2])) as.numeric(m[2]) else 1
  unit <- substr(m[3], 1, 1)
  mins <- n * switch(unit, m = 1, h = 60, d = 1440, w = 10080)
  if (!is.finite(mins) || mins <= 0) stop("`", arg, "` must be greater than zero.")
  if (mins < 1) stop("`", arg, "` = \"", x, "\" may not be less than \"1 minute\".")
  if (mins > 1440 && mins %% 1440 != 0) stop("`", arg, "` = \"", x, "\": intervals longer than a day must be whole days (e.g. \"2 day\", \"1 week\").")
  if (mins < 1440 && abs(mins - round(mins)) > 1e-9) stop("`", arg, "` = \"", x, "\": intervals shorter than a day must be whole minutes.")
  mins
}

#' @keywords internal
#' @noRd
plotframe.interval.label <- function(mins) {
  if (mins %% 1440 == 0) paste(mins / 1440, "day")
  else if (mins %% 60 == 0) paste(mins / 60, "hour")
  else paste(mins, "min")
}

#' @keywords internal
#' @noRd
plotframe.pool.origin <- function(pool.start, d) {
  if (!is.character(pool.start) || length(pool.start) != 1 || is.na(pool.start)) stop("`pool.start` must be one text value.")
  ps <- trimws(pool.start); low <- tolower(ps)
  if (all(is.na(d))) stop("No usable dates to pool.")
  if (low == "observed") return(list(origin = min(d, na.rm = TRUE)))
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", ps)) {
    o <- as.Date(ps)
    if (is.na(o)) stop("`pool.start` = \"", ps, "\" is not a valid date.")
    return(list(origin = o))
  }
  month <- NA_integer_
  if (low == "julian") month <- 1L
  mm <- regmatches(low, regexec("^%m\\s*=\\s*<?\\s*([0-9]{1,2})\\s*>?$", low))[[1]]
  if (length(mm) == 2) month <- as.integer(mm[2])
  if (is.na(month)) {
    hit <- which(substr(tolower(month.name), 1, 3) == substr(low, 1, 3) &
                 startsWith(tolower(month.name), low))
    if (length(hit) == 1 && nchar(low) >= 3) month <- hit
  }
  if (is.na(month) || month < 1 || month > 12) {
    stop("`pool.start` = \"", ps, "\" not recognized - use \"observed\", a date \"YYYY-MM-DD\", ",
         "a month (\"%m = 1\", \"jan\" or \"January\") or \"julian\".")
  }
  ## restart every year on the 1st of that month
  y <- as.integer(format(d, "%Y"))
  start.this <- as.Date(sprintf("%04d-%02d-01", y, month))
  y <- ifelse(!is.na(d) & d < start.this, y - 1L, y)
  origin <- as.Date(sprintf("%04d-%02d-01", y, month))
  cycle.end <- as.Date(sprintf("%04d-%02d-01", y + 1L, month))
  list(origin = origin, cycle.end = cycle.end)
}


#' $aru.serial / old $serial reconciliation for batz.generate_plotframe.bat() (internal)
#'
#' \code{$aru.serial} is the current header; \code{$serial} is the old one.
#' Returns \code{data} with exactly one \code{$aru.serial} column (and no
#' \code{$serial}), printing a WARNING for each case - see "Serial numbers" in
#' the Details of \code{batz.generate_plotframe.bat()}.
#' @keywords internal
#' @noRd
plotframe.reconcile.serial <- function(data) {
  std <- standardize.headers(names(data))
  new.idx <- which(std == "aru_serial")
  old.idx <- which(std == "serial")
  is.blank <- function(x) is.na(x) | !nzchar(trimws(x))

  if (length(new.idx) > 0 && length(old.idx) > 0) {
    new.vals <- as.character(data[[new.idx[1]]])
    old.vals <- as.character(data[[old.idx[1]]])
    new.blank <- is.blank(new.vals)
    old.blank <- is.blank(old.vals)
    n.missing.new <- sum(new.blank & !old.blank)
    n.missing.old <- sum(old.blank & !new.blank)
    n.diff <- sum(!new.blank & !old.blank & trimws(new.vals) != trimws(old.vals))

    if (n.missing.new > 0 || n.missing.old > 0) {
      cat(sprintf(paste0("WARNING: Check input file, it has both $aru.serial & $serial headers.\n",
                         "  %d row(s) of data are missing from $aru.serial\n",
                         "  %d row(s) of data are missing from $serial\n"),
                  n.missing.new, n.missing.old))
    }
    if (n.diff > 0) {
      cat(sprintf(paste0("WARNING: Check input file, it has both $aru.serial & $serial headers. There were\n",
                         "  %d row(s) of data which did not match between the two, defaulted to $aru.serial for those records.\n"),
                  n.diff))
    }
    if (n.missing.new == 0 && n.missing.old == 0 && n.diff == 0) {
      cat("WARNING: Check input file, it has both $aru.serial & $serial headers (their values match - $aru.serial was used).\n")
    }

    ## fill each blank $aru.serial from $serial; where both have data,
    ## $aru.serial wins
    new.vals[new.blank] <- old.vals[new.blank]
    data[[new.idx[1]]] <- new.vals
    names(data)[new.idx[1]] <- "aru.serial"
    data <- data[, -old.idx, drop = FALSE]
  } else if (length(old.idx) > 0) {
    names(data)[old.idx[1]] <- "aru.serial"
    cat("WARNING: Check input file, $aru.serial was absent, but the old header $serial was present - that was used instead.\n")
  } else if (length(new.idx) > 0) {
    names(data)[new.idx[1]] <- "aru.serial"
  } else {
    data$aru.serial <- rep("missing", nrow(data))
    cat("WARNING: input file was missing $aru.serial - added it with the value \"missing\" as a placeholder. ",
        "Check input file to allow later QA/QC.\n", sep = "")
  }

  vals <- as.character(data$aru.serial)
  n.no.serial <- sum(is.blank(vals) | tolower(trimws(vals)) == "missing")
  if (n.no.serial > 0) {
    cat(sprintf("WARNING: %d row(s) of data did not have a serial number.\n", n.no.serial))
  }

  data
}

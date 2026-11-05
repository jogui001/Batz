#' Merge and format raw temperature-logger exports
#'
#' Merges raw \verb{*templog.csv} datalogger exports (with or without a
#' header row, varying column counts, date formats and encodings) into one
#' standardized data frame. Joins in station metadata from
#' \code{templog.meta.csv}, calculates relative humidity for humidity
#' loggers, drops records outside each unit's deployment/recovery window,
#' and returns a per-file log. Both tables can be written to CSV.
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param load.pattern Character vector of length 2, default
#'   \code{c("*templog.csv", "*templog.meta.csv")}. Wildcard (glob) patterns
#'   for (1) the raw datalogger files and (2) the station-metadata file(s).
#'   Files matching the second pattern are never treated as raw files.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   every subdirectory of \code{dir.load}. Set it to \code{TRUE} if
#'   \code{templog.meta.csv} sits in a subfolder (e.g. \code{meta files/}),
#'   or it will not be found.
#' @param dir.save Character. Directory the two CSVs are written to when
#'   \code{write.output = TRUE}. Default \code{getwd()}.
#' @param write.output Logical, default \code{TRUE}. If \code{TRUE}, write
#'   \code{templog.notes} and \code{templog.merged} to CSV in
#'   \code{dir.save} (see Value). \code{$rh} is rounded to 3 decimal places
#'   in the written file only.
#' @param project.name Character, default \code{""}. First token of both
#'   output file names. \code{""} uses \code{"templog"}.
#' @param snake_case Logical, default \code{FALSE}. If \code{TRUE}, every
#'   \code{"."} in the output column names of both tables (and the CSVs) is
#'   replaced by \code{"_"} (\code{$date.time} -> \code{$date_time}). The
#'   special-character log columns (\code{$strip.special},
#'   \code{$accented.letters.header}, ...) keep their names.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols are simplified (\code{é} -> \code{e},
#'   \code{×} -> \code{X}, curly quotes -> straight quotes) and other
#'   non-ASCII characters (e.g. \code{°}, \code{µ}) are removed, in each
#'   meta file and raw file right after loading. Files are always read as
#'   UTF-8 with a Latin-1 fallback, whatever this is set to. Temperature-unit
#'   detection always uses the original header row, so this never changes
#'   \code{$temp.type}.
#'
#' @return Invisibly, a list of two data frames:
#'   \describe{
#'     \item{templog.merged}{One row per kept record: \code{$observations.count}
#'       (row number within the file), \code{$date.time} (POSIXct,
#'       America/New_York), \code{$temp.dry.c}, \code{$temp.wet.c}, \code{$rh},
#'       \code{$logger.serial}, then every column of the matched meta row
#'       except the join key (e.g. \code{$logger.serial_short},
#'       \code{$logger.type}, \code{$station.code}, \code{$date.deployment},
#'       \code{$time.deployment}, \code{$date.recovery},
#'       \code{$time.recovery}, \code{$room.code}, \code{$room.name}; other
#'       meta columns keep their standardized names). Meta columns are
#'       \code{NA} for a file with no confident meta match.}
#'     \item{templog.notes}{One row per raw file: \code{$logger.serial},
#'       \code{$date.start}, \code{$date.end}, \code{$temp.type}
#'       (\code{"F"}, \code{"C"} or \code{"C.default"}), \code{$rows.in},
#'       \code{$rows.out}, \code{$rows.trimmed}, \code{$notes},
#'       \code{$strip.special}, \code{$accented.letters.header},
#'       \code{$removed.symbols.header}, \code{$accented.letters.data},
#'       \code{$removed.symbols.data}.}
#'   }
#'   With \code{write.output = TRUE}, two files are written to
#'   \code{dir.save}:
#'   \verb{<project.name>_templog.notes_<daterange>_<timestamp>.csv} and
#'   \verb{<project.name>_templog.merged_<daterange>_<timestamp>.csv}.
#'   \verb{<daterange>} is \verb{<YYYYMMDD>to<YYYYMMDD>} from the earliest and
#'   latest \code{$date.time} (\code{"nodata"} if there is none);
#'   \verb{<timestamp>} is \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")}, so
#'   nothing is overwritten.
#'
#' @details
#' \strong{Meta file.} All files matching \code{load.pattern[2]} are read as
#' text, combined, and exact duplicate rows removed. Headers are
#' standardized with \code{standardize.headers()} (trimmed, lower case, runs
#' of punctuation or spaces become \code{"_"}), then renamed:
#' \itemize{
#'   \item \code{serial#} (or \code{serial.num}) -> \code{$logger.serial}:
#'     full 8-digit serial, the preferred join key.
#'   \item \code{serial.short} -> \code{$logger.serial_short}: last 3-4
#'     digits; used as the join key only when there is no full serial.
#'   \item \code{logger.type} (or \code{logger_type}) -> \code{$logger.type}:
#'     \code{"H"} (temperature + humidity) or \code{"T"} (temperature only).
#'     If missing, taken from the last letter of \code{$station.code}.
#'   \item \code{station.code} -> \code{$station.code}.
#'   \item \code{date.deployment}, \code{time.deployment},
#'     \code{date.recovery}, \code{time.recovery} -> same names with dots
#'     (dates \code{m/d/Y}, times \code{H:M:S}).
#'   \item \code{room.number} -> \code{$room.code}; \code{room.name} ->
#'     \code{$room.name}.
#' }
#'
#' \strong{Raw files.} For each file matching \code{load.pattern[1]}:
#' \itemize{
#'   \item The logger serial is the first 8 digits of the file name
#'     (\code{NA} if none).
#'   \item Empty lines are dropped; columns past the fourth are trimmed
#'     (noted in \code{$notes}).
#'   \item A leading \code{"Plot Title"} row is dropped, and rows with both
#'     temperature columns blank are dropped. \code{$rows.in} and
#'     \code{$rows.out} count rows before and after this cleanup.
#'   \item A first row containing \code{"date"} is treated as the header.
#'     \code{°F}/\code{F}/\code{fahrenheit} in it gives \code{$temp.type =
#'     "F"} (values converted to Celsius); \code{°C}/\code{C}/\code{celsius}
#'     gives \code{"C"}; otherwise, or with no header (\code{"no header
#'     present"} in \code{$notes}), \code{"C.default"}.
#'   \item Column 2 is the date-time. The format (\code{m/d/Y} or
#'     \code{m/d/y}, 12- or 24-hour, with or without seconds) is taken from
#'     the first non-blank value. Column 3 is the dry-bulb temperature and
#'     column 4 the wet-bulb temperature.
#' }
#'
#' \strong{Meta matching.} The file's \code{$logger.serial} is matched
#' exactly to \code{$logger.serial} in the meta table. With no full serial
#' column, the last 4 digits are matched to \code{$logger.serial_short}
#' (digits only), then the last 3 digits; a 3-digit code that also appears
#' inside a 4-digit code is skipped as ambiguous. When several meta rows
#' match, those whose deployment-recovery dates overlap the file's
#' \code{$date.start}-\code{$date.end} are kept. If the remaining rows point
#' to more than one \code{$station.code}, the match is ambiguous and no meta
#' row is used. The outcome is written to \code{$notes}.
#'
#' \strong{Relative humidity.} Based only on the matched meta row's
#' \code{$logger.type}:
#' \itemize{
#'   \item \code{"H"}: \code{$temp.wet.c} keeps column 4 and \code{$rh} is
#'     calculated from dry and wet bulb (psychrometric formula, 1013.25 hPa,
#'     clamped to 0-100).
#'   \item \code{"T"}, or no confident meta match: \code{$temp.wet.c} and
#'     \code{$rh} are \code{NA}.
#' }
#' \code{$rh} keeps full precision in the returned object.
#'
#' \strong{Deployment window.} When the matched meta row has all four of
#' \code{$date.deployment}, \code{$time.deployment}, \code{$date.recovery}
#' and \code{$time.recovery}, records with \code{$date.time} before
#' deployment or after recovery are dropped. Records with an unreadable
#' \code{$date.time} are kept. The count is in \code{$rows.trimmed} (and in
#' \code{$notes} when above 0). Otherwise nothing is trimmed.
#'
#' \strong{Special-character log.} \code{$strip.special} is \code{"FALSE"}
#' (not selected), \code{"TRUE NONE"} (nothing found) or
#' \code{"TRUE ; <header>; ..."}. Raw columns are named by their output
#' names (\code{V1} -> \code{observations.count}, \code{V2} ->
#' \code{date.time}, \code{V3} -> \code{temp.dry.c}, \code{V4} ->
#' \code{temp.wet.c}; extra columns keep \code{V5}, ...). When the file
#' matched a meta row, changed meta headers are added as
#' \code{meta:<header>}. The four count columns give the number of unique
#' column names / data values with a character simplified or removed
#' (\code{NA} when \code{strip.special = FALSE}).
#'
#' @examples
#' \dontrun{
#' result <- batz.merge_temp.logger(dir.load = "path/to/data", dir.sub = TRUE)
#' result$templog.merged
#' result$templog.notes
#'
#' # snake_case output headers
#' result <- batz.merge_temp.logger(dir.load = "path/to/data", snake_case = TRUE)
#'
#' # custom project.name token in the saved file names
#' result <- batz.merge_temp.logger(dir.load = "path/to/data", project.name = "siteA")
#' }
#'
#' @export
batz.merge_temp.logger <- function(dir.load = getwd(),
                                          load.pattern = c("*templog.csv", "*templog.meta.csv"),
                                          dir.sub = FALSE,
                                          dir.save = getwd(),
                                          write.output = TRUE,
                                          project.name = "",
                                          snake_case = FALSE,
                                          strip.special = TRUE) {

  ## ---- internal helpers ----------------------------------------------------
  ## convert a plain wildcard/glob suffix pattern (or vector of them) into one
  ## combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  rbind.fill <- function(a, b) {
    if (nrow(a) == 0) return(b)
    if (nrow(b) == 0) return(a)
    all.cols <- union(names(a), names(b))
    for (col in setdiff(all.cols, names(a))) a[[col]] <- NA
    for (col in setdiff(all.cols, names(b))) b[[col]] <- NA
    rbind(a[all.cols], b[all.cols])
  }

  ## 2026-09-30 (special characters): now wraps the shared
  ## special.read.lines() helper, which skips a UTF-8 BOM and falls back to
  ## Latin-1 for lines that aren't valid UTF-8 (the previous raw-byte reader
  ## marked every byte as UTF-8, so a Latin-1 degree byte broke later
  ## regex/header checks).
  read.raw.lines <- function(file) {
    special.read.lines(file)
  }

  f.to.c <- function(temp.f) (temp.f - 32) * 5 / 9

  sat.vapor.pressure <- function(temp.c) {
    6.1094 * exp((17.625 * temp.c) / (temp.c + 243.04))
  }

  calc.rh <- function(temp.dry.c, temp.wet.c,
                       pressure.hpa = 1013.25,
                       psychrometer.coeff = 0.000662) {
    es.wet <- sat.vapor.pressure(temp.wet.c)
    e      <- es.wet - psychrometer.coeff * pressure.hpa * (temp.dry.c - temp.wet.c)
    es.dry <- sat.vapor.pressure(temp.dry.c)
    rh <- 100 * e / es.dry
    pmin(pmax(rh, 0), 100)
    # NOTE: kept at full precision here. Rounding (3 dp) is applied only at
    # the final CSV-write step below, never to intermediate values, so that
    # $rh stays at full precision for any further calculations.
  }

  build.dt.format <- function(sample) {
    has.ampm  <- grepl("(AM|PM)$", sample, ignore.case = TRUE)
    date.part <- sub(" .*$", "", sample)
    year.4digit <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", date.part)
    date.fmt  <- if (year.4digit) "%m/%d/%Y" else "%m/%d/%y"
    time.part <- sub("^[^ ]+ ", "", sample)
    time.part <- sub(" (AM|PM)$", "", time.part, ignore.case = TRUE)
    n.colons  <- lengths(regmatches(time.part, gregexpr(":", time.part)))
    if (has.ampm) {
      time.fmt <- if (n.colons >= 2) "%I:%M:%S %p" else "%I:%M %p"
    } else {
      time.fmt <- if (n.colons >= 2) "%H:%M:%S" else "%H:%M"
    }
    paste(date.fmt, time.fmt)
  }

  parse.dt <- function(x) {
    valid <- which(!is.na(x) & nzchar(trimws(x)))
    if (length(valid) == 0) return(as.POSIXct(rep(NA_character_, length(x))))
    fmt <- build.dt.format(x[valid[1]])
    as.POSIXct(x, format = fmt, tz = "America/New_York")
  }

  ## combine a templog.meta.csv $date.deployment/$date.recovery ("m/d/Y")
  ## with its companion $time.deployment/$time.recovery ("H:M:S") into one
  ## POSIXct. Returns NA if either piece is missing/blank/unparseable.
  parse.meta.datetime <- function(date.str, time.str) {
    if (is.null(date.str) || is.null(time.str) ||
        is.na(date.str) || is.na(time.str) ||
        !nzchar(trimws(date.str)) || !nzchar(trimws(time.str))) {
      return(as.POSIXct(NA))
    }
    as.POSIXct(paste(date.str, time.str), format = "%m/%d/%Y %H:%M:%S",
               tz = "America/New_York")
  }

  is.blank <- function(x) is.na(x) | trimws(x) == ""
  deg <- "\u00b0"

  ## ===========================================================================
  ## STEP 1: load templog.meta.csv file(s), merge + de-dup
  ## ===========================================================================
  meta.files <- list.files(dir.load, pattern = pattern.regex(load.pattern[2]),
                            recursive = dir.sub, full.names = TRUE)
  meta.special.raw <- character(0)   # raw meta headers where special characters were removed
  meta.special.counts <- NULL        # simplified/removed counts across meta files (2026-10-02)

  if (length(meta.files) > 0) {
    meta.list <- lapply(meta.files, special.read.csv, stringsAsFactors = FALSE,
                         colClasses = "character", check.names = FALSE)
    ## strip special characters from each meta file right after loading
    ## (2026-09-30) - before merging/de-duplication
    for (mi in seq_along(meta.list)) {
      s.meta <- special.strip(meta.list[[mi]], strip.special)
      meta.list[[mi]] <- s.meta$df
      meta.special.raw <- c(meta.special.raw, s.meta$headers)
      meta.special.counts <- special.counts.add(meta.special.counts, s.meta$counts)
    }
    templog.meta <- Reduce(rbind.fill, meta.list)
    templog.meta <- templog.meta[!duplicated(templog.meta), ]
    ## header standardization (per Josh, 2026-09-14 project preference):
    ## templog.meta.csv's own headers are a literal, uninvented copy of the
    ## meta file's real column text, so they're standardized the same as
    ## any other loaded file's headers - see @details "Header
    ## standardization" above.
    names(templog.meta) <- standardize.headers(names(templog.meta))
  } else {
    templog.meta <- data.frame(serial_num = character(0),
                                station_code = character(0),
                                stringsAsFactors = FALSE)
  }

  ## column-identifier canonicalization (2026-09-27 rename batch, per
  ## Josh's reference workbook) - see @details, "Column identifiers
  ## renamed, 2026-09-27". Applied uniformly whether templog.meta.csv was
  ## actually found (post-standardize.headers()) or we fell back to the
  ## empty scaffold above (which is deliberately given the same legacy
  ## standardized-alias names, so this one rename step covers both cases
  ## identically). Anything standardize.headers() could have produced from
  ## a legacy dotted raw header is renamed here, once, to this function's
  ## new canonical identifier, so every reference below can use the new
  ## name directly. (\code{serial}/\code{serial_num} are handled just
  ## below instead, since only one of the two is ever the real join key.)
  rename.map <- c(date_deployment = "date.deployment",
                   date_recovery   = "date.recovery",
                   room_name       = "room.name",
                   room_number     = "room.code",
                   serial_short    = "logger.serial_short",
                   station_code    = "station.code",
                   time_deployment = "time.deployment",
                   time_recovery   = "time.recovery",
                   logger_type     = "logger.type")
  match.idx <- match(names(templog.meta), names(rename.map))
  names(templog.meta)[!is.na(match.idx)] <- rename.map[match.idx[!is.na(match.idx)]]

  ## if $logger.type doesn't exist, derive it from the last letter of
  ## $station.code (T = temp only, H = temp + humidity)
  if (nrow(templog.meta) > 0 && !"logger.type" %in% names(templog.meta)) {
    templog.meta$logger.type <- toupper(substr(templog.meta$station.code,
                                                 nchar(templog.meta$station.code),
                                                 nchar(templog.meta$station.code)))
  }

  ## --- meta matching setup -------------------------------------------------
  ## Preferred path: templog.meta.csv has a real 8-digit serial number column
  ## (seen as "serial#" in practice, standardized to "serial") - match a
  ## file's logger.serial to it EXACTLY. The same physical logger gets
  ## redeployed to different stations over time, so a serial can
  ## legitimately appear more than once; disambiguate using the file's own
  ## observed date range against each candidate row's
  ## [$date.deployment, $date.recovery] window.
  ##
  ## Fallback path: older/partial meta files only have $logger.serial_short,
  ## which is just the last 3-4 digits of the real serial (and may carry
  ## stray characters like "7273+*"). Match a file's logger.serial by
  ## comparing its last 4 digits first, falling back to the last 3 only
  ## when that 3-digit value isn't also a substring of some 4-digit code in
  ## the table (a 3-digit reading could otherwise just be a truncated
  ## 4-digit one - unresolvable, so it's skipped rather than guessed at).
  ##
  ## NOTE (2026-09-27 rename batch): the detection strings below ("serial",
  ## "serial_num") are deliberately left as-is - they are the LEGACY
  ## standardized spellings that a real incoming templog.meta.csv will
  ## still produce (standardize.headers() collapses "serial#"/"serial.num"
  ## to exactly these), i.e. input aliases. Once a match is found, the
  ## matched column is renamed to this function's new canonical
  ## \code{logger.serial} identifier below, before it's used anywhere else.
  serial.col <- (if ("serial" %in% names(templog.meta)) "serial"
                 else if ("serial_num" %in% names(templog.meta)) "serial_num"
                 else NA_character_)

  ## canonical (standardized + renamed) names of the meta headers where
  ## special characters were removed - for templog.notes$strip.special
  meta.special <- unique(standardize.headers(meta.special.raw))
  meta.special.idx <- match(meta.special, names(rename.map))
  meta.special[!is.na(meta.special.idx)] <- rename.map[meta.special.idx[!is.na(meta.special.idx)]]

  if (!is.na(serial.col)) {
    meta.special[meta.special == serial.col] <- "logger.serial"
    names(templog.meta)[names(templog.meta) == serial.col] <- "logger.serial"
    serial.col <- "logger.serial"
  }

  if (nrow(templog.meta) > 0 && is.na(serial.col) && "logger.serial_short" %in% names(templog.meta)) {
    templog.meta$serial_short_clean <- gsub("[^0-9]", "", templog.meta$logger.serial_short)
    all.4digit <- unique(templog.meta$serial_short_clean[nchar(templog.meta$serial_short_clean) == 4])
    ambiguous.3digit <- unique(templog.meta$serial_short_clean[
      nchar(templog.meta$serial_short_clean) == 3 &
      sapply(templog.meta$serial_short_clean, function(d) any(grepl(d, all.4digit, fixed = TRUE)))
    ])
  } else {
    ambiguous.3digit <- character(0)
  }

  ## if >1 meta row is a candidate (after date filtering), it's genuinely
  ## ambiguous - return no row (caller treats that like "no match") and
  ## surface all distinct candidate stations in the notes rather than
  ## silently picking one
  resolve.match <- function(match.rows, match.desc) {
    stations <- unique(match.rows$station.code)
    if (length(stations) == 1) {
      list(meta.row = match.rows[1, , drop = FALSE],
           meta.notes = paste0("meta match ", match.desc, ": ", stations))
    } else {
      list(meta.row = NULL,
           meta.notes = paste0("meta match ambiguous ", match.desc, ": could be ",
                                paste(stations, collapse = " or "), " - check templog.meta.csv"))
    }
  }

  ## restrict candidate meta rows to ones whose deployment window overlaps
  ## the file's own observed date range (when both are available); if that
  ## leaves nothing, fall back to the full candidate set
  filter.by.date <- function(match.rows, date.start, date.end) {
    if (nrow(match.rows) <= 1 || is.na(date.start) || is.na(date.end) ||
        !all(c("date.deployment", "date.recovery") %in% names(match.rows))) {
      return(match.rows)
    }
    dep <- as.Date(match.rows$date.deployment, format = "%m/%d/%Y")
    rec <- as.Date(match.rows$date.recovery, format = "%m/%d/%Y")
    f.start <- as.Date(date.start)
    f.end   <- as.Date(date.end)
    keep <- !is.na(dep) & !is.na(rec) & !(f.end < dep | f.start > rec)
    if (any(keep)) match.rows[keep, , drop = FALSE] else match.rows
  }

  ## column names to append from a matched meta row - everything except
  ## whatever the join actually used to find it (serial# duplicates
  ## $logger.serial already; the internal .clean helper isn't real meta data)
  meta.append.cols <- setdiff(names(templog.meta), c(serial.col, "serial_short_clean"))

  ## look up the single matching meta row (if any) + a diagnostic note, for
  ## one file's logger.serial / observed date range
  lookup.meta <- function(serial.num, date.start, date.end) {
    no.match <- list(meta.row = NULL, meta.notes = NA_character_)
    if (nrow(templog.meta) == 0 || is.na(serial.num)) return(no.match)

    if (!is.na(serial.col)) {
      match.rows <- templog.meta[templog.meta[[serial.col]] == serial.num, ]
      if (nrow(match.rows) == 0) {
        return(list(meta.row = NULL,
                    meta.notes = "no meta match found (serial# not in templog.meta.csv)"))
      }
      match.rows <- filter.by.date(match.rows, date.start, date.end)
      return(resolve.match(match.rows, paste0("on serial# (", serial.num, ")")))
    }

    ## fallback: logger.serial_short suffix matching (only used when the
    ## meta file has no real serial column at all)
    if (!"logger.serial_short" %in% names(templog.meta)) return(no.match)
    last4 <- substr(serial.num, nchar(serial.num) - 3, nchar(serial.num))
    last3 <- substr(serial.num, nchar(serial.num) - 2, nchar(serial.num))

    match.rows <- templog.meta[templog.meta$serial_short_clean == last4, ]
    if (nrow(match.rows) > 0) {
      match.rows <- filter.by.date(match.rows, date.start, date.end)
      return(resolve.match(match.rows, paste0("on last 4 digits (", last4, ")")))
    }
    if (last3 %in% ambiguous.3digit) {
      return(list(meta.row = NULL,
                  meta.notes = paste0("meta match skipped: last 3 digits (", last3,
                                       ") ambiguous with a 4-digit logger.serial_short")))
    }
    match.rows <- templog.meta[templog.meta$serial_short_clean == last3, ]
    if (nrow(match.rows) > 0) {
      match.rows <- filter.by.date(match.rows, date.start, date.end)
      return(resolve.match(match.rows, paste0("on last 3 digits (", last3, ")")))
    }
    list(meta.row = NULL, meta.notes = "no meta match found")
  }

  ## ===========================================================================
  ## STEP 2: set up templog.notes and the merged output frame
  ## ===========================================================================
  templog.notes <- data.frame(
    logger.serial = character(0), date.start = character(0),
    date.end = character(0), temp.type = character(0),
    rows.in = integer(0), rows.out = integer(0), rows.trimmed = integer(0),
    notes = character(0), special.log.cols.empty(),
    stringsAsFactors = FALSE
  )

  templog.merged <- data.frame(
    observations.count = integer(0), date.time = as.POSIXct(character(0)),
    temp.dry.c = numeric(0), temp.wet.c = numeric(0), rh = numeric(0),
    logger.serial = character(0), stringsAsFactors = FALSE
  )

  ## ===========================================================================
  ## STEP 3: find all *templog.csv files (excluding the meta files themselves)
  ## ===========================================================================
  all.candidates <- list.files(dir.load, pattern = pattern.regex(load.pattern[1]),
                                recursive = dir.sub, full.names = TRUE)
  templog.files <- all.candidates[!grepl(pattern.regex(load.pattern[2]), all.candidates)]

  ## ===========================================================================
  ## STEP 4: process each file
  ## ===========================================================================
  for (f in templog.files) {

    fname <- basename(f)

    serial.num <- regmatches(fname, regexpr("^[0-9]{8}", fname))
    if (length(serial.num) == 0 || serial.num == "") serial.num <- NA_character_

    raw.lines <- read.raw.lines(f)
    raw.lines <- raw.lines[nzchar(raw.lines)]
    df <- read.csv(text = raw.lines, header = FALSE, colClasses = "character",
                   fill = TRUE, quote = "\"", strip.white = TRUE,
                   stringsAsFactors = FALSE)
    names(df) <- paste0("V", seq_len(ncol(df)))

    ## strip special characters right after loading (2026-09-30). The
    ## unstripped copy is kept ONLY so the degree-sign unit detection below
    ## still sees the original header row; row.id tracks which original
    ## row each remaining row of df is.
    df.orig <- df
    s.strip <- special.strip(df, strip.special)
    df <- s.strip$df
    ## keep every column character, as before (special.strip re-types a
    ## column it changed, the way read.csv would)
    for (col in s.strip$headers) df[[col]] <- as.character(df[[col]])
    row.id <- seq_len(nrow(df))
    v.out.names <- c(V1 = "observations.count", V2 = "date.time", V3 = "temp.dry.c", V4 = "temp.wet.c")
    file.special <- ifelse(s.strip$headers %in% names(v.out.names),
                           v.out.names[s.strip$headers], s.strip$headers)

    col.notes <- NA_character_
    if (ncol(df) > 4) {
      n.trim <- ncol(df) - 4
      df <- df[, 1:4]
      col.notes <- paste0("trimmed ", n.trim, " excess columns")
    }
    names(df) <- c("V1", "V2", "V3", "V4")

    rows.in <- nrow(df)

    if (nrow(df) > 0 && grepl("^Plot Title", df$V1[1], ignore.case = TRUE)) {
      df <- df[-1, , drop = FALSE]
      row.id <- row.id[-1]
    }
    keep.nonblank <- !(is.blank(df$V3) & is.blank(df$V4))
    df <- df[keep.nonblank, , drop = FALSE]
    row.id <- row.id[keep.nonblank]
    row.names(df) <- NULL

    rows.out <- nrow(df)

    header.notes <- NA_character_
    has.header <- (nrow(df) > 0 && grepl("date", df$V1[1], ignore.case = TRUE)) ||
                  (nrow(df) > 0 && any(grepl("date", as.character(df[1, ]), ignore.case = TRUE)))

    if (!has.header) {
      header.notes <- "no header present"
      temp.type <- "C.default"
    } else {
      ## unit detection uses the ORIGINAL (unstripped) header row, so
      ## strip.special can never change temp.type - see @details
      header.row <- as.character(df.orig[row.id[1], seq_len(min(4, ncol(df.orig)))])
      if (any(grepl(paste0(deg, "f|\\bF\\b|fahrenheit"), header.row, ignore.case = TRUE))) {
        temp.type <- "F"
      } else if (any(grepl(paste0(deg, "c|\\bC\\b|celsius"), header.row, ignore.case = TRUE))) {
        temp.type <- "C"
      } else {
        temp.type <- "C.default"
      }
      df <- df[-1, , drop = FALSE]
      row.names(df) <- NULL
    }

    date.time <- parse.dt(df$V2)

    date.start <- if (all(is.na(date.time))) NA else format(min(date.time, na.rm = TRUE))
    date.end   <- if (all(is.na(date.time))) NA else format(max(date.time, na.rm = TRUE))

    meta.result <- lookup.meta(serial.num, date.start, date.end)
    meta.row    <- meta.result$meta.row
    meta.notes  <- meta.result$meta.notes

    ## ---- trim records that fall outside the matched unit's deployment/
    ## recovery window (date AND time) -----------------------------------
    ## Only applied when the matched meta row carries full date+time columns
    ## for both deployment and recovery; meta files without
    ## $time.deployment/$time.recovery (or with no confident match at all)
    ## skip this step entirely - nothing is trimmed, same as before this
    ## feature existed.
    rows.trimmed <- 0L
    if (!is.null(meta.row) &&
        all(c("date.deployment", "time.deployment", "date.recovery", "time.recovery") %in% names(meta.row))) {
      deploy.dt  <- parse.meta.datetime(meta.row$date.deployment[1], meta.row$time.deployment[1])
      recover.dt <- parse.meta.datetime(meta.row$date.recovery[1], meta.row$time.recovery[1])
      if (!is.na(deploy.dt) && !is.na(recover.dt)) {
        ## keep records with an unparseable date.time too (can't judge them
        ## against the window, so don't silently drop them here)
        keep <- is.na(date.time) | (date.time >= deploy.dt & date.time <= recover.dt)
        rows.trimmed <- sum(!keep)
        if (rows.trimmed > 0) {
          df <- df[keep, , drop = FALSE]
          date.time <- date.time[keep]
          row.names(df) <- NULL
        }
      }
    }
    window.notes <- if (rows.trimmed > 0) {
      paste0("trimmed ", rows.trimmed, " record", if (rows.trimmed == 1) "" else "s",
             " outside deployment/recovery window")
    } else NA_character_

    notes.parts <- c(col.notes, header.notes, meta.notes, window.notes)
    notes.parts <- notes.parts[!is.na(notes.parts)]
    notes <- if (length(notes.parts) == 0) NA_character_ else paste(notes.parts, collapse = "; ")

    templog.notes <- rbind.fill(templog.notes, data.frame(
      logger.serial = serial.num, date.start = date.start, date.end = date.end,
      temp.type = temp.type, rows.in = rows.in, rows.out = rows.out,
      rows.trimmed = rows.trimmed, notes = notes,
      special.log.cols(strip.special, c(
        file.special, if (!is.null(meta.row) && length(meta.special) > 0) paste0("meta:", meta.special)),
        if (!is.null(meta.row)) special.counts.add(s.strip$counts, meta.special.counts) else s.strip$counts),
      stringsAsFactors = FALSE
    ))

    temp.col3 <- suppressWarnings(as.numeric(df$V3))
    temp.col4 <- suppressWarnings(as.numeric(df$V4))

    if (identical(temp.type, "F")) {
      temp.col3 <- f.to.c(temp.col3)
      temp.col4 <- f.to.c(temp.col4)
    }

    temp.dry.c <- temp.col3

    ## RH gated strictly on the matched meta row's $logger.type: H keeps the
    ## wet-bulb reading and gets RH calculated; T (or no confident meta
    ## match at all) gets NA for both
    logger.type <- if (!is.null(meta.row) && "logger.type" %in% names(meta.row)) meta.row$logger.type[1] else NA_character_
    if (identical(logger.type, "H")) {
      temp.wet.c <- temp.col4
      rh <- calc.rh(temp.dry.c, temp.wet.c)
    } else {
      temp.wet.c <- NA_real_
      rh <- NA_real_
    }

    ## append the REMAINING meta columns (everything but the join key) to
    ## every record from this file, defaulting to NA with no confident match.
    ## Recycled to nrow(df) explicitly (not left as length-1 scalars) because
    ## data.frame() cannot recycle a length-1 vector down to 0 rows - without
    ## this, a file whose deployment/recovery window trims away every single
    ## record would crash the function instead of contributing 0 rows.
    meta.extra <- as.list(rep(NA_character_, length(meta.append.cols)))
    names(meta.extra) <- meta.append.cols
    if (!is.null(meta.row)) {
      for (col in meta.append.cols) meta.extra[[col]] <- meta.row[[col]][1]
    }
    meta.extra <- lapply(meta.extra, function(v) rep(v, length.out = nrow(df)))

    ## BUGFIX (2026-08-19, found by running on real hobotemp/*templog.csv test
    ## data): serial.num and (on the "T"/no-confident-match branch)
    ## temp.wet.c/rh are constant length-1 scalars for the whole file, which
    ## data.frame() can recycle up to any nrow(df) >= 1 but NOT down to 0. A
    ## file whose deployment/recovery window trimmed away every single record
    ## (nrow(df) == 0) used to crash the whole function here. Skipping the
    ## append entirely when there's nothing left to add is a no-op for
    ## templog.merged either way (rbind.fill treats a 0-row frame as a
    ## no-op), and rows.in/rows.out/rows.trimmed/notes for the file are
    ## already recorded in templog.notes above regardless.
    if (nrow(df) > 0) {
      file.df <- do.call(data.frame, c(
        list(observations.count = seq_len(nrow(df)), date.time = date.time, temp.dry.c = temp.dry.c,
             temp.wet.c = temp.wet.c, rh = rh, logger.serial = serial.num),
        meta.extra,
        stringsAsFactors = FALSE
      ))
      templog.merged <- rbind.fill(templog.merged, file.df)
    }
  }

  ## ---- daterange token (round twenty-two, 2026-09-24, per Josh) - see
  ## @details, "File naming (round twenty-two)". Computed from
  ## templog.merged$date.time BEFORE the snake_case rename just below, so
  ## this always reads the dot-separated column regardless of that
  ## parameter.
  valid.dt <- templog.merged$date.time[!is.na(templog.merged$date.time)]
  daterange.token <- if (length(valid.dt) > 0) {
    sprintf("%sto%s", format(min(valid.dt), "%Y%m%d"), format(max(valid.dt), "%Y%m%d"))
  } else {
    "nodata"
  }

  ## snake_case (per Josh, 2026-09-22, project-wide audit/extension of the
  ## snake_case output option - see @details) is applied here, as the very
  ## last step for BOTH returned data frames, before write.output (below)
  ## copies/rounds/writes templog.merged - so the written CSV(s) and the
  ## invisibly-returned list agree on column naming.
  if (snake_case) {
    names(templog.merged) <- snake.headers(names(templog.merged))
    names(templog.notes)  <- snake.headers(names(templog.notes))
    ## keep the log column's exact name (2026-09-30)
    special.cols <- c("strip.special", "accented.letters.header", "removed.symbols.header",
                      "accented.letters.data", "removed.symbols.data")
    hit <- match(names(templog.notes), snake.headers(special.cols))
    names(templog.notes)[!is.na(hit)] <- special.cols[hit[!is.na(hit)]]
  }

  if (write.output) {
    # Rounding is applied only here, at the final save step - never to
    # intermediate values, and never to the object returned to R (below),
    # which stays at full precision for any further calculations.
    templog.merged.out <- templog.merged
    templog.merged.out$rh <- round(templog.merged.out$rh, 3)

    ## ---- resolve the output file names (round twenty-two, 2026-09-24) -
    ## <project.name>_<filetype.name>_<daterange>_<timestamp>.csv - see
    ## @param project.name and @details, "File naming (round twenty-two)".
    base.name <- if (identical(project.name, "")) "templog" else project.name
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

    write.csv(templog.notes, file.path(dir.save, sprintf(
      "%s_templog.notes_%s_%s.csv", base.name, daterange.token, timestamp)), row.names = FALSE)
    write.csv(templog.merged.out, file.path(dir.save, sprintf(
      "%s_templog.merged_%s_%s.csv", base.name, daterange.token, timestamp)), row.names = FALSE)
  }

  invisible(list(templog.merged = templog.merged, templog.notes = templog.notes))
}

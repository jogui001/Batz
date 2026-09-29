#' Merge and format raw temperature-logger exports
#'
#' Merges raw \verb{*templog.csv} datalogger exports (heterogeneous formats -
#' varying header presence, column counts, embedded commas, date formats,
#' encodings) into one standardized sheet, joins in station metadata from
#' \code{templog.meta.csv}, calculates relative humidity for loggers flagged
#' as humidity-capable, and trims records recorded outside each unit's
#' deployment/recovery window.
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real, documented output-schema change.} \code{templog.meta.csv}'s own
#' headers are a literal, uninvented copy of that file's real column text
#' (not a \code{batz}-invented shorthand), so they are now run through the
#' shared package helper \code{standardize.headers()} (trim whitespace,
#' collapse every run of non-alphanumeric characters to a single
#' underscore, lowercase) right after loading - this function had no
#' header normalization of any kind before this change, so it also newly
#' tolerates whitespace/case/punctuation variation in the meta file that
#' used to require an exact literal match. Every meta-file column the code
#' refers to by name is renamed accordingly: \code{serial#} -> \code{
#' serial} (the \code{#} is stripped as a non-alphanumeric character),
#' \code{serial.num} -> \code{serial_num}, \code{serial.short} -> \code{
#' serial_short}, \code{logger.type} -> \code{logger_type}, \code{
#' station.code} -> \code{station_code}, \code{date.deployment} -> \code{
#' date_deployment}, \code{time.deployment} -> \code{time_deployment},
#' \code{date.recovery} -> \code{date_recovery}, \code{time.recovery} ->
#' \code{time_recovery}, \code{room.number} -> \code{room_number}, \code{
#' room.name} -> \code{room_name}. Since every OTHER meta column (besides
#' the join key) is appended verbatim onto \code{templog.merged} (see
#' below), this cascades into a real, visible change in that returned data
#' frame's own column names. \strong{This does NOT affect \code{
#' templog.merged}'s/\code{templog.notes}'s own \code{$serial.num} column}
#' - that one is this function's own INVENTED identifier, parsed from each
#' raw file's NAME (its leading 8-digit serial), not a header loaded from
#' any file, so it keeps its existing dot-separated name per this
#' project's ordinary output convention. \strong{Anyone with a saved
#' templog.meta.csv using the old dotted header spellings should re-save
#' it with the new underscore spellings (or simply let it load as-is,
#' since \code{standardize.headers()} converts either spelling to the same
#' result), and update any downstream script that reads
#' \code{templog.merged} by the old dotted meta-column names.}
#'
#' RH is gated strictly on the matched \code{templog.meta.csv} \code{$logger_type}
#' - not on header text or any other heuristic:
#' \itemize{
#'   \item \code{logger_type == "H"}: \code{$temp.wet.c} keeps the real
#'     column-4 reading, and \code{$rh} is calculated from dry/wet bulb.
#'   \item \code{logger_type == "T"}: \code{$temp.wet.c} and \code{$rh} are
#'     both forced to \code{NA}, even if the raw file has a second
#'     temperature column.
#'   \item No confident meta match (missing, ambiguous, or no meta.csv at
#'     all): treated the same as \code{"T"} (both \code{NA}).
#' }
#'
#' The meta join uses ONLY \code{$logger.serial} (the file's real 8-digit
#' serial, matched against a standardized \code{serial#} header) plus the
#' file's own observed \code{[$date.start, $date.end]} to pick the right
#' meta row - \code{$logger.serial_short} and \code{$logger_type} are never
#' used to find or disambiguate the match (only as a fallback for older
#' meta files with no \code{serial#} column at all). Once a single
#' confident meta row is found, every OTHER column from that row
#' (\code{logger.serial_short}, \code{date.deployment}, \code{date.recovery},
#' \code{room.code}, \code{room.name}, \code{station.code},
#' \code{logger_type}) is appended to every record from that file in
#' \code{$templog.merged}, not just used internally.
#'
#' Once a meta row is matched, any record whose \code{$date.time} falls
#' before \code{$date.deployment}+\code{$time.deployment} or after
#' \code{$date.recovery}+\code{$time.recovery} is dropped from
#' \code{$templog.merged} (e.g. readings taken during setup before actual
#' deployment, or after physical recovery but before the logger stopped
#' recording). This only runs when the matched meta row has BOTH date and
#' time columns for deployment and recovery - meta files with date-only
#' columns (no \code{$time.deployment}/\code{$time.recovery}), or files with
#' no confident meta match at all, are left untrimmed. The count of trimmed
#' records per file is in \code{$templog.notes$rows.trimmed}, and (when > 0)
#' also called out in \code{$templog.notes$notes}.
#'
#' \code{$rh} is kept at full precision through every internal calculation
#' and in the returned object; rounding (3 dp) is applied only when
#' \code{write.output = TRUE}, to the CSV written to disk.
#'
#' This function is \code{batz.merge_temp.logger()}: it merges raw
#' \verb{*templog.csv} datalogger exports into one standardized sheet
#' (subject = "temp.logger").
#'
#' See \code{dev-scripts/batz.merge_temp.logger.dev.R} in the package
#' source repo for the tested procedural version and the full list of
#' assumptions made where the spec was ambiguous (dry/wet column mapping
#' when unlabeled, date.end = max(), blank-row cleanup scope, RH formula,
#' and the "no confident meta match = treat like T" default). Those
#' assumptions apply here unchanged and should be reviewed before relying on
#' this in production.
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request to audit and extend the
#' snake_case output option package-wide:} added a \code{snake_case}
#' parameter (default \code{FALSE}, matching
#' \code{\link{batz.generate_plotframe.bat}}'s own). When \code{TRUE}, both
#' \code{templog.merged} and \code{templog.notes} have their own output
#' column names run through \code{standardize.headers()} as the very last
#' step before they're written to CSV (when \code{write.output = TRUE}) and
#' returned - e.g. \code{$date.time} becomes \code{$date_time},
#' \code{$temp.dry.c} becomes \code{$temp_dry_c}, \code{$serial.num}
#' becomes \code{$serial_num}. This is unrelated to, and does not change,
#' the existing \code{templog.meta.csv} header standardization described
#' above (which already always runs, regardless of this parameter, on the
#' loaded meta file's own real headers before they're matched/joined) -
#' \code{snake_case} only affects the final output column names of this
#' function's own two returned/written data frames.
#'
#' \strong{File naming (round twenty-two), 2026-09-24, per Josh's
#' package-wide request ("Update all functions that save files or charts:
#' ... for files follow <project.name>_<filetype.name>_<daterange>_
#' <timestamp>"):} this is the ONE function in this round that needed a
#' brand-new parameter rather than just a naming tweak - flagged as the
#' single biggest change in this round, please review closely.
#' \strong{\code{project.name} did not exist on this function at all
#' before this round}; it is added here (default \code{""}, following the
#' same blank-default-falls-back-to-a-base-token convention already used by
#' \code{\link{batz.generate_suntimes.arulist}}'s own \code{project.name} -
#' \code{""} falls back to the literal base token \code{"templog"}, chosen
#' as a judgment call since Josh did not specify one; \strong{please
#' confirm \code{"templog"} is the right fallback base name}). This
#' function also had NO date-range concept anywhere in its output before
#' this round - a NEW \code{<daterange>} token is now computed from
#' \code{templog.merged$date.time} (the earliest/latest non-\code{NA}
#' value, each formatted \code{\%Y\%m\%d}, joined as
#' \code{<DATE1>to<DATE2>}), also flagged as a new, previously-nonexistent
#' computed field added purely to satisfy the naming convention. If every
#' \code{$date.time} value is \code{NA} (or there are zero rows), the
#' literal token \code{"nodata"} is used in place of a real date range
#' rather than crashing on an empty \code{min()}/\code{max()} - this
#' fallback was not specified either and is likewise a judgment call.
#' \strong{Both output file names change from the previous hardcoded,
#' non-timestamped \code{"templog.notes.csv"}/\code{"templog.merged.csv"}
#' to} \verb{<project.name>_templog.notes_<daterange>_<timestamp>.csv} and
#' \verb{<project.name>_templog.merged_<daterange>_<timestamp>.csv}
#' respectively - using \code{"templog.notes"}/\code{"templog.merged"} as
#' each file's \code{<filetype.name>} token (the same names as the two
#' returned list elements, for consistency). \strong{This is a real
#' behavior change beyond naming}: previously, repeat calls with the same
#' \code{dir.save} silently overwrote the same two file names every time;
#' now, like every other \code{batz} function that saves output, every
#' call produces two freshly timestamped files and nothing is ever
#' silently overwritten. \code{<timestamp>} uses the same 14-digit,
#' no-separator \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")} shape as
#' every other \code{batz} function in this round - see each function's
#' own "Timestamp format (round twenty-two)" \code{@details} entry.
#'
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's
#' reference-workbook "Change.to" column.} The following raw/legacy header
#' spellings were renamed to their new canonical form throughout this
#' function's code and documentation: \code{date_deployment} ->
#' \code{date.deployment}, \code{date_recovery} -> \code{date.recovery},
#' \code{room_name} -> \code{room.name}, \code{room_number} ->
#' \code{room.code}, \code{serial_short} -> \code{logger.serial_short},
#' \code{station_code} -> \code{station.code}, \code{time_deployment} ->
#' \code{time.deployment}, \code{time_recovery} -> \code{time.recovery}.
#' \strong{Two further renames converge on one name:} the meta file's raw
#' logger-serial-number input column (standardized from \code{serial#} to
#' \code{serial}) is renamed \code{serial} -> \code{logger.serial}, and
#' this function's own invented output column (parsed from each raw
#' file's leading 8-digit serial) is renamed \code{serial.num} ->
#' \code{logger.serial} - the same concept, input alias and output name
#' now unified under one identifier. Because \code{standardize.headers()}
#' collapses any punctuation in a raw header to an underscore, an
#' incoming \code{templog.meta.csv} will still standardize to the legacy
#' spellings \code{serial}/\code{serial_num} (from \code{serial#}/
#' \code{serial.num}) and \code{serial_short} (from \code{serial.short});
#' the header-matching/detection logic below still checks for those
#' legacy standardized spellings as input aliases (real incoming files
#' will still be named that way), and once a column is matched it is
#' renamed internally to its new canonical identifier
#' (\code{logger.serial}/\code{logger.serial_short}) for every downstream
#' use, including \code{templog.merged}/\code{templog.notes}'s own output
#' columns.
#'
#' @param dir.load Directory to search for files matching \code{load.pattern}.
#'   Default: current working directory.
#' @param load.pattern Character vector of length 2, default
#'   \code{c("*templog.csv", "*templog.meta.csv")}: the file-name suffix
#'   patterns (plain wildcard/glob style - \code{"*"} as a leading wildcard,
#'   everything else literal) that identify, respectively, (1) the raw
#'   datalogger export files and (2) the station-metadata file(s).
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   every subdirectory of \code{dir.load}. \strong{Note:} the real test data
#'   keeps \code{templog.meta.csv} inside a \code{meta files/} subfolder, so
#'   with the new \code{FALSE} default that meta file will NOT be found
#'   unless you explicitly pass \code{dir.sub = TRUE}.
#' @param dir.save Directory to write \code{templog.merged.csv} and
#'   \code{templog.notes.csv} into when \code{write.output = TRUE}. Default:
#'   the current working directory (\code{getwd()}) - set this separately if
#'   the output should be written somewhere other than where the input
#'   \verb{*templog.csv} files were loaded from. (Standardized 2026-08-29,
#'   per Josh: previously defaulted to \code{dir.load}, which silently
#'   mirrored whatever \code{dir.load} was rather than defaulting to a
#'   sensible location on its own - the same pattern already fixed as a bug
#'   in \code{batz.suntimes_generate()}; \code{dir.save} now always
#'   defaults to \code{getwd()} for consistency across the package.)
#' @param write.output If \code{TRUE} (default), also write
#'   \code{templog.merged.csv} and \code{templog.notes.csv} into
#'   \code{dir.save} (with \code{$rh} rounded to 3 decimal places in the
#'   written CSV only). \strong{As of round twenty-two (2026-09-24) these
#'   two file names are no longer literally \code{"templog.merged.csv"}/
#'   \code{"templog.notes.csv"} - see \code{@param project.name} below and
#'   @details, "File naming (round twenty-two)".}
#' @param project.name Character, default \code{""}. \strong{New in round
#'   twenty-two (2026-09-24), per Josh's package-wide file-naming request -
#'   see @details, "File naming (round twenty-two)"; this parameter did not
#'   exist before this round.} Base project-name token used to build both
#'   output CSV names when \code{write.output = TRUE}:
#'   \verb{<project.name>_templog.notes_<daterange>_<timestamp>.csv} and
#'   \verb{<project.name>_templog.merged_<daterange>_<timestamp>.csv}, where
#'   \code{<daterange>} is computed from the earliest/latest
#'   \code{$date.time} in \code{templog.merged} (\code{<DATE1>to<DATE2>},
#'   \code{\%Y\%m\%d} each, or the literal \code{"nodata"} if none is
#'   available) and \code{<timestamp>} is \code{format(Sys.time(),
#'   "\%Y\%m\%d\%H\%M\%S")}. \code{""} (default) falls back to the literal
#'   base token \code{"templog"} - a judgment call, please confirm this is
#'   the right default.
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-22, per
#'   Josh's request to audit and extend the snake_case output option
#'   package-wide (see \code{\link{batz.generate_plotframe.bat}}, the first
#'   function this was added to). Controls only \code{templog.merged}'s/
#'   \code{templog.notes}'s OWN output column names (and, when
#'   \code{write.output = TRUE}, the columns of the CSVs written to disk),
#'   applied as the very last step before either is written/returned.
#'   \code{FALSE} (default) keeps this function's normal dot-separated
#'   output column names (\code{$date.time}, \code{$temp.dry.c}, ...)
#'   exactly as always. \code{TRUE} runs every output column name through
#'   \code{standardize.headers()} instead (e.g. \code{$date.time} ->
#'   \code{$date_time}, \code{$temp.dry.c} -> \code{$temp_dry_c}) - for a
#'   caller who specifically wants a snake_case CSV/data frame out of this
#'   function, without having to convert it themselves afterward.
#'
#' @return Invisibly, a list with two data frames:
#'   \describe{
#'     \item{templog.merged}{\code{$obs}, \code{$date.time},
#'       \code{$temp.dry.c}, \code{$temp.wet.c}, \code{$rh},
#'       \code{$logger.serial}, plus every other column from the matched
#'       \code{templog.meta.csv} row (standardized names - see "Header
#'       standardization" above).}
#'     \item{templog.notes}{\code{$logger.serial}, \code{$date.start},
#'       \code{$date.end}, \code{$temp.type}, \code{$rows.in},
#'       \code{$rows.out}, \code{$rows.trimmed}, \code{$notes}.}
#'   }
#'   (or their snake_case equivalents if \code{snake_case = TRUE} - see
#'   that parameter above).
#'
#' @examples
#' \dontrun{
#' result <- batz.merge_temp.logger(dir.load = "path/to/data", dir.sub = TRUE)
#' result$templog.merged
#' result$templog.notes
#'
#' # snake_case output headers instead of this function's usual dot-style
#' result <- batz.merge_temp.logger(dir.load = "path/to/data", snake_case = TRUE)
#'
#' # custom project.name token in the saved file names (round twenty-two)
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
                                          snake_case = FALSE) {

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

  read.raw.lines <- function(file) {
    raw3 <- readBin(file, what = "raw", n = 3)
    has.bom <- length(raw3) == 3 &&
               identical(as.integer(raw3), c(0xEFL, 0xBBL, 0xBFL))
    con <- file(file, open = "rb")
    if (has.bom) readBin(con, what = "raw", n = 3)
    raw.all <- readBin(con, what = "raw", n = file.info(file)$size)
    close(con)
    txt <- rawToChar(raw.all, multiple = FALSE)
    Encoding(txt) <- "UTF-8"
    strsplit(txt, "\r\n|\n|\r")[[1]]
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
  deg <- "°"

  ## ===========================================================================
  ## STEP 1: load templog.meta.csv file(s), merge + de-dup
  ## ===========================================================================
  meta.files <- list.files(dir.load, pattern = pattern.regex(load.pattern[2]),
                            recursive = dir.sub, full.names = TRUE)

  if (length(meta.files) > 0) {
    meta.list <- lapply(meta.files, read.csv, stringsAsFactors = FALSE,
                         colClasses = "character", check.names = FALSE)
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
                   time_recovery   = "time.recovery")
  match.idx <- match(names(templog.meta), names(rename.map))
  names(templog.meta)[!is.na(match.idx)] <- rename.map[match.idx[!is.na(match.idx)]]

  ## if $logger_type doesn't exist, derive it from the last letter of
  ## $station.code (T = temp only, H = temp + humidity)
  if (nrow(templog.meta) > 0 && !"logger_type" %in% names(templog.meta)) {
    templog.meta$logger_type <- toupper(substr(templog.meta$station.code,
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

  if (!is.na(serial.col)) {
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
    notes = character(0),
    stringsAsFactors = FALSE
  )

  templog.merged <- data.frame(
    obs = integer(0), date.time = as.POSIXct(character(0)),
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
    }
    df <- df[!(is.blank(df$V3) & is.blank(df$V4)), , drop = FALSE]
    row.names(df) <- NULL

    rows.out <- nrow(df)

    header.notes <- NA_character_
    has.header <- (nrow(df) > 0 && grepl("date", df$V1[1], ignore.case = TRUE)) ||
                  (nrow(df) > 0 && any(grepl("date", as.character(df[1, ]), ignore.case = TRUE)))

    if (!has.header) {
      header.notes <- "no header present"
      temp.type <- "C.default"
    } else {
      header.row <- as.character(df[1, ])
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
      rows.trimmed = rows.trimmed, notes = notes, stringsAsFactors = FALSE
    ))

    temp.col3 <- suppressWarnings(as.numeric(df$V3))
    temp.col4 <- suppressWarnings(as.numeric(df$V4))

    if (identical(temp.type, "F")) {
      temp.col3 <- f.to.c(temp.col3)
      temp.col4 <- f.to.c(temp.col4)
    }

    temp.dry.c <- temp.col3

    ## RH gated strictly on the matched meta row's $logger_type: H keeps the
    ## wet-bulb reading and gets RH calculated; T (or no confident meta
    ## match at all) gets NA for both
    logger.type <- if (!is.null(meta.row) && "logger_type" %in% names(meta.row)) meta.row$logger_type[1] else NA_character_
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
        list(obs = seq_len(nrow(df)), date.time = date.time, temp.dry.c = temp.dry.c,
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
    names(templog.merged) <- standardize.headers(names(templog.merged))
    names(templog.notes)  <- standardize.headers(names(templog.notes))
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

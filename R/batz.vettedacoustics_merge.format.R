#' Merge vetted bat-acoustic-call files (k-Pro/Kaleidoscope or SonoBat) into one master table
#'
#' Searches a directory (and its subdirectories, if \code{dir.sub = TRUE})
#' for vetted bat-acoustic-call files exported from k-Pro/Kaleidoscope or
#' SonoBat vetting software, merges every file that has the expected columns
#' into one master data frame, standardizes/reorders its headers, adds a
#' combined call date-time column, recodes species identifiers to a single
#' format, optionally fills blank manual IDs from the matching auto ID
#' column, and optionally drops noise/unidentified rows.
#'
#' \strong{Header standardization - a real bug in the spec's own snippet was
#' caught and fixed here.} Josh's spec gives the standardization line
#' literally as \code{tolower(gsub("[[:punct:]]", "", names(temp)))}, but
#' \code{[[:punct:]]} matches punctuation only, not whitespace. Applied
#' literally, the real header \code{"Species Manual ID"} would standardize
#' to \code{"species manual id"} (spaces survive) - which would never match
#' the spec's own expected header \code{"speciesmanualid"}, and every real
#' file would be skipped as "mismatched headers" forever. Fixed here to
#' strip ALL non-alphanumeric characters:
#' \code{tolower(gsub("[^[:alnum:]]", "", names(tmp)))} - verified against a
#' real file (\code{FinalVetted.csv}, 6,487 rows) that this exact
#' substitution turns \code{"Species Manual ID"} into
#' \code{"speciesmanualid"} and \code{"WA|Kaleidoscope|Auto ID"} into
#' \code{"wakaleidoscopeautoid"}, both matching the spec's expected list
#' exactly.
#'
#' Expected (standardized) headers: \code{filename}, \code{monitoringnight},
#' \code{speciesmanualid}, \code{wakaleidoscopeautoid}, \code{sppaccp},
#' \code{lat}, \code{serial}. A file missing any of these (after
#' standardization) is skipped with \code{$reason = "mismatched headers"}
#' and \code{$headers.missing} listing which ones (comma-separated). A file
#' with all expected headers but zero data rows is skipped with
#' \code{$reason = "no records"} and \code{$headers.missing = "none"}. A
#' file that fails to read at all is skipped with \code{$reason = "could
#' not read file"} (not literally specified - added as a safety fallback,
#' consistent with other \code{batz} functions). Every other file is
#' trimmed to just the expected headers and appended to
#' \code{vetted.merged}.
#'
#' \code{duplicates.remove} is used in the spec's own Steps section but is
#' missing from its Optional Inputs list - added as a real parameter,
#' default \code{TRUE} (matching every other \code{batz} dedup-flag
#' default).
#'
#' Once every file is merged (and de-duplicated, if requested), two columns
#' are expanded from the raw data (confirmed against the real
#' \code{FinalVetted.csv}, not guessed): \code{$lat} - which holds BOTH
#' coordinates as \code{"<lat> <lon>"} (a single space between two decimal
#' degrees, longitude negative in the western hemisphere, e.g.
#' \code{"43.59303 -71.73640"}) - is split into numeric \code{$lat} and
#' \code{$lon}. \code{$filename} - which follows
#' \code{"<ARU name>_<YYYYMMDD>_<HHMMSS>_<junk>"} (e.g.
#' \code{"CLERK_20260627_001635_000.wav"}) - is parsed into
#' \code{$aru.name}, \code{$date} (an 8-character \code{"YYYYMMDD"} string,
#' not converted to a \code{Date}), and \code{$time} (a 6-character
#' \code{"HHMMSS"} string; the spec calls this "HHMMDD," read as a typo,
#' since there's no sensible day-of-month after an 8-digit date, and the
#' spec itself calls it "military time"). The trailing junk
#' (\code{"_000.wav"}) is discarded, not stored anywhere.
#'
#' \strong{Update (2026-08-26) - header rename/reorder + species-ID
#' pipeline.} Once the steps above finish, the (still 11-column) table is
#' put through a further pipeline:
#' \enumerate{
#'   \item \strong{Positional rename.} Josh's own rename list, given
#'     verbatim, was missing a comma between \code{"autoid.sb"} and
#'     \code{"lat"} - read as a typo, not a real 10-element list. The 11
#'     existing columns (in the order produced by the steps above:
#'     \code{filename, monitoringnight, speciesmanualid,
#'     wakaleidoscopeautoid, sppaccp, lat, serial, lon, aru.name, date,
#'     time}) are renamed POSITIONALLY to \code{filename, date.mon, manid,
#'     autoid.kp, autoid.sb, lat, serial, lon, aru.name, date, time}.
#'     \code{"wakaleidoscopeautoid"} becomes \code{"autoid.kp"} and
#'     \code{"sppaccp"} becomes \code{"autoid.sb"} - this makes sense once
#'     you notice the function merges files from either k-Pro/Kaleidoscope
#'     ("kp") or SonoBat ("sb") vetting software: Kaleidoscope's own auto
#'     ID column becomes \code{$autoid.kp}, SonoBat's "accepted species"
#'     column becomes \code{$autoid.sb}.
#'   \item \strong{Reorder} to \code{filename, date.mon, aru.name, serial,
#'     lat, lon, manid, autoid.kp, autoid.sb, date, time}.
#'   \item \code{\link{batz.datawrangler_call.datetime}} is run on
#'     \code{$date}/\code{$time} to add \code{$call.datetime}.
#'   \item \code{\link{batz.batusa_recode.names}} (default
#'     \code{output.format = "common"}, default \code{grammar.dash =
#'     TRUE} - not specified which format to recode to, flagged) is run on
#'     \code{$manid}, \code{$autoid.kp}, and \code{$autoid.sb} in place -
#'     any value not recognized as a species name/code (e.g. \code{"NOISE"},
#'     \code{"NoID"}, or a blank) passes through unchanged.
#'   \item If \code{manid.kp = TRUE} (default), \code{$manid.kp} is created
#'     as a copy of (the now-recoded) \code{$manid}; every blank/NA element
#'     of \code{$manid.kp} is overwritten with that row's \code{$autoid.kp}
#'     value. ("Blank" means \code{NA} or a blank/whitespace-only string -
#'     not specified either way, same convention already used in
#'     \code{batz.datawrangler_call.datetime}.) The spec's fill-in prose
#'     names the source column \code{"$auto.kp"}, which doesn't exist
#'     anywhere - read as shorthand for \code{$autoid.kp}, matching the
#'     rest of the spec's own naming.
#'   \item If \code{manid.sb = TRUE} (default), the same thing happens for
#'     \code{$manid.sb}, filled from \code{$autoid.sb} ("$auto.sb" in the
#'     spec's prose, read the same way).
#'   \item If \code{trim.noise = TRUE} (default), every row whose
#'     \code{$manid} equals \code{"noise"} (case-insensitive, whitespace-
#'     trimmed) is removed.
#'   \item If \code{trim.noid = TRUE} (default \code{FALSE}), every row
#'     whose \code{$manid} equals \code{"NoID"} (case-insensitive,
#'     whitespace-trimmed) is removed.
#' }
#' \code{$manid.kp}/\code{$manid.sb} (when created) and \code{$call.datetime}
#' are appended at the very end of the column order (not specified where
#' they should go). \code{trim.noise}/\code{trim.noid} check \code{$manid}
#' itself (not \code{$manid.kp}/\code{$manid.sb}), AFTER the
#' \code{batz.batusa_recode.names()} step - safe because that step passes
#' unrecognized values like \code{"noise"}/\code{"NoID"} through unchanged,
#' so they're still there to match against.
#'
#' @param dir.load Character, default \code{getwd()}. Directory to scan.
#' @param load.pattern Character vector, default \code{c("*vetted.csv")}. A
#'   wildcard/glob pattern (or vector of patterns) identifying which files to
#'   load, converted internally to a regex via \code{utils::glob2rx()}.
#'   Matching is CASE-INSENSITIVE (confirmed necessary against real data -
#'   see Details).
#' @param dir.sub Logical, default \code{FALSE}. Also search subdirectories
#'   of \code{dir.load}.
#' @param duplicates.remove Logical, default \code{TRUE}. Remove exact
#'   duplicate rows (base \code{duplicated()}) from the final merged table,
#'   after all files are combined (checked before the rename/reorder/species
#'   pipeline runs).
#' @param log.file Logical, default \code{FALSE}. When \code{TRUE}, also
#'   creates \code{vetted.merged_log.file} (one row per SKIPPED file, with
#'   \code{$filepath}, \code{$reason}, \code{$headers.missing}).
#' @param manid.kp Logical, default \code{TRUE}. Create \code{$manid.kp}
#'   (a copy of \code{$manid} with blanks filled from \code{$autoid.kp} -
#'   Kaleidoscope's auto ID). See Details.
#' @param manid.sb Logical, default \code{TRUE}. Create \code{$manid.sb}
#'   (a copy of \code{$manid} with blanks filled from \code{$autoid.sb} -
#'   SonoBat's auto ID). See Details.
#' @param trim.noise Logical, default \code{TRUE}. Remove rows where
#'   \code{$manid} is \code{"noise"} (case-insensitive). See Details.
#' @param trim.noid Logical, default \code{FALSE}. Remove rows where
#'   \code{$manid} is \code{"NoID"} (case-insensitive). See Details.
#'
#' @return Invisibly, a named list: \code{vetted.merged} (always), and
#'   \code{vetted.merged_log.file} (only if \code{log.file = TRUE}). As a
#'   side effect, the same object(s) are also assigned directly into the
#'   calling environment (same auto-assign convention as
#'   \code{batz.arumeta_merge.format}/\code{batz.datawrangler_load.files}),
#'   so a bare call with no assignment populates \code{vetted.merged}
#'   (and \code{vetted.merged_log.file}) directly.
#'
#' @examples
#' \dontrun{
#' # bare call - creates `vetted.merged` (and `vetted.merged_log.file`, if
#' # log.file = TRUE) directly in the calling environment
#' batz.vettedacoustics_merge.format(dir.sub = TRUE, log.file = TRUE)
#' head(vetted.merged)
#' vetted.merged_log.file
#' }
#'
#' @export
batz.vettedacoustics_merge.format <- function(dir.load = getwd(),
                                               load.pattern = c("*vetted.csv"),
                                               dir.sub = FALSE,
                                               duplicates.remove = TRUE,
                                               log.file = FALSE,
                                               manid.kp = TRUE,
                                               manid.sb = TRUE,
                                               trim.noise = TRUE,
                                               trim.noid = FALSE) {

  expected.headers <- c("filename", "monitoringnight", "speciesmanualid",
                         "wakaleidoscopeautoid", "sppaccp", "lat", "serial")

  normalize.header <- function(x) tolower(gsub("[^[:alnum:]]", "", x))

  regex.pattern <- paste(utils::glob2rx(load.pattern), collapse = "|")
  files <- list.files(dir.load, pattern = regex.pattern, recursive = dir.sub,
                       full.names = TRUE, ignore.case = TRUE)

  vetted.merged <- data.frame()
  log.rows <- list()

  add.log <- function(filepath, reason, headers.missing) {
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filepath = filepath, reason = reason, headers.missing = headers.missing,
      stringsAsFactors = FALSE
    )
  }

  for (f in files) {
    tmp <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { add.log(f, "could not read file", "none"); next }
    names(tmp) <- normalize.header(names(tmp))
    missing.headers <- setdiff(expected.headers, names(tmp))
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", ")); next
    }
    if (nrow(tmp) == 0) { add.log(f, "no records", "none"); next }
    tmp <- tmp[, expected.headers, drop = FALSE]
    vetted.merged <- rbind(vetted.merged, tmp)
  }

  if (duplicates.remove && nrow(vetted.merged) > 0) {
    vetted.merged <- vetted.merged[!duplicated(vetted.merged), ]
  }

  if (nrow(vetted.merged) > 0) {
    ## $lat holds "<lat> <lon>" - split into two numeric columns
    latlon <- strsplit(trimws(vetted.merged$lat), "\\s+")
    vetted.merged$lat <- vapply(latlon, function(x) as.numeric(x[1]), numeric(1))
    vetted.merged$lon <- vapply(latlon, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA_real_, numeric(1))

    ## $filename = "<ARU>_<YYYYMMDD>_<HHMMSS>_<junk>" -> $aru.name/$date/$time
    m <- regmatches(vetted.merged$filename,
                     regexec("^([^_]+)_(\\d{8})_(\\d{6})_", vetted.merged$filename))
    vetted.merged$aru.name <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
    vetted.merged$date     <- vapply(m, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
    vetted.merged$time     <- vapply(m, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1))

    ## --- rename, reorder, call.datetime, recode.names, manid.kp/sb
    ## fill-in, trim.noise/trim.noid - see @details above -----------------

    ## positional rename
    names(vetted.merged) <- c("filename", "date.mon", "manid", "autoid.kp",
                               "autoid.sb", "lat", "serial", "lon", "aru.name",
                               "date", "time")

    ## reorder
    vetted.merged <- vetted.merged[, c("filename", "date.mon", "aru.name",
                                        "serial", "lat", "lon", "manid",
                                        "autoid.kp", "autoid.sb", "date", "time")]

    ## $call.datetime
    vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = vetted.merged$date, time = vetted.merged$time)

    ## recode manid/autoid.kp/autoid.sb (default output.format = "common")
    vetted.merged$manid     <- batz.batusa_recode.names(vetted.merged$manid)
    vetted.merged$autoid.kp <- batz.batusa_recode.names(vetted.merged$autoid.kp)
    vetted.merged$autoid.sb <- batz.batusa_recode.names(vetted.merged$autoid.sb)

    is.empty <- function(x) is.na(x) | !nzchar(trimws(x))

    if (manid.kp) {
      vetted.merged$manid.kp <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.kp)
      vetted.merged$manid.kp[blank] <- vetted.merged$autoid.kp[blank]
    }

    if (manid.sb) {
      vetted.merged$manid.sb <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.sb)
      vetted.merged$manid.sb[blank] <- vetted.merged$autoid.sb[blank]
    }

    if (trim.noise) {
      vetted.merged <- vetted.merged[!(tolower(trimws(vetted.merged$manid)) == "noise"), , drop = FALSE]
    }

    if (trim.noid) {
      vetted.merged <- vetted.merged[!(tolower(trimws(vetted.merged$manid)) == "noid"), , drop = FALSE]
    }
  }

  rownames(vetted.merged) <- NULL
  result <- list(vetted.merged = vetted.merged)

  if (log.file) {
    vetted.merged_log.file <- if (length(log.rows) > 0) do.call(rbind, log.rows) else
      data.frame(filepath = character(0), reason = character(0), headers.missing = character(0))
    rownames(vetted.merged_log.file) <- NULL
    result$vetted.merged_log.file <- vetted.merged_log.file
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())
  invisible(result)
}

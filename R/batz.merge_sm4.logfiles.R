#' Merge and standardize SM4 ARU activity-log summary files
#'
#' Searches a directory (and, optionally, its subdirectories) for SM4
#' Autonomous Recording Unit activity-log summary files
#' (\code{"*_A_Summary*.txt"}/\code{"*_B_Summary*.txt"}), validates each
#' file's headers, and merges them into one standardized master data frame:
#' the ARU name is extracted from the file name, \code{$date} is normalized
#' to \code{YYYY-MM-DD}, and \code{$lat}/\code{$ns} and
#' \code{$longitude}/\code{$ew} are converted to signed decimal degrees
#' (\code{$Y}/\code{$X}).
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard/glob patterns (converted
#'   internally to a regex via \code{utils::glob2rx()}), default
#'   \code{c("*_A_Summary*.txt", "*_B_Summary*.txt")}. The second \code{*}
#'   (added 2026-09-29, per Josh) lets any text sit between
#'   \code{"_Summary"} and \code{".txt"}, so renamed/suffixed copies such as
#'   \code{"WTG-GOM102_A_Summary - Copy.txt"} or
#'   \code{"AYERS_A_Summary_2026.txt"} are also picked up; the plain
#'   \code{"*_A_Summary.txt"} form still matches.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from the final merged data frame. (Not listed in the
#'   original spec's "Optional inputs", but used in its Steps section -
#'   added as a real parameter; see the dev script's header comment.)
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and auto-assign) \code{log.file_sm4}: one row per
#'   file examined (whether it was successfully merged in or not; per
#'   Josh's 2026-09-22 redesign of this parameter - see \code{@details}),
#'   with columns \code{$aru.name}, \code{$filename}, \code{$version},
#'   \code{$date.start},
#'   \code{$date.end}, \code{$date.unique}, \code{$date.range},
#'   \code{$records}, \code{$load.status} (\code{"Success"}/\code{"Failure"}),
#'   \code{$reason}, and \code{$filepath}.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplifies accented letters and
#'   symbols (\code{é} -> \code{e}, \code{×} -> \code{X}) and removes other non-ASCII special characters (e.g. \code{°}, \code{µ},
#'   \code{™}) from each file's data after loading. Files are
#'   always read with a UTF-8/Latin-1 fallback, so a stray \code{°} can't
#'   stop them loading, whatever this is set to. The log gets a last column
#'   \code{$strip.special}: \code{"FALSE"} (not selected), \code{"TRUE
#'   NONE"} (nothing found) or \code{"TRUE ; <header>; ..."} (headers where
#'   characters were removed).
#'
#' @return Invisibly, a named list: \code{sm4logs.merged} (always), plus
#'   \code{log.file_sm4} when \code{log.file = TRUE}. Every
#'   element is also auto-assigned into the calling environment (same
#'   pattern already used in \code{batz.merge_aru.meta},
#'   \code{batz.datawrangler_load.files}, and
#'   \code{batz.arumeta_generate.eventlog}), so a bare call with no
#'   assignment creates \code{sm4logs.merged} (and the log table, if
#'   requested) directly in your workspace.
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real, documented output-schema change.} The 11 expected SM4 summary
#' columns are a literal, uninvented copy of the ARU device's own real
#' export column text (not a \code{batz}-invented shorthand), so - like the
#' raw headers read off every file - they are now run through the shared
#' package helper \code{standardize.headers()} (trim whitespace, collapse
#' every run of non-alphanumeric characters to a single underscore,
#' lowercase). The expected-header list itself was rewritten to the
#' standardized spellings so both sides of the header-validation check line
#' up: \code{DATE} -> \code{date}, \code{TIME} -> \code{time}, \code{LAT} ->
#' \code{lat}, \code{NS} -> \code{ns}, \code{LON} -> \code{lon}, \code{EW} ->
#' \code{ew}, \code{POWER(V)} -> \code{power_v}, \code{TEMP(C)} ->
#' \code{temp_c}, \code{#FILES} -> \code{files}, \code{#SCRUBBED} ->
#' \code{scrubbed}, \code{MIC0 TYPE} -> \code{mic0_type}. Since these are the
#' exact columns kept (and renamed to nothing else, aside from the further
#' \code{lon} -> \code{longitude} output rename documented below) in
#' \code{sm4logs.merged}, this is a real, visible change in that returned
#' data frame's own column names - anyone with existing code reading
#' \code{sm4logs.merged$DATE}, \code{$NS}, \code{$EW}, etc. by the old
#' upper-case/punctuated names will need to switch to the new standardized
#' ones. \strong{This does NOT affect \code{$aru.name}, \code{$X}, or
#' \code{$Y}} - none of the three is a header loaded from any file:
#' \code{aru.name} is parsed from the file's own NAME, and \code{X}/\code{Y}
#' are this function's own derived/computed columns, so all three keep
#' their existing names per this project's ordinary output convention.
#'
#' \strong{Header validation:} a file must have all 11 expected columns
#' (now matched by their standardized spellings, case-insensitively and
#' whitespace/punctuation-insensitively by construction) to be merged in. A
#' file missing one or more of them, or with a header row but zero data
#' rows, is not merged into \code{sm4logs.merged} - see the
#' \code{log.file} redesign below for how this is now reported. Extra,
#' unexpected columns don't cause a skip - only a missing expected column
#' does.
#'
#' \strong{ARU name:} taken from the file name, everything before the first
#' \code{"_"} (e.g. \code{"AYERS_A_Summary.txt"} -> \code{"AYERS"}).
#'
#' \strong{Date conversion:} only the observed real-data format,
#' \code{"YYYY-Mon-DD"} with a 3-letter month abbreviation (e.g.
#' \code{"2026-Jun-26"}), is converted to \code{"YYYY-MM-DD"}; a value in
#' any other format is left unchanged. Uses a fixed, locale-independent
#' month-name lookup rather than \code{strptime}'s locale-dependent
#' \code{\%b}.
#'
#' \strong{Coordinate conversion:} \code{$lat}/\code{$longitude} in the real
#' data are already plain decimal degrees, so \code{$Y}/\code{$X} are
#' produced by applying the correct sign from the hemisphere letter only
#' (\code{"s"} -> negative \code{$Y}, \code{"w"} -> negative \code{$X}) -
#' not a degrees-minutes-seconds parse. The original
#' \code{$lat}/\code{$ns}/\code{$longitude}/\code{$ew} columns are kept
#' alongside the new \code{$Y}/\code{$X} columns, not replaced.
#'
#' \strong{Follow-up, 2026-09-22, per Josh - \code{log.file} completely
#' redesigned.} Previously, \code{log.file_sm4} only had a row
#' for a SKIPPED file (\code{$filepath}/\code{$reason} only). Josh asked for
#' a richer log covering every file examined, success or failure, with a
#' per-file date/record summary. \code{log.file_sm4} (when
#' \code{log.file = TRUE}) now has exactly one row per file matched by
#' \code{load.pattern}, with columns \code{$aru.name} (always set, parsed
#' from the file name the same way as \code{sm4logs.merged}'s own
#' \code{$aru.name}), \code{$filename} (the file's base name, always set),
#' \code{$filepath} (always set), \code{$load.status} (\code{"Success"} if
#' the file had all 11 expected headers AND at least one data row,
#' \code{"Failure"} otherwise), \code{$reason} (Josh's literal text: for a
#' success, \code{"All headers present and observation in file"}; for a
#' failure, \code{"no data"} (headers fine, zero rows), \code{"These
#' headers are missing: <list>"} (>=1 row but headers missing), or
#' \code{"no data and These headers are missing: <list>"} (both) - plus
#' \code{"could not read file"} for a file \code{read.csv()} itself
#' errored on, an edge case outside Josh's given reason list, added for
#' parity with the (removed) old scheme's equivalent case), and, for a
#' \code{"Success"} row only (\code{NA} on every \code{"Failure"} row, per
#' Josh's explicit "all other headers = NA"): \code{$date.start}/
#' \code{$date.end} (earliest/latest \code{$date} value in that one file,
#' after the same \code{YYYY-MM-DD} conversion \code{sm4logs.merged} itself
#' uses - a plain string min/max is chronologically correct here since
#' zero-padded ISO dates sort the same as strings or as dates),
#' \code{$date.unique} (count of distinct \code{$date} values in that
#' file), \code{$date.range} (the number of calendar days from
#' \code{$date.start} to \code{$date.end} inclusive - compare against
#' \code{$date.unique} to spot gap days with no records), and
#' \code{$records} (row count of that one file, before the
#' package-level \code{duplicates.remove} step below, which only ever
#' operates on the final merged \code{sm4logs.merged}, never per-file).
#' \strong{Flagged, not in Josh's spec:} a \code{$date} value that
#' \code{convert.date()} doesn't recognize (see "Date conversion" above)
#' is still counted in \code{$date.unique} but excluded from the
#' \code{$date.start}/\code{$date.end}/\code{$date.range} calculation (via
#' an ISO-format validity check), so one malformed date value can't corrupt
#' the file's chronological summary; no real file has been seen to trigger
#' this. This is a full replacement of the previous \code{log.file}
#' behavior - a workflow reading the old \code{$filepath}/\code{$reason}-
#' only schema (skipped-files only) needs to be updated for the new
#' 10-column, one-row-per-file schema.
#'
#' @details
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's
#' reference-workbook "Change.to" column.} In \code{log.file_sm4}
#' (returned when \code{log.file = TRUE}): \code{file.name} ->
#' \code{filename}. In \code{sm4logs.merged}: the standardized input column
#' \code{lon} -> \code{longitude} - header-presence validation against the
#' SM4 device's own real \code{LON} export text is unchanged (that raw
#' export text still standardizes to \code{lon} internally for matching
#' purposes), only the merged output's column spelling changes.
#'
#' \strong{Follow-up, 2026-09-29, per Josh - wider default
#' \code{load.pattern}.} Default changed from
#' \code{c("*_A_Summary.txt", "*_B_Summary.txt")} to
#' \code{c("*_A_Summary*.txt", "*_B_Summary*.txt")}, so files with extra
#' text between \code{"_Summary"} and \code{".txt"} (e.g. a Windows
#' \code{" - Copy"} duplicate) are now found too. Matching is still
#' case-insensitive, and the ARU name is still everything before the first
#' \code{"_"}, so \code{"WTG-GOM102_A_Summary - Copy.txt"} -> \code{"WTG-GOM102"}.
#' \strong{Heads-up:} a " - Copy" file that is an exact duplicate of its
#' original adds identical rows; \code{duplicates.remove = TRUE} (the
#' default) drops those from \code{sm4logs.merged}, but both files still
#' get their own row in \code{log.file_sm4}.
#'
#' \strong{Follow-up, 2026-09-29, per Josh - SM4/SM5 detection and
#' \code{$version}.} Each file's headers are now checked to see which SM
#' unit wrote it (SM4, SM5 firmware 1.5 or earlier, or SM5 firmware 1.6 or
#' later) before loading. Only SM4 files are merged into
#' \code{sm4logs.merged}; an SM5 file matched by \code{load.pattern} is
#' skipped with \code{$load.status = "Failure"} and a \code{$reason}
#' pointing to \code{batz.merge_sm5.logfiles()} or
#' \code{batz.merge_sm.logfiles()}. \code{log.file_sm4} gains a
#' new \code{$version} column (after \code{$filename}): \code{"SM4"},
#' \code{"SM5.1.5"}, \code{"SM5.1.6"}, or \code{"unknown"} (the file
#' can't be read, or its headers match none of the three - reason
#' \code{"could not identify SM version from headers"}). The reading,
#' header checking, date/coordinate conversion and logging now live in a
#' shared internal engine (\code{sm.logfile.merge()}, in
#' \code{batz.util_sm.logfile.R}) used by all three SM merge functions -
#' \code{sm4logs.merged} itself is unchanged. See that file for how the
#' version is detected.
#'
#' \strong{Follow-up, 2026-09-29, per Josh - log renamed.} The log
#' object \code{sm4logs.merged_log.file} is now \code{log.file_sm4}, so it
#' can't be overwritten by (or overwrite) the log from any other
#' \code{batz} function. SM5 logs are \code{log.file_sm5}
#' (\code{batz.merge_sm5.logfiles()}) and \code{log.file_sm}
#' (\code{batz.merge_sm.logfiles()}). Earlier paragraphs in this
#' documentation use the new name. Code that reads
#' \code{sm4logs.merged_log.file} needs updating to \code{log.file_sm4}.
#' \code{sm4logs.merged} is unchanged.
#'
#' \strong{Follow-up, 2026-09-29, per Josh - function renamed.}
#' \code{batz.merge_sm4.logfile()} is now \code{batz.merge_sm4.logfiles()}
#' (file \code{R/batz.merge_sm4.logfiles.R}), matching
#' \code{batz.merge_sm.logfiles()}. Same inputs and outputs; code calling
#' the old name needs updating. Earlier paragraphs use the new name.
#'
#' \strong{Follow-up, 2026-09-30, per Josh - special characters.} New
#' input \code{strip.special} (see above); files now load even with a
#' stray Latin-1 \code{°} byte, and the log has a new last column
#' \code{$strip.special}.
#'
#' \strong{Follow-up, 2026-10-02, per Josh - accents simplified, log
#' counts.} With \code{strip.special = TRUE}, accented letters and common
#' symbols are now simplified instead of dropped (\code{café} ->
#' \code{cafe}, \code{Quercus × bebbiana} -> \code{Quercus X bebbiana},
#' curly quotes -> straight quotes); characters with no plain equivalent
#' (e.g. \code{°}, \code{µ}, \code{™}) are still removed. Column names
#' are cleaned the same way. The log gets four new columns right after
#' \code{$strip.special}: \code{$Accented.letters.header} and
#' \code{$removed.symbols.header} (number of unique column names with a
#' character simplified / removed), and \code{$Accented.letters.data} and
#' \code{$removed.symbols.data} (number of unique data values with a
#' character simplified / removed - \code{café} in 500 rows counts once;
#' \code{café} and \code{French café} count twice). They are \code{NA}
#' when \code{strip.special = FALSE}.
#'
#' @seealso \code{\link{batz.merge_sm5.logfiles}},
#'   \code{\link{batz.merge_sm.logfiles}}
#'
#' @examples
#' \dontrun{
#' batz.merge_sm4.logfiles()
#' # sm4logs.merged is now in your workspace
#'
#' batz.merge_sm4.logfiles(dir.sub = TRUE, log.file = TRUE)
#' # sm4logs.merged and log.file_sm4 both created
#' }
#'
#' @export
batz.merge_sm4.logfiles <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE,
                                   strip.special     = TRUE) {

  ## 2026-09-29: shared engine (batz.util_sm.logfile.R) - detects each
  ## file's SM version from its headers and only merges SM4 files here.
  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = "SM4", caller.name = "batz.merge_sm4.logfiles()",
                          strip.special = strip.special)

  sm4logs.merged <- out$data[["SM4"]]
  if (nrow(sm4logs.merged) == 0) {
    cat("\nNo files were successfully loaded - sm4logs.merged is empty.\n")
  }

  result <- list(sm4logs.merged = sm4logs.merged)
  if (log.file) result$log.file_sm4 <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

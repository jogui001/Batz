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
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real matching-behavior change, flagged not silently made.} This
#' function's own prior header normalization -
#' \code{tolower(gsub("[^[:alnum:]]", "", names(tmp)))}, which deleted every
#' separator entirely - has been replaced by the shared package helper
#' \code{standardize.headers()} (trim whitespace, collapse every run of
#' non-alphanumeric characters to a single underscore, lowercase - i.e.
#' snake_case). \code{expected.headers} is itself a literal, uninvented copy
#' of the vetting software's own real column text (not a \code{batz}-
#' invented shorthand), so per this project's header-standardization
#' preference both sides are standardized together: two of its six entries
#' change, because the real headers they represent contain spaces/
#' punctuation that used to be deleted and are now preserved as underscores
#' - \code{"speciesmanualid"} -> \code{"species_manual_id"} (real header:
#' \code{"Species Manual ID"}) and \code{"wakaleidoscopeautoid"} ->
#' \code{"wa_kaleidoscope_auto_id"} (real header: \code{"WA|Kaleidoscope|
#' Auto ID"}). The other four (\code{filename}, \code{monitoringnight},
#' \code{sppaccp}, \code{lat}) are unaffected, since their real headers
#' (\code{"Filename"}, \code{"MonitoringNight"}, \code{"SppAccp"},
#' \code{"Lat"}) have no separators to begin with. This is purely a
#' vocabulary change - the POSITIONAL rename immediately below (which maps
#' these six standardized names, by column position, onto \code{filename,
#' date.monitoringnight, manid, autoid.kp, autoid.sb, lat}) is unaffected, since it never
#' refers to the old squished text by name. \strong{Historical note (no
#' longer applicable after this change):} Josh's original spec gave this
#' standardization line literally as \code{tolower(gsub("[[:punct:]]", "",
#' names(temp)))}, which matched punctuation only, not whitespace - that bug
#' (and the fix that replaced it with the old delete-based
#' \code{normalize.header()}) is superseded by the switch to
#' \code{standardize.headers()} described above.
#'
#' Expected (standardized) headers: \code{filename}, \code{monitoringnight},
#' \code{species_manual_id}, \code{wa_kaleidoscope_auto_id}, \code{sppaccp},
#' \code{lat}. A file missing any of these (after
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
#'     \code{filename, monitoringnight, species_manual_id,
#'     wa_kaleidoscope_auto_id, sppaccp, lat, serial, lon, aru.name, date,
#'     time}) are renamed POSITIONALLY to \code{filename, date.monitoringnight, manid,
#'     autoid.kp, autoid.sb, lat, serial, lon, aru.name, date, time}.
#'     \code{"wa_kaleidoscope_auto_id"} becomes \code{"autoid.kp"} and
#'     \code{"sppaccp"} becomes \code{"autoid.sb"} - this makes sense once
#'     you notice the function merges files from either k-Pro/Kaleidoscope
#'     ("kp") or SonoBat ("sb") vetting software: Kaleidoscope's own auto
#'     ID column becomes \code{$autoid.kp}, SonoBat's "accepted species"
#'     column becomes \code{$autoid.sb}.
#'   \item \strong{Reorder} to \code{filename, date.monitoringnight, aru.name, serial,
#'     lat, lon, manid, autoid.kp, autoid.sb, date, time}.
#'   \item \code{\link{batz.datawrangler_call.datetime}} is run on
#'     \code{$date}/\code{$time} to add \code{$call.datetime}.
#'   \item \code{\link{batz.batusa_recode.names}} is run on \code{$manid},
#'     \code{$autoid.kp}, and \code{$autoid.sb} in place, using
#'     \code{batname.format.out = bat.names.out} (default \code{grammar.dash =
#'     TRUE}) - any value not recognized as a species name/code (e.g.
#'     \code{"NOISE"}, \code{"NoID"}, or a blank) passes through
#'     unchanged.
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
#' \strong{Update (2026-08-26, later) - \code{bat.names.out}.} Josh's own
#' instruction for this input was terse ("If bat.names = 'code4' ... set
#' the output name to default") and admits more than one reading - read
#' here as: \code{bat.names.out} IS the \code{batname.format.out} value passed to
#' every \code{batz.batusa_recode.names()} call in the pipeline above
#' (replacing the previously-hardcoded, unconfigurable \code{"common"}),
#' and "default" refers to \code{bat.names.out}'s OWN default value
#' (\code{"code4"}), not to \code{batz.batusa_recode.names()}'s internal
#' default (\code{"common"}). Concretely: with the default
#' \code{bat.names.out = "code4"}, \code{$manid}/\code{$autoid.kp}/
#' \code{$autoid.sb} now come back as 4-letter codes (e.g.
#' \code{"epfu"}) instead of common names (e.g. \code{"Big brown bat"}) -
#' a real behavior change from the version delivered earlier the same day.
#' \strong{Please confirm this is what was meant} - the alternative
#' reading (leave \code{batz.batusa_recode.names()} at its own built-in
#' default of \code{"common"} whenever \code{bat.names.out == "code4"}, making
#' the new parameter inert for its default value) was considered and
#' rejected as a strange thing to add a whole new parameter for.
#'
#' (2026-08-29: \code{batz.batusa_recode.names()}'s \code{output.format}
#' parameter was renamed to \code{batname.format.out}; call sites and this
#' documentation updated to match.)
#'
#' (2026-08-29, later, per Josh: this function's own \code{bat.names}
#' parameter was renamed to \code{bat.names.out}, since it too is an OUTPUT
#' format passed straight through to \code{batname.format.out} - matching
#' the package-wide \code{.in}/\code{.out} format-parameter convention.)
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("update
#' batz.merge_vetted.acoustics() to include copying over $sunregion
#' from input data"): $sunregion is now copied through when a raw input
#' file already has it.} Read as OPTIONAL pass-through, not a new required
#' header: \code{$sunregion} is NOT added to the expected-headers list
#' above, so a file that lacks it is still merged normally exactly as
#' before - its rows just get \code{NA} for \code{$sunregion} - matching
#' this function's existing tolerant, skip-only-on-genuinely-missing-
#' required-headers design. A file whose (standardized) headers DO include
#' \code{sunregion} has that column carried straight through, unchanged,
#' into \code{vetted.merged}, placed next to \code{$serial}/\code{
#' $aru.name} (the other detector-level columns) in the final column
#' order rather than at the very end. This does NOT make the function
#' itself perform the join described in \code{\link{batz.generate_plotframe.bat}}'s
#' own documentation (matching \code{$aru.name} against an
#' \code{*arulist.csv}) - it only preserves \code{$sunregion} when the raw
#' per-file input already carries it (e.g. a future export, or a file
#' Josh has manually augmented). Verified with two new files added to a
#' small synthetic fixture (one WITH a \code{sunregion}-named header in
#' varying case/punctuation, one entirely WITHOUT), merged together in one
#' call: the file that had it comes through with its real value, the file
#' that didn't gets \code{NA}, and the real \code{FinalVetted.csv} test
#' data (which has no \code{sunregion} column at all) is unaffected -
#' still merges the same row count as before, just with an all-
#' \code{NA} \code{$sunregion} column added. Full existing test suite (10
#' tests) re-run clean, no regressions.
#'
#' \strong{Follow-up, 2026-08-30, per Josh (bug report: a real Mobile-transect
#' vetted export - \code{FY26_SevenIslands_NABat_105059_Mobile_FinalVetted.csv}
#' - was silently skipped entirely) - \code{$serial} is now OPTIONAL, handled
#' exactly like \code{$sunregion} above.} Confirmed against the reported file:
#' it has no \code{Serial}/similar header anywhere (Mobile-transect exports
#' use a vehicle-mounted detector with no fixed instrument serial number, only
#' a route/grid ID embedded in \code{$filename}, e.g.
#' \code{"105059-MOB_..."}), so it was failing \code{expected.headers} on
#' \code{serial} alone and being skipped with \code{$reason = "mismatched
#' headers"} - silently, since \code{log.file} defaults to \code{FALSE}. Load-
#' pattern matching was NOT the problem (verified directly: both
#' \code{"*vetted.csv"} and the exact file name match this file's name via
#' \code{glob2rx()}/\code{list.files()}). \code{serial} is now REMOVED from
#' \code{expected.headers}, so a file is never skipped for lacking it; its
#' value is instead captured (like \code{$sunregion}) before the
#' \code{expected.headers} trim and carried through - a stationary-ARU file
#' that has \code{Serial} keeps its real value, a Mobile file (or any file
#' without it) gets \code{NA} for \code{$serial} instead of being dropped
#' entirely. Column order is unaffected (\code{$serial} still lands in the
#' same position it always did, immediately before \code{$sunregion}).
#'
#' \strong{Added 2026-09-22, per Josh's request to audit and extend the
#' snake_case output option package-wide.} A recent audit flagged this
#' function, alongside \code{\link{batz.plotactivity_daily.count}}, as
#' producing a data-frame output in this package's own dot-separated header
#' convention without the \code{snake_case} escape hatch already shipped in
#' \code{\link{batz.generate_plotframe.bat}} (2026-09-21). The new
#' \code{snake_case} parameter here works exactly the same way: it is
#' applied, as the very last step, only to \code{vetted.merged}'s OWN output
#' column names (\code{$date.monitoringnight}, \code{$aru.name}, \code{$autoid.kp}, ...
#' - this function's own invented schema) - never to any raw per-file input
#' header, which is already standardized separately via
#' \code{standardize.headers()} upstream of the rename/reorder pipeline (see
#' \strong{Header standardization} above). It is also applied, when
#' \code{log.file = TRUE}, to \code{vetted.merged_log.file}'s own headers
#' (\code{$filepath}, \code{$reason}, \code{$headers.missing}) for the same
#' output-uniformity reason, even though none of those three names contain a
#' dot to begin with and so are unaffected in practice by
#' \code{standardize.headers()} - included anyway so a caller gets a
#' consistently-cased pair of outputs rather than having to remember that
#' only one of the two objects responds to \code{snake_case}.
#'
#' \strong{\code{date.mon} renamed to \code{date.monitoringnight} (round
#' twenty-five), 2026-09-25, per Josh ("I changed my mine and want to use
#' date.monitoringnight instead of date.mon to be more consistent with
#' collaborators").} This function's own output column, previously
#' \code{$date.mon} (set by the positional rename and then carried through
#' reorder/every downstream step - see "Update (2026-08-26)" above), is now
#' \code{$date.monitoringnight} - the positional rename's target list and
#' the reorder's column list are both updated accordingly. This applies the
#' same package-wide rename already made in
#' \code{\link{batz.plotactivity_daily.count}},
#' \code{\link{batz.plotactivity_heatmap}},
#' \code{\link{batz.generate_suntimes.arulist}}, and
#' \code{\link{batz.generate_plotframe.bat}} this same round, so every
#' \code{batz} function agrees on one name for this field rather than
#' mixing \code{date.mon} and \code{date.monitoringnight} across the
#' package. Nothing else in this function's logic changes - this is a pure
#' rename of the string literal in two places (the positional-rename target
#' vector and the reorder column vector).
#'
#' \strong{Follow-up, 2026-09-25, per Josh's package-wide audit request ("as
#' I make these changes ID any other functions in the package that maybe
#' impacted, make changes as needed") following his column rename inside
#' \code{\link{batz.batusa_recode.names}}'s embedded reference table -
#' checked, and this function is NOT broken by it, only its docs are
#' stale.} \code{bat.names.out}'s default (\code{"code4"}) is untouched by
#' the rename, so every call in this function's pipeline
#' (\code{batname.format.out = bat.names.out}, on \code{$manid}/
#' \code{$autoid.kp}/\code{$autoid.sb}) keeps working exactly as before for
#' any caller who hasn't touched \code{bat.names.out}. \strong{The dated
#' entries above ("Update (2026-08-26, later)") are left exactly as
#' originally written, per this project's standing append-don't-rewrite-
#' history convention, even though they describe
#' \code{batz.batusa_recode.names()}'s OLD internal default as
#' \code{"common"} - that default is now \code{"common_name"}.} Only the
#' current-interface \code{@param bat.names.out} example list just below
#' was updated to the new names (\code{"common_name"}/
#' \code{"scientific_name"}), since that describes the CURRENT valid
#' values, not a historical snapshot. If a caller explicitly passes
#' \code{bat.names.out = "common"} or \code{bat.names.out = "latin"} (the
#' old names), that call will now error - same as any other now-invalid
#' \code{batname.format.out} literal - but no such call site was found
#' anywhere in this package.
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
#' @param bat.names.out Character, default \code{"code4"}. The
#'   \code{batname.format.out} passed to \code{\link{batz.batusa_recode.names}}
#'   when recoding \code{$manid}/\code{$autoid.kp}/\code{$autoid.sb} - must
#'   be one of that function's valid \code{batname.format.out} values (e.g.
#'   \code{"code4"}, \code{"common_name"}, \code{"code6"},
#'   \code{"scientific_name"}, ... - \code{"common"}/\code{"latin"} prior to
#'   2026-09-25, see Details).
#'   Not specified in the original spec beyond its default; read as "the
#'   format the recoded manual/auto ID columns end up in" and passed
#'   straight through to \code{batname.format.out} - see Details.
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
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-22, per
#'   Josh's request to audit and extend the snake_case output option
#'   package-wide (see \code{\link{batz.generate_plotframe.bat}}'s sibling
#'   parameter). Controls only \code{vetted.merged}'s (and, if
#'   \code{log.file = TRUE}, \code{vetted.merged_log.file}'s) OWN output
#'   column names, applied as the very last step before either is
#'   returned/assigned - it has no effect on any raw per-file input header.
#'   \code{FALSE} (default) keeps this function's normal dot-separated
#'   output column names (\code{$date.monitoringnight}, \code{$aru.name},
#'   \code{$autoid.kp}, ...) exactly as always. \code{TRUE} runs every
#'   output column name through \code{standardize.headers()} instead (e.g.
#'   \code{$date_monitoringnight}, \code{$aru_name}) - for a caller who specifically
#'   wants a snake_case CSV/data frame out of this function, without having
#'   to convert it themselves afterward.
#'
#' @return Invisibly, a named list: \code{vetted.merged} (always), and
#'   \code{vetted.merged_log.file} (only if \code{log.file = TRUE}) - or
#'   their snake_case equivalents if \code{snake_case = TRUE} (see that
#'   parameter above). As a side effect, the same object(s) are also
#'   assigned directly into the calling environment (same auto-assign
#'   convention as \code{batz.merge_aru.meta}/
#'   \code{batz.datawrangler_load.files}), so a bare call with no
#'   assignment populates \code{vetted.merged} (and
#'   \code{vetted.merged_log.file}) directly.
#'
#' @examples
#' \dontrun{
#' # bare call - creates `vetted.merged` (and `vetted.merged_log.file`, if
#' # log.file = TRUE) directly in the calling environment
#' batz.merge_vetted.acoustics(dir.sub = TRUE, log.file = TRUE)
#' head(vetted.merged)
#' vetted.merged_log.file
#'
#' # snake_case output headers instead of this function's usual dot-style
#' batz.merge_vetted.acoustics(snake_case = TRUE)
#' }
#'
#' @export
batz.merge_vetted.acoustics <- function(dir.load = getwd(),
                                               load.pattern = c("*vetted.csv"),
                                               dir.sub = FALSE,
                                               duplicates.remove = TRUE,
                                               log.file = FALSE,
                                               bat.names.out = "code4",
                                               manid.kp = TRUE,
                                               manid.sb = TRUE,
                                               trim.noise = TRUE,
                                               trim.noid = FALSE,
                                               snake_case = FALSE) {

  ## header standardization (per Josh, 2026-09-14 project preference):
  ## expected.headers is a literal, uninvented copy of the vetting
  ## software's own real column text, so it is standardized right along
  ## with incoming raw headers via the shared standardize.headers()
  ## helper - see @details "Header standardization" above.
  expected.headers <- c("filename", "monitoringnight", "species_manual_id",
                         "wa_kaleidoscope_auto_id", "sppaccp", "lat")

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
    names(tmp) <- standardize.headers(names(tmp))
    missing.headers <- setdiff(expected.headers, names(tmp))
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", ")); next
    }
    if (nrow(tmp) == 0) { add.log(f, "no records", "none"); next }
    ## $serial is OPTIONAL (2026-08-30 follow-up), not one of the required
    ## expected.headers - a file is never skipped for lacking it (e.g. a
    ## Mobile-transect export, which has no fixed instrument serial number at
    ## all). Captured BEFORE trimming to expected.headers below, exactly like
    ## $sunregion, since that trim would otherwise silently drop it.
    serial.vals <- if ("serial" %in% names(tmp)) as.character(tmp$serial) else
      rep(NA_character_, nrow(tmp))
    ## $sunregion is OPTIONAL, not one of the required expected.headers - a
    ## file is never skipped for lacking it. If a file's own (standardized)
    ## headers happen to include it, copy those values straight through;
    ## otherwise this file's rows get NA for $sunregion (still needs to be
    ## joined in separately, exactly as before, for files that don't already
    ## carry it). Captured BEFORE trimming to expected.headers below, since
    ## that trim would otherwise silently drop it.
    sunregion.vals <- if ("sunregion" %in% names(tmp)) as.character(tmp$sunregion) else
      rep(NA_character_, nrow(tmp))
    tmp <- tmp[, expected.headers, drop = FALSE]
    tmp$serial <- serial.vals
    tmp$sunregion <- sunregion.vals
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

    ## positional rename ($sunregion, appended right after $serial back in
    ## the per-file loop above, keeps its own name here - no rename needed)
    names(vetted.merged) <- c("filename", "date.monitoringnight", "manid", "autoid.kp",
                               "autoid.sb", "lat", "serial", "sunregion", "lon",
                               "aru.name", "date", "time")

    ## reorder ($sunregion placed with the other detector-level columns,
    ## next to $serial/$aru.name)
    vetted.merged <- vetted.merged[, c("filename", "date.monitoringnight", "aru.name",
                                        "serial", "sunregion", "lat", "lon",
                                        "manid", "autoid.kp", "autoid.sb",
                                        "date", "time")]

    ## $call.datetime
    vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = vetted.merged$date, time = vetted.merged$time)

    ## recode manid/autoid.kp/autoid.sb (batname.format.out = bat.names.out, default "code4")
    vetted.merged$manid     <- batz.batusa_recode.names(vetted.merged$manid, batname.format.out = bat.names.out)
    vetted.merged$autoid.kp <- batz.batusa_recode.names(vetted.merged$autoid.kp, batname.format.out = bat.names.out)
    vetted.merged$autoid.sb <- batz.batusa_recode.names(vetted.merged$autoid.sb, batname.format.out = bat.names.out)

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

  ## snake_case output option (per Josh's request to audit and extend this
  ## package-wide, 2026-09-22) - applied last, only to this function's own
  ## invented output column names, mirroring batz.generate_plotframe.bat()'s
  ## established pattern. See @param snake_case.
  if (snake_case) names(vetted.merged) <- standardize.headers(names(vetted.merged))

  result <- list(vetted.merged = vetted.merged)

  if (log.file) {
    vetted.merged_log.file <- if (length(log.rows) > 0) do.call(rbind, log.rows) else
      data.frame(filepath = character(0), reason = character(0), headers.missing = character(0))
    rownames(vetted.merged_log.file) <- NULL
    if (snake_case) names(vetted.merged_log.file) <- standardize.headers(names(vetted.merged_log.file))
    result$vetted.merged_log.file <- vetted.merged_log.file
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())
  invisible(result)
}

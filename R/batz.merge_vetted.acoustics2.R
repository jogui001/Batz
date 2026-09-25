#' Merge vetted bat-acoustic-call files (v2, 2026-09-08 revision) - short
#' abbreviated headers throughout, header-rename lookup, two auto-ID
#' programs, and a single data+log.file return object
#'
#' A revised version of \code{\link{batz.merge_vetted.acoustics}}, and a
#' further revision of an earlier \code{batz.merge_vetted.acoustics2}
#' (2026-09-04, revised 2026-09-06, revised again 2026-09-08). Scans a
#' directory for vetted bat-acoustic-call files, standardizes each file's
#' headers into a fixed set of SHORT, abbreviated column names via a rename
#' lookup table, validates the result against three header categories
#' (required, results, optional), merges every file that passes into one
#' combined table, and returns a single object holding both the merged data
#' and a per-file load-status log - always as two data frames named
#' \code{data} and \code{log.file} (see Value). Optionally saves both to a
#' single xlsx workbook.
#'
#' @param dir.load Character, default \code{getwd()}. Directory to scan.
#' @param load.pattern Character vector, default \code{c("*vetted.csv")}. A
#'   wildcard/glob pattern (or vector of patterns) identifying which files to
#'   load, converted internally to a regex via \code{utils::glob2rx()}.
#'   Matching is case-insensitive.
#' @param dir.sub Logical, default \code{FALSE}. Also search subdirectories
#'   of \code{dir.load}.
#' @param duplicates.remove Logical, default \code{TRUE}. Remove exact
#'   duplicate rows (base \code{duplicated()}) from the final merged table.
#' @param rename Logical, default \code{TRUE}. If \code{TRUE}, each file's
#'   standardized headers are first run through a rename lookup read from
#'   \code{header.rename.path} (see Details) before validation. If
#'   \code{FALSE}, headers are only standardized (see "Header
#'   standardization" below), and only headers that already happen to be
#'   one of this function's short target names (e.g. a file whose own
#'   column is already literally called \code{date.monitoringnight}) will validate.
#' @param header.rename.path Character, default
#'   \code{"arumerge.headerrename.csv"}. Path to the header-rename reference
#'   CSV, read once via \code{read.csv(header.rename.path)} when
#'   \code{rename = TRUE} and added to the (built-in, empty) \code{headers}
#'   lookup table - see Details.
#' @param bat.names.out Character, default \code{"code4"}. Passed as
#'   \code{batname.format.out} to \code{\link{batz.batusa_recode.names}} when
#'   recoding \code{$manid} and any surviving auto-ID column(s) (\code{
#'   $auto.kp}/\code{$auto.sb}).
#' @param manid.kp Logical, default \code{TRUE}. Create \code{$manid.kp} (a
#'   copy of \code{$manid} with blanks/NA filled from \code{$auto.kp}) - only
#'   if \code{$auto.kp} survived the all-NA column drop (see Details).
#' @param manid.sb Logical, default \code{TRUE}. Same idea, filled from
#'   \code{$auto.sb}.
#' @param trim.noise Logical, default \code{TRUE}. Remove rows where
#'   \code{$manid} is \code{"noise"} (case-insensitive).
#' @param trim.noid Logical, default \code{FALSE}. Remove rows where
#'   \code{$manid} is \code{"NoID"} (case-insensitive).
#' @param project.name Character, default \code{""}. Used to build the saved
#'   xlsx file's name (see \code{save.xlsx} below). \strong{As of round
#'   twenty-two (2026-09-24) the saved file name also includes a
#'   \code{<daterange>} token - see \code{@param save.xlsx} and @details,
#'   "File naming (round twenty-two)".}
#' @param save.xlsx Logical, default \code{TRUE}. If \code{TRUE}, writes
#'   \code{data} and \code{log.file} as two sheets of one xlsx workbook into
#'   \code{dir.load}, named
#'   \verb{<project.name>_merge_vetted.acoustics_<daterange>_<timestamp>.xlsx}
#'   (\strong{changed in round twenty-two, 2026-09-24, per Josh's
#'   package-wide file-naming request - previously
#'   \code{"<project.name>_<Date>_merge_vetted.acoustics.xlsx"}; see
#'   @details, "File naming (round twenty-two)"}). Guarded with
#'   \code{requireNamespace("openxlsx", ...)} - warns and skips the save
#'   (does not error) if not installed.
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-22, per
#'   Josh, alongside the tolerant-header-matching fix above. Controls only
#'   \code{data}/\code{log.file}'s OWN output column names, applied as the
#'   very last step before the optional xlsx write and before they're
#'   returned/auto-assigned - it has no effect on how an incoming file's
#'   headers are matched/validated. \code{FALSE} (default) keeps this
#'   function's normal dot-separated output column names (\code{
#'   $date.monitoringnight}, \code{$auto.kp}, \code{$aru.serial}, ...) exactly as
#'   always. \code{TRUE} runs every output column name through
#'   \code{standardize.headers()} instead (e.g. \code{$mon_ngh}, \code{
#'   $aru_serial}) - for a caller who specifically wants a snake_case
#'   CSV/xlsx/data frame out of this function, without having to convert it
#'   themselves afterward.
#'
#' @return Invisibly, a named list of exactly two data frames: \code{data}
#'   (the merged/processed table) and \code{log.file} (one row per file the
#'   function ATTEMPTED to load - success or failure, always, since the
#'   2026-09-06 revision removed the old \code{log.file} INPUT parameter that
#'   used to gate this - with \code{$filename}, \code{$status}, \code{
#'   $reason}, \code{$missing headers}). As a side effect, \code{data} and
#'   \code{log.file} are also assigned directly into the calling
#'   environment (same auto-assign convention as
#'   \code{batz.merge_vetted.acoustics}), so a bare call with no assignment
#'   populates both names directly.
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real matching-behavior change, flagged not silently made:} every
#' incoming raw file header, and the "raw" column of the
#' \code{header.rename.path} reference table, are now both run through the
#' shared package helper \code{standardize.headers()} (trim whitespace,
#' collapse every run of non-alphanumeric characters to a single
#' underscore, lowercase - i.e. snake_case), replacing this function's own
#' prior \code{normalize.header <- function(x) tolower(gsub("[^[:alnum:]]",
#' "", x))}, which stripped separators out entirely instead of preserving
#' them as underscores. Because the old normalization deleted word
#' boundaries, a raw header like \code{"Species Manual ID"} or \code{"Sun
#' Region"} used to collapse all the way down to \code{"speciesmanualid"}/
#' \code{"sunregion"} - which happened to already equal one of this
#' function's canonical short target names, so it matched WITHOUT needing a
#' \code{header.rename.path} row at all. Under the new, word-boundary-
#' preserving standardization, those same headers become
#' \code{"species_manual_id"}/\code{"sun_region"} instead, which no longer
#' auto-match the canonical names by coincidence. \code{arumerge.headerrename.csv}
#' was updated accordingly (see "Flagged assumptions" below) so real-world
#' spelled-out headers still resolve correctly - anyone with their OWN
#' header-rename CSV (not the one shipped in this project) should re-check
#' it against this new normalization.
#'
#' \strong{Short target column names.} Where the prior (2026-09-04) version
#' of this function used the long, vetting-software-native header names
#' (\code{monitoringnight}, \code{speciesmanualid}, \code{
#' wakaleidoscopeautoid}, \code{sppaccp}, \code{serial}) as the actual
#' working/output column names, this revision uses short abbreviated names
#' throughout instead: \code{filename}, \code{date.monitoringnight}, \code{manid},
#' \code{auto.kp}, \code{auto.sb}, \code{lat}, \code{lon}, \code{aru.serial},
#' \code{sunregion}. These are this function's own invented output schema
#' (like the rest of this package's dot-separated naming convention) and are
#' NOT run through \code{standardize.headers()} themselves - only the
#' incoming raw headers and the rename table's raw column are. (The
#' 2026-09-06 revision briefly added two further auto-ID columns, \code{
#' auto.bc}/\code{auto.ec}, for a raw \code{BCID}/\code{EchoClass} header -
#' these were removed again in the 2026-09-08 revision; see "Flagged
#' assumptions" below.)
#'
#' \strong{Header rename (\code{rename}/\code{header.rename.path}).} The
#' working \code{headers} lookup table starts empty; when \code{rename =
#' TRUE}, \code{header.rename.path} is read once via \code{read.csv()} as a
#' plain two-column reference table (read BY POSITION, not by name) and
#' added to it - column 1 is a RAW header a file might actually have (this
#' can be an abbreviated Josh-ism like \code{file.name}/\code{serial}/
#' \code{sunregions}, OR the long name a real vetting-software export
#' normalizes to, like \code{monitoringnight}/\code{speciesmanualid}/\code{
#' wakaleidoscopeautoid}/\code{sppaccp}, OR an alternate location-column
#' spelling like \code{long}/\code{x}/\code{y}), column 2 is the SHORT
#' target name it becomes. Only column 1 is standardized (via
#' \code{standardize.headers()}) to match a file's own standardized headers
#' - column 2 is kept exactly as given (including its dots, e.g. \code{
#' "date.monitoringnight"} - standardizing it too would turn the dot into an underscore).
#' If the CSV itself has more than one row for the same raw header, only the
#' FIRST is kept ("ignore conflicting headers") - later duplicate rows are
#' silently dropped. A header with no match anywhere in the table is left
#' unchanged (i.e. only useful if it's already one of the short target
#' names, or gets dropped as unrecognized by the validation step below).
#' \code{claude/arumerge.headerrename.csv} was updated for this revision -
#' see "Flagged assumptions" below.
#'
#' \strong{Three header categories.} After standardize + optional rename,
#' every file's headers are checked against:
#' \itemize{
#'   \item \strong{Required} - \code{filename}, \code{date.monitoringnight}, \code{manid}.
#'     ALL must be present, or the file fails.
#'   \item \strong{Results} - \code{auto.kp}, \code{auto.sb}. At least ONE
#'     must be present, or the file fails; an absent one is carried as NA
#'     for that file's rows and the file's status is \code{"success
#'     missing"} rather than a failure.
#'   \item \strong{Optional} - \code{lat}/\code{lon} (see below),
#'     \code{aru.serial}, \code{sunregion}. NEVER block loading; carried as
#'     NA for any file that lacks them.
#' }
#' \code{$lat}/\code{$lon} moved from their own blocking "location" category
#' in the 2026-09-04 build to fully optional here: if a file has both
#' \code{lat}/\code{lon}, they're used directly (coerced numeric); if it has
#' only \code{$lat}, that raw value holds \code{"<lat> <lon>"} as one
#' space-separated string, split into numeric \code{$lat}/\code{$lon}; if
#' neither is present, both are NA. \code{$long} (full spelling instead of
#' \code{$lon}) and an \code{X}/\code{Y} column pair are also recognized -
#' via the \code{header.rename.path} table (\code{long -> lon},
#' \code{x -> long}, \code{y -> lat}; the \code{x -> long} row then
#' resolves the rest of the way to \code{lon} through the same built-in
#' \code{long}-is-an-alias-for-\code{lon} step used for a bare \code{$long}
#' column) rather than through dedicated location-parsing code - so this
#' only works when \code{rename = TRUE} (the default) and the table
#' includes those rows (added 2026-09-06, per Josh - see "Flagged
#' assumptions" below). A file that fails to read at all, has zero data
#' rows, or fails the required/results checks is skipped (not merged into
#' \code{data}).
#'
#' \strong{Everything after validation/merge}: exact-duplicate removal
#' (\code{duplicates.remove}), \code{$filename} parsed into
#' \code{$aru.name}/\code{$date}/\code{$time}, \code{$call.datetime} via
#' \code{\link{batz.datawrangler_call.datetime}}, then a step that drops any
#' of \code{$auto.kp}/\code{$auto.sb} that is 100\% \code{NA} across the
#' WHOLE merged table (i.e. no file that contributed to \code{data} ever
#' supplied that auto-ID program) entirely as a column - then species-ID
#' recoding via \code{\link{batz.batusa_recode.names}} (\code{
#' batname.format.out = bat.names.out}; NA values are left as NA rather than
#' passed to the recode function) on \code{$manid} and whichever auto-ID
#' column(s) survived, then \code{$manid.kp}/\code{$manid.sb} blank-fill
#' (each only created if its flag is TRUE AND its source auto-ID column
#' survived the all-NA drop), and finally \code{trim.noise}/\code{trim.noid}
#' row removal - unchanged since the 2026-09-06 revision.
#'
#' \strong{\code{log.file} format.} One row per file the function attempted
#' to load (success or failure - always, since the 2026-09-06 revision
#' removed the old \code{log.file} INPUT parameter): \code{$filename} (full
#' path), \code{$status} (\code{"failure"}, \code{"success all headers"}, or
#' \code{"success missing"}), \code{$reason} (\code{"unreadable"}, \code{
#' "missing rows"}, \code{"missing required headers"}, and/or \code{"missing
#' results headers"} for a failure - blank for a success), \code{$missing
#' headers} (comma-separated list of missing results/optional headers for a
#' \code{"success missing"} file, or the missing required/results headers
#' for a \code{"failure"} file - blank for \code{"success all headers"}).
#'
#' \strong{Flagged assumptions (spec was open on these):}
#' \enumerate{
#'   \item The supplied header-rename table's rows read literally as
#'     \verb{<in> <recode>}. Two of its rows (\code{file.name ->
#'     filename}, \code{sunregions -> sunregion}) are unambiguous
#'     raw-header-text -> short-target-name pairs; the others, read the
#'     same way, are equally consistent with that direction (e.g.
#'     \code{monitoringnight} - the long name a real export normalizes to -
#'     renamed to the short \code{mon.ngh}, per \code{arumerge.headerrename.csv}'s
#'     own target column at the time). All rows are therefore applied
#'     uniformly as raw-header-text -> short-target-name. \strong{As of
#'     round twenty-five, \code{mon.ngh} is this function's OLD internal
#'     target name - see the round-twenty-five entry below.}
#'   \item The \code{serial} row's given target was \code{serials}, but the
#'     requested blank-output header is \code{$aru.serial} - read as a typo
#'     and corrected to \code{serial -> aru.serial} in
#'     \code{arumerge.headerrename.csv}.
#'   \item \strong{Removed 2026-09-08, per Josh:} the 2026-09-06 revision had
#'     briefly added \code{auto.bc}/\code{auto.ec} results columns (from raw
#'     \code{BCID}/\code{EchoClass} headers), their \code{manid.bc}/\code{
#'     manid.ec} fill-in columns, and matching \code{BCID -> auto.bc}/\code{
#'     EchoClass -> auto.ec} rows in \code{arumerge.headerrename.csv}. Josh
#'     asked for these removed because BCID and EchoClass don't follow the
#'     same export format as Kaleidoscope/SonoBat: research into how those
#'     two programs actually produce output (neither vendor's native export
#'     is a clean flat table with a literal \code{BCID}/\code{EchoClass}
#'     header cell the way Kaleidoscope's \code{wakaleidoscopeautoid} and
#'     SonoBat's \code{sppaccp} are - both are multi-section Excel reports
#'     that need marker-string row-scanning just to locate a data table, and
#'     EchoClass's real species-result column is named \code{Prominent
#'     Species}) confirmed the simple raw-header-cell-rename model this
#'     function uses for Kaleidoscope/SonoBat doesn't fit either program.
#'     \code{results.headers} is back to \code{auto.kp}/\code{auto.sb} only
#'     (at least one required); \code{manid.kp}/\code{manid.sb} are the only
#'     blank-fill columns again. Should BCID/EchoClass results need to be
#'     folded in later, they'll need either their own dedicated parsing step
#'     (mirroring how a raw export is actually structured) or confirmation
#'     that Josh's team already hand-builds a column literally labeled
#'     \code{BCID}/\code{EchoClass} into the vetted sheet before it reaches
#'     this function.
#'   \item \strong{Follow-up (2026-09-06, per Josh):} the initial version of
#'     \code{arumerge.headerrename.csv} for the 2026-09-06 revision dropped
#'     the alternate location-header spellings that the 2026-09-04 build had
#'     handled with dedicated code (\code{$long} full spelling, and an
#'     \code{X}/\code{Y} column pair) - flagged then as "not part of that
#'     revision's spec". Josh asked for them back, so three rows were added
#'     to the table instead of restoring the old dedicated code:
#'     \code{long -> lon}, \code{x -> long}, \code{y -> lat}. The
#'     \code{x -> long} row relies on the same \code{long}-is-an-alias-for-
#'     \code{lon} step (see the "Three header categories" section above)
#'     to finish resolving to \code{lon} - so \code{X}/\code{Y} support now
#'     depends on \code{rename = TRUE} (the default) and an intact
#'     \code{header.rename.path} table, rather than working unconditionally
#'     as it did in the 2026-09-04 build. These rows are unaffected by the
#'     2026-09-08 BCID/EchoClass removal above.
#'   \item "add user defined headers to the headers dataframe by column
#'     position, ignore conflicting headers" is read as: the working lookup
#'     table starts empty, the CSV is added to it by column position when
#'     \code{rename = TRUE}, and a raw header that repeats within that CSV
#'     keeps only its first occurrence (later, conflicting rows for the same
#'     raw header are ignored).
#'   \item \strong{2026-09-14, per the new header-standardization
#'     preference:} \code{arumerge.headerrename.csv}'s raw column was
#'     rewritten to spell out the real-world header text with its original
#'     spaces/punctuation (\code{"Species Manual ID"}, \code{
#'     "WA|Kaleidoscope|Auto ID"}, \code{"Sun Region"}) instead of the old
#'     pre-squished no-separator text (\code{"speciesmanualid"}, \code{
#'     "wakaleidoscopeautoid"}) - since that raw column is now run through
#'     \code{standardize.headers()} the same as any other incoming header,
#'     writing it as literal real-world text is both more legible and
#'     future-proof than hand-squishing it. See the "Header standardization"
#'     paragraph above for why this was necessary (not just cosmetic).
#'   \item \code{trim.noise}/\code{trim.noid}/\code{manid.kp}/\code{
#'     manid.sb} are unchanged carryovers from the 2026-09-04 build (not
#'     mentioned in the 2026-09-06 round's add/remove-inputs sections at
#'     all, and untouched by the 2026-09-08 removal above).
#'   \item The \code{log.file} RETURNED DATA FRAME still exists (it's one of
#'     the two required outputs) - only the old \code{log.file} INPUT
#'     PARAMETER was removed (2026-09-06), per the request's explicit
#'     "remove inputs: log.file = TRUE". Every attempted file is
#'     unconditionally logged (matching the old parameter's \code{TRUE}
#'     behavior).
#'   \item No \code{dir.save} parameter was requested for the xlsx-save
#'     step - defaults to \code{dir.load}, unchanged from the 2026-09-04
#'     build (also unchanged by the round-twenty-two file-naming update
#'     below - see @details, "File naming (round twenty-two)").
#'   \item The xlsx save's missing-package fallback (warn + skip, no error)
#'     is unchanged. This sandbox still has no network access to install
#'     \pkg{openxlsx}, so only that fallback path was exercised here - Josh
#'     should confirm the real two-sheet save on his own machine.
#' }
#'
#' \strong{BUGFIX (2026-09-22, per Josh's request to audit and extend the
#' 2026-09-21 header-canonicalization fix - see
#' \code{\link{batz.generate_plotframe.bat}}'s own \code{canonicalize.headers}
#' \code{@details} entry - package-wide):} \code{required.headers}/\code{
#' results.headers}/\code{optional.headers} - this function's own canonical
#' short target names (\code{date.monitoringnight}, \code{auto.kp}, \code{auto.sb}, \code{
#' aru.serial}, etc.) - were never themselves standardized on the comparison
#' side, even though every incoming raw header IS run through
#' \code{standardize.headers()} before comparison (see "Header
#' standardization" above). In practice this meant a column that already
#' legitimately carried (or standardized to) one of these canonical names,
#' just spelled in a different case/separator style (e.g. \code{Mon_Ngh},
#' \code{MON.NGH}, \code{mon_ngh}), was falsely reported as MISSING unless
#' \code{header.rename.path} happened to have an exact row for that specific
#' raw spelling - the same class of bug already fixed elsewhere in this
#' package via \code{\link{canonicalize.headers}}. Fixed the same way here:
#' after the existing \code{header.rename.path}-based rename step runs
#' (unchanged - it's still needed for genuinely different raw names, e.g.
#' mapping Kaleidoscope's own native column names onto these short canonical
#' ones), any of \code{required.headers}/\code{results.headers}/\code{
#' optional.headers} not yet present under its exact canonical spelling is
#' ALSO run through a \code{\link{canonicalize.headers}}-style pass - both
#' \code{tmp}'s current column names and the still-needed canonical name(s)
#' are standardized purely to match them up, and any match found is renamed,
#' in this function's own per-file working copy only, to the exact canonical
#' spelling - as a supplementary fallback layer on top of the rename table,
#' not a replacement for it. Every other existing behavior (the
#' \code{results.headers} "at least one of" logic, the all-NA auto-id-column
#' drop, the \code{manid.kp}/\code{manid.sb} fill-in logic, duplicate
#' handling, the \code{$lat}/\code{$lon}/\code{$long}/\code{X}/\code{Y}
#' location handling, everything) is unchanged. Full dev-script test suite
#' re-run (no regressions), plus a new test confirming a file whose headers
#' are already spelled like the canonical names in a different case/
#' separator style (e.g. \code{Mon_Ngh}, \code{AUTO.KP}), with no
#' corresponding row in \code{arumerge.headerrename.csv}, now loads/merges
#' successfully instead of being rejected as missing required headers.
#'
#' \strong{NEW (2026-09-22, per Josh, alongside the BUGFIX above):} added the
#' \code{snake_case} parameter (see below) - mirroring the identically-named,
#' identically-behaved parameter already shipped in
#' \code{\link{batz.generate_plotframe.bat}}. \code{FALSE} (default) leaves
#' \code{data}/\code{log.file}'s own output column names exactly as always
#' (this function's usual dot-separated convention, e.g. \code{$date.monitoringnight},
#' \code{$aru.serial}). \code{TRUE} runs both \code{data}'s and \code{
#' log.file}'s column names through \code{standardize.headers()} as the very
#' last step before the optional xlsx write and before they're returned/
#' auto-assigned into the caller's environment - so a caller who specifically
#' wants a snake_case CSV/xlsx/data frame out of this function (e.g.
#' \code{$mon_ngh}, \code{$aru_serial}, \code{$missing_headers}) gets one
#' without converting it themselves afterward. This has no effect on how an
#' incoming file's own raw headers are standardized/matched/validated -
#' those steps are entirely upstream of this, and unaffected either way.
#'
#' \strong{File naming (round twenty-two), 2026-09-24, per Josh's
#' package-wide request ("Update all functions that save files or charts:
#' ... for files follow <project.name>_<filetype.name>_<daterange>_
#' <timestamp>"):} the saved xlsx workbook's name changes from
#' \code{"<project.name>_<Date>_merge_vetted.acoustics.xlsx"} (a bare
#' calendar-day stamp, \code{format(Sys.Date(), "\%Y-\%m-\%d")}, with no
#' data-range concept at all) to
#' \verb{<project.name>_merge_vetted.acoustics_<daterange>_<timestamp>.xlsx},
#' using \code{"merge_vetted.acoustics"} as the \code{<filetype.name>}
#' token (matching the function's own name/subject). \strong{Flagged as a
#' judgment call:} the new \code{<daterange>} token is computed from
#' \code{data.merged$date} - the 8-digit \code{YYYYMMDD} token this
#' function already parses out of each row's \code{$filename} - rather than
#' from \code{$call.datetime} (the fuller parsed timestamp derived from
#' that same \code{$date}/\code{$time} pair); \code{$date} was chosen
#' because it's the simpler, already-string-formatted source and a
#' date-only range reads more naturally as a \verb{<DATE1>to<DATE2>} token
#' than a full datetime would. \strong{Please confirm \code{$date} (not
#' \code{$call.datetime}) is the right source.} If \code{data.merged} has
#' zero rows (nothing loaded/merged, or everything trimmed away by
#' \code{trim.noise}/\code{trim.noid}), the literal token \code{"nodata"}
#' is used in place of a real date range rather than crashing on an empty
#' \code{min()}/\code{max()} - also not explicitly specified, likewise a
#' judgment call. \code{<timestamp>} uses the same 14-digit, no-separator
#' \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")} shape as every other
#' \code{batz} function in this round - see each function's own "Timestamp
#' format (round twenty-two)" \code{@details} entry. \code{dir.load} (not a
#' separate \code{dir.save}) remains the save location, unchanged - see
#' "Flagged assumptions" above.
#'
#' \strong{\code{mon.ngh} renamed to \code{date.monitoringnight} (round
#' twenty-five), 2026-09-25, per Josh ("same thing make the change" -
#' extending item 1's \code{date.mon} -> \code{date.monitoringnight} rename
#' to this function's own \code{mon.ngh} abbreviation, so every \code{batz}
#' function agrees on one name for this field).} \code{required.headers},
#' \code{canonical.headers}, and the final reorder step's column list are
#' all updated from \code{mon.ngh} to \code{date.monitoringnight} - this
#' function's OWN internal target name for the field is now
#' \code{date.monitoringnight} throughout, matching
#' \code{\link{batz.merge_vetted.acoustics}},
#' \code{\link{batz.plotactivity_daily.count}},
#' \code{\link{batz.plotactivity_heatmap}},
#' \code{\link{batz.generate_suntimes.arulist}},
#' \code{\link{batz.generate_plotframe.bat}}, and
#' \code{\link{batz.plotsm4_heatmap}} from this same round.
#'
#' \strong{Flagged, important: this does NOT touch
#' \code{arumerge.headerrename.csv}.} That file lives outside this
#' package's own \code{R}/\code{man} files (it's read at runtime from
#' \code{header.rename.path}, default \code{"arumerge.headerrename.csv"},
#' wherever \code{dir.load} points), so it can't be edited from here - and
#' as of this round its own target/"standard" column still maps various
#' raw headers (e.g. \code{monitoringnight}) to the OLD short name
#' \code{mon.ngh}, not the new \code{date.monitoringnight}. Left as-is,
#' this means a file processed with \code{rename = TRUE} (the default)
#' would get its \code{monitoringnight}-style raw header renamed to
#' \code{mon.ngh} by the CSV, which would then still satisfy
#' \code{required.headers}/\code{canonical.headers} ONLY because of the
#' 2026-09-22 \code{canonicalize.headers()}-based tolerant-fallback fix
#' described above - that fallback matches on standardized
#' (separator/case-insensitive) spelling, and \code{mon.ngh} does not
#' standardize to anything close to \code{date.monitoringnight}, so the
#' fallback will NOT rescue this: a real file run through this function
#' right now would fail with "missing required headers" for
#' \code{date.monitoringnight} unless \code{arumerge.headerrename.csv}'s
#' target column is updated to say \code{date.monitoringnight} instead of
#' \code{mon.ngh}. \strong{Josh needs to update that CSV's target column
#' himself (or ask for help doing so) before this function will work
#' against real data again} - flagged here rather than guessed at, since
#' this package's own files don't include that CSV's actual current
#' contents.
#'
#' @examples
#' \dontrun{
#' # bare call - creates `data` and `log.file` directly in the calling
#' # environment, and (by default) saves both to one xlsx workbook in
#' # dir.load
#' batz.merge_vetted.acoustics2(dir.sub = TRUE, project.name = "SevenIslands")
#' head(data)
#' log.file
#' }
#'
#' @export
batz.merge_vetted.acoustics2 <- function(dir.load = getwd(),
                                          load.pattern = c("*vetted.csv"),
                                          dir.sub = FALSE,
                                          duplicates.remove = TRUE,
                                          rename = TRUE,
                                          header.rename.path = "arumerge.headerrename.csv",
                                          bat.names.out = "code4",
                                          manid.kp = TRUE,
                                          manid.sb = TRUE,
                                          trim.noise = TRUE,
                                          trim.noid = FALSE,
                                          project.name = "",
                                          save.xlsx = TRUE,
                                          snake_case = FALSE) {

  ## ---- three header categories - location is under "optional" ----
  required.headers <- c("filename", "date.monitoringnight", "manid")
  results.headers  <- c("auto.kp", "auto.sb")
  optional.headers <- c("aru.serial", "sunregion")
  canonical.headers <- c("filename", "date.monitoringnight", "manid", "auto.kp", "auto.sb",
                          "lat", "lon", "aru.serial", "sunregion")

  ## header standardization (per Josh, 2026-09-14 project preference): uses
  ## the shared package helper standardize.headers() - trims whitespace,
  ## collapses every run of non-alphanumeric characters to a single
  ## underscore, lowercases - in place of this function's own prior
  ## normalize.header(). See @details "Header standardization" above for
  ## the real matching-behavior change this causes and how
  ## arumerge.headerrename.csv was updated to compensate.

  ## ---- "headers" dataframe: built-in (empty) baseline, with the
  ## user-defined header.rename.path CSV added to it by column position when
  ## rename = TRUE. Conflicting rows (a raw header that already has a
  ## mapping) are ignored - first occurrence wins. ----
  headers.default <- data.frame(raw = character(0), standard = character(0),
                                 stringsAsFactors = FALSE)
  header.rename.table <- headers.default

  if (rename) {
    user.headers <- read.csv(header.rename.path, stringsAsFactors = FALSE, check.names = FALSE)
    user.headers <- data.frame(
      ## only the RAW/source column is standardized, to match a file's own
      ## standardized headers - the STANDARD/target column is the literal
      ## final column name (kept exactly as given, dots and all - e.g.
      ## "date.monitoringnight", "auto.kp" - standardizing it too would turn those dots
      ## into underscores)
      raw      = standardize.headers(as.character(user.headers[[1]])),
      standard = trimws(as.character(user.headers[[2]])),
      stringsAsFactors = FALSE
    )
    user.headers <- user.headers[!duplicated(user.headers$raw), , drop = FALSE]
    new.rows <- user.headers[!(user.headers$raw %in% header.rename.table$raw), , drop = FALSE]
    header.rename.table <- rbind(header.rename.table, new.rows)
  }

  ## ---- location: $lat/$lon are OPTIONAL (never block loading). If a file
  ## has both, use them directly; if it has only $lat, that raw value holds
  ## "<lat> <lon>" as one string, split into numeric $lat/$lon; if neither
  ## is present, both are carried as NA. ($long, if present instead of
  ## $lon, is treated as an alias - kept for resilience, not specified.) ----
  apply.location <- function(tmp) {
    nms <- names(tmp)
    if ("long" %in% nms && !("lon" %in% nms)) names(tmp)[names(tmp) == "long"] <- "lon"
    nms <- names(tmp)
    has.lat <- "lat" %in% nms
    has.lon <- "lon" %in% nms

    if (has.lat && has.lon) {
      tmp$lat <- as.numeric(tmp$lat)
      tmp$lon <- as.numeric(tmp$lon)
    } else if (has.lat && !has.lon) {
      latlon <- strsplit(trimws(as.character(tmp$lat)), "\\s+")
      tmp$lat <- vapply(latlon, function(x) as.numeric(x[1]), numeric(1))
      tmp$lon <- vapply(latlon, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA_real_, numeric(1))
    } else {
      tmp$lat <- rep(NA_real_, nrow(tmp))
      tmp$lon <- rep(NA_real_, nrow(tmp))
    }
    tmp
  }

  regex.pattern <- paste(utils::glob2rx(load.pattern), collapse = "|")
  files <- list.files(dir.load, pattern = regex.pattern, recursive = dir.sub,
                       full.names = TRUE, ignore.case = TRUE)

  data.merged <- data.frame()
  log.rows <- list()

  add.log <- function(filepath, status, reason, headers.missing) {
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filename = filepath, status = status, reason = reason,
      `missing headers` = headers.missing, stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  for (f in files) {
    tmp <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { add.log(f, "failure", "unreadable", "none"); next }
    if (nrow(tmp) == 0) { add.log(f, "failure", "missing rows", "none"); next }

    names(tmp) <- standardize.headers(names(tmp))

    if (rename && nrow(header.rename.table) > 0) {
      tmp <- batz.datawrangler_rename(tmp, header.rename.table, headers.rename = TRUE)
    }

    ## fallback tolerant match (per Josh, 2026-09-22 - see @details): for any
    ## of required.headers/results.headers/optional.headers not yet present
    ## under its exact canonical spelling after the rename-table step above,
    ## also try a canonicalize.headers()-style match - standardizing both
    ## tmp's current column names and the canonical name itself - so a
    ## column already spelled close to (or exactly like) the canonical name
    ## in a different case/separator style is recognized even with no
    ## header.rename.path row for it. This never touches a column already
    ## present under its exact canonical name.
    target.headers <- c(required.headers, results.headers, optional.headers)
    still.needed <- setdiff(target.headers, names(tmp))
    if (length(still.needed) > 0) {
      tmp <- canonicalize.headers(tmp, still.needed)$df
    }

    tmp <- apply.location(tmp)

    required.missing <- setdiff(required.headers, names(tmp))
    results.present   <- intersect(results.headers, names(tmp))
    results.missing   <- setdiff(results.headers, names(tmp))

    if (length(required.missing) > 0 || length(results.present) == 0) {
      fail.reasons <- character(0)
      fail.missing <- character(0)
      if (length(required.missing) > 0) {
        fail.reasons <- c(fail.reasons, "missing required headers")
        fail.missing <- c(fail.missing, required.missing)
      }
      if (length(results.present) == 0) {
        fail.reasons <- c(fail.reasons, "missing results headers")
        fail.missing <- c(fail.missing, results.headers)
      }
      add.log(f, "failure", paste(fail.reasons, collapse = "; "),
              paste(fail.missing, collapse = ", "))
      next
    }

    optional.missing <- setdiff(optional.headers, names(tmp))
    still.missing <- c(results.missing, optional.missing)

    row <- as.data.frame(matrix(NA_character_, nrow = nrow(tmp), ncol = length(canonical.headers)),
                          stringsAsFactors = FALSE)
    names(row) <- canonical.headers
    for (nm in canonical.headers) {
      if (nm %in% names(tmp)) row[[nm]] <- as.character(tmp[[nm]])
    }

    if (length(still.missing) == 0) {
      add.log(f, "success all headers", "", "")
    } else {
      add.log(f, "success missing", "", paste(still.missing, collapse = ", "))
    }

    data.merged <- rbind(data.merged, row)
  }

  if (duplicates.remove && nrow(data.merged) > 0) {
    data.merged <- data.merged[!duplicated(data.merged), ]
  }

  if (nrow(data.merged) > 0) {
    data.merged$lat <- as.numeric(data.merged$lat)
    data.merged$lon <- as.numeric(data.merged$lon)

    ## $filename = "<ARU>_<YYYYMMDD>_<HHMMSS>_<junk>" -> $aru.name/$date/$time
    m <- regmatches(data.merged$filename,
                     regexec("^([^_]+)_(\\d{8})_(\\d{6})_", data.merged$filename))
    data.merged$aru.name <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
    data.merged$date     <- vapply(m, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
    data.merged$time     <- vapply(m, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1))

    ## reorder (only the auto-id columns actually present at this point are
    ## included here - none have been dropped yet, so both that were ever
    ## present in canonical.headers still are)
    auto.cols.present <- intersect(c("auto.kp", "auto.sb"), names(data.merged))
    data.merged <- data.merged[, c("filename", "date.monitoringnight", "aru.name", "aru.serial",
                                    "sunregion", "lat", "lon", "manid",
                                    auto.cols.present, "date", "time")]

    ## $call.datetime
    data.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = data.merged$date, time = data.merged$time)

    ## remove unused auto id programs - if a whole auto-id column is NA for
    ## every record in the merge, drop the column entirely
    for (ac in c("auto.kp", "auto.sb")) {
      if (ac %in% names(data.merged) && all(is.na(data.merged[[ac]]))) {
        data.merged[[ac]] <- NULL
      }
    }

    ## recode manid + any surviving auto-id columns (batname.format.out = bat.names.out);
    ## NA elements are left as NA rather than passed into the recode function
    recode.safe <- function(x, fmt) {
      out <- x
      not.na <- !is.na(x)
      if (any(not.na)) out[not.na] <- batz.batusa_recode.names(x[not.na], batname.format.out = fmt)
      out
    }

    data.merged$manid <- recode.safe(data.merged$manid, bat.names.out)
    for (ac in intersect(c("auto.kp", "auto.sb"), names(data.merged))) {
      data.merged[[ac]] <- recode.safe(data.merged[[ac]], bat.names.out)
    }

    is.empty <- function(x) is.na(x) | !nzchar(trimws(x))

    ## fill blanks - $manid.kp/$manid.sb, one per surviving auto-id column,
    ## only created if that column is present (wasn't dropped above) and
    ## its corresponding flag is TRUE
    fill.map <- list(manid.kp = list(flag = manid.kp, src = "auto.kp"),
                      manid.sb = list(flag = manid.sb, src = "auto.sb"))
    for (out.nm in names(fill.map)) {
      spec <- fill.map[[out.nm]]
      if (isTRUE(spec$flag) && spec$src %in% names(data.merged)) {
        data.merged[[out.nm]] <- data.merged$manid
        blank <- is.empty(data.merged[[out.nm]])
        data.merged[[out.nm]][blank] <- data.merged[[spec$src]][blank]
      }
    }

    if (trim.noise) {
      data.merged <- data.merged[!(tolower(trimws(data.merged$manid)) == "noise"), , drop = FALSE]
    }

    if (trim.noid) {
      data.merged <- data.merged[!(tolower(trimws(data.merged$manid)) == "noid"), , drop = FALSE]
    }
  }

  rownames(data.merged) <- NULL

  log.file.df <- if (length(log.rows) > 0) do.call(rbind, log.rows) else
    data.frame(filename = character(0), status = character(0), reason = character(0),
               `missing headers` = character(0), stringsAsFactors = FALSE, check.names = FALSE)
  rownames(log.file.df) <- NULL

  ## ---- daterange token (round twenty-two, 2026-09-24, per Josh) - see
  ## @details, "File naming (round twenty-two)". Computed from
  ## data.merged$date (the parsed 8-digit YYYYMMDD filename token) BEFORE
  ## the snake_case rename below, and guarded for the 0-row / no-$date-
  ## column case (nothing loaded, or everything trimmed away).
  if ("date" %in% names(data.merged) && nrow(data.merged) > 0) {
    valid.dates <- as.Date(data.merged$date[!is.na(data.merged$date)], format = "%Y%m%d")
    valid.dates <- valid.dates[!is.na(valid.dates)]
  } else {
    valid.dates <- as.Date(character(0))
  }
  daterange.token <- if (length(valid.dates) > 0) {
    sprintf("%sto%s", format(min(valid.dates), "%Y%m%d"), format(max(valid.dates), "%Y%m%d"))
  } else {
    "nodata"
  }

  ## snake_case (per Josh, 2026-09-22): controls only these OUTPUT data
  ## frames' own column names, applied as the very last step before the
  ## xlsx write and the return/auto-assign below - mirrors the pattern
  ## already used in batz.generate_plotframe.bat(). FALSE (default) keeps
  ## this function's normal dot-separated names exactly as always.
  if (snake_case) {
    names(data.merged) <- standardize.headers(names(data.merged))
    names(log.file.df) <- standardize.headers(names(log.file.df))
  }

  result <- list(data = data.merged, log.file = log.file.df)

  if (save.xlsx) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      ## <project.name>_merge_vetted.acoustics_<daterange>_<timestamp>.xlsx -
      ## round twenty-two, 2026-09-24, per Josh's package-wide file-naming
      ## request - see @param save.xlsx and @details, "File naming (round
      ## twenty-two)". Previously "<project.name>_<Date>_merge_vetted.acoustics.xlsx".
      out.name <- sprintf("%s_merge_vetted.acoustics_%s_%s.xlsx",
                           project.name, daterange.token,
                           format(Sys.time(), "%Y%m%d%H%M%S"))
      out.path <- file.path(dir.load, out.name)
      openxlsx::write.xlsx(list(data = result$data, log.file = result$log.file),
                            file = out.path)
    } else {
      warning("Package 'openxlsx' is not installed - skipping xlsx save. ",
              "Install it with install.packages('openxlsx') to enable saving.")
    }
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())
  invisible(result)
}

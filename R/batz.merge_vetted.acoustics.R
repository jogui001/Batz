#' Merge vetted bat-acoustic-call files (k-Pro/Kaleidoscope or SonoBat) into one master table
#'
#' Searches a directory (and its subdirectories, if \code{dir.sub = TRUE})
#' for vetted bat-acoustic-call files exported from k-Pro/Kaleidoscope or
#' SonoBat vetting software, merges every file that has the expected columns
#' into one master data frame, standardizes/reorders its headers, adds a
#' combined call date-time column, recodes species identifiers to a single
#' format, and optionally fills blank manual IDs from the matching auto ID
#' column. Noise/NoID rows are kept; filter them downstream if needed.
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
#' punctuation that used to be deleted and are now preserved as underscores -
#' \code{"speciesmanualid"} -> \code{"species_manual_id"} (real header:
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
#' }
#' \code{$manid.kp}/\code{$manid.sb} (when created) and \code{$call.datetime}
#' are appended at the very end of the column order (not specified where
#' they should go).
#'
#' \strong{trim.noise / trim.noid removed (2026-10-07, per Josh).} The
#' former \code{trim.noise}/\code{trim.noid} arguments were removed. When
#' \code{$manid} was entirely blank (read by \code{read.csv()} as logical
#' \code{NA}), their \code{df[NA, ]} row filter turned every row into an
#' all-\code{NA} row. No rows are dropped by this function anymore;
#' noise/NoID rows pass through unchanged.
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
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's
#' reference-workbook "Change.to" column.} Three of this function's own
#' column identifiers are renamed to match Josh's tracked header-name
#' workbook: \code{$lon} (an output-only column, produced by splitting
#' \code{$lat}'s combined \code{"<lat> <lon>"} string - see "Once every
#' file is merged..." above) is now \code{$longitude}; \code{$serial} (both
#' an input-matching identifier and this function's own output column - see
#' the 2026-08-30 follow-up above) is now \code{$aru.serial}; and the
#' required raw-input identifier previously named \code{"monitoringnight"}
#' in \code{expected.headers} (the standardized form of the real vetting-
#' software header \code{"MonitoringNight"}) is now
#' \code{"date.monitoringnight"} in \code{expected.headers} itself.
#' \strong{Backward-compatible input aliasing:} because a real vetted-file
#' export still literally has a header that standardizes to
#' \code{"monitoringnight"} (never to \code{"date.monitoringnight"}) and
#' \code{"serial"} (never to \code{"aru.serial"}), both OLD spellings are
#' still accepted as raw-input aliases immediately after
#' \code{standardize.headers()} runs, and (for \code{monitoringnight})
#' renamed in-place to the new identifier before header validation/
#' matching proceeds - mirroring this project's existing
#' \code{standardize.headers()}/canonicalization convention elsewhere
#' (e.g. the \code{long}/\code{X}/\code{Y} location aliasing in
#' \code{\link{batz.merge_vetted.acoustics2}}). \code{$lon}/\code{$longitude}
#' has no such alias concern, since it is never read from a raw file header
#' at all - it is entirely computed internally by splitting \code{$lat}.
#'
#' \strong{Follow-up, 2026-09-30, per Josh - special characters.} New
#' argument \code{strip.special} (default \code{TRUE}). Every vetted file is
#' now read with the shared \code{special.read.csv()} helper, which falls back
#' to Latin-1 when a file isn't valid UTF-8 (so a stray Excel degree sign no
#' longer makes the file fail as "could not read file"); this always applies.
#' Immediately after each file is loaded (before header standardization,
#' header checks, trimming and merging), \code{special.strip()} removes
#' non-ASCII characters from its text columns when \code{strip.special =
#' TRUE}. \code{vetted.merged_log.file} (which, as before, only lists
#' SKIPPED files) gains a last column \code{$strip.special} giving, per
#' skipped file, the (standardized) headers where characters were removed;
#' a file that could not be read at all gets \code{"FALSE"}/\code{"TRUE
#' NONE"}. The column keeps the exact name \code{strip.special} even when
#' \code{snake_case = TRUE}. Merged files that had characters stripped are
#' not listed, since the log has no rows for successfully merged files.
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
#'   \code{$filepath}, \code{$reason}, \code{$headers.missing}, and - last -
#'   \code{$strip.special}: \code{"FALSE"} (not selected), \code{"TRUE NONE"}
#'   (nothing found), or \code{"TRUE ; <header>; ..."} (the headers where
#'   characters were removed)). If no file was skipped, no log is created
#'   (and any \code{vetted.merged_log.file} left from an earlier call is
#'   removed); instead \code{"All <n> files successfully merged, no log
#'   file created"} is printed. The log is kept in R only - it is not
#'   written to disk.
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
#'   \strong{2026-10-30, per Josh:} snake_case now replaces every \code{"."}
#'   with \code{"_"} and changes nothing else (\code{snake.headers()}), so
#'   \code{"aru.name"} -> \code{"aru_name"} and \code{"GlobalID"} stays
#'   \code{"GlobalID"} - matching \code{$name.snake} in the Batz reference db.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE}, simplifies accented
#'   letters (\code{é} -> \code{e}) and removes other non-ASCII special
#'   characters (e.g. \code{°}, \code{µ}) from the data after loading - see \code{@details}. Files are
#'   always read with a UTF-8/Latin-1 fallback, so a stray \code{°} can't stop
#'   them loading, whatever this is set to.
#' @param regions Character vector, default \code{c("ALL", "NABAT")}. Added
#'   2026-10-23. The region(s) the bats were recorded in. When
#'   \code{$manid}/\code{$autoid.kp}/\code{$autoid.sb} (and any extra auto-ID
#'   column) are recoded, only the records of the \code{BAT Groupings},
#'   \code{NABATFrequency Classes} and \code{bat.names} tables whose
#'   \code{$Region} matches one of these are used; records marked \code{All}
#'   are always used. A \code{$Region} cell may list several regions separated
#'   by \code{;} or \code{,} (e.g. \code{NABAT; CA, SouthAmerica}) - the record
#'   is used if any of them matches. Case and quotes are ignored.
#' @param autoid.header_in Character vector, default \code{c("autoid")}. Added
#'   2026-10-23. Other names for an auto-ID column, for vetting sheets not made
#'   with the standard BRI pipeline. Compared after header standardization
#'   (case, spaces and punctuation ignored), as a whole header - e.g.
#'   \code{"AUTO ID"} matches a header \code{AUTO ID} or \code{auto_id}, but
#'   not \code{WA|Kaleidoscope|Auto ID}. Only used when the file doesn't
#'   already have the standard column.
#' @param autoid.header_out Character vector, default \code{c("auto.kp")}, the
#'   same length as \code{autoid.header_in} (otherwise the function stops).
#'   Which program each \code{autoid.header_in} header comes from:
#'   \code{"auto.kp"} (k-Pro/Kaleidoscope, merged into \code{$autoid.kp}) or
#'   \code{"auto.sb"} (SonoBat, merged into \code{$autoid.sb}). Any other name
#'   is treated as a different vetting program and added as its own output
#'   column with exactly that name (after \code{$autoid.sb}); files without it
#'   get \code{NA}. Its values are recoded like the other ID columns.
#'
#' @return Invisibly, a named list: \code{vetted.merged} (always), and
#'   \code{vetted.merged_log.file} (only if \code{log.file = TRUE} and at
#'   least one file was skipped) - or
#'   their snake_case equivalents if \code{snake_case = TRUE} (see that
#'   parameter above). As a side effect, the same object(s) are also
#'   assigned directly into the calling environment (same auto-assign
#'   convention as \code{batz.merge_aru.meta}/
#'   \code{batz.datawrangler_load.files}), so a bare call with no
#'   assignment populates \code{vetted.merged} (and
#'   \code{vetted.merged_log.file}) directly. \code{vetted.merged} has a
#'   \code{$aru.model} column (\code{"SM4"} or \code{"SM5"}) right after
#'   \code{$aru.name}, and the log has an \code{$aru.model} column too.
#'
#' @details
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
#' \strong{Follow-up, 2026-10-02, per Josh - SM5 file names and
#' monitoring-night aliases.} \code{$aru.name}, \code{$date} and
#' \code{$time} are read from \code{$filename} in either layout:
#' SM4 \verb{<ARU>_<YYYYMMDD>_<HHMMSS>_<anything>} or SM5
#' \verb{<ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_<anything>}. The layout found is
#' stored in the new \code{$aru.model} column (\code{"SM4"}/\code{"SM5"};
#' \code{NA} if the name fits neither). The MIC part of an SM5 name is
#' not kept as a column. The log's \code{$aru.model} lists the model(s)
#' found in that file (\code{"SM4"}, \code{"SM5"} or \code{"SM4; SM5"}).
#' The required \code{date.monitoringnight} column is also accepted as
#' \code{MONITORINGNIGHT} or \code{DATE-12} (any case); it is renamed to
#' \code{$date.monitoringnight}.
#'
#' \strong{Follow-up, 2026-10-23, per Josh - log message and quieter
#' recode warnings.} (1) With \code{log.file = TRUE}, if every file merged
#' (nothing skipped) no empty log is created; the function prints
#' \code{"All <n> files successfully merged, no log file created"}. (2)
#' Blank values, \code{"NOISE"} and \code{"NoID"} in \code{$manid},
#' \code{$autoid.kp} and \code{$autoid.sb} are no longer sent to
#' \code{\link{batz.batusa_recode.names}} - they are not species, so they
#' are kept exactly as they are without a \code{"WARNING: ... inputs did
#' not match"} message. Any other unmatched value (e.g. SonoBat's
#' \code{"Luso"}) is still reported.
#'
#' \strong{Follow-up, 2026-10-23 (later), per Josh - shared bat tables,
#' regions, alternative headers.}
#' \enumerate{
#'   \item Bat names are recoded with the shared tables in
#'     \code{R/batz.util_bat.reference.R}: \code{BAT Groupings} and
#'     \code{NABATFrequency Classes} are checked first, then \code{bat.names}
#'     (see \code{\link{batz.batusa_recode.names}}), using only records that
#'     match \code{regions}. E.g. SonoBat's \code{Luso} is now recoded to
#'     \code{LUSO} (little brown bat / Indiana bat) instead of being reported
#'     as unmatched.
#'   \item New inputs \code{autoid.header_in}/\code{autoid.header_out} - see
#'     their descriptions above.
#'   \item A header made only of the words Manual, Species, ID and
#'     Identification, ending in ID or Identification and containing Manual or
#'     Species (e.g. \code{Manual ID}, \code{Species ID}, \code{Species
#'     Identification}) is read as \code{Species Manual ID}, if the file has no
#'     \code{Species Manual ID} column. The first one found is used.
#'   \item \strong{Behavior change:} a file no longer needs BOTH the k-Pro
#'     (\code{WA|Kaleidoscope|Auto ID}) and SonoBat (\code{SppAccp}) columns -
#'     at least one auto-ID column (k-Pro, SonoBat, or one named in
#'     \code{autoid.header_out}) is enough; the missing one is filled with
#'     \code{NA}. Without this, a non-BRI sheet with a single auto-ID column
#'     would always be skipped. \code{Filename}, \code{MonitoringNight},
#'     \code{Species Manual ID} and \code{Lat} are still required.
#'   \item The column rename after merging is now done by name instead of by
#'     position (same output for standard files).
#'   \item \code{$manid.kp}/\code{$manid.sb} are unchanged; no
#'     \code{$manid.<other>} column is made for an extra auto-ID column.
#' }
#'
#' \strong{Follow-up, 2026-10-23 (later still), per Josh - other export
#' layouts (e.g. a Kaleidoscope export of the Amboseli survey).}
#' \enumerate{
#'   \item \code{IN FILE} (or \code{File Name}, \code{File}) is read as
#'     \code{Filename} when the file has no \code{Filename} column.
#'   \item \code{Latitude} is read as \code{Lat}. A separate \code{Longitude}
#'     (or \code{Long}, \code{Lon}) column is joined onto \code{Lat} as
#'     \verb{<lat> <lon>}, then split into \code{$lat}/\code{$longitude} as for
#'     the standard files. A row with either value blank keeps only what it
#'     has.
#'   \item \code{aru.serial} is accepted as well as \code{Serial} for
#'     \code{$aru.serial}.
#'   \item If a file has an \code{aru.name} or \code{site} column AND a serial
#'     column, and the first part of a row's wav name is that row's serial
#'     number (e.g. \code{656346_20230506_180008_000.wav}, serial
#'     \code{656346}), \code{$aru.name} comes from the \code{aru.name}/
#'     \code{site} column instead of the wav name. Otherwise it is read from
#'     the wav name as before.
#'   \item Dates are not converted: \code{MonitoringNight}/\code{DATE-12} is
#'     \verb{m/d/yyyy} in both the BRI files and the Amboseli export, so
#'     \code{$date.monitoringnight} is kept as it is.
#'   \item \code{$aru.model} is still worked out from the wav-name layout, so
#'     a non-Wildlife Acoustics recorder with the same naming (e.g. the
#'     Amboseli \code{Chorus} units) shows as \code{"SM4"}.
#' }
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
                                               snake_case = FALSE,
                                               strip.special = TRUE,
                                               regions = c("ALL", "NABAT"),
                                               autoid.header_in = c("autoid"),
                                               autoid.header_out = c("auto.kp")) {

  ## 2026-10-23, per Josh: alternative auto-ID headers (non-BRI vetting
  ## sheets). autoid.header_in[i] is another name for autoid.header_out[i].
  if (length(autoid.header_in) != length(autoid.header_out)) {
    stop(paste0("the inputs \"autoid.header_in\" and \"autoid.header_out\" are of unequal lengths ",
                "so they are mis-matched, make both have the same vector length:\n",
                "autoid.header_in has a length of ", length(autoid.header_in), "\n",
                "autoid.header_out has a length of ", length(autoid.header_out)), call. = FALSE)
  }
  autoid.header_out <- trimws(as.character(autoid.header_out))
  ## "auto.kp"/"auto.sb" (or "autoid.kp"/"autoid.sb") are the standard k-Pro/
  ## SonoBat columns; anything else is another vetting program and becomes
  ## its own output column, named exactly as given.
  autoid.target <- ifelse(tolower(autoid.header_out) %in% c("auto.kp", "autoid.kp"), "wa_kaleidoscope_auto_id",
                   ifelse(tolower(autoid.header_out) %in% c("auto.sb", "autoid.sb"), "sppaccp",
                          autoid.header_out))
  autoid.in.std <- standardize.headers(autoid.header_in)
  extra.autoid <- unique(autoid.target[!(autoid.target %in% c("wa_kaleidoscope_auto_id", "sppaccp"))])

  ## 2026-10-23: a header made only of the words Manual / Species / ID /
  ## Identification, ending in ID or Identification and containing Manual or
  ## Species (e.g. "Manual ID", "Species ID", "Manual Species Identification")
  ## is read as "Species Manual ID".
  is.manual.id <- function(h) {
    vapply(strsplit(h, "_", fixed = TRUE), function(tok) {
      length(tok) >= 2 && all(tok %in% c("manual", "species", "id", "identification")) &&
        tok[length(tok)] %in% c("id", "identification") && any(tok %in% c("manual", "species"))
    }, logical(1))
  }

  ## header standardization (per Josh, 2026-09-14 project preference):
  ## expected.headers is a literal, uninvented copy of the vetting
  ## software's own real column text, so it is standardized right along
  ## with incoming raw headers via the shared standardize.headers()
  ## helper - see @details "Header standardization" above.
  expected.headers <- c("filename", "date.monitoringnight", "species_manual_id",
                         "wa_kaleidoscope_auto_id", "sppaccp", "lat")

  regex.pattern <- paste(utils::glob2rx(load.pattern), collapse = "|")
  files <- list.files(dir.load, pattern = regex.pattern, recursive = dir.sub,
                       full.names = TRUE, ignore.case = TRUE)

  vetted.merged <- data.frame()
  log.rows <- list()

  ## 2026-10-02, per Josh: SM4 vs SM5 recordings have different file names -
  ##   SM4: <ARU>_<YYYYMMDD>_<HHMMSS>_<junk>
  ##   SM5: <ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_<junk>
  ## parse.aru.filename() works out which, row by row, and pulls out the ARU
  ## name, date and time. The part after HHMMSS may also be just the file
  ## extension (e.g. ".wav") or nothing.
  parse.aru.filename <- function(fn) {
    fn <- as.character(fn)
    rx5 <- "^([^_]+)_([^_]+)_(\\d{8})_(\\d{6})(_|\\.|$)"
    rx4 <- "^([^_]+)_(\\d{8})_(\\d{6})(_|\\.|$)"
    m4 <- regmatches(fn, regexec(rx4, fn))
    m5 <- regmatches(fn, regexec(rx5, fn))
    out <- data.frame(aru.name = rep(NA_character_, length(fn)), aru.model = NA_character_,
                      date = NA_character_, time = NA_character_, stringsAsFactors = FALSE)
    for (i in seq_along(fn)) {
      if (length(m4[[i]]) >= 4) {
        out[i, ] <- c(m4[[i]][2], "SM4", m4[[i]][3], m4[[i]][4])
      } else if (length(m5[[i]]) >= 5) {
        out[i, ] <- c(m5[[i]][2], "SM5", m5[[i]][4], m5[[i]][5])
      }
    }
    out
  }
  ## one log value per file: "SM4", "SM5", "SM4; SM5" (mixed) or NA
  file.aru.model <- function(df) {
    if (is.null(df) || !("filename" %in% names(df)) || nrow(df) == 0) return(NA_character_)
    mods <- sort(unique(stats::na.omit(parse.aru.filename(df$filename)$aru.model)))
    if (length(mods) == 0) NA_character_ else paste(mods, collapse = "; ")
  }

  add.log <- function(filepath, reason, headers.missing, special.headers = character(0)) {
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filepath = filepath, reason = reason, headers.missing = headers.missing,
      aru.model = cur.model,
      special.log.cols(strip.special, special.headers, cur.counts),
      stringsAsFactors = FALSE
    )
  }

  cur.counts <- NULL   # 2026-10-02: simplified/removed counts for the current file's log row
  cur.model <- NA_character_
  for (f in files) {
    cur.counts <- NULL
    cur.model <- NA_character_
    tmp <- tryCatch(special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { add.log(f, "could not read file", "none"); next }
    ## strip special characters right after loading (2026-09-30)
    s.strip <- special.strip(tmp, strip.special)
    tmp <- s.strip$df
    special.hdrs <- standardize.headers(s.strip$headers)
    cur.counts <- s.strip$counts
    special.hdrs[special.hdrs == "monitoringnight"] <- "date.monitoringnight"
    names(tmp) <- standardize.headers(names(tmp))
    cur.model <- file.aru.model(tmp)
    ## Column identifiers renamed 2026-09-27 (see @details, "Column
    ## identifiers renamed" above): this function's own required raw-column
    ## identifier is now "date.monitoringnight", but the real vetting-
    ## software header ("MonitoringNight") still standardizes to the OLD
    ## spelling "monitoringnight" - accepted here as a legacy input alias so
    ## real incoming files keep matching.
    ## 2026-10-02, per Josh: also accept "DATE-12" (Kaleidoscope's
    ## noon-shifted date) and a literal "date.monitoringnight" (which
    ## standardizes to "date_monitoringnight") - first one found is used.
    if (!("date.monitoringnight" %in% names(tmp))) {
      mn.alias <- intersect(c("date_monitoringnight", "monitoringnight", "date_12"), names(tmp))
      if (length(mn.alias) > 0) names(tmp)[names(tmp) == mn.alias[1]] <- "date.monitoringnight"
    }
    ## 2026-10-23 (later), per Josh - other export layouts (e.g. a
    ## Kaleidoscope export with "IN FILE", separate LATITUDE/LONGITUDE):
    ## "IN FILE"/"File Name"/"File" is read as Filename, "Latitude" as Lat,
    ## and a separate Longitude/Long/Lon column is joined onto Lat as
    ## "<lat> <lon>" (the format the standard files use, split again below).
    if (!("filename" %in% names(tmp))) {
      fa <- intersect(c("in_file", "file_name", "file"), names(tmp))
      if (length(fa) > 0) names(tmp)[names(tmp) == fa[1]] <- "filename"
    }
    if (!("lat" %in% names(tmp)) && "latitude" %in% names(tmp)) {
      names(tmp)[names(tmp) == "latitude"] <- "lat"
    }
    lon.col <- intersect(c("longitude", "long", "lon"), names(tmp))
    if ("lat" %in% names(tmp) && length(lon.col) > 0) {
      la <- trimws(ifelse(is.na(tmp$lat), "", as.character(tmp$lat)))
      lo <- trimws(ifelse(is.na(tmp[[lon.col[1]]]), "", as.character(tmp[[lon.col[1]]])))
      join <- nzchar(la) & nzchar(lo) & !grepl("\\s", la)
      la[join] <- paste(la[join], lo[join])
      tmp$lat <- la
    }
    ## "Species Manual ID" alternatives (2026-10-23) - first one found is used
    if (!("species_manual_id" %in% names(tmp))) {
      mid <- names(tmp)[is.manual.id(names(tmp))]
      if (length(mid) > 0) names(tmp)[names(tmp) == mid[1]] <- "species_manual_id"
    }
    ## alternative auto-ID headers (2026-10-23) - only used when the file
    ## doesn't already have the standard column
    for (i in seq_along(autoid.in.std)) {
      if (autoid.in.std[i] %in% names(tmp) && !(autoid.target[i] %in% names(tmp))) {
        names(tmp)[names(tmp) == autoid.in.std[i]] <- autoid.target[i]
      }
    }
    ## 2026-10-23: the k-Pro (wa_kaleidoscope_auto_id) and SonoBat (sppaccp)
    ## columns are no longer both required - a file needs at least one auto-ID
    ## column (k-Pro, SonoBat or one named in autoid.header_out); a missing one
    ## is filled with NA.
    auto.headers <- c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)
    missing.headers <- setdiff(setdiff(expected.headers, auto.headers[1:2]), names(tmp))
    if (!any(auto.headers %in% names(tmp))) {
      missing.headers <- c(missing.headers, paste(auto.headers, collapse = " or "))
    }
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", "), special.hdrs); next
    }
    if (nrow(tmp) == 0) { add.log(f, "no records", "none", special.hdrs); next }
    ## $aru.serial is OPTIONAL (2026-08-30 follow-up), not one of the required
    ## expected.headers - a file is never skipped for lacking it (e.g. a
    ## Mobile-transect export, which has no fixed instrument serial number at
    ## all). Captured BEFORE trimming to expected.headers below, exactly like
    ## $sunregion, since that trim would otherwise silently drop it. The real
    ## vetting-software header ("Serial") standardizes to the OLD spelling
    ## "serial" - kept here as a legacy input alias (renamed 2026-09-27, see
    ## @details, "Column identifiers renamed" above) while this function's
    ## own internal/output column is now $aru.serial.
    ## 2026-10-23 (later): "aru.serial" (standardized aru_serial) accepted too
    ser.col <- intersect(c("aru_serial", "serial"), names(tmp))
    aru.serial.vals <- if (length(ser.col) > 0) as.character(tmp[[ser.col[1]]]) else
      rep(NA_character_, nrow(tmp))
    ## 2026-10-23 (later), per Josh: if the file has an aru.name or site column
    ## AND a serial column, and the first part of the wav name is that row's
    ## serial number (e.g. "656346_20230506_180008_000.wav", serial 656346),
    ## the ARU name is taken from the aru.name/site column instead of the wav
    ## name. Otherwise it is read from the wav name as before.
    aru.name.vals <- rep(NA_character_, nrow(tmp))
    name.col <- intersect(c("aru_name", "site"), names(tmp))
    if (length(name.col) > 0 && length(ser.col) > 0) {
      fn.aru <- parse.aru.filename(tmp$filename)$aru.name
      nm.vals <- trimws(as.character(tmp[[name.col[1]]]))
      use <- !is.na(fn.aru) & !is.na(aru.serial.vals) & trimws(fn.aru) == trimws(aru.serial.vals) &
        !is.na(nm.vals) & nzchar(nm.vals)
      aru.name.vals[use] <- nm.vals[use]
    }
    ## $sunregion is OPTIONAL, not one of the required expected.headers - a
    ## file is never skipped for lacking it. If a file's own (standardized)
    ## headers happen to include it, copy those values straight through;
    ## otherwise this file's rows get NA for $sunregion (still needs to be
    ## joined in separately, exactly as before, for files that don't already
    ## carry it). Captured BEFORE trimming to expected.headers below, since
    ## that trim would otherwise silently drop it.
    sunregion.vals <- if ("sunregion" %in% names(tmp)) as.character(tmp$sunregion) else
      rep(NA_character_, nrow(tmp))
    for (h in c(auto.headers)) if (!(h %in% names(tmp))) tmp[[h]] <- NA_character_
    tmp <- tmp[, c(expected.headers, extra.autoid), drop = FALSE]
    tmp[c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)] <-
      lapply(tmp[c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)], as.character)
    tmp$aru.serial <- aru.serial.vals
    tmp$sunregion <- sunregion.vals
    tmp$aru.name.file <- aru.name.vals
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

    ## $filename -> $aru.name/$date/$time, SM4 or SM5 layout (2026-10-02)
    fn.parts <- parse.aru.filename(vetted.merged$filename)
    vetted.merged$aru.name <- ifelse(is.na(vetted.merged$aru.name.file), fn.parts$aru.name,
                                     vetted.merged$aru.name.file)
    vetted.merged$date     <- fn.parts$date
    vetted.merged$time     <- fn.parts$time
    aru.model.vals         <- fn.parts$aru.model

    ## --- rename, reorder, call.datetime, recode.names, manid.kp/sb
    ## fill-in - see @details above -----------------

    ## positional rename ($sunregion, appended right after $aru.serial back
    ## in the per-file loop above, keeps its own name here - no rename
    ## needed)
    ## 2026-10-23: renamed by name instead of position, so extra auto-ID
    ## columns (autoid.header_out) can sit in between
    rename.map <- c(species_manual_id = "manid", wa_kaleidoscope_auto_id = "autoid.kp",
                    sppaccp = "autoid.sb", lon = "longitude")
    hit <- names(vetted.merged) %in% names(rename.map)
    names(vetted.merged)[hit] <- rename.map[names(vetted.merged)[hit]]

    ## reorder ($sunregion placed with the other detector-level columns,
    ## next to $aru.serial/$aru.name)
    vetted.merged$aru.model <- aru.model.vals
    vetted.merged <- vetted.merged[, c("filename", "date.monitoringnight", "aru.name", "aru.model",
                                        "aru.serial", "sunregion", "lat", "longitude",
                                        "manid", "autoid.kp", "autoid.sb", extra.autoid,
                                        "date", "time")]

    ## $call.datetime
    vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = vetted.merged$date, time = vetted.merged$time)

    is.empty <- function(x) is.na(x) | !nzchar(trimws(x))

    ## recode manid/autoid.kp/autoid.sb (batname.format.out = bat.names.out, default "code4")
    ## 2026-10-23, per Josh: blanks, "NOISE" and "NoID" are not species, so
    ## they are left as they are and not sent to batz.batusa_recode.names()
    ## (which listed them as "inputs did not match"). Any other value that
    ## doesn't match is still reported.
    recode.skip <- c("noise", "noid")
    recode.ids <- function(x) {
      x <- as.character(x)
      send <- !is.empty(x) & !(tolower(trimws(x)) %in% recode.skip)
      if (any(send)) x[send] <- batz.batusa_recode.names(x[send], batname.format.out = bat.names.out,
                                                         regions = regions)
      x
    }
    vetted.merged$manid     <- recode.ids(vetted.merged$manid)
    vetted.merged$autoid.kp <- recode.ids(vetted.merged$autoid.kp)
    vetted.merged$autoid.sb <- recode.ids(vetted.merged$autoid.sb)
    for (h in extra.autoid) vetted.merged[[h]] <- recode.ids(vetted.merged[[h]])

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
  }

  rownames(vetted.merged) <- NULL

  ## snake_case output option (per Josh's request to audit and extend this
  ## package-wide, 2026-09-22) - applied last, only to this function's own
  ## invented output column names, mirroring batz.generate_plotframe.bat()'s
  ## established pattern. See @param snake_case.
  if (snake_case) names(vetted.merged) <- snake.headers(names(vetted.merged))

  result <- list(vetted.merged = vetted.merged)

  ## 2026-10-23, per Josh: when no file was skipped there is nothing to log,
  ## so no (empty) log is created - a one-line message is printed instead. A
  ## log left over from an earlier call is removed so it isn't mistaken for
  ## this run's.
  if (log.file && length(log.rows) == 0) {
    if (length(files) == 0) {
      cat("No files matching load.pattern found, no log file created\n")
    } else {
      cat(sprintf("All %d files successfully merged, no log file created\n", length(files)))
    }
    if (exists("vetted.merged_log.file", envir = parent.frame(), inherits = FALSE)) {
      rm("vetted.merged_log.file", envir = parent.frame())
    }
  } else if (log.file) {
    vetted.merged_log.file <- do.call(rbind, log.rows)
    rownames(vetted.merged_log.file) <- NULL
    if (snake_case) {
      names(vetted.merged_log.file) <- snake.headers(names(vetted.merged_log.file))
      ## keep the log column's exact name (2026-09-30)
      special.cols <- c("strip.special", "Accented.letters.header", "removed.symbols.header",
                        "Accented.letters.data", "removed.symbols.data")
      hit <- match(names(vetted.merged_log.file), snake.headers(special.cols))
      names(vetted.merged_log.file)[!is.na(hit)] <- special.cols[hit[!is.na(hit)]]
    }
    result$vetted.merged_log.file <- vetted.merged_log.file
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())
  invisible(result)
}

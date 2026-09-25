#' Recode US bat species identifiers between common name, latin name, species codes, and status fields
#'
#' Given a vector or data frame containing any mix of common name, latin
#' (scientific) name, 4-letter species code, or 6-letter species code for
#' North American bat species, looks each value up in an internal reference
#' table and returns it re-expressed in a single chosen format
#' (\code{batname.format.out}). Matching ignores case, underscores, dashes, and
#' leading/trailing/extra whitespace, so formatting differences between the
#' input and the reference table (or between different inputs) don't cause a
#' false mismatch.
#'
#' @param data A vector, or a data frame, of bat species identifiers to
#'   recode. If a data frame is supplied, every column is recoded the same
#'   way (there is no column-selection argument) and comes back as a data
#'   frame of the same dimensions, with columns returned as character
#'   vectors.
#' @param batname.format.out Character, default \code{"common_name"}. The
#'   desired output format - must be one of the reference table's own
#'   headers: \code{"scientific_name"}, \code{"common_name"}, \code{"code4"},
#'   \code{"code6"}, \code{"listing.status_federal"},
#'   \code{"listing.status_iucn"}, \code{"states.listed"},
#'   \code{"states.present"}, \code{"states.endangered"},
#'   \code{"states.threatened"}, \code{"state.sppofconcern"},
#'   \code{"listing.status_federal.proposed"}, \code{"hibernation.strategy"},
#'   \code{"phonic.group"}, or \code{"notes"}. Matching an input element
#'   to a reference row always uses \code{scientific_name}/\code{common_name}/
#'   \code{code4}/\code{code6} only, regardless of \code{batname.format.out} -
#'   so, for example, \code{batname.format.out = "listing.status_federal"}
#'   looks a species up by any of its four names/codes and returns its
#'   federal listing status instead of another name/code. An unrecognized
#'   value is an error.
#'
#'   \code{"hibernation.strategy"} is one of \code{"migratory"} (tree bats that
#'   head south for winter), \code{"hibernating"} (cave bats that go into
#'   torpor over winter), \code{"resident"} (active year-round, no
#'   hibernation), \code{"mixed"} (species with both migratory and
#'   non-migratory populations), or \code{"unknown"}. \code{"phonic.group"}
#'   is one of \code{"Lof"} (echolocation calls below 35 kHz), \code{"Hif"}
#'   (above 35 kHz), \code{"None"} (does not echolocate), or
#'   \code{"Unknown"}. Both are populated from general bat natural-history/
#'   acoustics literature, not a source file Josh supplied for these two
#'   columns specifically - see \code{NAbat.names.csv}'s own \code{$notes}
#'   column (also selectable via \code{batname.format.out = "notes"}) for the
#'   species where a call is genuinely split or lower-confidence.
#'
#'   Eight non-species detection/category labels are also recognized as
#'   ordinary rows in the same reference table (added 2026-08-27, per
#'   Josh): \code{"All detections"}, \code{"40KHzMyo"}, \code{"HiF"},
#'   \code{"LoF"}, \code{"HiFrag"}, \code{"LoFrag"}, \code{"Multiple"},
#'   \code{"Social"}. They match the same case-insensitive way as species
#'   names (e.g. \code{"hif"}, \code{"HIF"}, \code{"Hif"} all match), and
#'   \code{batname.format.out} values of \code{"scientific_name"}/
#'   \code{"common_name"}/\code{"code4"}/\code{"code6"} all return the
#'   exact literal casing shown above. Every other \code{batname.format.out}
#'   (\code{"listing.status_federal"}, \code{"states.present"},
#'   \code{"phonic.group"}, etc.) returns \code{""} for these eight, since
#'   those columns don't apply to a non-species label.
#' @param grammar.dash Logical, default \code{TRUE}. Hyphens are ignored
#'   (treated the same as a space) when MATCHING an input value regardless of
#'   this flag. This flag only controls the OUTPUT: \code{TRUE} (default)
#'   returns matched values exactly as written in the reference table
#'   (hyphens kept, e.g. \code{"Silver-haired bat"}); \code{FALSE} replaces
#'   every hyphen in a matched value with a space instead (e.g.
#'   \code{"Silver haired bat"}).
#'
#' @return A vector (if \code{data} is a vector) or data frame (if
#'   \code{data} is a data frame) of the same length/dimensions as
#'   \code{data}, with every element re-expressed in \code{batname.format.out}.
#'   An input element with no match anywhere in the reference table is
#'   returned unchanged (not \code{NA}, no error).
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference) - does not apply to this function, flagged not silently skipped.} The project-wide preference is that headers coming from a loaded file or an externally-supplied data frame are run through the shared package helper \code{standardize.headers()} (trim whitespace, collapse non-alphanumeric runs to underscores, lowercase). This function has no raw-header step for that preference to attach to: every element of \code{data} is recoded as a VALUE (a species identifier - a name or code), never as a header, and when \code{data} is a data frame this function never reads, matches on, or otherwise interprets its column names at all - \code{names(out) <- names(data)} just carries them through unchanged as a pure pass-through label. The embedded \code{nabat.names} reference table's own column names (\code{$scientific_name}/\code{$common_name}/\code{$code4}/\code{$code6}/\code{$listing.status_federal}/etc.) are this function's fixed, already-established internal schema - not raw headers copied fresh from a file for this rollout - so they are not run through \code{standardize.headers()} either, the same treatment already given to every other \code{batz} function's own output-schema column names.
#'
#' (Renamed 2026-08-29, per Josh, from \code{output.format} to
#' \code{batname.format.out}, to standardize \code{format}-suffixed
#' parameters as \code{.in}/\code{.out}.)
#'
#' \strong{BUGFIX (2026-09-21, per Josh's real-world error report):} the
#' embedded \code{nabat.names} reference table's \code{$fed.proposed} column
#' was missing one blank \code{""} element (61 instead of 62), which made
#' every single call to this function crash - not conditionally, since
#' \code{reference[] <- lapply(reference, ...)} runs unconditionally near
#' the top of the function body, and R's data-frame replacement validates
#' every column's length against the table's declared 62 rows the moment
#' that line runs. The error looked like
#' \code{Error in `[<-.data.frame`(...) : replacement element N has 61 rows,
#' need 62} (N depends on which copy of this file is running - both a
#' sandbox-side transcription slip and Josh's own installed copy hit this
#' same failure mode, at different column positions, before this fix).
#' Root-caused by checking every column's length against the declared
#' 62-row table and confirming exactly one column (\code{fed.proposed}) was
#' short; the missing blank was restored in the block of blanks before row
#' 39 (\code{Myotis lucifugus}), which realigns \code{fed.proposed}'s two
#' real values (\code{"Under Review, start year unconfirmed"} at row 39,
#' \code{"Proposed Endangered, 2022"} at row 52) with the matching
#' \code{$fedstatus} rows (\code{"Under Review"} at row 39 - \emph{Myotis
#' lucifugus}, the little brown bat, under status review for white-nose
#' syndrome; \code{"Proposed Endangered"} at row 52 - \emph{Perimyotis
#' subflavus}, the tri-colored bat, proposed for ESA listing in 2022).
#' Verified after the fix: every one of the table's 15 columns is exactly
#' 62 elements long, and a full \code{batz.merge_vetted.acoustics()} run
#' against real vetted-acoustics test data (6,777 rows) completes with no
#' error. \strong{Action needed on Josh's machine:} replace
#' \code{R/batz.batusa_recode.names.R} in the \code{Batz} GitHub repo with
#' this corrected file, commit, push, then reinstall
#' (\code{pak::pak("jogui001/Batz")}) - this file has no exported-name or
#' argument changes, so no \code{devtools::document()}/NAMESPACE update is
#' needed, just the file replacement.
#'
#' \strong{RECURRENCE (2026-09-22, per Josh's "Error is back with the last
#' push" report):} the exact same crash resurfaced
#' (\code{Error in `[<-.data.frame`(...) : replacement element 12 has 61
#' rows, need 62}). Direct execution confirmed the 2026-09-21 fix above was
#' never actually applied to the shipped \code{$fed.proposed} column data -
#' the file's prose said the column was restored to 62 elements, but the
#' embedded vector itself was still 61 elements long (one blank \code{""}
#' still missing from the leading run before row 39), so
#' \code{"Under Review, start year unconfirmed"} was landing on row 38 and
#' \code{"Proposed Endangered, 2022"} on row 51 - both one row too early.
#' This time the missing blank was restored and, critically, verified by
#' actually running \code{sapply(nabat.names, length)} against the live
#' file content (not just re-reading the prose) before shipping: all 15
#' columns confirmed exactly 62 elements, the two real \code{fed.proposed}
#' values now land on rows 39/52 as documented above, and a synthetic
#' 3-row \code{data.frame} recode exercising the same
#' \code{$manid}/\code{$autoid.kp}/\code{$autoid.sb} call pattern used by
#' \code{batz.merge_vetted.acoustics()} completes with no error.
#' \strong{Action needed on Josh's machine (same as above):} replace
#' \code{R/batz.batusa_recode.names.R}, commit, push, reinstall. As a
#' standing safeguard against a silent future re-break of this same kind,
#' any change to the \code{nabat.names} table should be followed by running
#' \code{stopifnot(all(sapply(nabat.names, length) == nrow(nabat.names)))}
#' before shipping - this is now the required verification step, not an
#' optional one, precisely because narrative claims of "verified" in this
#' file's own history were not sufficient to prevent this recurrence.
#'
#' \strong{Known typo/variant corrections, applied BEFORE the normal lookup
#' (added 2026-09-24, per Josh).} A small \code{typo.corrections} lookup
#' (name -> canonical spelling) catches known misspellings/shorthand for
#' the non-species category labels above - currently \code{"40kMyo"} ->
#' \code{"40KHzMyo"} and \code{"2bat"} -> \code{"Multiple"} - and rewrites a
#' matching input to its canonical spelling BEFORE the ordinary
#' \code{latin}/\code{common}/\code{code4}/\code{code6} lookup runs, so it
#' then matches the reference table the normal way. This correction is
#' matched the exact same case/whitespace/dash-underscore-insensitive way
#' as everything else in this function (via the same \code{normalize()}
#' helper), so \code{"40KMYO"}, \code{"40k_myo"}, \code{" 2Bat "}, etc. are
#' all caught too - not just the two literal spellings Josh gave. If an
#' input doesn't match anything even after this correction step (e.g. a
#' future \code{typo.corrections} entry pointing at a canonical value that
#' isn't actually in the reference table), the ORIGINAL uncorrected input is
#' what's returned unchanged, consistent with every other unmatched value.
#' To add another known typo/variant, add one more
#' \code{"typo" = "canonical value"} entry to \code{typo.corrections} below.
#'
#' If one or more input elements don't match anything in the reference
#' table, a warning is printed (not raised via \code{warning()} - a plain
#' \code{cat()} message, matching how similar diagnostics are reported
#' elsewhere in the \code{batz} package): \code{"WARNING: X inputs did not
#' match: ..."}, where X counts every unmatched INSTANCE (not just distinct
#' values), followed by the first 25 unique unmatched values (a note is
#' appended if more than 25 unique values were omitted from the printed
#' list).
#'
#' The reference table (54 North American bat species, as supplied in
#' Josh's \code{NAbat.names.csv}, now including \code{$hibernation.strat}/
#' \code{$phonic.group}/\code{$notes} added 2026-08-25, plus 8 non-species
#' detection/category label rows added 2026-08-27 - see \code{batname.format.out}
#' above) is embedded directly in this function - there is no
#' reference-file-path argument, since the spec's inputs are just
#' \code{data}/\code{batname.format.out}/\code{grammar.dash}. To update the
#' species list later, replace the \code{nabat.names} data frame inside
#' this function with a newer export of the same 15-column format (and
#' re-check every column's length equals \code{nrow()}, per the bugfix
#' above, before shipping it).
#'
#' \strong{Reference table export, 2026-09-25, per Josh's request
#' ("generate reference table for batz.batusa_recode.names").} A design
#' question raised in an earlier round - whether this function should be
#' rewritten to load \code{nabat.names} from \code{NAbat.names.csv} at
#' call time, like \code{\link{batz.treeusa_recode.names}} already does,
#' instead of embedding it - was put to Josh directly; he chose neither
#' rewiring option, opting instead for a plain export/documentation of the
#' table exactly as it's embedded here, with no new file dependency added
#' to this function. \code{batz.batusa_recode.names_reference.csv} is that
#' export: a 62-row x 15-column snapshot of the live \code{nabat.names}
#' table above, pulled directly from this function's own source (by
#' introspecting \code{body(batz.batusa_recode.names)} and evaluating just
#' the \code{nabat.names <-} assignment, not retyped by hand - precisely to
#' avoid the transcription slip that caused the 2026-09-21/2026-09-22
#' \code{$fed.proposed} bugs above). \strong{This CSV is a read-only
#' snapshot for review/audit purposes (diffing against
#' \code{NAbat.names.csv}, opening in Excel, sanity-checking a value) - no
#' \code{batz} function reads it, and \code{batz.batusa_recode.names()}
#' itself is completely unchanged by this: it keeps embedding its own copy
#' exactly as before.} It will silently go stale if \code{nabat.names} is
#' ever edited inside this function without also re-exporting it; there is
#' no automatic sync. To regenerate it: source this file, introspect
#' \code{body(batz.batusa_recode.names)} for the \code{nabat.names <-}
#' assignment (the same method used to build this export - safer than
#' copy/pasting the table by hand), re-run the
#' \code{stopifnot(all(sapply(nabat.names, length) == nrow(nabat.names)))}
#' check from the bugfix above, then \code{write.csv()} it.
#'
#' \strong{Data correction, 2026-09-25, per Josh (\"Western yellow bat is
#' not listed in OK\"):} the embedded table's \code{$states.the} value for
#' \emph{Lasiurus xanthinus} (\code{code4 = "laxa"}, row 25) was
#' \code{"OK"} - incorrect, per Josh's direct correction, and inconsistent
#' with the rest of that row besides (\code{$states.present} for this
#' species is \code{"AZ,CA,NM"}; Oklahoma isn't part of its documented
#' range in this table at all, and every other state-listing column for
#' this row was already blank) - almost certainly a stray value from an
#' earlier data-entry slip in \code{NAbat.names.csv}, the same class of
#' issue as the \code{$fed.proposed} row-alignment bugs above, just a
#' single cell rather than a whole column shifted. Cleared to blank.
#' Applied and verified the same way as every embedded-table change in
#' this file: extracted the live table via
#' \code{body(batz.batusa_recode.names)} introspection (never retyped by
#' hand), the single-cell change confirmed by a full column-by-column diff
#' against the table before the edit (exactly one cell differed:
#' \code{states.the[25]}, \code{"OK"} -> \code{""}), then re-verified with
#' \code{stopifnot(all(sapply(nabat.names, length) == nrow(nabat.names)))}
#' before shipping. \code{batz.batusa_recode.names_reference.csv} (see the
#' export entry just above) was regenerated from this corrected table, so
#' it and the embedded table agree again.
#'
#' \strong{Column-name rename, 2026-09-25, per Josh's explicit mapping.}
#' Nine of the embedded table's 15 column names were renamed - a real
#' interface change, since every valid \code{batname.format.out} value is
#' literally one of these column names, and the parameter's own default
#' changed with them (\code{"common"} -> \code{"common_name"}):
#' \tabular{ll}{
#'   \strong{old name} \tab \strong{new name} \cr
#'   \code{latin} \tab \code{scientific_name} \cr
#'   \code{common} \tab \code{common_name} \cr
#'   \code{fedstatus} \tab \code{listing.status_federal} \cr
#'   \code{iucnstatus} \tab \code{listing.status_iucn} \cr
#'   \code{states.end} \tab \code{states.endangered} \cr
#'   \code{states.the} \tab \code{states.threatened} \cr
#'   \code{state.soc} \tab \code{state.sppofconcern} \cr
#'   \code{fed.proposed} \tab \code{listing.status_federal.proposed} \cr
#'   \code{hibernation.strat} \tab \code{hibernation.strategy} \cr
#' }
#' \code{code4}, \code{code6}, \code{states.listed}, \code{states.present},
#' \code{phonic.group}, and \code{notes} are unchanged. Only the column
#' NAMES changed - every cell's value is untouched (verified: a full
#' column-by-column value diff, matched old name to new name, showed zero
#' differing cells, on top of the standing
#' \code{stopifnot(all(sapply(nabat.names, length) == nrow(nabat.names)))}
#' integrity check). \strong{Historical \code{@details} paragraphs above
#' this one (the BUGFIX/RECURRENCE entries, and the reference-table-export
#' and data-correction entries immediately above) describe the table under
#' its PRE-RENAME column names (e.g. \code{$fed.proposed}, \code{$states.the})
#' - left as originally written, per this project's standing
#' append-don't-rewrite-history convention, rather than retroactively
#' edited to the new names.} Anywhere this file's history mentions one of
#' the old names in the left column above, read it as referring to what is
#' now the corresponding new name on the right.
#' \code{batz.batusa_recode.names_reference.csv} was regenerated again
#' under the new column names, so the embedded table, this export, and
#' this documentation are all consistent going forward.
#'
#' \strong{Known typo/variant corrections, expanded (2026-09-25), per
#' Josh's request ("add step at the start Batz.batusa_recode.names() Use
#' the spell check tab to build internal db that corrects common
#' misspellings and variations first") and his uploaded workbook
#' (\code{batz.batusa_recode.names_reference_20260925.xlsx}, sheet "spell
#' check").} The typo-correction step itself already ran first, before the
#' ordinary lookup (added 2026-09-24 - see the entry above) - what's new
#' here is the \code{typo.corrections} table it's built from: expanded from
#' the original 2 entries to the 7 distinct pairs on that sheet (an 8th
#' row, \code{"2Bat" -> "Multiple"}, is the same correction as
#' \code{"2bat" -> "Multiple"} once case is folded by \code{normalize()},
#' so it isn't a separate table entry). Pulled directly from the
#' spreadsheet's cells (not retyped by hand), the new entries are:
#' \code{"40kmyomyvo" -> "40KHzMyomyvo"}, \code{"HighF/HiF" -> "HiF"},
#' \code{"LowF/LoF" -> "LoF"}, \code{"LowF" -> "LoF"}, and
#' \code{"HighF" -> "HiF"}.
#'
#' \strong{Two things flagged for Josh, not silently resolved:} (1) the
#' sheet's \code{"In"} values \code{"HighF/HiF"} and \code{"LowF/LoF"} are
#' read LITERALLY, as single combined labels some vetting output
#' apparently uses (not as "either half maps to the given canonical
#' spelling") - matched exactly like every other entry here, case/
#' whitespace-insensitively but with the \code{/} itself kept as-is (this
#' function's \code{normalize()} only folds \code{-}/\code{_} to a space,
#' never \code{/}), so only that exact combined string (in any
#' case/whitespace variant) is corrected; a bare \code{"HighF"} or
#' \code{"LowF"} is still separately covered by its own row. (2)
#' \code{"40kmyomyvo" -> "40KHzMyomyvo"} corrects to a spelling that isn't
#' actually one of the 8 non-species category rows in the embedded
#' reference table above (only \code{"40KHzMyo"} is) - per the existing
#' pass-through convention, an input landing on \code{"40KHzMyomyvo"}
#' simply won't match anything downstream and is returned unchanged,
#' exactly like any other unmatched value, so this is safe to ship as-is,
#' but it means the correction currently has no reference-table row to
#' land on. Josh's own \code{"Sheet3"} tab in the same workbook defines
#' \code{"40kmyomyvo"} as "Myotis volans or other species of Myotis with
#' pulses that have a minimum frequency of approximately 35-45 kHz" - a
#' real, distinct category, not a typo for one of the 8 existing rows.
#' \strong{Please confirm whether \code{"40KHzMyomyvo"} should be added as
#' a 9th non-species category row (same pattern as the existing 8, added
#' 2026-08-27)} - not added here, since that's a reference-table content
#' change beyond what was asked in this round.
#'
#' @examples
#' \dontrun{
#' batz.batusa_recode.names(c("epfu", "myotis_lucifugus", "Hoary bat"))
#' # -> "Big brown bat"    "Little brown bat"    "Hoary bat"
#'
#' batz.batusa_recode.names("epfu", batname.format.out = "scientific_name")
#' # -> "Eptesicus fuscus"
#'
#' batz.batusa_recode.names("lano", batname.format.out = "common_name", grammar.dash = FALSE)
#' # -> "Silver haired bat"   (hyphen replaced with a space)
#'
#' batz.batusa_recode.names("myse", batname.format.out = "listing.status_federal")
#' # -> "Endangered"
#'
#' batz.batusa_recode.names("tabr", batname.format.out = "hibernation.strategy")
#' # -> "mixed"   (most populations migrate to Mexico; Florida's is resident)
#'
#' batz.batusa_recode.names("mylu", batname.format.out = "phonic.group")
#' # -> "Hif"
#'
#' batz.batusa_recode.names(c("hif", "LOFRAG", "40khzmyo"))
#' # -> "HiF"      "LoFrag"   "40KHzMyo"
#'
#' batz.batusa_recode.names(c("40kMyo", "2bat"))
#' # -> "40KHzMyo"   "Multiple"   (known typo/shorthand corrections)
#'
#' batz.batusa_recode.names(c("HighF", "LowF", "HighF/HiF"))
#' # -> "HiF"   "LoF"   "HiF"   (corrections added 2026-09-25, from Josh's
#' #    "spell check" tab)
#' }
#'
#' @export
batz.batusa_recode.names <- function(data, batname.format.out = "common_name", grammar.dash = TRUE) {

  # ---------------------------------------------------------------------------
  # Reference database (Josh's real NAbat.names.csv, embedded as supplied -
  # 54 species x 15 columns, including $hibernation.strat/$phonic.group/
  # $notes added 2026-08-25, plus 8 non-species detection/category label
  # rows - All detections/40KHzMyo/HiF/LoF/HiFrag/LoFrag/Multiple/Social -
  # added 2026-08-27, per Josh). See @details above for how to update this.
  #
  # BUGFIX (2026-09-21, RECURRED and re-fixed 2026-09-22): $fed.proposed
  # was one element short (61 vs 62) - restored below. The 2026-09-21 fix
  # was documented but never actually landed in this data (still 61 as of
  # 2026-09-22); this time confirmed by directly running
  # sapply(nabat.names, length) against the live file, not just re-reading
  # the prose. See @details "BUGFIX"/"RECURRENCE" paragraphs above for the
  # full story and how the correct row alignment was confirmed.
  #
  # Header standardization (per Josh, 2026-09-14 project preference): NOT
  # applied to this table's own column names (scientific_name/common_name/
  # code4/code6/listing.status_federal/etc.) - these are this function's own fixed, already-established
  # output-schema names, not raw headers freshly copied from a loaded file for
  # this rollout. Nor is it applied anywhere to `data` itself: `data`'s
  # elements are recoded as VALUES (species identifiers), never as headers,
  # and when `data` is a data frame its column NAMES are only ever passed
  # through unchanged (`names(out) <- names(data)`), never read or matched
  # against. See @details "Header standardization" above.
  # ---------------------------------------------------------------------------
  nabat.names <- structure(list(scientific_name = c("Antrozous pallidus", "Artibeus jamaicensis", 
"Brachyphylla cavernarum", "Choeronycteris mexicana", "Corynorhinus rafinesquii", 
"Corynorhinus townsendii", "Corynorhinus townsendii ingens", 
"Corynorhinus townsendii virginianus", "Diphylla ecaudata", "Eptesicus fuscus", 
"Euderma maculatum", "Eumops floridanus", "Eumops perotis", "Eumops underwoodi", 
"Idionycteris phyllotis", "Lasionycteris noctivagans", "Lasiurus borealis", 
"Lasiurus cinereus", "Lasiurus cinereus semotus", "Lasiurus ega", 
"Lasiurus frantzii", "Lasiurus intermedius", "Lasiurus minor", 
"Lasiurus seminolus", "Lasiurus xanthinus", "Leptonycteris nivalis", 
"Leptonycteris yerbabuenae", "Macrotus californicus", "Molossus molossus", 
"Mormoops megalophylla", "Myotis auriculus", "Myotis austroriparius", 
"Myotis californicus", "Myotis ciliolabrum", "Myotis evotis", 
"Myotis grisescens", "Myotis keenii", "Myotis leibii", "Myotis lucifugus", 
"Myotis occultus", "Myotis septentrionalis", "Myotis sodalis", 
"Myotis thysanodes", "Myotis velifer", "Myotis volans", "Myotis yumanensis", 
"Noctilio leporinus", "Nycticeius humeralis", "Nyctinomops femorosaccus", 
"Nyctinomops macrotis", "Parastrellus hesperus", "Perimyotis subflavus", 
"Stenoderma rufum", "Tadarida brasiliensis", "All detections", 
"40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", "Multiple", "Social"
), common_name = c("Pallid bat", "Jamaican fruit-eating bat", 
"Antillean fruit-eating bat", "Mexican long-tongued bat", "Rafinesque's big-eared bat", 
"Townsend's big-eared bat", "Ozark big-eared bat", "Virginia big-eared bat", 
"Hairy-legged vampire bat", "Big brown bat", "Spotted bat", "Florida bonneted bat", 
"Greater bonneted bat", "Underwood's bonneted bat", "Allen's big-eared bat", 
"Silver-haired bat", "Eastern red bat", "Hoary bat", "Hawaiian hoary bat", 
"Southern yellow bat", "Desert Red Bat", "Northern yellow bat", 
"Minor red bat", "Seminole bat", "Western yellow bat", "Mexican long-nosed bat", 
"Lesser long-nosed bat", "California leaf-nosed bat", "Pallas' mastiff bat", 
"Peter's ghost-faced bat", "Southwestern myotis", "Southeastern myotis", 
"California myotis", "Western small-footed myotis", "Long-eared myotis", 
"Gray bat", "Keen's myotis", "Eastern small-footed myotis", "Little brown bat", 
"Arizona myotis", "Northern long-eared bat", "Indiana bat", "Fringed myotis", 
"Cave bat myotis", "Long-legged myotis", "Yuma myotis", "Greater bulldog bat", 
"Evening bat", "Pocketed free-tailed bat", "Big free-tailed bat", 
"Canyon bat", "Tri-colored bat", "Red fruit bat", "Brazilian free-tailed bat", 
"All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", 
"Multiple", "Social"), code4 = c("anpa", "arja", "brca", "chme", 
"cora", "coto", "coti", "cotv", "diec", "epfu", "euma", "eufl", 
"eupe", "euun", "idph", "lano", "labo", "laci", "lacs", "laeg", 
"lafr", "lain", "lami", "lase", "laxa", "leni", "leye", "maca", 
"momo", "mome", "myar", "myau", "myca", "myci", "myev", "mygr", 
"myke", "myle", "mylu", "myoc", "myse", "myso", "myth", "myve", 
"myvo", "myyu", "nole", "nyhu", "nyfe", "nyma", "pahe", "pesu", 
"stru", "tabr", "All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", 
"LoFrag", "Multiple", "Social"), code6 = c("antpal", "artjam", 
"bracav", "chomex", "corraf", "cortow", "cotoin", "cotovi", "dipeca", 
"eptfus", "eudmac", "eumflo", "eumper", "eumund", "idiphy", "lasnoc", 
"lasbor", "lascin", "lacise", "lasega", "lasfra", "lasint", "lasmin", 
"lassem", "lasxan", "lepniv", "lepyer", "maccal", "molmol", "mormeg", 
"myoaur", "myoaus", "myocal", "myocil", "myoevo", "myogri", "myokee", 
"myolei", "myoluc", "myoocc", "myosep", "myosod", "myothy", "myovel", 
"myovol", "myoyum", "noclep", "nychum", "nycfem", "nycmac", "parhes", 
"persub", "steruf", "tadbra", "All detections", "40KHzMyo", "HiF", 
"LoF", "HiFrag", "LoFrag", "Multiple", "Social"), listing.status_federal = c("Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Endangered", "Endangered", "Not Listed", "Not Listed", "Not Listed", 
"Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Endangered", "Endangered", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered", 
"Not Listed", "Not Listed", "Under Review", "Not Listed", "Endangered", 
"Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Proposed Endangered", "Not Listed", "Not Listed", "", "", "", 
"", "", "", "", ""), listing.status_iucn = c("Least Concern", 
"Least Concern", "Least Concern", "Near Threatened", "Least Concern", 
"Least Concern", "", "", "Least Concern", "Least Concern", "Least Concern", 
"Vulnerable", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "", "Least Concern", "Vulnerable", "Least Concern", 
"Least Concern", "Endangered", "Vulnerable", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Vulnerable", 
"Least Concern", "Endangered", "Endangered", "Least Concern", 
"Near Threatened", "Near Threatened", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Vulnerable", 
"Near Threatened", "Least Concern", "", "", "", "", "", "", "", 
""), states.listed = c("", "", "", "AZ,CA", "", "", "", "", "", 
"", "", "FL", "", "", "", "", "", "", "", "", "", "", "", "OK", 
"", "NM,TX", "", "", "", "", "", "", "", "", "", "", "AK,WA", 
"CT,GA,MA,MD,MO,NC,NH,NJ,NY,OH,OK,PA,TN,VA,VT,WV", "CT,MA,ME,MI,NH,NJ,OH,PA,TN,VA,VT,WI", 
"", "", "", "", "", "", "", "", "IN,KY,MI,OH", "", "", "", "", 
"", "", "", "", "", "", "", "", "", ""), states.present = c("AZ,CA,CO,ID,KS,MT,NM,NV,OK,OR,TX,UT,WA", 
"PR", "PR,VI", "AZ,CA,NM,TX", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,SC,TN,VA,WV", 
"AR,AZ,CA,CO,ID,KS,KY,MO,MT,NC,ND,NE,NM,NV,OK,OR,SD,TX,UT,VA,WA,WV,WY", 
"AR,MO,OK", "KY,NC,VA,WV", "TX", "AK,AL,AR,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,CO,MT,NM,NV,OR,UT,WA,WY", "FL", "AZ,CA,NM,TX", "AZ", "AZ,CA,CO,NM,NV,UT", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AL,AR,CO,CT,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,HI,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"HI", "AZ,CA,NM,TX", "AZ,CA,NM,TX", "AL,FL,GA,LA,MS,NC,PA,SC,TX,VA", 
"PR", "AL,AR,FL,GA,KY,LA,MO,MS,NC,OK,SC,TN,TX,VA", "AZ,CA,NM", 
"AZ,NM,TX", "AZ,CA,NM", "AZ,CA,NV", "FL", "AZ,TX", "AZ,NM", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,OK,SC,TN,TX", 
"AZ,CA,CO,ID,MT,NM,NV,OR,TX,UT,WA,WY", "AZ,CA,CO,ID,KS,MT,ND,NE,NM,NV,OK,OR,SD,TX,UT,WA,WY", 
"AZ,CA,CO,ID,MT,ND,NM,NV,OR,SD,UT,WA,WY", "AL,AR,GA,IL,IN,KS,KY,MO,MS,NC,OK,TN,VA,WV", 
"AK,WA", "AL,AR,CT,GA,KY,MA,MD,ME,MI,MO,NC,NH,NJ,NY,OH,OK,PA,RI,TN,VA,VT,WV", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,NM", "AL,AR,CT,DE,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NY,OH,OK,PA,RI,SC,SD,TN,VA,VT,WI,WV,WY", 
"AL,AR,CT,IA,IL,IN,KY,MD,MI,MO,NC,NJ,NY,OH,OK,PA,TN,VA,VT,WV", 
"AZ,CA,CO,NM,NV,OR,SD,TX,UT,WA,WY", "AZ,CA,KS,NM,OK,TX", "AK,CA,CO,ID,MT,ND,NE,NM,OR,SD,TX,WY", 
"CA,CO,ID,MT,NV,OR,TX,UT,WA", "", "AL,AR,FL,GA,IA,IL,IN,KS,KY,LA,MD,MI,MN,MO,MS,NC,NE,OH,OK,PA,SC,TN,TX,VA,WI,WV", 
"AZ,CA,NM,TX", "CA,NV,TX,UT", "AZ,CA,CO,NM,NV,OK,TX,UT,WA", "AL,AR,CO,CT,DC,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,NC,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"PR,VI", "AZ,CA,CO,FL,KS,NM,NV,OK,TX,UT", "", "", "", "", "", 
"", "", ""), states.endangered = c("", "", "", "", "", "", "", 
"", "", "", "", "FL", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "NM,TX", "", "", "", "", "", "", "", "", "", "", 
"", "NH", "CT,MA,ME,NH,NJ,PA,VA,VT", "", "", "", "", "", "", 
"", "", "IN", "", "", "", "", "", "", "", "", "", "", "", "", 
"", ""), states.threatened = c("", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "PA,VT", 
"TN,WI", "", "", "", "", "", "", "", "", "KY,MI", "", "", "", 
"", "", "", "", "", "", "", "", "", "", ""), state.sppofconcern = c("", 
"", "", "AZ,CA", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NJ,NY,OH,OK,TN,VA,WV", 
"MI,OH", "", "", "", "", "", "", "", "", "OH", "", "", "", "", 
"", "", "", "", "", "", "", "", "", ""), listing.status_federal.proposed = c("", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "Under Review, start year unconfirmed", "", 
"", "", "", "", "", "", "", "", "", "", "", "Proposed Endangered, 2022", 
"", "", "", "", "", "", "", "", "", ""), hibernation.strategy = c("resident", 
"resident", "resident", "migratory", "hibernating", "hibernating", 
"hibernating", "hibernating", "unknown", "hibernating", "mixed", 
"resident", "resident", "resident", "unknown", "migratory", "migratory", 
"migratory", "resident", "resident", "migratory", "resident", 
"resident", "mixed", "resident", "migratory", "migratory", "resident", 
"resident", "unknown", "hibernating", "mixed", "hibernating", 
"hibernating", "hibernating", "hibernating", "hibernating", "hibernating", 
"hibernating", "hibernating", "hibernating", "hibernating", "hibernating", 
"mixed", "hibernating", "mixed", "unknown", "migratory", "migratory", 
"migratory", "resident", "hibernating", "resident", "mixed", 
"", "", "", "", "", "", "", ""), phonic.group = c("Lof", "None", 
"None", "Hif", "Lof", "Lof", "Lof", "Lof", "None", "Lof", "Lof", 
"Lof", "Lof", "Lof", "Lof", "Lof", "Hif", "Lof", "Lof", "Hif", 
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "None", "Hif", 
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
"Hif", "Lof", "Lof", "Hif", "Hif", "None", "Lof", "", "", "", 
"", "", "", "", ""), notes = c("", "", "", "", "", "", "same as C. townsendii (subspecies)", 
"same as C. townsendii (subspecies)", "only marginal/historical US records - winter behavior in US range not documented; $phonic.group reflects general vampire-bat biology (faint, short-range echolocation), not US-specific data", 
"", "some individuals migrate to warmer areas in winter, others do not - genuinely mixed at species level per general accounts, LOWER CONFIDENCE on the exact split", 
"", "", "very limited US (AZ) records", "poorly studied - winter/hibernation-site behavior not well documented for this species", 
"", "", "", "same call/hibernation biology as mainland L. cinereus, but the Hawaiian population does not undertake the mainland's continental migration", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group, less-studied)", 
"recently split from L. blossevillii - LOWER CONFIDENCE, based on close congeners", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group)", 
"Caribbean population - LOWER CONFIDENCE, based on close congeners (L. borealis-type)", 
"documented partial migrant - some individuals overwinter via torpor in the Deep South rather than migrating", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group)", 
"", "", "the ONLY North American bat documented to stay fully active year-round with no hibernation or migration, even in the desert", 
"", "LOWER CONFIDENCE hibernation call: only a marginal edge-of-range US (TX/AZ) population, poorly documented", 
"LOWER CONFIDENCE (desert Myotis, less-studied than eastern species)", 
"documented species-level variability - some populations hibernate in caves, Florida populations largely remain active year-round", 
"LOWER CONFIDENCE (mild-climate coastal populations may be less strict hibernators than assumed here)", 
"", "", "", "", "", "", "LOWER CONFIDENCE (desert Myotis, less-studied)", 
"", "", "", "documented species-level variability - northern populations hibernate, southern/border populations may remain active in mild winters", 
"", "documented species-level variability - similar pattern to M. velifer/austroriparius", 
"no confirmed current PR/US-territory population per $states.present (blank) - hibernation.strat reflects lack of a documented US-range population, not species biology generally; $phonic.group instead reflects general species/family biology (a loud, high-frequency fishing bat) since call type is a fixed physical trait independent of range presence", 
"some populations migrate, southern populations may be more resident - classified migratory per general accounts, LOWER CONFIDENCE on the split", 
"", "", "", "", "", "very well-documented species-level mix: most populations (e.g. the famous Bracken Cave, TX colony) migrate to Mexico for winter, but Florida/Gulf coast populations are non-migratory and active year-round", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".", 
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\"."
)), row.names = c(NA, -62L), class = "data.frame")

  # ---------------------------------------------------------------------------
  # Known typo/variant corrections for the non-species category labels -
  # applied BEFORE the normal latin/common/code4/code6 lookup below, per
  # Josh (2026-09-24). See @details, "Known typo/variant corrections", for
  # the full rationale. Add another known typo/variant by adding one more
  # "typo" = "canonical value" entry here.
  # ---------------------------------------------------------------------------
  # Sourced directly from Josh's uploaded "spell check" tab
  # (batz.batusa_recode.names_reference_20260925.xlsx, 2026-09-25) - see
  # @details, "Known typo/variant corrections, expanded (2026-09-25)".
  # "2Bat" (the tab's own duplicate, differing only in case from "2bat")
  # is omitted here - normalize() already folds case, so a second entry
  # would just be a redundant duplicate key, not a distinct correction.
  typo.corrections <- c(
    "40kMyo"     = "40KHzMyo",
    "2bat"       = "Multiple",
    "40kmyomyvo" = "40KHzMyomyvo",
    "HighF/HiF"  = "HiF",
    "LowF/LoF"   = "LoF",
    "LowF"       = "LoF",
    "HighF"      = "HiF"
  )

  match.cols <- c("scientific_name", "common_name", "code4", "code6")

  if (!(batname.format.out %in% names(nabat.names))) {
    stop(sprintf("batname.format.out must be one of the reference database's headers: %s (got '%s')",
                  paste(names(nabat.names), collapse = ", "), batname.format.out))
  }

  reference <- nabat.names
  reference[] <- lapply(reference, function(col) trimws(as.character(col)))

  # matching-only normalization: fold case, treat underscores/dashes as
  # spaces, collapse/trim whitespace. Never affects the VALUE returned.
  normalize <- function(x) {
    x <- as.character(x)
    x <- gsub("[-_]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }

  recode.vec <- function(x) {
    lookup.values <- unlist(lapply(match.cols, function(cn) normalize(reference[[cn]])),
                             use.names = FALSE)
    lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

    x.orig <- as.character(x)

    # Known typo/variant corrections - see typo.corrections above and
    # @details. A recognized misspelling/shorthand is rewritten to its
    # canonical spelling BEFORE the ordinary lookup below runs, matched the
    # same case/whitespace/dash-underscore-insensitive way as everything
    # else via normalize(). If a corrected value still doesn't match
    # anything downstream, the ORIGINAL uncorrected input is what's
    # returned unchanged (see "out <- x.orig" below), not the corrected
    # guess - consistent with every other unmatched value.
    typo.idx    <- match(normalize(x.orig), normalize(names(typo.corrections)))
    typo.found  <- !is.na(typo.idx)
    x.corrected <- x.orig
    x.corrected[typo.found] <- unname(typo.corrections[typo.idx[typo.found]])

    x.norm <- normalize(x.corrected)

    match.idx <- match(x.norm, lookup.values)
    row.idx   <- lookup.rowidx[match.idx]   # NA where match.idx is NA
    found     <- !is.na(row.idx)

    out <- x.orig
    out[found] <- as.character(reference[[batname.format.out]][row.idx[found]])

    if (!grammar.dash) {
      out[found] <- gsub("-", " ", out[found])
    }

    list(values = out, unmatched = x.orig[!found])
  }

  if (is.data.frame(data)) {
    results <- lapply(data, recode.vec)
    out <- as.data.frame(lapply(results, function(r) r$values), stringsAsFactors = FALSE)
    names(out) <- names(data)
    unmatched.all <- unlist(lapply(results, function(r) r$unmatched), use.names = FALSE)
  } else {
    result <- recode.vec(data)
    out <- result$values
    unmatched.all <- result$unmatched
  }

  if (length(unmatched.all) > 0) {
    warning.vector <- unique(unmatched.all)
    shown <- head(warning.vector, 25)
    omitted.note <- if (length(warning.vector) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(warning.vector))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match: %s%s\n",
                 length(unmatched.all), paste(shown, collapse = ", "), omitted.note))
  }

  out
}

#' Summarize vetted bat-acoustic detections into a plotting-ready frame
#'
#' Given a fully-assembled data frame of vetted bat-acoustic detections
#' (the output of \code{\link{batz.merge_vetted.acoustics}}, further
#' joined with a call-datetime column - see Details), builds a summary
#' table of detection counts and the earliest/latest call time (in
#' minutes since the start of that monitoring night) per species,
#' monitoring night, and grouping column - the shape you'd feed straight
#' into a plotting function. Also loads an \code{*arulist.csv} file and
#' joins its \code{$sunregion} column onto \code{data} itself (see
#' \strong{$sunregion lookup} below) - unlike every other required column,
#' \code{data} does NOT need to already have \code{$sunregion}.
#'
#' \strong{Required input columns.} `data` must have every one of:
#' \code{filename}, \code{date.mon}, \code{manid}, \code{autoid.kp},
#' \code{autoid.sb}, \code{lat}, \code{serial}, \code{lon},
#' \code{aru.name}, \code{date}, \code{time}, \code{call.datetime}. All
#' twelve come straight out of \code{\link{batz.merge_vetted.acoustics}}
#' - no renaming needed (an earlier version of this function required
#' \code{$call.time} instead, which didn't match that function's own
#' \code{$call.datetime} output column; standardized on
#' \code{$call.datetime} across both, per Josh, 2026-08-26).
#' \code{$sunregion} is deliberately NOT in this list - see
#' \strong{$sunregion lookup} below.
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project
#' preference) - two different things in this function, treated
#' differently.} This function's \code{required.headers} list above
#' (\code{filename}, \code{date.mon}, \code{manid}, ... \code{call.datetime})
#' is \code{data}'s own already-established \code{batz} output schema -
#' every one of those names comes straight out of
#' \code{\link{batz.merge_vetted.acoustics}}'s own dot-separated,
#' \code{batz}-invented column names, not raw text copied from some
#' loaded file's real header row. Renaming these to snake_case would
#' break the upstream/downstream contract with every function that
#' produces or consumes them by exact name, so they are deliberately left
#' untouched - out of scope for this preference, the same reasoning
#' already applied elsewhere in this package. The \code{*arulist.csv}
#' file loaded in \strong{$sunregion lookup} below is different: those
#' are real, loaded-from-disk file headers, so they now go through the
#' shared package helper \code{standardize.headers()} (trim whitespace,
#' collapse non-alphanumeric runs to underscores, lowercase), replacing
#' this function's prior local, more lenient ad hoc normalizer (which
#' stripped every non-alphanumeric character with no separator instead of
#' substituting an underscore). This is a no-op in practice against every
#' real \code{WTG.arulist.csv} checked so far - \code{$aru}/\code{$sunregion}
#' are already single lowercase words with nothing for either normalizer
#' to differ on - but it brings this function's file-loading step in line
#' with every other \code{batz} function that loads a file, and guards
#' against a future arulist file whose headers pick up stray whitespace,
#' mixed case, or punctuation.
#'
#' \strong{BUGFIX/NEW (2026-09-21, per Josh's real-world report of a
#' header mismatch caused by header-standardization elsewhere in the
#' pipeline):} \code{data}'s own headers (matched against
#' \code{required.headers} above) are now canonicalized via the shared
#' package helper \code{\link{canonicalize.headers}} rather than a plain
#' \code{setdiff()}: both \code{data}'s real column names and
#' \code{required.headers} are standardized to snake_case purely to MATCH
#' them up, so \code{data} is accepted whether its columns are already
#' this function's own dot-separated convention (\code{"date.mon"}), have
#' come back snake_cased from some intervening save/reload step
#' (\code{"date_mon"}), or anything equivalent in between - every matched
#' column is renamed, in this function's own local working copy only, back
#' to the exact dot-separated spelling in \code{required.headers}, so
#' every line of code below this check keeps referencing \code{$date.mon}/
#' \code{$aru.name}/etc. exactly as before. This never mutates the
#' \code{data} object the caller passed in (R already copies a data frame
#' argument on modification) and never changes \code{plfr.batsummary}'s
#' own output column names - see the new \code{snake_case} parameter below
#' for that.
#'
#' \strong{$sunregion lookup.} \code{$sunregion} isn't produced by
#' \code{\link{batz.merge_vetted.acoustics}} (or any upstream
#' \code{batz} function) as a column of \code{data} itself, so this
#' function loads it separately: \code{dir.load} (searched recursively if
#' \code{dir.sub = TRUE}, the default) is scanned for file(s) matching
#' \code{load.pattern} (default \code{"*.arulist.csv"}, e.g. the real
#' \code{WTG.arulist.csv}), each matching file is read and must have (after
#' running its headers through \code{standardize.headers()} - see
#' \strong{Header standardization} above) both an \code{$aru} and a
#' \code{$sunregion} column - a file missing either is skipped with a
#' \code{message()}, not a hard stop, in case other unrelated files happen
#' to also match \code{load.pattern}. Every valid file's
#' \code{$aru}/\code{$sunregion} columns are row-bound together (with
#' \code{$aru} immediately renamed to \code{$aru.name} in this function's
#' own local lookup table - see \strong{Follow-up, 2026-09-22} below), then
#' joined onto \code{data} by matching \code{data$aru.name} against the
#' arulist's own \code{$aru.name} (always \code{$aru.name} specifically,
#' never whatever \code{groupby} is set to - \code{groupby} can be
#' overridden to an unrelated column like \code{"serial"} that wouldn't
#' correspond to the arulist's \code{$aru.name} values at all). Any
#' \code{$aru.name} value with no match in the loaded arulist gets
#' \code{NA} for \code{$sunregion}, with a \code{warning()} listing every
#' such value (not a hard stop - matches this function's existing
#' tolerant-but-vocal style elsewhere, e.g. \code{trim.noise}/\code{trim.noid}).
#' No matching file found at all, or files found but none with the right
#' columns, IS a hard stop - there'd be no way to populate \code{$sunregion}
#' at all. Any \code{$sunregion} value already present in the \code{data}
#' passed in is overwritten by this fresh join, not preserved.
#'
#' \strong{Steps.} If any required header is missing, stops immediately
#' and lists every missing header by name. If \code{duplicates.remove =
#' TRUE} (default), exact duplicate rows are dropped from \code{data}.
#' Every row gets a helper \code{$obs = 1}. \code{$mins2.noon} is computed
#' per row as the number of minutes from noon on the date named by
#' \code{groupby.date} (the start of that monitoring night, since these
#' are nocturnal-animal records - a night starting at noon on
#' \code{$date.mon} and ending at noon the next calendar day) to
#' \code{$call.datetime} for that row - both columns' formats are
#' auto-detected against a small built-in candidate list rather than
#' assumed fixed (see below).
#'
#' A per-\code{spp.id}/\code{groupby.date}/\code{groupby} summary
#' (\code{plfr.batsummary}) is always built first, with columns
#' \code{$spp.id} (the VALUES of whichever column \code{spp.id} names),
#' \code{$date} (the values of the \code{groupby.date} column),
#' \code{$group} (the values of the \code{groupby} column - e.g. a
#' detector name like \code{"105059-NW3"} - see \strong{$group vs
#' $groupedby} below), \code{$groupedby}/\code{$groupby.date} (metadata -
#' see below), \code{$sunregion} (the \code{data}'s own \code{$sunregion}
#' value for that group - see \strong{Follow-up} below), \code{$obs}
#' (count of detections in that group), and \code{$mins2.noon.min}/
#' \code{$mins2.noon.max} (the smallest/largest \code{$mins2.noon} in
#' that group).
#'
#' \strong{$group vs $groupedby/$groupby.date - a new, flagged
#' interpretive call (2026-08-28), per Josh's request to add
#' "\code{$groupby.date} = that the date was grouped by e.g.
#' \code{'date.mon'} or \code{'week.mon'}", "\code{$groupedby} = what the
#' data was grouped by e.g. \code{'aru.name'} or \code{'deployment.type'}",
#' and "\code{$group} = \code{'aru.name'} or \code{'deployment.type'} for
#' that observation".} \code{$groupedby} and \code{$groupby.date} are
#' implemented as METADATA columns - constant for every row of a single
#' call, holding the literal NAME of the column that was grouped by (the
#' current value of the \code{groupby}/\code{groupby.date} parameters,
#' e.g. \code{"aru.name"}/\code{"date.mon"}) - while \code{$group} is the
#' actual per-row grouping VALUE (e.g. \code{"105059-NW3"}, not the column
#' name \code{"aru.name"} again). Josh's own spec text gives \code{$group}
#' the exact same example values as \code{$groupedby}
#' (\code{"aru.name"}/\code{"deployment.type"}), which would make the two
#' columns redundant if taken completely literally; read instead as a
#' copy/paste artifact in the spec, since \code{$group}'s own wording -
#' "for that observation" - only makes sense as a per-row VALUE, and a
#' redundant column wouldn't match the stated intent of \code{$groupedby}
#' (naming the CATEGORY, e.g. \code{"aru.name"} vs \code{"deployment.type"})
#' vs \code{$group} (the specific instance within that category, e.g. one
#' particular detector or deployment type). \strong{Please confirm this
#' reading is correct} - if \code{$group} was actually meant to just repeat
#' the column name, say so and it's a one-line change.
#'
#' If \code{alldetections = FALSE}, \code{$vetting.type} (the literal
#' NAME of the column \code{spp.id} points at, e.g. \code{"manid.sb"}) is
#' added to every row and this per-species table is returned as-is.
#'
#' If \code{alldetections = TRUE} (default), rows where the \code{spp.id}
#' column is \code{"noise"} (if \code{trim.noise = TRUE}, default) or
#' \code{"NoID"} (if \code{trim.noid = TRUE}, default) - case-insensitive -
#' are additionally removed from a COPY of the data, and the SAME
#' summarization is repeated on that trimmed copy, grouped only by
#' \code{groupby.date}/\code{groupby} (species collapsed), with
#' \code{$spp.id} forced to the literal string \code{"All Detections"}.
#' That table is row-bound onto the per-species table from above, then
#' \code{$vetting.type} is added to every row and the combined table is
#' returned. Because the trim only happens for this second, collapsed
#' table, \code{"noise"}/\code{"NoID"} still appear as their own rows in
#' the per-species breakdown (so you can see how much noise/unidentified
#' activity there was per night/group) while the \code{"All
#' Detections"} total reflects only genuine wildlife activity. This
#' "build both, row-bind them" behavior was the most open-ended part of
#' the original spec and is flagged as a real interpretive call - please
#' confirm it's what's wanted (see the dev script's header comment for
#' the alternative readings that were considered and set aside).
#'
#' \strong{Date/time format auto-detection.} Neither \code{groupby.date}
#' (e.g. raw \code{$date.mon}/\code{$monitoringnight} values like
#' \code{"6/26/2026"}) nor \code{call.datetime}'s own values (which may
#' already be in \code{\link{batz.datawrangler_call.datetime}}-style
#' format OR a raw recorder \code{Timestamp} string like
#' \code{"5/17/2026 21:21"}, confirmed against real data) can be assumed
#' to already be in one fixed format. \code{groupby.date} is parsed by
#' reusing \code{\link{batz.datawrangler_call.datetime}}'s own format
#' auto-detection (fed alongside a constant noon time). \code{
#' call.datetime} is parsed via a small internal regex-guarded candidate
#' list covering both shapes above - same exact-full-string-match-before-
#' parsing pattern used elsewhere in this package, to avoid a silent
#' partial-string parse.
#'
#' Unlike the merge/load-style \code{batz} functions, this one has no
#' bare-call auto-assign side effect and writes nothing to disk - the
#' original spec's own wording just says "return plfr.batsummary" (one
#' data frame), so it's returned directly.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("include
#' $sunregion from the merged data"): \code{$sunregion} is now carried
#' through into \code{plfr.batsummary}.} \code{$sunregion} was already a
#' REQUIRED input column (see above) but, before this change, was
#' validated and then silently dropped - never appearing anywhere in the
#' output. It's collapsed the same way as every other summary column,
#' one value per \code{spp.id}/\code{groupby.date}/\code{groupby}
#' group, and placed right after \code{$group} in the output
#' (it's a detector-level attribute, so it reads naturally grouped with
#' \code{$group} rather than at the end). Since \code{$sunregion}
#' is expected to be constant for a given detector (it's joined in via
#' \code{$aru.name} upstream of this function - see the required-columns
#' paragraph above), each group is checked for internal consistency
#' rather than just taking the first value seen: if a single
#' \code{spp.id}/\code{groupby.date}/\code{groupby} group somehow
#' contains more than one distinct \code{$sunregion} value (e.g.
#' \code{groupby} overridden to a column, such as \code{"serial"},
#' that doesn't line up 1:1 with \code{$sunregion} the way \code{$aru.name}
#' does), the function now stops with a message naming the exact group
#' and the conflicting values, rather than silently picking one. A group
#' with no non-blank \code{$sunregion} value at all (shouldn't happen
#' given the required-column check, but defensive) gets \code{NA}
#' rather than erroring. No other column, ordering, or behavior changed;
#' full dev-script test suite re-run against real data, plus a new test
#' for the inconsistent-\code{$sunregion} error path.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh: this function now
#' loads \code{$sunregion} itself from an \code{*arulist.csv} file,
#' instead of requiring it already be a column of \code{data}.} Three new
#' parameters were added - \code{dir.load} (default \code{getwd()}),
#' \code{load.pattern} (default \code{"*.arulist.csv"}), \code{dir.sub}
#' (default \code{TRUE}) - matching the naming and \code{glob2rx()}-based
#' matching style already used by
#' \code{\link{batz.merge_vetted.acoustics}}'s own \code{dir.load}/
#' \code{load.pattern}/\code{dir.sub}, though \code{dir.sub} defaults to
#' \code{TRUE} here (searching subdirectories by default) rather than that
#' function's \code{FALSE}, per Josh's explicit spec for this update.
#' \code{$sunregion} is REMOVED from the required-input-columns list above
#' - the previous entry (directly above this one) still describes it as
#' required because that was true until this change; it no longer is. See
#' the new \strong{$sunregion lookup} paragraph, near the top of Details,
#' for the full join mechanism (skip-with-message for an arulist file
#' missing \code{$aru}/\code{$sunregion}, warn-not-stop for an unmatched
#' \code{$aru.name}, hard stop only if no usable arulist file is found at
#' all). Interpretive calls made here, flagged for Josh to confirm: (1)
#' any \code{$sunregion} already present in the \code{data} passed in is
#' silently overwritten by the fresh join, not preserved or checked for
#' agreement - "append the $sunregion to the bioactivity file" was read as
#' "make sure it ends up there, from the arulist," not "only fill it in if
#' missing"; (2) multiple files matching \code{load.pattern} are all
#' loaded and row-bound together (rather than erroring on finding more
#' than one, or using only the first) - useful if ARUs are split across
#' more than one arulist file, but means two files defining conflicting
#' \code{$sunregion} values for the same \code{$aru} would silently let
#' whichever row \code{match()} finds first win; not specifically guarded
#' against, since Josh's own real setup has always used exactly one
#' \code{WTG.arulist.csv}. Verified against the real
#' \code{WTG.arulist.csv} (in \code{dir.load}, found via the default
#' \code{load.pattern}): every real test-data \code{$aru.name} value
#' resolves to \code{"penobscotbay"}, matching the arulist's own values
#' exactly, and a synthetic unmatched \code{$aru.name} correctly triggers
#' the new unmatched-value warning with \code{$sunregion = NA}. Full
#' dev-script test suite re-run, no regressions.
#'
#' \strong{Follow-up, 2026-08-28, per Josh: function and two parameters
#' renamed; three new/changed output columns added.} Renamed
#' \code{batz.plotframe_batactivity()} to
#' \code{batz.generate_plotframe.bat()}; renamed the \code{date.groupby}
#' parameter to \code{groupby.date} and the \code{aru.groupby} parameter to
#' \code{groupby} (every internal reference, error message, and this
#' documentation updated to match - no functional change from the rename
#' itself). Added \code{$groupby.date}/\code{$groupedby} (metadata columns
#' naming which column was grouped by) and renamed the old \code{
#' $aru.groupby} output column (which held the actual per-row grouping
#' value) to \code{$group} - see \strong{$group vs $groupedby} above for
#' the interpretive call flagged around these three columns. All prior
#' behavior (arulist loading/joining, sunregion consistency check,
#' alldetections logic, date/time auto-detection) is unchanged; full
#' dev-script test suite updated for the new names and re-run, no
#' regressions.
#'
#' \strong{Standardized 2026-08-29, per Josh: two parameter defaults
#' changed for consistency.} \code{dir.sub}'s default changed from
#' \code{TRUE} back to \code{FALSE}, matching every other \code{batz}
#' function's \code{dir.sub} default (the earlier \code{TRUE} default
#' was per an explicit spec at the time - this reverses that for
#' consistency). \code{trim.noid}'s default changed from \code{TRUE} to
#' \code{FALSE}. No other behavior changed.
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request ("change all functions
#' that have aru as an header to \"aru.name\"", found via the project's own
#' reference workbook and cross-checked against this function's live
#' source): the \code{*arulist.csv} lookup table this function builds
#' internally now uses \code{$aru.name} instead of bare \code{$aru}.} This
#' is a purely internal rename, entirely local to this function's own
#' \code{arulist} lookup object (never returned or exposed to the caller,
#' and never merged as a COLUMN with \code{data} - only its VALUES are
#' ever joined via \code{match()}) - done for consistency with the rest of
#' the package's ARU-identifier naming convention (\code{$aru.name} is
#' already required in \code{data}'s own schema above, and is now also
#' \code{\link{batz.generate_suntimes.arulist}}'s own output column name,
#' renamed the same day). \strong{The raw \code{*arulist.csv} file itself
#' does NOT need to change} - the per-file check just below (see
#' \strong{$sunregion lookup}) still looks for a column that standardizes
#' to literal \code{"aru"}, exactly as before; immediately after a file
#' passes that check, its \code{$aru} column is renamed, in this
#' function's own local lookup table only, to \code{$aru.name} before
#' being row-bound in - the same rename-after-match idiom already used by
#' \code{canonicalize.headers()} elsewhere in this function (see
#' "BUGFIX/NEW" above), applied here directly since this particular check
#' is a plain per-file \code{%in%} test, not a \code{canonicalize.headers()}
#' call. The join itself (\code{match(data$aru.name, arulist$aru)}) now
#' reads \code{match(data$aru.name, arulist$aru.name)} - both sides
#' already meant the same thing, this just makes the column names agree
#' too. No functional/behavioral change: every real \code{WTG.arulist.csv}
#' still needs the same raw columns it always did, and the join still
#' matches the same values the same way. Full dev-script test suite
#' re-run clean after the rename (no regressions).
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request ("Update function
#' batz.generate_plotframe.bat() spp.id = 'manid.sb' if value = '' then
#' replace with 'NOID'"): a blank \code{$spp.id} value is now standardized
#' to the literal string \code{"NOID"}.} Applied immediately after the
#' spp.id/groupby.date/groupby column-existence check, before the
#' \code{$sunregion} lookup or any summarizing - so every downstream step
#' (the per-species breakdown, \code{trim.noid}'s case-insensitive
#' \code{"NoID"} filter, and the \code{$vetting.type} column) sees
#' \code{"NOID"} rather than an empty string for these rows. A "blank"
#' value covers an empty string, \code{NA}, and a whitespace-only string
#' (all coerced to character first) - read as the natural reading of
#' Josh's literal \code{value = ""}, since a real loaded CSV can produce
#' any of the three for a genuinely missing manual ID. \strong{Judgment
#' call, flagged for Josh}: the replacement is case-sensitive uppercase
#' \code{"NOID"} (matching Josh's own literal spelling), which is a
#' distinct string from the existing lowercase-insensitive \code{"NoID"}
#' that \code{trim.noid} already recognized before this change - the two
#' now coexist as the same case-insensitive value (\code{trim.noid}'s
#' check is \code{tolower(...) == "noid"}, so both spellings match it
#' identically), so no existing behavior changes for rows that already
#' said "NoID"; this only affects rows that were previously blank.
#'
#' \strong{Follow-up, 2026-09-23, per Josh's request ("if spp.id =
#' manid.sb and trim.noid = TRUE then remove any records that are
#' blank"): blank \code{$manid.sb} records are now removed entirely,
#' instead of filled in with "NOID", when this specific combination
#' applies.} A narrower, opt-in alternative to the blanket "NOID" fill-in
#' immediately above: when \code{spp.id} is still specifically the
#' default column name \code{"manid.sb"} \strong{and} \code{trim.noid =
#' TRUE}, a row with a blank value in that column (empty string, or
#' \code{NA}/whitespace-only - the same "blank" definition as the NOID
#' fill-in above) is dropped from \code{data} entirely, before any
#' summarizing, rather than having its \code{spp.id} value replaced with
#' \code{"NOID"}. Every other combination - a non-default \code{spp.id}
#' column, or \code{trim.noid = FALSE} - keeps the unconditional "NOID"
#' fill-in exactly as before. \strong{This is a different mechanism from
#' the pre-existing \code{trim.noise}/\code{trim.noid} behavior described
#' above} (which only excludes \code{"noise"}/\code{"NoID"} rows from the
#' collapsed \code{"All Detections"} copy, while those rows still appear
#' in the per-species breakdown): this new removal deletes the
#' originally-blank rows from \code{data} itself, so they disappear from
#' \strong{both} the per-species table and the \code{"All Detections"}
#' total - there is no row left for them to appear as \code{"NOID"} in
#' either place. \strong{Judgment call, flagged for Josh}: read
#' \code{spp.id = manid.sb} in the request as testing the \emph{current
#' value of the \code{spp.id} parameter} (i.e. still its default,
#' whether left at the default or explicitly passed as
#' \code{"manid.sb"}), not testing some other column named
#' \code{manid.sb} while \code{spp.id} points elsewhere - the two are the
#' same column in every real call seen so far, so this only matters if
#' \code{spp.id} is ever overridden to a different column while a
#' \code{$manid.sb} column also happens to exist in \code{data}, which is
#' not one of the two documented columns. Full dev-script test suite
#' re-run, including a new test confirming the general/unconditional NOID
#' fill-in is unchanged for every other \code{spp.id}/\code{trim.noid}
#' combination.
#'
#' @param data A data frame with every column listed above already
#'   present (see Details for how to assemble one). Column headers may
#'   arrive in this function's own dot-separated style OR already
#'   snake_cased (e.g. by an intervening save/reload step) - see the
#'   BUGFIX/NEW entry in Details, "canonicalize.headers".
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from \code{data} before summarizing.
#' @param spp.id Character, default \code{"manid.sb"}. Name of the column
#'   in \code{data} holding the species identifier to summarize by. A
#'   blank value in this column (empty string, or \code{NA}/whitespace-only)
#'   is replaced with the literal string \code{"NOID"} before summarizing -
#'   see \strong{Follow-up, 2026-09-22} in Details - UNLESS \code{spp.id}
#'   is still \code{"manid.sb"} and \code{trim.noid = TRUE}, in which case
#'   those blank-valued rows are removed from \code{data} entirely instead
#'   - see \strong{Follow-up, 2026-09-23} in Details.
#' @param groupby.date Character, default \code{"date.mon"}. Name of the
#'   column in \code{data} holding the date/interval to summarize by.
#'   (Named \code{"groupby.date"} rather than the originally-specced
#'   \code{"date"}, to match the sibling \code{groupby} parameter and
#'   avoid colliding with \code{data}'s own separate \code{$date} column -
#'   told Josh about the rename; renamed again from \code{"date.groupby"}
#'   to \code{"groupby.date"} on 2026-08-28, per Josh's explicit request.)
#' @param groupby Character, default \code{"aru.name"}. Name of the
#'   column in \code{data} holding the detector unit (or other grouping
#'   category, e.g. \code{"deployment.type"}) to summarize by. (Renamed
#'   from \code{"aru.groupby"} to \code{"groupby"} on 2026-08-28, per
#'   Josh's explicit request.)
#' @param alldetections Logical, default \code{TRUE}. See Details.
#' @param trim.noise Logical, default \code{TRUE}. When \code{alldetections
#'   = TRUE}, exclude rows where the \code{spp.id} column is
#'   \code{"noise"} (case-insensitive) from the \code{"All Detections"}
#'   summary.
#' @param trim.noid Logical, default \code{FALSE}. When \code{alldetections
#'   = TRUE}, exclude rows where the \code{spp.id} column is
#'   \code{"NoID"} (case-insensitive) from the \code{"All Detections"}
#'   summary. (Named \code{"trim.noid"} rather than the originally-specced
#'   \code{"trim.noID"}, to match the identically-purposed parameter
#'   already shipped in \code{\link{batz.merge_vetted.acoustics}}.
#'   Standardized 2026-08-29, per Josh: default changed from \code{TRUE}
#'   to \code{FALSE}.) When \code{TRUE} and \code{spp.id} is still
#'   \code{"manid.sb"}, this ALSO causes originally-blank \code{spp.id}
#'   rows to be removed from \code{data} entirely rather than filled in
#'   with \code{"NOID"} - see \strong{Follow-up, 2026-09-23} in Details.
#' @param dir.load Character, default \code{getwd()}. Directory to search
#'   for the \code{*arulist.csv} file(s) used to look up \code{$sunregion}.
#'   See \strong{$sunregion lookup} in Details.
#' @param load.pattern Character vector, default \code{c("*.arulist.csv")}.
#'   A wildcard/glob pattern (or vector of patterns) identifying which
#'   file(s) in \code{dir.load} to load as the arulist, converted
#'   internally to a regex via \code{utils::glob2rx()} (same mechanism
#'   \code{\link{batz.merge_vetted.acoustics}} uses for its own
#'   \code{load.pattern}). Matching is CASE-INSENSITIVE.
#' @param dir.sub Logical, default \code{FALSE}. Also search subdirectories
#'   of \code{dir.load} for the arulist file(s). (Standardized 2026-08-29,
#'   per Josh: default changed from \code{TRUE} back to \code{FALSE} to
#'   match every other \code{batz} function's \code{dir.sub} default -
#'   previously deliberately \code{TRUE} per an earlier explicit spec;
#'   this reverses that for consistency.)
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-21, per
#'   Josh, alongside the \code{canonicalize.headers} fix above. Controls
#'   only \code{plfr.batsummary}'s OWN output column names, applied as the
#'   very last step before it's returned - it has no effect on
#'   \code{data}, which is never modified beyond this function's own local
#'   working copy. \code{FALSE} (default) keeps this function's normal
#'   dot-separated output column names (\code{$spp.id}, \code{$date},
#'   \code{$group}, ...) exactly as always. \code{TRUE} runs every output
#'   column name through \code{standardize.headers()} instead (e.g.
#'   \code{$spp_id}, \code{$mins2_noon_min}) - for a caller who
#'   specifically wants a snake_case CSV/data frame out of this function,
#'   without having to convert it themselves afterward.
#'
#' @return A data frame, \code{plfr.batsummary}, with columns
#'   \code{$spp.id}, \code{$date}, \code{$group}, \code{$groupedby},
#'   \code{$groupby.date}, \code{$sunregion}, \code{$obs},
#'   \code{$mins2.noon.min}, \code{$mins2.noon.max}, \code{$vetting.type}
#'   (or their snake_case equivalents if \code{snake_case = TRUE} - see
#'   that parameter above). See \strong{$group vs $groupedby} in Details
#'   for what \code{$group}/\code{$groupedby}/\code{$groupby.date} each
#'   hold.
#'
#' @examples
#' \dontrun{
#' # default dir.load/load.pattern look for "*.arulist.csv" in the current
#' # working directory (and its subdirectories) to look up $sunregion
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged)
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged, alldetections = FALSE)
#'
#' # explicit dir.load, if the arulist file lives somewhere else
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged,
#'   dir.load = "C:/path/to/arulist/folder")
#'
#' # group by deployment type instead of detector
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged,
#'   groupby = "deployment.type")
#'
#' # snake_case output headers instead of this function's usual dot-style
#' plfr.batsummary <- batz.generate_plotframe.bat(vetted.merged, snake_case = TRUE)
#' }
#'
#' @export
batz.generate_plotframe.bat <- function(data,
                                         duplicates.remove = TRUE,
                                         spp.id = "manid.sb",
                                         groupby.date = "date.mon",
                                         groupby = "aru.name",
                                         alldetections = TRUE,
                                         trim.noise = TRUE,
                                         trim.noid = FALSE,
                                         dir.load = getwd(),
                                         load.pattern = c("*.arulist.csv"),
                                         dir.sub = FALSE,
                                         snake_case = FALSE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

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
  ## this check keeps referencing $date.mon/$aru.name/etc. exactly as
  ## before. See @details, "Header standardization"/"BUGFIX/NEW".
  required.headers <- c("filename", "date.mon", "manid", "autoid.kp",
                         "autoid.sb", "lat", "serial", "lon", "aru.name",
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
  ## as an header to \"aru.name\""): the loaded file itself is still
  ## checked for a column that standardizes to literal "aru" (unchanged -
  ## no real *arulist.csv file needs to be edited for this), but that
  ## column is renamed to $aru.name in this function's own local lookup
  ## table immediately after the check, so the join below reads
  ## $aru.name on both sides - see @details, "Follow-up, 2026-09-22...aru
  ## as an header".
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
    tmp <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { arulist.skipped <- c(arulist.skipped, paste0(f, " (could not read file)")); next }
    names(tmp) <- standardize.headers(names(tmp))
    if (!all(c("aru", "sunregion") %in% names(tmp))) {
      arulist.skipped <- c(arulist.skipped, paste0(f, " (missing $aru and/or $sunregion column)")); next
    }
    tmp.sub <- tmp[, c("aru", "sunregion"), drop = FALSE]
    names(tmp.sub)[names(tmp.sub) == "aru"] <- "aru.name"
    arulist <- rbind(arulist, tmp.sub)
  }
  if (length(arulist.skipped) > 0) {
    message("batz.generate_plotframe.bat: skipped arulist file(s) that didn't have ",
            "both an $aru and $sunregion column: ", paste(arulist.skipped, collapse = "; "))
  }
  if (nrow(arulist) == 0) {
    stop("Found ", length(arulist.files), " file(s) matching `load.pattern` in `dir.load`, ",
         "but none had both an `$aru` and `$sunregion` column - cannot look up $sunregion.")
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

  data$obs <- 1

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
    date.vals  <- as.character(df[[groupby.date]])
    group.vals <- as.character(df[[groupby]])
    sun.vals   <- as.character(df$sunregion)

    key <- paste(spp.vals, date.vals, group.vals, sep = "\r")
    ag.obs <- tapply(df$obs, key, sum)
    ag.min <- tapply(df$.mins2.noon, key, safe.min)
    ag.max <- tapply(df$.mins2.noon, key, safe.max)

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
      date           = vapply(parts, `[`, character(1), 2),
      group          = vapply(parts, `[`, character(1), 3),
      groupedby      = groupby,
      groupby.date   = groupby.date,
      sunregion      = as.character(ag.sun[keys]),
      obs            = as.numeric(ag.obs[keys]),
      mins2.noon.min = as.numeric(ag.min[keys]),
      mins2.noon.max = as.numeric(ag.max[keys]),
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    out
  }

  if (!alldetections) {
    plfr.batsummary <- build.summary(data)
    plfr.batsummary$vetting.type <- spp.id
    if (snake_case) names(plfr.batsummary) <- standardize.headers(names(plfr.batsummary))
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
                                            plfr.batsummary$spp.id), ]
  rownames(plfr.batsummary) <- NULL

  if (snake_case) names(plfr.batsummary) <- standardize.headers(names(plfr.batsummary))

  plfr.batsummary
}

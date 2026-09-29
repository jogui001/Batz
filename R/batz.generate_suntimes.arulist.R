#' Generate sunrise/sunset times for an ARU deployment list
#'
#' For every ARU in an \verb{*arulist.csv} deployment list, and for every
#' calendar date in that ARU's \code{[$date.start, $date.end]} range,
#' calculates sunset on \code{$date}, sunrise on \code{$date}, and sunrise on
#' the FOLLOWING day - i.e. the start/end bounds of that night's monitoring
#' window (sunset of \code{$date} through sunrise of \code{$date + 1}).
#'
#' Solar times are calculated in base R (no package dependencies) using the
#' standard astronomy-answers.nl / NOAA sunrise-sunset algorithm - the same
#' approach used by NOAA's solar calculator and the widely-used "suncalc"
#' JS/R libraries, accurate to within about a minute of NOAA's published
#' tables.
#'
#' Before any solar calculation runs, ARU rows are expanded to one row per
#' (aru, date) and then collapsed to the DISTINCT set of
#' (\code{sunregion}, \code{calc.lat}, \code{calc.long}, \code{time.zone},
#' \code{date}) combinations actually needed (\code{calc.lat}/\code{calc.long}
#' per the \code{$sunregion.type} handling below). ARUs that share a site
#' with identical or overlapping date ranges automatically reuse one
#' calculation per shared date instead of repeating it per ARU.
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real, documented output-schema change.} The raw \verb{*arulist.csv}'s
#' own headers are now run through the shared package helper
#' \code{standardize.headers()} (trim whitespace, collapse every run of
#' non-alphanumeric characters to a single underscore, lowercase) right
#' after loading, before the required-header check below runs. Unlike most
#' other \code{batz} functions, this one's \code{required.headers} list is
#' a literal, uninvented copy of the ARU list file's own real column text
#' (not a \code{batz}-invented shorthand) - so per this project's
#' header-standardization preference, both the raw incoming headers AND
#' \code{required.headers} are standardized together, and the change
#' cascades all the way through to this function's own returned
#' \code{aru.suntimes} column names, since those columns are pass-through
#' copies of the loaded file's fields, not an independently-invented output
#' schema. Concretely, every dot-separated required header becomes
#' underscore-separated: \code{sunregion.type} -> \code{sunregion_type},
#' \code{sunregion.long} -> \code{sunregion_long}, \code{sunregion.lat} ->
#' \code{sunregion_lat}, \code{date.start} -> \code{date_start},
#' \code{date.end} -> \code{date_end}, \code{time.zone} -> \code{time_zone}.
#' \code{aru}, \code{long}, \code{lat}, \code{sunregion}, \code{schedual1},
#' \code{schedual2} have no separators and are unaffected. This does NOT
#' touch this function's own INVENTED fields, which never came from the
#' loaded file - \code{date.monitoringnight}, \code{suns}, \code{suns.unix}, \code{sunr},
#' \code{sunr.unix}, \code{sunr.mon}, \code{sunr.mon.unix}, \code{calc.lat},
#' \code{calc.long} all keep their existing dot-separated names, per this
#' project's ordinary (unrelated to this preference) dot-separated output
#' convention. \strong{Anyone with a saved *arulist.csv using the old
#' dotted header spellings should re-save it with the new underscore
#' spellings (or simply let \code{read.csv()} load it as-is and rely on
#' \code{standardize.headers()} to convert it automatically - a header
#' spelled \code{"sunregion.long"} standardizes to \code{"sunregion_long"}
#' exactly the same as \code{"Sunregion Long"} would), and update any
#' downstream script that reads \code{aru.suntimes} by the old dotted
#' column names.}
#'
#' \strong{Required input headers (updated 2026-09-28 - see \code{@details},
#' "Follow-up, 2026-09-28" below for the dot/underscore-equivalence change
#' that made this possible).} \code{dir.load}'s \verb{*arulist.csv} must
#' have every one of (matched via the shared \code{canonicalize.headers()}
#' helper, which treats \code{"."} and \code{"_"} as equivalent on both
#' sides - see below): \code{aru.name}, \code{long}, \code{lat},
#' \code{sunregion}, \code{sunregion.longitude}, \code{sunregion.latitude},
#' \code{date.start}, \code{date.end}, \code{time_zone},
#' \code{sunregion_type}, \code{schedual1}, \code{schedual2} (a real file's
#' raw header can be spelled with either separator style - see below). If
#' any are missing, the function stops immediately with \code{"inputfile
#' is missing these headers"} followed by the list of missing header names
#' (Josh's own literal message text, used verbatim). \code{$sunregion_type}
#' is now REQUIRED - the previous behavior of defaulting to
#' \code{"fixed.unique"} when the column was entirely absent no longer
#' applies, since a missing \code{$sunregion_type} column now fails this
#' header check before reaching that point. \code{$schedual1}/
#' \code{$schedual2} are new, pass-through-only columns (not used in any
#' calculation, just carried into the output - see below); note the
#' spelling is Josh's own ("schedual", not "schedule"), kept exactly as
#' given.
#'
#' \strong{Only \code{"fixed.unique"}/\code{"fixed.pooled"} rows get
#' records generated (updated 2026-08-26, per Josh) - a real behavior
#' change from erroring to filtering.} Four \code{$sunregion_type}
#' categories exist - \code{"fixed.unique"}, \code{"fixed.pooled"},
#' \code{"mobile.unique"}, \code{"mobile.pooled"} - but only rows where
#' \code{$sunregion_type} is \code{"fixed.unique"} or \code{"fixed.pooled"}
#' get records generated. Any OTHER value (\code{"mobile.unique"}/
#' \code{"mobile.pooled"}, or anything else, including a typo) is
#' EXCLUDED from the run with a console \code{NOTE} listing the affected
#' ARU(s) and their actual \code{$sunregion_type} value, rather than
#' stopping the whole function. \strong{The PREVIOUS version of this
#' function hard-stopped the entire run if ANY row was
#' \code{"mobile.unique"}/\code{"mobile.pooled"}; that hard stop is gone
#' now} - those rows are simply left out of \code{aru.suntimes}. If every
#' row ends up excluded, the function still stops (nothing to generate).
#'
#' \strong{Solar-calculation coordinates now come directly from
#' \code{$sunregion.long}/\code{$sunregion.lat} (updated 2026-08-26, per
#' Josh) - a real behavior change.} The PREVIOUS version used the ARU's
#' own exact \code{$lat}/\code{$long} for \code{"fixed.unique"} rows, and
#' a COMPUTED MEAN of \code{$lat}/\code{$long} across every ARU sharing a
#' \code{$sunregion} for \code{"fixed.pooled"} rows. Now, for EVERY kept
#' row (both types), the solar calculation uses \code{$sunregion.longitude}/
#' \code{$sunregion.latitude} exactly as given in the input file - no
#' averaging happens here anymore; the input file itself is now
#' responsible for carrying one consistent coordinate pair on every row
#' sharing a \code{$sunregion}. \code{$lat}/\code{$long} (the ARU's own
#' coordinates) are still required as input and still appear in the
#' output, just no longer used for the calculation itself. A console
#' \code{NOTE} (non-blocking) is printed if any \code{$sunregion} has
#' more than one distinct \code{$sunregion.longitude}/\code{$sunregion.latitude}
#' pair across its rows - not explicitly requested, added as a light
#' data-entry sanity check matching the existing
#' \code{$sunregion_type}-consistency \code{NOTE} below.
#'
#' A row marked \code{"fixed.unique"} whose \code{$sunregion} does not equal
#' its \code{$aru.name} is flagged with a console \code{NOTE} (per the
#' spec's own definition that the two should match for this type) but does
#' not block the run.
#'
#' \strong{Assumptions made (spec was ambiguous on these - flag for
#' review):}
#' \itemize{
#'   \item \code{$suns} is treated as SUNSET on \code{$date} (the shorthand
#'     "suns"/"sunr" strongly implies sunset vs. sunrise, and \code{$sunr}
#'     is separately and explicitly defined as sunrise - having both be
#'     sunrise would make \code{$suns} a pure duplicate).
#'   \item \code{$date.monitoringnight} is \code{$date} itself at noon (12:00:00), NOT
#'     \code{$date + 1} - taken literally from "date plus time of
#'     12:00:00", with no mention of the following day (unlike
#'     \code{$sunr.mon}, which explicitly says "for the following day").
#'     Noon (rather than midnight) is used so the timestamp can't drift to
#'     the wrong calendar day when read back in a different time zone.
#'   \item \code{[$date_start, $date_end]} is inclusive of both endpoints.
#'   \item Sunrise/sunset use the standard -0.833 degree solar-elevation
#'     threshold (atmospheric refraction + the sun's angular radius).
#'   \item Locations that would produce polar day/night (no sunrise or
#'     sunset on some date) are not expected in this data and are not
#'     specially handled beyond returning \code{NA}.
#' }
#'
#' See \code{batz.generate_suntimes.arulist.dev.R} in the package source repo for
#' the tested procedural version and the full assumptions list. Those
#' assumptions apply here unchanged and should be reviewed before relying
#' on this in production - in particular, please confirm the \code{$suns} =
#' sunset interpretation above is what was intended.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.generate_suntimes.arulist()}: family = "suntimes" (functions
#' that work with ARU deployment lists and solar-time calculations), action =
#' "generate".
#'
#' \strong{Follow-up, 2026-08-30, per Josh: renamed from
#' \code{batz.suntimes_generate()} to \code{batz.generate_suntimes.arulist()}}
#' - this breaks the project's own \code{family_action.subject} convention
#' (the verb normally comes after the underscore, e.g. \code{suntimes_generate},
#' \code{batusa_recode.names}) since "generate" now comes first, but the user
#' was asked to confirm and explicitly chose this exact literal name over a
#' convention-conforming alternative, so it was used as given.
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request to audit and extend the
#' snake_case output option package-wide:} added a \code{snake_case}
#' parameter (default \code{FALSE}, matching
#' \code{\link{batz.generate_plotframe.bat}}'s own). When \code{TRUE},
#' \code{aru.suntimes}'s own output column names are run through
#' \code{standardize.headers()} as the very last step before it's written
#' to CSV (when \code{write.output = TRUE}) and returned - e.g.
#' \code{$date.monitoringnight} becomes \code{$date_monitoringnight}, \code{$sunr.mon.unix} becomes
#' \code{$sunr_mon_unix}. This is unrelated to, and does not change, the
#' existing \verb{*arulist.csv} header standardization described above
#' (which already always runs, regardless of this parameter, on the loaded
#' file's own real headers before the required-header check). The
#' \code{efficiency} list element is left untouched either way - its three
#' columns (\code{aru.date.rows}, \code{site.date.rows},
#' \code{shared.sites}) have no dot-separated words to convert.
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request ("change all functions
#' that have aru as an header to \code{"aru.name"}", found via the project's own
#' reference workbook and cross-checked against this function's live
#' source): the ARU-identifier column is now \code{$aru.name} everywhere in
#' this function's own internals and output, not bare \code{$aru}.} This
#' is an internal/output rename only, done for consistency with the rest of
#' the package's ARU-identifier naming convention (\code{$aru.name} is
#' already the standard name in \code{\link{batz.merge_sm4.logfile}},
#' \code{\link{batz.merge_vetted.acoustics}}/\code{\link{batz.merge_vetted.acoustics2}},
#' and \code{\link{batz.generate_plotframe.bat}}'s own required \code{data}
#' schema). \strong{The raw \verb{*arulist.csv} file itself does NOT need to
#' change} - the required-header check just above (see "Required input
#' headers") still expects a column that standardizes to literal
#' \code{"aru"}, exactly as before; immediately after that check succeeds,
#' the matched column is renamed, in this function's own working copy, from
#' \code{aru} to \code{aru.name} - the same rename-after-match idiom already
#' used by \code{canonicalize.headers()} elsewhere in this package, just
#' applied here directly since this function's own required-header check is
#' a plain \code{setdiff()}, not a \code{canonicalize.headers()} call. Every
#' downstream reference (\code{aru.list$aru}, the \code{fixed.unique}
#' mismatch check, \code{expand.one()}'s per-row expansion, and the final
#' \code{aru.suntimes} data frame) was updated to \code{$aru.name}
#' accordingly - see \code{@return} above, updated to match. This does NOT
#' collide with anything: this rename lives entirely inside this function's
#' own \code{aru.list}/\code{aru.expand}/\code{aru.suntimes} objects, never
#' merged as columns with any other function's \code{data} argument (only
#' VALUES are ever joined by \code{match()}, e.g. in
#' \code{\link{batz.generate_plotframe.bat}}'s own arulist lookup, which
#' received the identical rename the same day). Full dev-script test suite
#' re-run clean after the rename (no regressions) - verified by directly
#' inspecting \code{names(aru.suntimes)} and \code{names(result$aru.suntimes)}
#' for \code{"aru.name"} (not \code{"aru"}) in the real-file-shaped test
#' cases. \strong{Superseded 2026-09-25 (round twenty-six, below): the raw
#' \verb{*arulist.csv}'s own required spelling changed from bare
#' \code{"aru"} to \code{"aru_name"}.}
#'
#' \strong{Output field renamed \code{$date.mon} -> \code{$date.monitoringnight}
#' (round twenty-five), 2026-09-25, per Josh: "I changed my mine and want
#' to use date.monitoringnight instead of date.mon to be more consistent
#' with collaborators."} This function's own \code{$date.mon} was never
#' part of the earlier \code{monnight.date}/\code{date.mon} naming debate
#' (see \code{\link{batz.plotactivity_daily.count}}'s own \code{@details}
#' for that history) - it had already used \code{date.mon} for this same
#' noon-anchored monitoring-night concept all along - but Josh's
#' collaborator-consistency request applies here identically, so this
#' field is renamed to \code{$date.monitoringnight} the same day, alongside
#' every other function using \code{date.mon} for the same concept
#' package-wide (see that same \code{@details} entry for the full list).
#'
#' \strong{Round twenty-six, 2026-09-25, per Josh: full in/out header rename
#' pass ("change these in and out headers") - real, real-file-affecting
#' change, supersedes the affected parts of the "Header standardization"
#' paragraph above.} Josh gave an explicit rename list covering both this
#' function's REQUIRED INPUT headers and its OWN OUTPUT columns:
#' \code{sunregion_lat -> sunregion_latitude}, \code{sunregion_long ->
#' sunregion_longitude}, \code{aru -> aru.name}, \code{long -> longitude},
#' \code{lat -> latitude}, \code{suns -> sunset}, \code{sunr -> sunrise},
#' \code{suns.unix -> sunset.unix}, \code{sunr.unix -> sunrise.unix},
#' \code{sunr.mon -> sunrise.monitoringnight}, \code{sunr.mon.unix ->
#' sunrise.monitoringnight.unix}. Applied as follows:
#' \itemize{
#'   \item \strong{Required input headers changed for real} (not just an
#'     internal/output rename this time, unlike the 2026-09-22
#'     \code{aru}->\code{aru.name} follow-up above, which deliberately left
#'     the raw file's own required spelling as bare \code{aru}): the raw
#'     \verb{*arulist.csv} must now have a column that standardizes to
#'     \code{aru_name} (not bare \code{aru}), \code{longitude} (not
#'     \code{long}), \code{latitude} (not \code{lat}),
#'     \code{sunregion_longitude} (not \code{sunregion_long}), and
#'     \code{sunregion_latitude} (not \code{sunregion_lat}) - see
#'     \code{required.headers} below, updated accordingly.
#'     \strong{Anyone with a saved *arulist.csv using the old spellings
#'     needs to rename those columns} (or let a header standardization
#'     step do it) before this function will accept the file - unlike the
#'     2026-09-22 \code{aru} rename, this one is NOT backward-compatible
#'     with the old raw spellings, per Josh's explicit instruction to
#'     rename these "in" headers, not just the "out" ones. Real project
#'     test-data files were updated to match the same day - see this
#'     project's own preferences log for which files were touched.
#'   \item \strong{Output columns renamed to match}: \code{$aru.name} was
#'     already the output spelling (unchanged); \code{$sunregion_long}/
#'     \code{$sunregion_lat} -> \code{$sunregion_longitude}/
#'     \code{$sunregion_latitude}; \code{$lat}/\code{$long} ->
#'     \code{$latitude}/\code{$longitude}; \code{$suns}/\code{$suns.unix}
#'     -> \code{$sunset}/\code{$sunset.unix}; \code{$sunr}/\code{$sunr.unix}
#'     -> \code{$sunrise}/\code{$sunrise.unix}; \code{$sunr.mon}/
#'     \code{$sunr.mon.unix} -> \code{$sunrise.monitoringnight}/
#'     \code{$sunrise.monitoringnight.unix}. See \code{@return} below,
#'     updated to match.
#'   \item \strong{Purely-internal working names left unchanged, not part
#'     of this rename}: \code{calc.lat}/\code{calc.long} (the resolved
#'     lat/long actually fed into the solar calculation) are never exposed
#'     in \code{required.headers} or in \code{aru.suntimes}'s own output -
#'     Josh's rename list didn't name them, and renaming a purely-internal
#'     variable carries no user-visible benefit, so they were left as-is
#'     to keep this change's footprint minimal.
#'   \item \code{$sunregion}, \code{$date_start}, \code{$date_end},
#'     \code{$time_zone}, \code{$sunregion_type}, \code{$schedual1},
#'     \code{$schedual2} are NOT in Josh's rename list and are unchanged
#'     (this specifically supersedes the "Header standardization" @details
#'     paragraph above only for the \code{aru}/\code{long}/\code{lat}/
#'     \code{sunregion_long}/\code{sunregion_lat} items it lists - every
#'     other item in that older paragraph's list is still accurate).
#'     \strong{Superseded 2026-09-28 for \code{$date_start}/\code{$date_end}
#'     specifically - see the 2026-09-28 \code{@details} entry below: these
#'     two are now \code{$date.start}/\code{$date.end}, both in
#'     \code{required.headers} and in \code{aru.suntimes}'s own output.}
#'     \strong{Superseded again 2026-09-28 (later the same day) for
#'     \code{$sunregion_longitude}/\code{$sunregion_latitude} specifically -
#'     see the "Reference-workbook Change.to pass" \code{@details} entry
#'     below: these two are now \code{$sunregion.longitude}/
#'     \code{$sunregion.latitude}, both in \code{required.headers} and in
#'     \code{aru.suntimes}'s own output. \code{$sunregion_type} was NOT
#'     renamed the same round - see that entry for why.}
#'     \strong{Superseded 2026-09-29 - see the "Merge, 2026-09-29"
#'     \code{@details} entry below: Josh confirmed the merge, so
#'     \code{$sunregion_type} is now \code{$sunregion.type} too, both in
#'     \code{required.headers} and in \code{aru.suntimes}'s own output.}
#'   \item \strong{Also checked package-wide for conflicts, per Josh's
#'     explicit request:} every other \code{batz} function was searched for
#'     any reference to the OLD spellings of these specific tokens as
#'     column names on \code{aru.suntimes}-shaped data (as opposed to a
#'     same-spelled-but-unrelated column on a different object, e.g. a raw
#'     vetted-acoustics file's own \code{$lat}/\code{$long}, which is a
#'     different data source entirely and out of scope for this rename).
#'     See this project's preferences log for the resulting audit findings
#'     and any consequent fixes elsewhere in the package.
#' }
#'
#' \strong{File naming (round twenty-two), 2026-09-24, per Josh's
#' package-wide request ("Update all functions that save files or charts:
#' ... for files follow <project.name>_<filetype.name>_<daterange>_
#' <timestamp>"):} the output CSV name's token ORDER is now reshuffled to
#' put the filetype token ("suntimes") in SECOND position, matching
#' the requested \code{<project.name>_<filetype.name>_<daterange>_
#' <timestamp>} shape exactly, and the redundant \code{"sav"} prefix that
#' used to sit directly in front of the timestamp is dropped (it served no
#' purpose beyond labeling the timestamp as a save-time, which is now
#' already implied by the unified pattern). Concretely, the output file
#' name changes from \verb{<base>_<DATE1>to<DATE2>_sav<timestamp>_
#' suntimes.csv} to \verb{<base>_suntimes_<DATE1>to<DATE2>_<timestamp>.csv}
#' - e.g. what used to save as
#' \code{"aru_20250101to20250202_sav20260826113700_suntimes.csv"} now saves
#' as \code{"aru_suntimes_20250101to20250202_20260924153000.csv"}. The
#' \code{<timestamp>} token itself keeps the same 14-digit, no-separator
#' \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")} shape this function
#' already used before this round (this function's own pre-existing
#' no-underscore timestamp format was in fact the precedent cited when
#' extending that same 14-digit shape to every other \code{batz} function
#' in this round - see each other function's own "Timestamp format (round
#' twenty-two)" \code{@details} entry). \strong{Flagged as a judgment
#' call:} \code{strip.autoname()} (used so a prior stamped output file
#' name can be fed back in as \code{project.name} and get a fresh stamp
#' rather than a second one stacked on top) was updated to match the new
#' token order/shape; it now only strips the NEW-format suffix correctly.
#' A file name saved under the OLD (pre-round-twenty-two) format fed back
#' in as \code{project.name} will no longer strip cleanly - this was not
#' explicitly asked about one way or the other, and re-stripping old-format
#' names was judged not worth the added complexity for what is expected to
#' be a short-lived transition window. \strong{Please confirm this is
#' acceptable}, or say if old-format \code{project.name} values still need
#' to keep stripping correctly going forward.
#'
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's
#' reference-workbook "Change.to" column.} \code{shared.sites} ->
#' \code{shared.sites_count} and \code{site.date.rows} ->
#' \code{site.date_rows.count} in the \code{efficiency} list element's own
#' output column names - see \code{@return} above, updated to match. Both
#' are purely invented output labels (never loaded from, or matched
#' against, any raw file), so the rename is safe to make directly with no
#' real-file-compatibility concern. \strong{Two further renames from the
#' same workbook batch were reviewed and left UNCHANGED at the time,
#' flagged rather than guessed:} \code{aru_name} -> \code{aru.name} and
#' \code{date_end} -> \code{date.end}/\code{date_start} -> \code{date.start}
#' would all touch entries in \code{required.headers} (see the
#' required-header check above) that were, at the time, compared directly
#' against the raw \verb{*arulist.csv}'s OWN headers via a plain
#' \code{setdiff()} after they'd already been run through
#' \code{standardize.headers()} - and that standardization step can never
#' produce a dot (only underscores), so a dotted \code{required.headers}
#' entry could never match ANY real file's column this way, however that
#' file happens to spell it. \strong{Resolved 2026-09-28 - see the
#' "Follow-up, 2026-09-28" \code{@details} entry directly below: switching
#' the required-header check itself to the shared
#' \code{canonicalize.headers()} helper (which standardizes BOTH sides
#' before comparing, treating \code{"."} and \code{"_"} as equivalent)
#' removed this obstacle, so all three renames are now applied for real.}
#'
#' \strong{Follow-up, 2026-09-28, per Josh: dot/underscore equivalence in
#' required-header matching - real, real-file-affecting fix, unblocks the
#' three renames left pending in the 2026-09-27 entry directly above.}
#' Josh's instruction: "with the required headers, treat \".\" the same as
#' \"_\" when checking if the required headers are there." The
#' required-header check no longer does a bare
#' \code{setdiff(required.headers, names(aru.list))} after standardizing
#' only the incoming file's headers - it now calls the shared
#' \code{canonicalize.headers()} helper (\code{batz.util_standardize.headers.R},
#' already used elsewhere in this package), which runs
#' \code{standardize.headers()} on BOTH \code{required.headers} AND the
#' loaded file's own column names before comparing them, so a dot or an
#' underscore in either one matches the other (\code{"aru.name"} and
#' \code{"aru_name"} both standardize to \code{"aru_name"} and are treated
#' as the same header). Every matched column is then renamed, in this
#' function's own working copy only, to \code{required.headers}' own
#' spelling - so \code{required.headers} can now use this function's
#' normal dot-separated convention directly, and this closes the gap the
#' 2026-09-27 entry above flagged. Concretely, \code{required.headers} is
#' updated: \code{aru_name -> aru.name}, \code{date_start -> date.start},
#' \code{date_end -> date.end} (the other entries - \code{longitude},
#' \code{latitude}, \code{sunregion}, \code{sunregion_longitude},
#' \code{sunregion_latitude}, \code{time_zone}, \code{sunregion_type},
#' \code{schedual1}, \code{schedual2} - have no dots either way and are
#' unchanged at THIS point in the history - see the next entry for the
#' sunregion_longitude/sunregion_latitude follow-up later the same day).
#' \code{$date.start}/\code{$date.end} are also renamed
#' throughout this function's own internals and in \code{aru.suntimes}'s
#' own output (previously \code{$date_start}/\code{$date_end}) - see
#' \code{@return} below, updated to match. \strong{No real-file changes
#' are needed}: an existing \verb{*arulist.csv} with underscore-spelled
#' headers (\code{aru_name}, \code{date_start}, \code{date_end}) continues
#' to satisfy the required-header check exactly as before, since
#' \code{canonicalize.headers()} standardizes \code{required.headers}'
#' dots away for the comparison - this is purely a widening of what's
#' accepted, in both directions, never a narrowing. Full dev-script test
#' suite re-run clean after this change, plus a new explicit
#' dot/underscore-equivalence test case (see
#' \code{batz.generate_suntimes.arulist.dev.R}).
#'
#' \strong{Reference-workbook Change.to pass, 2026-09-28 (later the same
#' day), per Josh: "several $name.standard have $Change.to values that have
#' not been changed, make those changes now or flag why they can not be
#' made".} Of the reference workbook's pending renames touching this
#' function, \code{sunregion_latitude -> sunregion.latitude} and
#' \code{sunregion_longitude -> sunregion.longitude} are applied here, the
#' same way \code{date_start}/\code{date_end} were just above: both are
#' now spelled with a dot in \code{required.headers} and throughout this
#' function's internals/output, relying on the same
#' \code{canonicalize.headers()} dot/underscore equivalence fix (directly
#' above) to keep accepting an existing \verb{*arulist.csv}'s underscore-
#' spelled columns with no real-file changes needed - this is purely a
#' widening, exactly like the \code{date.start}/\code{date.end} case.
#' \strong{\code{sunregion_type -> sunregion.type} was NOT applied}, and is
#' flagged back to Josh instead: the reference workbook already has a
#' SEPARATE \code{Header.names} row named \code{sunregion.type}, used by
#' \code{\link{batz.plotdetections_first.last}}/
#' \code{\link{batz.plotactivity_observations}}, whose own description
#' explicitly says it is "distinct from raw sunregion_type" (i.e. from
#' THIS function's own column, on purpose). Applying this function's
#' pending \code{Change.to} as given would silently collide this
#' function's \code{$sunregion_type} with that already-distinct identifier
#' - please confirm whether that merge is actually wanted (and, if so,
#' whether the two other functions' own \code{sunregion.type} column
#' should also change), or whether this function's \code{Change.to} entry
#' should instead target a different, still-distinct dotted spelling.
#'
#' \strong{Merge, 2026-09-29, per Josh ("These are the same things" - directly
#' answering the question raised in the entry above): confirmed the two
#' \code{sunregion.type} identifiers ARE the same concept, not a real
#' collision.} \code{sunregion_type -> sunregion.type} is now applied.
#' Because this function already checks its required headers through
#' \code{canonicalize.headers()} (see "Follow-up, 2026-09-28" above), this is
#' a pure spelling change with no alias needed: \code{required.headers} now
#' reads \code{sunregion.type}, and every internal reference
#' (\code{aru.list$sunregion_type}, the \code{aggregate()} formula, the
#' \code{expand.one()} field, and \code{aru.suntimes}'s own output column) is
#' renamed to \code{sunregion.type} to match. \code{\link{batz.plotdetections_first.last}}/
#' \code{\link{batz.plotactivity_observations}} already used
#' \code{sunregion.type} and needed no change; the reference workbook's two
#' \code{Header.names} rows are merged into one (this function's
#' \code{functions.in} added to the existing \code{sunregion.type} row, the
#' separate \code{sunregion_type} row deleted). No real-file changes are
#' needed: an existing \verb{*arulist.csv} with an underscore-spelled
#' \code{sunregion_type} column continues to satisfy the required-header
#' check exactly as before. Full dev-script test suite re-run clean after
#' this change.
#'
#' @param dir.load Directory to search for files matching \code{load.pattern}.
#'   Default: current working directory. Must actually contain the
#'   \verb{*arulist.csv} file(s) - if no matching file is found, the
#'   function stops with a clear error rather than proceeding on an empty
#'   deployment list.
#' @param load.pattern Character, default \code{"*arulist.csv"}: the
#'   file-name suffix pattern (plain wildcard/glob style - \code{"*"} as a
#'   leading wildcard, everything else literal) that identifies the ARU
#'   deployment-list file(s) to load.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   every subdirectory of \code{dir.load}.
#' @param write.output If \code{TRUE} (default), also write
#'   \code{aru.suntimes.csv} into \code{dir.save} (with the \verb{*.unix}
#'   columns rounded to whole seconds in the written CSV only).
#' @param dir.save Directory to write the output CSV into when
#'   \code{write.output = TRUE}. Default: the current working directory
#'   (\code{getwd()}) - set this separately if the output should be
#'   written somewhere other than where the input \verb{*arulist.csv} was
#'   loaded from. (Standardized 2026-08-29, per Josh: previously defaulted
#'   to \code{dir.load}, which silently mirrored whatever \code{dir.load}
#'   was rather than defaulting to a sensible location on its own -
#'   flagged as a bug and fixed.)
#' @param project.name Character, default \code{""}: base name for the
#'   output CSV written when \code{write.output = TRUE}. Every output
#'   file name gets a filetype token, a date-range token, and a save-time
#'   stamp appended automatically, so no two runs ever silently overwrite
#'   each other: \verb{<base>_suntimes_<DATE1>to<DATE2>_<timestamp>.csv},
#'   where \code{DATE1}/\code{DATE2} are the earliest/latest \code{$date}
#'   in the output (\code{YYYYMMDD}) and \code{<timestamp>} is when the
#'   file was written (\code{YYYYMMDDHHMMSS}, e.g. \code{20260826114426}).
#'   \strong{Reordered and the \code{"sav"} prefix dropped (round
#'   twenty-two, 2026-09-24, per Josh) - see @details, "File naming (round
#'   twenty-two)"; the previous shape was \verb{<base>_<DATE1>to<DATE2>_
#'   sav<timestamp>_suntimes.csv}.} \code{""} (default) uses \code{"aru"}
#'   as \verb{<base>}. Any other value is used as \verb{<base>} instead -
#'   e.g. \code{project.name = "projectname"} produces something like
#'   \code{"projectname_suntimes_20250101to20250202_20260924113700.csv"}.
#'   If the given value already ends in a previously-auto-generated
#'   NEW-format suffix (e.g. you passed a prior run's output file name
#'   back in), that suffix is stripped back off first, so the file gets a
#'   fresh stamp instead of a second one stacked on top; a name saved
#'   under the OLD (pre-round-twenty-two) format is not recognized by this
#'   stripping step - see @details.
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-22, per
#'   Josh's request to audit and extend the snake_case output option
#'   package-wide (see \code{\link{batz.generate_plotframe.bat}}, the first
#'   function this was added to). Controls only \code{aru.suntimes}'s OWN
#'   output column names (and, when \code{write.output = TRUE}, the CSV
#'   written to disk), applied as the very last step before each is
#'   written/returned. \code{FALSE} (default) keeps this function's normal
#'   column names exactly as always. \code{TRUE} runs every output column
#'   name through \code{standardize.headers()} instead (e.g.
#'   \code{$sunregion.longitude} becomes \code{$sunregion_longitude},
#'   \code{$date.monitoringnight}
#'   becomes \code{$date_monitoringnight}, \code{$sunrise.monitoringnight.unix}
#'   becomes \code{$sunrise_monitoringnight_unix}, \code{$aru.name} becomes
#'   \code{$aru_name}) - for a caller who specifically wants a snake_case
#'   CSV/data frame out of this function, without having to convert it
#'   themselves afterward.
#'
#' @return Invisibly, a list with:
#'   \describe{
#'     \item{aru.suntimes}{One row per (aru, date), \code{"fixed.unique"}/
#'       \code{"fixed.pooled"} rows only: \code{$aru.name}, \code{$date},
#'       \code{$date.monitoringnight}, \code{$sunregion}, \code{$sunregion.longitude},
#'       \code{$sunregion.latitude}, \code{$date.start}, \code{$date.end},
#'       \code{$time_zone}, \code{$sunregion_type}, \code{$schedual1},
#'       \code{$schedual2}, \code{$latitude}, \code{$longitude}, \code{$sunset},
#'       \code{$sunset.unix}, \code{$sunrise}, \code{$sunrise.unix},
#'       \code{$sunrise.monitoringnight}, \code{$sunrise.monitoringnight.unix}
#'       (or their snake_case equivalents if \code{snake_case = TRUE} - see
#'       that parameter above). \strong{\code{$aru.name} was renamed from bare
#'       \code{$aru} 2026-09-22, per Josh - see @details, "Follow-up,
#'       2026-09-22...aru as an header". \code{$latitude}/\code{$longitude}, and
#'       \code{$sunset}/\code{$sunset.unix}/\code{$sunrise}/\code{$sunrise.unix}/
#'       \code{$sunrise.monitoringnight}/\code{$sunrise.monitoringnight.unix}
#'       were renamed from
#'       \code{$lat}/\code{$long}, and \code{$suns}/\code{$suns.unix}/
#'       \code{$sunr}/\code{$sunr.unix}/\code{$sunr.mon}/\code{$sunr.mon.unix}
#'       respectively, 2026-09-25 (round twenty-six) - see @details, "Round
#'       twenty-six". \code{$date.start}/\code{$date.end} were renamed from
#'       \code{$date_start}/\code{$date_end} 2026-09-28 - see @details,
#'       "Follow-up, 2026-09-28". \code{$sunregion.longitude}/
#'       \code{$sunregion.latitude} were renamed from
#'       \code{$sunregion_longitude}/\code{$sunregion_latitude} 2026-09-28
#'       (later the same day) - see @details, "Reference-workbook Change.to
#'       pass".}}
#'     \item{efficiency}{One-row summary: \code{$aru.date.rows} (rows needed
#'       without de-duplication), \code{$site.date_rows.count} (unique
#'       site-date rows actually calculated), \code{$shared.sites_count}
#'       (count of sites with more than one ARU sharing the same
#'       calculation site). \strong{\code{$site.date_rows.count}/
#'       \code{$shared.sites_count} were renamed from \code{$site.date.rows}/
#'       \code{$shared.sites} 2026-09-27, per Josh's reference-workbook
#'       "Change.to" column - see @details.} Not affected by
#'       \code{snake_case} - see that parameter above.}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- batz.generate_suntimes.arulist(dir.load = "path/to/data")
#' result$aru.suntimes
#' result$efficiency
#'
#' # snake_case output headers instead of this function's usual dot-style
#' result <- batz.generate_suntimes.arulist(dir.load = "path/to/data", snake_case = TRUE)
#' }
#'
#' @export
batz.generate_suntimes.arulist <- function(dir.load = getwd(),
                                    load.pattern = "*arulist.csv",
                                    dir.sub = FALSE,
                                    write.output = TRUE,
                                    dir.save = getwd(),
                                    project.name = "",
                                    snake_case = FALSE) {

  ## convert a plain wildcard/glob suffix pattern (or vector of them) into
  ## one combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  ## ---- internal helpers: solar calculation (base R port of the standard
  ## astronomy-answers.nl / NOAA sunrise-sunset algorithm) -------------------
  rad          <- pi / 180
  day.ms       <- 86400 * 1000
  J1970        <- 2440588
  J2000        <- 2451545
  e.obliquity  <- rad * 23.4397
  J0           <- 0.0009

  to.julian <- function(date.posix.utc) {
    as.numeric(date.posix.utc) * 1000 / day.ms - 0.5 + J1970
  }
  from.julian <- function(j) {
    as.POSIXct((j + 0.5 - J1970) * day.ms / 1000, origin = "1970-01-01", tz = "UTC")
  }
  to.days <- function(date.posix.utc) to.julian(date.posix.utc) - J2000

  declination <- function(l, b) {
    asin(sin(b) * cos(e.obliquity) + cos(b) * sin(e.obliquity) * sin(l))
  }
  solar.mean.anomaly <- function(d) rad * (357.5291 + 0.98560028 * d)
  ecliptic.longitude <- function(M) {
    C <- rad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M))
    P <- rad * 102.9372
    M + C + P + pi
  }
  julian.cycle    <- function(d, lw) round(d - J0 - lw / (2 * pi))
  approx.transit  <- function(Ht, lw, n) J0 + (Ht + lw) / (2 * pi) + n
  solar.transit.j <- function(ds, M, L) J2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L)
  hour.angle      <- function(h, phi, d) acos((sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d)))

  get.set.j <- function(h, lw, phi, dec, n, M, L) {
    w <- hour.angle(h, phi, dec)
    a <- approx.transit(w, lw, n)
    solar.transit.j(a, M, L)
  }

  ## Vectorized sunrise/sunset (UTC POSIXct) for parallel date/lat/lon
  ## vectors. Anchored at UTC NOON (not UTC midnight) of the given calendar
  ## date - anchoring at midnight can round the algorithm's internal
  ## "julian cycle" integer to the PREVIOUS day for any location west of
  ## Greenwich, shifting every result a full day early; noon keeps the
  ## rounding correct for every longitude from -180 to +180.
  calc.suntimes <- function(date, lat, lon) {
    date.utc.anchor <- as.POSIXct(paste(date, "12:00:00"), tz = "UTC")
    lw  <- rad * -lon
    phi <- rad * lat
    d   <- to.days(date.utc.anchor)
    n   <- julian.cycle(d, lw)
    ds  <- approx.transit(0, lw, n)
    M   <- solar.mean.anomaly(ds)
    L   <- ecliptic.longitude(M)
    dec <- declination(L, 0)
    Jnoon <- solar.transit.j(ds, M, L)

    h0 <- -0.833 * rad
    suppressWarnings({
      Jset <- get.set.j(h0, lw, phi, dec, n, M, L)
    })
    Jrise <- Jnoon - (Jset - Jnoon)

    data.frame(sunrise = from.julian(Jrise), sunset = from.julian(Jset))
  }

  ## format a vector of UTC POSIXct instants as local wall-clock strings,
  ## grouped by time zone (a POSIXct vector can only carry one tzone
  ## attribute at a time, so this keeps mixed time zones correct)
  format.local <- function(instant.utc, tz) {
    out <- character(length(instant.utc))
    for (this.tz in unique(tz)) {
      idx <- which(tz == this.tz)
      out[idx] <- format(instant.utc[idx], tz = this.tz, usetz = FALSE)
    }
    out
  }

  parse.simple.date <- function(x) {
    x <- trimws(x)
    out <- as.Date(rep(NA_character_, length(x)))
    fmts <- c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y")
    for (fmt in fmts) {
      still.na <- is.na(out) & nzchar(x)
      if (!any(still.na)) break
      parsed <- as.Date(x, format = fmt)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  ## ===========================================================================
  ## load ARU deployment list(s)
  ## ===========================================================================
  aru.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE)

  ## fail loudly and clearly here instead of letting a zero-file match
  ## cascade into a malformed aru.list (a plain, non-data.frame list
  ## produced by `NULL$field <- value`) whose nrow() misbehaves several
  ## steps later, several steps away from the real cause.
  if (length(aru.files) == 0) {
    stop("No files matching load.pattern (\"", paste(load.pattern, collapse = "\", \""),
         "\") were found in dir.load (\"", dir.load, "\"). Check that dir.load points ",
         "to the folder containing your *arulist.csv file.")
  }

  aru.list <- do.call(rbind, lapply(aru.files, read.csv,
                                     stringsAsFactors = FALSE,
                                     colClasses = "character"))

  ## header standardization (per Josh, 2026-09-14 project preference): the
  ## raw *arulist.csv's own headers are standardized (trim, collapse
  ## non-alphanumeric runs to a single underscore, lowercase) before the
  ## required-header check below runs - see @details "Header
  ## standardization" above for why this cascades into required.headers
  ## and this function's own returned column names.
  names(aru.list) <- standardize.headers(names(aru.list))

  ## ===========================================================================
  ## required-header check (added 2026-08-26, per Josh). Message text is
  ## Josh's own, used verbatim. Round twenty-six (2026-09-25, per Josh): the
  ## raw *arulist.csv itself must have columns that standardize to
  ## "aru_name"/"longitude"/"latitude"/"sunregion_longitude"/
  ## "sunregion_latitude" - a real change from the old "aru"/"long"/"lat"/
  ## "sunregion_long"/"sunregion_lat" spellings (unlike the 2026-09-22
  ## $aru->$aru.name follow-up, which was internal/output-only and left the
  ## raw file's own required spelling as bare "aru") - see @details, "Round
  ## twenty-six".
  ##
  ## Follow-up, 2026-09-28, per Josh ("treat \".\" the same as \"_\" when
  ## checking if the required headers are there"): this check now goes
  ## through the shared canonicalize.headers() helper
  ## (batz.util_standardize.headers.R) instead of a plain setdiff() against
  ## already-standardized names - canonicalize.headers() standardizes BOTH
  ## required.headers AND aru.list's own names before comparing (so a dot
  ## or an underscore in either one is treated as equivalent), then renames
  ## every matched column to required.headers' own spelling. This is what
  ## lets required.headers below use this function's normal dot-separated
  ## convention (aru.name/date.start/date.end/sunregion.longitude/
  ## sunregion.latitude) directly, without breaking real-file compatibility
  ## - see @details, "Follow-up, 2026-09-28" and "Reference-workbook
  ## Change.to pass".
  ## ===========================================================================
  required.headers <- c("aru.name", "longitude", "latitude", "sunregion", "sunregion.longitude",
                         "sunregion.latitude", "date.start", "date.end", "time_zone",
                         "sunregion.type", "schedual1", "schedual2")
  canon <- canonicalize.headers(aru.list, required.headers)
  if (length(canon$missing) > 0) {
    stop("inputfile is missing these headers: ", paste(canon$missing, collapse = ", "))
  }
  aru.list <- canon$df

  aru.list$latitude  <- as.numeric(aru.list$latitude)
  aru.list$longitude <- as.numeric(aru.list$longitude)
  aru.list$sunregion.longitude <- as.numeric(aru.list$sunregion.longitude)
  aru.list$sunregion.latitude  <- as.numeric(aru.list$sunregion.latitude)
  aru.list$date.start <- parse.simple.date(aru.list$date.start)
  aru.list$date.end   <- parse.simple.date(aru.list$date.end)

  ## ===========================================================================
  ## $sunregion.type (renamed from $sunregion_type, 2026-09-29 - see @details,
  ## "Merge, 2026-09-29"). Required (enforced by the header check above), so
  ## no more default-when-absent fallback. Only "fixed.unique"/"fixed.pooled"
  ## rows are kept; every other value (mobile.*, or a typo) is EXCLUDED with a
  ## NOTE instead of stopping the whole run - a real behavior change from the
  ## previous version, which hard-stopped on any mobile.* row.
  ## ===========================================================================
  aru.list$sunregion.type <- trimws(tolower(aru.list$sunregion.type))

  allowed.types <- c("fixed.unique", "fixed.pooled")
  keep.rows <- aru.list$sunregion.type %in% allowed.types
  if (any(!keep.rows)) {
    excluded <- aru.list[!keep.rows, ]
    cat("NOTE:", nrow(excluded), "row(s) excluded - $sunregion.type is not",
        "\"fixed.unique\"/\"fixed.pooled\":",
        paste(unique(paste0(excluded$aru.name, " (", excluded$sunregion.type, ")")), collapse = ", "),
        "\n\n")
  }
  aru.list <- aru.list[keep.rows, , drop = FALSE]
  if (nrow(aru.list) == 0) {
    stop("No rows remain after filtering to $sunregion.type \"fixed.unique\"/",
         "\"fixed.pooled\" - nothing to generate.")
  }

  is.fixed.unique <- aru.list$sunregion.type == "fixed.unique"
  mismatched.unique <- is.fixed.unique & (aru.list$sunregion != aru.list$aru.name)
  if (any(mismatched.unique)) {
    cat("NOTE:", sum(mismatched.unique), "row(s) marked \"fixed.unique\" have",
        "$sunregion != $aru.name (spec says these should match for this type):",
        paste(aru.list$aru.name[mismatched.unique], collapse = ", "), "\n\n")
  }

  type.per.region <- aggregate(sunregion.type ~ sunregion, data = aru.list,
                                FUN = function(x) length(unique(x)))
  mixed.regions <- type.per.region$sunregion[type.per.region$sunregion.type > 1]
  if (length(mixed.regions) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion.type across their ARUs:",
        paste(mixed.regions, collapse = ", "), "\n\n")
  }

  ## light data-entry sanity check (not requested, added to match the
  ## $sunregion.type-consistency NOTE above): flag a $sunregion whose
  ## $sunregion.longitude/$sunregion.latitude aren't identical across every
  ## row that shares it - not enforced/blocking, since the calculation
  ## below uses each row's own value directly (no averaging happens
  ## anymore)
  coord.per.region <- aggregate(cbind(n.long = sunregion.longitude, n.lat = sunregion.latitude) ~ sunregion,
                                 data = aru.list, FUN = function(x) length(unique(x)))
  mixed.coords <- coord.per.region$sunregion[coord.per.region$n.long > 1 | coord.per.region$n.lat > 1]
  if (length(mixed.coords) > 0) {
    cat("NOTE: sunregion(s) with inconsistent $sunregion.longitude/$sunregion.latitude across their ARUs:",
        paste(mixed.coords, collapse = ", "), "\n\n")
  }

  ## resolve the lat/long actually used for the solar calculation (updated
  ## 2026-08-26, per Josh): $sunregion.longitude/$sunregion.latitude are now
  ## used DIRECTLY for every kept row (both fixed.unique and fixed.pooled) -
  ## see @details above for the behavior change from the previous
  ## exact-ARU-coords/computed-mean split. (Purely-internal calc.lat/
  ## calc.long variable names left unchanged - see @details, "Round
  ## twenty-six".)
  aru.list$calc.lat  <- aru.list$sunregion.latitude
  aru.list$calc.long <- aru.list$sunregion.longitude

  ## ===========================================================================
  ## expand each ARU row to one row per date in its range
  ## ===========================================================================
  expand.one <- function(i) {
    row <- aru.list[i, ]
    dates <- seq(row$date.start, row$date.end, by = "day")
    data.frame(
      aru.name = row$aru.name, sunregion = row$sunregion,
      sunregion.type = row$sunregion.type,
      latitude = row$latitude, longitude = row$longitude,
      sunregion.longitude = row$sunregion.longitude, sunregion.latitude = row$sunregion.latitude,
      calc.lat = row$calc.lat, calc.long = row$calc.long,
      date.start = row$date.start, date.end = row$date.end,
      schedual1 = row$schedual1, schedual2 = row$schedual2,
      time_zone = row$time_zone, date = dates,
      stringsAsFactors = FALSE
    )
  }
  aru.expand <- do.call(rbind, lapply(seq_len(nrow(aru.list)), expand.one))

  ## ===========================================================================
  ## efficiency step - collapse to the distinct (sunregion, calc.lat,
  ## calc.long, time_zone, date) combinations actually needed. ARUs sharing
  ## a site with identical or overlapping date ranges collapse onto the
  ## same rows here, so the solar calculation runs once per unique
  ## site-date rather than once per aru-date.
  ## ===========================================================================
  site.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  site.dates <- aru.expand[!duplicated(site.key),
                            c("sunregion", "calc.lat", "calc.long", "time_zone", "date")]
  row.names(site.dates) <- NULL

  site.group.key <- with(aru.list, paste(sunregion, calc.lat, calc.long, sep = "|||"))
  arus.per.site  <- table(site.group.key)
  n.shared.sites <- sum(arus.per.site > 1)

  ## next-day look-ahead needed for $sunrise.monitoringnight
  lookahead <- site.dates
  lookahead$date <- lookahead$date + 1
  calc.key      <- with(site.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  lookahead.key <- with(lookahead,  paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  extra.needed  <- lookahead[!lookahead.key %in% calc.key & !duplicated(lookahead.key), ]

  calc.dates <- rbind(site.dates, extra.needed)
  row.names(calc.dates) <- NULL

  ## ===========================================================================
  ## run the solar calculation once per unique site-date
  ## ===========================================================================
  suns <- calc.suntimes(calc.dates$date, calc.dates$calc.lat, calc.dates$calc.long)
  calc.dates$sunrise.utc <- suns$sunrise
  calc.dates$sunset.utc  <- suns$sunset
  calc.dates$calc.key <- with(calc.dates, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))

  ## ===========================================================================
  ## join results back onto every (aru, date) row
  ## ===========================================================================
  aru.expand$calc.key <- with(aru.expand, paste(sunregion, calc.lat, calc.long, date, sep = "|||"))
  aru.expand$next.day.key <- with(aru.expand,
                                   paste(sunregion, calc.lat, calc.long, date + 1, sep = "|||"))

  lookup <- calc.dates[, c("calc.key", "sunrise.utc", "sunset.utc")]
  today  <- lookup[match(aru.expand$calc.key, lookup$calc.key), ]
  nextd  <- lookup[match(aru.expand$next.day.key, lookup$calc.key), ]

  aru.suntimes <- data.frame(
    aru.name             = aru.expand$aru.name,
    date                 = aru.expand$date,
    date.monitoringnight = as.POSIXct(paste(aru.expand$date, "12:00:00")),
    sunregion            = aru.expand$sunregion,
    sunregion.longitude  = aru.expand$sunregion.longitude,
    sunregion.latitude   = aru.expand$sunregion.latitude,
    date.start           = aru.expand$date.start,
    date.end             = aru.expand$date.end,
    time_zone            = aru.expand$time_zone,
    sunregion.type       = aru.expand$sunregion.type,
    schedual1            = aru.expand$schedual1,
    schedual2            = aru.expand$schedual2,
    latitude             = aru.expand$latitude,
    longitude            = aru.expand$longitude,
    stringsAsFactors = FALSE
  )
  aru.suntimes$sunset                       <- format.local(today$sunset.utc, aru.expand$time_zone)
  aru.suntimes$sunset.unix                  <- as.numeric(today$sunset.utc)
  aru.suntimes$sunrise                      <- format.local(today$sunrise.utc, aru.expand$time_zone)
  aru.suntimes$sunrise.unix                 <- as.numeric(today$sunrise.utc)
  aru.suntimes$sunrise.monitoringnight      <- format.local(nextd$sunrise.utc, aru.expand$time_zone)
  aru.suntimes$sunrise.monitoringnight.unix <- as.numeric(nextd$sunrise.utc)

  efficiency <- data.frame(
    aru.date.rows      = nrow(aru.expand),
    site.date_rows.count = nrow(site.dates),
    shared.sites_count = n.shared.sites
  )

  if (write.output) {
    # rounding is applied only here, at the final save step - never to
    # intermediate values or to the object returned to R (below)
    aru.suntimes.out <- aru.suntimes
    aru.suntimes.out$sunset.unix                  <- round(aru.suntimes.out$sunset.unix)
    aru.suntimes.out$sunrise.unix                 <- round(aru.suntimes.out$sunrise.unix)
    aru.suntimes.out$sunrise.monitoringnight.unix <- round(aru.suntimes.out$sunrise.monitoringnight.unix)
    # ---- resolve the output file names ----------------------------------
    # <base>_suntimes_<DATE1>to<DATE2>_<timestamp>.csv - see @param
    # project.name above. Round twenty-two, 2026-09-24, per Josh: the
    # filetype token ("suntimes") moved to second position and the "sav"
    # prefix in front of the timestamp was dropped - see @details, "File
    # naming (round twenty-two)". Strip any previously-auto-generated
    # NEW-format suffix off a user-given `project.name` first, so feeding
    # a prior stamped output file name back in re-stamps rather than
    # stacking a second stamp on top of the first.
    strip.autoname <- function(x) {
      x <- sub("\\.csv$", "", x, ignore.case = TRUE)
      x <- sub("_[0-9]{14}$", "", x, ignore.case = TRUE)
      x <- sub("_[0-9]{8}to[0-9]{8}$", "", x, ignore.case = TRUE)
      x <- sub("_suntimes$", "", x, ignore.case = TRUE)
      x
    }
    base.name <- if (identical(project.name, "")) "aru" else strip.autoname(project.name)

    date1     <- format(min(aru.suntimes$date), "%Y%m%d")
    date2     <- format(max(aru.suntimes$date), "%Y%m%d")

    out.file <- paste0(base.name, "_suntimes_", date1, "to", date2, "_",
                        format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")

    ## snake_case (per Josh, 2026-09-22, project-wide audit/extension of the
    ## snake_case output option - see @details) is applied here, to a copy
    ## used only for the CSV write, AFTER the rounding above (which still
    ## references this function's own dot-separated $sunset.unix/
    ## $sunrise.unix/$sunrise.monitoringnight.unix names regardless of this
    ## parameter).
    if (snake_case) names(aru.suntimes.out) <- standardize.headers(names(aru.suntimes.out))

    write.csv(aru.suntimes.out, file.path(dir.save, out.file), row.names = FALSE)
  }

  ## snake_case applied to the invisibly-returned aru.suntimes itself, as
  ## the very last step before it's returned (kept separate from the
  ## write.output copy above, since that copy is also rounded/renamed on
  ## its own timeline) - see @details, "Follow-up, 2026-09-22".
  if (snake_case) names(aru.suntimes) <- standardize.headers(names(aru.suntimes))

  invisible(list(aru.suntimes = aru.suntimes, efficiency = efficiency))
}

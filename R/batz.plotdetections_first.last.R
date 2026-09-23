#' Plot each species' earliest and latest nightly detections
#'
#' Generates the standard report plot showing, for every species (plus an
#' "All detections" panel and an optional overlaid 40kHzMyo indicator), the
#' earliest-to-latest detection window for each monitoring night, with
#' Dawn/Dusk/Midnight reference lines. One plot is produced per row of
#' \code{aru.metadata.db} whose \code{$plot.type} is \code{"bat.detection"}.
#'
#' @param data A data frame of already-summarized per-species,
#'   per-night detection windows. Must have \code{$spp.id}, \code{$date},
#'   \code{$group}, \code{$obs}, \code{$mins2.noon.min},
#'   \code{$mins2.noon.max}, \code{$vetting.type}.
#' @param fig.list A data frame listing the plot(s) to generate - one
#'   row per plot. Must have \code{$plot.type}, \code{$plot.name},
#'   \code{$facet}, \code{$facet.set}, \code{$MYSO}, \code{$Alldect},
#'   \code{$facet.panel}, \code{$40khzmyo}, \code{$facet.label},
#'   \code{$plot.set}, \code{$date.format}, \code{$date.start},
#'   \code{$date.end}, \code{$xaxe.interval}, \code{$xaxe.title}. Column
#'   names must be unique (see Details for a real duplicate-header bug this
#'   catches). An optional \code{$midnight} column (\code{"none"}/
#'   \code{"long"}/\code{"short"}) overrides
#'   \code{aes.default}'s \code{midnight} setting for this one
#'   plot row - see Details.
#' @param suntimes A data frame of sunrise/sunset times, e.g. the output
#'   of \code{batz.generate_suntimes.arulist()}. Must have \code{$aru.name}, \code{$date},
#'   \code{$date.mon}, \code{$sunregion}, \code{$time.zone},
#'   \code{$sunregion.type}, \code{$schedual1}, \code{$schedual2},
#'   \code{$suns}, \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'   \code{$sunr.mon}, \code{$sunr.mon.unix}.
#' @param aes.default A data frame of default plot settings, one
#'   row per parameter (e.g. \code{plotopts_first.last.csv}).
#'   Must have \code{$category}, \code{$parameter}, \code{$default.value};
#'   an \code{$overide.value} column (blank, user-fillable) and
#'   \code{$notes} are optional - see \code{project.name}/\code{aes.style}
#'   below and Details, "Settings resolution (round nineteen)". Must be
#'   the TIME-OF-DAY version of this file (see Details) - an older,
#'   numeric-minutes version will fail with a clear error rather than
#'   silently plotting the wrong axis.
#' @param project.name Character, default \code{"new.project"}. Per Josh's
#'   nineteenth follow-up (2026-09-16), this NO LONGER selects an
#'   \code{aes.default} override column - it is now used ONLY to build the
#'   first part of every saved file name:
#'   \code{"<project.name>_<ARU>_<timestamp>.png"}. See Details, "Settings
#'   resolution (round nineteen)" for what replaced the old column-matching
#'   behavior.
#' @param aes.style Character, default \code{"overide.value"}. Names which
#'   column of \code{aes.default} is checked FIRST for each parameter
#'   (falling back to \code{$default.value} when that column doesn't exist,
#'   or is blank for that row) - replaces the old \code{project.name}
#'   column-matching mechanism entirely. A given plot row's OWN value in
#'   \code{fig.list} (when that column exists there and is non-blank)
#'   still takes priority over both - see Details, "Settings resolution
#'   (round nineteen)" for the full precedence.
#' @param dir.save Character, default \code{getwd()}. Directory every
#'   generated PNG is saved into. Per Josh's nineteenth follow-up, the file
#'   name itself is now always \code{"<project.name>_<ARU>_<timestamp>.png"}
#'   (see \code{project.name} above) - \code{aes.default}'s
#'   \code{$output.filename.pattern} is DEPRECATED and no longer read.
#'
#' @return Invisibly, a list with \code{plots} (one entry per generated
#'   plot's prepared data - detection rows, suntimes rows, panel labels,
#'   resolved settings) and \code{ggplots} (the corresponding ggplot objects,
#'   only populated when the \code{ggplot2} package is available - see
#'   Details).
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference) -
#' does not mechanically apply to this function, flagged not silently
#' skipped.} The project-wide preference is that headers coming from a
#' loaded file or an externally-supplied data frame are run through the
#' shared package helper \code{standardize.headers()} (trim whitespace,
#' collapse non-alphanumeric runs to underscores, lowercase). This function
#' does not load any file itself, and all four of its arguments
#' (\code{data}, \code{fig.list}, \code{suntimes}, \code{aes.default}) are
#' already-loaded data frames handed in by the caller - \code{data} in
#' particular is expected to be the output of the upstream \code{batz}
#' summarizing function. \code{DATA.REQUIRED}/\code{SUNTIMES.REQUIRED}/
#' \code{FIG.LIST.REQUIRED}/\code{AES.DEFAULT.REQUIRED}/
#' \code{AES.DEFAULT.REQUIRED.PARAMETERS} in this function's code are its
#' own hardcoded interface contracts with those upstream functions'/files'
#' already-established output schemas (e.g. \code{$spp.id}/\code{$date}/
#' \code{$group}/\code{$obs}), not raw text copied from a loaded
#' file's real header row, so there is no raw-header step here for the
#' preference to attach to and none of these names were renamed. If
#' \code{fig.list}/\code{aes.default}/\code{suntimes} are ever built by
#' reading a CSV/spreadsheet directly (rather than being handed to this
#' function pre-loaded), that loading step - wherever it lives - should
#' run \code{standardize.headers()} on its own raw headers, and this
#' function's own required-header constants would need to be written to
#' match those standardized spellings; no such loading step exists inside
#' this function itself. This mirrors the same reasoning already applied
#' to \code{\link{batz.plotactivity_observations}} - see that function's
#' own \code{@details} for the identical analysis.
#'
#' \strong{BUGFIX (2026-09-21, per Josh's real-world error report of a
#' header mismatch): two separate, real problems, both fixed here.}
#' (1) \code{DATA.REQUIRED} still said \code{"aru.groupby"}, a name
#' \code{\link{batz.generate_plotframe.bat}} stopped producing on
#' 2026-08-28, when that function's own equivalent output column was
#' renamed to \code{$group} (see that function's own \code{@details},
#' "$group vs $groupedby") - this function's own required-header constant
#' and its one internal reference (\code{pd$aru.groupby}, used to filter
#' \code{data} down to a single \code{$plot.set}) were simply never updated
#' to match at the time, so passing this function \code{batz.generate_plotframe.bat}'s
#' real current output always failed the header check on that one column
#' name specifically, independent of anything else. Both now say
#' \code{"group"}/\code{pd$group}. (2) More generally, every one of
#' \code{data}/\code{suntimes}/\code{fig.list}/\code{aes.default}'s headers
#' is now matched via the shared package helper
#' \code{\link{canonicalize.headers}} rather than a plain \code{setdiff()}:
#' both sides are standardized to snake_case purely to find matching
#' columns (so a data frame is accepted whether its real column names are
#' already this function's own dot-separated style, e.g. \code{"date.mon"},
#' or have come back snake_cased from some intervening save/reload step,
#' e.g. \code{"date_mon"}), then every matched column is renamed, in this
#' function's own local working copies only, to the exact dot-separated
#' spelling this function's code already expects - so nothing below the
#' header check needed to change. This never mutates the caller's own
#' \code{data}/\code{suntimes}/\code{fig.list}/\code{aes.default} objects
#' (R already copies a data frame argument on modification) and this
#' function has no data-frame/CSV/xlsx output of its own to apply a
#' \code{snake_case=} option to (it only saves PNGs and returns ggplot
#' objects) - see \code{\link{batz.generate_plotframe.bat}} for that option
#' where it does apply. The missing-header message for MULTIPLE data
#' frames in one call is now also separated by a blank line (previously a
#' single \code{"\\n"}) for readability when more than one input is missing
#' headers at once.
#'
#' \strong{Iteration 1 ("basic layout") - per Josh's own framing that this
#' function would be built iteratively.} This covers: header validation
#' (including a duplicate-column-name check), settings resolution
#' (\code{aru.metadata.db} row > \code{project.name} column >
#' \code{$default.value}), the \code{$spp.plot}/\code{$facpan}
#' New-England-special-case + \code{$MYSO}/\code{$Alldect}/\code{$40khzmyo}
#' panel-building logic, facet labeling via
#' \code{batz.batusa_recode.names()}, and the crossbar/reference-line plot
#' itself. NOT yet implemented (deferred to a later iteration): any
#' \code{$facet} value besides \code{"sppid"}, any \code{$plot.type} besides
#' \code{"bat.detection"}, and a \code{dir.save}-style output-location
#' argument (plots currently save to the working directory).
#'
#' \strong{The ggplot2-rendering code has now been executed and the plot
#' verified.} \code{ggplot2} was successfully installed in this sandbox (via
#' the Debian \code{r-cran-ggplot2} package, since CRAN's network install
#' path is unavailable here) and the function was run end to end against a
#' synthetic, ARU/date-aligned copy of the real test data (the real files as
#' Josh has them don't overlap in date range - see the duplicate-column and
#' ARU/date-misalignment notes elsewhere in this section - so a synthetic
#' aligned copy was used specifically to exercise the plotting code). The
#' resulting rendered plot closely matches the target report image: 9 facet
#' panels, gray "All detections" crossbars, blue dashed Dawn / red dashed
#' Dusk / black solid Midnight reference lines, and a Noon-to-Midnight Y
#' axis with custom labels.
#'
#' \strong{Two real bugs were found and fixed specifically because of this
#' live rendering} (neither was visible from base-R testing alone, since
#' both only manifested as a rendering warning or a visibly wrong axis):
#' (1) the Dawn reference line was computed from \code{sdb$sunr} - sunrise
#' ON \code{$date}, i.e. the dawn ending the PREVIOUS night - rather than
#' \code{sdb$sunr.mon} - sunrise on the FOLLOWING day, the dawn actually
#' ending the monitoring night that starts at \code{$date}'s dusk. This
#' placed every Dawn line hours before the plotted Noon-to-Noon window,
#' silently dropping it from every panel (visible as a
#' \code{Removed N rows containing missing values} warning from
#' \code{geom_line()}); fixed by switching to \code{sdb$sunr.mon}. (2) The
#' custom Noon/Midnight Y-axis break labels (\code{y.breaks}/
#' \code{y.break.labels}, passed to \code{scale_y_datetime(breaks=,
#' labels=)}) were previously deferred as untested; they are now computed
#' and verified against the actual rendered axis.
#'
#' \strong{Two more real bugs were found and fixed after Josh reviewed the
#' first render against his own target image:} (1) \code{$date.format}
#' (e.g. \code{"\%b-\%d/n\%Y"}) is meant to break the X-axis date label onto
#' two lines, but a literal \code{/n} (forward-slash-n) is not a newline
#' escape that \code{strftime}-style date formatting recognizes, so it was
#' rendering as the literal two characters \code{/n} instead of a line
#' break. Fixed by converting any literal \code{/n} in \code{$date.format}
#' to an actual newline before it reaches \code{scale_x_date()}, rather
#' than relying on every future \code{plot.meta.csv} spelling it correctly.
#' (2) The X axis had no explicit range - it showed whatever dates
#' happened to have data, not the full \code{$date.start}-\code{$date.end}
#' window (misleading whenever detections don't span the whole requested
#' range, and NOT what Josh's own target image does - it always shows the
#' full window). Fixed by passing \code{limits = c(date.start, date.end)}
#' (with a half-day pad, and an explicit \code{geom_crossbar(width = 0.9)},
#' to avoid clipping the boundary days' detection bars - a real
#' \code{ggplot2} out-of-bounds/box-width interaction found while fixing
#' this) to \code{scale_x_date()}.
#'
#' \strong{A real bug was found in the aru.metadata.db test file
#' (plot.meta.csv) while building this}: it has TWO columns both named
#' \code{"xaxe.title"} - the second one's value ("Hour of mointoring") reads
#' like it was meant to be a Y-AXIS title override, i.e. the column should
#' be named \code{"yaxe.title"}. This function explicitly detects and stops
#' on any duplicate column name in \code{aru.metadata.db} rather than
#' silently using one of the two - rename the second occurrence before
#' running this against that file.
#'
#' \strong{The Y axis is one shared Noon-to-Noon window for every night},
#' regardless of each detection's or sun-time's real calendar date - the
#' real date drives the X axis (via faceting/\code{$date}) only. Every
#' \code{$mins2.noon.min}/\code{$mins2.noon.max}/sunrise/sunset value is
#' remapped onto one fixed internal reference date before plotting, purely
#' as a shared axis anchor - not a data change.
#'
#' \strong{Every panel in the resolved \code{$facpan} list is always shown},
#' even with zero matching detections that period (an empty panel, showing
#' just the reference lines) - matching the real target report image, which
#' shows exactly this for species with nothing detected in a given window.
#'
#' \strong{"Tricolored bat" (Josh's literal New-England-special-case spec
#' text) was corrected to "Tri-colored bat"} to match
#' \code{batz.batusa_recode.names()}'s actual reference-table spelling (with
#' a hyphen) - the un-hyphenated spelling doesn't match anything in that
#' reference table (there's no separator in "Tricolored" for the matching
#' logic to fold away), so using it as given would have silently produced a
#' panel that never lines up with real data. \code{$spp.plot}/\code{$facpan}
#' (from any source - the New England special case or
#' \code{default.plotaesthetics}) are always canonicalized through
#' \code{batz.batusa_recode.names()} before being used to filter/label data,
#' specifically to catch this class of mismatch generally, not just this
#' one case.
#'
#' \strong{Follow-up, per Josh's review of the second render:} (1) the
#' Midnight reference line's linewidth now always matches the facet
#' panel's own border box linewidth (both driven from the same
#' \code{default.plotaesthetics} setting, \code{panel.border.linewidth}).
#' (2) A new \code{midnight} setting (\code{default.plotaesthetics}
#' category "Reference lines", default \code{"short"}; overridable per
#' plot via \code{aru.metadata.db}'s optional \code{$midnight} column)
#' controls how the Midnight line is drawn: \code{"none"} omits it
#' entirely; \code{"long"} draws one straight line spanning the full
#' panel width, edge to edge (via \code{geom_hline()}, unaffected by
#' which/how many suntimes dates are actually present); \code{"short"}
#' keeps the original behavior - a line connecting each real suntimes
#' date present in \code{suntimes.db}, which can fall short of the panel
#' edges when that's narrower than the full
#' \code{$date.start}-\code{$date.end} window. An unrecognized value
#' falls back to \code{"short"} with a console \code{NOTE}. (3) The
#' crossbar fill legend no longer shows an "All detections" key (it's
#' the obvious default, not worth a legend entry); it shows a
#' "40kHzMyo" key (black, per \code{crossbar.40khzmyo.fill}'s own
#' default) only when a 40kHzMyo crossbar is actually present in that
#' specific plot's data - not merely because \code{$40khzmyo}/\code{$MYSO}
#' flags are set, since a plot's species list can include 40kHzMyo as an
#' option without any night actually triggering it.
#'
#' \strong{Real bug found and fixed, per Josh: calling with
#' \code{project.name = "gome"} had no effect.} The settings-resolution
#' code itself (\code{aru.metadata.db} row > \code{project.name}-matching
#' column > \code{$default.value}) was already correct and is unchanged -
#' the bug was in the DELIVERED \code{batactivity.plotoptions.csv}, whose
#' override column was named literally \code{"project.name"} (a leftover
#' placeholder from when the merge script that builds this CSV first added
#' it) rather than the name of any real project. Since the code's override
#' lookup is \code{project.name \%in\% names(default.plotaesthetics)} - it
#' matches the ARGUMENT VALUE against an actual COLUMN NAME - a column
#' literally named \code{"project.name"} can never match a real
#' \code{project.name} value like \code{"gome"}, so no override ever took
#' effect no matter what was in that column. Fixed by renaming that column
#' to \code{"gome"} (Josh's real, current project) in the delivered CSV;
#' additional real projects get their own same-pattern column added later.
#' Re-verified the full three-tier precedence explicitly with
#' \code{project.name = "gome"}: a plot's own \code{aru.metadata.db} value,
#' when present, wins over both a \code{gome} column value and the default
#' (this is why the Y-axis title still reads "Hour of mointoring" (sic) -
#' that's \code{plot.meta.csv}'s own real value, correctly taking priority
#' per this precedence, not a code defect - fix the typo directly in
#' \code{plot.meta.csv}, or blank that cell, to let a \code{gome}-column or
#' default value through instead); when \code{aru.metadata.db} has no value
#' for a parameter, the \code{gome} column's value wins over the default;
#' when neither has a value, the default is used.
#'
#' \strong{Follow-up, 2026-08-27 - checked Josh's test-data folder for
#' changes before re-running (per standing project convention), found
#' several, and used them to get a genuinely real (non-synthetic)
#' end-to-end render working:} \code{plot.meta.csv} had already been
#' hand-fixed by Josh (the duplicate \code{"xaxe.title"} column above is
#' now correctly named \code{"yaxe.title"}); \code{WTG.arulist.csv} and a
#' freshly-regenerated \code{suntimes.csv} (via \code{batz.suntimes_generate()})
#' now both cover ARU \code{"WTG-GOM102"} across a wide 2025-2030 window,
#' which for the first time genuinely overlaps \code{vetted.processed.csv}'s
#' real May 2026 detections for that same ARU. \strong{A real bug was found
#' and fixed while proving this out}: \code{suntimes.db}'s \code{$date}/
#' \code{$suns}/\code{$sunr.mon} were parsed with a hardcoded
#' \code{"\%m/\%d/\%Y"}/\code{"\%m/\%d/\%Y \%H:\%M"} format, but
#' \code{batz.suntimes_generate()} actually writes these in ISO format
#' (\code{"2026-05-15"}, \code{"2026-05-15 19:56:29"}) - every real
#' \code{suntimes.db} row silently parsed to \code{NA} and got filtered
#' out, even though the ARU/date range genuinely overlapped, with no error
#' (just the existing "matched 0 rows of suntimes.db" NOTE, which reads as
#' a data-alignment problem, not a parsing bug). Fixed by parsing every
#' date/datetime field (\code{$date.start}/\code{$date.end} from
#' \code{aru.metadata.db}, \code{plot.data$date}, and
#' \code{suntimes.db$date}/\code{$suns}/\code{$sunr.mon}) with a small
#' multi-format fallback parser (tries \code{"\%m/\%d/\%Y"} then
#' \code{"\%Y-\%m-\%d"}, etc.), mirroring the same multi-format approach
#' \code{batz.suntimes_generate()} itself already uses for its own input
#' dates - this only adds format support, so no existing m/d/Y-formatted
#' file is affected. \strong{A second, separate real mismatch was also
#' found (not fixed - flagged for Josh):} \code{plot.meta.csv}'s
#' \code{$plot.set} is \code{"WTG-GOM101"}, but every row of
#' \code{vetted.processed.csv} is \code{$aru.groupby = "WTG-GOM102"} - a
#' different ARU entirely, independent of the date-range issue, since
#' \code{$aru.groupby}/\code{$plot.set} are matched before the date filter
#' even runs. \code{plot.meta.csv}'s own \code{$date.start}/\code{$date.end}
#' (\code{4/8/2026}-\code{4/27/2026}) also still doesn't cover the real
#' detections (\code{5/15/2026}-\code{5/21/2026}). With both of those two
#' fields corrected on a copy of the real \code{aru.metadata.db} (purely to
#' prove the pipeline - not changed in Josh's actual file, since it's his
#' call which ARU/window this plot.meta.csv row should describe), a fully
#' real, non-synthetic end-to-end render now succeeds. \strong{Josh: please
#' confirm whether \code{plot.meta.csv}'s \code{$plot.set} should actually
#' be \code{"WTG-GOM102"} (matching the real detection file) and update
#' \code{$date.start}/\code{$date.end} to a range that covers your real
#' detections, if you'd like this exact file combination to render.}
#'
#' \strong{Follow-up, 2026-08-27, later the same day - re-checked the
#' test-data folder again before re-running, per standing convention.}
#' Josh had fixed \code{plot.meta.csv}'s \code{$plot.set} himself (now
#' \code{"WTG-GOM102"}, matching \code{vetted.processed.csv}'s real
#' \code{$aru.groupby} - one of the two items flagged just above). Only
#' \code{$date.start}/\code{$date.end} (still \code{4/8/2026}-
#' \code{4/27/2026}) remain unaligned with the real
#' \code{5/15/2026}-\code{5/21/2026} detections; re-ran the full test suite
#' against this update with no other file changes and no regressions (all
#' passing). \strong{Josh: updating just \code{$date.start}/\code{$date.end}
#' in \code{plot.meta.csv} to cover your real detection window is now the
#' only remaining step to get a real render straight from your own files.}
#'
#' \strong{Follow-up, 2026-08-27, later still - two real bugs found and
#' fixed from Josh's direct visual review of a rendered plot: "the midnight
#' line looks thicker than the box line and is not reaching the ends."}
#' \strong{(1) Thickness - a genuine, verified ggplot2 rendering bug.} A
#' pixel-level measurement of a real rendered PNG (integrated optical
#' density across the stroke, not just eyeballing) confirmed that a
#' \code{geom_line()}/\code{geom_hline()} drawn with a given
#' \code{linewidth} renders at almost exactly \strong{2x} the actual
#' rendered pixel width of a \code{theme_bw()} \code{panel.border} drawn
#' with \code{element_rect(linewidth = }the same value\code{)} - reproduced
#' in an isolated diagnostic script (not specific to this plot's data), so
#' the two were never actually going to match despite sharing the same
#' \code{$panel.border.linewidth} value. Fixed by halving only the Midnight
#' line's own linewidth (\code{panel.border.lw / 2}, both "short" and
#' "long" modes) while leaving the panel border itself untouched - verified
#' by re-measuring a fresh render: 2.227px vs 2.225px, effectively
#' identical. \strong{(2) "Not reaching the ends" turned out not to be a
#' second bug once (1) was investigated properly}: the render Josh was
#' looking at used a narrow 7-day test window (5/15-5/21/2026, chosen at
#' the time to match whatever real detection data happened to be
#' available), where "short" mode's inherent (documented, by-design)
#' half-day pad at each edge is a much bigger fraction of the total panel
#' width - visually reading as a real gap. Per Josh's own correction
#' ("The dates should start at \code{$date.start} = 4/8/2026
#' \code{$date.end} = 4/27/2026 as found in the meta files"), the dev
#' script's synthetic test window was switched to Josh's actual real
#' \code{plot.meta.csv} window (4/8/2026-4/27/2026, a full 20 days) - and
#' with a full window, and now using \code{suntimes.db.real} directly
#' (needs no re-dating anymore - Josh's regenerated \code{suntimes.csv}
#' already covers this range for the right ARU, per the entry above), a
#' pixel measurement of the Midnight line's left/right endpoints against
#' the panel border's own edges showed a 0px gap on both sides - it
#' reaches the true edges exactly. No code change was needed for this
#' part; the earlier appearance of a gap was the padding-to-window-width
#' ratio, not a defect in how "short" mode computes its line.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("40k Myo is
#' missing from the legend"): a real bug, and a second, real
#' `scale_fill_manual()` gotcha found while fixing it.} The 40kHzMyo
#' legend key was previously shown only when a 40kHzMyo row actually
#' survived into that plot's filtered data (an actual detection that
#' period) - MY OWN interpretive judgment call from earlier this session,
#' not Josh's own original wording ("40kHzMyo if on species list should
#' be [on the legend] and colored black"). Fixed to key off `$40khzmyo`
#' itself (now carried through per-plot as `$khz.flag`) rather than data
#' presence - exactly Josh's real \code{plot.meta.csv} (`$40khzmyo = TRUE`)
#' plus \code{vetted.processed.csv} (zero actual 40kHzMyo detections)
#' combination. \strong{The first fix attempt (just adding "40kHzMyo" to
#' `scale_fill_manual()`'s `breaks=`) looked right in code but was
#' verified WRONG by actually rendering it}: a manual scale's `breaks=`
#' are silently dropped from the real legend for any level that never
#' appears in the mapped data, no matter what's declared in `breaks=` -
#' confirmed with an isolated diagnostic (a bare `geom_col()` +
#' `scale_fill_manual(breaks = "B", ...)` with zero rows using fill
#' `"B"`: no legend key at all), the same lesson as `$labels$y` earlier
#' this session that a ggplot2 scale's declared settings can't always be
#' trusted without actually building/rendering. The real fix adds an
#' explicit `limits=` to `scale_fill_manual()` (`c("All detections",
#' "40kHzMyo")` when `$40khzmyo` is TRUE for this plot, `"All detections"`
#' alone otherwise) so "40kHzMyo" is in the scale's domain independent of
#' whether any row actually used that fill value that period - re-verified
#' by rendering: the black swatch now shows correctly with zero 40kHzMyo
#' detections present, matching Josh's real files exactly.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("plot.meta$xaxe.interval
#' = 4 which should make there only be four labeled dates on the X axes") and
#' ("add an option to \"batactivity.plotoptions\" that makes the mid night
#' line a serises of grey dots"): two changes, one a real bug fix and one a
#' new feature.} First, \code{$xaxe.interval} had the same class of bug as the
#' earlier "gome" issue: it was read with \code{get.default("xaxe.interval")},
#' never \code{get.setting(p$job, "xaxe.interval")}, so a plot's own real
#' \code{$xaxe.interval} value (Josh's real \code{plot.meta.csv} row has
#' \code{$xaxe.interval = 4}) was always silently ignored in favor of the
#' shared default.plotaesthetics value. Fixing just the lookup wasn't enough,
#' though: the value was being fed into \code{scale_x_date(date_breaks = ...)},
#' which expects a day-spacing string (e.g. \code{"4 days"} = one label every 4
#' days) - but Josh's real value (\code{4}) and his stated intent ("only be
#' four labeled dates") mean a COUNT of evenly-spaced labels, not a spacing
#' interval. Re-purposing \code{$xaxe.interval} as a day-spacing string was MY
#' OWN earlier interpretive choice this session, not something Josh asked
#' for. Fixed by computing \code{N} explicit break dates via
#' \code{seq(date.start, date.end, length.out = N)} (verified: this always
#' places the first/last break exactly at \code{date.start}/\code{date.end}, with
#' the remainder evenly spaced between) and passing them to
#' \code{scale_x_date(breaks = ...)} instead of \code{date_breaks =}. An
#' unparseable \code{$xaxe.interval} (not a positive number) falls back to 2
#' labels (just \code{date.start}/\code{date.end}) with a console NOTE, the same
#' graceful-fallback pattern used elsewhere in this function.
#'
#' Second, a new \code{$midnight = "dots"} mode was added alongside the existing
#' "none"/"long"/"short" values, drawing one grey dot (via \code{geom_point()})
#' per real suntimes date present, instead of a connecting line. \strong{This
#' is a judgment call, flagged here explicitly}: Josh's request ("makes the
#' mid night line a serises of grey dots") could instead have meant changing
#' the existing "short"/"long" line rendering itself (e.g. via
#' \code{$midnight.linetype}/\code{$midnight.color}) to look dotted rather than
#' adding a wholly new mode value - the new-mode reading was chosen because
#' it parallels how the earlier none/long/short modes were themselves
#' introduced as new selectable values, and because it leaves the existing
#' "short"/"long" appearance completely unchanged for anyone not opting in.
#' Two new default.plotaesthetics parameters were added,
#' \code{$midnight.dots.color} (default \code{"grey50"}) and
#' \code{$midnight.dots.size} (default \code{"1.5"}, in \code{geom_point()}
#' "size" units), kept deliberately separate from \code{$midnight.color}/
#' \code{$midnight.linetype} so switching a plot to "dots" mode can't change
#' what "short"/"long" mode looks like for any other plot sharing the same
#' default.plotaesthetics file. Verified via \code{ggplot_build()}
#' introspection of the actual rendered layer geom class (\code{GeomPoint} for
#' "dots" vs \code{GeomLine} for "short") and resolved color (matching
#' \code{$midnight.dots.color} in dots mode, \code{$midnight.color} unchanged in
#' short mode) - not just by reading the code, per this session's established
#' practice of confirming ggplot2 behavior empirically.
#'
#' \strong{A third, real ggplot2 gotcha was found and fixed while visually
#' checking the "dots" mode legend, not caught by the geom-class/color checks
#' above.} Rendering the actual legend (not just introspecting the built
#' plot) showed that switching \code{$midnight} to "dots" also put a stray dot
#' marker on the Dawn AND Dusk legend keys, even though their own lines on
#' the panel were completely unaffected - ggplot2's default legend-key
#' merging draws every layer's key glyph onto every row of a shared discrete
#' color guide, regardless of which layer's data actually produced that
#' row, once any layer sharing that guide uses \code{geom_point()}. Confirmed
#' with an isolated diagnostic (two plain \code{geom_line()} layers plus one
#' \code{geom_point()} layer sharing a single \code{color} aesthetic: both
#' line-only legend rows picked up a stray point marker). \strong{The first fix
#' attempt (giving each layer its own explicit \code{key_glyph}) looked
#' plausible but was verified to make no visible difference when actually
#' rendered} - and introduced a separate hazard: ggplot2 marks a
#' \code{key_glyph}'d geom's class with a leading empty-string entry
#' internally, which would silently break any code checking
#' \code{class(layer$geom)[1]}. The real fix uses
#' \code{guide_legend(override.aes = list(shape = ...))}: the reference-line
#' legend's break order is always alphabetical (Dawn, Dusk, Midnight, since
#' \code{scale_color_manual()} here declares no explicit \code{breaks=}) - a
#' stable ggplot2 default, confirmed by rendering - so \code{shape} is pinned
#' per-row by position: no marker for Dawn/Dusk always, and a dot for
#' Midnight's own row only in "dots" mode. Re-verified by rendering both
#' "short" and "dots" mode side by side: Dawn/Dusk are back to plain dashed
#' lines in both, and only "dots" mode's Midnight key shows a dot.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("make midnight dots
#' into thin dashes instead"): "dots" mode's rendered glyph changed from a
#' filled circle to a thin horizontal dash.} \code{shape = 16} (a filled
#' circle) was changed to \code{shape = 45} - 45 is the literal "-" (hyphen)
#' ASCII character used as a plotting glyph, which renders as a short
#' horizontal dash rather than a circle; visually this reads as a dashed
#' line broken into one mark per real suntimes date, matching Josh's
#' request directly. \code{$midnight = "dots"} is kept as the setting's value
#' name (unchanged, so no existing config referencing it breaks) even though
#' the rendered glyph is now a dash, not a dot - the name describes the
#' per-date-marker MECHANISM (as opposed to "short"/"long", which draw one
#' continuous connected line), not the literal glyph shape. The legend
#' override introduced in the entry above was updated in lockstep (shape 45
#' instead of 16) so the "Midnight" legend key's dash matches the panel
#' exactly. Verified by rendering: the panel shows a clean row of short
#' dashes at each real suntimes date, and the legend key shows a matching
#' dash rather than a dot, with Dawn/Dusk unaffected either way.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("There is a problem
#' with the labels on the Xaxes, they do not appear to be the real dates
#' rather labels rewriting the dates"):} every individual x-axis break date
#' was already correct (re-confirmed via \code{scale_x_date()} introspection
#' and by re-checking the \code{$xaxe.interval} fix above), so this was not a
#' data or parsing bug. The real cause, found by zoom-cropping the actual
#' rendered PNG rather than by reading code alone: \code{$xaxe.interval}
#' places a break exactly at each panel's \code{date.end}, so that label is
#' horizontally centered ON the panel's right edge and roughly half its width
#' extends into the NEXT panel's plotting area; \code{theme_bw()}'s default
#' panel spacing (about 5.5pt) leaves too little of a gap for a two-line date
#' label to clear the corresponding \code{date.start} label of the next
#' panel, so the two visually run together into what reads as a garbled or
#' "rewritten" date even though each date is individually correct. Confirmed
#' by reproducing the collision in an isolated \code{facet_wrap()} diagnostic
#' before touching the real code. Fixed by adding a new
#' \code{$panel.spacing.x} default.plotaesthetics parameter (default
#' \code{"40"}, points) applied via \code{theme(panel.spacing.x =
#' grid::unit(...))}; the value was tuned empirically by rendering a sweep
#' (5.5/15/25/35/40/50/60/70pt) and visually checking panel-boundary
#' separation, not just picking a plausible-looking number. Re-verified on
#' Josh's own real data: the rightmost label of one panel and the leftmost
#' label of the next now render with a clear visible gap between them at
#' every panel boundary.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("make the dashed
#' midnight line thicker, half way towards being as thick as the dusk
#' line"):} \code{$midnight.dots.size} (see the "dots"/dashes entries above)
#' was increased from \code{"1.5"} to \code{"2.5"}. \strong{"Half way" is an
#' interpretive judgment call, flagged here explicitly}: read as the pixel
#' thickness half way between the dash's own current rendered thickness and
#' the dusk line's rendered thickness (rather than, say, half of the dusk
#' line's thickness outright). Measured empirically in the actual rendered
#' PNG using an integrated-optical-density method (summing a stroke's
#' cross-section darkness and dividing by its peak darkness, robust to
#' anti-aliasing): the midnight dash measured about 2.0px and the dusk line
#' about 4.5px, putting the halfway target at about 3.2px. At this plot's
#' save resolution (300 dpi), a glyph's rendered thickness is quantized to
#' whole pixel rows rather than continuous, so no \code{size} value lands
#' exactly on 3.2px; \code{size = 2.5} (and every value up to 3.25 tested)
#' renders at about 3.0px, the closest achievable step below the target
#' (the next step up, \code{size = 3.5}, overshoots to about 4.0px, further
#' from the target) - \code{size = 2.5} was kept as the best available
#' match, re-confirmed by re-measuring the dash and dusk line together in
#' the same rendered panel after the \code{$panel.spacing.x} fix above (which
#' shifts panel positions but does not otherwise change line rendering).
#' Re-verified visually: the midnight dash is now clearly heavier than
#' before while still clearly thinner than the dusk line beside it, with
#' \code{$dusk.color}/\code{$dusk.linetype} themselves unchanged.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("that is worse, I
#' only get a box now not a plot... Label for \"eastern small footed myotis\"
#' is cut off and the size of the plot windows is too small... Revert back
#' to the previous plot dimensions and reduce the size of the labels on the
#' x and Y axis until there is no overlap with either"): the
#' \code{$panel.spacing.x} fix above is reverted to a plain, dimension-neutral
#' value, and the real x-axis fix now comes from smaller tick-label text
#' instead.} The \code{$panel.spacing.x = 40} fix stopped the x-axis label
#' collision, but this function's saved figure width is a FIXED size
#' (\code{$plot.width + $ggsave.width.pad}, not something that grows with
#' the number of panels/gaps) - so widening the gaps between panels shrank
#' every panel's own width to make room, which in turn made the "Eastern
#' small-footed myotis" facet title too wide for its new, narrower panel
#' and cut it off. Per Josh's explicit correction, \code{$panel.spacing.x}
#' is reverted to \code{"5.5"} - \code{theme_bw()}'s own built-in default,
#' so this is a plain value revert that restores the original panel/figure
#' dimensions exactly, not a removal of the setting (it stays available to
#' override later). The actual label-collision fix now comes from
#' \code{$axis.text.size} instead, reduced from \code{"8"} to \code{"6"}:
#' smaller tick-label text needs less horizontal room, so the two-line date
#' labels clear each other even at the original tight panel spacing. Tuned
#' empirically, not guessed: pixel-cropped the rendered panel boundary at
#' several candidate sizes (8, 7, 6.5, 6) and found 8/7/6.5 still show
#' visible character-level overlap between adjacent panels' date labels
#' (e.g. the "8" of "May-28" touching the "M" of "May-07"), while 6 is the
#' first size with a clean, non-overlapping gap.
#'
#' \strong{A second, separate issue was found and corrected while
#' investigating the cut-off species title - a stale/incorrect
#' default.plotaesthetics documentation note, not a code bug, but an
#' attempted fix for it introduced a real regression that was caught before
#' shipping.} The existing note on \code{$axis.title.size} claimed it is
#' "also reused directly for facet strip text (strip.text)" - checked
#' directly via ggplot2's own \code{get_element_tree()} and confirmed FALSE:
#' \code{strip.text} inherits from the base \code{"text"} element, not from
#' \code{"title"}/\code{axis.title}, so changing \code{$axis.title.size} has
#' never actually had any effect on facet panel titles. An explicit
#' \code{strip.text = element_text(size = axis.title.size)} was added to
#' make the documented behavior real - but re-rendering showed this made
#' the cutoff WORSE, not better: ggplot2's actual fixed \code{strip.text}
#' size (\code{rel(0.8)} of \code{theme_bw()}'s \code{base_size} 11 = 8.8pt)
#' is SMALLER than \code{$axis.title.size}'s default of 10, so binding them
#' enlarged the title instead of shrinking it - caught by re-rendering and
#' comparing before/after, not assumed safe from the code alone. Reverted:
#' \code{strip.text} is left at ggplot2's native, non-configurable size, and
#' the \code{$axis.title.size} note is corrected to describe what the code
#' actually does (facet strip text is not independently configurable),
#' instead of changing the code to match a stale, inaccurate note.
#' Re-verified with both changes together (\code{$panel.spacing.x = "5.5"},
#' \code{$axis.text.size = "6"}, no \code{strip.text} override): the panel
#' grid is back to its original size, "Eastern small-footed myotis" renders
#' in full with no truncation, and the x-axis date labels still show a
#' clean gap at every panel boundary.
#'
#' \strong{Follow-up, 2026-08-27, later still - real error Josh hit on his
#' own machine: `devtools::document()` succeeded, but calling the function
#' against his own real, current objects crashed deep inside grid
#' graphics, not in this function's own code.} The actual error was
#' `Error in grid.Call.graphics(C_setviewport, vp, TRUE): non-finite
#' location and/or size for viewport` - naming no setting and giving no
#' hint of the real cause. Root cause: Josh's loaded
#' `default.plotaesthetics` (his own on-disk `batactivity.plotoptions.csv`)
#' was an OLDER copy from before `$panel.spacing.x` was added earlier this
#' same round - `get.default("panel.spacing.x")` silently returns `NA` for
#' any parameter not present as a row (its own documented, intentional
#' fallback behavior), `as.numeric(NA)` stayed `NA`, and
#' `grid::unit(NA, "pt")` only actually failed once ggplot2 tried to use it
#' to lay out the plot - three layers of code away from the real, fixable
#' cause (a stale CSV). \strong{Fixed defensively, not just by telling Josh
#' to update his file}: every `default.plotaesthetics` parameter this
#' function depends on ONLY via `get.default()` (no `aru.metadata.db`
#' per-job override path) is now checked up front, the same way
#' `plot.data`/`suntimes.db`/`aru.metadata.db`'s own required COLUMNS
#' already were - a missing row (e.g. an older `batactivity.plotoptions.csv`
#' that predates a newly-added setting) now stops immediately with a clear
#' message naming exactly which parameter row is missing, instead of
#' crashing unrecognizably deep in `grid`. Verified with a dedicated test:
#' a synthetic `default.plotaesthetics` with its `$panel.spacing.x` row
#' removed now stops with `"default.plotaesthetics is missing these
#' required $parameter rows: panel.spacing.x..."` instead of the
#' grid/viewport error. Immediate fix for Josh: re-save the current
#' `batactivity.plotoptions.csv` (already sent, with `$panel.spacing.x` and
#' the reduced `$axis.text.size`) into his test-data folder and reload it
#' before calling this function again.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("clean up
#' batz.plotdect_first.last()... change identifiers to"): the four main
#' argument names were shortened/renamed, with no change in behavior.}
#' `plot.data` -> `data`, `aru.metadata.db` -> `fig.list`,
#' `suntimes.db` -> `suntimes`, `default.plotaesthetics` ->
#' `aes.default` (`project.name` is unchanged). Every reference to
#' these four names inside the function body, the internal
#' `*.REQUIRED`/`*.REQUIRED.PARAMETERS` constant names, the
#' `@param`/`@examples` documentation, and every call site in the
#' `.dev.R` test script were updated together (verified with a
#' whole-file identifier search after the rename: zero remaining references
#' to any of the four old names as bare identifiers). \strong{The `Details`
#' entries ABOVE this one are left exactly as originally written, still
#' using the OLD parameter names throughout} - they are a dated history of
#' what was true and named at the time each entry was written, not a
#' description of the current interface; only this entry, the
#' `@param`/`usage`/`examples` sections above, and the actual
#' code describe the CURRENT (renamed) interface. Full test suite re-run
#' clean (all 11 scenarios, no regressions) after the rename - no functional
#' change, purely an identifier rename.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("change the pattern
#' from \"batactivity.plotoptions.csv\" to \"plotopts_first.last.csv"):
#' the on-disk file name this function's `aes.default` input is expected to
#' be loaded from was renamed - purely a file-naming change, not a
#' parameter/argument rename (that was the entry above) and not a change to
#' any column/row inside the file itself.} `batactivity.plotoptions.csv` ->
#' `plotopts_first.last.csv`, chosen to tie the file name to this specific
#' function (`first.last`) rather than the more generic "batactivity" name,
#' since the project has other `batz` plotting functions with their own,
#' separate settings files. Updated everywhere this file name appears as a
#' CURRENT, forward-looking reference: the `$yaxe.limit.min`/
#' `$yaxe.limit.max` HH:MM-format error message below (now names
#' `plotopts_first.last.csv`), `.dev.R`'s `read.csv()` call, and
#' `build_merged_plotoptions.R`'s `write.csv()` call. \strong{The dated
#' `Details` entries ABOVE this one are left exactly as originally
#' written, still naming the file `batactivity.plotoptions.csv`}, since
#' that was its actual name at the time each of those entries was written;
#' they are a historical record, not current guidance. \strong{Per Josh's
#' nineteenth follow-up (2026-09-16), the project's own saved master copy
#' has now ALSO been renamed to \code{plotopts_first.last.csv}} (was
#' \code{claude/plotoptions.batactivity.default.csv}), so the on-disk
#' master and this function's own documented expectation finally match -
#' see Details, "Settings resolution (round nineteen)" below. Full test
#' suite re-run clean (all 11 scenarios, no regressions) after the
#' original rename - no functional change, purely a file-naming change.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("change 'aru.meta.csv'
#' to 'fig.list.csv'"): the `.dev.R` test script's on-disk test file for the
#' `fig.list` argument was renamed to match a file Josh had already renamed
#' on his own machine.} No file literally named `aru.meta.csv` ever existed
#' in this project - read as referring to the test file this script loads
#' as `fig.list` (previously `plot.meta.csv`), the name apparently garbled
#' the same way `aru.metadata.db` (the OLD parameter name for `fig.list`,
#' renamed two entries above) was garbled as "aru.matadata.db" earlier; if
#' this reading is wrong, flag it and it'll be corrected. `plot.meta.csv` ->
#' `fig.list.csv`, purely a file-naming change in `.dev.R` (this package
#' function itself takes a data frame, not a file path, so nothing in `.R`
#' actually reads this file) - `.dev.R`'s `read.csv()` call and its TEST 9
#' diagnostic `cat()` labels (which print this file's name as part of their
#' output) were updated; every OTHER mention of `plot.meta.csv` in `.dev.R`'s
#' own header comments is dated narrative describing a specific past
#' investigation (e.g. the duplicate `xaxe.title` bug) and was left alone,
#' same convention as every other historical entry in this file. Full test
#' suite re-run clean (all 11 scenarios, no regressions) - confirmed it
#' actually reads `fig.list.csv` off disk with no complaint.
#'
#' \strong{Follow-up, 2026-08-27, later still, per Josh ("add in a dir.save =
#' getwd()"): a new \code{dir.save} parameter (default \code{getwd()}) now
#' controls where every generated PNG is saved.} The "Iteration 1" scope
#' paragraph near the top of Details lists a \code{dir.save}-style
#' output-location argument as explicitly NOT YET implemented ("plots
#' currently save to the working directory") - that was true when written
#' and is left as historical, per this project's own never-rewrite-history
#' convention; it's implemented now. Each plot's own file NAME is still
#' entirely driven by \code{aes.default}'s \code{$output.filename.pattern}
#' (unchanged); \code{dir.save} only changes which DIRECTORY that name is
#' written into (\code{file.path(dir.save, fname)}, right before the
#' \code{ggsave()} call). Default \code{getwd()} matches the directory
#' every prior call already implicitly saved into (a bare relative file
#' name passed to \code{ggsave()} resolves against the working directory),
#' so omitting \code{dir.save} changes nothing for existing callers.
#'
#' Same Follow-up, second part, per Josh ("change 'Earlies and lastest
#' ball' in the save name to 'Earliest and latest bat'"): the probable
#' typo in \code{aes.default}'s own \code{$output.filename.pattern}
#' DEFAULT VALUE - flagged, not silently fixed, when this function was
#' first built (see the \code{aes.default} settings file's own
#' \code{$notes} column) - is now corrected at Josh's explicit request.
#' This is a change to the DATA (the default \code{output.filename.pattern}
#' value shipped in \code{plotopts_first.last.csv}/the project's reference
#' copies of that settings file), not to this function's code - this
#' function only ever reads whatever pattern \code{aes.default} gives it
#' and does no string-literal matching/fixing of its own. Anyone whose own
#' local settings CSV still has the old, misspelled pattern value will keep
#' getting the old (misspelled) file names until they update that CSV too -
#' this function has no way to detect or correct that on its own.
#'
#' \strong{BUGFIX, 2026-08-27, later still - real bug Josh hit running his
#' own real \code{fig.list}: only 1 plot was produced when 9-10 rows should
#' have matched data.} Root cause: \code{plots} and \code{ggplots} were
#' keyed BY \code{job.label} (the display string - \code{$plot.name} when
#' non-blank, e.g. \code{plots[[job.label]] <- list(...)}), but nothing
#' requires \code{$plot.name} to be unique across \code{fig.list} rows -
#' it's a human-readable project/site label, and Josh's real file quite
#' reasonably uses the identical name for every one of its 10 monitoring-
#' window rows. Rows sharing a \code{$plot.name} therefore silently
#' OVERWROTE each other's entry in \code{plots} (plain R named-list
#' assignment - \code{x[["k"]] <- v} replaces any existing entry under
#' \code{"k"}, it doesn't add a second one), so only the LAST matching
#' row's entry survived to the rendering loop - even though "Prepared plot
#' data for '...'" printed once per matching row along the way (proving
#' each row's own data-filtering step ran fine up to that point). This
#' exactly matches what Josh saw both times: multiple "Prepared plot
#' data"/"NOTE" lines, but only one final "Saved:" line. No existing test
#' had ever exercised more than one \code{"bat.detection"} row matching
#' data in the same call, so this had gone uncaught until real data with
#' this exact (very reasonable) shape hit it. \strong{Fixed} by
#' introducing a separate \code{job.key} (each row's own loop index,
#' always unique) as the actual list key for \code{plots}/\code{ggplots};
#' \code{job.label} is kept, unchanged, as a pure DISPLAY string (still
#' used for console messages, the plot title, and nowhere else) - rows
#' sharing a \code{$plot.name} now each get their own entry and their own
#' saved PNG, same as rows with distinct names always did. Verified with a
#' new test: three \code{fig.list} rows built as identical copies of one
#' real row (guaranteeing all three match the exact same data, so any
#' shortfall below 3 plots could only be this naming-collision bug, not a
#' real data-overlap difference) now correctly produce 3 entries in both
#' \code{$plots} and \code{$ggplots} (previously collapsed to 1). Full
#' dev-script test suite re-run clean (14 scenarios, no regressions).
#'
#' \strong{Follow-up, 2026-08-28, per Josh ("I do not want 100\% duplicate
#' rows to produced multiple graphs. Instead remove duplicate rows then go
#' row by row producing 1 graph per row"):} the \code{job.key} fix directly
#' above guarantees that every \code{fig.list} row - even ones sharing the
#' same \code{$plot.name} - gets its own \code{$plots}/\code{$ggplots}
#' entry, but it doesn't address what happens when a row is repeated
#' outright: two (or more) rows identical in EVERY column would each still
#' get rendered and saved, and since the saved file's own name comes from
#' \code{aes.default}'s \code{$output.filename.pattern} (typically built from
#' the ARU/dates/a second-resolution timestamp - all identical for true
#' duplicates), the later save can silently overwrite the earlier one's PNG
#' on disk with no warning (confirmed empirically: counting actual files on
#' disk after a duplicate-heavy run turned up fewer files than "Saved:"
#' lines). Per Josh's explicit instruction, this is fixed at the source
#' instead of by disambiguating filenames: exact full-row duplicates in
#' \code{fig.list} (every column identical, not just \code{$plot.name}) are
#' now collapsed down to their first occurrence - via
#' \code{jobs <- jobs[!duplicated(jobs), , drop = FALSE]} - before any
#' plotting happens, unconditionally (no new parameter to opt in/out). This
#' is order-preserving (the first occurrence of each distinct row survives)
#' and applies unconditionally, with a console \code{NOTE} reporting how many
#' rows were removed and how many distinct rows remain when any are found.
#' \strong{This does NOT affect rows that merely share \code{$plot.name}
#' but differ in any other column} (e.g. a different \code{$date.start}) -
#' those are not duplicates by this check and each still gets its own plot,
#' exactly as the \code{job.key} fix above already guaranteed. Verified with
#' two new tests: (1) three rows identical in every column produce exactly 1
#' \code{$plots}/\code{$ggplots} entry (2 duplicates removed, reported via
#' the console \code{NOTE}), and (2) a mixed set of two exact duplicates of
#' one row plus one genuinely distinct row produces exactly 2 entries (1
#' duplicate removed). The existing job-key-collision test (three rows
#' sharing \code{$plot.name} but each with a distinct \code{$xaxe.interval})
#' was re-checked to still produce 3 entries, confirming this new dedup step
#' does not fold together rows that merely share a display name. Full
#' dev-script test suite re-run clean (16 scenarios, no regressions).
#'
#' \strong{Settings resolution (round nineteen), per Josh's 2026-09-16
#' follow-up ("reorder the headings in all plotopts files to be $category
#' $parameter $default.value $overide.value $notes ... add arguments
#' aes.style = \"overide.value\" ... Function logic will first look in the
#' column with the header = aes.style ... then if that element is blank
#' use $default.value"):} the \code{project.name}-matches-a-column-name
#' mechanism described several entries above (the "gome" column, etc.) is
#' REPLACED entirely. Every \code{aes.default} sheet now has a fixed
#' \code{$overide.value} column (blank by default, between
#' \code{$default.value} and \code{$notes} - see
#' \code{\link{batz.generate_plotopts}}), which the user fills in directly
#' on their own copy of the CSV to override a setting. The new
#' \code{aes.style} argument (default \code{"overide.value"}) names which
#' column \code{get.default()} checks FIRST; when that column doesn't
#' exist (an older sheet) or is blank for a given row, \code{$default.value}
#' is used, exactly as before. \code{project.name} no longer participates
#' in settings resolution AT ALL - it is now used purely to build the saved
#' file name (see "File naming (round nineteen)" below). The THIRD tier of
#' the old precedence - a \code{fig.list} row's own value beating both the
#' column and the default - is UNCHANGED: \code{get.setting(job, param)}
#' still checks \code{job} first, then falls through to the (now
#' \code{aes.style}-driven) \code{get.default(param)}. This is a judgment
#' call on backward compatibility, flagged for Josh: an older
#' \code{aes.default} sheet with its own \code{project.name}-matching
#' column (e.g. a column literally named \code{"gome"}) will simply be
#' ignored now - re-fill any values that were in that column into the new
#' \code{$overide.value} column instead.
#'
#' \strong{File naming (round nineteen), same follow-up:} every saved PNG's
#' file name is now always \code{"<project.name>_<ARU>_<timestamp>.png"} -
#' \code{aes.default}'s \code{$output.filename.pattern} is DEPRECATED and no
#' longer read at all (same deprecation pattern as \code{$plot.width} in
#' \code{\link{batz.plotcover_bullseye}} - the row is left in place in
#' \code{plotopts_first.last.csv}, harmless, simply ignored).
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.plotdetections_first.last()}: family = "plotdetections" (the
#' verb "plot" is baked into the family name, same pattern as
#' \code{batz.plotframe_batactivity}), subject = "first.last" (each
#' species' first and last nightly detection). Requested as
#' \code{batz.plotdections_first.last()} - "plotdections" was a typo for
#' "plotdetections", fixed here; nothing else changed.
#'
#' \strong{Follow-up, 2026-09-22, per Josh's request ("change all functions
#' that have aru as an header to \"aru.name\"", found via the project's own
#' reference workbook and cross-checked against this function's live
#' source): \code{SUNTIMES.REQUIRED}'s \code{"aru"} entry is now
#' \code{"aru.name"}, matching \code{\link{batz.generate_suntimes.arulist}}'s
#' own output column, renamed the same day.} This function's own internal
#' reference (\code{sdb$aru}, used to filter \code{suntimes} down to a
#' single \code{$plot.set}) is now \code{sdb$aru.name}. Since
#' \code{suntimes}'s required headers are matched via
#' \code{canonicalize.headers()} (see "BUGFIX (2026-09-21...)" above), a
#' \code{suntimes} argument arriving with either the old bare \code{$aru}
#' spelling or the new \code{$aru.name} spelling is still accepted and
#' renamed to this function's own canonical spelling before use - this
#' change only affects what that canonical spelling now IS. Full test
#' suite re-run clean after the rename (no regressions).
#'
#' \strong{Follow-up, 2026-09-23, per Josh: fig.list's \code{$plot.sets}
#' column was renamed to \code{$plot.set} in \code{\link{batz.plotactivity_observations}}
#' this same round (standardizing on the singular spelling, since
#' \code{fig.list.csv} is one file shared across every \code{batz} plotting
#' function) - checked and confirmed NO code change was needed here.} This
#' function's own \code{FIG.LIST.REQUIRED} already listed \code{"plot.set"}
#' (singular), and its one internal reference (\code{job$plot.set}, matched
#' against a single literal value via \code{tolower(trimws(pd$group)) ==
#' tolower(plot.set.val)} - never parsed into multiple tokens the way
#' \code{\link{batz.plotactivity_observations}}'s \code{$plot.set} is) was
#' already using this spelling from the start (see the "clean up
#' batz.plotdect_first.last()" identifier-rename entry above, 2026-08-27),
#' so the two sibling functions' \code{fig.list} column names were already
#' aligned before this round - only \code{batz.plotactivity_observations}
#' itself needed the rename, per Josh's confirmed design there (see that
#' function's own \code{@details}, "Follow-up, 2026-09-23", for the full
#' \code{$plot.group}-optional design discussion, which likewise doesn't
#' apply here - this function's grouping column has always been the fixed,
#' non-configurable \code{$group}, with no per-row \code{$plot.group}
#' override to begin with). No test or behavior change here.
#'
#' @examples
#' \dontrun{
#' # default dir.save = getwd() - saves into the current working directory,
#' # default project.name = "new.project"
#' result <- batz.plotdetections_first.last(
#'   data = vetted.processed,
#'   fig.list = plot.meta,
#'   suntimes = aru.suntimes,
#'   aes.default = batactivity.plotoptions
#' )
#' result$ggplots[[1]]
#'
#' # explicit project.name/dir.save
#' result <- batz.plotdetections_first.last(
#'   data = vetted.processed,
#'   fig.list = plot.meta,
#'   suntimes = aru.suntimes,
#'   aes.default = batactivity.plotoptions,
#'   project.name = "gome",
#'   dir.save = "C:/path/to/output/folder"
#' )
#' }
#'
#' @export
batz.plotdetections_first.last <- function(data, fig.list, suntimes,
                                            aes.default, project.name = "new.project",
                                            aes.style = "overide.value",
                                            dir.save = getwd()) {

  ## Header standardization (per Josh, 2026-09-14 project preference): NOT
  ## applied to any of the four *.REQUIRED*/DATA.REQUIRED constants below -
  ## `data`/`fig.list`/`suntimes`/`aes.default` are already-loaded data
  ## frames handed in by the caller (this function loads no file itself),
  ## and these names are this function's own interface contract with those
  ## upstream functions'/files' already-established output schemas, not raw
  ## loaded headers. See @details "Header standardization" above.
  ##
  ## BUGFIX (2026-09-21, per Josh): "aru.groupby" corrected to "group" -
  ## batz.generate_plotframe.bat() stopped producing $aru.groupby (renamed
  ## to $group) back on 2026-08-28; this constant (and the one place below
  ## that reads pd$aru.groupby) were never updated to match at the time.
  ## See @details, "BUGFIX (2026-09-21...)".
  ##
  ## Follow-up (2026-09-22, per Josh: "change all functions that have aru
  ## as an header to \"aru.name\""): SUNTIMES.REQUIRED's "aru" entry is now
  ## "aru.name", matching batz.generate_suntimes.arulist()'s own renamed
  ## output column - see @details, "Follow-up, 2026-09-22...aru as an
  ## header".
  ##
  ## Follow-up (2026-09-23, per Josh): fig.list's $plot.sets column was
  ## renamed to $plot.set in batz.plotactivity_observations() this same
  ## round - checked here and confirmed FIG.LIST.REQUIRED already said
  ## "plot.set" (singular), so no change was needed in this constant or
  ## the one place below that reads job$plot.set. See @details, "Follow-up,
  ## 2026-09-23...".
  DATA.REQUIRED <- c("spp.id", "date", "group", "obs",
                           "mins2.noon.min", "mins2.noon.max", "vetting.type")
  SUNTIMES.REQUIRED <- c("aru.name", "date", "date.mon", "sunregion", "time.zone",
                             "sunregion.type", "schedual1", "schedual2", "suns",
                             "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                                 "Alldect", "facet.panel", "40khzmyo", "facet.label",
                                 "plot.set", "date.format", "date.start", "date.end",
                                 "xaxe.interval", "xaxe.title")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  # 2026-08-27, later still - real bug hit on Josh's machine: his loaded
  # aes.default was an OLDER copy of batactivity.plotoptions.csv
  # from before $panel.spacing.x was added (see the round above). The
  # column-structure check right below (AES.DEFAULT.REQUIRED)
  # only verifies aes.default HAS the right columns
  # (category/parameter/default.value) - it never checked that every
  # PARAMETER ROW this function actually depends on is present. With
  # $panel.spacing.x missing, get.default("panel.spacing.x") silently
  # returned NA (its own documented behavior for an unknown parameter),
  # as.numeric(NA) stayed NA, and grid::unit(NA, "pt") only failed much
  # later and far downstream, deep inside grid's own rendering code -
  # "Error in grid.Call.graphics(C_setviewport, vp, TRUE): non-finite
  # location and/or size for viewport" - which names no setting and gives
  # no hint that a CSV row is missing. Reproduced directly: calling
  # get.default() on a parameter absent from a real aes.default
  # data frame returns NA_character_, and unit(as.numeric(NA), "pt") does
  # print/render as a non-finite unit once used in theme(), confirming this
  # is exactly what happened. Every parameter name this function looks up
  # ONLY via get.default() (i.e. no fig.list per-job override path)
  # is now checked up front, the same way data/suntimes/
  # fig.list's own required COLUMNS already are - missing rows now
  # stop with one clear, actionable message instead of a cryptic grid
  # crash three layers of code away from the real cause.
  ##
  ## "output.filename.pattern" deliberately removed from this required list
  ## per Josh's nineteenth follow-up (2026-09-16) - the saved file name is
  ## now always "<project.name>_<ARU>_<timestamp>.png"; no longer read at
  ## all. An $output.filename.pattern row left in an existing aes.default
  ## sheet is harmless (simply ignored).
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
    "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
    "panel.border.linewidth", "legend.position", "xaxe.interval",
    "yaxe.break.interval", "yaxe.labelformat", "yaxe.break.labels",
    "midnight.linetype", "midnight.color", "midnight.dots.color",
    "midnight.dots.size", "dawn.linetype", "dawn.color", "dusk.linetype",
    "dusk.color", "reference.line.legend.title", "crossbar.alldetections.fill",
    "crossbar.40khzmyo.fill", "crossbar.linewidth", "crossbar.fill.legend.title",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "plot.width", "plot.height"
  )

  ## BUGFIX (2026-09-21, per Josh): every input frame's headers are now
  ## matched via the shared package helper canonicalize.headers() rather
  ## than a plain setdiff() - both this frame's real column names and the
  ## required list above are standardized to snake_case purely to find
  ## matching columns (so a frame is accepted whether its columns are
  ## already this function's own dot-separated style, e.g. "date.mon", or
  ## came back snake_cased from some intervening save/reload step, e.g.
  ## "date_mon"), then every matched column is renamed, in this function's
  ## own local copy only, to the exact spelling this function's code
  ## already expects. See @details "BUGFIX (2026-09-21...)" above.
  data.canon        <- canonicalize.headers(data, DATA.REQUIRED)
  suntimes.canon    <- canonicalize.headers(suntimes, SUNTIMES.REQUIRED)
  fig.list.canon    <- canonicalize.headers(fig.list, FIG.LIST.REQUIRED)
  aes.default.canon <- canonicalize.headers(aes.default, AES.DEFAULT.REQUIRED)

  missing.msg <- function(canon, label) {
    if (length(canon$missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(canon$missing, collapse = ", ")))
    }
    NULL
  }

  check.parameters <- function(df, required, label) {
    if (!("parameter" %in% names(df))) return(NULL)  # already reported by missing.msg above
    missing <- setdiff(required, df$parameter)
    if (length(missing) > 0) {
      return(sprintf("%s is missing these required $parameter rows: %s - it may be an older copy missing settings added since it was last saved",
                      label, paste(missing, collapse = ", ")))
    }
    NULL
  }

  check.duplicates <- function(df, label) {
    nm <- names(df)
    dups <- unique(nm[duplicated(nm)])
    if (length(dups) > 0) {
      return(sprintf("%s has duplicate column name(s): %s - every column name must be unique",
                      label, paste(dups, collapse = ", ")))
    }
    NULL
  }

  ## check.duplicates runs on the ORIGINAL (pre-canonicalization) frames -
  ## a frame with duplicate column names is stopped on before any renaming
  ## is attempted on it, same as before this round's change.
  problems <- c(
    missing.msg(data.canon, "data"),
    missing.msg(suntimes.canon, "suntimes"),
    missing.msg(fig.list.canon, "fig.list"),
    missing.msg(aes.default.canon, "aes.default"),
    check.parameters(aes.default.canon$df, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
    check.duplicates(data, "data"),
    check.duplicates(suntimes, "suntimes"),
    check.duplicates(fig.list, "fig.list"),
    check.duplicates(aes.default, "aes.default")
  )
  if (length(problems) > 0) {
    ## blank line between each data frame's own missing-headers message,
    ## per Josh (2026-09-21) - was a single "\n" before this round.
    stop(paste(problems, collapse = "\n\n"))
  }

  data        <- data.canon$df
  suntimes    <- suntimes.canon$df
  fig.list    <- fig.list.canon$df
  aes.default <- aes.default.canon$df

  unquote <- function(x) {
    x <- trimws(as.character(x))
    gsub('^"(.*)"$', "\\1", x)
  }

  # 2026-08-27: Josh's own batz.suntimes_generate() writes $date/$suns/
  # $sunr/$sunr.mon in ISO format ("2026-05-15", "2026-05-15 19:56:29"),
  # not m/d/Y ("5/15/2026", "5/15/2026 19:56") - a real ISO-format
  # suntimes.csv silently produced 0 rows here (all dates parsed to NA
  # under a hardcoded "%m/%d/%Y" format) even though the aru/date range
  # genuinely overlapped. data/fig.list (hand-typed by Josh)
  # have so far always been m/d/Y, but parsing flexibly for all
  # date/datetime fields - mirroring the multi-format parse.simple.date()
  # approach already used in batz.suntimes_generate - costs nothing and
  # avoids the same landmine wherever a date field's actual source changes.
  parse.flex.date <- function(x) {
    out <- as.Date(rep(NA_character_, length(x)))
    for (fmt in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.Date(x, format = fmt)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  parse.flex.datetime <- function(x, tz) {
    out <- as.POSIXct(rep(NA_character_, length(x)), tz = tz)
    for (fmt in c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  ## Settings resolution (round nineteen, per Josh's 2026-09-16 follow-up):
  ## $aes.style names a FIXED column to check first (default
  ## "overide.value" - a blank column the user fills in directly on their
  ## own copy of the CSV), falling back to $default.value when that column
  ## doesn't exist or is blank for this row. This replaces the old
  ## project.name-matches-a-column-name mechanism entirely - project.name
  ## no longer has any role in settings resolution, only in the saved file
  ## name (see the main loop below).
  get.default <- function(param) {
    row.idx <- which(aes.default$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (aes.style %in% names(aes.default)) {
      override <- aes.default[[aes.style]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  get.setting <- function(job, param) {
    if (param %in% names(job)) {
      v <- job[[param]]
      if (!is.null(v) && !is.na(v) && nzchar(trimws(as.character(v)))) {
        return(as.character(v))
      }
    }
    get.default(param)
  }

  NE.ALIASES <- c("new england", "ne")
  # "Tri-colored bat" (with hyphen) is the reference table's actual $common
  # spelling - see @details above for why Josh's literal "Tricolored bat"
  # was corrected here.
  SPECIAL.FACPAN <- c("Big brown bat", "Eastern red bat", "Hoary bat", "Silver-haired bat",
                       "Eastern small-footed myotis", "Little brown bat",
                       "Northern long-eared bat", "Tri-colored bat")

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("fig.list has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

  ## per Josh ("I do not want 100% duplicate rows to produce multiple
  ## graphs... remove duplicate rows then go row by row producing 1 graph
  ## per row"): an exact full-row duplicate in fig.list (every column
  ## identical, not just $plot.name) is collapsed down to its first
  ## occurrence before any plotting happens - see Details/Follow-up.
  ## Order-preserving; a row that merely SHARES $plot.name with another
  ## row but differs in any other column (e.g. a different $date.start) is
  ## NOT a duplicate and is left completely alone.
  n.jobs.before.dedup <- nrow(jobs)
  jobs <- jobs[!duplicated(jobs), , drop = FALSE]
  n.fig.list.duplicates.removed <- n.jobs.before.dedup - nrow(jobs)
  if (n.fig.list.duplicates.removed > 0) {
    cat(sprintf("NOTE: removed %d exact duplicate row(s) from fig.list before plotting (%d distinct row(s) remain).\n",
                 n.fig.list.duplicates.removed, nrow(jobs)))
  }

  plots <- list()

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, ]
    job.label <- if (nzchar(trimws(job$plot.name))) job$plot.name else sprintf("row %d", j)
    ## job.key (not job.label!) is the list key `plots`/`ggplots` are stored
    ## under - job.label is only a DISPLAY string (used in messages/plot
    ## titles/file names) and is NOT guaranteed unique across fig.list rows
    ## (Josh's own real fig.list has every row sharing the identical
    ## $plot.name) - see Details/Follow-up for the real bug this caused
    ## when job.label itself was used as the list key.
    job.key <- as.character(j)

    if (!identical(tolower(trimws(job$plot.type)), "bat.detection")) {
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped (only 'bat.detection' is implemented so far).\n",
                   job.label, job$plot.type))
      next
    }

    facet.kind <- tolower(trimws(job$facet))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: fig.list row for '%s' has facet = '%s' - skipped ($facet = \"sppid\" is the only value implemented so far).\n",
                   job.label, job$facet))
      next
    }

    # ---- spp.plot / facpan ----
    facet.set.val <- tolower(trimws(job$facet.set))
    if (facet.set.val %in% NE.ALIASES) {
      facpan <- SPECIAL.FACPAN
    } else {
      facpan <- strsplit(get.setting(job, "facpan"), ";", fixed = TRUE)[[1]]
    }
    spp.plot <- facpan

    myso.flag <- isTRUE(as.logical(job$MYSO))
    if (myso.flag) spp.plot <- c(spp.plot, "Indiana Bat")

    alldect.flag <- isTRUE(as.logical(job$Alldect))
    if (alldect.flag) {
      spp.plot <- c(spp.plot, "All detections")
      facpan   <- c(facpan, "All detections")
    }

    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) {
      spp.plot <- c(spp.plot, "40khzmyo")
      if (!alldect.flag) facpan <- c(facpan, "40khzmyo")
    }

    spp.plot <- unique(trimws(spp.plot))
    facpan   <- unique(trimws(facpan))

    # Canonicalize to the reference table's own spelling (see @details) -
    # keeps a slightly-off list (typed by hand, or from an older spec) lined
    # up with data$spp.common, which is always canonical.
    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common")
    facpan   <- batz.batusa_recode.names(facpan, batname.format.out = "common")

    # ---- filter data to this job's ARU + species list ----
    pd <- data
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common")

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$group)) == tolower(plot.set.val), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end   <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    tz <- get.setting(job, "time.zone")

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated. Check that $group/$date in data actually overlap this row's $plot.set/$date.start/$date.end.\n",
                   job.label, plot.set.val, date.start, date.end))
      next
    }

    # The Y axis is "hour of monitoring night", the same Noon-to-Noon window
    # for every night regardless of its real calendar date - see @details.
    y.ref.date <- as.Date("1970-01-02")
    y.ref.noon <- as.POSIXct(paste(y.ref.date, "12:00:00"), tz = tz)
    pd$time.min <- y.ref.noon + pd$mins2.noon.min * 60
    pd$time.max <- y.ref.noon + pd$mins2.noon.max * 60

    # 40khzmyo rows always overlay in the "All detections" panel when that
    # panel exists; only get their own panel when it doesn't.
    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel,
                                    "All detections", pd$spp.common)
    pd$crossbar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")

    # ---- suntimes reference lines: one row per date, no facet column, so
    # ggplot2 repeats them across every panel ----
    sdb <- suntimes
    sdb$date.parsed <- parse.flex.date(sdb$date)
    if (nzchar(plot.set.val)) {
      sdb <- sdb[tolower(trimws(sdb$aru.name)) == tolower(plot.set.val), , drop = FALSE]
    }
    sdb <- sdb[!is.na(sdb$date.parsed) & sdb$date.parsed >= date.start & sdb$date.parsed <= date.end, , drop = FALSE]

    if (nrow(sdb) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' matched 0 rows of suntimes for plot.set = '%s' between %s and %s - Dawn/Dusk/Midnight reference lines will be empty. Check that suntimes's $aru.name/$date actually cover this plot.set/date range.\n",
                   job.label, plot.set.val, date.start, date.end))
    }

    dusk.real <- parse.flex.datetime(sdb$suns, tz)
    # Dawn ending THIS monitoring night (which starts at $suns/dusk of
    # $date) is $sunr.mon - sunrise on the FOLLOWING day - not $sunr, which
    # is sunrise ON $date itself (i.e. the dawn ending the PREVIOUS night).
    # Using $sunr here was a real bug caught during rendering: it placed
    # Dawn ~5 hours before Noon on the reference date, outside the plotted
    # Noon-to-Noon window, silently dropping the Dawn line from every panel.
    dawn.real <- parse.flex.datetime(sdb$sunr.mon, tz)
    local.noon <- as.POSIXct(paste(sdb$date.parsed, "12:00:00"), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + 12 * 3600

    # ---- facet panel labels, via batz.batusa_recode.names() ----
    # Every panel in facpan is shown even with 0 matching detections (see
    # @details) - so the full facpan list defines the facet levels.
    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (!nzchar(facet.label.fmt)) facet.label.fmt <- "common"

    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, batname.format.out = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw

    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels,
                                    labels = panel.labels[ordered.levels])

    # ---- y-axis settings (time-of-day) ----
    yaxe.limit.min <- get.setting(job, "yaxe.limit.min")
    yaxe.limit.max <- get.setting(job, "yaxe.limit.max")
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.min) || !grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.max)) {
      stop(sprintf(paste("$yaxe.limit.min/$yaxe.limit.max ('%s'/'%s') don't look like HH:MM time-of-day",
                          "values - aes.default may be an old, numeric-minutes-based copy of",
                          "plotopts_first.last.csv. Please use the current time-of-day version."),
                    yaxe.limit.min, yaxe.limit.max))
    }
    y.start <- as.POSIXct(paste(y.ref.date, yaxe.limit.min), tz = tz)
    y.end   <- y.start + 24 * 3600   # Noon-to-Noon, one full monitoring night

    plots[[job.key]] <- list(
      job.label = job.label,
      job = job,
      pd = pd,
      sdb = sdb,
      panel.labels = panel.labels,
      facpan = facpan,
      spp.plot = spp.plot,
      y.start = y.start,
      y.end = y.end,
      tz = tz,
      date.start = date.start,   # carried through so the X axis can be forced to this exact range below, not just whatever dates happen to have data
      date.end = date.end,
      khz.flag = khz.flag,   # carried through so the legend key below can be driven by "$40khzmyo is TRUE for this plot" rather than "a detection happened to occur" - see the follow-up note below
      resolved.legend.position = get.default("legend.position"),  # exposed for testing the aes.style-driven resolver (round nineteen) without needing to render/introspect a ggplot object
      resolved.dawn.color = get.default("dawn.color")              # exposed for testing the fall-through-to-default case
    )

    cat(sprintf("Prepared plot data for '%s': %d detection rows across %d panel(s), %d suntimes row(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw), nrow(sdb)))
  }

  if (length(plots) == 0) {
    cat("No plots were generated - see NOTE messages above.\n")
    return(invisible(list(plots = list(), ggplots = list())))
  }

  # ---------------------------------------------------------------------------
  # Rendering verified 2026-08-27 against real ggplot2 (r-cran-ggplot2), against
  # a synthetic ARU/date-aligned copy of the real test data - see @details.
  # ---------------------------------------------------------------------------
  ggplots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (job.key in names(plots)) {
      p <- plots[[job.key]]
      job.label <- p$job.label   # display name only - see job.key note above

      # Explicit y-axis breaks/labels (e.g. "Noon"/"Midnight" instead of
      # "12:00"/"00:00") - computed here rather than left to
      # scale_y_datetime's automatic date_breaks/date_labels, which can't
      # apply Josh's custom per-break label text.
      y.breaks <- seq(p$y.start, p$y.end, by = get.default("yaxe.break.interval"))
      y.break.labels <- strsplit(get.default("yaxe.break.labels"), ";", fixed = TRUE)[[1]]
      if (length(y.break.labels) != length(y.breaks)) {
        cat(sprintf("NOTE: '%s' - $yaxe.break.labels has %d label(s) but $yaxe.break.interval produces %d break(s) - falling back to $yaxe.labelformat-formatted times instead of the custom labels.\n",
                     job.label, length(y.break.labels), length(y.breaks)))
        y.break.labels <- format(y.breaks, get.default("yaxe.labelformat"))
      }

      # $date.format (e.g. "%b-%d/n%Y") is meant to break the x-axis date
      # label onto two lines - Josh's real plot.meta.csv writes the line
      # break as literal "/n" rather than an actual newline, which
      # strftime-based formatting (what scale_x_date's date_labels uses under
      # the hood) does not treat as an escape sequence, so it was rendering
      # as the literal two characters "/n" in the axis label instead of a
      # line break. Real bug caught by Josh after the first render - fixed
      # here by converting any literal "/n" in the format string to an
      # actual newline before it's used, rather than relying on the source
      # CSV always spelling it correctly.
      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)

      # 2026-08-27, per Josh ("plot.meta$xaxe.interval = 4 which should make
      # there only be four labeled dates on the X axes"): two real bugs here.
      # (1) $xaxe.interval was being read with get.default("xaxe.interval"),
      # never get.setting(p$job, "xaxe.interval") - so a plot's OWN
      # $xaxe.interval value (e.g. Josh's real plot.meta.csv row) was
      # silently ignored no matter what it said, always falling through to
      # aes.default's generic value instead - the exact same
      # class of settings-resolution bug as the "gome"/project.name mismatch
      # found earlier this session, just in a different call site that
      # never got updated when the get.setting()/get.default() split was
      # introduced. (2) The value itself was being fed straight into
      # scale_x_date(date_breaks = ...), which expects a ggplot2/scales
      # interval STRING ("4 days") - i.e. "one break every N days" - but
      # Josh's actual real value is the bare number 4, and his stated
      # intent is "N labeled dates total", a different axis (a COUNT of
      # breaks, not a day-spacing) that date_breaks has no way to express
      # directly. Fixed by computing N evenly-spaced Date breakpoints
      # explicitly across [date.start, date.end] (seq.Date's own
      # length.out= already lands on whole calendar days, first/last break
      # always exactly date.start/date.end) and passing them to
      # scale_x_date(breaks = ...) instead of date_breaks=.
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        cat(sprintf("NOTE: '%s' - $xaxe.interval = '%s' is not a usable number of x-axis labels - defaulting to 2 (just date.start/date.end).\n",
                     job.label, get.setting(p$job, "xaxe.interval")))
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))

      # $panel.border.linewidth (Theme category, default "0.5" - matches
      # ggplot2's own theme_bw() default for panel.border, so nothing
      # changes visually unless it's edited) is applied to the panel border
      # itself AND drives the Midnight line's linewidth, so the two are
      # guaranteed to match exactly (per Josh) rather than just visually
      # similar by coincidence.
      panel.border.lw <- as.numeric(get.default("panel.border.linewidth"))

      # 2026-08-27, per Josh ("the midnight line looks thicker than the box
      # line"): confirmed with a pixel-level measurement of a real rendered
      # PNG (integrated optical density across the stroke, not just eyeballing)
      # that a geom_line()/geom_hline() drawn with linewidth = X renders at
      # ~2x the actual pixel width of a theme_bw() panel.border drawn with
      # element_rect(linewidth = X) - same nominal value, genuinely different
      # rendered thickness (a ggplot2 rendering quirk between how "rect" theme
      # elements and geom line/segment strokes convert linewidth to on-page
      # width - reproduced in isolation with a controlled diagnostic script,
      # not specific to this plot's data). Halving the Midnight line's own
      # linewidth (panel border itself is untouched, still exactly
      # $panel.border.linewidth) was verified to bring the two to within
      # measurement noise (2.227px vs 2.225px in the diagnostic render) of
      # the same rendered width.
      midnight.render.lw <- panel.border.lw / 2

      # $midnight (Reference lines category, default "short") controls how
      # the Midnight reference line is drawn, per Josh:
      #   "none"  - don't plot it at all.
      #   "long"  - a single straight line spanning the full panel width,
      #             edge to edge (via geom_hline, which is unaffected by
      #             which/how many real suntimes dates are present).
      #   "short" - the original behavior: a line connecting each real
      #             suntimes date's (constant) midnight value, which is
      #             visually a flat line but only spans from the first to
      #             the last date actually present in suntimes for this
      #             plot - can fall short of the panel edges if that's
      #             narrower than the full date.start-date.end window.
      #   "dots"  - 2026-08-27, per Josh ("add an option to
      #             batactivity.plotoptions that makes the midnight line a
      #             series of grey dots"): a new mode, added the same way
      #             none/long/short were - one grey dot per real suntimes
      #             date present for this plot (same date coverage as
      #             "short", via geom_point instead of geom_line, so it can
      #             likewise fall short of the panel edges for the same
      #             reason). Uses its own $midnight.dots.color/
      #             $midnight.dots.size settings rather than reusing
      #             $midnight.color/$midnight.linetype, so it doesn't
      #             change what none/long/short already look like by
      #             default - flagging this interpretation to Josh: "a
      #             series of dots" was read as a literal geom_point()
      #             marker mode (a genuinely new, separate $midnight value),
      #             not as "set the existing line's linetype to dotted" -
      #             ggplot2's built-in "dotted" linetype on the existing
      #             short/long line would also visually read as a dotted
      #             line and needs no new code at all (already available
      #             via $midnight.linetype/$midnight.color) if that's what
      #             was actually meant instead.
      midnight.mode <- tolower(trimws(get.setting(p$job, "midnight")))
      if (!midnight.mode %in% c("none", "long", "short", "dots")) {
        cat(sprintf("NOTE: '%s' - $midnight = '%s' is not one of none/long/short/dots - defaulting to 'short'.\n",
                     job.label, get.setting(p$job, "midnight")))
        midnight.mode <- "short"
      }
      # Midnight is always exactly 12 hours after y.start (Noon of the
      # shared reference date) regardless of any specific real calendar
      # date, so it's computed directly from p$y.start rather than from
      # p$sdb - this also means "long" mode still works even when
      # suntimes has 0 matched rows for this plot (sdb would be empty).
      midnight.const <- p$y.start + 12 * 3600
      # "dots" resolves its own grey color independent of $midnight.color
      # (which none/long/short keep using, default black, unchanged) - see
      # the mode note above.
      midnight.legend.color <- if (midnight.mode == "dots") get.default("midnight.dots.color") else get.default("midnight.color")
      midnight.layer <- NULL
      if (midnight.mode == "short") {
        midnight.layer <- ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                               linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw,
                                               inherit.aes = FALSE)
      } else if (midnight.mode == "long") {
        midnight.layer <- ggplot2::geom_hline(data = data.frame(midnight.time = midnight.const),
                                                ggplot2::aes(yintercept = midnight.time, color = "Midnight"),
                                                linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw)
      } else if (midnight.mode == "dots") {
        # 2026-08-27, per Josh ("make midnight dots into thin dashes
        # instead"): shape 45 is the literal "-" (hyphen) character used as
        # a plotting glyph, rendering as a short horizontal dash rather
        # than a filled circle - visually reads as a dashed line broken
        # into one mark per real suntimes date, matching Josh's request.
        # $midnight = "dots" is kept as the setting's value name (unchanged,
        # so any existing config isn't broken) even though the rendered
        # glyph is now a dash, not a dot.
        midnight.layer <- ggplot2::geom_point(data = p$sdb, ggplot2::aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                                shape = 45, size = as.numeric(get.default("midnight.dots.size")),
                                                inherit.aes = FALSE)
      }

      # 2026-08-27 finding, caught by actually rendering the legend after
      # adding "dots" mode (not just reading the code): ggplot2's default
      # legend-key merging draws EVERY layer's key glyph onto EVERY row of
      # a shared discrete color guide, regardless of which layer's data
      # actually produced that row - confirmed with an isolated diagnostic
      # (geom_line() x2 + geom_point() sharing one colour aes: the two
      # line-only rows both picked up a stray point marker) and NOT fixed
      # by giving each layer its own explicit key_glyph (tried first -
      # made no visible difference, and had its own side effect: ggplot2
      # marks a key_glyph'd geom's class with a leading "" entry
      # internally, which would have broken introspection code checking
      # class(layer$geom)[1]). The real fix uses guide_legend(override.aes
      # = ...): the reference-line legend's break order is always
      # alphabetical (Dawn, Dusk, Midnight, since scale_color_manual here
      # declares no explicit breaks=) - a stable ggplot2 default, confirmed
      # by rendering - so shape can be pinned per-row by position: NA (no
      # marker) for Dawn/Dusk always, and for Midnight's own row, 16 (a
      # dot) only in "dots" mode, NA otherwise. Only built when Midnight
      # actually has a legend row at all (i.e. midnight.mode != "none",
      # matching how the legend already naturally excludes Midnight when
      # there's no midnight.layer).
      midnight.legend.shape <- if (midnight.mode == "dots") 45 else NA
      reference.line.override.shape <- if (midnight.mode == "none") c(NA, NA) else c(NA, NA, midnight.legend.shape)

      # $crossbar.fill.legend.title's legend should never show an "All
      # detections" key (it's the obvious default, not worth a legend
      # entry per Josh) and should show a "40kHzMyo" key whenever
      # $40khzmyo is on this plot's species list, colored black - Josh's
      # own original wording: "40kHzMyo if on species list should be [on
      # the legend] and colored black."
      #
      # 2026-08-27, per Josh ("40k Myo is missing from the legend"): this
      # was previously driven by whether a 40kHzMyo row actually survived
      # into p$pd (i.e. an actual detection happened to occur that
      # period) - MY OWN interpretive judgment call at the time, not what
      # Josh's own spec text literally says, and it meant a plot whose
      # real $40khzmyo flag is TRUE (on the species list) but which
      # simply had no 40kHzMyo detections that period showed no legend
      # key at all - exactly Josh's real plot.meta.csv/vetted.processed.csv
      # combination. Fixed to key off p$khz.flag ($40khzmyo itself,
      # carried through from the settings-resolution loop above) instead
      # of data presence.
      #
      # First fix attempt (breaks = "40kHzMyo" alone, no limits) LOOKED
      # right but was verified wrong with an isolated diagnostic: a
      # scale_fill_manual()'s breaks are silently dropped from the actual
      # rendered legend for any level that never appears in the mapped
      # data, regardless of what's declared in breaks= - confirmed by
      # rendering (not just introspecting get_breaks() on the unbuilt
      # scale, which is unreliable here the same way $labels$y was found
      # to be earlier this session) a bare geom_col() + scale_fill_manual
      # with breaks="B" but no "B" rows: no legend at all. Real fix
      # needs limits= to explicitly put "40kHzMyo" into the scale's
      # domain whenever the flag is TRUE, independent of whether any row
      # actually used that fill value that period - re-verified by
      # rendering with limits= added: the key shows correctly even with
      # zero 40kHzMyo detections.
      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed)) +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dusk.time, color = "Dusk"),
                             linetype = get.default("dusk.linetype"), inherit.aes = FALSE) +
        midnight.layer +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dawn.time, color = "Dawn"),
                             linetype = get.default("dawn.linetype"), inherit.aes = FALSE) +
        # width is pinned explicitly (rather than left to geom_crossbar's
        # default, which auto-computes it from resolution() - the smallest
        # gap between any two distinct dates actually present in the data)
        # because that default varies with which detection rows happen to
        # survive filtering for a given plot (e.g. a run with fewer
        # surviving rows and a bigger minimum date gap computed a WIDER
        # crossbar than the half-day padding on scale_x_date's limits (just
        # below) was sized for, clipping the boundary-day bars again - a
        # real bug caught during testing). Pinning width = 0.9 (ggplot2's
        # own default for daily-resolution data) makes the box size
        # predictable regardless of which/how many dates are present, so
        # the padding below is always enough.
        ggplot2::geom_crossbar(ggplot2::aes(ymin = time.min, ymax = time.max, y = time.min, fill = crossbar.type),
                                 linewidth = as.numeric(get.default("crossbar.linewidth")),
                                 width = 0.9) +
        ggplot2::scale_color_manual(name = get.default("reference.line.legend.title"),
                                      values = c("Dawn" = get.default("dawn.color"),
                                                 "Midnight" = midnight.legend.color,
                                                 "Dusk" = get.default("dusk.color"))) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(shape = reference.line.override.shape))) +
        ggplot2::scale_fill_manual(name = get.default("crossbar.fill.legend.title"),
                                     breaks = fill.legend.breaks,
                                     limits = fill.legend.limits,
                                     values = c("All detections" = get.default("crossbar.alldetections.fill"),
                                                "40kHzMyo" = get.default("crossbar.40khzmyo.fill"))) +
        ggplot2::scale_y_datetime(limits = c(p$y.start, p$y.end),
                                    breaks = y.breaks,
                                    labels = y.break.labels,
                                    name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        # Half-day padding on each side of date.start/date.end: geom_crossbar
        # draws each day's box at a fixed width around its date, so a bar
        # sitting exactly ON a hard scale limit gets half its box clipped to
        # NA (ggplot2's default out-of-bounds behavior for scale_x_date) -
        # caught via a real "Removed N rows containing missing values
        # (geom_segment())" warning on the first/last day's bars once the
        # limits below were added. The padding keeps the visible range
        # exactly matching date.start/date.end (no extra days shown) while
        # letting the boundary days' full-width bars render uncut.
        ggplot2::scale_x_date(limits = c(p$date.start - 0.5, p$date.end + 0.5),
                                breaks = xaxe.breaks,
                                date_labels = xaxe.date.labels.fmt,
                                name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        ggplot2::facet_wrap(~ facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        strip.background = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(linewidth = panel.border.lw, colour = "grey20", fill = NA),
                        legend.position = get.default("legend.position"),
                        plot.title = ggplot2::element_text(hjust = as.numeric(get.default("plot.title.hjust")),
                                                             size = as.numeric(get.default("plot.title.size"))),
                        axis.title = ggplot2::element_text(size = as.numeric(get.default("axis.title.size"))),
                        axis.text = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
                        legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
                        legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
                        panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt"))

      ggplots[[job.key]] <- g

      ## Round nineteen, per Josh (2026-09-16): every saved file name is now
      ## always "<project.name>_<ARU>_<timestamp>.png" -
      ## $output.filename.pattern is DEPRECATED and no longer read.
      fname <- sprintf("%s_%s_%s.png", project.name, trimws(p$job$plot.set), format(Sys.time(), "%Y%m%d_%H%M%S"))
      ## save into dir.save (default getwd(), i.e. unchanged behavior for
      ## existing callers) rather than always the working directory - see
      ## Details/Follow-up
      fname <- file.path(dir.save, fname)

      ggplot2::ggsave(fname, plot = g,
                        width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
                        height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
                        units = get.default("ggsave.units"),
                        dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

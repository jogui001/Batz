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
#'   \code{$aru.groupby}, \code{$obs}, \code{$mins2.noon.min},
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
#'   of \code{batz.suntimes_generate()}. Must have \code{$aru}, \code{$date},
#'   \code{$date.mon}, \code{$sunregion}, \code{$time.zone},
#'   \code{$sunregion.type}, \code{$schedual1}, \code{$schedual2},
#'   \code{$suns}, \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'   \code{$sunr.mon}, \code{$sunr.mon.unix}.
#' @param aes.default A data frame of default plot settings, one
#'   row per parameter (e.g. \code{plotoptions.batactivity.default.csv}).
#'   Must have \code{$category}, \code{$parameter}, \code{$default.value};
#'   \code{$notes} and any \code{project.name}-matching override column(s)
#'   are optional. Must be the TIME-OF-DAY version of this file (see
#'   Details) - an older, numeric-minutes version will fail with a clear
#'   error rather than silently plotting the wrong axis.
#' @param project.name Character, default \code{""}. Must EXACTLY match a
#'   real column name already present in \code{aes.default} (e.g.
#'   \code{"gome"}) - it is not a value looked up within some generic
#'   "project.name" column; it IS the column name itself. When it matches,
#'   that column's non-blank values override \code{$default.value} for
#'   matching parameters. A given plot row's OWN value in
#'   \code{fig.list} (when that column exists there and is non-blank)
#'   takes priority over both. See Details for the full three-tier
#'   precedence and a real bug this caught.
#' @param dir.save Character, default \code{getwd()}. Directory every
#'   generated PNG is saved into (each file's own name still comes from
#'   \code{aes.default}'s \code{$output.filename.pattern} - see Details).
#'
#' @return Invisibly, a list with \code{plots} (one entry per generated
#'   plot's prepared data - detection rows, suntimes rows, panel labels,
#'   resolved settings) and \code{ggplots} (the corresponding ggplot objects,
#'   only populated when the \code{ggplot2} package is available - see
#'   Details).
#'
#' @details
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
#' be \[on the legend\] and colored black"). Fixed to key off `$40khzmyo`
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
#' from \"batactivity.plotoptions.csv\" to \"plotopts_first.last.csv\""):
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
#' they are a historical record, not current guidance. The project's own
#' saved master copy (`claude/plotoptions.batactivity.default.csv`) keeps
#' its existing, separate name - only the merged file Josh loads as
#' `aes.default` was renamed. Full test suite re-run clean (all 11
#' scenarios, no regressions) after the rename - no functional change,
#' purely a file-naming change.
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
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.plotdetections_first.last()}: family = "plotdetections" (the
#' verb "plot" is baked into the family name, same pattern as
#' \code{batz.plotframe_batactivity}), subject = "first.last" (each
#' species' first and last nightly detection). Requested as
#' \code{batz.plotdections_first.last()} - "plotdections" was a typo for
#' "plotdetections", fixed here; nothing else changed.
#'
#' @examples
#' \dontrun{
#' # default dir.save = getwd() - saves into the current working directory,
#' # same as every call before dir.save existed
#' result <- batz.plotdetections_first.last(
#'   data = vetted.processed,
#'   fig.list = plot.meta,
#'   suntimes = aru.suntimes,
#'   aes.default = batactivity.plotoptions
#' )
#' result$ggplots[[1]]
#'
#' # explicit dir.save, if the PNGs should land somewhere else
#' result <- batz.plotdetections_first.last(
#'   data = vetted.processed,
#'   fig.list = plot.meta,
#'   suntimes = aru.suntimes,
#'   aes.default = batactivity.plotoptions,
#'   dir.save = "C:/path/to/output/folder"
#' )
#' }
#'
#' @export
batz.plotdetections_first.last <- function(data, fig.list, suntimes,
                                            aes.default, project.name = "",
                                            dir.save = getwd()) {

  DATA.REQUIRED <- c("spp.id", "date", "aru.groupby", "obs",
                           "mins2.noon.min", "mins2.noon.max", "vetting.type")
  SUNTIMES.REQUIRED <- c("aru", "date", "date.mon", "sunregion", "time.zone",
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
    "output.filename.pattern", "plot.width", "plot.height"
  )

  check.headers <- function(df, required, label) {
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
    }
    NULL
  }

  check.parameters <- function(df, required, label) {
    if (!("parameter" %in% names(df))) return(NULL)  # already reported by check.headers above
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

  problems <- c(
    check.headers(data, DATA.REQUIRED, "data"),
    check.headers(suntimes, SUNTIMES.REQUIRED, "suntimes"),
    check.headers(fig.list, FIG.LIST.REQUIRED, "fig.list"),
    check.headers(aes.default, AES.DEFAULT.REQUIRED, "aes.default"),
    check.parameters(aes.default, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
    check.duplicates(data, "data"),
    check.duplicates(suntimes, "suntimes"),
    check.duplicates(fig.list, "fig.list"),
    check.duplicates(aes.default, "aes.default")
  )
  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n"))
  }

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

  get.default <- function(param) {
    row.idx <- which(aes.default$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (nzchar(project.name) && project.name %in% names(aes.default)) {
      override <- aes.default[[project.name]][row.idx[1]]
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

  plots <- list()

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, ]
    job.label <- if (nzchar(trimws(job$plot.name))) job$plot.name else sprintf("row %d", j)

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
    spp.plot <- batz.batusa_recode.names(spp.plot, output.format = "common")
    facpan   <- batz.batusa_recode.names(facpan, output.format = "common")

    # ---- filter data to this job's ARU + species list ----
    pd <- data
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, output.format = "common")

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$aru.groupby)) == tolower(plot.set.val), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end   <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    tz <- get.setting(job, "time.zone")

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated. Check that $aru.groupby/$date in data actually overlap this row's $plot.set/$date.start/$date.end.\n",
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
      sdb <- sdb[tolower(trimws(sdb$aru)) == tolower(plot.set.val), , drop = FALSE]
    }
    sdb <- sdb[!is.na(sdb$date.parsed) & sdb$date.parsed >= date.start & sdb$date.parsed <= date.end, , drop = FALSE]

    if (nrow(sdb) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' matched 0 rows of suntimes for plot.set = '%s' between %s and %s - Dawn/Dusk/Midnight reference lines will be empty. Check that suntimes's $aru/$date actually cover this plot.set/date range.\n",
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
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, output.format = facet.label.fmt)
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

    plots[[job.label]] <- list(
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
      khz.flag = khz.flag   # carried through so the legend key below can be driven by "$40khzmyo is TRUE for this plot" rather than "a detection happened to occur" - see the follow-up note below
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
    for (job.label in names(plots)) {
      p <- plots[[job.label]]

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
      # always exactly date.start/date.end) and passing those as
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
        # glyph is now a dash, not a circle.
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
      # class(layer$geom)[1]). The real fix is guide_legend(override.aes
      # = ...): the reference-line legend's break order is always
      # alphabetical (Dawn, Dusk, Midnight, since scale_color_manual below
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
      # actually used that fill value - re-verified by rendering with
      # limits= added: the key shows correctly even with zero 40kHzMyo
      # detections.
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
                        # 2026-08-27, later still - a documentation
                        # correction, NOT a behavior change: while
                        # investigating Josh's "eastern small footed myotis
                        # is cut off" report, found that the
                        # aes.default note on $axis.title.size has
                        # always incorrectly claimed strip.text (the facet
                        # panel title) "reuses" $axis.title.size - confirmed
                        # via ggplot2's own get_element_tree() and a minimal
                        # test plot that this was never true (strip.text
                        # inherits from "text", not "title"/axis.title;
                        # changing axis.title.size has zero effect on it). An
                        # explicit strip.text = element_text(size =
                        # axis.title.size) was tried here to make the
                        # documented behavior real, but re-rendering showed
                        # it made the cutoff WORSE, not better - ggplot2's
                        # own actual fixed strip.text size (rel(0.8) of
                        # theme_bw()'s base_size 11 = 8.8pt) is SMALLER than
                        # $axis.title.size's default of 10, so binding them
                        # enlarged the title instead of shrinking it.
                        # Reverted: strip.text is left at ggplot2's native,
                        # non-configurable size, and the $axis.title.size CSV
                        # note is corrected below to describe what the code
                        # actually does, instead of changing the code to
                        # match a stale, inaccurate note.
                        legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
                        legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
                        # 2026-08-27, per Josh ("labels on the Xaxes ... do
                        # not appear to be the real dates rather labels
                        # rewriting the dates"): this was NOT a data/parsing
                        # bug - every break IS the real date.start/date.end-
                        # derived calendar date, confirmed by inspecting the
                        # actual pixel text - the real defect was a LAYOUT
                        # collision. Forcing the first/last x-axis break to
                        # sit exactly at date.start/date.end (the earlier
                        # $xaxe.interval fix, by design) means the rightmost
                        # label of one facet panel is centered right at that
                        # panel's shared border with the next panel - with
                        # theme_bw()'s default (~5.5pt) panel spacing, the
                        # two-line label's own width bleeds across that
                        # border and overlaps the neighboring panel's
                        # leftmost label, so e.g. "May-27\n2026" visually
                        # smashes into the next panel's "May-08\n2026" and
                        # reads as garbled/wrong text, even though both
                        # dates are individually correct.
                        #
                        # REVISED 2026-08-27, later still, per Josh ("that is
                        # worse, I only get a box now not a plot... revert
                        # back to the previous plot dimensions and reduce the
                        # size of the labels on the x and y axis until there
                        # is no overlap"): the first fix widened
                        # $panel.spacing.x from theme_bw()'s ~5.5pt default to
                        # 40pt, which stopped the label collision but - since
                        # this function's overall saved figure width is a
                        # FIXED size ($plot.width + $ggsave.width.pad, not
                        # something that grows with the number of
                        # panels/gaps) - shrank every panel's own width to
                        # make room for the wider gaps, which in turn made
                        # the "Eastern small-footed myotis" facet title too
                        # wide for its now-narrower panel and cut it off. Per
                        # Josh's explicit correction, $panel.spacing.x is
                        # reverted to theme_bw()'s own built-in "5.5" default
                        # (restores the original panel/figure dimensions
                        # exactly - this is a plain value revert, not a
                        # removal, so the setting stays available to
                        # override later if ever needed) and the actual
                        # label-collision fix now comes from shrinking
                        # $axis.text.size instead (8 -> 6, see that
                        # parameter's own updated default/notes) - smaller
                        # text needs less horizontal room, so the two-line
                        # date labels clear each other even at the original
                        # tight spacing. Re-verified by rendering: panels are
                        # back to their original width (species title no
                        # longer cut off) and the x-axis labels still don't
                        # collide at any panel boundary.
                        panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt"))

      ggplots[[job.label]] <- g

      pattern <- get.default("output.filename.pattern")
      fname <- pattern
      fname <- gsub("<ARU>", trimws(p$job$plot.set), fname, fixed = TRUE)
      fname <- gsub("<date.start>", as.character(min(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<date.end>", as.character(max(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<timestamp>", format(Sys.time(), "%Y%m%d%H%M%S"), fname, fixed = TRUE)
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

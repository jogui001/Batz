#' Plot the nightly number of observations per species
#'
#' Generates the standard report plot showing, for every species (plus an
#' "All detections" panel and an optional overlaid 40kHzMyo indicator), the
#' number of observations (\code{$obs}) recorded each monitoring night, as a
#' bar per night per panel. One plot is produced per row of \code{fig.list}
#' whose \code{$plot.type} is \code{"call.observations"} (see Details for why
#' this value was chosen and how to change it).
#'
#' @param data A data frame of already-summarized per-species, per-night
#'   observation counts. Must have \code{$spp.id}, \code{$date},
#'   \code{$obs}, plus whatever column each \code{fig.list} row's own
#'   \code{$plot.group} names (see Details - this is no longer a fixed,
#'   hardcoded column).
#' @param fig.list A data frame listing the plot(s) to generate - one row
#'   per plot. Must have \code{$plot.type}, \code{$plot.name}, \code{$facet},
#'   \code{$facet.set}, \code{$MYSO}, \code{$Alldect}, \code{$facet.panel},
#'   \code{$40khzmyo}, \code{$facet.label}, \code{$plot.group},
#'   \code{$plot.sets}, \code{$pool}, \code{$date.format}, \code{$date.start},
#'   \code{$date.end}, \code{$xaxe.interval}. Column names must be unique. Five further
#'   columns are read PER-ROW if present but are entirely optional (each
#'   falls back to \code{aes.default} when blank or the column doesn't
#'   exist at all - see Details): \code{$Yaxe.trans} (\code{"none"}/
#'   \code{"log"}/\code{"log10"}), \code{$y.scale} (\code{"regular"}/
#'   \code{"rounded"}/\code{"custom"}), \code{$y.custom} (semicolon-separated
#'   break values, only read when \code{$y.scale = "custom"}),
#'   \code{$ymax} (the top value plotted on the Y axis), and \code{$legend}
#'   (\code{TRUE}/\code{FALSE} - whether to show a legend distinguishing
#'   which \code{$plot.sets} value each dodged bar is, when \code{$pool =
#'   FALSE} and more than one value is selected - see \strong{Follow-up,
#'   2026-08-29} in Details).
#' @param suntimes A data frame of sunrise/sunset times, e.g. the output of
#'   \code{batz.suntimes_generate()}. Must have \code{$aru}, \code{$date},
#'   \code{$date.mon}, \code{$sunregion}, \code{$time.zone},
#'   \code{$sunregion.type}, \code{$schedual1}, \code{$schedual2},
#'   \code{$suns}, \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'   \code{$sunr.mon}, \code{$sunr.mon.unix}. \strong{Accepted and header-
#'   checked, but not otherwise used yet this iteration} - see Details.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_callobs.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value};
#'   \code{$notes} and any \code{project.name}-matching override column(s)
#'   are optional.
#' @param project.name Character, default \code{""}. Must EXACTLY match a
#'   real column name already present in \code{aes.default} - see
#'   \code{batz.plotdetections_first.last()}'s own \code{@param} docs for
#'   the full three-tier precedence (this function resolves settings the
#'   same way: \code{fig.list} row > \code{project.name} column >
#'   \code{$default.value}).
#' @param dir.save Character, default \code{getwd()}. Directory every
#'   generated PNG is saved into (each file's own name still comes from
#'   \code{aes.default}'s \code{$output.filename.pattern}).
#' @param bar.border Logical, default \code{FALSE}. Only has any visible
#'   effect when bars are being dodged by \code{$group.val} (see the
#'   \code{$legend} follow-up below) - that is the only case where a bar
#'   outline is ever drawn. \code{FALSE} (default) draws no outline there
#'   either: each dodged bar's own \code{$group.val} color becomes its
#'   FILL instead. \code{TRUE} restores the original bordered look (grey
#'   fill + a separate outline-color legend) - see Details,
#'   \strong{Follow-up, 2026-08-30}.
#'
#' @return Invisibly, a list with \code{plots} (one entry per generated
#'   plot's prepared data) and \code{ggplots} (the corresponding ggplot
#'   objects, only populated when the \code{ggplot2} package is available).
#'
#' @details
#' \strong{Follow-up, 2026-08-30 - constant bar width across dates, and a
#' \code{bsize} filename prefix.} \code{position_dodge2()}'s default
#' \code{preserve = "total"} divides one fixed total width among however
#' many \code{$group.val} bars are present at a given date - so dates
#' with more detectors reporting got visibly thinner dodged bars than
#' dates with fewer, even though \code{bar.width.val} itself hadn't
#' changed. \code{preserve = "single"} is now passed instead, which fixes
#' each individual bar's own width at \code{bar.width.val} regardless of
#' how many detectors are present on that date. Separately, every saved
#' PNG's filename now gets a \code{bsize<value>_} prefix (e.g.
#' \code{"bsize0.8_..."}) using that job's own resolved
#' \code{bar.width.val}, rounded to 2 decimal places - useful since
#' \code{bar.width.val} can auto-size (see the \code{bar.width} follow-up
#' below) and so can differ between jobs even when \code{$bar.width} is
#' left blank for both.
#'
#' \strong{Follow-up, 2026-08-30 - \code{bar.border}, corrected.} A bar
#' outline was ALWAYS only ever drawn in one place: the dodged-by-
#' \code{$group.val} case (\code{$pool = FALSE}, \code{$legend = TRUE},
#' more than one \code{$plot.sets} value selected - see the \code{$legend}
#' follow-up below). The plain, non-dodged bars have never had an outline,
#' with or without \code{bar.border}. A first pass at this option only
#' swapped the plain bars' fill and deliberately left the dodge outline
#' alone; real output showed that was the wrong scope - the dodge outline
#' IS the border users see and want control over, so \code{bar.border} now
#' governs it directly:
#' \itemize{
#'   \item \code{FALSE} (default): no outline is drawn on the dodged bars
#'     either. Each dodged bar's own \code{$group.val} color - the same
#'     \code{$legend.groupval.colors} palette that used to color the
#'     outline - becomes its FILL instead, combined with the fixed
#'     "40kHzMyo" black into one fill legend (ggplot2 only supports one
#'     fill scale per plot, so this is a single combined key rather than a
#'     second scale). \code{$bar.alldetections.fill} is not used in this
#'     branch.
#'   \item \code{TRUE}: restores the original bordered look - uniform grey
#'     "All detections" fill (\code{$bar.alldetections.fill}) plus a
#'     separate colour-mapped outline legend distinguishing
#'     \code{$group.val} (titled from \code{$legend.groupval.title}).
#' }
#' \code{$bar.40khzmyo.fill} is a fixed color either way - it never varies
#' by \code{$group.val} - and is always drawn as the second/top
#' \code{geom_col()} layer (see the draw-order note below), in every
#' branch. Non-dodged jobs (single \code{$plot.sets} value, \code{$pool =
#' TRUE}, or \code{$legend = FALSE}) are unaffected by \code{bar.border} in
#' either direction, since there was never a border there to begin with -
#' \strong{FLAGGED for Josh}: confirm that scoping (no effect on non-dodged
#' plots) is correct, or say if a plain single-color plot should also get a
#' configurable border/fill-swap of its own.
#'
#' \strong{Follow-up, 2026-08-30 - \code{bar.width} auto-size.} A blank
#' \code{$default.value} for \code{bar.width} (and no usable per-plot
#' override) now means "auto-size to fit the plot window" instead of
#' erroring out via \code{as.numeric("")}. Computed per job from that
#' job's own actual plotted date spacing (the median gap between its
#' sorted, unique dates, x0.9 to leave a visible gap between adjacent
#' dates' bars) rather than one hardcoded guess, so it stays reasonable
#' whether monitoring nights are daily, every-other-day, weekly, etc. -
#' this also means bar width can now differ between two \code{fig.list}
#' rows/plots if their date spacing differs, even with the same blank
#' \code{$bar.width}. Falls back to the file's own prior flat \code{0.8}
#' default if a job has fewer than two distinct dates to measure a gap
#' from.
#'
#' \strong{Iteration 1 ("basic layout"), built 2026-08-28 per Josh's own
#' framing that this function would be developed iteratively, copying the
#' structure/steps of \code{batz.plotdetections_first.last()} and modifying
#' it for a count-based Y axis instead of a time-of-night Y axis.} Carried
#' over UNCHANGED from that function: header/duplicate-column validation,
#' settings resolution (\code{fig.list} row > \code{project.name} column >
#' \code{$default.value}), the New-England-special-case \code{$facpan} list,
#' \code{$MYSO}/\code{$Alldect}/\code{$40khzmyo} panel-building logic,
#' facet labeling and canonicalization via
#' \code{batz.batusa_recode.names()}, \code{$plot.order} panel ordering, the
#' \code{job.key}/\code{job.label} list-keying fix (rows sharing
#' \code{$plot.name} each still get their own plot), and the exact-full-row
#' \code{fig.list} duplicate-removal step (both fixed in that function after
#' real bugs Josh hit - see its own \code{@details} for the full history;
#' both are included here from the start rather than waiting to be
#' rediscovered). REMOVED entirely: the Dawn/Dusk/Midnight reference lines
#' and all of the time-of-night (\code{mins2.noon.min}/\code{max},
#' sunrise/sunset) math that supported them - per Josh's explicit
#' instruction ("There are no DAWN, DUSK or Midnight variables to be
#' ploted"). Each bar is simply one \code{fig.list}-matched data row's own
#' \code{$obs} value, plotted at its \code{$date} - no per-night aggregation
#' happens inside this function (a night's "All detections" bar is
#' \code{data}'s own pre-computed \code{$spp.id = "All Detections"} row for
#' that night/ARU, exactly parallel to how \code{batz.plotdetections_first.last()}
#' uses a pre-computed \code{"All detections"} row rather than summing
#' individual species rows itself).
#'
#' \strong{\code{$plot.type = "call.observations"}} was chosen for this
#' function's own \code{fig.list} rows (a judgment call, since Josh's spec
#' didn't name one) - \code{fig.list.csv} is shared across every \code{batz}
#' plotting function, each reading only the rows matching its own
#' \code{$plot.type} value and skipping (with a console \code{NOTE}, not an
#' error) any row belonging to another function, exactly like
#' \code{batz.plotdetections_first.last()} already does for
#' \code{"bat.detection"}. \strong{Josh's real \code{fig.list.csv} does not
#' yet have a row with this \code{$plot.type} value} - add one (with
#' \code{$Yaxe.trans}/\code{$y.scale}/\code{$y.custom}/\code{$ymax} filled
#' in as needed - those four columns already exist as blank headers in the
#' delivered file) before calling this function against real data. If a
#' different \code{$plot.type} string is wanted instead, it only needs to be
#' changed in one place (the \code{PLOT.TYPE} constant near the top of this
#' function's code).
#'
#' \strong{\code{suntimes} is accepted and header-checked but not otherwise
#' used in this iteration.} Josh's own spec listed it as an input alongside
#' \code{data}/\code{fig.list}/\code{aes.default}, but with no Dawn/Dusk/
#' Midnight lines to compute, nothing in this first pass actually needs
#' sunrise/sunset data - kept in the signature (rather than dropped) so
#' existing call sites built against the same four core inputs as the
#' sibling function keep working, and so a future iteration (e.g. excluding
#' nights with no suntimes coverage, or a twilight-shading feature) can add
#' real use of it without an interface change. \strong{Flagging this
#' explicitly for Josh}: if \code{suntimes} should actually do something in
#' this plot (e.g. gray out or omit un-monitored nights), say so and it can
#' be wired in.
#'
#' \strong{Follow-up, 2026-08-28, per Josh: \code{$plot.group}/
#' \code{$plot.sets}/\code{$pool} replace the old fixed \code{$aru.groupby}/
#' \code{$plot.set} single-detector-column design.} Previously, this
#' function always filtered \code{data} on a hardcoded \code{$aru.groupby}
#' column, matched against a single \code{$plot.set} value. Three
#' \code{fig.list} columns now generalize this:
#' \itemize{
#'   \item \code{$plot.group} names WHICH column of \code{data} to filter/
#'     group by, per \code{fig.list} row - e.g. \code{"aru.name"} to plot by
#'     detector (matching \code{\link{batz.generate_plotframe.bat}}'s own
#'     \code{groupby} parameter and its \code{$groupedby} output column) or
#'     \code{"deployment.type"} to plot by some other category entirely.
#'     This is what gives a single \code{fig.list}/this function the
#'     flexibility to plot other sheets/groupings without a code change -
#'     \code{data} no longer needs a column literally called
#'     \code{$aru.groupby} at all. A row with a blank \code{$plot.group}, or
#'     one naming a column \code{data} doesn't actually have, is skipped
#'     with a console \code{NOTE} (same tolerant, skip-don't-crash style
#'     used for an unrecognized \code{$plot.type}/\code{$facet}).
#'   \item \code{$plot.sets} (renamed from the old singular
#'     \code{$plot.set}) lists every value of the \code{$plot.group} column
#'     to include in this plot - now MULTIPLE values, not just one. Every
#'     double-quote character in the raw value is treated as a token
#'     delimiter (alongside whitespace) and stripped, so
#'     \code{"105059-NW3" "105059-SE3" "105059-SW3"}-shaped values parse
#'     into three tokens - this was deliberately NOT implemented as
#'     matching well-formed quote PAIRS, because Josh's own real
#'     \code{fig.list.csv} value for this column, once CSV-unescaped, comes
#'     through missing the very first value's leading quote (the field's
#'     own outer CSV quoting consumes it) - a quote-pair parser would
#'     silently drop that first token on real data. A single bare,
#'     unquoted value (e.g. \code{105059-NW3}, exactly like the old
#'     \code{$plot.set}) or several bare whitespace-separated values with
#'     no quoting at all also work. A blank \code{$plot.sets} matches every
#'     value of the \code{$plot.group} column (same as the old
#'     blank-\code{$plot.set} behavior). \strong{Limitation, flagged for
#'     Josh}: a value containing a literal space would be split into two -
#'     not expected for detector/group names, but worth knowing.
#'   \item \code{$pool} (\code{TRUE}/\code{FALSE}) controls how the
#'     (possibly several) selected \code{$plot.sets} values are combined:
#'     \code{TRUE} sums \code{$obs} across all of them into ONE pooled bar
#'     per date/panel (as if they were a single group); \code{FALSE} keeps
#'     each selected value as its own bar, drawn side-by-side (dodged)
#'     within the same date/panel. Bars are dodged via \code{ggplot2}'s own
#'     \code{position_dodge2()} at each date; fill still only distinguishes
#'     \code{"All detections"} vs \code{"40kHzMyo"}, exactly as before -
#'     WHICH \code{$plot.sets} value a given dodged bar is is now shown via
#'     a separate outline-color legend, when \code{$legend} is on - see
#'     \strong{Follow-up, 2026-08-29} below.
#' }
#'
#' \strong{Follow-up, 2026-08-29, per Josh ("add legend option to
#' function, legend = TRUE; add legend options to plotops file for things
#' like size and colors"): a new \code{$legend} column (per-\code{fig.list}
#' row, optional - falls back to \code{aes.default}'s own \code{legend}
#' parameter, default \code{TRUE}), and three new \code{aes.default}
#' parameters, resolve the "no legend for dodged bars" gap flagged
#' above.} When \code{$pool = FALSE} and more than one \code{$plot.sets}
#' value is actually selected (nothing to distinguish otherwise - a single
#' value, or a pooled bar, never shows this legend regardless of
#' \code{$legend}), every dodged bar's OUTLINE color is mapped to its own
#' \code{$plot.group} value (the same raw value carried as \code{$group}
#' by \code{\link{batz.generate_plotframe.bat}}, when \code{data} comes
#' from there) via a second, independent \code{ggplot2} color scale -
#' \code{fill} is already used for \code{"All detections"}/\code{"40kHzMyo"}
#' and is left completely alone, so both legends coexist without
#' conflict. Three new \code{aes.default} parameters, category
#' \code{"Legend"} (alongside the existing \code{legend.position}),
#' control it: \code{legend.groupval.title} (the new legend's title
#' text), \code{legend.groupval.colors} (a semicolon-separated list of hex
#' colors, e.g. \code{"#1b9e77;#d95f02;#7570b3"} - assigned to each
#' distinct selected value in sorted order, CYCLING back to the first
#' color if there are more selected values than colors given), and
#' \code{legend.groupval.outline.linewidth} (how thick that outline stroke
#' is drawn - "size" in Josh's own wording). \strong{Two interpretive
#' calls, flagged for Josh}: (1) the new legend distinguishes
#' \code{$plot.sets} values by bar OUTLINE color rather than fill, since
#' \code{fill} was already spoken for by the all-detections/40kHzMyo
#' distinction - a shared "fill" legend covering both dimensions at once
#' isn't something \code{ggplot2} supports natively without extra
#' packages; if a single combined legend is what's actually wanted instead,
#' say so. (2) \code{legend.groupval.colors} cycling (rather than erroring,
#' or auto-generating additional colors) when there are more selected
#' \code{$plot.sets} values than colors listed was chosen to keep behavior
#' predictable and non-fatal; a plot with more distinct values than
#' distinct colors will have two values sharing a color, which is worth
#' knowing about if it happens. Verified with a new test: 3 selected
#' \code{$plot.sets} values, 3 configured colors, each value gets its own
#' distinct outline color and shows up in the legend; \code{$legend =
#' FALSE} (or a single selected value, or \code{$pool = TRUE}) renders
#' exactly as before this change, with no outline-color mapping/legend at
#' all.
#'
#' \strong{Y-axis resolution - the newest, most detailed part of this
#' function - implements Josh's spec as follows:} \code{$Yaxe.trans}
#' (\code{"none"} default / \code{"log"} / \code{"log10"}) picks a transform
#' applied to \code{$obs} before it's used as a bar's plotted height;
#' \strong{\code{"log"}/\code{"log10"} are implemented as \code{log1p(obs)}/
#' \code{log10(obs + 1)}, not a bare \code{log(obs)}/\code{log10(obs)}} - a
#' deliberate choice (Josh's spec didn't say how to handle a night with 0
#' observations, but \code{data} can and does have real detections that
#' would sum to a whole-number 0 for some species/night combinations
#' logically, and a bare \code{log(0)} is \code{-Inf}, which would break the
#' axis) - verified against Josh's own target image
#' (\code{"Number of bat calls detected.png"}): its Y-axis breaks
#' (\code{0, 2, 8, 25, 75, 230}) are spaced almost exactly evenly once run
#' through \code{log1p()} (successive gaps of about 1.10, 1.10, 1.06, 1.07,
#' 1.11 log-units), confirming this is the transform that image's axis
#' actually used, not a coincidence. \code{$loglabels} (\code{FALSE}
#' default) then controls whether each break is LABELED with the real
#' \code{$obs}-scale number (\code{FALSE}) or the transformed value itself
#' (\code{TRUE}) - it only has any effect when \code{$Yaxe.trans} isn't
#' \code{"none"}, per Josh's spec.
#'
#' \code{$y.scale} picks which values become breaks: \code{"regular"}
#' places them at 5 points evenly spaced along the AXIS itself - i.e. evenly
#' spaced in whatever space \code{$Yaxe.trans} plots in, then converted back
#' to a raw count for the label (see the BUGFIX entry below for why this
#' matters); \code{"rounded"} places them at those same five axis positions
#' but rounds each resulting raw value to the nearest whole number
#' afterward (matters once the exact value isn't a whole number already,
#' e.g. \code{$ymax = 230} with \code{$Yaxe.trans = "none"} gives a raw 25\%
#' point of \code{57.5}, which \code{"rounded"} shows as \code{58});
#' \code{"custom"} ignores that computation entirely and uses whatever
#' numbers are in \code{$y.custom} instead (semicolon-separated, e.g.
#' \code{"0;2;8;25;75;230"} - the same convention as
#' \code{$yaxe.break.labels} in \code{batz.plotdetections_first.last()}),
#' matching Josh's target image exactly. \strong{Josh's own spec text marked
#' BOTH \code{"regular"} and \code{"rounded"} as \code{"(default)"} - almost
#' certainly a copy/paste slip, since only one can be the actual default.}
#' \code{"regular"} was picked as the real default here (it's the option
#' listed first, and is the more literal/simpler reading of "0,0.25,0.5,
#' 0.75,1"); \strong{Josh: please confirm this is the one you meant as the
#' default}, since the two only visibly differ when the exact 25/50/75\%
#' axis positions don't land on whole numbers.
#'
#' \strong{BUGFIX, 2026-08-28, per Josh ("scale is not working as expected.
#' The attached file scale jumps from the bottom of the Y axis to almost
#' the top. It should be evenly spaced along the y axes"): a real bug in
#' how \code{"regular"}/\code{"rounded"} computed break VALUES, found from
#' Josh's own real render with \code{$Yaxe.trans = "log10"},
#' \code{$y.scale = "regular"}, \code{$ymax = 230}.} The first version of
#' this function computed \code{"regular"}/\code{"rounded"} breaks as 5
#' values evenly spaced across the RAW count range (\code{0}, 25\%, 50\%,
#' 75\%, 100\% of \code{$ymax} - i.e. \code{0, 57.5, 115, 172.5, 230}), then
#' transformed only their AXIS POSITIONS through \code{$Yaxe.trans}. That's
#' fine when \code{$Yaxe.trans = "none"} (position and value are the same
#' thing), but once a log-family transform is applied, evenly-spaced RAW
#' values do NOT produce evenly-spaced POSITIONS - quite the opposite:
#' \code{log10(1) = 0} but \code{log10(58.5) = 1.77}, already most of the
#' way to \code{log10(231) = 2.36}, so the break from \code{0} to
#' \code{57.5} visually spanned almost the entire axis while \code{57.5},
#' \code{115}, \code{172.5}, and \code{230} all crowded into the remaining
#' sliver at the top - exactly what Josh described. \strong{Fixed} by
#' computing \code{"regular"}/\code{"rounded"} breaks the other way around:
#' 5 points evenly spaced along the TRANSFORMED axis itself (from
#' \code{$Yaxe.trans} applied to \code{0} to \code{$Yaxe.trans} applied to
#' \code{$ymax}), converted back to a raw count afterward for the label.
#' This guarantees genuinely even spacing on the rendered axis regardless
#' of \code{$Yaxe.trans}, and is a no-op change for \code{$Yaxe.trans =
#' "none"} (where the transform is the identity function, so "evenly spaced
#' in raw units" and "evenly spaced on the axis" were already the same
#' thing) - only \code{"log"}/\code{"log10"} renders differently now.
#' \strong{This means \code{"regular"}/\code{"rounded"} can now produce a
#' nicely-spaced log axis on their own, without needing hand-picked
#' \code{$y.custom} values} the way Josh's own target image did - though
#' \code{"custom"} is still there for full manual control, e.g. round
#' numbers instead of whatever the automatic 0/25/50/75/100\% axis points
#' happen to compute to. Verified by rendering Josh's own real
#' \code{fig.list} row (\code{$Yaxe.trans = "log10"}, \code{$y.scale =
#' "regular"}, \code{$ymax = 230}) before and after: before, the break
#' positions computed to 0\%/74.8\%/87.3\%/94.7\%/100\% of the axis height
#' (all but the first crowded into the top quarter); after, they compute to
#' exactly 0\%/25\%/50\%/75\%/100\%, genuinely even.
#'
#' \strong{A second, related defect surfaced while adding a test for the fix
#' above}: with \code{"regular"} (not \code{"rounded"}) and a log-family
#' transform, the inverse-transformed break VALUES are essentially never
#' round numbers (e.g. \code{2.898549...} for one of Josh's own breaks), and
#' the axis LABELS were built straight from those un-rounded numbers, so
#' \code{format()}'s default precision printed them out in full (e.g.
#' \code{"2.898549"} instead of something readable). \strong{Fixed} by
#' rounding to 1 decimal place for the LABEL text only (\code{2.9}); the
#' break POSITION on the axis still comes from the exact, unrounded value,
#' so this is purely cosmetic and does not reintroduce any unevenness.
#' \code{"custom"} breaks are never rounded this way - a user's own
#' hand-typed \code{$y.custom} numbers are always shown exactly as given.
#'
#' \code{$ymax} sets the top of the Y axis. When a \code{fig.list} row
#' leaves it blank (or gives something that isn't a usable positive
#' number), it's auto-computed as \code{max($obs)} across that plot's own
#' filtered data, with a console \code{NOTE} reporting the value used -
#' \strong{a placeholder for this first iteration, flagged for Josh}: this
#' gives the axis exactly enough headroom to fit the tallest bar and no
#' more, which may look visually tight; a fixed padding percentage (e.g.
#' 10\% above the max) could be added in a later iteration if wanted. The
#' actual plotted axis upper limit is never allowed to clip a real bar or a
#' user-supplied \code{$y.custom} value even if \code{$ymax} itself is
#' smaller than one of those (\code{max($ymax, $y.custom values, $obs)} is
#' used as the true limit) - this protects against a \code{$ymax}/
#' \code{$y.custom} mismatch silently cutting off part of the plot.
#'
#' \strong{A real data/reference-table mismatch was found and worked around
#' while building this against Josh's real \code{plfr.bats.csv}:} its 40kHz-
#' Myotis rows use \code{$spp.id = "40kMyo"}, but
#' \code{batz.batusa_recode.names()}'s own reference table's matching
#' non-species category label is \code{"40KHzMyo"} - these do NOT match
#' after that function's own case/dash/underscore-folding normalization
#' (folding case doesn't add the missing \code{"Hz"}), so running
#' \code{"40kMyo"} through \code{batz.batusa_recode.names()} alone leaves it
#' unmatched (passed through unchanged, plus a console \code{WARNING} from
#' that function). Worked around, scoped to this function only (the shared
#' recode reference table itself was not touched): every data row's raw
#' \code{$spp.id} is checked directly (case/punctuation-insensitively)
#' against both \code{"40khzmyo"} and \code{"40kmyo"} before recoding, and
#' either spelling is treated as the 40kHzMyo category for the bar-
#' overlay/legend logic, regardless of what \code{batz.batusa_recode.names()}
#' itself would have matched. \strong{Flagging for Josh}: worth deciding
#' whether \code{"40kMyo"} should be renamed to \code{"40KHzMyo"} at the
#' source (in whatever produces \code{plfr.bats.csv}), or whether
#' \code{"40kMyo"} should be added as a recognized alias directly in
#' \code{batz.batusa_recode.names()}'s own reference table (\code{NAbat.names.csv})
#' - either fix would make this function's own local workaround
#' unnecessary, but isn't required for this function to work correctly as
#' delivered.
#'
#' \strong{\code{$output.filename.pattern}'s default value} in
#' \code{plotopts_callobs.csv} (\code{"Number of bat calls detected at
#' <ARU> between <date.start> and <date.end> <timestamp>.png"}) was chosen
#' to match the literal file name of Josh's own target output image
#' (\code{"Number of bat calls detected.png"}), with the same \code{<ARU>}/
#' \code{<date.start>}/\code{<date.end>}/\code{<timestamp>} placeholders
#' \code{batz.plotdetections_first.last()} already uses.
#'
#' \strong{Real bug found and fixed while visually comparing a render
#' against Josh's target image: the 40kHzMyo bar overlay was invisible.}
#' The first version of this function plotted \code{"All detections"} and
#' \code{"40kHzMyo"} bars for the same night in ONE \code{geom_col()} call
#' (\code{aes(fill = bar.type)}) - the data was correct (verified
#' separately via \code{pd$bar.type}), but ggplot2 draws a single layer's
#' rows in the fill aesthetic's own factor-level order, which defaults to
#' alphabetical: \code{"40kHzMyo"} sorts before \code{"All detections"},
#' so the black bar was drawn FIRST/underneath and the gray bar (same
#' night, same width/x-position, fully opaque) completely covered it every
#' time - invisible in every render even though the underlying data was
#' right. Only caught by actually rendering and looking at the image, not
#' from data-level checks alone (mirrors the same "verify by rendering, not
#' just reading the code" lesson found repeatedly while building
#' \code{batz.plotdetections_first.last()}). Fixed by splitting into two
#' explicit \code{geom_col()} layers, each pre-filtered to one
#' \code{bar.type} and added to the plot in bottom-to-top order (all-
#' detections layer first, 40kHzMyo layer second) - this guarantees the
#' draw order regardless of the fill factor's own alphabetical sort.
#' Re-verified by rendering: the black segment now shows correctly at the
#' base of the gray bar on every night with a real 40kHzMyo observation.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.plotactivity_observations()}: family = "plotactivity" (the
#' verb "plot" is baked into the family name), subject = "observations"
#' (the number of observations/calls per night). \strong{Requested as
#' \code{batz.plotactivlty_observations()} - "plotactivlty" was a typo for
#' "plotactivity" (transposed letters), fixed here; nothing else about the
#' name changed. Please flag if a different family/subject split was
#' actually intended.}
#'
#' @examples
#' \dontrun{
#' result <- batz.plotactivity_observations(
#'   data = plfr.bats,
#'   fig.list = fig.list,
#'   suntimes = aru.suntimes,
#'   aes.default = plotopts.callobs
#' )
#' result$ggplots[[1]]
#' }
#'
#' @export
batz.plotactivity_observations <- function(data, fig.list, suntimes,
                                            aes.default, project.name = "",
                                            dir.save = getwd(), bar.border = FALSE) {

  # See @details above for why this value was chosen and how to change it.
  PLOT.TYPE <- "call.observations"

  ## $aru.groupby is NOT in this list anymore - which column of `data` to
  ## filter/group by is now named per fig.list row via $plot.group (see
  ## Details/Follow-up 2026-08-28) rather than being a fixed, hardcoded
  ## column data must always have.
  DATA.REQUIRED <- c("spp.id", "date", "obs")
  SUNTIMES.REQUIRED <- c("aru", "date", "date.mon", "sunregion", "time.zone",
                          "sunregion.type", "schedual1", "schedual2", "suns",
                          "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                          "Alldect", "facet.panel", "40khzmyo", "facet.label",
                          "plot.group", "plot.sets", "pool", "date.format",
                          "date.start", "date.end", "xaxe.interval")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
    "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
    "panel.border.linewidth", "legend.position",
    "xaxe.interval", "xaxe.title", "xaxe.date.buffer.days", "yaxe.title",
    "Yaxe.trans", "loglabels", "y.scale", "ymax", "bar.width",
    "bar.alldetections.fill", "bar.40khzmyo.fill", "bar.fill.legend.title",
    "legend", "legend.groupval.title", "legend.groupval.colors",
    "legend.groupval.outline.linewidth",
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

  # Parses $plot.sets (2026-08-28) into a character vector of one or more
  # values. Real spreadsheet-exported $plot.sets values (Josh's own
  # fig.list.csv, confirmed against the actual file) don't come through as
  # cleanly double-quoted-and-escaped as `"105059-NW3" "105059-SE3"
  # "105059-SW3"` might suggest - the CSV field's own OUTER quoting eats the
  # very first value's leading quote, so after read.csv() unescapes it, the
  # raw string actually looks like `105059-NW3" "105059-SE3" "105059-SW3"`
  # (no leading quote on the first token). Rather than depend on
  # well-formed quote PAIRS (which would silently drop the first token, or
  # even the whole value, on real data), every double-quote character is
  # simply treated as a token delimiter alongside whitespace: they're
  # stripped out entirely, then the remainder is split on whitespace. This
  # correctly recovers all N values from both the messy real file and a
  # cleanly-quoted one, and also handles a single bare, unquoted value
  # (e.g. `105059-NW3`, exactly like the old $plot.set) or several bare
  # whitespace-separated values with no quoting at all. The one thing it
  # can't handle is a value that itself contains a space (it would be
  # split into two) - not expected for detector/group names, but flagging
  # in case that's ever needed.
  parse.plot.sets <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) == 0 || is.na(x) || !nzchar(x)) return(character(0))
    x <- gsub('"', " ", x, fixed = TRUE)
    vals <- strsplit(trimws(x), "\\s+")[[1]]
    vals[nzchar(vals)]
  }

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
  SPECIAL.FACPAN <- c("Big brown bat", "Eastern red bat", "Hoary bat", "Silver-haired bat",
                       "Eastern small-footed myotis", "Little brown bat",
                       "Northern long-eared bat", "Tri-colored bat")

  # See @details above ("A real data/reference-table mismatch...") for why
  # this exists: Josh's real data uses "40kMyo", batz.batusa_recode.names()'s
  # own reference table uses "40KHzMyo" - the two don't match after that
  # function's own normalization, so this function checks the RAW $spp.id
  # itself, independent of recode().
  norm.simple <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
  KHZ.ALIASES <- c("40khzmyo", "40kmyo")

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("fig.list has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

  # Exact full-row fig.list duplicates are collapsed to their first
  # occurrence before any plotting happens - same fix as
  # batz.plotdetections_first.last() (see that function's own @details for
  # the real bug that led to it); included here from the start.
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
    job.key <- as.character(j)

    if (!identical(tolower(trimws(job$plot.type)), tolower(PLOT.TYPE))) {
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped (this function only handles plot.type = '%s').\n",
                   job.label, job$plot.type, PLOT.TYPE))
      next
    }

    facet.kind <- tolower(trimws(job$facet))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: fig.list row for '%s' has facet = '%s' - skipped ($facet = \"sppid\" is the only value implemented so far).\n",
                   job.label, job$facet))
      next
    }

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
      facpan <- c(facpan, "All detections")
    }
    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) {
      spp.plot <- c(spp.plot, "40khzmyo")
      if (!alldect.flag) facpan <- c(facpan, "40khzmyo")
    }
    spp.plot <- unique(trimws(spp.plot))
    facpan <- unique(trimws(facpan))

    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common")
    facpan <- batz.batusa_recode.names(facpan, batname.format.out = "common")

    pd <- data
    is.khz.raw <- norm.simple(pd$spp.id) %in% KHZ.ALIASES
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common")
    pd$spp.common[is.khz.raw] <- "40khzmyo"

    # --- $plot.group: which column of `data` to filter/group by (2026-08-28
    # follow-up - see Details). Replaces the old hardcoded $aru.groupby. ---
    group.col <- trimws(job$plot.group)
    if (!nzchar(group.col)) {
      cat(sprintf("NOTE: fig.list row for '%s' has a blank $plot.group - skipped (need the name of a column in `data` to filter/group by).\n",
                   job.label))
      next
    }
    if (!(group.col %in% names(pd))) {
      cat(sprintf("NOTE: fig.list row for '%s' has $plot.group = '%s', which is not a column of `data` - skipped.\n",
                   job.label, group.col))
      next
    }

    plot.sets.vals <- parse.plot.sets(job$plot.sets)
    if (length(plot.sets.vals) > 0) {
      pd <- pd[tolower(trimws(as.character(pd[[group.col]]))) %in% tolower(plot.sets.vals), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.group = '%s', plot.sets = '%s', %s to %s) matched 0 rows of data - no plot generated. Check that $%s/$date in data actually overlap this row's $plot.sets/$date.start/$date.end.\n",
                   job.label, group.col, paste(plot.sets.vals, collapse = ", "), date.start, date.end, group.col))
      next
    }

    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel, "All detections", pd$spp.common)
    pd$bar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")
    pd$group.val <- as.character(pd[[group.col]])

    # --- $pool: TRUE sums $obs across every selected $plot.sets value into
    # ONE pooled bar per date/panel; FALSE keeps each selected value as its
    # own bar (drawn side-by-side/dodged at plotting time below) - see
    # Details/Follow-up 2026-08-28. ---
    pool.flag <- isTRUE(as.logical(job$pool))
    if (pool.flag) {
      pd <- stats::aggregate(obs ~ spp.common + facet.panel.value + bar.type + date.parsed,
                              data = pd, FUN = sum)
      pd$group.val <- "pooled"
    }

    # --- $legend (2026-08-29 follow-up - see Details): whether to show the
    # outline-color legend distinguishing which $plot.sets value a dodged
    # bar is. Optional per-row, falls back to aes.default's own "legend"
    # parameter (default TRUE) - same get.setting()/get.default() pattern
    # as $Yaxe.trans/$y.scale/etc.
    legend.flag <- isTRUE(as.logical(get.setting(job, "legend")))

    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (is.na(facet.label.fmt) || !nzchar(facet.label.fmt)) facet.label.fmt <- "common"
    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, batname.format.out = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw
    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels, labels = panel.labels[ordered.levels])

    # --- Y-axis resolution: see @details above for the full explanation ---
    yaxe.trans <- tolower(trimws(get.setting(job, "Yaxe.trans")))
    if (!yaxe.trans %in% c("none", "log", "log10")) {
      cat(sprintf("NOTE: '%s' - $Yaxe.trans = '%s' is not one of none/log/log10 - defaulting to 'none'.\n",
                   job.label, get.setting(job, "Yaxe.trans")))
      yaxe.trans <- "none"
    }
    loglabels <- isTRUE(as.logical(get.setting(job, "loglabels")))

    y.scale.mode <- tolower(trimws(get.setting(job, "y.scale")))
    if (!y.scale.mode %in% c("regular", "rounded", "custom")) {
      cat(sprintf("NOTE: '%s' - $y.scale = '%s' is not one of regular/rounded/custom - defaulting to 'regular'.\n",
                   job.label, get.setting(job, "y.scale")))
      y.scale.mode <- "regular"
    }

    ymax.raw <- suppressWarnings(as.numeric(get.setting(job, "ymax")))
    if (is.na(ymax.raw) || ymax.raw <= 0) {
      ymax.raw <- max(pd$obs, na.rm = TRUE)
      cat(sprintf("NOTE: '%s' - $ymax not given (or not a usable positive number) - defaulting to the max observed count in this plot's data (%s).\n",
                   job.label, ymax.raw))
    }

    trans.fn <- switch(yaxe.trans,
      none  = function(x) x,
      log   = function(x) log1p(x),
      log10 = function(x) log10(x + 1)
    )
    # inverse of trans.fn - needed below to place "regular"/"rounded" breaks
    # evenly along the (possibly transformed) AXIS rather than evenly along
    # the raw count range. See @details ("BUGFIX...evenly spaced") for why.
    inv.trans.fn <- switch(yaxe.trans,
      none  = function(x) x,
      log   = function(x) expm1(x),
      log10 = function(x) 10^x - 1
    )

    if (y.scale.mode == "custom") {
      y.custom.raw <- suppressWarnings(as.numeric(strsplit(get.setting(job, "y.custom"), ";", fixed = TRUE)[[1]]))
      y.custom.raw <- sort(unique(y.custom.raw[!is.na(y.custom.raw)]))
      if (length(y.custom.raw) == 0) {
        cat(sprintf("NOTE: '%s' - $y.scale = 'custom' but $y.custom has no usable numbers - falling back to 'regular'.\n", job.label))
        y.scale.mode <- "regular"
      }
    }
    if (y.scale.mode != "custom") {
      # Breaks are placed at 0/25/50/75/100% evenly-spaced POSITIONS along
      # the axis (i.e. evenly spaced in TRANSFORMED space), then converted
      # back to raw counts for the label. When $Yaxe.trans = "none" this is
      # identical to the old "evenly spaced in raw count units" behavior
      # (trans.fn/inv.trans.fn are both the identity function, so nothing
      # changes for that case) - it only matters, and only differs, when
      # $Yaxe.trans is "log"/"log10".
      frac <- c(0, 0.25, 0.5, 0.75, 1)
      trans.lo <- trans.fn(0)
      trans.hi <- trans.fn(ymax.raw)
      raw.breaks <- inv.trans.fn(trans.lo + frac * (trans.hi - trans.lo))
      if (y.scale.mode == "rounded") raw.breaks <- round(raw.breaks)
    } else {
      raw.breaks <- y.custom.raw
    }
    raw.breaks <- sort(unique(raw.breaks))
    # never let $ymax/$y.custom clip a real bar or a user-declared break
    y.upper <- max(c(ymax.raw, raw.breaks, pd$obs), na.rm = TRUE)

    pd$obs.plot <- trans.fn(pd$obs)
    break.pos <- trans.fn(raw.breaks)
    # break.pos (axis position) is always computed from the exact, unrounded raw.breaks so the
    # BUGFIX 2026-08-28 even-spacing above stays exact. label.breaks is a display-only copy: under
    # "regular" with a log/log10 transform, inverse-transforming an evenly-spaced fraction produces
    # near-irrational values (e.g. 2.898549...), which format()'s default 7-significant-digit rule
    # then prints in full (e.g. "2.898549") - clearly not an intended axis label. Rounding to 1
    # decimal place for display only (not for "custom", where the user's own exact numbers are used
    # verbatim, and redundant but harmless for "rounded", which is already whole numbers) fixes this
    # without touching the break positions themselves.
    label.breaks <- if (y.scale.mode == "custom") raw.breaks else round(raw.breaks, 1)
    break.labels <- if (loglabels) {
      format(round(break.pos, 2))
    } else {
      format(label.breaks, big.mark = ",", trim = TRUE, scientific = FALSE)
    }

    plots[[job.key]] <- list(
      job.label = job.label, job = job, pd = pd, panel.labels = panel.labels,
      facpan = facpan, spp.plot = spp.plot, date.start = date.start, date.end = date.end,
      khz.flag = khz.flag, break.pos = break.pos, break.labels = break.labels,
      y.upper.plot = trans.fn(y.upper), group.col = group.col,
      plot.sets.vals = plot.sets.vals, pool.flag = pool.flag,
      legend.flag = legend.flag
    )
    cat(sprintf("Prepared plot data for '%s': %d observation row(s) across %d panel(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw)))
  }

  if (length(plots) == 0) {
    cat("No plots were generated - see NOTE messages above.\n")
    return(invisible(list(plots = list(), ggplots = list())))
  }

  ggplots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (job.key in names(plots)) {
      p <- plots[[job.key]]
      job.label <- p$job.label

      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        cat(sprintf("NOTE: '%s' - $xaxe.interval = '%s' is not a usable number of x-axis labels - defaulting to 2 (just date.start/date.end).\n",
                     job.label, get.setting(p$job, "xaxe.interval")))
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))
      xaxe.buffer <- suppressWarnings(as.numeric(get.default("xaxe.date.buffer.days")))
      if (is.na(xaxe.buffer)) xaxe.buffer <- 0.5

      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      # Drawn as TWO separate geom_col() layers (all-detections first/bottom,
      # 40kHzMyo second/on top), rather than one geom_col() call with
      # aes(fill = bar.type) - a real rendering bug found and fixed while
      # visually comparing a render to Josh's target image: a single
      # geom_col() call groups by the fill aesthetic's OWN factor-level
      # order (alphabetical, since no explicit levels were set), which put
      # "40kHzMyo" (before "All detections" alphabetically) UNDERNEATH the
      # gray all-detections bar for the same night - same width, same x
      # position, same day - completely hiding the black overlay every
      # time, even though both bars' data rows were present and correct.
      # Two explicit layers, added to the plot in bottom-to-top order,
      # guarantee 40kHzMyo always draws on top regardless of factor order.
      #
      # $bar.width (2026-08-30 follow-up - see Details): a blank
      # $default.value (and no usable per-plot override) now means
      # "auto-size to fit the plot window" instead of erroring out via
      # as.numeric(""). Computed from this job's own actual plotted date
      # spacing (median gap between its sorted, unique dates, x 0.9 to
      # leave a visible gap) rather than one hardcoded guess, so it stays
      # reasonable whether monitoring nights are daily, every-other-day,
      # weekly, etc. Falls back to a flat 0.8 (the file's own prior fixed
      # default) if a job has fewer than two distinct dates to measure a
      # gap from.
      bar.width.val <- suppressWarnings(as.numeric(get.default("bar.width")))
      if (is.na(bar.width.val)) {
        panel.dates <- sort(unique(p$pd$date.parsed))
        bar.width.val <- if (length(panel.dates) >= 2) {
          stats::median(diff(as.numeric(panel.dates))) * 0.9
        } else {
          0.8
        }
      }
      # preserve = "single" (2026-08-30 follow-up): position_dodge2()'s
      # default preserve = "total" divides one fixed total width among
      # however many $group.val bars are present at a given date, so
      # nights with more detectors reporting got visibly thinner bars
      # than nights with fewer - the bar size was tracking detector count
      # instead of staying constant. preserve = "single" fixes each bar's
      # own width at bar.width.val regardless of how many detectors are
      # present on that date.
      bar.position <- if (isTRUE(p$pool.flag) || length(unique(p$pd$group.val)) <= 1) {
        "identity"
      } else {
        ggplot2::position_dodge2(width = bar.width.val, padding = 0.1, preserve = "single")
      }

      # $legend (2026-08-29 follow-up) / $bar.border (2026-08-30 follow-up -
      # see Details): whether more than one selected $plot.sets value gets
      # its own dodged bar per date is still controlled by $pool/$legend as
      # before ("dodge.active"). What changed is $bar.border, default
      # FALSE: a border was ONLY ever drawn in the dodge case to begin with
      # (the plain, non-dodged bars never had one, with or without this
      # option) - FALSE now means no outline is drawn even there, and each
      # dodged bar's own $group.val color (the same $legend.groupval.colors
      # palette previously used for the outline) becomes its FILL instead,
      # combined with the fixed "40kHzMyo" black into one fill legend.
      # TRUE restores the original look: uniform grey "All detections"
      # fill plus a separate colour-mapped outline legend distinguishing
      # $group.val. $bar.40khzmyo.fill is a fixed color either way - it
      # never varies by $group.val - and is always the second/top
      # geom_col() layer (see draw-order note above), in every branch.
      # Non-dodge jobs are untouched by $bar.border in either direction,
      # since there was never a border there to remove - FLAGGED for
      # Josh to confirm that scoping is correct.
      dodge.active <- isTRUE(p$legend.flag) && !isTRUE(p$pool.flag) &&
        length(unique(p$pd$group.val)) > 1
      border.flag <- isTRUE(bar.border)

      if (dodge.active) {
        groupval.levels <- sort(unique(p$pd$group.val))
        groupval.palette <- strsplit(get.default("legend.groupval.colors"), ";", fixed = TRUE)[[1]]
        groupval.palette <- trimws(groupval.palette[nzchar(trimws(groupval.palette))])
        if (length(groupval.palette) == 0) {
          # fallback palette (ColorBrewer "Dark2"-style) if aes.default's
          # own $legend.groupval.colors is blank/unusable - never fatal.
          groupval.palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                                 "#66a61e", "#e6ab02", "#a6761d", "#666666")
        }
        # cycles back to the first color if there are more selected values
        # than colors configured - see Details for why this was chosen over
        # erroring or auto-generating extra colors.
        groupval.colors <- groupval.palette[((seq_along(groupval.levels) - 1) %% length(groupval.palette)) + 1]
        names(groupval.colors) <- groupval.levels
      }

      if (dodge.active && border.flag) {
        # ---- bordered look (original design, kept for $bar.border = TRUE):
        # uniform grey/black fill + a separate colour-mapped outline legend
        # distinguishing $group.val. ----
        groupval.lw <- suppressWarnings(as.numeric(get.default("legend.groupval.outline.linewidth")))
        if (is.na(groupval.lw)) groupval.lw <- 1

        g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed)) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val, colour = group.val),
                             width = bar.width.val, position = bar.position, linewidth = groupval.lw) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val, colour = group.val),
                             width = bar.width.val, position = bar.position, linewidth = groupval.lw) +
          ggplot2::scale_colour_manual(name = get.default("legend.groupval.title"), values = groupval.colors) +
          ggplot2::scale_fill_manual(name = get.default("bar.fill.legend.title"),
            breaks = fill.legend.breaks, limits = fill.legend.limits,
            values = c(`All detections` = get.default("bar.alldetections.fill"),
                       `40kHzMyo` = get.default("bar.40khzmyo.fill")))

      } else if (dodge.active && !border.flag) {
        # ---- borderless look (2026-08-30 follow-up, now the default): no
        # outline anywhere; each dodged bar's own $group.val color becomes
        # its FILL instead. ggplot2 only supports one fill scale per plot,
        # so this uses a single combined key (each $group.val name, plus a
        # fixed "40kHzMyo") rather than a second colour scale the way the
        # bordered branch above does. $bar.alldetections.fill is not used
        # in this branch - see Details, Follow-up 2026-08-30.
        pd.fill <- p$pd
        pd.fill$fill.key <- ifelse(pd.fill$bar.type == "40kHzMyo", "40kHzMyo", as.character(pd.fill$group.val))

        fill.title <- get.default("bar.fill.legend.title")
        if (is.na(fill.title) || !nzchar(trimws(fill.title))) fill.title <- get.default("legend.groupval.title")

        fill.values <- c(groupval.colors, `40kHzMyo` = get.default("bar.40khzmyo.fill"))
        fill.breaks <- c(groupval.levels, if (isTRUE(p$khz.flag)) "40kHzMyo" else NULL)

        g <- ggplot2::ggplot(pd.fill, ggplot2::aes(x = date.parsed)) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.key, group = group.val),
                             width = bar.width.val, position = bar.position, colour = NA) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.key, group = group.val),
                             width = bar.width.val, position = bar.position, colour = NA) +
          ggplot2::scale_fill_manual(name = fill.title, values = fill.values, breaks = fill.breaks)

      } else {
        # ---- not dodging (single $plot.sets value selected, $pool = TRUE,
        # or $legend = FALSE) - no border either way, since one was never
        # drawn here regardless of $bar.border; fill is the plain
        # $bar.alldetections.fill/$bar.40khzmyo.fill pair, unchanged. ----
        g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed)) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val),
                             width = bar.width.val, position = bar.position, colour = NA) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val),
                             width = bar.width.val, position = bar.position, colour = NA) +
          ggplot2::scale_fill_manual(name = get.default("bar.fill.legend.title"),
            breaks = fill.legend.breaks, limits = fill.legend.limits,
            values = c(`All detections` = get.default("bar.alldetections.fill"),
                       `40kHzMyo` = get.default("bar.40khzmyo.fill")))
      }

      g <- g +
        ggplot2::scale_y_continuous(limits = c(0, p$y.upper.plot), breaks = p$break.pos, labels = p$break.labels,
          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        ggplot2::scale_x_date(limits = c(p$date.start - xaxe.buffer, p$date.end + xaxe.buffer),
          breaks = xaxe.breaks, date_labels = xaxe.date.labels.fmt,
          name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        ggplot2::facet_wrap(~facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(linewidth = as.numeric(get.default("panel.border.linewidth")), colour = "grey20", fill = NA),
          legend.position = get.default("legend.position"),
          plot.title = ggplot2::element_text(hjust = as.numeric(get.default("plot.title.hjust")), size = as.numeric(get.default("plot.title.size"))),
          axis.title = ggplot2::element_text(size = as.numeric(get.default("axis.title.size"))),
          axis.text = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
          legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
          legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
          panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt")
        )

      ggplots[[job.key]] <- g

      pattern <- get.default("output.filename.pattern")
      fname <- pattern
      # <ARU> token: the selected $plot.sets value(s), joined with "+" (was
      # the single $plot.set value pre-2026-08-28); "-pooled" suffix added
      # when $pool = TRUE, since that collapses them into one bar/value.
      aru.token <- paste(p$plot.sets.vals, collapse = "+")
      if (!nzchar(aru.token)) aru.token <- p$group.col
      if (isTRUE(p$pool.flag)) aru.token <- paste0(aru.token, "-pooled")
      fname <- gsub("<ARU>", aru.token, fname, fixed = TRUE)
      fname <- gsub("<date.start>", as.character(min(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<date.end>", as.character(max(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<timestamp>", format(Sys.time(), "%Y%m%d%H%M%S"), fname, fixed = TRUE)
      # Prefix the filename with the job's own resolved bar.width.val (e.g.
      # "bsize0.8_..."), so plots made with different bar widths are easy
      # to tell apart on disk. Rounded to 2 decimal places since
      # auto-sized widths (median date-gap x 0.9) are rarely round numbers.
      bsize.token <- paste0("bsize", round(bar.width.val, 2))
      fname <- paste0(bsize.token, "_", fname)
      fname <- file.path(dir.save, fname)

      ggplot2::ggsave(fname, plot = g,
        width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
        height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
        units = get.default("ggsave.units"), dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

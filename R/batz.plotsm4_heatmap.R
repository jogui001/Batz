#' Plot an SM4Bat detector's operational-time heatmap
#'
#' Given a per-15-minute-interval operational-time summary for one or more
#' ARUs (e.g. the plfr-style output of whatever pipeline produces
#' \code{$mins.oper}/\code{$mins2.noon.mon}), draws a night-by-time-of-night
#' heatmap of how many minutes the SM4Bat detector was actually recording in
#' each interval, with the same Dawn/Dusk/Midnight reference lines used by
#' \code{\link{batz.plotdetections_first.last}}. One plot is produced per row
#' of \code{fig.list} whose \code{$plot.type} is \code{"sm4.heatmap"}.
#'
#' @param data A data frame of already-summarized per-ARU, per-interval
#'   operational-time records. Must have \code{$aru.name} (a raw \code{$aru}
#'   column is accepted too and renamed - see Details), \code{$date.monitoringnight},
#'   \code{$mins.oper}, \code{$mins2.noon.mon}.
#' @param fig.list A data frame listing the plot(s) to generate - one row
#'   per plot. Must have \code{$plot.type}, \code{$plot.name}, \code{$plot.set},
#'   \code{$date.format}, \code{$date.start}, \code{$date.end},
#'   \code{$xaxe.interval}, \code{$xaxe.title}, \code{$yaxe.title}. Unlike
#'   this package's other plotting functions, no species/facet-related
#'   columns (\code{$facet}/\code{$MYSO}/\code{$Alldect}/\code{$40khzmyo}/
#'   \code{$facet.label}/\code{$plot.group}/\code{$pool}/\code{$legend}) are
#'   read at all - a device-operational-time heatmap has no per-species
#'   dimension, so this function's own \code{FIG.LIST.REQUIRED} is
#'   deliberately a smaller subset of the shared \code{fig.list.csv}'s
#'   columns (see Details).
#' @param suntimes A data frame of sunrise/sunset times, e.g. the output of
#'   \code{batz.generate_suntimes.arulist()}. Must have \code{$aru.name},
#'   \code{$date}, \code{$date.monitoringnight}, \code{$sunregion}, \code{$time.zone},
#'   \code{$sunregion.type}, \code{$schedual1}, \code{$schedual2},
#'   \code{$suns}, \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'   \code{$sunr.mon}, \code{$sunr.mon.unix} - identical contract to
#'   \code{\link{batz.plotdetections_first.last}}.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_sm4heatmap.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value}; an
#'   \code{$overide.value} column (blank, user-fillable) and \code{$notes}
#'   are optional - same \code{$aes.style}-driven resolution as every other
#'   \code{batz} plot function.
#' @param project.name Character, default \code{"new.project"}. The first
#'   part of every saved file's name:
#'   \code{"<project.name>_<plot.name>_<daterange>_<timestamp>.png"} -
#'   matches the unified file-naming convention used by every other
#'   \code{batz} plot function (see Details, "File naming (round
#'   twenty-one)").
#' @param aes.style Character, default \code{"overide.value"}. Names which
#'   column of \code{aes.default} is checked FIRST for each parameter
#'   (falling back to \code{$default.value} when that column doesn't exist,
#'   or is blank for that row). A given plot row's OWN value in \code{fig.list}
#'   (when that column exists there and is non-blank) still takes priority
#'   over both.
#' @param dir.save Character, default \code{getwd()}. Directory every
#'   generated PNG is saved into.
#'
#' @return Invisibly, a list with \code{plots} (one entry per generated
#'   plot's prepared data - tile rows, suntimes rows, resolved settings) and
#'   \code{ggplots} (the corresponding ggplot objects, only populated when
#'   the \code{ggplot2} package is available).
#'
#' @details
#' \strong{Built 2026-09-23, per Josh's request to model this function on
#' \code{\link{batz.plotdetections_first.last}} and match a target
#' operational-time heatmap image}, from two real attached files: a
#' per-15-minute-interval SM4Bat activity summary (\code{$aru}/\code{$date.mon}/
#' \code{$mins.oper}/\code{$mins2.noon.mon}, among other columns not used
#' here) and a \code{suntimes}-shaped file. This function reuses
#' \code{\link{batz.plotdetections_first.last}}'s core machinery wholesale -
#' \code{canonicalize.headers()}-based header validation, the
#' \code{aes.style}/\code{$overide.value}-driven \code{get.default()}/
#' \code{get.setting()} settings resolution, the Noon-to-Noon
#' \code{y.ref.noon}-anchored Y axis with Dawn (\code{$sunr.mon} - sunrise
#' the FOLLOWING day, ending this monitoring night)/Dusk (\code{$suns})/
#' Midnight reference lines drawn via \code{geom_line()} against real
#' \code{suntimes} dates (\\"short\\" mode only - the other
#' none/long/dots modes \code{batz.plotdetections_first.last()} supports
#' were not requested and are not implemented here), and the multi-format
#' flexible date/datetime parsing helpers - rather than reimplementing any
#' of it from scratch. See "File naming (round twenty-one)" below for this
#' function's own saved-file naming, which has since diverged from the
#' original \code{"<project.name>_<ARU>_<timestamp>.png"} pattern.
#'
#' \strong{What's different from \code{batz.plotdetections_first.last()},
#' and why:} there is no species dimension here at all - every plot is one
#' single panel (no \code{facet_wrap()}), and \code{fig.list}'s species/
#' faceting columns (\code{$facet}/\code{$facet.set}/\code{$MYSO}/
#' \code{$Alldect}/\code{$facet.panel}/\code{$40khzmyo}/\code{$facet.label}/
#' \code{$plot.group}/\code{$pool}/\code{$legend}) are simply never read -
#' \code{FIG.LIST.REQUIRED} is a deliberately smaller subset of
#' \code{fig.list.csv}'s full column set (\code{$plot.type}/\code{$plot.name}/
#' \code{$plot.set}/\code{$date.format}/\code{$date.start}/\code{$date.end}/
#' \code{$xaxe.interval}/\code{$xaxe.title}/\code{$yaxe.title}) - a row for
#' this function's \code{$plot.type} (\code{"sm4.heatmap"}) can leave every
#' other column blank or absent without affecting anything, since
#' \code{fig.list.csv} is shared across every \code{batz} plotting function
#' and each one only reads the columns it actually needs, per the
#' established project convention (see e.g.
#' \code{\link{batz.plotactivity_observations}}'s own \code{@details}).
#' Instead of a crossbar/species fill, each cell of the heatmap is one
#' \code{geom_tile()} - one row of \code{data} per tile, positioned at its
#' own \code{$date.monitoringnight} (X) and \code{$mins2.noon.mon}-derived
#' time-of-night (Y, converted to the same shared reference-date datetime
#' axis \code{batz.plotdetections_first.last()} uses) - colored by
#' \code{$mins.oper} on a continuous (not discrete/manual) fill scale,
#' since operational minutes is a genuinely continuous quantity (0 to the
#' interval width), not a small fixed set of categories the way
#' \\"All detections\\"/\\"40kHzMyo\\" crossbar types are.
#'
#' \strong{\code{$aru}/\code{$aru.name} compatibility shim.} The real
#' attached SM4Bat activity file's own raw column is literally named
#' \code{$aru} (not \code{$aru.name}) - since \code{canonicalize.headers()}
#' only reconciles casing/separator style, never different WORDS (see
#' \code{\link{batz.plotactivity_observations}}'s own \code{@details} for
#' the identical point made about \code{batz.merge_vetted.acoustics2}'s
#' abbreviated schema), a plain \code{$aru} column would never automatically
#' satisfy a \code{$aru.name} requirement. Rather than requiring \code{$aru}
#' (breaking with the rest of the package's 2026-09-22 \code{aru}->
#' \code{aru.name} rename convention - see the Function/script log's
#' \\"aru -> aru.name header rename\\" entry in \code{claude/preferences.md}),
#' this function requires \code{$aru.name} like every other \code{batz}
#' function that reads an ARU identifier, and does one small explicit
#' rename - \code{if ("aru" \%in\% names(data) \&\& !("aru.name" \%in\%
#' names(data))) names(data)[names(data) == "aru"] <- "aru.name"} - BEFORE
#' the required-header check runs, so a raw file spelled either way is
#' accepted. This mirrors the same kind of explicit backward-compatible
#' rename \code{\link{batz.generate_suntimes.arulist}} already does for its
#' own raw \code{$aru} input column.
#'
#' \strong{Tile height (\code{bin.minutes}) is auto-detected from the data
#' itself, per plot, rather than a fixed argument} - computed as the
#' single most common (modal) positive gap between consecutive sorted
#' unique \code{$mins2.noon.mon} values WITHIN each \code{$date.monitoringnight}
#' night separately (pooled across all of that plot's nights before taking the
#' mode - see the BUGFIX entry below for why this must be done per-night,
#' not across every night's values sorted together), falling back to 15
#' (the target file's own real interval width) if this can't be computed
#' (e.g. a plot with only 1 distinct time value). This was chosen over a
#' fixed \code{bin.minutes} argument (the way
#' \code{\link{batz.plotactivity_heatmap}} takes one) because this
#' function's \code{data} is already fully pre-binned by whatever upstream
#' pipeline produced it - unlike \code{batz.plotactivity_heatmap()}, which
#' bins raw per-detection times itself, this function never re-bins
#' anything, it only needs to know how TALL to draw each already-existing
#' row's own tile. \strong{Real, boundary-interval data confirmed in the
#' attached file}: the very first interval of most recording nights is
#' shorter than the nominal 15 minutes (observed values of 1, 9, and 14
#' actual operational minutes for a nominal-15-minute slot, presumably
#' because the detector's recording schedule doesn't always start exactly
#' on a quarter-hour boundary) - these rows are drawn as an ordinary
#' full-height tile (same height as every other tile that night) but
#' colored by their own, lower \code{$mins.oper} value, which is exactly
#' what produces the isolated darker/greener cells visible at the very top
#' and bottom edges of Josh's target image.
#'
#' \strong{Colors and legend, matched directly against Josh's target
#' image}: Dawn is drawn in red-dashed, Dusk in blue-dashed, Midnight in
#' solid black (via new \code{dawn.color}/\code{dawn.linetype}/
#' \code{dusk.color}/\code{dusk.linetype}/\code{midnight.color}/
#' \code{midnight.linetype} defaults in \code{plotopts_sm4heatmap.csv},
#' independent of whatever colors \code{plotopts_first.last.csv} happens to
#' use - each \code{batz} plotting function's own settings file is
#' independent). The fill scale uses a 7-stop \code{viridis} ramp (baked in
#' as a fixed semicolon-separated hex list via
#' \code{viridisLite::viridis(7)}, not a live \code{scale_fill_viridis_c()}
#' dependency - matching the same \code{$fill.colors}-as-a-configurable-
#' hex-list convention \code{\link{batz.plotactivity_heatmap}} already
#' established, rather than introducing a second fill-scale mechanism into
#' the package), capped at \code{$fill.max} (default \code{15}, this
#' interval's nominal width in minutes) via \code{scale_fill_gradientn()}
#' - not \code{pmin()}-clamped the way \code{batz.plotactivity_heatmap()}'s
#' unbounded detection COUNT is, since operational minutes in a fixed-width
#' interval can never legitimately exceed the interval's own width, so
#' there's no "100+"-style open-ended top category needed here. Both this
#' fill legend and the Dawn/Dusk/Midnight line legend are docked together
#' at the bottom of the plot (\code{legend.position = "bottom"}, ggplot2's
#' own default behavior for combining a discrete + a continuous legend
#' side by side), with the colorbar drawn tall/narrow
#' (\code{guide_colorbar(direction = "vertical")}) rather than the wide/flat
#' horizontal bar \code{batz.plotactivity_heatmap()} uses - matching Josh's
#' target image, which shows a narrow vertical bar (ticks at 5/10/15) beside
#' the Dawn/Dusk/Midnight line-legend, not a single wide bar beneath the
#' whole plot.
#'
#' \strong{Verified end to end against Josh's real two attached files}
#' (a real \code{fig.list} row selecting \code{$plot.set = "WTG-GOM101"}
#' across the file's own full \code{2026-08-19} to \code{2026-09-10} date
#' range): the rendered PNG was visually compared side by side against
#' Josh's target image - same overall layout (Noon-to-Noon Y axis with the
#' same 9 break labels three hours apart, two-line month/year X-axis date
#' labels at evenly-spaced intervals, the same Dawn (red dashed)/Dusk (blue
#' dashed)/Midnight (solid black) line styling and gentle day-to-day
#' seasonal drift in the Dawn/Dusk lines' own vertical position, the same
#' near-solid-yellow field of fully-operational 15-minute tiles punctuated
#' by isolated darker/greener partial-interval cells at the top/bottom
#' edges, and the same bottom-docked, side-by-side combined legend - the
#' Dawn/Dusk/Midnight linetype legend and a narrow vertical viridis
#' colorbar with ticks at 5/10/15), confirming the reused
#' \code{batz.plotdetections_first.last()} machinery transfers correctly to
#' this genuinely different (continuous-fill, single-panel, no-species)
#' plot type.
#'
#' \strong{Two real bugs were found and fixed specifically because of this
#' live rendering} (neither was visible from reading the code alone):
#' (1) \code{bin.minutes} auto-detection (see above) originally computed
#' the modal gap across every night's \code{$mins2.noon.mon} values sorted
#' TOGETHER, which first render immediately exposed as an obviously wrong
#' \code{"bin width 0.02 minute(s)"} console message. Root cause: real
#' timestamps carry a few seconds of logging jitter night to night (e.g.
#' one night's "15-minutes-in" row logged at \code{12:15:26}, another's at
#' \code{12:15:31}), so once every night's values are pooled and sorted
#' together, the near-duplicate jittered values between DIFFERENT nights
#' dominate the gap list, drowning out the real ~15-minute within-night
#' interval width. Fixed by computing the gap within each \code{$date.monitoringnight}
#' group separately (\code{split()} by night, \code{diff()} within each,
#' pool the results, then take the mode) - re-verified: now correctly
#' reports \code{"bin width 15 minute(s)"}. (2) The very first tile of
#' every night (\code{$mins2.noon.mon} near \code{0}, e.g. \code{0.017})
#' was silently dropped entirely rather than drawn - \code{ggplot2}'s
#' default out-of-bounds behavior for a continuous scale
#' (\code{oob = scales::censor}) sets a value to \code{NA} whenever it
#' falls outside the scale's \code{limits}, and this applies not just to
#' each tile's own \code{y} center but to its computed \code{ymin}/\code{ymax}
#' extent too - a tile whose center sits exactly at \code{y.start} (Noon)
#' has a \code{ymin} half a bin-height BELOW \code{y.start}, i.e. outside
#' the axis limits, so the entire tile silently vanished (confirmed via
#' \code{ggplot_build()} introspection: 22 rows, one per monitoring night,
#' had \code{ymin = NA}) even though Josh's target image clearly shows
#' these edge tiles drawn (just visually clipped to the panel edge).
#' Fixed by adding \code{oob = scales::oob_squish} to both
#' \code{scale_y_datetime()} and \code{scale_x_date()} (squishes an
#' out-of-range extent to the boundary instead of discarding the whole
#' geometry) - re-verified via the same \code{ggplot_build()} introspection
#' (0 \code{NA} extents afterward) and by re-rendering: every night's first
#' tile is now visible, clipped cleanly to the Noon edge.
#'
#' \strong{2026-09-24 (Josh):} three follow-up changes made after seeing the
#' first live render. (1) The Dawn/Dusk/Midnight reference lines previously
#' only spanned the real suntimes dates present in the data, which stopped
#' half a day short of each panel edge once the x-axis's half-day padding
#' (see above) was added - visible as a small gap at both ends. Fixed by
#' flat-extending \code{sdb} with two synthetic boundary rows, one at
#' \code{date.start - 0.5} and one at \code{date.end + 0.5}, each copying the
#' nearest real row's own dusk/dawn/midnight time - this only changes how far
#' the lines reach, not their slope/shape in between real data points. (2)
#' The fill legend's colorbar is now drawn horizontally (left-to-right) when
#' \code{$legend.position} is \code{"bottom"}, and vertically
#' (bottom-to-top) otherwise, via \code{guide_colorbar(direction=)} with
#' \code{barheight}/\code{barwidth} swapped together so the "long" dimension
#' always matches the bar's actual orientation - previously the colorbar was
#' hardcoded vertical regardless of legend position, which read oddly
#' alongside a bottom-docked Dawn/Dusk/Midnight legend row. (3) Josh asked
#' whether an existing named color scheme runs dark-blue-to-green, cutting
#' off at green around data-value 12 on the current 0-15 fill scale. The
#' current \code{$fill.colors} default is \code{viridisLite::viridis(7)} -
#' i.e. plain viridis, "Blue to Yellow" per Josh's own label - and no
#' separately-named palette (\code{mako}, \code{cividis} checked) reproduces
#' this particular yellow-green; it is simply native viridis itself,
#' truncated. \code{viridisLite::viridis(7, end = 12/15)} reproduces the
#' exact color viridis already shows at value 12 (\code{"#7AD151"}) as its
#' own top stop, confirmed to the hex value both ways. Josh clarified he did
#' NOT want the 0-15 fill scale capped/plateaued at 12 - he wants the full,
#' uncapped 0-15 range kept exactly as-is, just recolored end to end as
#' Blue-to-Green instead of Blue-to-Yellow. So the one-off demo plot simply
#' swaps \code{$fill.colors} to this same truncated
#' \code{viridis(7, end = 0.8)} ramp, stretched evenly across the unchanged,
#' uncapped \code{$fill.max = 15} scale (no data capping, no plateau) - value
#' 15 (the top of the real scale) now renders green instead of yellow, and
#' every value in between blends smoothly toward it.
#'
#' \strong{File naming (round twenty-one), 2026-09-24, per Josh ("The plots
#' come out too fast resulting in overwriting of plots"):} the saved file
#' name is no longer \code{"<project.name>_<ARU>_<timestamp>.png"}
#' (\code{<ARU>} = the job's own \code{$plot.set} value) - it is now
#' \code{"<project.name>_<plot.name>_<daterange>_<timestamp>.png"}, where
#' \code{<plot.name>} is the job's own \code{$plot.name} (the same display
#' name already used as this job's \code{job.label}, falling back to
#' \code{"row N"} when \code{$plot.name} is blank - unchanged from how
#' \code{job.label} was already computed) and \code{<daterange>} is this
#' job's resolved \code{$date.start}/\code{$date.end} formatted as
#' \code{YYYYMMDDtoYYYYMMDD} (e.g. \code{20260408to20260427}). This directly
#' addresses Josh's report: two \code{fig.list} rows that share the same
#' \code{$plot.name} but cover different date windows previously could
#' still land on the identical timestamp-only-differentiated file name when
#' rendered back to back quickly enough (\code{format(Sys.time(), ...)} has
#' 1-second resolution) - the added \code{<daterange>} token means those two
#' rows now always produce distinct file names even when their save
#' timestamps happen to collide. \strong{Scoped to this function,
#' \code{\link{batz.plotdetections_first.last}}, and
#' \code{\link{batz.plotactivity_observations}} only, per Josh's explicit
#' request} - \code{\link{batz.plotactivity_heatmap}},
#' \code{\link{batz.plotactivity_daily.count}}, and
#' \code{\link{batz.plotcover_bullseye}} keep their own existing file-naming
#' conventions (a \code{site.label}/\code{aru.label} token, not
#' \code{fig.list}-driven the same way) unchanged.
#'
#' \strong{Timestamp format (round twenty-two), 2026-09-24, per Josh's
#' package-wide request ("Update all functions that save files or charts:
#' ... <timestamp> format match this format: YYYYMMDDHHHMMSS"):} the
#' \code{<timestamp>} token in every saved file name across this package -
#' this function included - is now built as \code{format(Sys.time(),
#' "\%Y\%m\%d\%H\%M\%S")}: 14 digits, no separator between the date and time
#' halves. This replaces the \code{"\%Y\%m\%d_\%H\%M\%S"} format (date and
#' time halves separated by an underscore) used everywhere in this package
#' since round nineteen. \strong{Read as a typo, flagged rather than
#' silently guessed at}: Josh's literal spec text, \code{"YYYYMMDDHHHMMSS"},
#' has 15 characters (three \code{H}s) where a clock time only ever needs
#' two digits each for hour/minute/second (14 digits total) - taken as a
#' dictation slip for \code{"YYYYMMDDHHMMSS"} (14 digits), which also
#' matches the no-underscore timestamp format
#' \code{\link{batz.generate_suntimes.arulist}} already used for its own
#' \code{sav<timestamp>} token before this round. \strong{Please confirm
#' this reading is right.} This is a pure formatting change to the
#' \code{<timestamp>} token only - its value (the exact second the file was
#' saved) and its position at the end of the file name are unchanged; only
#' the separator between the date and time portions is removed. Applied
#' package-wide in this same round: every other \code{batz} function that
#' saves a chart or a data file (CSV/xlsx) - see each function's own
#' \code{@details} for its own round-twenty-two entry - picks up the same
#' 14-digit, no-underscore \code{<timestamp>} format, whatever its own
#' file-naming pattern is otherwise.
#'
#' \strong{\code{date.mon} renamed to \code{date.monitoringnight} (round
#' twenty-five), 2026-09-25, per Josh ("I changed my mine and want to use
#' date.monitoringnight instead of date.mon to be more consistent with
#' collaborators").} Both \code{data} and \code{suntimes} are now required
#' to have \code{$date.monitoringnight} instead of \code{$date.mon} -
#' \code{DATA.REQUIRED} and \code{SUNTIMES.REQUIRED} are updated
#' accordingly, and the one internal reference,
#' \code{pd$date.parsed <- parse.flex.date(pd$date.mon)}, now reads
#' \code{pd$date.parsed <- parse.flex.date(pd$date.monitoringnight)}. This
#' applies the same package-wide rename already made in
#' \code{\link{batz.plotactivity_daily.count}},
#' \code{\link{batz.plotactivity_heatmap}},
#' \code{\link{batz.generate_suntimes.arulist}},
#' \code{\link{batz.generate_plotframe.bat}}, and
#' \code{\link{batz.merge_vetted.acoustics}} this same round. Nothing else
#' in this function's logic changes.
#'
#' @examples
#' \dontrun{
#' result <- batz.plotsm4_heatmap(
#'   data = sm4.activity,
#'   fig.list = fig.list,
#'   suntimes = aru.suntimes,
#'   aes.default = plotopts.sm4heatmap,
#'   project.name = "gome"
#' )
#' result$ggplots[[1]]
#' }
#'
#' @export
batz.plotsm4_heatmap <- function(data, fig.list, suntimes,
                                  aes.default, project.name = "new.project",
                                  aes.style = "overide.value",
                                  dir.save = getwd()) {

  ## $aru -> $aru.name compatibility shim - see @details. Runs BEFORE the
  ## required-header check, since canonicalize.headers() only reconciles
  ## casing/separator style, never a genuinely different column name.
  if ("aru" %in% names(data) && !("aru.name" %in% names(data))) {
    names(data)[names(data) == "aru"] <- "aru.name"
  }

  PLOT.TYPE <- "sm4.heatmap"

  DATA.REQUIRED <- c("aru.name", "date.monitoringnight", "mins.oper", "mins2.noon.mon")
  SUNTIMES.REQUIRED <- c("aru.name", "date", "date.monitoringnight", "sunregion", "time.zone",
                          "sunregion.type", "schedual1", "schedual2", "suns",
                          "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
  ## Deliberately a smaller subset of fig.list.csv's full column set - see
  ## @details for why no species/facet-related columns are read here.
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "plot.set", "date.format",
                          "date.start", "date.end", "xaxe.interval",
                          "xaxe.title", "yaxe.title")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "yaxe.break.interval", "yaxe.labelformat", "yaxe.break.labels",
    "dawn.linetype", "dawn.color", "dusk.linetype", "dusk.color",
    "midnight.linetype", "midnight.color", "panel.border.linewidth",
    "reference.line.legend.title",
    "fill.max", "fill.colors", "fill.legend.title", "tile.color", "tile.linewidth",
    "plot.title.size", "plot.title.hjust", "axis.title.size", "axis.text.size",
    "legend.text.size", "legend.title.size", "legend.position", "panel.spacing.x",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "plot.width", "plot.height"
  )

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
    if (!("parameter" %in% names(df))) return(NULL)
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
    stop(paste(problems, collapse = "\n\n"))
  }

  data        <- data.canon$df
  suntimes    <- suntimes.canon$df
  fig.list    <- fig.list.canon$df
  aes.default <- aes.default.canon$df

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

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("fig.list has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

  n.jobs.before.dedup <- nrow(jobs)
  jobs <- jobs[!duplicated(jobs), , drop = FALSE]
  n.fig.list.duplicates.removed <- n.jobs.before.dedup - nrow(jobs)
  if (n.fig.list.duplicates.removed > 0) {
    cat(sprintf("NOTE: removed %d exact duplicate row(s) from fig.list before plotting (%d distinct row(s) remain).\n",
                 n.fig.list.duplicates.removed, nrow(jobs)))
  }

  plots <- list()
  y.ref.date <- as.Date("1970-01-02")

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, ]
    job.label <- if (nzchar(trimws(job$plot.name))) job$plot.name else sprintf("row %d", j)
    job.key <- as.character(j)

    if (!identical(tolower(trimws(job$plot.type)), tolower(PLOT.TYPE))) {
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped.\n", job.label, job$plot.type))
      next
    }

    plot.set.val <- trimws(job$plot.set)
    pd <- data
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$aru.name)) == tolower(plot.set.val), , drop = FALSE]
    }

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end   <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date.monitoringnight)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated.\n",
                   job.label, plot.set.val, date.start, date.end))
      next
    }

    sdb <- suntimes
    sdb$date.parsed <- parse.flex.date(sdb$date)
    if (nzchar(plot.set.val)) {
      sdb <- sdb[tolower(trimws(sdb$aru.name)) == tolower(plot.set.val), , drop = FALSE]
    }
    sdb <- sdb[!is.na(sdb$date.parsed) & sdb$date.parsed >= date.start & sdb$date.parsed <= date.end, , drop = FALSE]

    if (nrow(sdb) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' matched 0 rows of suntimes for plot.set = '%s' between %s and %s - Dawn/Dusk/Midnight reference lines will be empty.\n",
                   job.label, plot.set.val, date.start, date.end))
    }

    tz <- if (nrow(sdb) > 0 && "time.zone" %in% names(sdb) && nzchar(trimws(sdb$time.zone[1]))) sdb$time.zone[1] else "UTC"
    y.ref.noon <- as.POSIXct(paste(y.ref.date, "12:00:00"), tz = tz)

    pd$tile.time <- y.ref.noon + pd$mins2.noon.mon * 60

    # bin.minutes auto-detected from this plot's own data - see @details.
    # BUGFIX (2026-09-23, caught by an obviously-wrong first render: "bin
    # width 0.02 minute(s)"): the gap must be computed WITHIN one night at
    # a time, then pooled - computing it across every night's mins2.noon.mon
    # values sorted TOGETHER (the original approach) mixes unrelated nights'
    # time-of-night values, which differ from each other by mere seconds of
    # real-world timestamp jitter (e.g. one night's 15:00-mark row logged at
    # 12:15:26, another's at 12:15:31) - once sorted across ~20+ nights,
    # those near-duplicate jittered values dominate the gap list entirely,
    # so the "most common gap" was a few hundredths of a minute of jitter,
    # not the real ~15-minute interval width.
    diffs <- unlist(lapply(split(pd$mins2.noon.mon, pd$date.monitoringnight), function(x) {
      x <- sort(unique(x))
      d <- round(diff(x), 1)
      d[d > 0]
    }))
    bin.minutes <- if (length(diffs) > 0) {
      as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])
    } else {
      NA_real_
    }
    if (is.na(bin.minutes) || bin.minutes <= 0) bin.minutes <- 15

    dusk.real <- parse.flex.datetime(sdb$suns, tz)
    # Dawn ending THIS monitoring night is $sunr.mon (sunrise the FOLLOWING
    # day), not $sunr (sunrise ON $date, ending the PREVIOUS night) - same
    # reasoning as batz.plotdetections_first.last()'s own BUGFIX for this.
    dawn.real <- parse.flex.datetime(sdb$sunr.mon, tz)
    local.noon <- as.POSIXct(paste(sdb$date.parsed, "12:00:00"), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + 12 * 3600

    # Extend the Dawn/Dusk/Midnight lines flat out to the true panel edges
    # (date.start - 0.5 / date.end + 0.5, the same half-day pad
    # scale_x_date()'s limits use below), per Josh (2026-09-24: "make the
    # midnight line and the dawn and dusk be drawn to the full the edges of
    # the plot"). Without this, each line only spans from the first to the
    # last real suntimes date present, which stops half a day short of each
    # edge once the x-axis padding is added - visible as a small gap at
    # both ends. Flat-extended (not extrapolated) from the nearest real
    # row's own value, so this only changes how far the line reaches, not
    # its slope/shape.
    if (nrow(sdb) > 0) {
      sdb <- sdb[order(sdb$date.parsed), , drop = FALSE]
      left.pad  <- sdb[1, , drop = FALSE]
      right.pad <- sdb[nrow(sdb), , drop = FALSE]
      left.pad$date.parsed  <- date.start - 0.5
      right.pad$date.parsed <- date.end + 0.5
      sdb <- rbind(left.pad, sdb, right.pad)
    }

    y.start <- y.ref.noon
    y.end   <- y.ref.noon + 24 * 3600

    plots[[job.key]] <- list(
      job.label = job.label, job = job, pd = pd, sdb = sdb,
      bin.minutes = bin.minutes, tz = tz,
      y.start = y.start, y.end = y.end,
      date.start = date.start, date.end = date.end,
      plot.set.val = plot.set.val
    )

    cat(sprintf("Prepared plot data for '%s': %d tile(s), bin width %g minute(s), %d suntimes row(s).\n",
                 job.label, nrow(pd), bin.minutes, nrow(sdb)))
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

      y.breaks <- seq(p$y.start, p$y.end, by = get.default("yaxe.break.interval"))
      y.break.labels <- strsplit(get.default("yaxe.break.labels"), ";", fixed = TRUE)[[1]]
      if (length(y.break.labels) != length(y.breaks)) {
        cat(sprintf("NOTE: '%s' - $yaxe.break.labels has %d label(s) but $yaxe.break.interval produces %d break(s) - falling back to $yaxe.labelformat-formatted times instead of the custom labels.\n",
                     job.label, length(y.break.labels), length(y.breaks)))
        y.break.labels <- format(y.breaks, get.default("yaxe.labelformat"))
      }

      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        cat(sprintf("NOTE: '%s' - $xaxe.interval = '%s' is not a usable number of x-axis labels - defaulting to 2 (just date.start/date.end).\n",
                     job.label, get.setting(p$job, "xaxe.interval")))
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))

      panel.border.lw <- as.numeric(get.default("panel.border.linewidth"))
      # Same 2x geom-line-vs-panel-border rendering quirk documented in
      # batz.plotdetections_first.last()'s own @details ("BUGFIX...midnight
      # line looks thicker than the box line"") - halved here identically.
      midnight.render.lw <- panel.border.lw / 2

      fill.max <- suppressWarnings(as.numeric(get.default("fill.max")))
      if (is.na(fill.max) || fill.max <= 0) fill.max <- 15
      fill.colors <- strsplit(get.default("fill.colors"), ";", fixed = TRUE)[[1]]
      fill.colors <- trimws(fill.colors[nzchar(trimws(fill.colors))])
      if (length(fill.colors) < 2) {
        fill.colors <- c("#440154", "#443A83", "#31688E", "#21908C", "#35B779", "#8FD744", "#FDE725")
      }
      fill.breaks <- pretty(c(0, fill.max), n = 3)
      fill.breaks <- fill.breaks[fill.breaks >= 0 & fill.breaks <= fill.max]

      tile.color <- get.default("tile.color")
      tile.lw <- suppressWarnings(as.numeric(get.default("tile.linewidth")))
      if (is.na(tile.lw)) tile.lw <- 0

      # Colorbar orientation follows $legend.position, per Josh (2026-09-24):
      # "the Color band in the legend goes from bottom to top orientation
      # which matches if legend is on the side of the plot. For instances
      # when the legend is placed at the bottom of the plot I want it to
      # run left to right." A vertical (bottom-to-top) bar reads naturally
      # beside a side legend; a horizontal (left-to-right) bar reads
      # naturally in a legend row docked under the plot. barheight/barwidth
      # are swapped together so the "long" dimension always matches
      # whichever way the bar is actually drawn.
      legend.pos <- get.default("legend.position")
      colorbar.horizontal <- identical(tolower(trimws(legend.pos)), "bottom")
      colorbar.direction <- if (colorbar.horizontal) "horizontal" else "vertical"
      colorbar.barheight <- if (colorbar.horizontal) grid::unit(0.4, "cm") else grid::unit(3.2, "cm")
      colorbar.barwidth  <- if (colorbar.horizontal) grid::unit(3.2, "cm") else grid::unit(0.4, "cm")

      g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed, y = tile.time)) +
        ggplot2::geom_tile(ggplot2::aes(fill = mins.oper), width = 1,
                             height = p$bin.minutes * 60,
                             color = tile.color, linewidth = tile.lw) +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dusk.time, color = "Dusk"),
                             linetype = get.default("dusk.linetype"), inherit.aes = FALSE) +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                             linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw,
                             inherit.aes = FALSE) +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dawn.time, color = "Dawn"),
                             linetype = get.default("dawn.linetype"), inherit.aes = FALSE) +
        ggplot2::scale_color_manual(name = get.default("reference.line.legend.title"),
                                      breaks = c("Dawn", "Dusk", "Midnight"),
                                      values = c("Dawn" = get.default("dawn.color"),
                                                 "Dusk" = get.default("dusk.color"),
                                                 "Midnight" = get.default("midnight.color"))) +
        ggplot2::scale_fill_gradientn(colors = fill.colors, name = get.default("fill.legend.title"),
                                        limits = c(0, fill.max), breaks = fill.breaks,
                                        guide = ggplot2::guide_colorbar(direction = colorbar.direction,
                                          barheight = colorbar.barheight, barwidth = colorbar.barwidth,
                                          title.position = "top")) +
        ggplot2::scale_y_datetime(limits = c(p$y.start, p$y.end),
                                    breaks = y.breaks, labels = y.break.labels,
                                    expand = c(0, 0), oob = scales::oob_squish,
                                    name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        ggplot2::scale_x_date(limits = c(p$date.start - 0.5, p$date.end + 0.5),
                                breaks = xaxe.breaks, date_labels = xaxe.date.labels.fmt,
                                expand = c(0, 0), oob = scales::oob_squish,
                                name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(linewidth = panel.border.lw, colour = "grey20", fill = NA),
                        legend.position = get.default("legend.position"),
                        legend.box = "horizontal",
                        plot.title = ggplot2::element_text(hjust = as.numeric(get.default("plot.title.hjust")),
                                                             size = as.numeric(get.default("plot.title.size"))),
                        axis.title = ggplot2::element_text(size = as.numeric(get.default("axis.title.size"))),
                        axis.text = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
                        legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
                        legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
                        panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt"))

      ggplots[[job.key]] <- g

      # File naming (round twenty-one), per Josh (2026-09-24): "the plots
      # come out too fast resulting in overwriting of plots" - see
      # @details, "File naming (round twenty-one)". <daterange> is this
      # job's own resolved $date.start/$date.end (Date objects, already
      # parsed above), formatted YYYYMMDDtoYYYYMMDD.
      daterange.token <- sprintf("%sto%s", format(p$date.start, "%Y%m%d"), format(p$date.end, "%Y%m%d"))
      fname <- sprintf("%s_%s_%s_%s.png", project.name, job.label, daterange.token, format(Sys.time(), "%Y%m%d%H%M%S"))
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

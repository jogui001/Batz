#' Plot the nightly number of observations per species
#'
#' Draws the standard report plot of nightly observation counts. For each
#' species facet (plus an optional "All detections" facet and an optional
#' 40kHzMyo overlay) it shows one bar per night with that night's
#' \code{$observations.count}. Several groups (e.g. ARUs) can be pooled into
#' one bar or drawn side by side. One PNG is saved per \code{fig.list} row
#' whose \code{$plot.type} is \code{"call.observations"} (or one per facet
#' when \code{$facet.plot = FALSE}).
#'
#' @param data Data frame of per-species, per-night counts, normally the
#'   output of \code{\link{batz.generate_plotframe.bat}}. Required columns:
#'   \code{$spp.id}, \code{$date}, \code{$observations.count}, plus the
#'   grouping column named by the row's \code{$plot.group} (default
#'   \code{$group}). Old header \code{$obs} is still accepted (used as
#'   \code{$observations.count}, with a WARNING). Raw \code{$spp.id} values
#'   \code{40kMyo} or \code{40kHzMyo} (any case or punctuation) are treated
#'   as 40kHzMyo.
#' @param fig.list Data frame, one row per plot. See "fig.list columns" in
#'   Details. Column names must be unique.
#'   Cells holding "NA" (a heading not used by this plot type, as in
#'   \code{\link{batz.generate_figlist}}) are treated as blank.
#'   Only rows whose \code{$plot} is \code{TRUE} or \code{1} are
#'   plotted; rows with \code{FALSE}, any other entry or a blank are
#'   skipped (NOTE). A fig.list with no \code{$plot} column has every
#'   row plotted (NOTE).
#' @param suntimes Data frame of sunrise and sunset times, e.g. the output of
#'   \code{\link{batz.generate_suntimes.arulist}}. Its headers are checked
#'   (\code{$aru.name}, \code{$date}, \code{$date.monitoringnight},
#'   \code{$sunregion}, \code{$time.zone}, \code{$sunregion.type},
#'   \code{$schedule1}, \code{$schedule2}, \code{$sunset},
#'   \code{$sunset.unix}, \code{$sunrise}, \code{$sunrise.unix},
#'   \code{$sunrise.monitoringnight}, \code{$sunrise.monitoringnight.unix};
#'   old \code{schedual1} / \code{schedual2} are still accepted), but the
#'   values are not used in the plot.
#' @param aes.plotopts Data frame of plot settings (plotopts), one row per
#'   parameter, e.g. \verb{plotopts_callobs.csv} from
#'   \code{\link{batz.generate_plotopts}}. Required columns:
#'   \code{$category}, \code{$parameter}, \code{$default.value}. The custom
#'   style column (named by \code{aes.style}) and \code{$notes} are optional.
#'   See "plotopts" in Details.
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plotact_observations} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#'   (The old argument name \code{aes.default} is still read, with a WARNING.)
#'   Optional rows \code{plot.title.face}, \code{axis.title.face} and
#'   \code{legend.title.face} (\code{"plain"}, \code{"bold"}, \code{"italic"},
#'   \code{"bold.italic"}) set the title fonts; a missing row gives bold.
#'   \code{plot.zero} (the fig.list column \code{$plot.zero}, the plotopts row or typed into the call), default
#'   \code{FALSE}: values of 0 (bars, tiles, cells) are not drawn; \code{TRUE}
#'   draws them. A 0 bar keeps its place (grouped / side-by-side bars do
#'   not move); it is only made invisible.
#'   Optional row \code{strip.text.size}: size of the facet panel titles
#'   (a missing row gives 10).
#' @param project.name Character, default \code{"new.project"}. Put at the
#'   start of every saved file name. A blank value leaves it out.
#' @param ... Plot settings typed into the call, named as the
#'   \code{aes.plotopts} \code{$parameter} rows (or fig.list setting
#'   columns), e.g. \code{legend.position = "bottom"},
#'   \code{plot.width = 8}. A vector is joined with ";". Each setting is
#'   taken from the function call, then the fig.list row, then \code{aes.plotopts} (its custom-style column, then
#'   \code{$default.value}). A setting given here does not need a row in
#'   \code{aes.plotopts}. A NOTE lists the settings used and any name the
#'   function does not use.
#' @param aes.style Character, default \code{"override.value"}. Name of the
#'   \code{aes.plotopts} column checked first for each parameter. When that
#'   column does not exist, or its cell is blank, \code{$default.value} is
#'   used. With the default, an old \code{$overide.value} column is still
#'   read when there is no \code{$override.value} column.
#' @param dir.save Character, default \code{getwd()}. Folder every PNG is
#'   saved into.
#' @param strip.special Logical, default \code{TRUE}. TRUE simplifies
#'   accented letters and removes other non-ASCII characters (e.g. degree or
#'   micro signs) in \code{data} and \code{suntimes}, before any header
#'   check. FALSE leaves them as they are.
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same
#'   clean-up for the settings tables \code{fig.list} and
#'   \code{aes.plotopts}. FALSE leaves them untouched (plot text keeps its
#'   symbols), but a cell that cannot be read in this R session stops the
#'   function with an error naming the table, the number of bad characters
#'   and the first rows per header. TRUE strips them instead.
#'
#' @return Invisibly, a list with \code{$plots} and \code{$ggplots}, keyed by
#'   the row number among the kept \code{fig.list} rows (\code{"1"},
#'   \code{"2"}, ...). Plots split by \code{$facet.plot = FALSE} are keyed
#'   \code{"1.1"}, \code{"1.2"}, and so on. Both are empty lists when no plot
#'   is made. Each \code{$plots} entry holds:
#'   \describe{
#'     \item{\code{$job.label}, \code{$job}}{Plot title (\code{$plot.name},
#'       or \code{"row N"}) and the \code{fig.list} row.}
#'     \item{\code{$pd}}{Plotted rows: the \code{data} columns plus
#'       \code{$spp.common}, \code{$date.parsed}, \code{$facet.panel.value}
#'       (factor of facet titles), \code{$bar.type} (\code{"All detections"}
#'       or \code{"40kHzMyo"}), \code{$group.val} and \code{$obs.plot} (bar
#'       height on the transformed axis). When pooled, only
#'       \code{$spp.common}, \code{$facet.panel.value}, \code{$bar.type},
#'       \code{$date.parsed}, \code{$observations.count},
#'       \code{$group.val} (\code{"pooled"}) and \code{$obs.plot}.}
#'     \item{\code{$panel.labels}, \code{$facpan}, \code{$spp.plot}}{Facet
#'       titles, facet list and the species list used to filter.}
#'     \item{\code{$date.start}, \code{$date.end}}{X-axis date window.}
#'     \item{\code{$khz.flag}}{TRUE when 40kHzMyo is on for this plot.}
#'     \item{\code{$break.pos}, \code{$break.labels},
#'       \code{$y.upper.plot}}{Y break positions (transformed), their labels
#'       and the transformed axis top.}
#'     \item{\code{$group.col}, \code{$plot.sets.vals}}{Grouping column and
#'       the selected \code{$plot.set} values.}
#'     \item{\code{$pool.flag}, \code{$legend.flag}}{Resolved \code{$pool}
#'       and \code{$legend}.}
#'     \item{\code{$facet.plot}, \code{$facet.raw}, \code{$facet.ids}}{Facet
#'       mode, facet names and their short IDs used in the file name.}
#'     \item{\code{$resolved.legend.position}}{Resolved plotopts
#'       \code{legend.position}.}
#'   }
#'   \code{$ggplots} holds the ggplot objects (empty when ggplot2 is not
#'   installed).
#'
#'   Files written: one PNG per plot in \code{dir.save}. Faceted:
#'   \verb{<project.name>_<plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#'   One per facet:
#'   \verb{<project.name>_<plot.name>_<facet>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#'   The \code{<plot.set>} part is the selected values joined with "+", with
#'   \code{-pooled} added when \code{$pool} is TRUE. A blank \code{$plot.set}
#'   gives \code{all} (just \code{-pooled} when pooled).
#'   Facet IDs are four-letter codes (\code{code4}), \code{AllDet} and
#'   \code{40kMyo} (a 40kHzMyo drawn inside the "All detections" facet is not
#'   counted). Spaces become "-" and characters Windows does not allow are
#'   removed. Names over 100 characters are shortened step by step (drop the
#'   year from a same-year end date, then cut each word of the facet IDs,
#'   \code{<plot.set>} and \code{$plot.name} to 6 characters).
#'
#' @details
#' \strong{fig.list columns.} Required and optional headers are matched
#' ignoring case and "." / "_" / space differences.
#' \describe{
#'   \item{\code{$plot.type}}{Only \code{"call.observations"} rows are
#'     plotted; other values are skipped with a NOTE. Rows with a blank
#'     \code{$plot.type} are dropped.}
#'   \item{\code{$plot.name}}{Plot title and second part of the file name (after \code{project.name}).
#'     Blank gives \code{"row N"}.}
#'   \item{\code{$facet.header}}{What the facets are. Only \code{"sppid"}
#'     (species facets) is supported; other values are skipped with a NOTE.}
#'   \item{\code{$facet.plot}}{TRUE (or anything but FALSE) = one faceted
#'     plot. FALSE = one plot per facet; facets with no data are skipped with
#'     a NOTE.}
#'   \item{\code{$facet.set}}{Facets and their order: a preset (\code{"NE"},
#'     also \code{"New England"}) or a list separated by ";" (or ",") of
#'     species in any format \code{\link{batz.batusa_recode.names}} reads,
#'     \code{"AllDet"} / \code{"All detections"}, \code{"40khzmyo"} and preset
#'     names, mixed freely (e.g. \code{"AllDet;epfu;labo;lano"} or
#'     \code{"AllDet;NE"}). Blank = \code{"NE"} (NOTE). Unrecognized entries
#'     are kept as written (NOTE). Duplicates are dropped. Every facet is
#'     shown, even with no data.}
#'   \item{\code{$myso}}{TRUE adds Indiana bat (MYSO). If little brown bat
#'     (MYLU) records are in the plotted data (plot set and date range), all
#'     MYLU and MYSO records are pooled as LUSO and LUSO replaces MYLU/MYSO in
#'     the facets (in the place of the first of them, or added last), with a
#'     NOTE. Otherwise Indiana bat is plotted and gets its own facet (added
#'     last if not listed in \code{$facet.set}).}
#'   \item{\code{$all.detections}}{TRUE adds an "All detections" facet first,
#'     if \code{$facet.set} does not already list it.}
#'   \item{\code{$40khzmyo}}{TRUE (or \code{"40khzmyo"} in
#'     \code{$facet.set}) plots 40kHzMyo bars. They are drawn on top of the
#'     bars in the "All detections" facet when it exists, otherwise in their
#'     own last facet. The 40kHzMyo legend key is shown whenever this is on.}
#'   \item{\code{$facet.label_format}}{Format of species facet titles:
#'     \code{"common_name"} (used when blank), \code{"scientific_name"},
#'     \code{"label_name"} (e.g. "Big brown (EPFU)"), \code{"label2_name"}
#'     (e.g. "Big brown bat (EPFU)"), \code{"label3_name"} (e.g. "Arizona
#'     bat (MYOC)"), \code{"code4"}, \code{"code6"},
#'     \code{"both"}, or \code{"common"},
#'     \code{"latin"}, \code{"scientific"}. Other values stop with an error
#'     naming the row. Non-species facets keep their own name. An older
#'     fig.list whose \code{$facet.label} holds these formats (and has no
#'     \code{$facet.label_format}) is read as \code{$facet.label_format}
#'     (WARNING).}
#'   \item{\code{$facet.label}}{Optional. Alternative names for the panels
#'     (or other labelled parts of the graph): a ";" list, either
#'     \code{name=label} pairs (e.g. \code{"lano=Silver-haired;AllDet=All
#'     bats"}; a name is matched to the panel's name, its label or its species code or name (AllDet = All detections), ignoring
#'     case, spaces, dots and underscores) or labels in panel order. Panels not
#'     given a label keep theirs; a NOTE names pairs that match no panel and
#'     a count that doesn't match.}
#'   \item{\code{$plot.set}}{One or more values of the \code{$plot.group}
#'     column to include, separated by ";" (e.g.
#'     \code{"CLERK; SUGARHILL; AYERS"}; spaces around each value are
#'     ignored), matched ignoring case. Without a ";", values separated by
#'     spaces are read (double quotes are treated as spaces). Blank = all
#'     values.}
#'   \item{\code{$pool}}{TRUE sums \code{$observations.count} over all
#'     selected \code{$plot.set} values into one bar per date and facet.
#'     FALSE keeps one bar per value, side by side.}
#'   \item{\code{$date.format}}{X-axis date label format (strftime style,
#'     e.g. \code{"\%b-\%d/n\%Y"}). A literal "/n" becomes a line break.}
#'   \item{\code{$date.start}, \code{$date.end}}{Date window of the plot.
#'     Read as m/d/Y, Y-m-d or m/d/y. Several dates separated by ";" (e.g.
#'     \code{"2026-06-26; 2026-06-28; 2026-06-29; 2026-07-01"} in both) give
#'     one window per pair (first \code{$date.start} with first
#'     \code{$date.end}, ...): only the days in the windows are plotted, side
#'     by side, and the days between them are left out. With the same list
#'     in both, or a list in one and the other blank, only those dates are
#'     plotted. Otherwise the two lists must be the same length.}
#'   \item{\code{$xaxe.interval}}{Number of evenly spaced labeled dates on
#'     the X axis, from \code{$date.start} to \code{$date.end}. An unusable
#'     value gives 2 labels (NOTE). With date windows, every plotted day is
#'     labelled when there are no more days than \code{$xaxe.interval}.}
#'   \item{Optional: \code{$plot.group}}{Column of \code{data} to filter and
#'     group by. Blank or absent = \code{$group}. A name that is not a column
#'     of \code{data} skips the row (NOTE).}
#'   \item{Optional: \code{$yaxe.trans}}{Y-axis transform: \code{"none"},
#'     \code{"log"} (log(x + 1)) or \code{"log10"} (log10(x + 1)). Other
#'     values give \code{"none"} (NOTE).}
#'   \item{Optional: \code{$loglabels}}{TRUE labels Y breaks with the
#'     transformed values (2 decimals) instead of counts.}
#'   \item{Optional: \code{$y.scale}}{How Y breaks are chosen:
#'     \code{"regular"}, \code{"rounded"} or \code{"custom"}. Other values
#'     give \code{"regular"} (NOTE).}
#'   \item{Optional: \code{$y.custom}}{Break values separated by ";", used
#'     when \code{$y.scale = "custom"}. No usable numbers gives
#'     \code{"regular"} (NOTE).}
#'   \item{Optional: \code{$ymax}}{Top count on the Y axis. Blank or not a
#'     positive number uses the largest count in the plot (NOTE).}
#'   \item{Optional: \code{$legend}}{TRUE colors side-by-side bars by their
#'     \code{$plot.set} value and shows a legend for them.}
#'   \item{Optional: \code{$xaxe.title}, \code{$yaxe.title}}{Axis titles.}
#' }
#' Each optional column, when blank or missing, falls back to the plotopts
#' parameter of the same name. Old fig.list files still load: \code{$facet}
#' is used as \code{$facet.header}, \code{$Alldect} /
#' \code{$all.dectections} as \code{$all.detections}, and a missing
#' \code{$facet.plot} is added as TRUE, each with a WARNING. Old
#' \code{$facet.panel}, \code{$facpan} and \code{$plot.order} columns are
#' ignored (NOTE). Exact duplicate rows (every column the same) are removed
#' before plotting (NOTE).
#'
#' \strong{plotopts (aes.plotopts).} Each setting is read from the first row
#' whose \code{$parameter} matches the name (ignoring case and spaces).
#' The value is the \code{aes.style} column when that cell is not blank,
#' otherwise \code{$default.value}. For the fig.list settings above, a
#' non-blank fig.list value wins over both. These \code{$parameter} rows must
#' exist (case and spaces ignored), or the function stops listing the missing ones:
#' \code{facpan.numcol}, \code{plot.title.size}, \code{plot.title.hjust},
#' \code{axis.title.size}, \code{axis.text.size}, \code{legend.text.size},
#' \code{legend.title.size}, \code{panel.spacing.x},
#' \code{panel.border.linewidth}, \code{legend.position},
#' \code{xaxe.interval}, \code{xaxe.title}, \code{xaxe.date.buffer.days}
#' (padding in days on each side of the date window; 0.5 if not a number),
#' \code{yaxe.title}, \code{yaxe.trans}, \code{loglabels}, \code{y.scale},
#' \code{ymax}, \code{bar.width}, \code{bar.alldetections.fill},
#' \code{bar.40khzmyo.fill}, \code{bar.fill.legend.title}, \code{legend},
#' \code{legend.groupval.title}, \code{legend.groupval.colors} (";"-separated
#' colors for side-by-side bars; colors repeat when there are more values
#' than colors, and a built-in 8-color palette is used when blank),
#' \code{ggsave.dpi}, \code{ggsave.units}, \code{ggsave.width.pad},
#' \code{ggsave.height.pad}, \code{plot.width}, \code{plot.height}. An
#' \code{output.filename.pattern} row is ignored.
#'
#' \strong{Input checks.}
#' \itemize{
#'   \item Special characters are stripped or checked (\code{strip.special},
#'     \code{strip.special.plotopts}).
#'   \item Required headers of all four inputs are matched and renamed to the
#'     expected spelling. Missing headers, missing plotopts rows and
#'     duplicate column names are collected and reported in one error.
#'   \item Plot-frame rows from pools shorter than one day are combined to
#'     one row per species/group/date, summing \code{$observations.count}
#'     (NOTE).
#' }
#'
#' \strong{Preparing each plot.}
#' \itemize{
#'   \item \code{data} is given \code{$spp.common} (common names) and
#'     filtered to the \code{$plot.set} values, the facet species (plus
#'     Indiana bat or LUSO for \code{$myso}, plus 40kHzMyo) and the date
#'     window. A row with no data left is skipped (NOTE).
#'   \item With \code{$pool = TRUE}, counts are summed by species, facet, bar
#'     type and date.
#'   \item Y axis: for \code{"regular"} and \code{"rounded"}, 5 breaks are
#'     placed evenly along the (transformed) axis from 0 to \code{$ymax} and
#'     turned back into counts (\code{"rounded"} rounds them to whole
#'     numbers; labels are rounded to 1 decimal with thousands commas). For
#'     \code{"custom"}, the \code{$y.custom} values are used. The axis top is
#'     never below the largest bar or break.
#' }
#'
#' \strong{Drawing.}
#' \itemize{
#'   \item Bars are side by side only when \code{$pool} is FALSE and more
#'     than one group value is present; side-by-side bars keep a constant
#'     width (\code{bar.width}).
#'   \item The "All detections" bars are drawn first and the 40kHzMyo bars on
#'     top, so the overlay is never hidden.
#'   \item Colors: with side-by-side bars and \code{$legend} TRUE, each group
#'     value gets its own color from \code{legend.groupval.colors} (legend
#'     title \code{legend.groupval.title}) and 40kHzMyo keeps
#'     \code{bar.40khzmyo.fill}. Otherwise bars use
#'     \code{bar.alldetections.fill} and \code{bar.40khzmyo.fill} (legend
#'     title \code{bar.fill.legend.title}, showing only the 40kHzMyo key).
#'   \item The X axis runs from \code{$date.start} to \code{$date.end} plus
#'     \code{xaxe.date.buffer.days} on each side (with date windows: only the
#'     days in the windows, one slot each). Facets are laid out in
#'     \code{facpan.numcol} columns.
#'   \item The PNG size is \code{plot.width} + \code{ggsave.width.pad} by
#'     \code{plot.height} + \code{ggsave.height.pad}, in \code{ggsave.units},
#'     at \code{ggsave.dpi}. A \code{Saved:} line is printed per file.
#'   \item If ggplot2 is not installed, only the prepared data is returned.
#' }
#'
#' \strong{Saved size.}
#' \describe{
#'   \item{\code{$plot.dimensions}}{Optional fig.list column: width x height
#'     of the saved PNG in inches. "1/3 page" = 6 x 3.5, "1/2 page" = 6 x 4,
#'     "2/3 page" = 6 x 6, "1 page" = 6 x 7.5, "1/3 ppt" = 6 x 3,
#'     "1/2 ppt" = 12 x 3, "2/3 ppt" = 8 x 6, "1 ppt" = 12 x 6 (case and
#'     spaces ignored), or any "W x H" (e.g. "7 x 4"). It replaces
#'     plot.width, plot.height, the pads and ggsave.units. Blank or "NA":
#'     the plotopts size settings are used; any other entry gives a NOTE and
#'     the plotopts size. The old spelling $plot.dimentions is also read, and it
#'     can be typed into the call (e.g. plot.dimensions = "1/2 page").}
#'   \item{\code{$plot.zero}}{Optional fig.list column: FALSE (default) =
#'     values of 0 (bars, tiles, cells) are not drawn (bars keep their
#'     place); TRUE draws them. Can
#'     also be set in plotopts or typed into the call (plot.zero = TRUE).}
#'   \item{\code{$plot.zero_all.detections}}{Optional: plot.zero for the All detections bars only
#'     (FALSE = 0 not drawn). Wins over $plot.zero at the same level (call,
#'     fig.list, plotopts); blank uses $plot.zero.}
#'   \item{\code{$plot.zero_40khzmyo}}{Optional: plot.zero for the 40kHzMyo bars only
#'     (FALSE = 0 not drawn). Wins over $plot.zero at the same level (call,
#'     fig.list, plotopts); blank uses $plot.zero.}
#' }
#'
#' @seealso \code{\link{batz.generate_plotframe.bat}},
#'   \code{\link{batz.generate_plotopts}},
#'   \code{\link{batz.plotdetections_first.last}}
#'
#' @examples
#' \dontrun{
#' result <- batz.plotactivity_observations(
#'   data = plfr.bats,
#'   fig.list = fig.list,
#'   suntimes = aru.suntimes,
#'   aes.plotopts = plotopts.callobs,
#'   dir.save = "C:/path/to/output/folder"
#' )
#' result$ggplots[[1]]
#' }
#'
#' @export
batz.plotactivity_observations <- function(data, fig.list, suntimes,
                                            aes.plotopts = NULL, project.name = "new.project",
                                            aes.style = "override.value",
                                            dir.save = getwd(),
                                            strip.special = TRUE,
                                            strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(plotutil_dots(...), "batz.plotactivity_observations")

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.plotopts <- paste(deparse(substitute(aes.plotopts)), collapse = "")
  fig.list <- plotutil_figlist_na(fig.list)       # "NA" = heading not used by this plot type
  fig.list <- plotutil_figlist_plot(fig.list, "batz.plotactivity_observations")   # only rows with $plot = TRUE or 1
  if (is.data.frame(fig.list) && nrow(fig.list) == 0) return(invisible(list(plots = list(), ggplots = list())))
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_aes_legacy(plotutil_dots(...), "batz.plotactivity_observations")   # old argument name aes.default / plotopts
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_default_plotopts("batz.plotactivity_observations")   # built-in settings
  data <- special.strip(data, strip.special)$df
  ## fig.list is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    fig.list <- special.strip(fig.list, TRUE)$df
  } else {
    special.check.plotopts(fig.list, "fig.list", objname.fig.list, "batz.plotactivity_observations")
  }
  suntimes <- special.strip(suntimes, strip.special)$df
  ## aes.plotopts is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.plotopts <- special.strip(aes.plotopts, TRUE)$df
  } else {
    special.check.plotopts(aes.plotopts, "aes.plotopts", objname.aes.plotopts, "batz.plotactivity_observations")
  }

  # See @details above for why this value was chosen and how to change it.
  PLOT.TYPE <- "call.observations"

  ## $aru.groupby is NOT in this list anymore - which column of `data` to
  ## filter/group by is now named per fig.list row via $plot.group (see
  ## Details/Follow-up 2026-08-28) rather than being a fixed, hardcoded
  ## column data must always have. Follow-up, 2026-09-23 (per Josh):
  ## $plot.group is no longer REQUIRED here at all - it's now resolved
  ## per-row below, defaulting to "group" when blank/absent - see Details.
  ## $plot.sets was also renamed to $plot.set this same round, standardizing
  ## on the singular spelling batz.plotdetections_first.last() already used.
  ##
  ## Header standardization (per Josh, 2026-09-14 project preference):
  ## standardize.headers() itself is still NOT applied directly to any of
  ## the five *.REQUIRED*/DATA.REQUIRED constants below, or renamed onto
  ## `data`/`fig.list`/`suntimes`/`aes.plotopts` wholesale - these names
  ## remain this function's own interface contract with those upstream
  ## functions'/files' already-established output schemas, not raw loaded
  ## headers. See @details "Header standardization" above. As of the
  ## 2026-09-21 follow-up below, though, canonicalize.headers() IS used
  ## (further down, where the header check happens) so that a column
  ## arriving in a different casing/separator style than these constants
  ## still matches correctly - see @details "Follow-up, 2026-09-21" above.
  DATA.REQUIRED <- c("spp.id", "date", "observations.count")
  # Renamed 2026-09-25 (round twenty-six, per Josh's package-wide suntimes
  # header rename - see @details, "Follow-up, 2026-09-25...round
  # twenty-six"): matches batz.generate_suntimes.arulist()'s own renamed
  # output columns. $date.mon -> $date.monitoringnight is a separate,
  # pre-existing bug fixed in this same audit pass (round twenty-five,
  # never caught up here at the time) - see the same @details entry.
  SUNTIMES.REQUIRED <- c("aru.name", "date", "date.monitoringnight", "sunregion", "time.zone",
                          "sunregion.type", "schedule1", "schedule2", "sunset",
                          "sunset.unix", "sunrise", "sunrise.unix", "sunrise.monitoringnight",
                          "sunrise.monitoringnight.unix")
  # "plot.group" removed from this required list 2026-09-23 (per Josh, see
  # @details "Follow-up, 2026-09-23") - it's now optional, resolved per-row
  # below. "plot.sets" renamed to "plot.set" the same round. $Alldect ->
  # $all.dectections renamed 2026-09-27 (per Josh's reference-workbook
  # "Change.to" column) - see @details, "Column identifiers renamed,
  # 2026-09-27".
  ## 2026-10-09, per Josh: $facet -> $facet.header, new $facet.plot,
  ## $facet.panel dropped (old files still load - see plotutil_figlist_legacy())
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet.header", "facet.plot", "facet.set", "myso",
                          "all.detections", "40khzmyo", "facet.label_format",
                          "plot.set", "pool", "date.format",
                          "date.start", "date.end", "xaxe.interval")
  # Follow-up, 2026-09-23, per Josh (bug report: an explicit $yaxe.trans =
  # "log10" set in fig.list was silently ignored, rendering as if it were
  # "none") - every OPTIONAL per-row job setting get.setting()/the $plot.group
  # resolver read via an exact `%in% names(job)` check, same as
  # FIG.LIST.REQUIRED's own columns, and needs the same canonicalize.headers()
  # tolerance - see "Follow-up, 2026-09-23 (fig.list optional-column
  # whitespace/casing bug)" in Details.
  ## $plot.order/$facpan removed 2026-10-09 - $facet.set now sets the
  ## facets and their order
  FIG.LIST.OPTIONAL <- c("facet.label", "plot.group", "yaxe.trans", "loglabels", "y.scale",
                          "y.custom", "ymax", "legend", "plot.dimensions", "plot.zero",
                          "plot.zero_all.detections", "plot.zero_40khzmyo")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  ## "output.filename.pattern" deliberately removed from this required list
  ## per Josh's nineteenth follow-up (2026-09-16) - the saved file name is
  ## now built as described in @details, "File naming (round twenty-one)";
  ## $output.filename.pattern is no longer read at all. An
  ## $output.filename.pattern row left in an existing aes.plotopts sheet is
  ## harmless (simply ignored).
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
    "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
    "panel.border.linewidth", "legend.position",
    "xaxe.interval", "xaxe.title", "xaxe.date.buffer.days", "yaxe.title",
    "yaxe.trans", "loglabels", "y.scale", "ymax", "bar.width",
    "bar.alldetections.fill", "bar.40khzmyo.fill", "bar.fill.legend.title",
    "legend", "legend.groupval.title", "legend.groupval.colors",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "plot.width", "plot.height"
  )

  # canonicalize.headers()/standardize.headers() - see
  # claude/batz.util_standardize.headers.R and @details "Follow-up,
  # 2026-09-21" above: standardizes both a data frame's own headers and a
  # required-header list purely to MATCH columns, then renames every
  # matched column, IN A LOCAL COPY ONLY, to the required list's own
  # spelling - never mutates the caller's own data/suntimes/fig.list/
  # aes.plotopts object, and never changes anything this function returns.
  missing.msg <- function(canon, label) {
    if (length(canon$missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(canon$missing, collapse = ", ")))
    }
    NULL
  }

  check.parameters <- function(df, required, label) {
    if (!("parameter" %in% names(df))) return(NULL)  # already reported by missing.msg above
    missing <- required[!plotutil_param_key(required) %in% plotutil_param_key(df$parameter)]
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

  ## $obs -> $observations.count (2026-10-07, per Josh). Old plot frames
  ## saved before the rename still load: a lone $obs column is renamed to
  ## $observations.count in this function's own local copy.
  obs.std <- standardize.headers(names(data))
  if ("obs" %in% obs.std && !("observations_count" %in% obs.std)) {
    names(data)[obs.std == "obs"] <- "observations.count"
    cat("WARNING: `data` has the old header $obs - used as $observations.count. ",
        "Re-run batz.generate_plotframe.bat() to update the input file.\n", sep = "")
  }
  ## fig.list format 2026-10-09: old headers ($facet, $Alldect, no
  ## $facet.plot) still accepted - see plotutil_figlist_legacy()
  fig.list <- plotutil_figlist_legacy(fig.list, "batz.plotactivity_observations")
  data.canon        <- canonicalize.headers(data, DATA.REQUIRED)
  suntimes.canon    <- canonicalize.headers(suntimes, SUNTIMES.REQUIRED)
  fig.list.canon    <- canonicalize.headers(fig.list, FIG.LIST.REQUIRED)
  # Follow-up, 2026-09-23: a second canonicalize pass over fig.list.canon$df,
  # for the OPTIONAL per-row settings columns (see FIG.LIST.OPTIONAL above).
  # Deliberately separate from the required-header pass immediately above -
  # canonicalize.headers()'s own $missing would otherwise report an absent
  # OPTIONAL column as if it were a missing REQUIRED one (a hard stop), when
  # an absent/blank optional column is supposed to just fall through to
  # aes.plotopts via get.setting()/the $plot.group resolver. Only $df is kept
  # from this second pass; fig.list.canon$missing (used below to decide
  # whether to stop) still reflects FIG.LIST.REQUIRED only, unaffected.
  fig.list.canon$df <- canonicalize.headers(fig.list.canon$df, FIG.LIST.OPTIONAL)$df
  aes.plotopts.canon <- canonicalize.headers(aes.plotopts, AES.DEFAULT.REQUIRED)

  problems <- c(
    missing.msg(data.canon, "data"),
    missing.msg(suntimes.canon, "suntimes"),
    missing.msg(fig.list.canon, "fig.list"),
    missing.msg(aes.plotopts.canon, "aes.plotopts"),
    check.parameters(aes.plotopts.canon$df, plotutil_required_left(AES.DEFAULT.REQUIRED.PARAMETERS, plot.inputs), "aes.plotopts"),
    check.duplicates(data, "data"),
    check.duplicates(suntimes, "suntimes"),
    check.duplicates(fig.list, "fig.list"),
    check.duplicates(aes.plotopts, "aes.plotopts")
  )
  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n\n"))
  }

  data        <- data.canon$df
  suntimes    <- suntimes.canon$df
  fig.list    <- fig.list.canon$df
  aes.plotopts <- aes.plotopts.canon$df

  ## 2026-10-02, per Josh: data from batz.generate_plotframe.bat() with a
  ## pool.interval under one day has several rows per species/group/date -
  ## combined here to one row per date (see plotutil_collapse_pools()).
  data <- plotutil_collapse_pools(data, "batz.plotactivity_observations")

  unquote <- function(x) {
    x <- trimws(as.character(x))
    gsub('^"(.*)"$', "\\1", x)
  }

  # Parses $plot.set (2026-08-28, renamed from $plot.sets 2026-09-23) into a
  # character vector of one or more values - see @details above.
  parse.plot.sets <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) == 0 || is.na(x) || !nzchar(x)) return(character(0))
    x <- gsub('"', " ", x, fixed = TRUE)
    ## ";" list (2026-11-09); without a ";", values separated by spaces
    vals <- if (grepl(";", x, fixed = TRUE)) trimws(strsplit(x, ";", fixed = TRUE)[[1]]) else strsplit(trimws(x), "\\s+")[[1]]
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
  ## $date.start / $date.end: one date each (a window), or lists of the same
  ## length ("2026-06-26; 2026-06-28" / "2026-06-26; 2026-06-28"): each pair is
  ## a window and only the days in the windows are plotted (gaps left out)
  resolve.dates <- function(job, job.label) {
    sp <- function(v) { x <- trimws(gsub('"', "", unlist(strsplit(as.character(v), "[;,]")))); x[!is.na(x) & nzchar(x) & toupper(x) != "NA"] }
    ds <- parse.flex.date(sp(get.setting(job, "date.start"))); de <- parse.flex.date(sp(get.setting(job, "date.end")))
    if (length(ds) > 1 && !length(de)) de <- ds                   # list of dates, blank end: only those dates
    if (length(de) > 1 && !length(ds)) ds <- de
    if (length(ds) <= 1 && length(de) <= 1) return(list(start = ds[1], end = de[1], days = NULL))
    if (length(ds) != length(de))
      stop(sprintf("fig.list row '%s': $date.start has %d date(s) and $date.end has %d - give one date each, or the same number (one window per pair).",
                   job.label, length(ds), length(de)), call. = FALSE)
    if (any(is.na(ds) | is.na(de) | de < ds))
      stop(sprintf("fig.list row '%s': a $date.start / $date.end pair is not a date or ends before it starts.", job.label), call. = FALSE)
    days <- sort(unique(do.call(c, lapply(seq_along(ds), function(i) seq(ds[i], de[i], by = "day")))))
    cat(sprintf("NOTE: '%s' - %d date window(s) in $date.start/$date.end: only these %d day(s) are plotted.\n",
                job.label, length(ds), length(days)))
    list(start = min(days), end = max(days), days = days)
  }

  ## Settings resolution (round nineteen, per Josh's 2026-09-16 follow-up):
  ## $aes.style names a FIXED column to check first (default "override.value"
  ## - a blank column the user fills in directly on their own copy of the
  ## CSV), falling back to $default.value when that column doesn't exist or
  ## is blank for this row. Replaces the old project.name-matches-a-
  ## column-name mechanism entirely - project.name no longer participates in
  ## settings resolution, only in the saved file name (see the main loop
  ## below).
  get.default <- function(param) {
    if (!is.null(v.in <- plotutil_input_get(plot.inputs, param))) return(v.in)   # function call first
    row.idx <- which(plotutil_param_key(aes.plotopts$parameter) == plotutil_param_key(param))
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.plotopts$default.value[row.idx[1]])
    if (!is.na(aes.col <- plotutil_aes_style_col(aes.plotopts, aes.style))) {
      override <- aes.plotopts[[aes.col]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.plotopts$parameter, names(fig.list), "plot.dimensions", "plot.dimentions"), "batz.plotactivity_observations")

  get.setting <- function(job, param) {
    if (!is.null(v.in <- plotutil_input_get(plot.inputs, param))) return(v.in)   # function call, then fig.list, then plotopts
    if (param %in% names(job)) {
      v <- job[[param]]
      if (!is.null(v) && !is.na(v) && nzchar(trimws(as.character(v)))) {
        return(as.character(v))
      }
    }
    get.default(param)
  }

  ## facet presets (e.g. "NE") now live in plotutil_facet_presets
  ## (batz.util_plot.helpers.R) - 2026-10-09

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

    facet.kind <- tolower(trimws(job$facet.header))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: fig.list row for '%s' has facet.header = '%s' - skipped ($facet.header = \"sppid\" is the only value implemented so far).\n",
                   job.label, job$facet.header))
      next
    }
    facet.plot.flag <- !identical(toupper(trimws(as.character(job$facet.plot))), "FALSE")

    ## 2026-10-09, per Josh: $facet.set = a preset (e.g. "NE") or an
    ## ordered list ("AllDet;epfu;labo"); the order written is the facet
    ## order. $all.detections = TRUE adds "All detections" as the FIRST
    ## facet if $facet.set doesn't list it; a 40kHzMyo facet of its own
    ## goes last if not listed.
    facpan <- plotutil_resolve_facet_set(job$facet.set, job.label, "batz.plotactivity_observations")
    alldect.flag <- isTRUE(as.logical(job$all.detections)) || "All detections" %in% facpan
    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]])) || "40khzmyo" %in% facpan
    if (alldect.flag && !("All detections" %in% facpan)) facpan <- c("All detections", facpan)
    if (alldect.flag) facpan <- setdiff(facpan, "40khzmyo")   # drawn inside the All detections facet
    if (khz.flag && !alldect.flag && !("40khzmyo" %in% facpan)) facpan <- c(facpan, "40khzmyo")

    spp.plot <- facpan
    myso.flag <- isTRUE(as.logical(job$myso))
    if (myso.flag) spp.plot <- c(spp.plot, "Indiana bat")
    if (khz.flag) spp.plot <- c(spp.plot, "40khzmyo")
    spp.plot <- unique(trimws(spp.plot))
    facpan <- unique(trimws(facpan))

    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common_name")
    facpan <- batz.batusa_recode.names(facpan, batname.format.out = "common_name")

    pd <- data
    is.khz.raw <- norm.simple(pd$spp.id) %in% KHZ.ALIASES
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common_name")
    pd$spp.common[is.khz.raw] <- "40khzmyo"

    # --- $plot.group: which column of `data` to filter/group by (2026-08-28
    # follow-up - see Details). Follow-up, 2026-09-23, per Josh: no longer a
    # REQUIRED fig.list header - a blank value, or the column not existing
    # in fig.list at all, now defaults to "group" (matching
    # batz.generate_plotframe.bat()'s own output column and
    # batz.plotdetections_first.last()'s fixed grouping column) instead of
    # skipping the row - see Details for the full history/judgment call. A
    # row can still supply its own $plot.group value to override this. ---
    group.col <- plotutil_job_text(job, "plot.group")             # blank, NA or "NA" -> ""
    if (!nzchar(group.col)) {
      group.col <- "group"
    }
    if (!(group.col %in% names(pd))) {
      cat(sprintf("NOTE: fig.list row for '%s' has $plot.group = '%s', which is not a column of `data` - skipped.\n",
                   job.label, group.col))
      next
    }

    plot.sets.vals <- parse.plot.sets(job$plot.set)
    if (length(plot.sets.vals) > 0) {
      pd <- pd[tolower(trimws(as.character(pd[[group.col]]))) %in% tolower(plot.sets.vals), , drop = FALSE]
    }
    if (myso.flag) {
      dd <- parse.flex.date(pd$date)
      rd <- resolve.dates(job, job.label)
      in.range <- !is.na(dd) & dd >= rd$start & dd <= rd$end & (is.null(rd$days) | dd %in% rd$days)
      lu <- plotutil_myso_luso(pd$spp.common, spp.plot, facpan, in.range, "batz.plotactivity_observations", job.label)
      pd$spp.common <- lu$spp.common
      spp.plot <- lu$spp.plot
      facpan <- lu$facpan
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    rd <- resolve.dates(job, job.label)
    date.start <- rd$start; date.end <- rd$end; date.days <- rd$days
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]
    if (!is.null(date.days)) pd <- pd[pd$date.parsed %in% date.days, , drop = FALSE]

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.group = '%s', plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated. Check that $%s/$date in data actually overlap this row's $plot.set/$date.start/$date.end.\n",
                   job.label, group.col, paste(plot.sets.vals, collapse = ", "), date.start, date.end, group.col))
      next
    }

    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel, "All detections", pd$spp.common)
    pd$bar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")
    pd$group.val <- as.character(pd[[group.col]])

    # --- $pool: TRUE sums $observations.count across every selected $plot.set value into
    # ONE pooled bar per date/panel; FALSE keeps each selected value as its
    # own bar (drawn side-by-side/dodged at plotting time below) - see
    # Details/Follow-up 2026-08-28. ---
    pool.flag <- isTRUE(as.logical(job$pool))
    if (pool.flag) {
      pd <- stats::aggregate(observations.count ~ spp.common + facet.panel.value + bar.type + date.parsed,
                              data = pd, FUN = sum)
      pd$group.val <- "pooled"
    }

    # --- $legend (2026-08-29 follow-up, round 2 - see Details): whether to
    # color each dodged bar by which $plot.set value it is. Optional
    # per-row, falls back to aes.plotopts's own "legend" parameter (default
    # TRUE) - same get.setting()/get.default() pattern as $yaxe.trans/
    # $y.scale/etc.
    legend.flag <- isTRUE(as.logical(get.setting(job, "legend")))

    # $facet = "sppid": $facet.label sets the species name format of the
    # panel titles (2026-10-02, per Josh) - "common_name" (default when
    # blank), "scientific_name", "code4", "code6", "both", or the older
    # "common"/"latin"/"scientific". See plotutil_facet_labels() in
    # batz.util_plot.helpers.R.
    panel.levels.raw <- facpan
    panel.labels <- plotutil_facet_labels(panel.levels.raw, get.setting(job, "facet.label_format"),
                                      job.label, "batz.plotactivity_observations", alt = plotutil_job_text(job, "facet.label"))
    ## facet order = $facet.set order (2026-10-09; replaces $plot.order)
    ordered.levels <- panel.levels.raw
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels, labels = panel.labels[ordered.levels])

    # --- Y-axis resolution: see @details above for the full explanation ---
    yaxe.trans <- tolower(trimws(get.setting(job, "yaxe.trans")))
    if (!yaxe.trans %in% c("none", "log", "log10")) {
      cat(sprintf("NOTE: '%s' - $yaxe.trans = '%s' is not one of none/log/log10 - defaulting to 'none'.\n",
                   job.label, get.setting(job, "yaxe.trans")))
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
      ymax.raw <- max(pd$observations.count, na.rm = TRUE)
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
      # back to raw counts for the label. When $yaxe.trans = "none" this is
      # identical to the old "evenly spaced in raw count units" behavior
      # (trans.fn/inv.trans.fn are both the identity function, so nothing
      # changes for that case) - it only matters, and only differs, when
      # $yaxe.trans is "log"/"log10".
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
    y.upper <- max(c(ymax.raw, raw.breaks, pd$observations.count), na.rm = TRUE)

    pd$obs.plot <- trans.fn(pd$observations.count)
    ## plot.zero = FALSE: 0 bars are kept (bar positions unchanged) but drawn fully transparent
    ## one setting per data set: plot.zero_all.detections, plot.zero_40khzmyo (else plot.zero)
    z.all <- plotutil_zero_set("all.detections", plot.inputs, job, get.default)
    z.khz <- plotutil_zero_set("40khzmyo", plot.inputs, job, get.default)
    pd$.alpha <- ifelse(pd$observations.count == 0 & !ifelse(pd$bar.type == "40kHzMyo", z.khz, z.all), 0, 1)
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
      facpan = facpan, spp.plot = spp.plot, date.start = date.start, date.end = date.end, date.days = date.days,
      khz.flag = khz.flag, break.pos = break.pos, break.labels = break.labels,
      y.upper.plot = trans.fn(y.upper), group.col = group.col,
      plot.sets.vals = plot.sets.vals, pool.flag = pool.flag,
      legend.flag = legend.flag,
      facet.plot = facet.plot.flag, facet.raw = ordered.levels,
      facet.ids = plotutil_facet_id(ordered.levels, facet.kind),
      resolved.legend.position = get.default("legend.position")  # exposed for testing the aes.style resolver (round nineteen)
    )
    cat(sprintf("Prepared plot data for '%s': %d observation row(s) across %d panel(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw)))
  }

  ## $facet.plot = FALSE (2026-10-09): one plot per facet instead of one
  ## faceted plot
  plots.expanded <- list()
  for (job.key in names(plots)) {
    p <- plots[[job.key]]
    if (isTRUE(p$facet.plot)) {
      plots.expanded[[job.key]] <- p
    } else {
      parts <- plotutil_split_facets(p, p$facet.raw, "sppid", "batz.plotactivity_observations")
      for (k in seq_along(parts)) plots.expanded[[paste0(job.key, ".", k)]] <- parts[[k]]
    }
  }
  plots <- plots.expanded

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
      ## date windows: the X axis holds only the listed days, side by side
      p$pd$x.val <- if (is.null(p$date.days)) p$pd$date.parsed else
        factor(format(p$pd$date.parsed, "%Y-%m-%d"), levels = format(p$date.days, "%Y-%m-%d"))
      xaxe.buffer <- suppressWarnings(as.numeric(get.default("xaxe.date.buffer.days")))
      if (is.na(xaxe.buffer)) xaxe.buffer <- 0.5

      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      # Drawn as TWO separate geom_col() layers (all-detections first/bottom,
      # 40kHzMyo second/on top) - see @details for the real overlay bug this
      # fixed.
      #
      # $pool = FALSE (2026-08-28 follow-up - see Details): when more than
      # one $plot.set value is selected and not pooled, each is drawn as
      # its own bar, dodged side-by-side within the same date (via
      # position_dodge2(), grouped by $group.val - the raw $plot.group
      # column's value for that row). $pool = TRUE (or only one selected
      # value to begin with) draws a single bar per date, same as before.
      dodge.flag <- !isTRUE(p$pool.flag) && length(unique(p$pd$group.val)) > 1
      bar.width.val <- as.numeric(get.default("bar.width"))
      bar.position <- if (dodge.flag) {
        # preserve = "single" (2026-08-29 follow-up, per Josh "keep the bar
        # sizes constant"): see Details.
        ggplot2::position_dodge2(width = bar.width.val, padding = 0.1, preserve = "single")
      } else {
        "identity"
      }

      # $legend / per-bar coloring (2026-08-29 follow-up, round 2 - see
      # Details): when dodging (pool = FALSE, >1 selected $plot.set value)
      # AND $legend is on, every bar is FILLED with its own $plot.set
      # value's own color. The "40kHzMyo" bar keeps its own fixed color
      # regardless of which value it belongs to - both live in ONE combined
      # fill scale/legend, since fill can only be mapped to one variable at
      # a time.
      show.groupval.color <- isTRUE(p$legend.flag) && dodge.flag

      if (show.groupval.color) {
        groupval.levels <- sort(unique(p$pd$group.val))
        groupval.palette <- strsplit(get.default("legend.groupval.colors"), ";", fixed = TRUE)[[1]]
        groupval.palette <- trimws(groupval.palette[nzchar(trimws(groupval.palette))])
        if (length(groupval.palette) == 0) {
          # fallback palette (ColorBrewer "Dark2"-style) if aes.plotopts's
          # own $legend.groupval.colors is blank/unusable - never fatal.
          groupval.palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                                 "#66a61e", "#e6ab02", "#a6761d", "#666666")
        }
        # cycles back to the first color if there are more selected values
        # than colors configured - see Details for why this was chosen over
        # erroring or auto-generating extra colors.
        groupval.colors <- groupval.palette[((seq_along(groupval.levels) - 1) %% length(groupval.palette)) + 1]
        names(groupval.colors) <- groupval.levels

        pd.fill <- p$pd
        pd.fill$fill.val <- ifelse(pd.fill$bar.type == "40kHzMyo", "40kHzMyo", pd.fill$group.val)
        fill.values <- c(groupval.colors, `40kHzMyo` = get.default("bar.40khzmyo.fill"))
        fill.breaks <- if (isTRUE(p$khz.flag)) c(groupval.levels, "40kHzMyo") else groupval.levels

        g <- ggplot2::ggplot(pd.fill, ggplot2::aes(x = x.val)) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.val, group = group.val, alpha = .alpha),
                             width = bar.width.val, position = bar.position) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.val, group = group.val, alpha = .alpha),
                             width = bar.width.val, position = bar.position) +
          ggplot2::scale_fill_manual(name = get.default("legend.groupval.title"),
                                      breaks = fill.breaks, limits = names(fill.values), values = fill.values)
      } else {
        g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = x.val)) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val, alpha = .alpha),
                             width = bar.width.val, position = bar.position) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val, alpha = .alpha),
                             width = bar.width.val, position = bar.position) +
          ggplot2::scale_fill_manual(name = get.default("bar.fill.legend.title"),
            breaks = fill.legend.breaks, limits = fill.legend.limits,
            values = c(`All detections` = get.default("bar.alldetections.fill"),
                       `40kHzMyo` = get.default("bar.40khzmyo.fill")))
      }

      g <- g + ggplot2::scale_alpha_identity() +                         # 0 bars transparent (plot.zero = FALSE)
        ggplot2::scale_y_continuous(limits = c(0, p$y.upper.plot), breaks = p$break.pos, labels = p$break.labels,
          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        (if (is.null(p$date.days))
          ggplot2::scale_x_date(limits = c(p$date.start - xaxe.buffer, p$date.end + xaxe.buffer),
            breaks = xaxe.breaks, date_labels = xaxe.date.labels.fmt,
            name = paste0("\n", get.setting(p$job, "xaxe.title")))
        else {
          lv <- format(p$date.days, "%Y-%m-%d")
          br <- if (length(lv) <= round(xaxe.n.labels) || round(xaxe.n.labels) < 2) lv else
            lv[unique(round(seq(1, length(lv), length.out = round(xaxe.n.labels))))]
          ggplot2::scale_x_discrete(limits = lv, breaks = br, drop = FALSE,
            labels = stats::setNames(format(p$date.days, xaxe.date.labels.fmt), lv),
            name = paste0("\n", get.setting(p$job, "xaxe.title")))
        }) +
        ggplot2::facet_wrap(~facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(linewidth = as.numeric(get.default("panel.border.linewidth")), colour = "grey20", fill = NA),
          legend.position = get.default("legend.position"),
          plot.title = ggplot2::element_text(hjust = as.numeric(get.default("plot.title.hjust")), face = plotutil_face(get.default("plot.title.face")), size = as.numeric(get.default("plot.title.size"))),
          axis.title = ggplot2::element_text(size = as.numeric(get.default("axis.title.size")), face = plotutil_face(get.default("axis.title.face"))),
          axis.text = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
          legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
          legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size")), face = plotutil_face(get.default("legend.title.face"))),
          strip.text = ggplot2::element_text(size = plotutil_num(get.default("strip.text.size"), 10)),
          panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt")
        )

      ggplots[[job.key]] <- g

      ## File naming (round twenty-one), per Josh (2026-09-24): "the plots
      ## come out too fast resulting in overwriting of plots" - see
      ## @details, "File naming (round twenty-one)". <daterange> is this
      ## job's own resolved $date.start/$date.end (Date objects, already
      ## parsed above), formatted YYYYMMDDtoYYYYMMDD. This REPLACES the old
      ## aru.token ($plot.set value(s) joined with "+", "-pooled" suffix)
      ## as the filename's own plot-identifying token, per Josh's literal
      ## naming spec - see @details for this scope decision, flagged for
      ## Josh (the $plot.set/pooled distinction is no longer recoverable
      ## from the file name alone).
      ## File naming 2026-10-09, per Josh - see plotutil_figlist_filename():
      ## <plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<dates>_<TIMESTAMP>.png
      ## (faceted) or <plot.name>_<facet>_<plot.set>_<dates>_<TIMESTAMP>.png
      fname <- plotutil_figlist_filename(p$job$plot.name, p$facet.ids, p$facet.plot,
                                     if (isTRUE(p$pool.flag)) paste0(paste(p$plot.sets.vals, collapse = "+"), "-pooled") else p$plot.sets.vals,
                                     p$date.start, p$date.end, fn.name = "batz.plotactivity_observations", project.name = project.name)
      fname <- file.path(dir.save, fname)

      dims <- plotutil_plot_dimensions(p$job, plot.inputs, job.label, "batz.plotactivity_observations")   # $plot.dimensions (inches)
      ggplot2::ggsave(fname, plot = g,
        width = if (!is.null(dims)) dims[1] else as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
        height = if (!is.null(dims)) dims[2] else as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
        units = if (!is.null(dims)) "in" else get.default("ggsave.units"), dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

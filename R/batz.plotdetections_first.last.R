#' Plot each species' earliest and latest nightly detections
#'
#' Draws the standard bat-detection report plot. For each species facet
#' (plus an optional "All detections" facet and an optional 40kHzMyo
#' indicator) it shows the earliest-to-latest detection window of every
#' monitoring night as a crossbar, with Dusk, Midnight and Dawn reference
#' lines. The Y axis is one shared Noon-to-Noon window; the X axis is the
#' date. One PNG is saved per \code{fig.list} row whose \code{$plot.type} is
#' \code{"bat.detection"} (or one per facet when \code{$facet.plot = FALSE}).
#'
#' @param data Data frame of per-species, per-night detection windows,
#'   normally the output of \code{\link{batz.generate_plotframe.bat}}.
#'   Required columns: \code{$spp.id}, \code{$date}, \code{$group},
#'   \code{$observations.count}, \code{$mins2.noon.min},
#'   \code{$mins2.noon.max}, \code{$vetting.type}. \code{$mins2.noon.min}
#'   and \code{$mins2.noon.max} are minutes from that night's noon to the
#'   first and last detection. Old header \code{$obs} is still accepted (used
#'   as \code{$observations.count}, with a WARNING).
#' @param fig.list Data frame, one row per plot. See "fig.list columns" in
#'   Details. Column names must be unique.
#'   Cells holding "NA" (a heading not used by this plot type, as in
#'   \code{\link{batz.generate_figlist}}) are treated as blank.
#'   Only rows whose \code{$plot} is \code{TRUE} or \code{1} are
#'   plotted; rows with \code{FALSE}, any other entry or a blank are
#'   skipped (NOTE). A fig.list with no \code{$plot} column has every
#'   row plotted (NOTE).
#' @param suntimes Data frame of sunrise and sunset times, e.g. the output of
#'   \code{\link{batz.generate_suntimes.arulist}}. Required columns:
#'   \code{$aru.name}, \code{$date}, \code{$date.monitoringnight},
#'   \code{$sunregion}, \code{$time.zone}, \code{$sunregion.type},
#'   \code{$schedule1}, \code{$schedule2}, \code{$sunset},
#'   \code{$sunset.unix}, \code{$sunrise}, \code{$sunrise.unix},
#'   \code{$sunrise.monitoringnight}, \code{$sunrise.monitoringnight.unix}.
#'   Only \code{$aru.name}, \code{$date}, \code{$sunset} (Dusk) and
#'   \code{$sunrise.monitoringnight} (Dawn: the sunrise that ends the night
#'   starting on \code{$date}) are used for drawing.
#' @param aes.plotopts Data frame of plot settings (plotopts), one row per
#'   parameter, e.g. \verb{plotopts_first.last.csv} from
#'   \code{\link{batz.generate_plotopts}}. Required columns:
#'   \code{$category}, \code{$parameter}, \code{$default.value}. The custom
#'   style column (named by \code{aes.style}) and \code{$notes} are optional.
#'   See "plotopts" in Details.
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plotdet_first.last} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#'   (The old argument name \code{aes.default} is still read, with a WARNING.)
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
#'     \item{\code{$pd}}{Plotted detection rows: the \code{data} columns plus
#'       \code{$spp.common}, \code{$date.parsed}, \code{$time.min},
#'       \code{$time.max}, \code{$facet.panel.value} (factor of facet titles)
#'       and \code{$crossbar.type}.}
#'     \item{\code{$sdb}}{Suntimes rows used: the \code{suntimes} columns plus
#'       \code{$date.parsed}, \code{$dusk.time}, \code{$dawn.time},
#'       \code{$midnight.time}.}
#'     \item{\code{$panel.labels}, \code{$facpan}, \code{$spp.plot}}{Facet
#'       titles, facet list and the species list used to filter.}
#'     \item{\code{$y.start}, \code{$y.end}, \code{$tz}}{Y-axis window and
#'       time zone.}
#'     \item{\code{$date.start}, \code{$date.end}}{X-axis date window.}
#'     \item{\code{$facet.plot}, \code{$facet.raw}, \code{$facet.ids}}{Facet
#'       mode, facet names and their short IDs used in the file name.}
#'     \item{\code{$khz.flag}}{TRUE when 40kHzMyo is on for this plot.}
#'     \item{\code{$resolved.legend.position}, \code{$resolved.dawn.color}}{Resolved
#'       plotopts values.}
#'   }
#'   \code{$ggplots} holds the ggplot objects (empty when ggplot2 is not
#'   installed).
#'
#'   Files written: one PNG per plot in \code{dir.save}. Faceted:
#'   \verb{<project.name>_<plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#'   One per facet:
#'   \verb{<project.name>_<plot.name>_<facet>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#'   Facet IDs are four-letter codes (\code{code4}), \code{AllDet} and
#'   \code{40kMyo}. A blank \code{$plot.set} gives \code{all}. Spaces become
#'   "-" and characters Windows does not allow are removed. Names over 100
#'   characters are shortened step by step (drop the year from a same-year
#'   end date, then cut each word of the facet IDs, \code{$plot.set} and
#'   \code{$plot.name} to 6 characters).
#'
#' @details
#' \strong{fig.list columns.} Required headers are matched ignoring case and
#' "." / "_" / space differences.
#' \describe{
#'   \item{\code{$plot.type}}{Only \code{"bat.detection"} rows are plotted;
#'     other values are skipped with a NOTE. Rows with a blank
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
#'     shown, even with no detections.}
#'   \item{\code{$myso}}{TRUE adds Indiana bat (MYSO). If little brown bat
#'     (MYLU) records are in the plotted data (plot set and date range), all
#'     MYLU and MYSO records are pooled as LUSO and LUSO replaces MYLU/MYSO in
#'     the facets (in the place of the first of them, or added last), with a
#'     NOTE. Otherwise Indiana bat is plotted and gets its own facet (added
#'     last if not listed in \code{$facet.set}).}
#'   \item{\code{$all.detections}}{TRUE adds an "All detections" facet first,
#'     if \code{$facet.set} does not already list it.}
#'   \item{\code{$40khzmyo}}{TRUE (or \code{"40khzmyo"} in
#'     \code{$facet.set}) plots 40kHzMyo detections. They are drawn inside the "All detections" facet
#'     when it exists, otherwise in their own last facet. The 40kHzMyo legend
#'     key is shown whenever this is on, even with no 40kHzMyo detections.}
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
#'   \item{\code{$plot.set}}{One ARU/group value. \code{data} is filtered on
#'     \code{$group} and \code{suntimes} on \code{$aru.name} (ignoring case).
#'     Blank = no filter.}
#'   \item{\code{$date.format}}{X-axis date label format (strftime style,
#'     e.g. \code{"\%b-\%d/n\%Y"}). A literal "/n" becomes a line break.}
#'   \item{\code{$date.start}, \code{$date.end}}{Date window of the plot.
#'     Read as m/d/Y, Y-m-d or m/d/y.}
#'   \item{\code{$xaxe.interval}}{Number of evenly spaced labeled dates on
#'     the X axis, from \code{$date.start} to \code{$date.end}. An unusable
#'     value gives 2 labels (NOTE).}
#'   \item{\code{$xaxe.title}}{X-axis title.}
#'   \item{Optional: \code{$yaxe.title}, \code{$midnight}, \code{$time.zone},
#'     \code{$yaxe.limit.min}, \code{$yaxe.limit.max}}{A non-blank value
#'     overrides the plotopts value of the same name for that plot.}
#' }
#' Old fig.list files still load: \code{$facet} is used as
#' \code{$facet.header}, \code{$Alldect} / \code{$all.dectections} as
#' \code{$all.detections}, and a missing \code{$facet.plot} is added as TRUE,
#' each with a WARNING. Old \code{$facet.panel}, \code{$facpan} and
#' \code{$plot.order} columns are ignored (NOTE). Exact duplicate rows
#' (every column the same) are removed before plotting (NOTE).
#'
#' \strong{plotopts (aes.plotopts).} Each setting is read from the first row
#' whose \code{$parameter} matches the name (ignoring case and spaces).
#' The value is the \code{aes.style} column when that cell is not blank,
#' otherwise \code{$default.value}. For the optional fig.list settings above,
#' a non-blank fig.list value wins over both. These \code{$parameter} rows
#' must exist (case and spaces ignored), or the function stops listing the missing
#' ones: \code{facpan.numcol}, \code{plot.title.size},
#' \code{plot.title.hjust}, \code{axis.title.size}, \code{axis.text.size},
#' \code{legend.text.size}, \code{legend.title.size},
#' \code{panel.spacing.x}, \code{panel.border.linewidth},
#' \code{legend.position}, \code{xaxe.interval},
#' \code{yaxe.break.interval}, \code{yaxe.labelformat},
#' \code{yaxe.break.labels}, \code{midnight.linetype},
#' \code{midnight.color}, \code{midnight.dots.color},
#' \code{midnight.dots.size}, \code{dawn.linetype}, \code{dawn.color},
#' \code{dusk.linetype}, \code{dusk.color},
#' \code{reference.line.legend.title}, \code{crossbar.alldetections.fill},
#' \code{crossbar.40khzmyo.fill}, \code{crossbar.linewidth},
#' \code{crossbar.fill.legend.title}, \code{ggsave.dpi},
#' \code{ggsave.units}, \code{ggsave.width.pad}, \code{ggsave.height.pad},
#' \code{plot.width}, \code{plot.height}. Also read: \code{time.zone},
#' \code{yaxe.title}, \code{midnight}, and \code{yaxe.limit.min} /
#' \code{yaxe.limit.max} (HH:MM; other values stop with an error pointing to
#' an old numeric-minutes plotopts file). An \code{output.filename.pattern}
#' row is ignored.
#'
#' \strong{Input checks.}
#' \itemize{
#'   \item Special characters are stripped or checked (\code{strip.special},
#'     \code{strip.special.plotopts}).
#'   \item Required headers of all four inputs are matched and renamed to the
#'     expected spelling. Missing headers, missing plotopts rows and
#'     duplicate column names are collected and reported in one error.
#'   \item Plot-frame rows from pools shorter than one day are combined to
#'     one row per species/group/date: \code{$observations.count} summed,
#'     earliest \code{$mins2.noon.min}, latest \code{$mins2.noon.max}
#'     (NOTE).
#' }
#'
#' \strong{Preparing each plot.}
#' \itemize{
#'   \item \code{data} is given \code{$spp.common} (common names) and
#'     filtered to the \code{$plot.set} group, the facet species (plus
#'     Indiana bat or LUSO for \code{$myso}, plus 40kHzMyo) and the date
#'     window. A row with no data left is skipped (NOTE).
#'   \item \code{$mins2.noon.min} / \code{$mins2.noon.max} are placed on one
#'     shared reference noon (\code{$time.min}, \code{$time.max}), so every
#'     night uses the same Y axis. The Y window runs 24 hours from
#'     \code{yaxe.limit.min}.
#'   \item \code{suntimes} is filtered to the same ARU and dates (no match
#'     only gives a NOTE). Dusk, Dawn and Midnight are placed on the same
#'     axis.
#' }
#'
#' \strong{Drawing.}
#' \itemize{
#'   \item Y breaks every \code{yaxe.break.interval}, labeled with the
#'     ";"-separated \code{yaxe.break.labels}. If the counts differ, times
#'     formatted with \code{yaxe.labelformat} are used (NOTE).
#'   \item Midnight line (\code{midnight}): \code{"short"} (line across the
#'     suntimes dates), \code{"long"} (full panel width), \code{"dots"} (one
#'     short dash per date, \code{midnight.dots.color} and
#'     \code{midnight.dots.size}) or \code{"none"}. Other values fall back to
#'     \code{"short"} (NOTE). Its width is half of
#'     \code{panel.border.linewidth}, so it looks as thick as the panel
#'     border.
#'   \item Crossbars have a fixed width of 0.9 day and are filled by
#'     \code{$crossbar.type} (\code{crossbar.alldetections.fill},
#'     \code{crossbar.40khzmyo.fill}). The "All detections" fill has no
#'     legend key.
#'   \item The X axis runs from \code{$date.start} to \code{$date.end} with
#'     half a day of padding so edge bars are not cut. Facets are laid out in
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
#' }
#'
#' @seealso \code{\link{batz.generate_plotframe.bat}},
#'   \code{\link{batz.generate_suntimes.arulist}},
#'   \code{\link{batz.generate_plotopts}},
#'   \code{\link{batz.plotactivity_observations}}
#'
#' @examples
#' \dontrun{
#' result <- batz.plotdetections_first.last(
#'   data = vetted.processed,
#'   fig.list = fig.list,
#'   suntimes = aru.suntimes,
#'   aes.plotopts = plotopts.first.last,
#'   dir.save = "C:/path/to/output/folder"
#' )
#' result$ggplots[[1]]
#' }
#'
#' @export
batz.plotdetections_first.last <- function(data, fig.list, suntimes,
                                            aes.plotopts = NULL, project.name = "new.project",
                                            aes.style = "override.value",
                                            dir.save = getwd(),
                                            strip.special = TRUE,
                                            strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(plotutil_dots(...), "batz.plotdetections_first.last")

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.plotopts <- paste(deparse(substitute(aes.plotopts)), collapse = "")
  fig.list <- plotutil_figlist_na(fig.list)       # "NA" = heading not used by this plot type
  fig.list <- plotutil_figlist_plot(fig.list, "batz.plotdetections_first.last")   # only rows with $plot = TRUE or 1
  if (is.data.frame(fig.list) && nrow(fig.list) == 0) return(invisible(list(plots = list(), ggplots = list())))
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_aes_legacy(plotutil_dots(...), "batz.plotdetections_first.last")   # old argument name aes.default / plotopts
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_default_plotopts("batz.plotdetections_first.last")   # built-in settings
  data <- special.strip(data, strip.special)$df
  ## fig.list is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    fig.list <- special.strip(fig.list, TRUE)$df
  } else {
    special.check.plotopts(fig.list, "fig.list", objname.fig.list, "batz.plotdetections_first.last")
  }
  suntimes <- special.strip(suntimes, strip.special)$df
  ## aes.plotopts is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.plotopts <- special.strip(aes.plotopts, TRUE)$df
  } else {
    special.check.plotopts(aes.plotopts, "aes.plotopts", objname.aes.plotopts, "batz.plotdetections_first.last")
  }

  ## Header standardization (per Josh, 2026-09-14 project preference): NOT
  ## applied to any of the four *.REQUIRED*/DATA.REQUIRED constants below -
  ## `data`/`fig.list`/`suntimes`/`aes.plotopts` are already-loaded data
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
  ##
  ## Follow-up (2026-09-25, round twenty-six, per Josh's package-wide
  ## suntimes header rename): $suns/$suns.unix/$sunr/$sunr.unix/$sunr.mon/
  ## $sunr.mon.unix are now $sunset/$sunset.unix/$sunrise/$sunrise.unix/
  ## $sunrise.monitoringnight/$sunrise.monitoringnight.unix - NOT
  ## backward-compatible (a different word in every case, which
  ## canonicalize.headers() cannot bridge) - see @details, "Follow-up,
  ## 2026-09-25...round twenty-six". $date.mon (a separate, pre-existing,
  ## unrelated bug also caught in this same audit pass) is now
  ## $date.monitoringnight, matching round twenty-five - see @details,
  ## "Same audit pass, a second, separate...bug".
  ##
  ## Follow-up (2026-09-27, per Josh's reference-workbook "Change.to"
  ## column): FIG.LIST.REQUIRED's "Alldect" entry is now "all.dectections"
  ## (Josh's own spelling, kept exactly as given) - see @details, "Column
  ## identifiers renamed, 2026-09-27...".
  DATA.REQUIRED <- c("spp.id", "date", "group", "observations.count",
                           "mins2.noon.min", "mins2.noon.max", "vetting.type")
  SUNTIMES.REQUIRED <- c("aru.name", "date", "date.monitoringnight", "sunregion", "time.zone",
                             "sunregion.type", "schedule1", "schedule2", "sunset",
                             "sunset.unix", "sunrise", "sunrise.unix", "sunrise.monitoringnight",
                             "sunrise.monitoringnight.unix")
  ## 2026-10-09, per Josh: $facet -> $facet.header, new $facet.plot,
  ## $facet.panel dropped (old files still load - see plotutil_figlist_legacy())
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet.header", "facet.plot", "facet.set", "myso",
                                 "all.detections", "40khzmyo", "facet.label_format",
                                 "plot.set", "date.format", "date.start", "date.end",
                                 "xaxe.interval", "xaxe.title")
  FIG.LIST.OPTIONAL <- c("facet.label", "plot.dimensions")   # read directly when present
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  # 2026-08-27, later still - real bug hit on Josh's machine: his loaded
  # aes.plotopts was an OLDER copy of batactivity.plotoptions.csv
  # from before $panel.spacing.x was added (see the round above). The
  # column-structure check right below (AES.DEFAULT.REQUIRED)
  # only verifies aes.plotopts HAS the right columns
  # (category/parameter/default.value) - it never checked that every
  # PARAMETER ROW this function actually depends on is present. With
  # $panel.spacing.x missing, get.default("panel.spacing.x") silently
  # returned NA (its own documented behavior for an unknown parameter),
  # as.numeric(NA) stayed NA, and grid::unit(NA, "pt") only failed much
  # later and far downstream, deep inside grid's own rendering code -
  # "Error in grid.Call.graphics(C_setviewport, vp, TRUE): non-finite
  # location and/or size for viewport" - which names no setting and gives
  # no hint that a CSV row is missing. Reproduced directly: calling
  # get.default() on a parameter absent from a real aes.plotopts
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
  ## all. An $output.filename.pattern row left in an existing aes.plotopts
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
  fig.list <- plotutil_figlist_legacy(fig.list, "batz.plotdetections_first.last")
  data.canon        <- canonicalize.headers(data, DATA.REQUIRED)
  suntimes.canon    <- canonicalize.headers(suntimes, SUNTIMES.REQUIRED)
  fig.list.canon    <- canonicalize.headers(fig.list, FIG.LIST.REQUIRED)
  aes.plotopts.canon <- canonicalize.headers(aes.plotopts, AES.DEFAULT.REQUIRED)

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

  ## check.duplicates runs on the ORIGINAL (pre-canonicalization) frames -
  ## a frame with duplicate column names is stopped on before any renaming
  ## is attempted on it, same as before this round's change.
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
    ## blank line between each data frame's own missing-headers message,
    ## per Josh (2026-09-21) - was a single "\n" before this round.
    stop(paste(problems, collapse = "\n\n"))
  }

  data        <- data.canon$df
  suntimes    <- suntimes.canon$df
  fig.list    <- fig.list.canon$df
  aes.plotopts <- aes.plotopts.canon$df

  ## 2026-10-02, per Josh: data from batz.generate_plotframe.bat() with a
  ## pool.interval under one day has several rows per species/group/date -
  ## combined here to one row per date (see plotutil_collapse_pools()).
  data <- plotutil_collapse_pools(data, "batz.plotdetections_first.last")

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
  # (Round twenty-six, 2026-09-25: $suns/$sunr.mon referenced in this
  # historical comment are now $sunset/$sunrise.monitoringnight - see
  # @details, "Follow-up, 2026-09-25...round twenty-six".)
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
  ## "override.value" - a blank column the user fills in directly on their
  ## own copy of the CSV), falling back to $default.value when that column
  ## doesn't exist or is blank for this row. This replaces the old
  ## project.name-matches-a-column-name mechanism entirely - project.name
  ## no longer has any role in settings resolution, only in the saved file
  ## name (see the main loop below).
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
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.plotopts$parameter, names(fig.list), "plot.dimensions", "plot.dimentions"), "batz.plotdetections_first.last")

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
    facpan <- plotutil_resolve_facet_set(job$facet.set, job.label, "batz.plotdetections_first.last")
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
    facpan   <- unique(trimws(facpan))

    # Canonicalize to the reference table's own spelling (see @details) -
    # keeps a slightly-off list (typed by hand, or from an older spec) lined
    # up with data$spp.common, which is always canonical.
    #
    # batname.format.out = "common_name" (was "common" prior to 2026-09-25 -
    # see @details, "Follow-up, 2026-09-25...column rename").
    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common_name")
    facpan   <- batz.batusa_recode.names(facpan, batname.format.out = "common_name")

    # ---- filter data to this job's ARU + species list ----
    pd <- data
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common_name")

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$group)) == tolower(plot.set.val), , drop = FALSE]
    }
    if (myso.flag) {
      dd <- parse.flex.date(pd$date)
      in.range <- !is.na(dd) & dd >= parse.flex.date(get.setting(job, "date.start")) &
        dd <= parse.flex.date(get.setting(job, "date.end"))
      lu <- plotutil_myso_luso(pd$spp.common, spp.plot, facpan, in.range, "batz.plotdetections_first.last", job.label)
      pd$spp.common <- lu$spp.common
      spp.plot <- lu$spp.plot
      facpan <- lu$facpan
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

    dusk.real <- parse.flex.datetime(sdb$sunset, tz)
    # Dawn ending THIS monitoring night (which starts at $sunset/dusk of
    # $date) is $sunrise.monitoringnight - sunrise on the FOLLOWING day - not
    # $sunrise, which is sunrise ON $date itself (i.e. the dawn ending the
    # PREVIOUS night). Using $sunrise here was a real bug caught during
    # rendering (back when these columns were named $sunr/$sunr.mon): it
    # placed Dawn ~5 hours before Noon on the reference date, outside the
    # plotted Noon-to-Noon window, silently dropping the Dawn line from
    # every panel. (Round twenty-six, 2026-09-25: $suns/$sunr.mon renamed
    # to $sunset/$sunrise.monitoringnight - see @details, "Follow-up,
    # 2026-09-25...round twenty-six".)
    dawn.real <- parse.flex.datetime(sdb$sunrise.monitoringnight, tz)
    local.noon <- as.POSIXct(paste(format(sdb$date.parsed), rep("12:00:00", nrow(sdb))), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + rep(12 * 3600, nrow(sdb))

    # ---- facet panel labels, via batz.batusa_recode.names() ----
    # Every panel in facpan is shown even with 0 matching detections (see
    # @details) - so the full facpan list defines the facet levels.
    # $facet = "sppid": $facet.label sets the species name format of the
    # panel titles (2026-10-02, per Josh) - "common_name" (default when
    # blank), "scientific_name", "code4", "code6", "both", or the older
    # "common"/"latin"/"scientific". See plotutil_facet_labels() in
    # batz.util_plot.helpers.R.
    panel.levels.raw <- facpan
    panel.labels <- plotutil_facet_labels(panel.levels.raw, get.setting(job, "facet.label_format"),
                                      job.label, "batz.plotdetections_first.last", alt = plotutil_job_text(job, "facet.label"))

    ## facet order = $facet.set order (2026-10-09; replaces $plot.order)
    ordered.levels <- panel.levels.raw
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels,
                                    labels = panel.labels[ordered.levels])

    # ---- y-axis settings (time-of-day) ----
    yaxe.limit.min <- get.setting(job, "yaxe.limit.min")
    yaxe.limit.max <- get.setting(job, "yaxe.limit.max")
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.min) || !grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.max)) {
      stop(sprintf(paste("$yaxe.limit.min/$yaxe.limit.max ('%s'/'%s') don't look like HH:MM time-of-day",
                          "values - aes.plotopts may be an old, numeric-minutes-based copy of",
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
      facet.plot = facet.plot.flag, facet.raw = ordered.levels,
      facet.ids = plotutil_facet_id(ordered.levels, facet.kind),
      khz.flag = khz.flag,   # carried through so the legend key below can be driven by "$40khzmyo is TRUE for this plot" rather than "a detection happened to occur" - see the follow-up note below
      resolved.legend.position = get.default("legend.position"),  # exposed for testing the aes.style-driven resolver (round nineteen) without needing to render/introspect a ggplot object
      resolved.dawn.color = get.default("dawn.color")              # exposed for testing the fall-through-to-default case
    )

    cat(sprintf("Prepared plot data for '%s': %d detection rows across %d panel(s), %d suntimes row(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw), nrow(sdb)))
  }

  ## $facet.plot = FALSE (2026-10-09): one plot per facet instead of one
  ## faceted plot
  plots.expanded <- list()
  for (job.key in names(plots)) {
    p <- plots[[job.key]]
    if (isTRUE(p$facet.plot)) {
      plots.expanded[[job.key]] <- p
    } else {
      parts <- plotutil_split_facets(p, p$facet.raw, "sppid", "batz.plotdetections_first.last")
      for (k in seq_along(parts)) plots.expanded[[paste0(job.key, ".", k)]] <- parts[[k]]
    }
  }
  plots <- plots.expanded

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
      # here by converting any literal "/n" in $date.format to an
      # actual newline before it's used, rather than relying on the source
      # CSV always spelling it correctly.
      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)

      # 2026-08-27, per Josh ("plot.meta$xaxe.interval = 4 which should make
      # there only be four labeled dates on the X axes"): two real bugs here.
      # (1) $xaxe.interval was being read with get.default("xaxe.interval"),
      # never get.setting(p$job, "xaxe.interval") - so a plot's OWN
      # $xaxe.interval value (e.g. Josh's real plot.meta.csv row) was
      # silently ignored no matter what it said, always falling through to
      # aes.plotopts's generic value instead - the exact same
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
      # own original wording: "40kHzMyo if on species list should be \[on
      # the legend\] and colored black."
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
      ## File naming 2026-10-09, per Josh - see plotutil_figlist_filename():
      ## <plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<dates>_<TIMESTAMP>.png
      ## (faceted) or <plot.name>_<facet>_<plot.set>_<dates>_<TIMESTAMP>.png
      fname <- plotutil_figlist_filename(p$job$plot.name, p$facet.ids, p$facet.plot, trimws(p$job$plot.set),
                                     p$date.start, p$date.end, fn.name = "batz.plotdetections_first.last", project.name = project.name)
      ## save into dir.save (default getwd(), i.e. unchanged behavior for
      ## existing callers) rather than always the working directory - see
      ## Details/Follow-up
      fname <- file.path(dir.save, fname)

      dims <- plotutil_plot_dimensions(p$job, plot.inputs, job.label, "batz.plotdetections_first.last")   # $plot.dimensions (inches)
      ggplot2::ggsave(fname, plot = g,
                        width = if (!is.null(dims)) dims[1] else as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
                        height = if (!is.null(dims)) dims[2] else as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
                        units = if (!is.null(dims)) "in" else get.default("ggsave.units"),
                        dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

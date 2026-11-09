#' Plot an SM4Bat detector's operational-time heatmap
#'
#' Draws a night-by-time-of-night heatmap of how many minutes an SM4Bat
#' detector was recording in each interval of the night, from an
#' already-summarized per-interval table (\code{$mins.oper},
#' \code{$mins2.noon.mon}). Dawn, Dusk and Midnight reference lines are drawn
#' as in \code{\link{batz.plotdetections_first.last}}. The Y axis is one
#' shared Noon-to-Noon window; the X axis is the date. One single-panel PNG is
#' saved per \code{fig.list} row whose \code{$plot.type} is
#' \code{"sm4.heatmap"}.
#'
#' @param data Data frame of per-ARU, per-interval operational-time records.
#'   Required columns: \code{$aru.name}, \code{$date.monitoringnight},
#'   \code{$mins.oper} (minutes recorded in the interval),
#'   \code{$mins2.noon.mon} (minutes from that night's noon to the interval).
#'   A column named exactly \code{aru} is used as \code{$aru.name} when there
#'   is no \code{aru.name} column.
#' @param fig.list Data frame, one row per plot. Required columns:
#'   \code{$plot.type}, \code{$plot.name}, \code{$plot.set},
#'   \code{$date.format}, \code{$date.start}, \code{$date.end},
#'   \code{$xaxe.interval}, \code{$xaxe.title}, \code{$yaxe.title}. Other
#'   columns (species and facet columns of a shared fig.list) are ignored.
#'   See Details.
#'   Cells holding "NA" (a heading not used by this plot type, as in
#'   \code{\link{batz.generate_figlist}}) are treated as blank.
#'   Only rows whose \code{$plot} is \code{TRUE} or \code{1} are
#'   plotted; rows with \code{FALSE}, any other entry or a blank are
#'   skipped (NOTE). A fig.list with no \code{$plot} column has every
#'   row plotted (NOTE).
#' @param suntimes Optional, default \code{NULL}: no Dawn and Dusk lines are
#'   drawn (the Midnight line still is). Otherwise a data frame of sunrise
#'   and sunset times, e.g. the output of
#'   \code{\link{batz.generate_suntimes.arulist}}. Required columns:
#'   \code{$aru.name}, \code{$date}, \code{$date.monitoringnight},
#'   \code{$sunregion}, \code{$time.zone}, \code{$sunregion.type},
#'   \code{$schedule1}, \code{$schedule2}, \code{$sunset},
#'   \code{$sunset.unix}, \code{$sunrise}, \code{$sunrise.unix},
#'   \code{$sunrise.monitoringnight}, \code{$sunrise.monitoringnight.unix}
#'   (old spellings \code{schedual1} / \code{schedual2} are still accepted).
#'   Only \code{$aru.name}, \code{$date}, \code{$time.zone}, \code{$sunset}
#'   (Dusk) and \code{$sunrise.monitoringnight} (Dawn: the sunrise that ends
#'   the night starting on \code{$date}) are used for drawing.
#' @param aes.default Data frame of plot settings (plotopts), one row per
#'   parameter, e.g. \verb{plotopts_sm4heatmap.csv}. Required columns:
#'   \code{$category}, \code{$parameter}, \code{$default.value}. The custom
#'   style column (named by \code{aes.style}) and \code{$notes} are
#'   optional. See "plotopts" in Details.
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plotsm4_heatmap} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of each saved file name.
#' @param ... Plot settings typed into the call, named as the
#'   \code{aes.default} \code{$parameter} rows (or fig.list setting
#'   columns), e.g. \code{legend.position = "bottom"},
#'   \code{plot.width = 8}. A vector is joined with ";". Each setting is
#'   taken from the function call, then the fig.list row, then \code{aes.default} (its custom-style column, then
#'   \code{$default.value}). A setting given here does not need a row in
#'   \code{aes.default}. A NOTE lists the settings used and any name the
#'   function does not use.
#' @param aes.style Character, default \code{"override.value"}. Name of the
#'   \code{aes.default} column checked first for each parameter. When that
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
#'   \code{aes.default}. FALSE leaves them untouched (plot text keeps its
#'   symbols), but a cell that cannot be read in this R session stops the
#'   function with an error naming the table, the number of bad characters
#'   and the first rows per header. TRUE strips them instead.
#'
#' @return Invisibly, a list with \code{$plots} and \code{$ggplots}, keyed by
#'   the row number among the kept \code{fig.list} rows (\code{"1"},
#'   \code{"2"}, ...). Both are empty lists when no plot is made. Each
#'   \code{$plots} entry holds:
#'   \describe{
#'     \item{\code{$job.label}, \code{$job}}{Plot title (\code{$plot.name},
#'       or \code{"row N"}) and the \code{fig.list} row.}
#'     \item{\code{$pd}}{Plotted tile rows: the \code{data} columns plus
#'       \code{$date.parsed} and \code{$tile.time}.}
#'     \item{\code{$sdb}}{Suntimes rows used, plus \code{$date.parsed},
#'       \code{$dusk.time}, \code{$dawn.time}, \code{$midnight.time}, with
#'       one extra row copied at each end (half a day outside the date
#'       window) so the lines reach the panel edges.}
#'     \item{\code{$bin.minutes}}{Tile height in minutes (see Details).}
#'     \item{\code{$tz}}{Time zone used.}
#'     \item{\code{$y.start}, \code{$y.end}}{Y-axis window (Noon to Noon).}
#'     \item{\code{$date.start}, \code{$date.end}}{X-axis date window.}
#'     \item{\code{$plot.set.val}}{The \code{$plot.set} value.}
#'   }
#'   \code{$ggplots} holds the ggplot objects (empty when ggplot2 is not
#'   installed).
#'
#'   Files written: one PNG per plot in \code{dir.save}, named
#'   \verb{<project.name>_<plot.name>_<YYYYMMDD>to<YYYYMMDD>_<YYYYMMDDHHMMSS>.png}
#'   (\code{$date.start} to \code{$date.end}, then a time stamp). A
#'   \code{Saved:} line with the full path is printed per file.
#'
#' @details
#' \strong{fig.list columns.} Required headers are matched ignoring case and
#' "." / "_" / space differences.
#' \describe{
#'   \item{\code{$plot.type}}{Only \code{"sm4.heatmap"} rows are plotted;
#'     other values are skipped with a NOTE. Rows with a blank
#'     \code{$plot.type} are dropped. Exact duplicate rows are removed
#'     (NOTE).}
#'   \item{\code{$plot.name}}{Plot title and part of the file name. Blank
#'     gives \code{"row N"}.}
#'   \item{\code{$plot.set}}{One ARU. \code{data} and \code{suntimes} are
#'     filtered on \code{$aru.name} (ignoring case). Blank = no filter.}
#'   \item{\code{$date.start}, \code{$date.end}}{Date window of the plot
#'     (m/d/Y, Y-m-d or m/d/y). Both are needed: a blank or unreadable date
#'     leaves no rows. A row with no data left makes no plot (NOTE).}
#'   \item{\code{$date.format}}{X-axis date label format (strftime style).
#'     A literal "/n" becomes a line break.}
#'   \item{\code{$xaxe.interval}}{Number of evenly spaced labeled dates from
#'     \code{$date.start} to \code{$date.end}. An unusable value gives 2
#'     labels (NOTE).}
#'   \item{\code{$xaxe.title}, \code{$yaxe.title}}{Axis titles.}
#' }
#'
#' \strong{Tiles.} Each \code{data} row is one tile, placed at its
#' \code{$date.monitoringnight} (X) and \code{$mins2.noon.mon} (Y, on the
#' shared Noon-to-Noon axis) and filled by \code{$mins.oper}. The data is
#' not re-binned. The tile height is the most common gap between
#' consecutive \code{$mins2.noon.mon} values within each night (rounded to
#' 0.1 minute), or 15 minutes when it cannot be worked out. A partial first
#' or last interval is drawn at full height but with its lower
#' \code{$mins.oper} color. Tiles that cross the panel edge are clipped to
#' it.
#'
#' \strong{Reference lines.} Dusk is \code{$sunset} and Dawn is
#' \code{$sunrise.monitoringnight}, both placed on the shared axis; Midnight
#' is a straight line at 00:00. The lines run across the suntimes dates and
#' are extended flat to the panel edges. The time zone is the first
#' \code{$time.zone} of the matched suntimes rows (UTC when there are
#' none). No matching suntimes rows only gives a NOTE (no lines).
#'
#' \strong{plotopts (aes.default).} Each setting is read from the first row
#' whose \code{$parameter} matches the name (ignoring case and
#' spaces). The value is the \code{aes.style} column when that cell is not
#' blank, otherwise \code{$default.value}. Only the fig.list columns listed
#' above are read from \code{fig.list}; every other setting comes from
#' plotopts. These \code{$parameter} rows must exist (case and spaces ignored), or
#' the function stops listing the missing ones:
#' \code{yaxe.break.interval}, \code{yaxe.labelformat},
#' \code{yaxe.break.labels}, \code{dawn.linetype}, \code{dawn.color},
#' \code{dusk.linetype}, \code{dusk.color}, \code{midnight.linetype},
#' \code{midnight.color}, \code{panel.border.linewidth},
#' \code{reference.line.legend.title}, \code{fill.max}, \code{fill.colors},
#' \code{fill.legend.title}, \code{tile.color}, \code{tile.linewidth},
#' \code{plot.title.size}, \code{plot.title.hjust}, \code{axis.title.size},
#' \code{axis.text.size}, \code{legend.text.size},
#' \code{legend.title.size}, \code{legend.position},
#' \code{panel.spacing.x}, \code{ggsave.dpi}, \code{ggsave.units},
#' \code{ggsave.width.pad}, \code{ggsave.height.pad}, \code{plot.width},
#' \code{plot.height}.
#' \itemize{
#'   \item Y breaks every \code{yaxe.break.interval} (e.g.
#'     \code{"3 hours"}), labeled with the ";"-separated
#'     \code{yaxe.break.labels}. If the counts differ, times formatted with
#'     \code{yaxe.labelformat} are used (NOTE).
#'   \item Fill: a continuous gradient through the ";"-separated
#'     \code{fill.colors} (fewer than 2 colors gives a 7-color viridis
#'     ramp), from 0 to \code{fill.max} (an unusable value gives 15).
#'     Values above \code{fill.max} are out of range and drawn grey.
#'   \item The colorbar runs left to right when \code{legend.position} is
#'     \code{"bottom"}, otherwise bottom to top.
#'   \item The Midnight line width is half of \code{panel.border.linewidth},
#'     so it looks as thick as the panel border.
#'   \item The PNG size is \code{plot.width} + \code{ggsave.width.pad} by
#'     \code{plot.height} + \code{ggsave.height.pad}, in
#'     \code{ggsave.units}, at \code{ggsave.dpi}.
#' }
#'
#' \strong{Input checks.} Special characters are stripped or checked
#' (\code{strip.special}, \code{strip.special.plotopts}). Required headers
#' of all four inputs are matched and renamed to the expected spelling.
#' Missing headers, missing plotopts rows and duplicate column names are
#' reported together in one error. If ggplot2 is not installed, only the
#' prepared data is returned.
#'
#' @seealso \code{\link{batz.plotdetections_first.last}},
#'   \code{\link{batz.generate_suntimes.arulist}},
#'   \code{\link{batz.generate_plotopts}}
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
batz.plotsm4_heatmap <- function(data, fig.list, suntimes = NULL,
                                  aes.default = NULL, project.name = "new.project",
                                  aes.style = "override.value",
                                  dir.save = getwd(),
                                  strip.special = TRUE,
                                  strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(plotutil_dots(...), "batz.plotsm4_heatmap")

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
  fig.list <- plotutil_figlist_na(fig.list)       # "NA" = heading not used by this plot type
  fig.list <- plotutil_figlist_plot(fig.list, "batz.plotsm4_heatmap")   # only rows with $plot = TRUE or 1
  if (is.data.frame(fig.list) && nrow(fig.list) == 0) return(invisible(list(plots = list(), ggplots = list())))
  if (is.null(aes.default)) aes.default <- plotutil_default_plotopts("batz.plotsm4_heatmap")   # built-in settings
  data <- special.strip(data, strip.special)$df
  ## fig.list is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    fig.list <- special.strip(fig.list, TRUE)$df
  } else {
    special.check.plotopts(fig.list, "fig.list", objname.fig.list, "batz.plotsm4_heatmap")
  }
  has.sun <- !is.null(suntimes)                  # no suntimes: no Dawn / Dusk lines
  if (has.sun) suntimes <- special.strip(suntimes, strip.special)$df
  ## aes.default is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.default <- special.strip(aes.default, TRUE)$df
  } else {
    special.check.plotopts(aes.default, "aes.default", objname.aes.default, "batz.plotsm4_heatmap")
  }

  ## $aru -> $aru.name compatibility shim - see @details. Runs BEFORE the
  ## required-header check, since canonicalize.headers() only reconciles
  ## casing/separator style, never a genuinely different column name.
  if ("aru" %in% names(data) && !("aru.name" %in% names(data))) {
    names(data)[names(data) == "aru"] <- "aru.name"
  }

  PLOT.TYPE <- "sm4.heatmap"

  DATA.REQUIRED <- c("aru.name", "date.monitoringnight", "mins.oper", "mins2.noon.mon")
  ## Round twenty-six, 2026-09-25 (per Josh's package-wide suntimes header
  ## rename): $suns/$suns.unix/$sunr/$sunr.unix/$sunr.mon/$sunr.mon.unix are
  ## now $sunset/$sunset.unix/$sunrise/$sunrise.unix/
  ## $sunrise.monitoringnight/$sunrise.monitoringnight.unix - NOT
  ## backward-compatible - see @details.
  SUNTIMES.REQUIRED <- c("aru.name", "date", "date.monitoringnight", "sunregion", "time.zone",
                          "sunregion.type", "schedule1", "schedule2", "sunset",
                          "sunset.unix", "sunrise", "sunrise.unix", "sunrise.monitoringnight",
                          "sunrise.monitoringnight.unix")
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
  suntimes.canon    <- if (has.sun) canonicalize.headers(suntimes, SUNTIMES.REQUIRED) else list(df = NULL, missing = character(0))
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

  problems <- c(
    missing.msg(data.canon, "data"),
    missing.msg(suntimes.canon, "suntimes"),
    missing.msg(fig.list.canon, "fig.list"),
    missing.msg(aes.default.canon, "aes.default"),
    check.parameters(aes.default.canon$df, plotutil_required_left(AES.DEFAULT.REQUIRED.PARAMETERS, plot.inputs), "aes.default"),
    check.duplicates(data, "data"),
    if (has.sun) check.duplicates(suntimes, "suntimes"),
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
    if (!is.null(v.in <- plotutil_input_get(plot.inputs, param))) return(v.in)   # function call first
    row.idx <- which(plotutil_param_key(aes.default$parameter) == plotutil_param_key(param))
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (!is.na(aes.col <- plotutil_aes_style_col(aes.default, aes.style))) {
      override <- aes.default[[aes.col]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.default$parameter, names(fig.list)), "batz.plotsm4_heatmap")

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

    if (!has.sun) {                               # Midnight line only
      tz <- "UTC"
      y.ref.noon <- as.POSIXct(paste(y.ref.date, "12:00:00"), tz = tz)
      sdb <- data.frame(date.parsed = c(date.start - 0.5, date.end + 0.5))
      sdb$midnight.time <- y.ref.noon + rep(12 * 3600, nrow(sdb))
    } else {
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
    }

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

    # Round twenty-six, 2026-09-25 (per Josh's package-wide suntimes header
    # rename): $suns/$sunr.mon are now $sunset/$sunrise.monitoringnight -
    # see @details, "suntimes's $suns/$sunr-family columns renamed".
    if (has.sun) {
    dusk.real <- parse.flex.datetime(sdb$sunset, tz)
    # Dawn ending THIS monitoring night is $sunrise.monitoringnight (sunrise
    # the FOLLOWING day), not $sunrise (sunrise ON $date, ending the
    # PREVIOUS night) - same reasoning as
    # batz.plotdetections_first.last()'s own BUGFIX for this.
    dawn.real <- parse.flex.datetime(sdb$sunrise.monitoringnight, tz)
    local.noon <- as.POSIXct(paste(format(sdb$date.parsed), rep("12:00:00", nrow(sdb))), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + rep(12 * 3600, nrow(sdb))
    }

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
    if (has.sun && nrow(sdb) > 0) {
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

    cat(sprintf("Prepared plot data for '%s': %d tile(s), bin width %g minute(s), %s.\n",
                 job.label, nrow(pd), bin.minutes,
                 if (has.sun) sprintf("%d suntimes row(s)", nrow(sdb)) else "no suntimes (no Dawn / Dusk lines)"))
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
        (if (has.sun) ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dusk.time, color = "Dusk"),
                             linetype = get.default("dusk.linetype"), inherit.aes = FALSE)) +
        ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                             linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw,
                             inherit.aes = FALSE) +
        (if (has.sun) ggplot2::geom_line(data = p$sdb, ggplot2::aes(x = date.parsed, y = dawn.time, color = "Dawn"),
                             linetype = get.default("dawn.linetype"), inherit.aes = FALSE)) +
        ggplot2::scale_color_manual(name = get.default("reference.line.legend.title"),
                                      breaks = if (has.sun) c("Dawn", "Dusk", "Midnight") else "Midnight",
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

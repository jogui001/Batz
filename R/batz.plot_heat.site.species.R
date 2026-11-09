#' Heat map of one species' passes per site by time of night
#'
#' Draws a site-by-time-of-night heat map for one bat species (or several
#' pooled): one column per site (\code{$aru.name}), one row per
#' \code{bin.minutes} interval of the night (\code{time.start} at the
#' bottom, \code{time.end} at the top), each cell colored by a class of
#' passes per monitoring night (e.g. 0, 1-10, 11-50, 51-100, 101-150 and
#' over 150). Sites can be grouped into labelled blocks (e.g. communities),
#' selected sites dropped, and selected sites shown in bold. One PNG is
#' saved per \code{fig.list} row whose \code{$plot.type} is
#' \code{"heat.site.species"}.
#'
#' @param data Data frame of vetted detections, one row per recording
#'   (pass). Required columns: \code{$aru.name}, the species column named by
#'   \code{spp.id}, the monitoring-night date column named by
#'   \code{groupby.date}, and the clock-time column named by
#'   \code{groupby.time}. Headers are matched ignoring case and "." / "_" /
#'   space differences (e.g. \code{ARU Name}, \code{DATE-12}). A column
#'   named in a fig.list row's \code{$plot.group} must also exist, or that
#'   row is skipped (NOTE).
#' @param fig.list Data frame, one row per plot. Required columns:
#'   \code{$plot.type}, \code{$plot.name}, \code{$plot.species},
#'   \code{$plot.set}, \code{$date.start}, \code{$date.end},
#'   \code{$xaxe.title}, \code{$yaxe.title}. Optional:
#'   \code{$plot.group}, \code{$group.labels}, \code{$site.exclude},
#'   \code{$site.bold}. A row can also override any \code{aes.default}
#'   parameter with a column of the same name (e.g. \code{$bin.minutes},
#'   \code{$time.start}). See Details.
#'   Cells holding "NA" (a heading not used by this plot type, as in
#'   \code{\link{batz.generate_figlist}}) are treated as blank.
#'   Only rows whose \code{$plot} is \code{TRUE} or \code{1} are
#'   plotted; rows with \code{FALSE}, any other entry or a blank are
#'   skipped (NOTE). A fig.list with no \code{$plot} column has every
#'   row plotted (NOTE).
#' @param aes.default Data frame of plot settings (plotopts), normally
#'   \verb{plotopts_heat.site.species.csv}. Required columns:
#'   \code{$category}, \code{$parameter}, \code{$default.value}. The custom
#'   style column (named by \code{aes.style}) and \code{$notes} are
#'   optional. See "plotopts" in Details.
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plot_heat.site.species} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#' @param spp.id Character, default \code{"species"}. Name of the species
#'   column in \code{data}.
#' @param groupby.date Character, default \code{"date.monitoringnight"}.
#'   Name of the monitoring-night date column (read as m/d/Y, Y-m-d, m/d/y
#'   or Y/m/d). For a raw Kaleidoscope-style export use \code{"date-12"}.
#' @param groupby.time Character, default \code{"time"}. Name of the
#'   clock-time column. The first \code{HH:MM} found is used, so a full
#'   date-time such as \code{"2023-05-06 21:14:08"} also works. Use the real
#'   recording time, not \code{TIME-12}.
#' @param duplicates.remove Logical, default \code{TRUE}. TRUE drops exact
#'   duplicate rows of \code{data} before counting (NOTE with the number
#'   dropped).
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
#'   read when there is no \code{$override.value} column. A non-blank
#'   fig.list value of the same name wins over both.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in (created if needed).
#' @param strip.special Logical, default \code{TRUE}. TRUE simplifies
#'   accented letters and removes other non-ASCII characters in \code{data}
#'   (local copy only) before matching. FALSE leaves them as they are.
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same
#'   clean-up for \code{fig.list} and \code{aes.default}. FALSE keeps their
#'   text, but a cell that cannot be read in this R session stops the
#'   function with an error. TRUE strips them instead.
#'
#' @return Invisibly, a list with \code{$plots} and \code{$ggplots}, one
#'   element per plot made, named by \code{$plot.name} (\code{"rowN"} when
#'   blank). Each \code{$plots} element holds:
#'   \describe{
#'     \item{\code{$cells}}{One row per site x time bin: \code{$group},
#'       \code{$aru.name}, \code{$bin} (0 = first bin), \code{$bin.start},
#'       \code{$bin.end} (HH:MM), \code{$passes}, \code{$nights},
#'       \code{$value}, \code{$value.class} (factor of class labels).}
#'     \item{\code{$sites}}{One row per site, in plot order: \code{$group},
#'       \code{$aru.name}, \code{$nights}, \code{$passes}.}
#'     \item{\code{$settings}}{List: \code{$time.start}, \code{$time.end},
#'       \code{$bin.minutes}, \code{$n.bins}, \code{$value.type},
#'       \code{$bin.breaks}, \code{$bin.colors}, \code{$plot.group},
#'       \code{$species}, \code{$dates} (first and last night).}
#'     \item{\code{$file}}{Saved PNG name (\code{NA} when no PNG was saved).}
#'   }
#'   \code{$ggplots} holds the ggplot objects (empty when ggplot2 is not
#'   installed). Files written: one PNG per plot in \code{dir.save}, named
#'   \verb{<project.name>_<plot.name>_<YYYYMMDD>to<YYYYMMDD>_<YYYYMMDDHHMMSS>.png}
#'   (first and last monitoring night in the selected data, then a time
#'   stamp). A \code{Saved:} line is printed per file.
#'
#' @details
#' \strong{fig.list columns.} List cells are separated by ";" or ",".
#' \describe{
#'   \item{\code{$plot.type}}{Must be \code{"heat.site.species"}; other rows
#'     are skipped with a NOTE, so one shared fig.list can hold rows for
#'     several plot functions. Rows with a blank \code{$plot.type} are
#'     dropped, and exact duplicate rows are removed.}
#'   \item{\code{$plot.name}}{Used in the file name and as the list name.}
#'   \item{\code{$plot.species}}{Species to plot, matched to
#'     \code{data[[spp.id]]} ignoring case and outer spaces. Several species
#'     are pooled into one plot; the title joins them with " + ". Blank
#'     stops with an error; species not found give a NOTE.}
#'   \item{\code{$plot.group}}{Optional. Column of \code{data} that groups
#'     sites into blocks (e.g. \code{"community"}). Blank or absent = one
#'     block, no headings. A site with more than one value uses the first
#'     one (NOTE).}
#'   \item{\code{$plot.set}}{Sites to include: values of \code{$plot.group}
#'     when it is given (e.g. \code{"OLMN;KA"}), otherwise \code{$aru.name}
#'     values. Blocks are drawn in \code{$plot.set} order. Values not found
#'     give a NOTE. Blank = every site (blocks sorted).}
#'   \item{\code{$group.labels}}{Optional. Block headings as
#'     \code{"value=label"} pairs, e.g. \code{"OLMN=Kajiado;KA=NKRCP"}. A
#'     block without a label uses its own value.}
#'   \item{\code{$site.exclude}}{Optional. Sites (\code{$aru.name}) to leave
#'     out.}
#'   \item{\code{$site.bold}}{Optional. Sites whose X-axis name is drawn in
#'     \code{site.bold.face}.}
#'   \item{\code{$date.start}, \code{$date.end}}{Monitoring nights to
#'     include (m/d/Y, Y-m-d, m/d/y or Y/m/d). Blank = no limit on that
#'     side.}
#'   \item{\code{$xaxe.title}, \code{$yaxe.title}}{Axis titles.}
#' }
#' A row that matches no records makes no plot (NOTE).
#'
#' \strong{Records.} Records with a blank \code{$aru.name} or an unreadable
#' \code{groupby.date} are dropped (NOTE). Records with no readable time
#' count toward monitoring nights but are not plotted (NOTE).
#'
#' \strong{Time bins.} Each record's clock time is placed in a
#' \code{bin.minutes} bin counted from \code{time.start}. The window runs to
#' \code{time.end} and may cross midnight; equal start and end give 24
#' hours. Passes outside the window are not plotted (NOTE) but their
#' records still count toward monitoring nights. The Y axis is labelled
#' every \code{yaxe.break.hours}.
#'
#' \strong{Cell values.} With \code{value.type = "mean.night"} a cell is
#' the site's passes in that bin divided by the site's monitoring nights:
#' the number of unique \code{groupby.date} values for that site among all
#' records (any species) inside the date window. A night when a detector
#' ran but saved no files is not in the data, so it is not counted.
#' \code{value.type = "total"} plots raw counts. Other values stop with an
#' error. Every site x bin cell is drawn, so a bin with no passes shows as
#' class 0.
#'
#' \strong{Color classes.} \code{bin.breaks} (e.g. \code{10;50;100;150})
#' are the upper edges of the classes after 0: 0, (0,10], (10,50],
#' (50,100], (100,150], >150, labelled 0, 1-10, 11-50, 51-100, 101-150,
#' \code{>150}. A fractional mean (e.g. 0.33) falls in 1-10. Every class is
#' shown in the legend even when no cell uses it. \code{bin.colors} needs
#' one color per class (number of breaks + 2); otherwise a NOTE is printed
#' and white + viridis colors are used. \code{legend.title} may contain
#' \code{<bin.minutes>}, which is replaced by the bin length.
#'
#' \strong{Site order.} Within each block, sites are sorted by the text
#' before their number, then by the number (OLMN3, OLMN7, OLMN13, then
#' KA01, KA02). Blocks are separated by a line (\code{group.divider.color},
#' \code{group.divider.linewidth}).
#'
#' \strong{plotopts (aes.default).} These \code{$parameter} rows must exist,
#' or the function stops listing the missing ones: \code{time.start},
#' \code{time.end} (HH:MM), \code{bin.minutes}, \code{yaxe.break.hours},
#' \code{value.type}, \code{bin.breaks}, \code{bin.colors},
#' \code{legend.title}, \code{legend.position}, \code{legend.nrow},
#' \code{tile.color}, \code{tile.linewidth}, \code{site.bold.face} (bold,
#' italic, bold.italic or plain; others give bold),
#' \code{xaxe.text.angle}, \code{group.divider.color},
#' \code{group.divider.linewidth}, \code{strip.text.size},
#' \code{plot.title.size}, \code{plot.title.face}, \code{plot.title.hjust},
#' \code{axis.title.size}, \code{axis.text.size}, \code{legend.text.size},
#' \code{legend.title.size}, \code{ggsave.dpi}, \code{ggsave.units},
#' \code{ggsave.width.pad}, \code{ggsave.height.pad}, \code{plot.width},
#' \code{plot.height}. A numeric setting that is not a number stops with an
#' error. The PNG size is \code{plot.width} + \code{ggsave.width.pad} by
#' \code{plot.height} + \code{ggsave.height.pad}, in \code{ggsave.units},
#' at \code{ggsave.dpi}, on a white background.
#'
#' If ggplot2 is not installed, only the prepared data is returned.
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
#' @seealso \code{\link{batz.plot_bar.bat.group}},
#'   \code{\link{batz.generate_plotopts}}
#'
#' @examples
#' \dontrun{
#' vetted <- read.csv("South_North_combined_vetted_1.csv", check.names = FALSE)
#' fig.list <- read.csv("fig.list_heat.site.species.csv", check.names = FALSE)
#' plotopts <- read.csv("plotopts_heat.site.species.csv", check.names = FALSE)
#' res <- batz.plot_heat.site.species(vetted, fig.list, plotopts,
#'                                    groupby.date = "date-12",
#'                                    project.name = "south.north")
#' res$ggplots[[1]]
#' head(res$plots[[1]]$cells)
#' }
#'
#' @export
batz.plot_heat.site.species <- function(data, fig.list, aes.default = NULL,
                                        spp.id = "species",
                                        groupby.date = "date.monitoringnight",
                                        groupby.time = "time",
                                        duplicates.remove = TRUE,
                                        project.name = "new.project",
                                        aes.style = "override.value",
                                        dir.save = getwd(),
                                        strip.special = TRUE,
                                        strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(plotutil_dots(...), "batz.plot_heat.site.species")

  FN <- "batz.plot_heat.site.species"
  PLOT.TYPE <- "heat.site.species"

  ## ---- special characters ----------------------------------------------
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
  fig.list <- plotutil_figlist_na(fig.list)       # "NA" = heading not used by this plot type
  fig.list <- plotutil_figlist_plot(fig.list, "batz.plot_heat.site.species")   # only rows with $plot = TRUE or 1
  if (is.data.frame(fig.list) && nrow(fig.list) == 0) return(invisible(list(plots = list(), ggplots = list())))
  if (is.null(aes.default)) aes.default <- plotutil_default_plotopts("batz.plot_heat.site.species")   # built-in settings
  if (!is.data.frame(data)) stop(FN, ": `data` must be a data frame.")
  if (!is.data.frame(fig.list)) stop(FN, ": `fig.list` must be a data frame.")
  if (!is.data.frame(aes.default)) stop(FN, ": `aes.default` must be a data frame.")
  data <- special.strip(data, strip.special)$df
  if (isTRUE(strip.special.plotopts)) {
    fig.list <- special.strip(fig.list, TRUE)$df
    aes.default <- special.strip(aes.default, TRUE)$df
  } else {
    special.check.plotopts(fig.list, "fig.list", objname.fig.list, FN)
    special.check.plotopts(aes.default, "aes.default", objname.aes.default, FN)
  }

  ## ---- header checks -----------------------------------------------------
  DATA.REQUIRED <- unique(c("aru.name", spp.id, groupby.date, groupby.time))
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "plot.species", "plot.set",
                         "date.start", "date.end", "xaxe.title", "yaxe.title")
  FIG.LIST.OPTIONAL <- c("plot.group", "group.labels", "site.exclude", "site.bold", "plot.dimensions")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "time.start", "time.end", "bin.minutes", "yaxe.break.hours",
    "value.type", "bin.breaks", "bin.colors",
    "legend.title", "legend.position", "legend.nrow",
    "tile.color", "tile.linewidth", "site.bold.face", "xaxe.text.angle",
    "group.divider.color", "group.divider.linewidth", "strip.text.size",
    "plot.title.size", "plot.title.face", "plot.title.hjust",
    "axis.title.size", "axis.text.size", "legend.text.size", "legend.title.size",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "plot.width", "plot.height")

  data.canon <- canonicalize.headers(data, DATA.REQUIRED)
  fig.canon <- canonicalize.headers(fig.list, c(FIG.LIST.REQUIRED, FIG.LIST.OPTIONAL))
  fig.canon$missing <- setdiff(fig.canon$missing, FIG.LIST.OPTIONAL)
  aes.canon <- canonicalize.headers(aes.default, AES.DEFAULT.REQUIRED)

  missing.msg <- function(canon, label) {
    if (length(canon$missing) > 0) sprintf("%s is missing these headers: %s", label,
                                            paste(canon$missing, collapse = ", "))
  }
  problems <- c(missing.msg(data.canon, "data"),
                missing.msg(fig.canon, "fig.list"),
                missing.msg(aes.canon, "aes.default"))
  if ("parameter" %in% names(aes.canon$df)) {
    req.par <- plotutil_required_left(AES.DEFAULT.REQUIRED.PARAMETERS, plot.inputs)
    miss.par <- req.par[!plotutil_param_key(req.par) %in% plotutil_param_key(aes.canon$df$parameter)]
    if (length(miss.par) > 0) {
      problems <- c(problems, sprintf(
        "aes.default is missing these required $parameter rows: %s - it may be an older copy of plotopts_heat.site.species.csv",
        paste(miss.par, collapse = ", ")))
    }
  }
  if (length(problems) > 0) stop(paste(problems, collapse = "\n\n"), call. = FALSE)
  data <- data.canon$df
  fig.list <- fig.canon$df
  aes.default <- aes.canon$df

  if (isTRUE(duplicates.remove)) {
    dup <- duplicated(data)
    if (any(dup)) {
      cat(sprintf("NOTE: %s - %d exact duplicate row(s) removed from data.\n", FN, sum(dup)))
      data <- data[!dup, , drop = FALSE]
    }
  }

  ## ---- small helpers -------------------------------------------------------
  blank <- function(v) is.null(v) || length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v)))
  split.list <- function(v) {
    if (blank(v)) return(character(0))
    x <- trimws(gsub('"', "", strsplit(as.character(v), "[;,]")[[1]]))
    x[nzchar(x)]
  }
  key <- function(v) tolower(trimws(as.character(v)))
  get.default <- function(param) {
    if (!is.null(v.in <- plotutil_input_get(plot.inputs, param))) return(v.in)   # function call first
    i <- which(plotutil_param_key(aes.default$parameter) == plotutil_param_key(param))[1]
    if (is.na(i)) return(NA_character_)
    val <- as.character(aes.default$default.value[i])
    if (!is.na(aes.col <- plotutil_aes_style_col(aes.default, aes.style))) {
      ov <- aes.default[[aes.col]][i]
      if (!blank(ov)) val <- as.character(ov)
    }
    if (is.na(val)) "" else val
  }
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.default$parameter, names(fig.list), "plot.dimensions", "plot.dimentions"), "batz.plot_heat.site.species")
  get.setting <- function(job, param) {
    if (!is.null(v.in <- plotutil_input_get(plot.inputs, param))) return(v.in)   # function call, then fig.list, then plotopts
    if (param %in% names(job) && !blank(job[[param]])) return(as.character(job[[param]]))
    get.default(param)
  }
  num.setting <- function(job, param) {
    v <- suppressWarnings(as.numeric(get.setting(job, param)))
    if (length(v) == 0 || is.na(v)) stop(sprintf("%s: setting '%s' = '%s' is not a number.",
                                                 FN, param, get.setting(job, param)), call. = FALSE)
    v
  }
  parse.flex.date <- function(x) {
    x <- as.character(x)
    out <- as.Date(rep(NA_character_, length(x)))
    for (fmt in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y", "%Y/%m/%d")) {
      todo <- is.na(out) & !is.na(x) & nzchar(trimws(x))
      if (!any(todo)) break
      out[todo] <- as.Date(x[todo], format = fmt)
    }
    out
  }
  hhmm.to.min <- function(x, label) {
    m <- regmatches(x, regexpr("[0-9]{1,2}:[0-9]{2}", x))
    if (length(m) != 1) stop(sprintf("%s: %s = '%s' is not a HH:MM time.", FN, label, x), call. = FALSE)
    p <- as.integer(strsplit(m, ":")[[1]])
    p[1] * 60 + p[2]
  }
  ## minute of day from a time / date-time value (first HH:MM found)
  clock.minutes <- function(x) {
    x <- as.character(x)
    m <- regmatches(x, regexpr("[0-9]{1,2}:[0-9]{2}", x), invert = FALSE)
    out <- rep(NA_real_, length(x))
    hit <- regexpr("[0-9]{1,2}:[0-9]{2}", x) > 0 & !is.na(x)
    p <- strsplit(m, ":")
    out[hit] <- vapply(p, function(z) as.numeric(z[1]) * 60 + as.numeric(z[2]), 0)
    out[!is.na(out) & out >= 24 * 60] <- NA
    out
  }
  min.to.hhmm <- function(m) sprintf("%02d:%02d", (m %/% 60) %% 24, round(m %% 60))
  nat.order <- function(s) {
    pre <- sub("[0-9]+.*$", "", s)
    n <- suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", s)))
    n[!grepl("[0-9]", s)] <- NA
    order(tolower(pre), is.na(n), n, s)
  }

  ## ---- working copy of data -------------------------------------------------
  d <- data.frame(aru.name = trimws(as.character(data$aru.name)),
                  species  = trimws(as.character(data[[spp.id]])),
                  night    = parse.flex.date(data[[groupby.date]]),
                  minute   = clock.minutes(data[[groupby.time]]),
                  stringsAsFactors = FALSE, check.names = FALSE)
  bad <- is.na(d$aru.name) | !nzchar(d$aru.name) | is.na(d$night)
  if (any(bad)) {
    cat(sprintf("NOTE: %s - %d record(s) dropped: blank aru.name or unreadable %s.\n",
                FN, sum(bad), groupby.date))
  }
  no.time <- is.na(d$minute) & !bad
  if (any(no.time)) {
    cat(sprintf("NOTE: %s - %d record(s) have no readable time in '%s'; they count toward monitoring nights but are not plotted.\n",
                FN, sum(no.time), groupby.time))
  }
  keep.rows <- !bad
  d <- d[keep.rows, , drop = FALSE]
  data <- data[keep.rows, , drop = FALSE]

  ## ---- jobs ------------------------------------------------------------------
  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  jobs <- jobs[!duplicated(jobs), , drop = FALSE]
  if (nrow(jobs) == 0) stop(FN, ": fig.list has no plot rows (every $plot.type is blank).", call. = FALSE)

  have.gg <- requireNamespace("ggplot2", quietly = TRUE)
  if (!have.gg) cat("ggplot2 is not installed - returning prepared data only, no PNG produced.\n")
  if (have.gg && !dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)

  plots <- list(); ggplots <- list()

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, , drop = FALSE]
    job.label <- if (blank(job$plot.name)) sprintf("row%d", j) else trimws(job$plot.name)

    if (!identical(key(job$plot.type), PLOT.TYPE)) {
      cat(sprintf("NOTE: fig.list row '%s' has plot.type = '%s' - skipped.\n", job.label, job$plot.type))
      next
    }

    ## settings
    t.start <- hhmm.to.min(get.setting(job, "time.start"), "time.start")
    t.end   <- hhmm.to.min(get.setting(job, "time.end"), "time.end")
    win <- (t.end - t.start) %% 1440
    if (win == 0) win <- 1440
    bin.minutes <- num.setting(job, "bin.minutes")
    if (bin.minutes <= 0) stop(FN, ": bin.minutes must be > 0.", call. = FALSE)
    n.bins <- ceiling(win / bin.minutes)
    value.type <- key(get.setting(job, "value.type"))
    if (!value.type %in% c("mean.night", "total")) {
      stop(sprintf("%s: value.type = '%s' - use 'mean.night' or 'total'.", FN, value.type), call. = FALSE)
    }

    ## group column
    group.col <- if ("plot.group" %in% names(job) && !blank(job$plot.group)) trimws(job$plot.group) else ""
    if (nzchar(group.col)) {
      gc <- canonicalize.headers(data, group.col)
      if (length(gc$missing) > 0) {
        cat(sprintf("NOTE: fig.list row '%s' - $plot.group '%s' is not a column of data - skipped.\n",
                    job.label, group.col))
        next
      }
      grp <- trimws(as.character(gc$df[[group.col]]))
    } else {
      grp <- rep("", nrow(d))
    }

    ## date window
    ds <- parse.flex.date(get.setting(job, "date.start"))
    de <- parse.flex.date(get.setting(job, "date.end"))
    in.dates <- (is.na(ds) | d$night >= ds) & (is.na(de) | d$night <= de)

    ## site selection
    sel.set <- split.list(job$plot.set)
    if (length(sel.set) > 0) {
      if (nzchar(group.col)) in.set <- key(grp) %in% key(sel.set) else in.set <- key(d$aru.name) %in% key(sel.set)
      unknown <- sel.set[!key(sel.set) %in% key(if (nzchar(group.col)) grp else d$aru.name)]
      if (length(unknown) > 0) cat(sprintf("NOTE: fig.list row '%s' - $plot.set value(s) not found in data: %s\n",
                                           job.label, paste(unknown, collapse = ", ")))
    } else {
      in.set <- rep(TRUE, nrow(d))
    }
    excl <- split.list(if ("site.exclude" %in% names(job)) job$site.exclude else "")
    if (length(excl) > 0) {
      unknown <- excl[!key(excl) %in% key(d$aru.name)]
      if (length(unknown) > 0) cat(sprintf("NOTE: fig.list row '%s' - $site.exclude site(s) not found in data: %s\n",
                                           job.label, paste(unknown, collapse = ", ")))
    }
    use <- in.dates & in.set & !(key(d$aru.name) %in% key(excl))
    if (!any(use)) {
      cat(sprintf("NOTE: fig.list row '%s' matched 0 records of data - no plot made.\n", job.label))
      next
    }
    dj <- d[use, , drop = FALSE]
    dj$group <- grp[use]

    ## a site belongs to one group (first one seen if it is in several)
    site.tab <- dj[!duplicated(dj$aru.name), c("group", "aru.name"), drop = FALSE]
    multi <- tapply(dj$group, dj$aru.name, function(g) length(unique(g)))
    if (any(multi > 1)) {
      cat(sprintf("NOTE: fig.list row '%s' - site(s) with more than one $%s value, first one used: %s\n",
                  job.label, group.col, paste(names(multi)[multi > 1], collapse = ", ")))
    }
    dj$group <- site.tab$group[match(dj$aru.name, site.tab$aru.name)]

    ## group order: $plot.set order, then any others alphabetically
    groups <- unique(site.tab$group)
    if (nzchar(group.col) && length(sel.set) > 0) {
      ord <- match(key(groups), key(sel.set))
      groups <- groups[order(is.na(ord), ord, groups)]
    } else {
      groups <- sort(groups)
    }
    site.tab$group <- factor(site.tab$group, levels = groups)
    site.tab <- site.tab[order(site.tab$group), , drop = FALSE]
    site.tab <- do.call(rbind, lapply(split(site.tab, site.tab$group, drop = TRUE),
                                      function(s) s[nat.order(s$aru.name), , drop = FALSE]))
    sites <- as.character(site.tab$aru.name)

    ## effort: monitoring nights per site (all species)
    nights <- tapply(dj$night, dj$aru.name, function(x) length(unique(x)))

    ## passes of the species, binned
    spp <- split.list(job$plot.species)
    if (length(spp) == 0) stop(sprintf("%s: fig.list row '%s' has a blank $plot.species.", FN, job.label), call. = FALSE)
    is.spp <- key(dj$species) %in% key(spp)
    miss.spp <- spp[!key(spp) %in% key(dj$species)]
    if (length(miss.spp) > 0) cat(sprintf("NOTE: fig.list row '%s' - species not found in the selected data: %s\n",
                                          job.label, paste(miss.spp, collapse = ", ")))
    rel <- (dj$minute - t.start) %% 1440
    in.win <- is.spp & !is.na(rel) & rel < win
    out.win <- sum(is.spp & !is.na(rel) & rel >= win)
    if (out.win > 0) cat(sprintf("NOTE: '%s' - %d pass(es) outside %s-%s not plotted.\n", job.label, out.win,
                                 min.to.hhmm(t.start), min.to.hhmm(t.end)))
    bin <- floor(rel[in.win] / bin.minutes)

    cells <- expand.grid(bin = seq_len(n.bins) - 1L, aru.name = sites, stringsAsFactors = FALSE)
    cnt <- table(factor(dj$aru.name[in.win], levels = sites), factor(bin, levels = seq_len(n.bins) - 1L))
    cells$passes <- as.integer(cnt[cbind(match(cells$aru.name, sites), cells$bin + 1L)])
    cells$nights <- as.integer(nights[cells$aru.name])
    cells$value <- if (value.type == "mean.night") cells$passes / cells$nights else as.numeric(cells$passes)
    cells$group <- as.character(site.tab$group[match(cells$aru.name, sites)])
    cells$bin.start <- min.to.hhmm(t.start + cells$bin * bin.minutes)
    cells$bin.end <- min.to.hhmm(t.start + pmin((cells$bin + 1) * bin.minutes, win))

    ## color classes
    brk <- sort(unique(suppressWarnings(as.numeric(split.list(get.setting(job, "bin.breaks"))))))
    brk <- brk[!is.na(brk) & brk > 0]
    if (length(brk) == 0) stop(FN, ": bin.breaks has no usable positive numbers.", call. = FALSE)
    lo <- c(1, head(brk, -1) + 1)
    class.labels <- c("0", sprintf("%s-%s", format(lo, trim = TRUE), format(brk, trim = TRUE)),
                      sprintf(">%s", format(max(brk), trim = TRUE)))
    cells$value.class <- factor(class.labels[findInterval(cells$value, c(0, brk), left.open = TRUE) + 1L],
                                levels = class.labels)
    cells$value.class[cells$value == 0] <- "0"
    cols <- split.list(get.setting(job, "bin.colors"))
    if (length(cols) != length(class.labels)) {
      cat(sprintf("NOTE: '%s' - bin.colors has %d color(s) but there are %d classes - using white + viridis.\n",
                  job.label, length(cols), length(class.labels)))
      cols <- c("#FFFFFF", rev(grDevices::hcl.colors(length(class.labels) - 1, "viridis")))
    }
    names(cols) <- class.labels

    cells <- cells[, c("group", "aru.name", "bin", "bin.start", "bin.end", "passes",
                       "nights", "value", "value.class")]
    sites.out <- data.frame(group = as.character(site.tab$group), aru.name = sites,
                            nights = as.integer(nights[sites]),
                            passes = as.integer(tapply(cells$passes, factor(cells$aru.name, levels = sites), sum)),
                            stringsAsFactors = FALSE)
    rownames(sites.out) <- NULL

    plot.dates <- range(dj$night)
    settings <- list(time.start = min.to.hhmm(t.start), time.end = min.to.hhmm(t.end),
                     bin.minutes = bin.minutes, n.bins = n.bins, value.type = value.type,
                     bin.breaks = brk, bin.colors = cols, plot.group = group.col,
                     species = spp, dates = plot.dates)
    plots[[job.label]] <- list(cells = cells, sites = sites.out, settings = settings, file = NA_character_)

    cat(sprintf("Prepared '%s': %d site(s), %d bin(s) of %g min, %d pass(es), max cell value %g.\n",
                job.label, length(sites), n.bins, bin.minutes, sum(cells$passes), max(cells$value)))

    if (!have.gg) next

    ## ---- plot ------------------------------------------------------------------
    lab.pairs <- split.list(if ("group.labels" %in% names(job)) job$group.labels else "")
    lab.map <- setNames(trimws(sub("^[^=]*=", "", lab.pairs)), key(sub("=.*$", "", lab.pairs)))
    group.heading <- function(g) {
      out <- unname(lab.map[key(g)])
      ifelse(is.na(out) | !nzchar(out), g, out)
    }
    cells$group.lab <- factor(group.heading(cells$group), levels = unique(group.heading(groups)))
    cells$aru.name <- factor(cells$aru.name, levels = sites)

    bold.sites <- split.list(if ("site.bold" %in% names(job)) job$site.bold else "")
    miss.bold <- bold.sites[!key(bold.sites) %in% key(sites)]
    if (length(miss.bold) > 0) cat(sprintf("NOTE: '%s' - $site.bold site(s) not in the plot: %s\n",
                                           job.label, paste(miss.bold, collapse = ", ")))
    bold.face <- key(get.setting(job, "site.bold.face"))
    if (!bold.face %in% c("bold", "italic", "bold.italic", "plain")) bold.face <- "bold"
    x.labels <- function(x) {
      do.call(expression, lapply(x, function(s) {
        if (!key(s) %in% key(bold.sites) || bold.face == "plain") return(s)
        switch(bold.face, bold = bquote(bold(.(s))), italic = bquote(italic(.(s))),
               bold.italic = bquote(bolditalic(.(s))))
      }))
    }

    step <- max(1, round(num.setting(job, "yaxe.break.hours") * 60 / bin.minutes))
    y.breaks <- seq(0, n.bins - 1, by = step)
    y.labels <- min.to.hhmm(t.start + y.breaks * bin.minutes)
    legend.title <- gsub("<bin.minutes>", format(bin.minutes, trim = TRUE), get.setting(job, "legend.title"), fixed = TRUE)
    angle <- num.setting(job, "xaxe.text.angle")
    face.of <- function(p) { f <- key(get.setting(job, p)); if (f %in% c("plain", "bold", "italic", "bold.italic")) f else "plain" }
    grouped <- nzchar(group.col)
    divider <- data.frame(group.lab = factor(levels(cells$group.lab)[-1], levels = levels(cells$group.lab)))

    g <- ggplot2::ggplot(cells, ggplot2::aes(x = aru.name, y = bin + 0.5, fill = value.class)) +
      ggplot2::geom_tile(width = 1, height = 1, color = get.setting(job, "tile.color"),
                         linewidth = num.setting(job, "tile.linewidth")) +
      ggplot2::scale_fill_manual(values = cols, limits = class.labels, drop = FALSE, name = legend.title,
                                 guide = ggplot2::guide_legend(nrow = num.setting(job, "legend.nrow"),
                                                               title.position = "top", title.hjust = 0.5,
                                                               override.aes = list(color = "grey60"))) +
      ggplot2::scale_x_discrete(labels = x.labels, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, n.bins), breaks = y.breaks, labels = y.labels,
                                  expand = c(0, 0)) +
      ggplot2::labs(title = paste(spp, collapse = " + "),
                    x = get.setting(job, "xaxe.title"), y = get.setting(job, "yaxe.title")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     panel.spacing.x = grid::unit(0, "pt"),
                     strip.text = ggplot2::element_text(face = "bold", size = num.setting(job, "strip.text.size")),
                     axis.text = ggplot2::element_text(size = num.setting(job, "axis.text.size")),
                     axis.text.x = ggplot2::element_text(angle = angle, hjust = if (angle > 0) 1 else 0.5,
                                                         vjust = if (angle > 0) 1 else 0.5),
                     axis.ticks = ggplot2::element_line(color = "grey40"),
                     axis.title = ggplot2::element_text(size = num.setting(job, "axis.title.size")),
                     plot.title = ggplot2::element_text(face = face.of("plot.title.face"),
                                                        size = num.setting(job, "plot.title.size"),
                                                        hjust = num.setting(job, "plot.title.hjust")),
                     legend.position = get.setting(job, "legend.position"),
                     legend.text = ggplot2::element_text(size = num.setting(job, "legend.text.size")),
                     legend.title = ggplot2::element_text(size = num.setting(job, "legend.title.size")),
                     plot.background = ggplot2::element_rect(fill = "white", color = NA))
    if (grouped) {
      g <- g + ggplot2::facet_grid(. ~ group.lab, scales = "free_x", space = "free_x")
      if (nrow(divider) > 0) {
        g <- g + ggplot2::geom_vline(data = divider, ggplot2::aes(xintercept = 0.5),
                                     color = get.setting(job, "group.divider.color"),
                                     linewidth = num.setting(job, "group.divider.linewidth"))
      }
    }
    ggplots[[job.label]] <- g

    daterange.token <- sprintf("%sto%s", format(plot.dates[1], "%Y%m%d"), format(plot.dates[2], "%Y%m%d"))
    fname <- sprintf("%s_%s_%s_%s.png", project.name, job.label, daterange.token,
                     format(Sys.time(), "%Y%m%d%H%M%S"))
    dims <- plotutil_plot_dimensions(job, plot.inputs, job.label, "batz.plot_heat.site.species")   # $plot.dimensions (inches)
    ggplot2::ggsave(file.path(dir.save, fname), plot = g,
                    width = if (!is.null(dims)) dims[1] else num.setting(job, "plot.width") + num.setting(job, "ggsave.width.pad"),
                    height = if (!is.null(dims)) dims[2] else num.setting(job, "plot.height") + num.setting(job, "ggsave.height.pad"),
                    units = if (!is.null(dims)) "in" else get.setting(job, "ggsave.units"), dpi = num.setting(job, "ggsave.dpi"),
                    bg = "white")
    plots[[job.label]]$file <- fname
    cat("Saved:", fname, "\n")
  }

  if (length(plots) == 0) cat("No plots were made - see NOTE messages above.\n")
  invisible(list(plots = plots, ggplots = ggplots))
}

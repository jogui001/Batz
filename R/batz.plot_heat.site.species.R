#' Heat map of one species' passes per site by time of night
#'
#' Draws a site-by-time-of-night heat map for one bat species (or several
#' pooled): one column per site (\code{$aru.name}), one row per
#' \code{bin.minutes} interval of the night (default 20 minutes, 19:00 at
#' the bottom to 06:00 at the top), each cell colored by a class of
#' passes per monitoring night (default classes 0, 1-10, 11-50, 51-100,
#' 101-150, >150; 0 is white). Sites can be grouped into labelled blocks
#' (e.g. communities), selected sites dropped, and selected sites shown in
#' bold. One PNG per row of \code{fig.list} whose \code{$plot.type} is
#' \code{"heat.site.species"}.
#'
#' @param data Data frame of vetted detections, one row per recording
#'   (pass). Must have \code{$aru.name}, the species column named by
#'   \code{spp.id}, the monitoring-night date column named by
#'   \code{groupby.date}, and the clock-time column named by
#'   \code{groupby.time}. Every column named in a fig.list row's
#'   \code{$plot.group} must also exist. Headers are matched with
#'   \code{canonicalize.headers()}, so dot-, underscore-, space- or
#'   mixed-case spellings are all accepted (e.g. \code{ARU Name},
#'   \code{DATE-12}).
#' @param fig.list Data frame, one row per plot. Required columns:
#'   \code{$plot.type}, \code{$plot.name}, \code{$plot.species},
#'   \code{$plot.set}, \code{$date.start}, \code{$date.end},
#'   \code{$xaxe.title}, \code{$yaxe.title}. Optional columns:
#'   \code{$plot.group}, \code{$group.labels}, \code{$site.exclude},
#'   \code{$site.bold}. List-type cells (\code{$plot.species},
#'   \code{$plot.set}, \code{$site.exclude}, \code{$site.bold},
#'   \code{$group.labels}) are separated by \code{";"} or \code{","}. A
#'   row can also override any \code{aes.default} parameter by having a
#'   column of the same name (e.g. \code{$bin.minutes}, \code{$time.start}).
#'   See Details.
#' @param aes.default Data frame of plot settings, one row per parameter -
#'   normally \code{plotopts_heat.site.species.csv}. Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value}; an
#'   \code{$overide.value} column and \code{$notes} are optional.
#' @param spp.id Character, default \code{"species"}. Name of the species
#'   column in \code{data} (same parameter name as
#'   \code{\link{batz.generate_plotframe.bat}}).
#' @param groupby.date Character, default \code{"date.monitoringnight"}.
#'   Name of the monitoring-night date column. A raw Kaleidoscope-style
#'   export has this as \code{DATE-12}; pass \code{groupby.date = "date-12"}.
#' @param groupby.time Character, default \code{"time"}. Name of the
#'   clock-time column (\code{HH:MM:SS}; a full date-time such as
#'   \code{"2023-05-06 21:14:08"} also works - the first \code{HH:MM}
#'   found is used). Use the real recording time, not \code{TIME-12}.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows of \code{data} before counting; the number dropped is
#'   reported.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of each saved file name.
#' @param aes.style Character, default \code{"overide.value"}. Column of
#'   \code{aes.default} checked first for each parameter, falling back to
#'   \code{$default.value} when blank or absent. A fig.list row's own value
#'   for that parameter wins over both.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in.
#' @param strip.special Logical, default \code{TRUE}. Simplifies accented
#'   letters and removes other non-ASCII characters in \code{data} (this
#'   function's local copy only) before matching, same as every other
#'   \code{batz} function.
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same for
#'   \code{fig.list} and \code{aes.default}. \code{FALSE} keeps their text
#'   as is and stops with a readable error if a cell can't be read.
#'
#' @return Invisibly, a list with \code{plots} and \code{ggplots}, one
#'   element per plot made (named by \code{$plot.name}). Each
#'   \code{plots} element has \code{$cells} - one row per site x time bin:
#'   \code{$group}, \code{$aru.name}, \code{$bin}, \code{$bin.start},
#'   \code{$bin.end}, \code{$passes}, \code{$nights}, \code{$value},
#'   \code{$value.class} - and \code{$sites} - one row per site:
#'   \code{$group}, \code{$aru.name}, \code{$nights}, \code{$passes} -
#'   plus \code{$file} (the saved PNG name) and \code{$settings}.
#'
#' @details
#' \strong{Added 2026-10-16, per Josh.} Built from the \emph{Otomops
#' harrisoni} heat map made for the OLMN (Kajiado) and KA (NKRCP) sites
#' from \code{South_North_combined_vetted_1.csv}, with the same settings
#' style as the other \code{batz} plot functions (\code{fig.list} +
#' \code{aes.default} / \code{$overide.value}). The name follows the
#' \code{batz.<family>_<action>.<subject>()} convention as given
#' (family \code{plot}, action \code{heat}, subject \code{site.species}).
#'
#' \strong{fig.list columns.}
#' \itemize{
#'   \item \code{$plot.type} - must be \code{"heat.site.species"}; other
#'     rows are skipped with a NOTE, so one shared \code{fig.list.csv}
#'     can hold rows for several plot functions.
#'   \item \code{$plot.name} - used in the saved file name.
#'   \item \code{$plot.species} - species to plot, matched to
#'     \code{data[[spp.id]]} ignoring case and outer spaces. Several
#'     species are pooled into one plot. Also the plot title.
#'   \item \code{$plot.group} (optional) - column of \code{data} that groups
#'     sites into blocks (e.g. \code{"community"}). Blank or absent = one
#'     block of sites, no headings.
#'   \item \code{$plot.set} - which sites to include: values of
#'     \code{$plot.group} when it is given (e.g. \code{"OLMN;KA"} = every
#'     site in those communities), otherwise \code{$aru.name} values.
#'     Blank = every site. Blocks are drawn in \code{$plot.set} order.
#'   \item \code{$group.labels} (optional) - headings for the blocks, as
#'     \code{"value=label"} pairs, e.g. \code{"OLMN=Kajiado;KA=NKRCP"}. A
#'     block without a label uses its own value.
#'   \item \code{$site.exclude} (optional) - sites (\code{$aru.name}) to
#'     leave out, e.g. \code{"KABC_06;KABC_07"}.
#'   \item \code{$site.bold} (optional) - sites whose x-axis name is drawn
#'     in \code{$site.bold.face} (default bold), e.g. \code{"OLMN3;KA07"}.
#'   \item \code{$date.start} / \code{$date.end} - monitoring nights to
#'     include (\code{m/d/Y}, \code{Y-m-d} or \code{m/d/y}). Blank = no
#'     limit on that side.
#'   \item \code{$xaxe.title} / \code{$yaxe.title} - axis titles.
#' }
#'
#' \strong{Time bins.} Each record's clock time is placed in a
#' \code{bin.minutes} bin counted from \code{time.start}; the window runs
#' to \code{time.end} and may cross midnight. Records outside the window
#' are not plotted (they still count toward monitoring nights). The y axis
#' runs from \code{time.start} at the bottom to \code{time.end} at the
#' top, labelled every \code{yaxe.break.hours}.
#'
#' \strong{Cell values.} With \code{value.type = "mean.night"} (default)
#' a cell is the site's passes in that bin divided by the site's
#' monitoring nights: the number of unique \code{groupby.date} values for
#' that site among ALL records (any species) inside the date window - the
#' same effort definition as \code{\link{batz.generate_plotframe.conservancyrate}}.
#' A night when a detector ran but saved no files is not in the data, so it
#' is not counted. With one night per site the mean equals the total.
#' \code{value.type = "total"} plots raw counts. Every site x bin cell is
#' drawn, so a bin with no passes shows as 0 (white) - an empty cell means
#' none were detected.
#'
#' \strong{Color classes.} \code{bin.breaks} (default \code{10;50;100;150})
#' are the upper edges of the classes after 0: 0, (0,10], (10,50],
#' (50,100], (100,150], >150, labelled 0, 1-10, 11-50, 51-100, 101-150,
#' >150. A fractional mean (e.g. 0.33 with three nights) falls in 1-10.
#' Every class is shown in the legend even when no cell uses it.
#' \code{bin.colors} needs one color per class (\code{bin.breaks} + 2);
#' if the count is wrong a NOTE is printed and a white + viridis set is
#' used instead.
#'
#' \strong{Site order.} Within each block, sites are sorted by the text
#' before their number, then by the number (OLMN3, OLMN7, OLMN13 ... ,
#' KA01, KA02 ...).
#'
#' \strong{File name.} \code{"<project.name>_<plot.name>_<daterange>_<timestamp>.png"},
#' \code{<daterange>} = first and last monitoring night plotted
#' (\code{YYYYMMDDtoYYYYMMDD}), \code{<timestamp>} = \code{YYYYMMDDHHMMSS}
#' - the same pattern as \code{\link{batz.plotsm4_heatmap}}. Each saved name
#' is printed.
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
batz.plot_heat.site.species <- function(data, fig.list, aes.default,
                                        spp.id = "species",
                                        groupby.date = "date.monitoringnight",
                                        groupby.time = "time",
                                        duplicates.remove = TRUE,
                                        project.name = "new.project",
                                        aes.style = "overide.value",
                                        dir.save = getwd(),
                                        strip.special = TRUE,
                                        strip.special.plotopts = FALSE) {

  FN <- "batz.plot_heat.site.species"
  PLOT.TYPE <- "heat.site.species"

  ## ---- special characters ----------------------------------------------
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
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
  FIG.LIST.OPTIONAL <- c("plot.group", "group.labels", "site.exclude", "site.bold")
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
    miss.par <- setdiff(AES.DEFAULT.REQUIRED.PARAMETERS, trimws(aes.canon$df$parameter))
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
    i <- which(trimws(aes.default$parameter) == param)[1]
    if (is.na(i)) return(NA_character_)
    val <- as.character(aes.default$default.value[i])
    if (aes.style %in% names(aes.default)) {
      ov <- aes.default[[aes.style]][i]
      if (!blank(ov)) val <- as.character(ov)
    }
    if (is.na(val)) "" else val
  }
  get.setting <- function(job, param) {
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
    ggplot2::ggsave(file.path(dir.save, fname), plot = g,
                    width = num.setting(job, "plot.width") + num.setting(job, "ggsave.width.pad"),
                    height = num.setting(job, "plot.height") + num.setting(job, "ggsave.height.pad"),
                    units = get.setting(job, "ggsave.units"), dpi = num.setting(job, "ggsave.dpi"),
                    bg = "white")
    plots[[job.label]]$file <- fname
    cat("Saved:", fname, "\n")
  }

  if (length(plots) == 0) cat("No plots were made - see NOTE messages above.\n")
  invisible(list(plots = plots, ggplots = ggplots))
}

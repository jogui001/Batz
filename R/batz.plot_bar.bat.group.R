#' Bar plot of one species' activity and site detections per community
#'
#' For each community (\code{$community}) draws, side by side in its own
#' color: a pale bar for the number of sites sampled and a solid bar for the
#' number of sites where the species was detected (right axis), with a
#' narrow black bar for passes per monitoring night (left axis, log
#' scale). Communities can be split into labelled panels (e.g. by
#' \code{$project}: Kajiado / NKRCP). The input is the table made by
#' \code{\link{batz.generate_plotframe.bat.group}}. One PNG per row of
#' \code{fig.list} whose \code{$plot.type} is \code{"bar.bat.group"}.
#'
#' @param data Data frame from \code{\link{batz.generate_plotframe.bat.group}}
#'   (dot or snake_case headers). Must have \code{$year}, \code{$community},
#'   \code{$species}, \code{$observation_count}, \code{$sites_sampled},
#'   \code{$sites_detected}, \code{$date.monitoringnight_sampled},
#'   \code{$date.monitoringnight_detected}, \code{$project}. Every column
#'   named in a fig.list row's \code{$plot.group} must also exist.
#' @param fig.list Data frame, one row per plot. Required columns:
#'   \code{$plot.type}, \code{$plot.name}, \code{$plot.species},
#'   \code{$plot.set}, \code{$xaxe.title}, \code{$yaxe.title}. Optional:
#'   \code{$plot.year}, \code{$plot.group}, \code{$group.labels},
#'   \code{$community.exclude}, \code{$yaxe.title.right}. A row can also
#'   override any \code{aes.default} parameter with a column of the same
#'   name. See Details.
#' @param aes.default Data frame of plot settings - normally
#'   \code{plotopts_bar.bat.group.csv}. Must have \code{$category},
#'   \code{$parameter}, \code{$default.value}; \code{$overide.value} and
#'   \code{$notes} optional.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of each saved file name.
#' @param aes.style Character, default \code{"overide.value"}. Column of
#'   \code{aes.default} checked first for each parameter, falling back to
#'   \code{$default.value}; a fig.list row's own value wins over both.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in.
#' @param strip.special Logical, default \code{TRUE}. Simplifies accented
#'   letters and removes other non-ASCII characters in \code{data} (local
#'   copy only).
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same for
#'   \code{fig.list} and \code{aes.default}; \code{FALSE} keeps their text
#'   and stops with a readable error if a cell can't be read.
#'
#' @return Invisibly, a list with \code{plots} and \code{ggplots}, one
#'   element per plot made (named by \code{$plot.name}). Each \code{plots}
#'   element has \code{$bars} - one row per community: \code{$group},
#'   \code{$community}, \code{$observation_count}, \code{$sites_sampled},
#'   \code{$sites_detected}, \code{$date.monitoringnight_sampled},
#'   \code{$date.monitoringnight_detected}, \code{$passes.per.night},
#'   \code{$color} - plus \code{$file} (saved PNG name) and \code{$settings}.
#'
#' @details
#' \strong{Added 2026-10-21, per Josh.} Built from the \emph{Otomops
#' harrisoni} community bar plot (Kajiado and NKRCP panels) made in an
#' earlier chat, whose plotting code was never saved. The heights were
#' checked against that image: the black bars are
#' \code{$observation_count / $date.monitoringnight_sampled} for
#' \code{$year = "all"} (e.g. OLMN 1924 / 7 = 275, KA 1629 / 13 = 125), the
#' pale bars \code{$sites_sampled} and the solid bars
#' \code{$sites_detected}. The default community colors are the ones read
#' off that image (ColorBrewer Paired for the Kajiado communities, Dark2
#' for NKRCP).
#'
#' \strong{fig.list columns.}
#' \itemize{
#'   \item \code{$plot.type} - must be \code{"bar.bat.group"}; other rows
#'     are skipped with a NOTE.
#'   \item \code{$plot.name} - used in the saved file name.
#'   \item \code{$plot.species} - species to plot (one), matched to
#'     \code{$species} ignoring case. Also the plot title.
#'   \item \code{$plot.year} (optional) - \code{$year} value to plot;
#'     blank = \code{"all"} (all years pooled).
#'   \item \code{$plot.group} (optional) - column that splits communities
#'     into panels, e.g. \code{"project"}. Blank = one panel.
#'   \item \code{$plot.set} - which panels to include, as values of
#'     \code{$plot.group} (e.g. \code{"South;North"}), drawn in this order;
#'     without \code{$plot.group}, a list of communities. Blank = all.
#'   \item \code{$group.labels} (optional) - panel headings,
#'     \code{"South=Kajiado;North=NKRCP"}.
#'   \item \code{$community.exclude} (optional) - communities to leave out.
#'   \item \code{$xaxe.title}, \code{$yaxe.title} - axis titles (left axis);
#'     \code{$yaxe.title.right} (optional, default
#'     \code{"Number of Sites"}) for the right axis.
#' }
#' List cells are separated by \code{";"} or \code{","}.
#'
#' \strong{Axes.} The left axis is \code{log1p}-scaled (so 0 can be shown)
#' with breaks \code{yaxe.left.breaks} (default 0, 4, 20, 80, 400). The
#' right axis is linear with breaks \code{yaxe.right.breaks}. The two are
#' lined up so that the top left break and the top right break sit at the
#' same height; data above either is still drawn (the axis extends).
#' \code{passes.per} chooses the black-bar denominator: \code{"sampled"}
#' (default, all site-nights in the community) or \code{"detected"}
#' (only site-nights with the species). A community with 0 passes has no
#' black bar.
#'
#' \strong{Colors.} \code{community.colors} is a list of
#' \code{community=#hex} pairs; a community not in the list gets a color
#' from \code{grDevices::hcl.colors(, "Dark 3")} and a NOTE. The pale bar
#' uses the same color at \code{sampled.alpha}.
#'
#' \strong{File name.} \code{"<project.name>_<plot.name>_<year>_<timestamp>.png"},
#' timestamp \code{YYYYMMDDHHMMSS}.
#'
#' @examples
#' \dontrun{
#' vetted <- read.csv("South_North_combined_vetted_1.csv", check.names = FALSE)
#' plfr <- batz.generate_plotframe.bat.group(vetted, groupby.date = "date-12",
#'                                           write.output = FALSE)
#' fig.list <- read.csv("fig.list_bar.bat.group.csv", check.names = FALSE)
#' plotopts <- read.csv("plotopts_bar.bat.group.csv", check.names = FALSE)
#' res <- batz.plot_bar.bat.group(plfr, fig.list, plotopts, project.name = "south.north")
#' res$plots[[1]]$bars
#' }
#'
#' @export
batz.plot_bar.bat.group <- function(data, fig.list, aes.default,
                                    project.name = "new.project",
                                    aes.style = "overide.value",
                                    dir.save = getwd(),
                                    strip.special = TRUE,
                                    strip.special.plotopts = FALSE) {

  FN <- "batz.plot_bar.bat.group"
  PLOT.TYPE <- "bar.bat.group"

  ## ---- special characters ----------------------------------------------
  objname.fig.list <- paste(deparse(substitute(fig.list)), collapse = "")
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
  for (a in c("data", "fig.list", "aes.default")) {
    if (!is.data.frame(get(a))) stop(sprintf("%s: `%s` must be a data frame.", FN, a), call. = FALSE)
  }
  data <- special.strip(data, strip.special)$df
  if (isTRUE(strip.special.plotopts)) {
    fig.list <- special.strip(fig.list, TRUE)$df
    aes.default <- special.strip(aes.default, TRUE)$df
  } else {
    special.check.plotopts(fig.list, "fig.list", objname.fig.list, FN)
    special.check.plotopts(aes.default, "aes.default", objname.aes.default, FN)
  }

  ## ---- header checks -----------------------------------------------------
  DATA.REQUIRED <- c("year", "community", "species", "observation_count", "sites_sampled",
                     "sites_detected", "date.monitoringnight_sampled",
                     "date.monitoringnight_detected", "project")
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "plot.species", "plot.set",
                         "xaxe.title", "yaxe.title")
  FIG.LIST.OPTIONAL <- c("plot.year", "plot.group", "group.labels", "community.exclude",
                         "yaxe.title.right")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "passes.per", "yaxe.left.breaks", "yaxe.right.breaks", "community.colors",
    "sampled.alpha", "bar.width", "passes.bar.width", "passes.bar.color",
    "bar.outline.color", "bar.outline.linewidth", "passes.legend.label",
    "community.legend.title", "legend.position", "legend.nrow",
    "strip.fill", "strip.text.size", "plot.title.size", "plot.title.face",
    "plot.title.hjust", "axis.title.size", "axis.text.size", "legend.text.size",
    "legend.title.size", "ggsave.dpi", "ggsave.units", "ggsave.width.pad",
    "ggsave.height.pad", "plot.width", "plot.height")

  data.canon <- canonicalize.headers(data, DATA.REQUIRED)
  fig.canon <- canonicalize.headers(fig.list, c(FIG.LIST.REQUIRED, FIG.LIST.OPTIONAL))
  fig.canon$missing <- setdiff(fig.canon$missing, FIG.LIST.OPTIONAL)
  aes.canon <- canonicalize.headers(aes.default, AES.DEFAULT.REQUIRED)
  missing.msg <- function(canon, label) {
    if (length(canon$missing) > 0) sprintf("%s is missing these headers: %s", label,
                                            paste(canon$missing, collapse = ", "))
  }
  problems <- c(missing.msg(data.canon, "data"), missing.msg(fig.canon, "fig.list"),
                missing.msg(aes.canon, "aes.default"))
  if ("parameter" %in% names(aes.canon$df)) {
    miss.par <- setdiff(AES.DEFAULT.REQUIRED.PARAMETERS, trimws(aes.canon$df$parameter))
    if (length(miss.par) > 0) problems <- c(problems, sprintf(
      "aes.default is missing these required $parameter rows: %s - it may be an older copy of plotopts_bar.bat.group.csv",
      paste(miss.par, collapse = ", ")))
  }
  if (length(problems) > 0) stop(paste(problems, collapse = "\n\n"), call. = FALSE)
  data <- data.canon$df
  fig.list <- fig.canon$df
  aes.default <- aes.canon$df

  ## ---- helpers -----------------------------------------------------------
  blank <- function(v) is.null(v) || length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v)))
  split.list <- function(v) {
    if (blank(v)) return(character(0))
    x <- trimws(gsub('"', "", strsplit(as.character(v), "[;,]")[[1]]))
    x[nzchar(x)]
  }
  key <- function(v) tolower(trimws(as.character(v)))
  get.default <- function(param) {
    i <- which(trimws(aes.default$parameter) == param)[1]
    if (is.na(i)) return("")
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
  num.list <- function(job, param) {
    v <- suppressWarnings(as.numeric(split.list(get.setting(job, param))))
    v <- sort(unique(v[!is.na(v) & v >= 0]))
    if (length(v) == 0) stop(sprintf("%s: setting '%s' has no usable numbers.", FN, param), call. = FALSE)
    v
  }
  face.of <- function(job, p) {
    f <- key(get.setting(job, p))
    if (f %in% c("plain", "bold", "italic", "bold.italic")) f else "plain"
  }
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))

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

    spp <- split.list(job$plot.species)
    if (length(spp) != 1) stop(sprintf("%s: fig.list row '%s' needs exactly one $plot.species.", FN, job.label), call. = FALSE)
    year <- if ("plot.year" %in% names(job) && !blank(job$plot.year)) trimws(as.character(job$plot.year)) else "all"
    d <- data[key(data$species) == key(spp) & key(data$year) == key(year), , drop = FALSE]
    if (nrow(d) == 0) {
      cat(sprintf("NOTE: fig.list row '%s' - no rows for species '%s', year '%s' - no plot made.\n",
                  job.label, spp, year))
      next
    }

    group.col <- if ("plot.group" %in% names(job) && !blank(job$plot.group)) trimws(job$plot.group) else ""
    if (nzchar(group.col)) {
      gc <- canonicalize.headers(d, group.col)
      if (length(gc$missing) > 0) {
        cat(sprintf("NOTE: fig.list row '%s' - $plot.group '%s' is not a column of data - skipped.\n", job.label, group.col))
        next
      }
      d$group <- trimws(as.character(gc$df[[group.col]]))
    } else {
      d$group <- ""
    }
    sel <- split.list(job$plot.set)
    if (length(sel) > 0) {
      on <- if (nzchar(group.col)) d$group else d$community
      unknown <- sel[!key(sel) %in% key(on)]
      if (length(unknown) > 0) cat(sprintf("NOTE: fig.list row '%s' - $plot.set value(s) not found: %s\n",
                                           job.label, paste(unknown, collapse = ", ")))
      d <- d[key(on) %in% key(sel), , drop = FALSE]
    }
    excl <- split.list(if ("community.exclude" %in% names(job)) job$community.exclude else "")
    d <- d[!key(d$community) %in% key(excl), , drop = FALSE]
    if (nrow(d) == 0) {
      cat(sprintf("NOTE: fig.list row '%s' matched 0 communities - no plot made.\n", job.label))
      next
    }

    ## one row per community (pooled across rows if a community appears twice in a group)
    agg.cols <- c("observation_count", "sites_sampled", "sites_detected",
                  "date.monitoringnight_sampled", "date.monitoringnight_detected")
    for (cc in agg.cols) d[[cc]] <- num(d[[cc]])
    bars <- stats::aggregate(d[agg.cols], by = list(group = d$group, community = d$community), FUN = sum)
    groups <- unique(bars$group)
    if (nzchar(group.col) && length(sel) > 0) {
      o <- match(key(groups), key(sel)); groups <- groups[order(is.na(o), o, groups)]
    } else groups <- sort(groups)
    bars <- bars[order(match(bars$group, groups), tolower(bars$community)), , drop = FALSE]
    rownames(bars) <- NULL

    passes.per <- key(get.setting(job, "passes.per"))
    if (!passes.per %in% c("sampled", "detected")) {
      stop(sprintf("%s: passes.per = '%s' - use 'sampled' or 'detected'.", FN, passes.per), call. = FALSE)
    }
    den <- if (passes.per == "sampled") bars$date.monitoringnight_sampled else bars$date.monitoringnight_detected
    bars$passes.per.night <- ifelse(den > 0, bars$observation_count / den, 0)

    ## colors
    pairs <- split.list(get.setting(job, "community.colors"))
    cmap <- setNames(trimws(sub("^[^=]*=", "", pairs)), key(sub("=.*$", "", pairs)))
    bars$color <- unname(cmap[key(bars$community)])
    no.col <- is.na(bars$color) | !nzchar(bars$color)
    if (any(no.col)) {
      cat(sprintf("NOTE: '%s' - no community.colors entry for: %s - default colors used.\n",
                  job.label, paste(bars$community[no.col], collapse = ", ")))
      bars$color[no.col] <- grDevices::hcl.colors(sum(no.col), "Dark 3")
    }

    lab.pairs <- split.list(if ("group.labels" %in% names(job)) job$group.labels else "")
    lab.map <- setNames(trimws(sub("^[^=]*=", "", lab.pairs)), key(sub("=.*$", "", lab.pairs)))
    heading <- function(g) { h <- unname(lab.map[key(g)]); ifelse(is.na(h) | !nzchar(h), g, h) }

    l.breaks <- num.list(job, "yaxe.left.breaks")
    r.breaks <- num.list(job, "yaxe.right.breaks")
    k <- log1p(max(l.breaks)) / max(r.breaks)            # right-axis units -> left (log1p) units
    top <- max(log1p(max(l.breaks)), log1p(max(bars$passes.per.night)), k * max(bars$sites_sampled))

    settings <- list(species = spp, year = year, plot.group = group.col, passes.per = passes.per,
                     yaxe.left.breaks = l.breaks, yaxe.right.breaks = r.breaks, right.to.left = k)
    plots[[job.label]] <- list(bars = bars[, c("group", "community", agg.cols, "passes.per.night", "color")],
                               settings = settings, file = NA_character_)
    cat(sprintf("Prepared '%s': %d communit%s in %d panel(s), max passes per night %.1f.\n", job.label,
                nrow(bars), if (nrow(bars) == 1) "y" else "ies", length(groups), max(bars$passes.per.night)))
    if (!have.gg) next

    ## ---- plot ---------------------------------------------------------------
    pd <- bars
    pd$community <- factor(pd$community, levels = unique(pd$community))
    pd$panel <- factor(heading(pd$group), levels = unique(heading(groups)))
    pd$y.sampled <- pd$sites_sampled * k
    pd$y.detected <- pd$sites_detected * k
    pd$y.passes <- log1p(pd$passes.per.night)
    fills <- setNames(pd$color, as.character(pd$community))
    bw <- num.setting(job, "bar.width")
    pw <- num.setting(job, "passes.bar.width")
    right.title <- if ("yaxe.title.right" %in% names(job) && !blank(job$yaxe.title.right)) job$yaxe.title.right else "Number of Sites"
    passes.lab <- get.setting(job, "passes.legend.label")

    g <- ggplot2::ggplot(pd, ggplot2::aes(x = community)) +
      ggplot2::geom_col(ggplot2::aes(y = y.sampled, fill = community), width = bw,
                        alpha = num.setting(job, "sampled.alpha"), show.legend = FALSE) +
      ggplot2::geom_col(ggplot2::aes(y = y.detected, fill = community), width = bw,
                        color = get.setting(job, "bar.outline.color"),
                        linewidth = num.setting(job, "bar.outline.linewidth")) +
      ggplot2::geom_col(data = pd[pd$passes.per.night > 0, , drop = FALSE],
                        ggplot2::aes(y = y.passes, color = passes.lab), width = pw,
                        fill = get.setting(job, "passes.bar.color")) +
      ggplot2::scale_fill_manual(values = fills, breaks = names(fills),
                                 name = get.setting(job, "community.legend.title"),
                                 guide = ggplot2::guide_legend(nrow = num.setting(job, "legend.nrow"), byrow = TRUE,
                                                               order = 2,
                                                               override.aes = list(alpha = 1))) +
      ggplot2::scale_color_manual(values = setNames(get.setting(job, "passes.bar.color"), passes.lab),
                                  name = NULL,
                                  guide = ggplot2::guide_legend(order = 1,
                                    override.aes = list(fill = get.setting(job, "passes.bar.color")))) +
      ggplot2::scale_y_continuous(breaks = log1p(l.breaks), labels = format(l.breaks, trim = TRUE),
                                  limits = c(0, top), expand = ggplot2::expansion(mult = c(0, 0.04)),
                                  sec.axis = ggplot2::sec_axis(~ . / k, breaks = r.breaks, name = right.title)) +
      ggplot2::labs(title = spp, x = get.setting(job, "xaxe.title"), y = get.setting(job, "yaxe.title")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     strip.background = ggplot2::element_rect(fill = get.setting(job, "strip.fill")),
                     strip.text = ggplot2::element_text(face = "bold", size = num.setting(job, "strip.text.size")),
                     plot.title = ggplot2::element_text(face = face.of(job, "plot.title.face"),
                                                        size = num.setting(job, "plot.title.size"),
                                                        hjust = num.setting(job, "plot.title.hjust")),
                     axis.title = ggplot2::element_text(size = num.setting(job, "axis.title.size")),
                     axis.text = ggplot2::element_text(size = num.setting(job, "axis.text.size")),
                     legend.position = get.setting(job, "legend.position"),
                     legend.box = "horizontal",
                     legend.text = ggplot2::element_text(size = num.setting(job, "legend.text.size")),
                     legend.title = ggplot2::element_text(size = num.setting(job, "legend.title.size")))
    if (nzchar(group.col)) g <- g + ggplot2::facet_grid(. ~ panel, scales = "free_x", space = "free_x")
    ggplots[[job.label]] <- g

    fname <- sprintf("%s_%s_%s_%s.png", project.name, job.label, year, format(Sys.time(), "%Y%m%d%H%M%S"))
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

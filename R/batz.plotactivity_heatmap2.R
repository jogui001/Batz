#' Heatmap of bat activity by date and time of day (v2)
#'
#' Added 2026-10-07, per Josh. Draws a ggplot2 heatmap of observation
#' counts - \code{$observations.count} (or the older \code{$obs}) - with
#' date on the X axis and time of day on the Y axis. Built for the output
#' of \code{\link{batz.generate_plotframe.bat}} (e.g. with
#' \code{pool.interval = "15 min"}), but works on any data frame with a
#' date column, a time column and a count column.
#'
#' \strong{Two ways to run it (2026-10-09).} With \code{fig.list = NULL}
#' (the original behaviour) one PNG is saved per \code{$group} x
#' \code{$spp.id} combination in \code{data}, named
#' \verb{<project.name>_<group>_<spp.id>_<date.min>to<date.max>.png}. With
#' a \code{fig.list}, each row with \code{$plot.type = "activity.heatmap"}
#' makes a plot (or one plot per facet) the same way as the other batz
#' fig.list plot functions - see \strong{fig.list} below.
#'
#' \strong{Where each setting comes from.} Every setting below
#' (\code{date.col} through \code{time.interval}, plus the plotopts-only
#' ones such as \code{plot.width}) is taken from, in order: (1) the
#' fig.list row's own column of that name, if filled; (2) the function
#' argument, if you passed it; (3) \code{aes.default} (its
#' \code{aes.style} column, then \code{$default.value}); (4) the argument's
#' default. See \code{plotopts_heatmap2.csv} for every setting.
#'
#' \strong{Day layout.} With \code{date.col = "date.monitoringnight"} the
#' Y axis runs from noon on one calendar day to noon the next, and a solid
#' black line marks 00:00:00 (midnight). With \code{date.col =
#' "date.calendar"} (\code{"date.calander"} is also accepted) the Y axis
#' runs from midnight to midnight, and the black line marks 12:00:00
#' (noon). Time runs upward from \code{time.start} (bottom) to
#' \code{time.end} (top), like \code{\link{batz.plotactivity_heatmap}}.
#' Monitoring-night dates are moved to calendar days (and back) when the
#' layout needs it.
#'
#' \strong{Date column.} If \code{data} has a column named
#' \code{date.col} it is used. Otherwise, for a
#' \code{batz.generate_plotframe.bat()} frame (which has \code{$date} and a
#' \code{$groupby.date} naming the date type), \code{$date} is used.
#'
#' \strong{Time column.} Only the time of day is read from
#' \code{time.col} - e.g. \code{"2026-08-14 21:15:00"}, \code{"21:15"},
#' \code{"21:15:00"} or \code{"9:15 PM"}. Each record goes in the bin its
#' time falls in, and counts in the same date/bin are summed. Bins with no
#' records count as 0.
#'
#' \strong{Colour scale (bin.scale, 2026-10-09).}
#' \itemize{
#'   \item \code{"discrete"} (default): \code{bin.values} are the upper
#'     limits of each bin. With \code{c(0, 1, 10, 25, 50)} the bins are 0,
#'     1, 2-10, 11-25, 26-50 and 51+, so \code{bin.colors} needs one more
#'     colour than \code{bin.values}. Every bin is shown in the legend,
#'     with its colour, even when no cell falls in it.
#'   \item \code{"continuous"}: a colour gradient from \code{min(bin.values)}
#'     to \code{max(bin.values)}, with \code{bin.colors} spread evenly from
#'     the bottom to the top of the scale. Counts above the top value get
#'     the top colour; the legend ticks are at \code{bin.values}, the top
#'     one labelled e.g. \code{"20+"}.
#' }
#'
#' \strong{Named palettes (2026-10-09).} \code{bin.colors} may be a list
#' of colours or ONE palette name, from \pkg{viridis}/\pkg{viridisLite}
#' (\code{"magma"}, \code{"viridis"}, ...), \pkg{scico} (\code{"batlow"},
#' \code{"lajolla"}, ...), \pkg{RColorBrewer} (\code{"YlOrRd"}, ...),
#' \code{"Okabe-Ito"} (base R) or \pkg{colorspace} (\code{"Heat"},
#' \code{"Sunset"}, ...). Name the package to be exact -
#' \code{"scico::batlow"}, \code{"RColorBrewer::YlOrRd"},
#' \code{"colorspace::YlOrRd"}. Put \code{"-"} in front to reverse it
#' (\code{"-magma"}). The right number of colours is taken from the
#' palette. \strong{Name clashes:} colorspace has its own version of many
#' viridis, scico and RColorBrewer palettes under the same names (e.g.
#' \code{YlOrRd}, \code{Blues}, \code{batlow}, \code{viridis},
#' \code{plasma}) - close but not identical colours. scico, RColorBrewer,
#' viridis and Okabe-Ito don't clash with each other. A bare name uses the
#' original package (viridis, then scico, then RColorBrewer, then
#' Okabe-Ito, then colorspace) and prints a NOTE when it clashes; write
#' \code{"colorspace::<name>"} for the colorspace version. The packages are
#' optional - only the one a palette comes from has to be installed.
#'
#' \strong{fig.list (2026-10-09).} Same format as the other batz fig.list
#' plot functions. Required columns: \code{$plot.type}
#' (\code{"activity.heatmap"}; other rows are skipped), \code{$plot.name},
#' \code{$facet.header} (\code{"sppid"}), \code{$facet.plot},
#' \code{$facet.set}, \code{$all.dectections}, \code{$40khzmyo},
#' \code{$facet.label}, \code{$plot.set}, \code{$date.start},
#' \code{$date.end}. Optional: any setting named above (e.g.
#' \code{$bin.scale}, \code{$bin.colors}, \code{$time.start}), plus
#' \code{$date.format}, \code{$xaxe.interval}, \code{$xaxe.title},
#' \code{$yaxe.title}. \code{$facet.set} is a preset (\code{"NE"}) or an
#' ordered list (\code{"AllDet;lano;labo"}); the order written is the
#' facet order. \code{$all.dectections = TRUE} adds All detections as the
#' first facet. A 40kHzMyo facet (\code{$40khzmyo = TRUE}) is drawn as its
#' own facet - a heatmap can't overlay it. All detections uses the
#' \code{"All Detections"} rows of \code{data}; if there are none, every
#' species row is summed. \code{$plot.set} picks \code{$group} value(s)
#' (several are summed together); blank = all. \code{$facet.plot = TRUE}
#' saves one faceted plot named
#' \verb{<plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png};
#' \code{FALSE} saves one plot per facet named
#' \verb{<plot.name>_<facet>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}
#' (facets with no data are skipped). Names over 100 characters are
#' shortened as described in \code{\link{batz.plotactivity_observations}}.
#' See \code{heat2.figlist.csv} for an example.
#'
#' @param data Data frame to plot. Needs \code{date.col} (see Details),
#'   \code{time.col} and \code{$observations.count} (or \code{$obs}).
#'   \code{$group} and \code{$spp.id} are used to split/select plots.
#' @param fig.list Optional data frame, one row per plot - see Details,
#'   "fig.list". \code{NULL} (default) = one plot per group x spp.id.
#' @param aes.default Optional plot-settings data frame
#'   (\code{plotopts_heatmap2.csv}: \code{$category}, \code{$parameter},
#'   \code{$default.value}, plus the \code{aes.style} column).
#' @param date.col Character, default \code{"date.monitoringnight"}. Date
#'   column, and the day layout: \code{"date.monitoringnight"} (noon to
#'   noon) or \code{"date.calendar"} (midnight to midnight).
#' @param time.col Character, default \code{"pool.start"}. Column holding
#'   each record's time of day.
#' @param bin.intervals Character, default \code{"15 min"}. Height of each
#'   time bin, e.g. \code{"15 min"}, \code{"30 min"}, \code{"1 hour"}. Must
#'   divide evenly into 24 hours.
#' @param bin.values Numeric, default \code{c(0, 1, 10, 25, 50)} (or text
#'   \code{"0;1;10;25;50"} in fig.list/plotopts). See Details, "Colour
#'   scale".
#' @param bin.colors Character, default \code{c("white", "#FFFF80",
#'   "#FFFF00", "#FFAA00", "#FF5500", "#FF0000")}. Colours (separated by
#'   \code{";"} in fig.list/plotopts) or one palette name - see Details.
#' @param bin.scale Character, \code{"discrete"} (default) or
#'   \code{"continuous"}. See Details, "Colour scale".
#' @param time.start Character, default \code{"15:00"}. Time at the
#'   bottom of the Y axis (HH:MM).
#' @param time.end Character, default \code{"08:00"}. Time at the top of
#'   the Y axis (HH:MM). With \code{"date.monitoringnight"} it may be after
#'   midnight (e.g. \code{"15:00"} to \code{"08:00"}).
#' @param time.interval Character, default \code{"1 hour"}. Spacing of the
#'   Y-axis labels.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of the file name when \code{fig.list = NULL} (fig.list plots are named
#'   from \code{$plot.name}).
#' @param aes.style Character, default \code{"overide.value"}. Column of
#'   \code{aes.default} that overrides \code{$default.value} when filled.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG(s)
#'   are saved in.
#'
#' @return Invisibly, a list: \code{$data} (the binned counts, one row per
#'   plot/facet/date/time bin), \code{$plots} (named list of ggplot
#'   objects) and \code{$files} (saved file paths).
#'
#' @examples
#' \dontrun{
#' plfr <- batz.generate_plotframe.bat(vetted.merged, pool.interval = "15 min")
#'
#' # no fig.list: one plot per group x species
#' batz.plotactivity_heatmap2(plfr[plfr$spp.id == "lano", ],
#'   project.name = "Riverpoint", dir.save = "C:/path/to/figures")
#'
#' # continuous scale with a named palette
#' batz.plotactivity_heatmap2(plfr, bin.scale = "continuous",
#'   bin.values = c(0, 1, 10, 12, 20), bin.colors = "scico::lajolla")
#'
#' # fig.list + plot settings files
#' heat2.figlist <- read.csv("heat2.figlist.csv", check.names = FALSE)
#' plotopts <- read.csv("plotopts_heatmap2.csv", check.names = FALSE)
#' batz.plotactivity_heatmap2(plfr, fig.list = heat2.figlist,
#'   aes.default = plotopts, dir.save = "C:/path/to/figures")
#' }
#'
#' @export
batz.plotactivity_heatmap2 <- function(data,
                                       fig.list = NULL,
                                       aes.default = NULL,
                                       date.col = "date.monitoringnight",
                                       time.col = "pool.start",
                                       bin.intervals = "15 min",
                                       bin.values = c(0, 1, 10, 25, 50),
                                       bin.colors = c("white", "#FFFF80", "#FFFF00", "#FFAA00", "#FF5500", "#FF0000"),
                                       bin.scale = "discrete",
                                       time.start = "15:00",
                                       time.end = "08:00",
                                       time.interval = "1 hour",
                                       project.name = "new.project",
                                       aes.style = "overide.value",
                                       dir.save = getwd()) {

  FN <- "batz.plotactivity_heatmap2"
  PLOT.TYPE <- "activity.heatmap"
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (nrow(data) == 0) stop("`data` has 0 rows - nothing to plot.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required - install.packages(\"ggplot2\").")

  ## ---- settings: fig.list row > explicit argument > aes.default > argument default
  arg.names <- c("date.col", "time.col", "bin.intervals", "bin.values", "bin.colors", "bin.scale",
                 "time.start", "time.end", "time.interval")
  arg.given <- c(date.col = !missing(date.col), time.col = !missing(time.col),
                 bin.intervals = !missing(bin.intervals), bin.values = !missing(bin.values),
                 bin.colors = !missing(bin.colors), bin.scale = !missing(bin.scale),
                 time.start = !missing(time.start), time.end = !missing(time.end),
                 time.interval = !missing(time.interval))
  arg.vals <- mget(arg.names, envir = environment())
  ## plotopts-only settings and their built-in defaults
  OPT.DEFAULTS <- list(
    legend.title = "Number of Observations", legend.position = "bottom",
    midline.color = "black", midline.linewidth = "0.6",
    facpan.numcol = "1", facpan.hgt = "3",
    plot.width = "10", plot.height = "6", ggsave.units = "in", ggsave.dpi = "300",
    plot.title.size = "12", axis.title.size = "12", axis.text.size = "10",
    strip.text.size = "10", legend.title.size = "11", legend.text.size = "10",
    date.format = "%d %b", xaxe.interval = "", xaxe.title = "", yaxe.title = "Time of Day"
  )
  if (!is.null(aes.default)) {
    if (!is.data.frame(aes.default)) stop("`aes.default` must be a data frame (e.g. read.csv(\"plotopts_heatmap2.csv\")).")
    ad.canon <- canonicalize.headers(aes.default, c("category", "parameter", "default.value"))
    if (length(ad.canon$missing) > 0) {
      stop("aes.default is missing these headers: ", paste(ad.canon$missing, collapse = ", "))
    }
    aes.default <- ad.canon$df
  }
  opt.lookup <- function(param) {
    if (is.null(aes.default)) return(NA_character_)
    i <- which(trimws(as.character(aes.default$parameter)) == param)
    if (length(i) == 0) return(NA_character_)
    if (aes.style %in% names(aes.default)) {
      o <- aes.default[[aes.style]][i[1]]
      if (!is.na(o) && nzchar(trimws(as.character(o)))) return(as.character(o))
    }
    v <- aes.default$default.value[i[1]]
    if (is.na(v) || !nzchar(trimws(as.character(v)))) NA_character_ else as.character(v)
  }
  setting <- function(job, param) {
    if (!is.null(job) && param %in% names(job)) {
      v <- job[[param]]
      if (length(v) == 1 && !is.na(v) && nzchar(trimws(as.character(v)))) return(trimws(as.character(v)))
    }
    if (param %in% arg.names && arg.given[[param]]) return(arg.vals[[param]])
    v <- opt.lookup(param)
    if (!is.na(v)) return(v)
    if (param %in% arg.names) return(arg.vals[[param]])
    OPT.DEFAULTS[[param]]
  }
  num.opt <- function(job, param) {
    v <- suppressWarnings(as.numeric(setting(job, param)))
    if (length(v) != 1 || is.na(v)) as.numeric(OPT.DEFAULTS[[param]]) else v
  }

  ## ---- data columns that don't depend on settings
  std <- standardize.headers(names(data))
  count.idx <- match(c("observations_count", "observation_count", "obs"), std)
  count.idx <- count.idx[!is.na(count.idx)]
  if (length(count.idx) == 0) stop("`data` needs an $observations.count (or $obs) column.")
  if (std[count.idx[1]] == "obs") {
    cat("WARNING: `data` has the old header $obs - used as $observations.count.\n")
  }
  group.idx <- match("group", std)
  spp.idx <- match("spp_id", std)

  ## ---- one "job" per plot request
  if (is.null(fig.list)) {
    jobs <- list(list(job = NULL, mode = "legacy"))
  } else {
    if (!is.data.frame(fig.list)) stop("`fig.list` must be a data frame (e.g. read.csv(\"heat2.figlist.csv\")).")
    fig.list <- plotutil_figlist_legacy(fig.list, FN)
    FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet.header", "facet.plot", "facet.set",
                           "all.dectections", "40khzmyo", "facet.label", "plot.set",
                           "date.start", "date.end")
    fl.canon <- canonicalize.headers(fig.list, FIG.LIST.REQUIRED)
    if (length(fl.canon$missing) > 0) stop("fig.list is missing these headers: ", paste(fl.canon$missing, collapse = ", "))
    fig.list <- canonicalize.headers(fl.canon$df, c(arg.names, names(OPT.DEFAULTS)))$df
    rows <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
    rows <- rows[!duplicated(rows), , drop = FALSE]
    jobs <- list()
    for (j in seq_len(nrow(rows))) {
      if (!identical(tolower(trimws(rows$plot.type[j])), PLOT.TYPE)) {
        cat(sprintf("NOTE: %s - fig.list row %d has plot.type = '%s' - skipped (this function only draws plot.type = '%s').\n",
                    FN, j, rows$plot.type[j], PLOT.TYPE))
        next
      }
      jobs[[length(jobs) + 1]] <- list(job = rows[j, , drop = FALSE], mode = "figlist", row = j)
    }
    if (length(jobs) == 0) stop("fig.list has no plot.type = \"", PLOT.TYPE, "\" rows - nothing to plot.")
  }

  dir.create(dir.save, showWarnings = FALSE, recursive = TRUE)
  all.binned <- list()
  plots <- list()
  files <- character(0)

  for (jb in jobs) {
    job <- jb$job
    job.label <- if (is.null(job)) "data" else if (nzchar(trimws(job$plot.name))) trimws(job$plot.name) else sprintf("row %d", jb$row)

    ## ---- resolve this job's settings
    date.col.j <- setting(job, "date.col")
    time.col.j <- setting(job, "time.col")
    night.mode <- grepl("monitoringnight", date.col.j, ignore.case = TRUE)
    if (!night.mode && !grepl("calend|calander", date.col.j, ignore.case = TRUE)) {
      cat("NOTE: date.col = \"", date.col.j, "\" is not a monitoring-night column - using the calendar-day ",
          "layout (midnight to midnight).\n", sep = "")
    }
    day.offset <- if (night.mode) 720 else 0
    bin.int.j <- setting(job, "bin.intervals")
    bin.mins <- heatmap2.parse.interval(bin.int.j, "bin.intervals")
    if (1440 %% bin.mins != 0) {
      stop("`bin.intervals` = \"", bin.int.j, "\" must divide evenly into 24 hours (e.g. \"15 min\", \"30 min\", \"1 hour\").")
    }
    label.mins <- heatmap2.parse.interval(setting(job, "time.interval"), "time.interval")
    bin.vals <- heatmap2.parse.values(setting(job, "bin.values"))
    scale.mode <- tolower(trimws(setting(job, "bin.scale")))
    if (!scale.mode %in% c("discrete", "continuous")) {
      stop("`bin.scale` = \"", scale.mode, "\" must be \"discrete\" or \"continuous\".")
    }
    if (scale.mode == "continuous" && length(bin.vals) < 2) {
      stop("`bin.scale = \"continuous\"` needs at least two `bin.values` (the bottom and top of the scale).")
    }
    bin.cols <- heatmap2.resolve.colors(setting(job, "bin.colors"),
                                        n = if (scale.mode == "discrete") length(bin.vals) + 1 else NA,
                                        scale.mode = scale.mode, fn.name = FN)
    t.start <- setting(job, "time.start"); t.end <- setting(job, "time.end")
    y.start <- (heatmap2.parse.clock(t.start, "time.start") - day.offset) %% 1440
    y.end   <- (heatmap2.parse.clock(t.end, "time.end") - day.offset) %% 1440
    if (y.end <= y.start) {
      if (y.end == 0) {
        y.end <- 1440
      } else {
        cat(sprintf("NOTE: time.end (%s) is not after time.start (%s) within a %s day - plotting the full day instead.\n",
                    t.end, t.start, if (night.mode) "noon-to-noon" else "midnight-to-midnight"))
        y.start <- 0; y.end <- 1440
      }
    }

    ## ---- records for this job's date/time columns
    rec <- heatmap2.records(data, std, count.idx[1], group.idx, spp.idx, date.col.j, time.col.j,
                            night.mode, day.offset, bin.mins)
    if (is.null(rec)) next

    ## ---- which plots/facets this job draws
    if (jb$mode == "legacy") {
      combos <- unique(rec[, c("group", "spp.id")])
      combos <- combos[order(combos$group, combos$spp.id), , drop = FALSE]
      panels <- lapply(seq_len(nrow(combos)), function(k) {
        sub <- rec[rec$group == combos$group[k] & rec$spp.id == combos$spp.id[k], , drop = FALSE]
        sub$facet <- combos$spp.id[k]
        list(rec = sub, group = combos$group[k], spp = combos$spp.id[k])
      })
      plot.units <- lapply(panels, function(pn) list(rec = pn$rec, facets = pn$spp, faceted = FALSE,
                                                     title = paste(pn$group, "-", pn$spp),
                                                     fname = sprintf("%s_%s_%s_%sto%s.png",
                                                                     heatmap2.file.token(project.name), heatmap2.file.token(pn$group),
                                                                     heatmap2.file.token(pn$spp), format(min(pn$rec$date), "%Y%m%d"),
                                                                     format(max(pn$rec$date), "%Y%m%d")),
                                                     d.start = min(pn$rec$date), d.end = max(pn$rec$date)))
    } else {
      if (!identical(tolower(trimws(job$facet.header)), "sppid")) {
        cat(sprintf("NOTE: %s - '%s' has facet.header = '%s' - skipped ($facet.header = \"sppid\" is the only value implemented so far).\n",
                    FN, job.label, job$facet.header))
        next
      }
      faceted <- !identical(toupper(trimws(as.character(job$facet.plot))), "FALSE")
      facets <- plotutil_resolve_facet_set(job$facet.set, job.label, FN)
      if (isTRUE(as.logical(job$all.dectections)) && !("All detections" %in% facets)) facets <- c("All detections", facets)
      if (isTRUE(as.logical(job[["40khzmyo"]])) && !("40khzmyo" %in% facets)) facets <- c(facets, "40khzmyo")

      ## plot.set -> $group value(s)
      sets <- trimws(strsplit(as.character(job$plot.set), "[;,]")[[1]])
      sets <- sets[!is.na(sets) & nzchar(sets)]
      if (length(sets) > 0) {
        rec <- rec[tolower(rec$group) %in% tolower(sets), , drop = FALSE]
        if (length(sets) > 1) cat(sprintf("NOTE: %s - '%s' plot.set has %d groups - their counts are summed.\n", FN, job.label, length(sets)))
      }
      blank <- function(v) length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v)))
      d.start <- if (blank(job$date.start)) NA else heatmap2.parse.date(job$date.start, "date.start")
      d.end <- if (blank(job$date.end)) NA else heatmap2.parse.date(job$date.end, "date.end")
      if (is.na(d.start)) d.start <- min(rec$date)
      if (is.na(d.end)) d.end <- max(rec$date)
      rec <- rec[rec$date >= d.start & rec$date <= d.end, , drop = FALSE]

      ## species of each record, as facet names
      simple <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))
      rec.spp <- simple(rec$spp.id)
      is.alldet <- rec.spp %in% c("alldetections", "alldet")
      is.khz <- rec.spp %in% c("40khzmyo", "40kmyo")
      rec.common <- rep(NA_character_, nrow(rec))
      if (any(!is.alldet & !is.khz)) {
        utils::capture.output(rc <- batz.batusa_recode.names(rec$spp.id[!is.alldet & !is.khz], batname.format.out = "common_name"))
        rec.common[!is.alldet & !is.khz] <- rc
      }
      facet.recs <- list()
      for (fc in facets) {
        sf <- simple(fc)
        if (sf == "alldetections") {
          sub <- rec[is.alldet, , drop = FALSE]
          if (nrow(sub) == 0 && nrow(rec) > 0) {
            sub <- rec[!is.khz, , drop = FALSE]
            cat(sprintf("NOTE: %s - '%s' data has no \"All Detections\" rows - All detections = every species row summed.\n", FN, job.label))
          }
        } else if (sf == "40khzmyo") {
          sub <- rec[is.khz, , drop = FALSE]
        } else {
          sub <- rec[!is.na(rec.common) & simple(rec.common) == sf, , drop = FALSE]
        }
        facet.recs[[fc]] <- sub
      }
      labs <- plotutil_facet_labels(facets, setting(job, "facet.label"), job.label, FN)
      ids <- plotutil_facet_id(facets, "sppid")
      set.tok <- if (length(sets) > 0) sets else ""
      if (faceted) {
        allrec <- do.call(rbind, lapply(facets, function(fc) {
          s <- facet.recs[[fc]]; if (nrow(s) == 0) return(NULL); s$facet <- labs[[fc]]; s }))
        if (is.null(allrec)) {
          cat(sprintf("NOTE: %s - '%s' matched 0 rows of data - no plot.\n", FN, job.label)); next
        }
        plot.units <- list(list(rec = allrec, facets = unname(labs[facets]), faceted = TRUE, title = job.label,
                                fname = plotutil_figlist_filename(job$plot.name, ids, TRUE, set.tok, d.start, d.end, fn.name = FN),
                                d.start = d.start, d.end = d.end))
      } else {
        plot.units <- list()
        for (k in seq_along(facets)) {
          s <- facet.recs[[facets[k]]]
          if (nrow(s) == 0) {
            cat(sprintf("NOTE: %s - '%s' facet '%s' has no data in range - no plot for it.\n", FN, job.label, labs[[facets[k]]]))
            next
          }
          s$facet <- labs[[facets[k]]]
          plot.units[[length(plot.units) + 1]] <- list(
            rec = s, facets = unname(labs[[facets[k]]]), faceted = FALSE, title = job.label,
            fname = plotutil_figlist_filename(job$plot.name, ids[k], FALSE, set.tok, d.start, d.end, fn.name = FN),
            d.start = d.start, d.end = d.end)
        }
      }
    }

    ## ---- draw and save each plot
    for (pu in plot.units) {
      out <- heatmap2.draw(pu, job = job, setting = setting, num.opt = num.opt, bin.mins = bin.mins,
                           bin.vals = bin.vals, bin.cols = bin.cols, scale.mode = scale.mode,
                           y.start = y.start, y.end = y.end, label.mins = label.mins,
                           day.offset = day.offset, night.mode = night.mode, fn.name = FN,
                           t.start = t.start, t.end = t.end)
      fname <- file.path(dir.save, pu$fname)
      n.rows <- if (pu$faceted) ceiling(length(pu$facets) / max(1, num.opt(job, "facpan.numcol"))) else 1
      height <- if (pu$faceted && n.rows > 1) num.opt(job, "facpan.hgt") * n.rows + 1.5 else num.opt(job, "plot.height")
      ggplot2::ggsave(fname, plot = out$plot, width = num.opt(job, "plot.width"), height = height,
                      units = setting(job, "ggsave.units"), dpi = num.opt(job, "ggsave.dpi"))
      cat("Saved:", fname, "\n")
      key <- sub("\\.png$", "", basename(fname))
      plots[[key]] <- out$plot
      out$grid$plot <- key
      all.binned[[length(all.binned) + 1]] <- out$grid
      files <- c(files, fname)
    }
  }

  binned <- if (length(all.binned) > 0) do.call(rbind, all.binned) else NULL
  if (!is.null(binned)) rownames(binned) <- NULL
  invisible(list(data = binned, plots = plots, files = files))
}


#' Build per-record bins for batz.plotactivity_heatmap2() (internal)
#'
#' Reads the date, time, count, group and spp.id columns of \code{data},
#' converts monitoring-night/calendar dates to the layout in use, drops
#' unusable rows (with a NOTE) and returns one row per record with its
#' time \code{$bin} (minutes from the day start). \code{NULL} if nothing is
#' usable.
#' @keywords internal
#' @noRd
heatmap2.records <- function(data, std, count.idx, group.idx, spp.idx, date.col, time.col,
                             night.mode, day.offset, bin.mins) {
  counts.raw <- suppressWarnings(as.numeric(data[[count.idx]]))

  date.idx <- match(standardize.headers(date.col), std)
  if (!is.na(date.idx)) {
    source.night <- night.mode
  } else {
    gbd.idx <- match("groupby_date", std)
    d.idx <- match("date", std)
    if (is.na(gbd.idx) || is.na(d.idx)) {
      stop("`data` has no `", date.col, "` column (and no $date + $groupby.date to use instead).")
    }
    gbd <- unique(as.character(data[[gbd.idx]]))
    gbd.night <- grepl("monitoringnight", gbd, ignore.case = TRUE)
    if (length(unique(gbd.night)) > 1) stop("`data`'s $groupby.date mixes monitoring-night and calendar dates.")
    date.idx <- d.idx
    source.night <- gbd.night[1]
  }
  dates <- heatmap2.parse.date(data[[date.idx]], names(data)[date.idx])

  time.idx <- match(standardize.headers(time.col), std)
  if (is.na(time.idx)) stop("`data` has no `", time.col, "` column.")
  clock <- heatmap2.time.of.day(data[[time.idx]])

  ## monitoring-night dates <-> calendar dates: a record before noon
  ## belongs to the next calendar day after its monitoring night
  if (source.night && !night.mode) {
    dates <- dates + ifelse(!is.na(clock) & clock < 720, 1, 0)
    cat("NOTE: dates are monitoring nights - records before noon moved to the next calendar day for the calendar layout.\n")
  } else if (!source.night && night.mode) {
    dates <- dates - ifelse(!is.na(clock) & clock < 720, 1, 0)
    cat("NOTE: dates are calendar days - records before noon moved to the previous monitoring night.\n")
  }

  groups <- if (!is.na(group.idx)) as.character(data[[group.idx]]) else rep("allgroups", nrow(data))
  spps   <- if (!is.na(spp.idx)) as.character(data[[spp.idx]]) else rep("allspp", nrow(data))

  bad <- is.na(dates) | is.na(clock) | is.na(counts.raw)
  if (any(bad)) cat(sprintf("NOTE: %d row(s) had an unreadable date, time or count - dropped.\n", sum(bad)))
  keep <- !bad
  if (!any(keep)) {
    cat("NOTE: `data` has no rows with a usable date, time and count - nothing to plot.\n")
    return(NULL)
  }

  if ("pool.interval" %in% names(data)) {
    pool.lab <- unique(as.character(data$pool.interval[keep]))
    pool.mins <- suppressWarnings(vapply(pool.lab, function(p) tryCatch(heatmap2.parse.interval(p, "pool.interval"),
                                                                        error = function(e) NA_real_), numeric(1)))
    if (any(!is.na(pool.mins) & pool.mins > bin.mins)) {
      cat(sprintf("NOTE: data was pooled at %s, coarser than bin.intervals (%g min) - each pool's count goes in the bin its start time falls in.\n",
                  paste(pool.lab, collapse = ", "), bin.mins))
    }
  }

  pos <- (clock[keep] - day.offset) %% 1440
  data.frame(group = groups[keep], spp.id = spps[keep], date = dates[keep],
             bin = floor(pos / bin.mins) * bin.mins, count = counts.raw[keep],
             stringsAsFactors = FALSE)
}


#' Draw one heatmap for batz.plotactivity_heatmap2() (internal)
#'
#' \code{pu$rec} holds the records (with \code{$facet}) and
#' \code{pu$facets} the facet labels in order. Fills a full date x time-bin
#' grid per facet (empty = 0) and returns \code{list(plot, grid)}.
#' @keywords internal
#' @noRd
heatmap2.draw <- function(pu, job, setting, num.opt, bin.mins, bin.vals, bin.cols, scale.mode,
                          y.start, y.end, label.mins, day.offset, night.mode, fn.name, t.start, t.end) {
  rec <- pu$rec
  all.dates <- seq(pu$d.start, pu$d.end, by = "day")
  all.bins <- seq(0, 1440 - bin.mins, by = bin.mins)
  grids <- lapply(pu$facets, function(fc) {
    sub <- rec[rec$facet == fc, , drop = FALSE]
    g <- expand.grid(date = all.dates, bin = all.bins)
    if (nrow(sub) > 0) {
      agg <- stats::aggregate(count ~ date + bin, data = sub, FUN = sum)
      g <- merge(g, agg, by = c("date", "bin"), all.x = TRUE)
    } else {
      g$count <- NA_real_
    }
    g$count[is.na(g$count)] <- 0
    g$facet <- fc
    g
  })
  grid <- do.call(rbind, grids)
  n.outside <- sum(rec$count[rec$bin + bin.mins <= y.start | rec$bin >= y.end])
  if (n.outside > 0) {
    cat(sprintf("NOTE: %s - '%s': %s observation(s) fall outside time.start-time.end (%s-%s) and are not shown.\n",
                fn.name, pu$title, format(n.outside), t.start, t.end))
  }
  grid <- grid[grid$bin + bin.mins > y.start & grid$bin < y.end, , drop = FALSE]
  grid$y <- grid$bin + bin.mins / 2
  grid$facet <- factor(grid$facet, levels = pu$facets)
  grid <- grid[order(grid$facet, grid$date, grid$bin), , drop = FALSE]

  ## ---- fill scale
  top.label <- paste0(format(floor(max(bin.vals)) + 1, trim = TRUE), "+")
  if (scale.mode == "discrete") {
    bin.labels <- character(length(bin.vals) + 1)
    for (i in seq_along(bin.vals)) {
      if (i == 1) {
        bin.labels[i] <- format(bin.vals[i], trim = TRUE)
      } else {
        lo <- floor(bin.vals[i - 1]) + 1
        bin.labels[i] <- if (lo >= bin.vals[i]) format(bin.vals[i], trim = TRUE) else
          paste0(format(lo, trim = TRUE), "-", format(bin.vals[i], trim = TRUE))
      }
    }
    bin.labels[length(bin.labels)] <- top.label
    grid$count.bin <- factor(bin.labels[findInterval(grid$count, bin.vals, left.open = TRUE) + 1], levels = bin.labels)
    ## every bin keeps its colour in the legend, used or not (2026-10-09 fix:
    ## ggplot2 >= 3.5 leaves unused legend keys blank unless the layer has
    ## show.legend = TRUE)
    fill.aes <- ggplot2::aes(fill = count.bin)
    fill.scale <- ggplot2::scale_fill_manual(values = stats::setNames(bin.cols, bin.labels), limits = bin.labels,
                                             drop = FALSE, name = setting(job, "legend.title"))
    fill.guide <- ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5,
                                                               override.aes = list(fill = unname(bin.cols))))
  } else {
    lim <- range(bin.vals)
    brk.labels <- format(bin.vals, trim = TRUE)
    brk.labels[length(brk.labels)] <- paste0(format(max(bin.vals), trim = TRUE), "+")
    fill.aes <- ggplot2::aes(fill = count)
    fill.scale <- ggplot2::scale_fill_gradientn(colours = bin.cols, limits = lim, breaks = bin.vals, labels = brk.labels,
                                                oob = function(x, range = c(0, 1), only.finite = TRUE) pmin(pmax(x, range[1]), range[2]),
                                                name = setting(job, "legend.title"))
    fill.guide <- ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = 0.5,
                                                                  barwidth = grid::unit(12, "lines")))
  }

  ## ---- axes
  clock.label <- function(m) {
    m <- (m + day.offset) %% 1440
    sprintf("%02d:%02d", m %/% 60, m %% 60)
  }
  y.breaks <- seq(y.start, y.end, by = label.mins)
  y.labels <- clock.label(y.breaks)
  if (y.end == 1440 && !night.mode) y.labels[y.breaks == 1440] <- "24:00"
  date.fmt <- gsub("/n", "\n", setting(job, "date.format"), fixed = TRUE)
  n.x <- suppressWarnings(as.numeric(setting(job, "xaxe.interval")))
  x.scale <- if (!is.na(n.x) && n.x >= 2) {
    ggplot2::scale_x_date(breaks = seq(pu$d.start, pu$d.end, length.out = round(n.x)), date_labels = date.fmt)
  } else {
    ggplot2::scale_x_date(date_labels = date.fmt)
  }
  x.title <- setting(job, "xaxe.title")
  if (!nzchar(x.title)) x.title <- if (night.mode) "Monitoring Night" else "Date"

  p <- ggplot2::ggplot(grid, ggplot2::aes(x = date, y = y)) +
    ggplot2::geom_tile(fill.aes, width = 1, height = bin.mins, show.legend = TRUE) +
    fill.scale + x.scale +
    ggplot2::scale_y_continuous(breaks = y.breaks, labels = y.labels) +
    ggplot2::coord_cartesian(ylim = c(y.start, y.end), expand = FALSE) +
    ggplot2::labs(x = x.title, y = setting(job, "yaxe.title"), title = pu$title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.border     = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
      legend.position  = setting(job, "legend.position"),
      legend.key       = ggplot2::element_rect(color = "grey60", linewidth = 0.3),
      plot.title       = ggplot2::element_text(size = num.opt(job, "plot.title.size")),
      axis.title       = ggplot2::element_text(size = num.opt(job, "axis.title.size")),
      axis.text        = ggplot2::element_text(size = num.opt(job, "axis.text.size")),
      strip.text       = ggplot2::element_text(size = num.opt(job, "strip.text.size")),
      legend.title     = ggplot2::element_text(size = num.opt(job, "legend.title.size")),
      legend.text      = ggplot2::element_text(size = num.opt(job, "legend.text.size"))
    ) +
    fill.guide
  if (720 > y.start && 720 < y.end) {
    p <- p + ggplot2::geom_hline(yintercept = 720, color = setting(job, "midline.color"),
                                 linewidth = num.opt(job, "midline.linewidth"))
  }
  if (isTRUE(pu$faceted)) {
    p <- p + ggplot2::facet_wrap(~facet, ncol = max(1, num.opt(job, "facpan.numcol")), drop = FALSE)
  }

  grid$time <- clock.label(grid$bin)
  out.grid <- data.frame(facet = as.character(grid$facet), date = grid$date, time = grid$time,
                         observations.count = grid$count, stringsAsFactors = FALSE)
  out.grid$count.bin <- if (scale.mode == "discrete") as.character(grid$count.bin) else NA_character_
  list(plot = p, grid = out.grid)
}


#' bin.values from a number vector or "0;1;10;25;50" text (internal)
#' @keywords internal
#' @noRd
heatmap2.parse.values <- function(x) {
  v <- if (is.numeric(x)) x else suppressWarnings(as.numeric(trimws(strsplit(paste(x, collapse = ";"), "[;,]")[[1]])))
  if (length(v) < 1 || any(is.na(v)) || is.unsorted(v, strictly = TRUE)) {
    stop("`bin.values` must be numbers in increasing order, e.g. c(0, 1, 10, 25, 50) or \"0;1;10;25;50\".")
  }
  v
}


#' Known palette names, by package (internal)
#'
#' Used to find which package a bare palette name belongs to (and to spot
#' clashes). Installed packages are asked for their own current list;
#' these built-in lists are only used when a package isn't installed, so
#' the NOTE can say which package to install.
#' @keywords internal
#' @noRd
heatmap2.palette.names <- function() {
  scico.names <- if (requireNamespace("scico", quietly = TRUE)) scico::scico_palette_names() else
    c("acton", "bam", "bamako", "bamO", "batlow", "batlowK", "batlowW", "berlin", "bilbao", "broc", "brocO",
      "buda", "bukavu", "cork", "corkO", "davos", "devon", "fes", "glasgow", "grayC", "hawaii", "imola",
      "lajolla", "lapaz", "lipari", "lisbon", "managua", "navia", "naviaW", "nuuk", "oleron", "oslo",
      "roma", "romaO", "tofino", "tokyo", "turku", "vanimo", "vik", "vikO")
  brewer.names <- if (requireNamespace("RColorBrewer", quietly = TRUE)) rownames(RColorBrewer::brewer.pal.info) else
    c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2",
      "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens",
      "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd")
  cs.names <- if (requireNamespace("colorspace", quietly = TRUE)) rownames(colorspace::hcl_palettes()) else character(0)
  list(viridis = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
       scico = scico.names, RColorBrewer = brewer.names, `Okabe-Ito` = "Okabe-Ito", colorspace = cs.names)
}


#' Turn bin.colors into a colour vector (internal)
#'
#' \code{x}: colours (vector, or one string separated by ";") or one
#' palette name, optionally "pkg::name" and/or a leading "-" (reverse).
#' \code{n}: colours needed (discrete); \code{NA} for continuous (any
#' number >= 2 of given colours is used as-is; a palette gives 9).
#' @keywords internal
#' @noRd
heatmap2.resolve.colors <- function(x, n, scale.mode, fn.name) {
  x <- as.character(x)
  if (length(x) == 1) x <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  x <- x[nzchar(x)]
  is.color <- function(v) vapply(v, function(cc) !inherits(try(grDevices::col2rgb(cc), silent = TRUE), "try-error"), logical(1))

  if (length(x) == 1 && !is.color(x)) {
    ## ---- a palette name
    spec <- x
    rev.flag <- startsWith(spec, "-")
    if (rev.flag) spec <- substring(spec, 2)
    pkg <- NA_character_
    if (grepl("::", spec, fixed = TRUE)) {
      pkg <- sub("::.*$", "", spec)
      spec <- sub("^.*::", "", spec)
    }
    simple <- function(v) gsub("[^a-z0-9]", "", tolower(v))
    pkg.alias <- c(viridis = "viridis", viridislite = "viridis", scico = "scico", rcolorbrewer = "RColorBrewer",
                   brewer = "RColorBrewer", colorspace = "colorspace", okabeito = "Okabe-Ito", grdevices = "Okabe-Ito")
    known <- heatmap2.palette.names()
    hits <- names(known)[vapply(known, function(nm) simple(spec) %in% simple(nm), logical(1))]
    if (!is.na(pkg)) {
      p2 <- pkg.alias[simple(pkg)]
      if (is.na(p2)) stop(sprintf("%s: bin.colors = '%s' - '%s' is not one of viridis, scico, RColorBrewer, colorspace, Okabe-Ito.", fn.name, x, pkg))
      if (p2 == "colorspace" && !requireNamespace("colorspace", quietly = TRUE)) {
        stop(sprintf("%s: bin.colors palette '%s' needs the colorspace package - install.packages(\"colorspace\").", fn.name, x))
      }
      if (!(p2 %in% hits)) stop(sprintf("%s: bin.colors = '%s' - no palette '%s' in %s.", fn.name, x, spec, p2))
      use <- p2
    } else {
      if (length(hits) == 0) {
        stop(sprintf("%s: bin.colors = '%s' is not a colour or a known palette (viridis, scico, RColorBrewer, Okabe-Ito, colorspace).", fn.name, x))
      }
      use <- hits[1]
      if (length(hits) > 1) {
        cat(sprintf("NOTE: %s - palette '%s' is in %s; used %s. Write e.g. '%s::%s' to pick another.\n",
                    fn.name, spec, paste(hits, collapse = " and "), use, hits[2], spec))
      }
    }
    name <- known[[use]][simple(known[[use]]) == simple(spec)][1]
    if (is.na(name)) name <- spec
    k <- if (is.na(n)) 9 else n
    need <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("%s: bin.colors palette '%s' needs the %s package - install.packages(\"%s\").", fn.name, x, p, p))
    cols <- switch(use,
      viridis = viridisLite::viridis(k, option = name),
      scico = { need("scico"); scico::scico(k, palette = name) },
      RColorBrewer = {
        need("RColorBrewer")
        mx <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
        base <- RColorBrewer::brewer.pal(max(3, min(k, mx)), name)
        if (k > mx) grDevices::colorRampPalette(base)(k) else base[seq_len(k)]
      },
      `Okabe-Ito` = {
        base <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
        if (k > length(base)) {
          cat(sprintf("NOTE: %s - Okabe-Ito has %d colours; %d needed - colours were blended to fill.\n", fn.name, length(base), k))
          grDevices::colorRampPalette(base)(k)
        } else base[seq_len(k)]
      },
      colorspace = {
        need("colorspace")
        type <- tolower(as.character(colorspace::hcl_palettes()[name, "type"]))
        if (grepl("qualitative", type)) colorspace::qualitative_hcl(k, palette = name)
        else if (grepl("flexible", type)) colorspace::divergingx_hcl(k, palette = name)
        else if (grepl("diverging", type)) colorspace::diverging_hcl(k, palette = name)
        else colorspace::sequential_hcl(k, palette = name)
      })
    cols <- unname(as.character(cols))
    if (rev.flag) cols <- rev(cols)
    return(cols)
  }

  ## ---- a list of colours
  bad <- x[!is.color(x)]
  if (length(bad) > 0) stop(sprintf("%s: bin.colors has value(s) that aren't colours: %s.", fn.name, paste(bad, collapse = ", ")))
  if (!is.na(n) && length(x) != n) {
    stop(sprintf("%s: bin.colors needs %d colours for bin.scale = \"discrete\" (one more than bin.values, for counts above the top value) - got %d.",
                 fn.name, n, length(x)))
  }
  if (is.na(n) && length(x) < 2) stop(sprintf("%s: bin.scale = \"continuous\" needs at least 2 colours (or a palette name).", fn.name))
  x
}


#' Helpers for batz.plotactivity_heatmap2() (internal)
#'
#' Added 2026-10-07. \code{heatmap2.parse.interval()} turns \code{"15 min"},
#' \code{"1 hour"} etc. into minutes; \code{heatmap2.parse.clock()} turns
#' \code{"15:00"} into minutes from midnight; \code{heatmap2.time.of.day()}
#' pulls the time of day out of a date-time or time value;
#' \code{heatmap2.parse.date()} reads a date column in one of the common
#' formats; \code{heatmap2.file.token()} makes a value safe for a file name.
#' @keywords internal
#' @noRd
heatmap2.parse.interval <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) stop("`", arg, "` must be one text value, e.g. \"15 min\" or \"1 hour\".")
  s <- tolower(gsub("\\s+", "", x))
  m <- regmatches(s, regexec("^([0-9]*\\.?[0-9]+)?(minutes|minute|mins|min|m|hours|hour|hrs|hr|h)$", s))[[1]]
  if (length(m) == 0) stop("`", arg, "` = \"", x, "\" not recognized - use a number and minutes or hours, e.g. \"15 min\" or \"1 hour\".")
  n <- if (nzchar(m[2])) as.numeric(m[2]) else 1
  mins <- n * if (substr(m[3], 1, 1) == "h") 60 else 1
  if (!is.finite(mins) || mins < 1 || abs(mins - round(mins)) > 1e-9 || mins > 1440) {
    stop("`", arg, "` = \"", x, "\" must be a whole number of minutes, from 1 minute to 24 hours.")
  }
  mins
}

#' @keywords internal
#' @noRd
heatmap2.parse.clock <- function(x, arg) {
  m <- regmatches(trimws(x), regexec("^([0-9]{1,2}):([0-9]{2})(:([0-9]{2}))?$", trimws(x)))[[1]]
  if (length(m) == 0) stop("`", arg, "` = \"", x, "\" must be a time like \"15:00\".")
  h <- as.numeric(m[2]); mi <- as.numeric(m[3])
  if (h > 24 || mi > 59 || (h == 24 && mi > 0)) stop("`", arg, "` = \"", x, "\" is not a valid time.")
  h * 60 + mi
}

#' @keywords internal
#' @noRd
heatmap2.time.of.day <- function(x) {
  x <- trimws(as.character(x))
  m <- regmatches(x, regexec("(^|[ T])([0-9]{1,2}):([0-9]{2})(:([0-9]{2}))?\\s*([AaPp][Mm])?", x))
  vapply(m, function(p) {
    if (length(p) == 0) return(NA_real_)
    h <- as.numeric(p[3]); mi <- as.numeric(p[4]); sec <- if (nzchar(p[6])) as.numeric(p[6]) else 0
    ampm <- tolower(p[7])
    if (ampm == "pm" && h < 12) h <- h + 12
    if (ampm == "am" && h == 12) h <- 0
    if (h > 23 || mi > 59) return(NA_real_)
    h * 60 + mi + sec / 60
  }, numeric(1))
}

#' @keywords internal
#' @noRd
heatmap2.parse.date <- function(x, label) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(format(x, "%Y-%m-%d")))
  x <- trimws(as.character(x))
  x <- sub("[ T].*$", "", x)                 # drop any time part
  present <- x[!is.na(x) & nzchar(x)]
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d", "%m/%d/%y", "%Y%m%d")) {
    d <- as.Date(present, format = fmt)
    if (length(present) > 0 && !any(is.na(d)) && all(as.numeric(format(d, "%Y")) > 1900)) {
      return(as.Date(x, format = fmt))
    }
  }
  stop("Could not read the dates in $", label, " - expected e.g. 2026-08-14 or 8/14/2026.")
}

#' @keywords internal
#' @noRd
heatmap2.file.token <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "-", trimws(as.character(x)))
  if (!nzchar(x)) "NA" else x
}

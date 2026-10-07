#' Heatmap of bat activity by date and time of day (v2)
#'
#' Added 2026-10-07, per Josh. Draws a ggplot2 heatmap of observation
#' counts - \code{$observations.count} (or the older \code{$obs}) - with
#' date on the X axis and time of day on the Y axis, using fixed count
#' bins and one colour per bin. Built for the output of
#' \code{\link{batz.generate_plotframe.bat}} (e.g. with
#' \code{pool.interval = "15 min"}), but works on any data frame with a
#' date column, a time column and a count column. One PNG is saved per
#' \code{$group} x \code{$spp.id} combination in \code{data}.
#'
#' \strong{Day layout.} With \code{date.col = "date.monitoringnight"} the
#' Y axis runs from noon on one calendar day to noon the next, and a solid
#' black line marks 00:00:00 (midnight). With \code{date.col =
#' "date.calendar"} (\code{"date.calander"} is also accepted) the Y axis
#' runs from midnight to midnight, and the black line marks 12:00:00
#' (noon). Time runs upward from \code{time.start} (bottom) to
#' \code{time.end} (top), like \code{\link{batz.plotactivity_heatmap}}.
#'
#' \strong{Date column.} If \code{data} has a column named
#' \code{date.col} it is used. Otherwise, for a
#' \code{batz.generate_plotframe.bat()} frame (which has \code{$date} and a
#' \code{$groupby.date} naming the date type), \code{$date} is used when
#' \code{$groupby.date} matches \code{date.col}.
#'
#' \strong{Time column.} Only the time of day is read from
#' \code{time.col} - e.g. \code{"2026-08-14 21:15:00"}, \code{"21:15"},
#' \code{"21:15:00"} or \code{"9:15 PM"}. Each record goes in the bin its
#' time falls in, and counts in the same date/bin are summed. Bins with no
#' records count as 0.
#'
#' \strong{Count bins.} \code{bin.values} are the upper limits of each
#' bin. With the default \code{c(0, 1, 10, 25, 50)} the bins are 0, 1,
#' 2-10, 11-25, 26-50 and 51+, so \code{bin.colors} needs one more colour
#' than \code{bin.values} (the last colour is for counts above the highest
#' value).
#'
#' \strong{File name.}
#' \verb{<project.name>_<group>_<spp.id>_<date.min>to<date.max>.png},
#' with dates as YYYYMMDD from the plotted data. If \code{data} has no
#' \code{$group} or \code{$spp.id} column, \code{"allgroups"} /
#' \code{"allspp"} is used.
#'
#' @param data Data frame to plot. Needs \code{date.col} (see Details),
#'   \code{time.col} and \code{$observations.count} (or \code{$obs}).
#'   Optional \code{$group} and \code{$spp.id} split the output into one
#'   plot per combination.
#' @param date.col Character, default \code{"date.monitoringnight"}. Date
#'   column, and the day layout: \code{"date.monitoringnight"} (noon to
#'   noon) or \code{"date.calendar"} (midnight to midnight).
#' @param time.col Character, default \code{"pool.start"}. Column holding
#'   each record's time of day.
#' @param bin.intervals Character, default \code{"15 min"}. Height of each
#'   time bin, e.g. \code{"15 min"}, \code{"30 min"}, \code{"1 hour"}. Must
#'   divide evenly into 24 hours.
#' @param bin.values Numeric, default \code{c(0, 1, 10, 25, 50)}. Upper
#'   limit of each count bin, in increasing order. See Details.
#' @param bin.colors Character, default \code{c("white", "#FFFF80",
#'   "#FFFF00", "#FFAA00", "#FF5500", "#FF0000")}. One colour per count
#'   bin - \code{length(bin.values) + 1} colours.
#' @param time.start Character, default \code{"15:00"}. Time at the
#'   bottom of the Y axis (HH:MM).
#' @param time.end Character, default \code{"08:00"}. Time at the top of
#'   the Y axis (HH:MM). With \code{"date.monitoringnight"} it may be after
#'   midnight (e.g. \code{"15:00"} to \code{"08:00"}).
#' @param time.interval Character, default \code{"1 hour"}. Spacing of the
#'   Y-axis labels.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of the saved file name.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG(s)
#'   are saved in.
#'
#' @return Invisibly, a list: \code{$data} (the binned counts, one row per
#'   group/spp.id/date/time bin, with \code{$count.bin}), \code{$plots}
#'   (named list of ggplot objects) and \code{$files} (saved file paths).
#'
#' @examples
#' \dontrun{
#' plfr <- batz.generate_plotframe.bat(vetted.merged, pool.interval = "15 min")
#' batz.plotactivity_heatmap2(plfr[plfr$spp.id == "lano", ],
#'   project.name = "Riverpoint", dir.save = "C:/path/to/figures")
#'
#' # calendar-day layout, 30-minute bins, labels every 2 hours
#' batz.plotactivity_heatmap2(plfr, date.col = "date.calendar",
#'   bin.intervals = "30 min", time.start = "00:00", time.end = "24:00",
#'   time.interval = "2 hour")
#' }
#'
#' @export
batz.plotactivity_heatmap2 <- function(data,
                                       date.col = "date.monitoringnight",
                                       time.col = "pool.start",
                                       bin.intervals = "15 min",
                                       bin.values = c(0, 1, 10, 25, 50),
                                       bin.colors = c("white", "#FFFF80", "#FFFF00", "#FFAA00", "#FF5500", "#FF0000"),
                                       time.start = "15:00",
                                       time.end = "08:00",
                                       time.interval = "1 hour",
                                       project.name = "new.project",
                                       dir.save = getwd()) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (nrow(data) == 0) stop("`data` has 0 rows - nothing to plot.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required - install.packages(\"ggplot2\").")

  ## ---- day layout: monitoring night (noon-noon) or calendar (midnight-midnight)
  night.mode <- grepl("monitoringnight", date.col, ignore.case = TRUE)
  if (!night.mode && !grepl("calend|calander", date.col, ignore.case = TRUE)) {
    cat("NOTE: date.col = \"", date.col, "\" is not a monitoring-night column - using the calendar-day ",
        "layout (midnight to midnight).\n", sep = "")
  }
  day.offset <- if (night.mode) 720 else 0   # minutes from midnight to the day start
  ref.line.label <- if (night.mode) "00:00" else "12:00"

  ## ---- intervals and bins
  bin.mins <- heatmap2.parse.interval(bin.intervals, "bin.intervals")
  if (1440 %% bin.mins != 0) {
    stop("`bin.intervals` = \"", bin.intervals, "\" must divide evenly into 24 hours (e.g. \"15 min\", \"30 min\", \"1 hour\").")
  }
  label.mins <- heatmap2.parse.interval(time.interval, "time.interval")

  if (!is.numeric(bin.values) || length(bin.values) < 1 || any(is.na(bin.values)) || is.unsorted(bin.values, strictly = TRUE)) {
    stop("`bin.values` must be numbers in increasing order, e.g. c(0, 1, 10, 25, 50).")
  }
  if (length(bin.colors) != length(bin.values) + 1) {
    stop(sprintf("`bin.colors` needs %d colours (one more than `bin.values`, for counts above %s) - got %d.",
                 length(bin.values) + 1, format(max(bin.values)), length(bin.colors)))
  }

  ## ---- Y range (minutes from the day start)
  y.start <- (heatmap2.parse.clock(time.start, "time.start") - day.offset) %% 1440
  y.end   <- (heatmap2.parse.clock(time.end, "time.end") - day.offset) %% 1440
  if (y.end <= y.start) {
    if (y.end == 0) {
      y.end <- 1440
    } else {
      cat(sprintf("NOTE: time.end (%s) is not after time.start (%s) within a %s day - plotting the full day instead.\n",
                  time.end, time.start, if (night.mode) "noon-to-noon" else "midnight-to-midnight"))
      y.start <- 0; y.end <- 1440
    }
  }

  ## ---- count column
  std <- standardize.headers(names(data))
  count.idx <- match(c("observations_count", "observation_count", "obs"), std)
  count.idx <- count.idx[!is.na(count.idx)]
  if (length(count.idx) == 0) stop("`data` needs an $observations.count (or $obs) column.")
  if (std[count.idx[1]] == "obs") {
    cat("WARNING: `data` has the old header $obs - used as $observations.count.\n")
  }
  counts.raw <- suppressWarnings(as.numeric(data[[count.idx[1]]]))

  ## ---- date column, and whether its dates are monitoring nights
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

  ## ---- time column
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

  ## ---- group / spp.id
  group.idx <- match("group", std)
  spp.idx <- match("spp_id", std)
  groups <- if (!is.na(group.idx)) as.character(data[[group.idx]]) else rep("allgroups", nrow(data))
  spps   <- if (!is.na(spp.idx)) as.character(data[[spp.idx]]) else rep("allspp", nrow(data))

  ## ---- drop unusable rows
  bad <- is.na(dates) | is.na(clock) | is.na(counts.raw)
  if (any(bad)) {
    cat(sprintf("NOTE: %d row(s) had an unreadable date, time or count - dropped.\n", sum(bad)))
  }
  keep <- !bad
  if (!any(keep)) stop("`data` has no rows with a usable date, time and count - nothing to plot.")

  pos <- (clock[keep] - day.offset) %% 1440           # minutes from day start
  rec <- data.frame(
    group = groups[keep], spp.id = spps[keep], date = dates[keep],
    bin = floor(pos / bin.mins) * bin.mins,
    count = counts.raw[keep],
    stringsAsFactors = FALSE
  )

  if ("pool.interval" %in% names(data)) {
    pool.lab <- unique(as.character(data$pool.interval[keep]))
    pool.mins <- suppressWarnings(vapply(pool.lab, function(p) tryCatch(heatmap2.parse.interval(p, "pool.interval"),
                                                                        error = function(e) NA_real_), numeric(1)))
    if (any(!is.na(pool.mins) & pool.mins > bin.mins)) {
      cat(sprintf("NOTE: data was pooled at %s, coarser than bin.intervals = \"%s\" - each pool's count goes in the bin its start time falls in.\n",
                  paste(pool.lab, collapse = ", "), bin.intervals))
    }
  }

  ## ---- bin labels: 0, 1, 2-10, 11-25, 26-50, 51+
  bin.labels <- character(length(bin.values) + 1)
  for (i in seq_along(bin.values)) {
    lower <- if (i == 1) NA else bin.values[i - 1]
    if (is.na(lower)) {
      bin.labels[i] <- format(bin.values[i], trim = TRUE)
    } else {
      lo <- floor(lower) + 1
      bin.labels[i] <- if (lo >= bin.values[i]) format(bin.values[i], trim = TRUE) else
        paste0(format(lo, trim = TRUE), "-", format(bin.values[i], trim = TRUE))
    }
  }
  bin.labels[length(bin.labels)] <- paste0(format(floor(max(bin.values)) + 1, trim = TRUE), "+")
  bin.of <- function(x) factor(bin.labels[findInterval(x, bin.values, left.open = TRUE) + 1], levels = bin.labels)

  ## ---- Y axis breaks/labels
  clock.label <- function(m) {
    m <- (m + day.offset) %% 1440
    sprintf("%02d:%02d", m %/% 60, m %% 60)
  }
  y.breaks <- seq(y.start, y.end, by = label.mins)
  y.labels <- clock.label(y.breaks)
  if (y.end == 1440 && !night.mode) y.labels[y.breaks == 1440] <- "24:00"

  dir.create(dir.save, showWarnings = FALSE, recursive = TRUE)
  combos <- unique(rec[, c("group", "spp.id")])
  combos <- combos[order(combos$group, combos$spp.id), , drop = FALSE]

  all.binned <- list()
  plots <- list()
  files <- character(0)

  for (k in seq_len(nrow(combos))) {
    g <- combos$group[k]; s <- combos$spp.id[k]
    sub <- rec[rec$group == g & rec$spp.id == s, , drop = FALSE]

    agg <- stats::aggregate(count ~ date + bin, data = sub, FUN = sum)
    all.dates <- seq(min(sub$date), max(sub$date), by = "day")
    all.bins <- seq(0, 1440 - bin.mins, by = bin.mins)
    grid <- expand.grid(date = all.dates, bin = all.bins)
    grid <- merge(grid, agg, by = c("date", "bin"), all.x = TRUE)
    grid$count[is.na(grid$count)] <- 0
    grid <- grid[grid$bin + bin.mins > y.start & grid$bin < y.end, , drop = FALSE]
    grid$count.bin <- bin.of(grid$count)
    grid$y <- grid$bin + bin.mins / 2
    grid <- grid[order(grid$date, grid$bin), , drop = FALSE]

    n.outside <- sum(sub$count[sub$bin + bin.mins <= y.start | sub$bin >= y.end])
    if (n.outside > 0) {
      cat(sprintf("NOTE: %s / %s - %s observation(s) fall outside time.start-time.end (%s-%s) and are not shown.\n",
                  g, s, format(n.outside), time.start, time.end))
    }

    p <- ggplot2::ggplot(grid, ggplot2::aes(x = date, y = y, fill = count.bin)) +
      ggplot2::geom_tile(width = 1, height = bin.mins) +
      ggplot2::scale_fill_manual(values = stats::setNames(bin.colors, bin.labels), drop = FALSE,
                                 name = "Number of Observations") +
      ggplot2::scale_x_date(date_labels = "%d %b") +
      ggplot2::scale_y_continuous(breaks = y.breaks, labels = y.labels) +
      ggplot2::coord_cartesian(ylim = c(y.start, y.end), expand = FALSE) +
      ggplot2::labs(x = if (night.mode) "Monitoring Night" else "Date", y = "Time of Day",
                    title = paste(g, "-", s)) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid       = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        panel.border     = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.5),
        axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.3),
        legend.position  = "bottom",
        legend.key       = ggplot2::element_rect(color = "grey60", linewidth = 0.3)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))

    if (720 > y.start && 720 < y.end) {
      p <- p + ggplot2::geom_hline(yintercept = 720, color = "black", linewidth = 0.6)
    }

    d.min <- format(min(sub$date), "%Y%m%d"); d.max <- format(max(sub$date), "%Y%m%d")
    fname <- file.path(dir.save, sprintf("%s_%s_%s_%sto%s.png",
                                          heatmap2.file.token(project.name), heatmap2.file.token(g),
                                          heatmap2.file.token(s), d.min, d.max))
    ggplot2::ggsave(fname, plot = p, width = 10, height = 6, units = "in", dpi = 300)
    cat("Saved:", fname, "\n")

    grid$group <- g; grid$spp.id <- s
    all.binned[[k]] <- grid[, c("group", "spp.id", "date", "bin", "count", "count.bin")]
    plots[[paste(g, s, sep = "_")]] <- p
    files <- c(files, fname)
  }

  binned <- do.call(rbind, all.binned)
  binned$time <- clock.label(binned$bin)
  binned <- binned[, c("group", "spp.id", "date", "time", "count", "count.bin")]
  names(binned)[names(binned) == "count"] <- "observations.count"
  rownames(binned) <- NULL

  invisible(list(data = binned, plots = plots, files = files))
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

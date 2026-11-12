#' Plot a bat activity heatmap of monitoring night by time-of-night bin
#'
#' Bins every detection into fixed-width intervals of the monitoring night
#' (noon through midnight to 11:59 the next morning) and draws a heatmap of
#' detection counts, monitoring night on the X axis and time bin on the Y
#' axis, colored on a sequential scale capped at a fixed maximum. The plot
#' is saved as a PNG. A natural input is the \code{$data} returned by
#' \code{\link{batz.plotactivity_daily.count}}.
#'
#' @details
#' \strong{Inputs and checks.} \code{data} must have the \code{date.col} and
#' \code{time.col} columns. They are matched with
#' \code{canonicalize.headers()} (case and \code{"."}/\code{"_"} differences
#' are ignored) and renamed to the given spelling; other columns are not
#' renamed. \code{aes.plotopts} must have \code{$category},
#' \code{$parameter}, \code{$default.value} and the \code{$parameter} rows
#' listed below. Missing headers or parameter rows and duplicate column
#' names in either table stop the function with one combined message.
#'
#' \strong{Special characters.} With \code{strip.special = TRUE}, accented
#' letters and common symbols in \code{data} are simplified (e.g. an accented
#' e becomes \code{e}) and other non-ASCII characters are removed before any
#' check. \code{aes.plotopts} is only cleaned when
#' \code{strip.special.plotopts = TRUE}; otherwise it is checked for
#' characters that cannot be read in this R session, and the function stops
#' with an error naming the table, the number of such characters and the
#' first 5 rows per header (\code{+} if more).
#'
#' \strong{Binning.} \code{date.col} is converted with \code{as.Date()};
#' rows with an unparseable date or time are dropped with a NOTE. The time
#' is re-anchored to the monitoring night: noon is bin 0, midnight is bin
#' \code{n.bins / 2}, and the last bin ends at noon the next day
#' (\code{n.bins = 1440 / bin.minutes}). Detections are counted per night
#' and bin, and every night in \code{data} gets every bin (missing
#' combinations are 0).
#'
#' \strong{Plot.} Tiles are colored by the count capped at
#' \code{fill.max} (legend "Number of Observations", top label
#' \verb{<fill.max>+}). The X axis runs from the first to the last night,
#' with a tick and month label (e.g. \code{"Jan"}) at the first of every
#' month. Y-axis labels are clock times every \code{ybreak.hours} hours.
#' Axis titles are "Monitoring Night" and "Hour of Monitoring". If
#' \code{ggplot2} is not installed, only the data are returned.
#'
#' \strong{aes.plotopts parameters} (all required as \code{$parameter} rows;
#' matched ignoring case). Each value is taken from the \code{aes.style}
#' column when it is not blank, otherwise from \code{$default.value}:
#' \itemize{
#'   \item \code{fill.max}: count at which the color scale is capped
#'     (default 100 if blank or invalid).
#'   \item \code{fill.colors}: semicolon-separated hex colors for the scale
#'     (a 7-color blue ramp if fewer than 2 are given).
#'   \item \code{ybreak.hours}: hours between Y-axis labels (default 2 if
#'     blank or invalid).
#'   \item \code{plot.title.size}, \code{axis.title.size},
#'     \code{axis.text.size}, \code{legend.title.size},
#'     \code{legend.text.size}: text sizes.
#'   \item \code{panel.border.linewidth}: axis line width.
#'   \item \code{plot.width}, \code{plot.height}, \code{ggsave.width.pad},
#'     \code{ggsave.height.pad}, \code{ggsave.units}, \code{ggsave.dpi}: PNG
#'     size (plot size plus pad), units and resolution.
#' }
#'
#' \strong{Saved file.} The PNG is saved to \code{dir.save} as
#' \verb{<project.name>_<site>_<YYYYMMDD>to<YYYYMMDD>_<YYYYMMDDHHMMSS>.png},
#' where \verb{<site>} is \verb{<site.label>-heatmap} (or \code{heatmap}
#' when \code{site.label} is blank) and the date range is the first and last
#' night in \code{data}.
#'
#' @param data Data frame with a monitoring-night date column and a
#'   time-of-night column (see \code{date.col}, \code{time.col}).
#' @param date.col Character, default \code{"date.monitoringnight"}. Column
#'   of \code{data} with the monitoring-night date (a \code{Date}, or text
#'   \code{as.Date()} can read, e.g. \code{"2025-06-01"}).
#' @param time.col Character, default \code{"time"}. Column of \code{data}
#'   with each detection's clock time, \code{"HH:MM"} or \code{"HH:MM:SS"}.
#' @param bin.minutes Single positive number, default \code{30}. Bin width
#'   in minutes; must divide evenly into 1440 (e.g. 15, 20, 30, 60),
#'   otherwise the function stops.
#' @param aes.plotopts Data frame of plot settings, one row per parameter
#'   (e.g. \code{plotopts_heatmap.csv}), with \code{$category},
#'   \code{$parameter}, \code{$default.value} and optionally an
#'   \code{$override.value} column (see Details).
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plotact_heatmap} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#'   (The old argument name \code{aes.default} is still read, with a WARNING.)
#'   Optional rows \code{plot.title.face}, \code{axis.title.face} and
#'   \code{legend.title.face} (\code{"plain"}, \code{"bold"}, \code{"italic"},
#'   \code{"bold.italic"}) set the title fonts; a missing row gives bold.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of the saved file name.
#' @param ... Plot settings typed into the call, named as the
#'   \code{aes.plotopts} \code{$parameter} rows (or fig.list setting
#'   columns), e.g. \code{legend.position = "bottom"},
#'   \code{plot.width = 8}. A vector is joined with ";". Each setting is
#'   taken from the function call, then \code{aes.plotopts} (its custom-style column, then
#'   \code{$default.value}). A setting given here does not need a row in
#'   \code{aes.plotopts}. A NOTE lists the settings used and any name the
#'   function does not use.
#' @param aes.style Character, default \code{"override.value"}. Column of
#'   \code{aes.plotopts} checked first for each parameter; blank cells fall
#'   back to \code{$default.value}. An old \code{overide.value} column is
#'   still read when \code{aes.style = "override.value"} and there is no
#'   \code{override.value} column.
#' @param site.label Character, default \code{""}. Site part of the saved
#'   file name (\code{"-heatmap"} is appended); blank uses \code{"heatmap"}.
#' @param dir.save Character, default \code{getwd()}. Directory the PNG is
#'   saved to.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplify or remove non-ASCII characters in \code{data} (see Details).
#' @param strip.special.plotopts Logical, default \code{FALSE}. If
#'   \code{TRUE}, clean \code{aes.plotopts} the same way, so a label such as
#'   "Temperature (degree C)" loses its symbol. If \code{FALSE}, symbols are
#'   kept and unreadable characters stop the function (see Details).
#'
#' @return Invisibly, a list with:
#'   \describe{
#'     \item{data}{The complete night x bin grid: a date column named after
#'       \code{date.col}, \code{$bin.index} (0 = noon), and
#'       \code{$observations.count} (0 where there were no detections). When
#'       the plot is made, \code{$n.capped} (the count capped at
#'       \code{fill.max}, used for the colors) is added.}
#'     \item{ggplot}{The ggplot object, or \code{NULL} if \code{ggplot2} is
#'       not installed.}
#'   }
#'   A PNG is written to \code{dir.save} when \code{ggplot2} is available.
#'
#' @seealso \code{\link{batz.plotactivity_daily.count}}, whose \code{$data}
#'   is the natural \code{data} input here.
#'
#' @examples
#' \dontrun{
#' daily.result <- batz.plotactivity_daily.count(
#'   data = readxl::read_excel("mine_only.xlsx"),
#'   aes.plotopts = read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE)
#' )
#' heatmap.result <- batz.plotactivity_heatmap(
#'   data = daily.result$data,
#'   aes.plotopts = read.csv("plotopts_heatmap.csv", stringsAsFactors = FALSE),
#'   bin.minutes = 30,
#'   project.name = "myproject",
#'   site.label = "site1"
#' )
#' heatmap.result$ggplot
#' }
#'
#' @export
batz.plotactivity_heatmap <- function(data,
                                       date.col = "date.monitoringnight",
                                       time.col = "time",
                                       bin.minutes = 30,
                                       aes.plotopts = NULL,
                                       project.name = "new.project",
                                       aes.style = "override.value",
                                       site.label = "",
                                       dir.save = getwd(),
                                       strip.special = TRUE,
                                       strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(plotutil_dots(...), "batz.plotactivity_heatmap")

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.aes.plotopts <- paste(deparse(substitute(aes.plotopts)), collapse = "")
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_aes_legacy(plotutil_dots(...), "batz.plotactivity_heatmap")   # old argument name aes.default / plotopts
  if (is.null(aes.plotopts)) aes.plotopts <- plotutil_default_plotopts("batz.plotactivity_heatmap")   # built-in settings
  data <- special.strip(data, strip.special)$df
  ## aes.plotopts is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.plotopts <- special.strip(aes.plotopts, TRUE)$df
  } else {
    special.check.plotopts(aes.plotopts, "aes.plotopts", objname.aes.plotopts, "batz.plotactivity_heatmap")
  }

  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "fill.max", "fill.colors", "ybreak.hours",
    "plot.title.size", "axis.title.size", "axis.text.size",
    "legend.title.size", "legend.text.size", "panel.border.linewidth",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "plot.width", "plot.height"
  )

  check.headers <- function(df, required, label) {
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
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

  # --- data's own required-header check tolerates casing/separator
  # differences via canonicalize.headers() - see @details, "Header check
  # for data now canonicalizes casing/separator differences". ---
  data.required <- c(date.col, time.col)
  data.canon <- canonicalize.headers(data, data.required)
  check.data.headers <- function() {
    if (length(data.canon$missing) > 0) {
      return(sprintf("data is missing these headers: %s", paste(data.canon$missing, collapse = ", ")))
    }
    NULL
  }

  problems <- c(
    check.headers(aes.plotopts, AES.DEFAULT.REQUIRED, "aes.plotopts"),
    check.parameters(aes.plotopts, plotutil_required_left(AES.DEFAULT.REQUIRED.PARAMETERS, plot.inputs), "aes.plotopts"),
    check.duplicates(aes.plotopts, "aes.plotopts"),
    check.data.headers(),
    check.duplicates(data, "data")
  )
  if (length(problems) > 0) stop(paste(problems, collapse = "\n\n"))

  data <- data.canon$df

  if (!is.numeric(bin.minutes) || length(bin.minutes) != 1 || is.na(bin.minutes) ||
      bin.minutes <= 0 || 1440 %% bin.minutes != 0) {
    stop(sprintf("bin.minutes (%s) must be a single positive number that divides evenly into 1440 (minutes/day) - e.g. 15, 20, 30, 60.",
                 paste(bin.minutes, collapse = ", ")))
  }
  n.bins <- 1440 / bin.minutes

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
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.plotopts$parameter), "batz.plotactivity_heatmap")

  data[[date.col]] <- as.Date(data[[date.col]])
  if (any(is.na(data[[date.col]]))) {
    cat(sprintf("NOTE: %d row(s) had an unparseable $%s - dropped.\n", sum(is.na(data[[date.col]])), date.col))
    data <- data[!is.na(data[[date.col]]), , drop = FALSE]
  }

  # --- minutes since midnight, from HH:MM(:SS) - see @details, "Monitoring
  # night convention". ---
  t.parts <- strsplit(as.character(data[[time.col]]), ":")
  time.ok <- lengths(t.parts) >= 2
  if (any(!time.ok)) {
    cat(sprintf("NOTE: %d row(s) had an unparseable $%s (expected HH:MM or HH:MM:SS) - dropped.\n",
                 sum(!time.ok), time.col))
    data <- data[time.ok, , drop = FALSE]
    t.parts <- t.parts[time.ok]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a parseable date/time - nothing to plot.")

  min.since.midnight <- vapply(t.parts, function(p) {
    p <- suppressWarnings(as.numeric(p))
    h <- p[1]; m <- p[2]; s <- if (length(p) >= 3) p[3] else 0
    h * 60 + m + s / 60
  }, numeric(1))
  if (any(is.na(min.since.midnight))) {
    cat(sprintf("NOTE: %d row(s) had a $%s with non-numeric HH/MM/SS components - dropped.\n",
                 sum(is.na(min.since.midnight)), time.col))
    keep <- !is.na(min.since.midnight)
    data <- data[keep, , drop = FALSE]
    min.since.midnight <- min.since.midnight[keep]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a usable time-of-night - nothing to plot.")

  # --- re-anchor to the monitoring night: noon = 0, midnight = n.bins/2,
  # the following 11:59am = the last bin. ---
  night.min <- ifelse(min.since.midnight >= 720, min.since.midnight - 720, min.since.midnight + 720)
  bin.index <- pmin(floor(night.min / bin.minutes), n.bins - 1)

  bin.label <- function(idx) {
    actual.min <- (idx * bin.minutes + 720) %% 1440
    sprintf("%02d:%02d", actual.min %/% 60, actual.min %% 60)
  }

  all.nights <- sort(unique(data[[date.col]]))
  all.bins <- 0:(n.bins - 1)

  ## <daterange> token - round twenty-three, per Josh's correction
  ## (2026-09-25): this function DOES have a date range after all (data's
  ## own date.col column), even though it has no explicit date.start/
  ## date.end parameters of its own the way batz.plotactivity_daily.count()
  ## does - so the token is built from the actual min/max date.col value
  ## present in data, not a per-call input. See @details, "File naming
  ## (round twenty-three)".
  daterange.token <- sprintf("%sto%s", format(min(all.nights), "%Y%m%d"), format(max(all.nights), "%Y%m%d"))

  ## Built with the column named dynamically after date.col (round
  ## twenty-four fix - see @details, "Internal hardcoded ... literals
  ## fixed"), rather than the literal "monnight.date" hardcoded here
  ## before this round - a caller-supplied date.col now actually works.
  ## Column identifiers renamed, 2026-09-27 (see @details, "Column
  ## identifiers renamed"): the count column built here is now named
  ## observations.count instead of n.
  raw.counts <- stats::aggregate(list(observations.count = rep(1, nrow(data))),
                                  by = stats::setNames(list(data[[date.col]], bin.index), c(date.col, "bin.index")),
                                  FUN = sum)
  # --- complete grid: every night x every bin, missing = 0 - see @details,
  # "Complete night x bin grid". ---
  full.grid <- stats::setNames(expand.grid(all.nights, all.bins), c(date.col, "bin.index"))
  counts <- merge(full.grid, raw.counts, by = c(date.col, "bin.index"), all.x = TRUE)
  counts$observations.count[is.na(counts$observations.count)] <- 0

  cat(sprintf("Binned %d observation row(s) into %d monitoring night(s) x %d bin(s) of %g minute(s) each.\n",
               nrow(data), length(all.nights), n.bins, bin.minutes))

  ggplot.obj <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    ybreak.hours <- suppressWarnings(as.numeric(get.default("ybreak.hours")))
    if (is.na(ybreak.hours) || ybreak.hours <= 0) ybreak.hours <- 2
    bins.per.break <- max(1, round(ybreak.hours * 60 / bin.minutes))
    y.breaks <- seq(0, n.bins - bins.per.break, by = bins.per.break)
    y.labels <- vapply(y.breaks, bin.label, character(1))
    y.breaks <- c(y.breaks, n.bins)
    y.labels <- c(y.labels, "12:00:00")

    x.start <- min(all.nights)
    x.end <- max(all.nights)
    x.breaks <- seq(as.Date(format(x.start, "%Y-%m-01")), as.Date(format(x.end, "%Y-%m-01")), by = "1 month")
    x.labels <- format(x.breaks, "%b")

    fill.max <- suppressWarnings(as.numeric(get.default("fill.max")))
    if (is.na(fill.max) || fill.max <= 0) fill.max <- 100
    counts$n.capped <- pmin(counts$observations.count, fill.max)

    fill.colors <- strsplit(get.default("fill.colors"), ";", fixed = TRUE)[[1]]
    fill.colors <- trimws(fill.colors[nzchar(trimws(fill.colors))])
    if (length(fill.colors) < 2) {
      fill.colors <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
    }
    fill.breaks <- fill.max * c(0, 0.25, 0.5, 0.75, 1)
    fill.labels <- c(format(fill.breaks[1:4], trim = TRUE), paste0(format(fill.breaks[5], trim = TRUE), "+"))

    minor.tick.len <- 0.7

    ## x = .data[[date.col]] (round twenty-four fix), not a hardcoded
    ## "monnight.date" - see @details, "Internal hardcoded ... literals
    ## fixed".
    ggplot.obj <- ggplot2::ggplot(counts, ggplot2::aes(x = .data[[date.col]], y = bin.index, fill = n.capped)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.15) +
      ggplot2::geom_segment(
        data = data.frame(x = x.breaks),
        ggplot2::aes(x = x, xend = x, y = -0.5, yend = -0.5 - minor.tick.len),
        inherit.aes = FALSE, color = "black", linewidth = 0.3
      ) +
      ggplot2::scale_x_date(breaks = x.breaks, labels = x.labels, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = y.breaks, labels = y.labels, expand = c(0, 0)) +
      ggplot2::scale_fill_gradientn(
        colors = fill.colors, name = "Number of Observations",
        limits = c(0, fill.max), breaks = fill.breaks, labels = fill.labels,
        guide = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5,
          barwidth = grid::unit(10, "cm"), barheight = grid::unit(0.4, "cm"))
      ) +
      ggplot2::labs(x = "Monitoring Night", y = "Hour of Monitoring") +
      ggplot2::coord_cartesian(xlim = c(x.start, x.end), ylim = c(-0.5, n.bins), clip = "off") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid       = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        axis.line        = ggplot2::element_line(color = "black", linewidth = as.numeric(get.default("panel.border.linewidth"))),
        axis.ticks.x     = ggplot2::element_blank(),
        axis.title       = ggplot2::element_text(face = plotutil_face(get.default("axis.title.face")), size = as.numeric(get.default("axis.title.size"))),
        axis.text        = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
        axis.text.x      = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
        plot.title       = ggplot2::element_text(size = as.numeric(get.default("plot.title.size")), face = plotutil_face(get.default("plot.title.face"))),
        plot.margin      = ggplot2::margin(t = 5.5, r = 20, b = 5.5, l = 5.5),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = ggplot2::element_text(size = as.numeric(get.default("legend.title.size")), face = plotutil_face(get.default("legend.title.face"))),
        legend.text      = ggplot2::element_text(size = as.numeric(get.default("legend.text.size")))
      )

    site.token <- if (nzchar(trimws(site.label))) paste0(trimws(site.label), "-heatmap") else "heatmap"
    ## File naming (round twenty-three), per Josh's correction (2026-09-25):
    ## see @details, "File naming (round twenty-three)" - <daterange> token
    ## added; timestamp format unchanged from round twenty-two (no
    ## underscore between the date and time halves).
    fname <- sprintf("%s_%s_%s_%s.png", project.name, site.token, daterange.token, format(Sys.time(), "%Y%m%d%H%M%S"))
    fname <- file.path(dir.save, fname)
    ggplot2::ggsave(fname, plot = ggplot.obj,
      width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
      height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
      units = get.default("ggsave.units"), dpi = as.numeric(get.default("ggsave.dpi")))
    cat("Saved:", fname, "\n")
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(data = counts, ggplot = ggplot.obj))
}

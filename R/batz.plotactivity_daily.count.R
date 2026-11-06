#' Plot nightly detection counts on a log-scaled bar chart
#'
#' Takes raw per-detection vetted-acoustics records (one row per call file),
#' optionally removes unidentified (\code{"NoID"}) calls, reads the call time
#' from the file name, keeps a date window, counts detections per monitoring
#' night and draws the counts as bars on a log-scaled Y axis with fixed break
#' values. Saves the plot as a PNG and returns the prepared data.
#'
#' @param data Data frame of raw per-detection records (e.g. a vetting sheet
#'   read with \code{readxl::read_excel()}). Must contain the columns named by
#'   \code{spp.id}, \code{filename.col} and \code{groupby.date}.
#' @param spp.id Character, default \code{"kpauto"}. Column holding the species
#'   or auto-ID label. Used for the \code{"NoID"} handling (see
#'   \code{trim.noid}).
#' @param filename.col Character, default \code{"filename"}. Column holding the
#'   recording file name; the call time is read from it.
#' @param groupby.date Character, default \code{"monnight"}. Column holding the
#'   monitoring-night date.
#' @param trim.noid Logical, default \code{FALSE}. \code{TRUE} removes rows whose
#'   \code{spp.id} value is \code{"NoID"} (any case). See Details for blank
#'   values.
#' @param date.start,date.end Date, character (\code{"YYYY-MM-DD"},
#'   \code{"MM/DD/YYYY"} or \code{"MM/DD/YY"}) or \code{NULL} (default). First
#'   and last monitoring night to plot. A \code{NULL} value defaults to
#'   July 1 (start) or December 31 (end) of the year of the earliest
#'   monitoring night in \code{data}.
#' @param aes.default Data frame of plot settings (a plotopts table, e.g.
#'   \verb{plotopts_dailycount.csv}), one row per parameter. Required columns:
#'   \code{$category}, \code{$parameter}, \code{$default.value}. Optional:
#'   the column named by \code{aes.style}, \code{$notes}.
#'   Default \code{NULL}: the built-in settings are used (NOTE): the
#'   \code{PLOTOPTS_plotact_daily.count} tab of Batz_reference_db, carried by the
#'   package. A data frame given here is used instead.
#' @param project.name Character, default \code{"new.project"}. First part of
#'   each saved file name.
#' @param ... Plot settings typed into the call, named as the
#'   \code{aes.default} \code{$parameter} rows (or fig.list setting
#'   columns), e.g. \code{legend.position = "bottom"},
#'   \code{plot.width = 8}. A vector is joined with ";". Each setting is
#'   taken from the function call, then \code{aes.default} (its custom-style column, then
#'   \code{$default.value}). A setting given here does not need a row in
#'   \code{aes.default}. A NOTE lists the settings used and any name the
#'   function does not use.
#' @param aes.style Character, default \code{"override.value"}. Column of
#'   \code{aes.default} checked first for each parameter; a blank cell (or a
#'   missing column) falls back to \code{$default.value}. An old
#'   \code{overide.value} column is still read when \code{aes.style =
#'   "override.value"}.
#' @param site.label Character, default \code{""}. Optional label put in the
#'   file name (\verb{<site.label>-dailycount}); blank gives
#'   \verb{dailycount}.
#' @param write.csv.data Logical, default \code{FALSE}. \code{TRUE} also writes
#'   the prepared per-detection data (the returned \code{$data}) to a CSV in
#'   \code{dir.save}.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG (and CSV)
#'   are saved in.
#' @param snake_case Logical, default \code{FALSE}. \code{TRUE} replaces every
#'   \code{"."} in the written CSV's column names with \code{"_"} (e.g.
#'   \code{date.monitoringnight} becomes \code{date_monitoringnight}). The
#'   returned \code{$data} always keeps the dot names.
#' @param strip.special Logical, default \code{TRUE}. \code{TRUE} simplifies
#'   accented letters and symbols in \code{data} (e.g. e-acute to \code{e},
#'   the multiplication sign to \code{X}) and removes other non-ASCII characters (e.g.
#'   degree and micro signs), before any header check.
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same for
#'   \code{aes.default}. With \code{FALSE}, plot text such as
#'   \code{"Temperature (degree sign C)"} is kept; a cell that cannot be read in this R
#'   session (e.g. a Latin-1 degree-sign byte) stops the function with an error
#'   naming the table, the number of bad characters and the first 5 rows per
#'   column.
#'
#' @details
#' \strong{Checks.} \code{aes.default} must have the required columns, unique
#' column names and these \code{$parameter} rows (matched ignoring case):
#' \code{y.custom}, \code{bar.fill}, \code{bar.width}, \code{xaxe.date.format},
#' \code{xaxe.date.buffer.days}, \code{plot.title.size},
#' \code{axis.title.size}, \code{axis.text.size},
#' \code{panel.border.linewidth}, \code{ggsave.dpi}, \code{ggsave.units},
#' \code{ggsave.width.pad}, \code{ggsave.height.pad}, \code{plot.width},
#' \code{plot.height}. Otherwise the function stops and lists the problems.
#'
#' \strong{Headers.} The column names of \code{data}, and the values of
#' \code{spp.id}, \code{filename.col} and \code{groupby.date}, are passed
#' through \code{standardize.headers()} (lower case, spaces and punctuation
#' to \code{"_"}), so \code{"KP Auto"} and \code{"kp_auto"} name the same
#' column. If \code{groupby.date} is one of \code{"monnight"},
#' \code{"monitoringnight"}, \code{"date-12"} or
#' \code{"date_monitoringnight"} and is not in \code{data}, the first of the
#' others that is present is used, with a NOTE. Duplicate column names in
#' \code{data} stop the function.
#'
#' \strong{NoID handling.} A blank \code{spp.id} value is filled in as
#' \code{"NOID"}. Exception: when \code{spp.id = "kpauto"} and
#' \code{trim.noid = TRUE}, blank rows are removed instead. Then, with
#' \code{trim.noid = TRUE}, every row whose \code{spp.id} is \code{"NoID"}
#' (any case) is removed, with a NOTE giving the count.
#'
#' \strong{Call time.} \code{$time} (\code{"HH:MM:SS"}) is read from the file
#' name:
#' \itemize{
#'   \item SM4: \verb{<ARU>_<YYYYMMDD>_<HHMMSS>_<...>}, e.g.
#'     \verb{MINE_20250109_152740_000.wav} gives \code{"15:27:40"}.
#'   \item SM5: \verb{<ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_<...>} (the 6 digits
#'     after an 8-digit date).
#' }
#' Rows whose file name fits neither shape are dropped, with a NOTE.
#'
#' \strong{Monitoring night and date window.} \code{$date.monitoringnight}
#' (Date) is parsed from \code{groupby.date} as \code{YYYY-MM-DD},
#' \code{MM/DD/YYYY} or \code{MM/DD/YY}; rows that cannot be parsed are
#' dropped, with a NOTE. Rows outside \code{date.start} to \code{date.end}
#' are dropped. The function stops if no rows are left after any step.
#'
#' \strong{Counts.} Each row gets \code{$observations.count = 1}; these are
#' summed per \code{$date.monitoringnight} with \code{stats::aggregate()}.
#'
#' \strong{Plot} (only when \code{ggplot2} is installed). Bar height is
#' \code{log1p(observations.count)}; the Y axis breaks are the values in
#' \code{y.custom} (semicolon-separated, at least 2 numbers, e.g.
#' \code{"0;5;30;200;1200;7500"}), labelled with the real counts. The Y axis
#' runs to the larger of the top break and the highest count. The X axis has
#' one break per month, labelled with \code{xaxe.date.format} (blank gives
#' \code{"\%b"}), and is padded by \code{xaxe.date.buffer.days} days (blank
#' gives 0.5) at each end so the first and last bars are not cut off. Bars use
#' \code{bar.fill} and \code{bar.width}. Axis titles are \code{"Date"} and
#' \code{"Number of Observations"}. The PNG is
#' \code{plot.width + ggsave.width.pad} by
#' \code{plot.height + ggsave.height.pad} \code{ggsave.units}, at
#' \code{ggsave.dpi}.
#'
#' \strong{File names.}
#' \verb{<project.name>_<site>_<daterange>_<timestamp>.png}, where
#' \verb{<site>} is \verb{dailycount} or \verb{<site.label>-dailycount},
#' \verb{<daterange>} is \verb{YYYYMMDDtoYYYYMMDD} (the window used) and
#' \verb{<timestamp>} is \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")}. The
#' CSV (when \code{write.csv.data = TRUE}) uses the same name with
#' \verb{.csv}.
#'
#' @return Invisibly, a list:
#' \describe{
#'   \item{\code{data}}{The filtered per-detection data frame, with
#'     standardized column names plus \code{$time}, \code{$date.monitoringnight}
#'     and \code{$observations.count}. Can be passed straight to
#'     \code{\link{batz.plotactivity_heatmap}}.}
#'   \item{\code{daily}}{One row per monitoring night:
#'     \code{$date.monitoringnight}, \code{$observations.count} (plus
#'     \code{$obs.plot}, the log1p value, when a plot was drawn).}
#'   \item{\code{ggplot}}{The ggplot object, or \code{NULL} when
#'     \code{ggplot2} is not installed.}
#' }
#' Files written to \code{dir.save}: the PNG (when \code{ggplot2} is
#' installed) and, with \code{write.csv.data = TRUE}, the CSV.
#'
#' @seealso \code{\link{batz.plotactivity_heatmap}}, which takes the returned
#'   \code{$data} as its \code{data} input.
#'
#' @examples
#' \dontrun{
#' result <- batz.plotactivity_daily.count(
#'   data = readxl::read_excel("mine_only.xlsx"),
#'   aes.default = read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE),
#'   trim.noid = TRUE,
#'   date.start = "2025-07-01", date.end = "2025-12-31"
#' )
#' result$ggplot
#'
#' # chain into the heatmap
#' batz.plotactivity_heatmap(
#'   data = result$data,
#'   aes.default = read.csv("plotopts_heatmap.csv", stringsAsFactors = FALSE)
#' )
#'
#' # also write the prepared data as a CSV with snake_case headers
#' batz.plotactivity_daily.count(
#'   data = readxl::read_excel("mine_only.xlsx"),
#'   aes.default = read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE),
#'   write.csv.data = TRUE, snake_case = TRUE
#' )
#' }
#'
#' @export
batz.plotactivity_daily.count <- function(data,
                                           spp.id = "kpauto",
                                           filename.col = "filename",
                                           groupby.date = "monnight",
                                           trim.noid = FALSE,
                                           date.start = NULL,
                                           date.end = NULL,
                                           aes.default = NULL,
                                           project.name = "new.project",
                                           aes.style = "override.value",
                                           site.label = "",
                                           write.csv.data = FALSE,
                                           dir.save = getwd(),
                                           snake_case = FALSE,
                                           strip.special = TRUE,
                                           strip.special.plotopts = FALSE, ...) {

  ## settings typed into the call: used before fig.list and plotopts
  plot.inputs <- plotutil_plot_inputs(list(...), "batz.plotactivity_daily.count")

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
  if (is.null(aes.default)) aes.default <- plotutil_default_plotopts("batz.plotactivity_daily.count")   # built-in settings
  data <- special.strip(data, strip.special)$df
  ## aes.default is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.default <- special.strip(aes.default, TRUE)$df
  } else {
    special.check.plotopts(aes.default, "aes.default", objname.aes.default, "batz.plotactivity_daily.count")
  }

  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "y.custom", "bar.fill", "bar.width", "xaxe.date.format", "xaxe.date.buffer.days",
    "plot.title.size", "axis.title.size", "axis.text.size",
    "panel.border.linewidth",
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

  problems <- c(
    check.headers(aes.default, AES.DEFAULT.REQUIRED, "aes.default"),
    check.parameters(aes.default, plotutil_required_left(AES.DEFAULT.REQUIRED.PARAMETERS, plot.inputs), "aes.default"),
    check.duplicates(aes.default, "aes.default")
  )
  if (length(problems) > 0) stop(paste(problems, collapse = "\n"))

  # Header standardization (per Josh, 2026-09-14 project preference) - see
  # @details, "Header standardization". `data`'s real column names, and the
  # three column-name arguments naming them, are both standardized the same
  # way so either spelling resolves to the same column.
  names(data) <- standardize.headers(names(data))
  spp.id <- standardize.headers(spp.id)
  filename.col <- standardize.headers(filename.col)
  groupby.date <- standardize.headers(groupby.date)

  ## 2026-10-02, per Josh: the monitoring-night column goes by several names
  ## ("MONNIGHT", "MONITORINGNIGHT", "DATE-12", "date.monitoringnight").
  ## If groupby.date names one of these and isn't in data, use whichever
  ## of the others is there (in that order).
  MN.ALIASES <- c("monnight", "monitoringnight", "date_12", "date_monitoringnight")
  if (!(groupby.date %in% names(data)) && groupby.date %in% MN.ALIASES) {
    mn.found <- intersect(MN.ALIASES, names(data))
    if (length(mn.found) > 0) {
      cat(sprintf("NOTE: groupby.date = '%s' is not a column of data - using '%s' instead.\n",
                  groupby.date, mn.found[1]))
      groupby.date <- mn.found[1]
    }
  }

  data.problem <- check.headers(data, c(spp.id, filename.col, groupby.date), "data")
  if (!is.null(data.problem)) stop(data.problem)
  dup.problem <- check.duplicates(data, "data")
  if (!is.null(dup.problem)) stop(dup.problem)

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
  plotutil_plot_inputs_check(plot.inputs, c(AES.DEFAULT.REQUIRED.PARAMETERS, aes.default$parameter), "batz.plotactivity_daily.count")

  # --- trim.noid: same $spp.id-blank-handling/removal behavior as
  # batz.generate_plotframe.bat() - see @details, "trim.noid matched to
  # batz.generate_plotframe.bat" (2026-09-28, per Josh). A blank $spp.id
  # value is normally filled in with the literal "NOID", UNLESS $spp.id is
  # still this function's own default ("kpauto") AND trim.noid = TRUE, in
  # which case those blank-valued rows are removed from `data` entirely
  # instead of being filled in. ---
  spp.id.vals <- as.character(data[[spp.id]])
  is.blank.spp.id <- is.na(spp.id.vals) | !nzchar(trimws(spp.id.vals))
  if (identical(spp.id, "kpauto") && isTRUE(trim.noid)) {
    data <- data[!is.blank.spp.id, , drop = FALSE]
    spp.id.vals <- spp.id.vals[!is.blank.spp.id]
  } else {
    spp.id.vals[is.blank.spp.id] <- "NOID"
  }
  data[[spp.id]] <- spp.id.vals

  # --- Then, whenever trim.noid = TRUE, drop any row whose $spp.id reads
  # "noid" (case-insensitive) - catches both pre-existing literal "NoID"
  # values and any blanks the fallback branch above just filled in. Same
  # parameter name/meaning as batz.generate_plotframe.bat()/
  # batz.merge_vetted.acoustics2()'s own $trim.noid, per this project's
  # cross-function parameter-naming convention. ---
  if (isTRUE(trim.noid)) {
    n.before <- nrow(data)
    data <- data[!tolower(trimws(as.character(data[[spp.id]]))) %in% "noid", , drop = FALSE]
    n.dropped <- n.before - nrow(data)
    if (n.dropped > 0) {
      cat(sprintf("NOTE: trim.noid = TRUE - dropped %d row(s) where $%s = 'NoID'.\n", n.dropped, spp.id))
    }
  }
  if (nrow(data) == 0) stop("data has 0 rows left after trim.noid filtering - nothing to plot.")

  # --- $time, extracted from $<filename.col> - see @details, "Time
  # extraction from the file name". ---
  raw.time <- sub("^[^_]*_[^_]*_([0-9]{6})_[^_]*$", "\\1", as.character(data[[filename.col]]))
  time.ok <- grepl("^[0-9]{6}$", raw.time)
  ## 2026-10-02, per Josh: SM5 file names have a microphone part
  ## (<ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_<...>); for any name the SM4 pattern
  ## above doesn't fit, take the 6 digits after an 8-digit date instead.
  rx.any <- "^.*?_[0-9]{8}_([0-9]{6})(_.*|\\..*)?$"
  fn.vals <- as.character(data[[filename.col]])
  retry <- !time.ok & grepl(rx.any, fn.vals, perl = TRUE)
  raw.time[retry] <- sub(rx.any, "\\1", fn.vals[retry], perl = TRUE)
  time.ok <- grepl("^[0-9]{6}$", raw.time)
  data$time <- ifelse(time.ok, paste(substr(raw.time, 1, 2), substr(raw.time, 3, 4), substr(raw.time, 5, 6), sep = ":"), NA_character_)
  if (any(!time.ok)) {
    cat(sprintf("NOTE: %d row(s) had a $%s that didn't match the expected '<site>_<date>_<HHMMSS>_<...>' (SM4) or '<site>_<mic>_<date>_<HHMMSS>_<...>' (SM5) shape - dropped.\n",
                 sum(!time.ok), filename.col))
    data <- data[time.ok, , drop = FALSE]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a parseable $time after filename parsing - nothing to plot.")

  # --- $date.monitoringnight - a flexible date parse of the groupby.date
  # column, same multi-format fallback pattern used elsewhere in this
  # package (e.g. batz.plotactivity_observations()'s parse.flex.date()).
  # Renamed from the original ad hoc script's "monnight_date", then
  # "date.mon" (round twenty-four), to "date.monitoringnight" (round
  # twenty-five) - see @details. ---
  parse.flex.date <- function(x) {
    out <- as.Date(rep(NA_character_, length(x)))
    for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.Date(x, format = fmt)
      out[still.na] <- parsed[still.na]
    }
    out
  }
  data$date.monitoringnight <- parse.flex.date(data[[groupby.date]])
  if (any(is.na(data$date.monitoringnight))) {
    cat(sprintf("NOTE: %d row(s) had a $%s that could not be parsed as a date - dropped.\n",
                 sum(is.na(data$date.monitoringnight)), groupby.date))
    data <- data[!is.na(data$date.monitoringnight), , drop = FALSE]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a parseable $date.monitoringnight - nothing to plot.")

  # --- date.start/date.end - see @details, "Default date window". ---
  if (is.null(date.start) || is.null(date.end)) {
    year.val <- format(min(data$date.monitoringnight, na.rm = TRUE), "%Y")
    if (is.null(date.start)) date.start <- as.Date(paste0(year.val, "-07-01"))
    if (is.null(date.end)) date.end <- as.Date(paste0(year.val, "-12-31"))
    cat(sprintf("NOTE: date.start/date.end not both given - defaulting to %s to %s (July 1 - Dec 31 of %s).\n",
                 date.start, date.end, year.val))
  } else {
    date.start <- parse.flex.date(as.character(date.start))
    date.end <- parse.flex.date(as.character(date.end))
  }

  data <- data[data$date.monitoringnight >= date.start & data$date.monitoringnight <= date.end, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(sprintf("0 rows of data fall within date.start/date.end (%s to %s) - nothing to plot.", date.start, date.end))
  }

  ## <daterange> token - round twenty-two, per Josh (2026-09-24): see
  ## @details, "File naming (round twenty-two)". Built from date.start/
  ## date.end AFTER they're fully resolved just above (whether explicitly
  ## passed or defaulted), so both the explicit-input and defaulted-window
  ## cases get a real, correct <daterange> token - same
  ## sprintf("%sto%s", ...) convention already used by
  ## batz.plotsm4_heatmap()/batz.plotdetections_first.last()/
  ## batz.plotactivity_observations().
  daterange.token <- sprintf("%sto%s", format(date.start, "%Y%m%d"), format(date.end, "%Y%m%d"))

  # --- daily aggregate: a helper $observations.count = 1 per row (same convention
  # batz.generate_plotframe.bat() uses), summed per $date.monitoringnight -
  # see @details, "Aggregation and Y-axis scale". ---
  data$observations.count <- 1
  daily <- stats::aggregate(observations.count ~ date.monitoringnight, data = data, FUN = sum)
  daily <- daily[order(daily$date.monitoringnight), , drop = FALSE]

  cat(sprintf("Rows after trim.noid + date filter: %d (%d distinct monitoring night(s)).\n", nrow(data), nrow(daily)))

  ggplot.obj <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    y.custom <- suppressWarnings(as.numeric(strsplit(get.default("y.custom"), ";", fixed = TRUE)[[1]]))
    y.custom <- sort(unique(y.custom[!is.na(y.custom)]))
    if (length(y.custom) < 2) {
      stop("aes.default's $y.custom must have at least 2 semicolon-separated numbers (e.g. \"0;5;30;200;1200;7500\").")
    }
    y.upper <- max(c(y.custom, daily$observations.count), na.rm = TRUE)
    daily$obs.plot <- log1p(daily$observations.count)
    break.pos <- log1p(y.custom)
    break.labels <- format(y.custom, big.mark = ",", trim = TRUE, scientific = FALSE)

    date.format <- get.default("xaxe.date.format")
    if (is.na(date.format) || !nzchar(date.format)) date.format <- "%b"
    month.breaks <- seq(as.Date(format(date.start, "%Y-%m-01")), date.end, by = "1 month")
    # $xaxe.date.buffer.days (default 0.5): without this, the boundary day's
    # own bar (drawn width = 1 day wide, so its outer edge sits half a day
    # past its own date) gets its outer edge clipped to NA by scale_x_date's
    # exact limits, which ggplot2 then silently drops with a "Removed 1 rows
    # containing missing values (geom_col())" warning - the same edge-
    # clipping issue already found and fixed the same way in
    # batz.plotdetections_first.last() (see that function's own @details,
    # the entry beginning "The X axis had no explicit range").
    xaxe.buffer <- suppressWarnings(as.numeric(get.default("xaxe.date.buffer.days")))
    if (is.na(xaxe.buffer)) xaxe.buffer <- 0.5

    ggplot.obj <- ggplot2::ggplot(daily, ggplot2::aes(x = date.monitoringnight, y = obs.plot)) +
      ggplot2::geom_col(fill = get.default("bar.fill"), width = as.numeric(get.default("bar.width"))) +
      ggplot2::scale_x_date(breaks = month.breaks, date_labels = date.format,
                             limits = c(date.start - xaxe.buffer, date.end + xaxe.buffer)) +
      ggplot2::scale_y_continuous(breaks = break.pos, labels = break.labels, limits = c(0, log1p(y.upper))) +
      ggplot2::labs(x = "Date", y = "Number of Observations") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid       = ggplot2::element_blank(),
        axis.line        = ggplot2::element_line(color = "black", linewidth = as.numeric(get.default("panel.border.linewidth"))),
        axis.title       = ggplot2::element_text(face = "plain", size = as.numeric(get.default("axis.title.size"))),
        axis.text        = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
        plot.title       = ggplot2::element_text(size = as.numeric(get.default("plot.title.size"))),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA)
      )

    site.token <- if (nzchar(trimws(site.label))) paste0(trimws(site.label), "-dailycount") else "dailycount"
    ## File naming (round twenty-two), per Josh (2026-09-24): see @details,
    ## "File naming (round twenty-two)" - <daterange> token added, and the
    ## timestamp format is now the 14-digit, no-underscore form (see
    ## @details, "Timestamp format (round twenty-two)").
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

  if (isTRUE(write.csv.data)) {
    site.token <- if (nzchar(trimws(site.label))) paste0(trimws(site.label), "-dailycount") else "dailycount"
    ## Same <daterange>/<timestamp> tokens as the PNG above - see @details,
    ## "File naming (round twenty-two)".
    csv.name <- file.path(dir.save, sprintf("%s_%s_%s_%s.csv", project.name, site.token, daterange.token, format(Sys.time(), "%Y%m%d%H%M%S")))
    ## snake_case output option (per Josh's request to audit and extend
    ## this package-wide, 2026-09-22) - applied only to this written copy,
    ## right before the write, never to the `data` object itself (which is
    ## still used, unchanged, in the return value below). See @param
    ## snake_case and @details, "A near-no-op for most columns".
    csv.data <- data
    if (snake_case) names(csv.data) <- snake.headers(names(csv.data))
    utils::write.csv(csv.data, csv.name, row.names = FALSE)
    cat("Saved:", csv.name, "\n")
  }

  invisible(list(data = data, daily = daily, ggplot = ggplot.obj))
}

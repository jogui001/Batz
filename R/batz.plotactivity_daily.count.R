#' Plot total nightly detection counts on a log-scaled bar chart
#'
#' Given a raw, per-detection vetted-acoustics export (one row per call/
#' observation), removes unidentified (\code{"NoID"}) calls, derives a
#' monitoring-night date and a time-of-night from the file's own columns,
#' restricts to a date window, aggregates to one count per monitoring night,
#' and draws that count as a grey bar chart on a log-scaled Y axis with
#' fixed, hand-picked break values. Replaces the ad hoc
#' \code{mine_bat_plot.R} script this function was converted from - see
#' Details for what changed along the way.
#'
#' @param data A data frame of raw, per-detection records (e.g. a
#'   \code{readxl::read_excel()}'d vetted-export sheet). Genuinely raw,
#'   externally-supplied headers - run through \code{standardize.headers()}
#'   on receipt (see Details, "Header standardization"). After
#'   standardization, must have whatever \code{spp.id}/\code{filename.col}/
#'   \code{groupby.date} (below, also standardized) name.
#' @param spp.id Character, default \code{"kpauto"}. Name of the column in
#'   \code{data} holding the species/auto-ID identifier - only used to
#'   detect \code{"NoID"} rows when \code{trim.noid = TRUE}, and to count
#'   rows per night. Matches this parameter's name in
#'   \code{\link{batz.generate_plotframe.bat}}/
#'   \code{\link{batz.merge_vetted.acoustics2}}, per this project's
#'   cross-function parameter-naming convention.
#' @param filename.col Character, default \code{"filename"}. Name of the
#'   column in \code{data} holding each recording's own file name - the
#'   source \code{$time} is extracted from (see Details, "Time extraction
#'   from the file name").
#' @param groupby.date Character, default \code{"monnight"}. Name of the
#'   column in \code{data} holding the monitoring-night date - matches this
#'   parameter's name in \code{\link{batz.generate_plotframe.bat}}.
#' @param trim.noid Logical, default \code{TRUE}. Drop rows where the
#'   \code{spp.id} column reads \code{"NoID"} (case-insensitive) before
#'   counting/plotting.
#' @param date.start,date.end A Date, or a character string in
#'   \code{"YYYY-MM-DD"}/\code{"MM/DD/YYYY"} form, or \code{NULL} (the
#'   default for both). When either is \code{NULL}, both default to July 1
#'   - December 31 of \code{data}'s own earliest monitoring-night year -
#'   see Details, "Default date window", for why and how to override it.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_dailycount.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value}; an
#'   \code{$overide.value} column (blank, user-fillable) and \code{$notes}
#'   are optional - same \code{$aes.style}-driven resolution as this
#'   package's other plot functions (see Details).
#' @param project.name Character, default \code{"new.project"}. The first
#'   part of every saved file's name:
#'   \code{"<project.name>_<site>_<timestamp>.png"} - matches the unified
#'   file-naming convention already used by every other \code{batz} plot
#'   function (see Details, "File naming").
#' @param aes.style Character, default \code{"overide.value"}. Names which
#'   column of \code{aes.default} is checked FIRST for each parameter
#'   (falling back to \code{$default.value} when that column doesn't exist,
#'   or is blank for that row).
#' @param site.label Character, default \code{""} (blank). The \code{<site>}
#'   token in the saved file name (see \code{project.name} above); falls
#'   back to the literal \code{"dailycount"} when left blank. Unlike this
#'   package's \code{fig.list}-driven plot functions, this function has no
#'   natural per-call "site" concept of its own (one call plots one dataset)
#'   - \strong{a judgment call, flagged}: pass e.g. an ARU/site name here if
#'   you want it in the saved file name.
#' @param write.csv.data Logical, default \code{FALSE}. Also write the
#'   filtered/prepared \code{data} (see Value, \code{$data}) to a CSV in
#'   \code{dir.save}. \strong{A deliberate behavior change from the original
#'   ad hoc script, flagged}: that script always wrote a CSV; this defaults
#'   to off, since silently writing a second file isn't this package's
#'   convention elsewhere.
#' @param dir.save Character, default \code{getwd()}. Directory the
#'   generated PNG (and CSV, if \code{write.csv.data = TRUE}) is saved into.
#' @param snake_case Logical, default \code{FALSE}. Added 2026-09-22, per
#'   Josh's request to audit and extend the snake_case output option
#'   package-wide (see \code{\link{batz.generate_plotframe.bat}}'s sibling
#'   parameter). Controls only the CSV written when \code{write.csv.data =
#'   TRUE} - applied to a copy of \code{data} right before that write, never
#'   to the \code{$data} this function returns (see Value; that object still
#'   needs its usual dot-separated names, e.g. \code{$monnight.date}, to
#'   chain straight into \code{\link{batz.plotactivity_heatmap}} per
#'   \code{@seealso} below). \code{FALSE} (default) leaves the written CSV's
#'   headers exactly as before. \code{TRUE} runs the written copy's column
#'   names through \code{standardize.headers()} first. See Details, "A
#'   near-no-op for most columns", for why this mostly only affects one
#'   column here.
#'
#' @return Invisibly, a list with \code{data} (the filtered per-detection
#'   data frame, with \code{$time} and \code{$monnight.date} added -
#'   suitable as the \code{data} input to
#'   \code{\link{batz.plotactivity_heatmap}}), \code{daily} (the aggregated
#'   per-night data frame: \code{$monnight.date}, \code{$obs}), and
#'   \code{ggplot} (the ggplot object, only populated when \code{ggplot2}
#'   is available).
#'
#' @details
#' \strong{Converted from \code{mine_bat_plot.R}, an ad hoc procedural
#' script, into a reusable \code{batz} function - not a full from-scratch
#' spec.} The original script's actual behavior (filter \code{kpauto ==
#' "noid"}, extract \code{$time} from \code{$filename}, restrict to Jul 1 -
#' Dec 31 of the data's year, aggregate by day, plot a grey log-scaled bar
#' chart) is preserved; what changed is packaging it to match every other
#' \code{batz} plot function's conventions - parameterized column names
#' instead of hardcoded ones, an \code{aes.default} settings file instead of
#' hardcoded plot constants, the unified \code{project.name}/\code{dir.save}
#' file-naming/save mechanism, and \code{data}/return-value shapes that
#' compose with \code{\link{batz.plotactivity_heatmap}} (see that
#' function's own \code{@details} for the pairing).
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' DOES apply here}, unlike this package's \code{fig.list}-driven plotting
#' functions: \code{data} is a genuinely raw, externally-supplied vet-sheet
#' export (real spreadsheet headers), not an upstream \code{batz}
#' function's already-standardized output - so it's run through the shared
#' package helper \code{standardize.headers()} on receipt, exactly like
#' \code{\link{batz.plotcover_bullseye}}'s \code{data}/\code{mic}. The
#' \code{spp.id}/\code{filename.col}/\code{groupby.date} column-name
#' parameters are standardized the same way, so a caller can pass either the
#' file's own raw spelling (\code{"KP Auto"}) or an already-standardized one
#' (\code{"kp_auto"}) and get the same result. \code{aes.default} is NOT
#' standardized, for the same reason it isn't in the sibling plotting
#' functions (its own invented parameter-name settings sheet).
#'
#' \strong{Time extraction from the file name} - a fixed, real recorder
#' file-naming convention carried over unchanged from the original script:
#' the segment between the 2nd and last underscore is read as an
#' \code{HHMMSS} clock time (e.g. \code{"MINE_20250109_152740_000.wav"} ->
#' \code{"152740"} -> \code{"15:27:40"}). \strong{Real behavior change from
#' the original script, flagged}: that script always built SOME \code{$time}
#' string via a bare \code{sub()}, even from a file name that didn't
#' actually match this shape (silently producing garbage). This function
#' checks the extracted segment is exactly 6 digits; any row whose file name
#' doesn't match is dropped (not silently mis-parsed), with a console
#' \code{NOTE} reporting how many.
#'
#' \strong{Default date window} - carried over unchanged from the original
#' script: when \code{date.start}/\code{date.end} are left \code{NULL}, both
#' default to July 1 - December 31 of \code{data}'s own earliest
#' \code{groupby.date} year. \strong{Flagged}: this was tuned to one
#' mid-year-deployed dataset (the original script's own comment says so
#' directly), not a generally-correct default - pass \code{date.start}/
#' \code{date.end} explicitly for any other monitoring window.
#'
#' \strong{Aggregation and Y-axis scale - converted from
#' \code{dplyr}/\code{tidyr}/\code{scales} to base R + \code{ggplot2} only},
#' matching how this package's other plotting functions avoid a
#' \code{tidyverse} dependency (e.g.
#' \code{\link{batz.plotactivity_observations}} uses \code{stats::aggregate}
#' directly, not \code{dplyr::count()}). Each row gets a helper \code{$obs =
#' 1} (same convention \code{\link{batz.generate_plotframe.bat}} uses), then
#' \code{stats::aggregate(obs ~ monnight.date, ...)} sums it per night - the
#' original script's own \code{dplyr::count()} equivalent. The Y axis reuses
#' the \code{log1p()}-transform-then-label-with-the-real-count approach
#' already established in \code{\link{batz.plotactivity_observations}}
#' (see that function's own \code{@details}, "Y-axis resolution") instead of
#' \code{scales::pseudo_log_trans()}/\code{scales::squish()} - visually
#' equivalent for a fixed, hand-picked break list (\code{aes.default}'s
#' \code{$y.custom}, default \code{"0;5;30;200;1200;7500"}, matching the
#' original script's own fixed breaks exactly), and avoids adding a new
#' package dependency this project doesn't otherwise need.
#'
#' \strong{Boundary-day bar clipping, found and fixed while testing.} A
#' \code{geom_col()} bar is drawn \code{$bar.width} days wide centered on its
#' own date, so the first/last day in the plotted range has half its bar
#' width extending past \code{date.start}/\code{date.end} - with no buffer,
#' \code{scale_x_date()}'s exact limits clip that edge to \code{NA} and
#' \code{ggplot2} drops it with a console warning. Fixed the same way
#' already established in \code{\link{batz.plotdetections_first.last}} (see
#' that function's own \code{@details}): \code{aes.default}'s
#' \code{$xaxe.date.buffer.days} (default \code{0.5}) pads both ends of the
#' plotted X-axis range.
#'
#' \strong{File naming and settings resolution} - this is a brand-new
#' function, built after Josh's "round nineteen" (2026-09-16) settings/
#' file-naming overhaul (see \code{\link{batz.plotcover_bullseye}}'s own
#' \code{@details} for that overhaul's full history) - so it adopts that
#' mechanism from the start rather than an older one: \code{aes.style}
#' (default \code{"overide.value"}) names a fixed \code{aes.default} column
#' checked before \code{$default.value}, and every saved PNG is named
#' \code{"<project.name>_<site>_<timestamp>.png"}. Unlike this package's
#' \code{fig.list}-driven plot functions, this function has no natural
#' per-call "site"/ARU token of its own (see \code{site.label} above -
#' \strong{a judgment call, flagged}).
#'
#' \strong{Filename collision, found and fixed while testing.} Calling this
#' function and \code{\link{batz.plotactivity_heatmap}} back-to-back with the
#' same \code{project.name}/\code{site.label} (a realistic pattern, since
#' \code{@seealso} below documents chaining them) can land on the same
#' whole-second timestamp, producing an identical filename and silently
#' overwriting one plot's PNG with the other's. Fixed by appending a
#' function-specific suffix to the non-blank \code{site.label} token
#' (\code{"-dailycount"} here, \code{"-heatmap"} in the sibling function), so
#' the two functions' saved files can never collide even when every other
#' naming input matches exactly.
#'
#' \strong{Added 2026-09-22, per Josh's request to audit and extend the
#' snake_case output option package-wide - "A near-no-op for most columns,"
#' flagged rather than skipped.} A recent audit flagged this function,
#' alongside \code{\link{batz.merge_vetted.acoustics}}, as producing a
#' CSV/data-frame output without the \code{snake_case} escape hatch already
#' shipped in \code{\link{batz.generate_plotframe.bat}} (2026-09-21).
#' Unlike that function's \code{plfr.batsummary} (whose whole schema is this
#' package's own invented dot-separated names), the object written here by
#' \code{write.csv.data} is mostly \code{data}'s own EXTERNAL, already-
#' standardized-on-receipt headers (see "Header standardization" above -
#' those already come back lowercase/underscored via
#' \code{standardize.headers()}, e.g. \code{spp.id}'s own default
#' \code{"kpauto"}/\code{filename.col}'s default \code{"filename"}/
#' \code{groupby.date}'s default \code{"monnight"} are all single words
#' with nothing for a second pass of \code{standardize.headers()} to
#' change) plus three columns this function itself adds - \code{$time},
#' \code{$monnight.date}, \code{$obs}. Of those three, only
#' \code{$monnight.date} actually contains this package's dot-separated
#' convention (\code{$time}/\code{$obs} are single words already). So for
#' the realistic default-column-name case, \code{snake_case = TRUE} here
#' changes exactly one header - \code{monnight.date} -> \code{monnight_date}
#' - not a true no-op, but close to one; implemented anyway (rather than
#' skipped) for consistency and package-wide API uniformity, per Josh's
#' explicit request to add it everywhere a data-frame/CSV output uses this
#' package's own dot-style names, however small the practical effect. It is
#' applied only to the copy of \code{data} written to \code{dir.save} as a
#' CSV, immediately before that \code{write.csv()} call - NOT to the
#' \code{$data} element of this function's own return value, since that
#' object is documented (see Value/\code{@seealso}) to feed straight into
#' \code{\link{batz.plotactivity_heatmap}}'s own \code{data} argument, which
#' expects \code{$monnight.date} by its usual dot-separated name; renaming
#' the returned object too would silently break that chaining contract for
#' no benefit (the CSV file is the only "external output" \code{write.csv.
#' data} is actually about).
#'
#' @seealso \code{\link{batz.plotactivity_heatmap}}, which takes this
#'   function's own \code{$data} return value (or any data frame shaped the
#'   same way) as its own \code{data} input.
#'
#' @examples
#' \dontrun{
#' result <- batz.plotactivity_daily.count(
#'   data = readxl::read_excel("mine_only.xlsx"),
#'   aes.default = read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE)
#' )
#' result$ggplot
#'
#' # feed straight into the heatmap sibling function:
#' batz.plotactivity_heatmap(
#'   data = result$data,
#'   aes.default = read.csv("plotopts_heatmap.csv", stringsAsFactors = FALSE)
#' )
#'
#' # snake_case the written CSV (write.csv.data = TRUE) without touching
#' # the returned $data object used above to chain into the heatmap
#' result <- batz.plotactivity_daily.count(
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
                                           trim.noid = TRUE,
                                           date.start = NULL,
                                           date.end = NULL,
                                           aes.default,
                                           project.name = "new.project",
                                           aes.style = "overide.value",
                                           site.label = "",
                                           write.csv.data = FALSE,
                                           dir.save = getwd(),
                                           snake_case = FALSE) {

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
    missing <- setdiff(required, df$parameter)
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
    check.parameters(aes.default, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
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

  data.problem <- check.headers(data, c(spp.id, filename.col, groupby.date), "data")
  if (!is.null(data.problem)) stop(data.problem)
  dup.problem <- check.duplicates(data, "data")
  if (!is.null(dup.problem)) stop(dup.problem)

  get.default <- function(param) {
    row.idx <- which(aes.default$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (aes.style %in% names(aes.default)) {
      override <- aes.default[[aes.style]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  # --- trim.noid: drops rows whose spp.id column reads "noid" (case-
  # insensitive). Same parameter name/meaning as
  # batz.generate_plotframe.bat()/batz.merge_vetted.acoustics2()'s own
  # $trim.noid, per this project's cross-function parameter-naming
  # convention. ---
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
  data$time <- ifelse(time.ok, paste(substr(raw.time, 1, 2), substr(raw.time, 3, 4), substr(raw.time, 5, 6), sep = ":"), NA_character_)
  if (any(!time.ok)) {
    cat(sprintf("NOTE: %d row(s) had a $%s that didn't match the expected '<site>_<date>_<HHMMSS>_<...>' shape - dropped.\n",
                 sum(!time.ok), filename.col))
    data <- data[time.ok, , drop = FALSE]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a parseable $time after filename parsing - nothing to plot.")

  # --- $monnight.date - a flexible date parse of the groupby.date column,
  # same multi-format fallback pattern used elsewhere in this package (e.g.
  # batz.plotactivity_observations()'s parse.flex.date()). Renamed from the
  # original ad hoc script's "monnight_date" to this project's own
  # dot-separated output-column convention. ---
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
  data$monnight.date <- parse.flex.date(data[[groupby.date]])
  if (any(is.na(data$monnight.date))) {
    cat(sprintf("NOTE: %d row(s) had a $%s that could not be parsed as a date - dropped.\n",
                 sum(is.na(data$monnight.date)), groupby.date))
    data <- data[!is.na(data$monnight.date), , drop = FALSE]
  }
  if (nrow(data) == 0) stop("data has 0 rows with a parseable $monnight.date - nothing to plot.")

  # --- date.start/date.end - see @details, "Default date window". ---
  if (is.null(date.start) || is.null(date.end)) {
    year.val <- format(min(data$monnight.date, na.rm = TRUE), "%Y")
    if (is.null(date.start)) date.start <- as.Date(paste0(year.val, "-07-01"))
    if (is.null(date.end)) date.end <- as.Date(paste0(year.val, "-12-31"))
    cat(sprintf("NOTE: date.start/date.end not both given - defaulting to %s to %s (July 1 - Dec 31 of %s).\n",
                 date.start, date.end, year.val))
  } else {
    date.start <- parse.flex.date(as.character(date.start))
    date.end <- parse.flex.date(as.character(date.end))
  }

  data <- data[data$monnight.date >= date.start & data$monnight.date <= date.end, , drop = FALSE]
  if (nrow(data) == 0) {
    stop(sprintf("0 rows of data fall within date.start/date.end (%s to %s) - nothing to plot.", date.start, date.end))
  }

  # --- daily aggregate: a helper $obs = 1 per row (same convention
  # batz.generate_plotframe.bat() uses), summed per $monnight.date - see
  # @details, "Aggregation and Y-axis scale". ---
  data$obs <- 1
  daily <- stats::aggregate(obs ~ monnight.date, data = data, FUN = sum)
  daily <- daily[order(daily$monnight.date), , drop = FALSE]

  cat(sprintf("Rows after trim.noid + date filter: %d (%d distinct monitoring night(s)).\n", nrow(data), nrow(daily)))

  ggplot.obj <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    y.custom <- suppressWarnings(as.numeric(strsplit(get.default("y.custom"), ";", fixed = TRUE)[[1]]))
    y.custom <- sort(unique(y.custom[!is.na(y.custom)]))
    if (length(y.custom) < 2) {
      stop("aes.default's $y.custom must have at least 2 semicolon-separated numbers (e.g. \"0;5;30;200;1200;7500\").")
    }
    y.upper <- max(c(y.custom, daily$obs), na.rm = TRUE)
    daily$obs.plot <- log1p(daily$obs)
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

    ggplot.obj <- ggplot2::ggplot(daily, ggplot2::aes(x = monnight.date, y = obs.plot)) +
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
    fname <- sprintf("%s_%s_%s.png", project.name, site.token, format(Sys.time(), "%Y%m%d_%H%M%S"))
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
    csv.name <- file.path(dir.save, sprintf("%s_%s_%s.csv", project.name, site.token, format(Sys.time(), "%Y%m%d_%H%M%S")))
    ## snake_case output option (per Josh's request to audit and extend
    ## this package-wide, 2026-09-22) - applied only to this written copy,
    ## right before the write, never to the `data` object itself (which is
    ## still used, unchanged, in the return value below). See @param
    ## snake_case and @details, "A near-no-op for most columns".
    csv.data <- data
    if (snake_case) names(csv.data) <- standardize.headers(names(csv.data))
    utils::write.csv(csv.data, csv.name, row.names = FALSE)
    cat("Saved:", csv.name, "\n")
  }

  invisible(list(data = data, daily = daily, ggplot = ggplot.obj))
}

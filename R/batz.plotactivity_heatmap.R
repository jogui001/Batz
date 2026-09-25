#' Plot a 30-minute-interval bat activity heatmap per monitoring night
#'
#' Given per-detection data with a monitoring-night date and a time-of-night
#' (e.g. the \code{$data} returned by
#' \code{\link{batz.plotactivity_daily.count}}), bins every detection into
#' fixed-width intervals of the monitoring night (noon -> midnight -> the
#' following 11:59am) and draws a night-by-bin heatmap of detection counts,
#' colored on a sequential scale capped at a fixed maximum. Replaces the ad
#' hoc \code{activity_heatmap.R} script this function was converted from -
#' see Details for what changed along the way.
#'
#' @param data A data frame with (at least) a monitoring-night date column
#'   and a time-of-night column - see \code{date.col}/\code{time.col}.
#'   Header standardization does \strong{not} mechanically apply here (see
#'   Details) - this is expected to be the already-standardized output of
#'   an upstream \code{batz} function, most naturally
#'   \code{\link{batz.plotactivity_daily.count}}'s own \code{$data}.
#' @param date.col Character, default \code{"date.monitoringnight"} (renamed
#'   from \code{"monnight.date"} in round twenty-four, then from
#'   \code{"date.mon"} to \code{"date.monitoringnight"} in round twenty-five,
#'   2026-09-25 - see \code{@details}). Name of the column in \code{data} holding the
#'   monitoring-night date (a \code{Date}, or a string \code{as.Date()}
#'   can parse in its default ISO form).
#' @param time.col Character, default \code{"time"}. Name of the column in
#'   \code{data} holding each detection's own clock time, as
#'   \code{"HH:MM"} or \code{"HH:MM:SS"}.
#' @param bin.minutes Single positive number, default \code{30}. Width of
#'   each time-of-night bin, in minutes - must divide evenly into 1440
#'   (e.g. 15, 20, 30, 60); an unrecognized value stops with a clear error.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_heatmap.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value}; an
#'   \code{$overide.value} column (blank, user-fillable) and \code{$notes}
#'   are optional - same \code{$aes.style}-driven resolution as this
#'   package's other plot functions (see Details).
#' @param project.name Character, default \code{"new.project"}. The first
#'   part of every saved file's name:
#'   \code{"<project.name>_<site>_<daterange>_<timestamp>.png"} - matches
#'   the unified file-naming convention already used by every other
#'   \code{batz} plot function with a date range (see \code{@details},
#'   "File naming (round twenty-three)").
#' @param aes.style Character, default \code{"overide.value"}. Names which
#'   column of \code{aes.default} is checked FIRST for each parameter
#'   (falling back to \code{$default.value} when that column doesn't exist,
#'   or is blank for that row).
#' @param site.label Character, default \code{""} (blank). The \code{<site>}
#'   token in the saved file name (see \code{project.name} above); falls
#'   back to the literal \code{"heatmap"} when left blank - same judgment
#'   call as \code{\link{batz.plotactivity_daily.count}}'s own
#'   \code{site.label} (see that function's own \code{@details}).
#' @param dir.save Character, default \code{getwd()}. Directory the
#'   generated PNG is saved into.
#'
#' @return Invisibly, a list with \code{data} (the complete night x bin
#'   grid actually plotted: a column named after \code{date.col} (default
#'   \code{$date.monitoringnight}), \code{$bin.index},
#'   \code{$n} - one row per night per bin, zero-filled where a night had no
#'   detections in that bin) and \code{ggplot} (the ggplot object, only
#'   populated when \code{ggplot2} is available).
#'
#' @details
#' \strong{Converted from \code{activity_heatmap.R}, an ad hoc procedural
#' script, into a reusable \code{batz} function - not a full from-scratch
#' spec.} The original script's actual behavior (re-anchor time-of-night to
#' a noon-to-noon monitoring-night axis, bin into 30-minute intervals,
#' complete the night x bin grid with zeros, draw a capped sequential-blue
#' heatmap with a horizontal colorbar) is preserved; what changed is
#' packaging it to match every other \code{batz} plot function's
#' conventions - a generalized \code{bin.minutes} instead of a hardcoded 30,
#' an \code{aes.default} settings file instead of hardcoded plot constants,
#' the unified \code{project.name}/\code{dir.save} file-naming/save
#' mechanism, and an X-axis range/labeling scheme generalized to whatever
#' date range \code{data} actually covers (see "X-axis range and labels"
#' below), rather than the original script's own hardcoded March 2025 -
#' January 2026 window.
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' does not mechanically apply here, flagged not silently skipped} - same
#' reasoning as \code{\link{batz.plotactivity_observations}}/
#' \code{\link{batz.plotdetections_first.last}}: \code{data} is expected to
#' already be the OUTPUT of an upstream \code{batz} function (most
#' naturally \code{\link{batz.plotactivity_daily.count}}'s own \code{$data},
#' whose \code{$date.monitoringnight}/\code{$time} columns are this package's own
#' invented output schema, not raw external headers) rather than a file
#' this function loads itself. \code{date.col}/\code{time.col} are still
#' validated by a plain required-header/duplicate-column check, same as
#' every other \code{batz} plot function does for its own inputs.
#'
#' \strong{Monitoring night convention} - unchanged from the original
#' script: noon (\code{12:00}) is bin 0, midnight is the bin at the exact
#' midpoint (\code{n.bins / 2}), and 11:59 the following morning is the
#' last bin - matching how every other \code{batz} plotting function in
#' this package treats a "monitoring night" as running noon-to-noon for
#' these nocturnal-animal records.
#'
#' \strong{Complete night x bin grid, converted from \code{tidyr::complete()}
#' to base R} (\code{expand.grid()} + \code{merge()}), matching how this
#' package's other plotting functions avoid a \code{dplyr}/\code{tidyr}
#' dependency (e.g.
#' \code{\link{batz.plotactivity_observations}}/
#' \code{\link{batz.plotactivity_daily.count}} both use
#' \code{stats::aggregate()} directly) - every night present in \code{data}
#' gets a row for every one of the \code{1440 / bin.minutes} bins, with a
#' missing night/bin combination filled to \code{0}, same as the original
#' script's own \code{tidyr::complete(..., fill = list(n = 0))}.
#'
#' \strong{Fill scale - capped/squished without a new \code{scales}
#' dependency.} The original script used
#' \code{scales::squish()}/\code{scales::pseudo_log_trans()}-style handling
#' to cap the color scale at a fixed maximum with a \code{"100+"} open-ended
#' top label. Since a fill scale (unlike a Y-axis position scale) only ever
#' needs the color, not the exact value, capping is done by simply clamping
#' the plotted value with \code{pmin(n, fill.max)} before mapping it to
#' \code{fill} - visually identical to \code{oob = scales::squish} for this
#' purpose, and avoids adding a package dependency this project doesn't
#' otherwise need. \code{aes.default}'s \code{$fill.max} (default \code{100})
#' and \code{$fill.colors} (a semicolon-separated hex list, default the same
#' 7-color sequential blue ramp the original script used, credited there as
#' the \code{dataviz} skill's default palette) control this.
#'
#' \strong{Filename collision, found and fixed while testing.} This function
#' and \code{\link{batz.plotactivity_daily.count}} share the same
#' \code{"<project.name>_<site>_<timestamp>.png"} naming scheme, so calling
#' both back-to-back with the same \code{project.name}/\code{site.label}
#' within the same whole second (a realistic pattern, since they're commonly
#' chained - see \code{@seealso} below) produced identical filenames and one
#' PNG silently overwrote the other. Fixed by appending a function-specific
#' suffix to the non-blank \code{site.label} token (\code{"-heatmap"} here,
#' \code{"-dailycount"} in the sibling function).
#'
#' \strong{X-axis range and labels - generalized, a real change from the
#' original script, flagged.} The original script hardcoded its X-axis
#' range (March 2025 - January 2026) and which specific months got a text
#' label (only April/June/August/October/December) - both tuned by eye to
#' one specific, multi-year, densely-sampled dataset, not a generally
#' correct default. This function instead spans whatever date range
#' \code{data} actually covers (from the first day of \code{data}'s
#' earliest month to the first day of its latest month) and labels EVERY
#' month tick by default (\code{"\%b"}) - simpler and safe for any dataset,
#' at the cost of a busier X axis on a long, densely-sampled one. A minor
#' (unlabeled) tick still marks every month in between, same as the
#' original script.
#'
#' \strong{Header check for \code{data} now canonicalizes casing/separator
#' differences (per Josh, 2026-09-22 - auditing and extending the
#' 2026-09-21 header-canonicalization fix package-wide).} The required-header
#' check for \code{data} previously did a literal \code{setdiff()} against
#' \code{date.col}/\code{time.col}, which meant a real, present column would
#' be falsely reported "missing" if it round-tripped through anything that
#' changes case or separator style (e.g. a CSV save/reload, or
#' \code{standardize.headers()} itself) before reaching this function - even
#' though \code{data} is still expected to be the OUTPUT of an upstream
#' \code{batz} function (see "Header standardization" above), just not
#' necessarily byte-for-byte identical to it anymore. This now goes through
#' the shared \code{canonicalize.headers()} helper instead (the same fix
#' already applied to \code{\link{batz.generate_plotframe.bat}}/
#' \code{\link{batz.plotdetections_first.last}}/
#' \code{\link{batz.plotactivity_observations}} on 2026-09-21): it matches
#' \code{data}'s real columns against \code{date.col}/\code{time.col}
#' tolerant of case/separator differences, then renames only the MATCHED
#' columns of a local copy back to this function's own \code{date.col}/
#' \code{time.col} spelling before anything downstream touches them by name -
#' \code{data} is deliberately NOT run through \code{standardize.headers()}
#' wholesale, since that would rename every column and break this function's
#' expectation of specific dot-style names. Separately, when both
#' \code{aes.default} and \code{data} are missing headers in the same call,
#' the combined error message now joins the two frames' complaints with a
#' blank line (\code{"\n\n"}) rather than a single newline, matching the
#' blank-line convention already used elsewhere in this package.
#'
#' \strong{Timestamp format (round twenty-two), 2026-09-24, per Josh's
#' package-wide request ("Update all functions that save files or charts:
#' ... <timestamp> format match this format: YYYYMMDDHHHMMSS"):} the
#' \code{<timestamp>} token in the saved file name is now built as
#' \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")} (14 digits, no separator
#' between the date and time halves) instead of \code{"\%Y\%m\%d_\%H\%M\%S"}
#' - see \code{\link{batz.plotsm4_heatmap}}'s own \code{@details} entry of
#' the same name for the full reasoning (including the \code{"YYYYMMDDHHHMMSS"}
#' typo read/flag) and package-wide scope.
#'
#' \strong{File naming (round twenty-three), 2026-09-25, per Josh's
#' correction to round twenty-two ("batz.plotactivity_heatmap() does have a
#' date range, this is monnight.date"):} round twenty-two's entry
#' immediately above originally said this function has no
#' \code{date.start}/\code{date.end} parameter and so was left out of the
#' "charts that have a date range" bucket - \strong{that was wrong, and is
#' corrected here.} \code{data}'s own \code{date.col} column (default
#' \code{"monnight.date"}) IS a real date range, it just isn't passed as a
#' pair of explicit \code{date.start}/\code{date.end} arguments the way
#' \code{\link{batz.plotactivity_daily.count}}'s is. The saved file name is
#' now \code{"<project.name>_<site>_<daterange>_<timestamp>.png"}, matching
#' every other date-ranged \code{batz} plot function, where
#' \code{<daterange>} is built from the actual minimum/maximum
#' \code{date.col} value present in \code{data} (\code{format(min(all.nights),
#' "\%Y\%m\%d")}/\code{format(max(all.nights), "\%Y\%m\%d")}, reusing the same
#' \code{all.nights} vector the night x bin grid itself is built from - see
#' "Complete night x bin grid" above) rather than a per-call
#' \code{date.start}/\code{date.end} pair, since this function has none of
#' its own.
#'
#' \strong{\code{date.col}'s default renamed \code{"monnight.date"} ->
#' \code{"date.mon"} (round twenty-four), 2026-09-25, per Josh's correction
#' and follow-up question.} This entry originally (round twenty-three)
#' left \code{date.col}'s default as \code{"monnight.date"}, flagging a
#' rename to \code{date.mon} as a collision with an already-real,
#' differently-scoped \code{date.mon} used elsewhere in the package
#' (\code{\link{batz.merge_vetted.acoustics}}/
#' \code{\link{batz.generate_plotframe.bat}}/
#' \code{\link{batz.plotsm4_heatmap}}). Josh then asked why a collision
#' would occur if \code{date.mon} and \code{monnight.date} hold the same
#' value per record - and he's right: every one of those other functions'
#' \code{date.mon} means exactly the same thing this function's
#' \code{monnight.date} always meant (the monitoring-night date a
#' detection belongs to), not a different, unrelated concept that happens
#' to share a name. \code{date.col}'s default is renamed here to
#' \code{"date.mon"}, matching the corresponding rename in
#' \code{\link{batz.plotactivity_daily.count}} (see that function's own
#' \code{@details}, "Output column renamed"), which keeps the two
#' functions' producer/consumer contract intact. See that same entry for
#' the one pre-existing (not introduced by this rename), non-blocking
#' wrinkle: the various \code{date.mon} columns elsewhere in the package
#' don't all share the same R type (raw character, noon-anchored
#' \code{POSIXct}, or plain \code{Date}) - this function's own
#' \code{date.col} handling coerces its input with \code{as.Date()}
#' regardless (see code below), so it accepts any of those representations
#' the same way it always has.
#'
#' \strong{\code{date.col}'s default renamed again, \code{"date.mon"} ->
#' \code{"date.monitoringnight"} (round twenty-five), 2026-09-25, per Josh:
#' "I changed my mine and want to use date.monitoringnight instead of
#' date.mon to be more consistent with collaborators."} This supersedes the
#' round twenty-four rename immediately above - \code{date.mon} is now
#' itself a retired spelling, folded into the package's header-rename
#' reference table (\code{\link{batz.generate_headers.acceptold}}) alongside
#' \code{monnight.date}/\code{mon.ngh}/\code{monitoringnight}, all pointing
#' at this same new name. Applied consistently, the same day, everywhere
#' this concept appears package-wide (see
#' \code{\link{batz.plotactivity_daily.count}}'s own \code{@details} for the
#' full list of sibling functions updated alongside this one), rather than
#' leaving this function and \code{batz.plotactivity_daily.count} on the new
#' name while every other function kept \code{date.mon} - which would have
#' silently recreated the exact cross-function mismatch this whole
#' back-and-forth has been trying to resolve.
#'
#' \strong{Internal hardcoded \code{"monnight.date"} literals fixed to use
#' \code{date.col} dynamically, same round.} While making the rename above,
#' a separate, pre-existing latent bug was found and fixed: this function's
#' internal night x bin grid (\code{expand.grid()}/\code{merge()}) and its
#' \code{ggplot2::aes()} mapping had \code{"monnight.date"} hardcoded
#' literally in three places, instead of referencing the \code{date.col}
#' variable - so a caller who passed a non-default \code{date.col} would
#' have had \code{data[[date.col]]} read correctly everywhere above that
#' point, but then hit a "not all arguments have the same length"/missing-
#' column error at the grid-building step, since the grid was always keyed
#' on the literal name \code{"monnight.date"} no matter what \code{date.col}
#' actually was. All three spots now build their column name from
#' \code{date.col} itself (\code{stats::setNames()} for the
#' \code{aggregate()}/\code{expand.grid()} calls, \code{.data[[date.col]]}
#' inside \code{aes()}), so \code{$data}'s own returned date column is
#' now genuinely named after whatever \code{date.col} was passed as,
#' matching this parameter's own documented contract for the first time.
#'
#' @seealso \code{\link{batz.plotactivity_daily.count}}, whose own
#'   \code{$data} return value is the natural \code{data} input here.
#'
#' @examples
#' \dontrun{
#' daily.result <- batz.plotactivity_daily.count(
#'   data = readxl::read_excel("mine_only.xlsx"),
#'   aes.default = read.csv("plotopts_dailycount.csv", stringsAsFactors = FALSE)
#' )
#' heatmap.result <- batz.plotactivity_heatmap(
#'   data = daily.result$data,
#'   aes.default = read.csv("plotopts_heatmap.csv", stringsAsFactors = FALSE)
#' )
#' heatmap.result$ggplot
#' }
#'
#' @export
batz.plotactivity_heatmap <- function(data,
                                       date.col = "date.monitoringnight",
                                       time.col = "time",
                                       bin.minutes = 30,
                                       aes.default,
                                       project.name = "new.project",
                                       aes.style = "overide.value",
                                       site.label = "",
                                       dir.save = getwd()) {

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
    check.headers(aes.default, AES.DEFAULT.REQUIRED, "aes.default"),
    check.parameters(aes.default, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
    check.duplicates(aes.default, "aes.default"),
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
  raw.counts <- stats::aggregate(list(n = rep(1, nrow(data))),
                                  by = stats::setNames(list(data[[date.col]], bin.index), c(date.col, "bin.index")),
                                  FUN = sum)
  # --- complete grid: every night x every bin, missing = 0 - see @details,
  # "Complete night x bin grid". ---
  full.grid <- stats::setNames(expand.grid(all.nights, all.bins), c(date.col, "bin.index"))
  counts <- merge(full.grid, raw.counts, by = c(date.col, "bin.index"), all.x = TRUE)
  counts$n[is.na(counts$n)] <- 0

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
    counts$n.capped <- pmin(counts$n, fill.max)

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
        axis.title       = ggplot2::element_text(face = "plain", size = as.numeric(get.default("axis.title.size"))),
        axis.text        = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
        axis.text.x      = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
        plot.title       = ggplot2::element_text(size = as.numeric(get.default("plot.title.size"))),
        plot.margin      = ggplot2::margin(t = 5.5, r = 20, b = 5.5, l = 5.5),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
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

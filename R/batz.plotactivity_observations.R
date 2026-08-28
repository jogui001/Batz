#' Plot the nightly number of observations per species
#'
#' Generates the standard report plot showing, for every species (plus an
#' "All detections" panel and an optional overlaid 40kHzMyo indicator), the
#' number of observations (\code{$obs}) recorded each monitoring night, as a
#' bar per night per panel. One plot is produced per row of \code{fig.list}
#' whose \code{$plot.type} is \code{"call.observations"} (see Details for why
#' this value was chosen and how to change it).
#'
#' @param data A data frame of already-summarized per-species, per-night
#'   observation counts. Must have \code{$spp.id}, \code{$date},
#'   \code{$aru.groupby}, \code{$obs}.
#' @param fig.list A data frame listing the plot(s) to generate - one row
#'   per plot. Must have \code{$plot.type}, \code{$plot.name}, \code{$facet},
#'   \code{$facet.set}, \code{$MYSO}, \code{$Alldect}, \code{$facet.panel},
#'   \code{$40khzmyo}, \code{$facet.label}, \code{$plot.set},
#'   \code{$date.format}, \code{$date.start}, \code{$date.end},
#'   \code{$xaxe.interval}. Column names must be unique. Four further
#'   columns are read PER-ROW if present but are entirely optional (each
#'   falls back to \code{aes.default} when blank or the column doesn't
#'   exist at all - see Details): \code{$Yaxe.trans} (\code{"none"}/
#'   \code{"log"}/\code{"log10"}), \code{$y.scale} (\code{"regular"}/
#'   \code{"rounded"}/\code{"custom"}), \code{$y.custom} (semicolon-separated
#'   break values, only read when \code{$y.scale = "custom"}), and
#'   \code{$ymax} (the top value plotted on the Y axis).
#' @param suntimes A data frame of sunrise/sunset times, e.g. the output of
#'   \code{batz.suntimes_generate()}. Must have \code{$aru}, \code{$date},
#'   \code{$date.mon}, \code{$sunregion}, \code{$time.zone},
#'   \code{$sunregion.type}, \code{$schedual1}, \code{$schedual2},
#'   \code{$suns}, \code{$suns.unix}, \code{$sunr}, \code{$sunr.unix},
#'   \code{$sunr.mon}, \code{$sunr.mon.unix}. \strong{Accepted and header-
#'   checked, but not otherwise used yet this iteration} - see Details.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_callobs.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value};
#'   \code{$notes} and any \code{project.name}-matching override column(s)
#'   are optional.
#' @param project.name Character, default \code{""}. Must EXACTLY match a
#'   real column name already present in \code{aes.default} - see
#'   \code{batz.plotdetections_first.last()}'s own \code{@param} docs for
#'   the full three-tier precedence (this function resolves settings the
#'   same way: \code{fig.list} row > \code{project.name} column >
#'   \code{$default.value}).
#' @param dir.save Character, default \code{getwd()}. Directory every
#'   generated PNG is saved into (each file's own name still comes from
#'   \code{aes.default}'s \code{$output.filename.pattern}).
#'
#' @return Invisibly, a list with \code{plots} (one entry per generated
#'   plot's prepared data) and \code{ggplots} (the corresponding ggplot
#'   objects, only populated when the \code{ggplot2} package is available).
#'
#' @details
#' \strong{Iteration 1 ("basic layout"), built 2026-08-28 per Josh's own
#' framing that this function would be developed iteratively, copying the
#' structure/steps of \code{batz.plotdetections_first.last()} and modifying
#' it for a count-based Y axis instead of a time-of-night Y axis.} Carried
#' over UNCHANGED from that function: header/duplicate-column validation,
#' settings resolution (\code{fig.list} row > \code{project.name} column >
#' \code{$default.value}), the New-England-special-case \code{$facpan} list,
#' \code{$MYSO}/\code{$Alldect}/\code{$40khzmyo} panel-building logic,
#' facet labeling and canonicalization via
#' \code{batz.batusa_recode.names()}, \code{$plot.order} panel ordering, the
#' \code{job.key}/\code{job.label} list-keying fix (rows sharing
#' \code{$plot.name} each still get their own plot), and the exact-full-row
#' \code{fig.list} duplicate-removal step (both fixed in that function after
#' real bugs Josh hit - see its own \code{@details} for the full history;
#' both are included here from the start rather than waiting to be
#' rediscovered). REMOVED entirely: the Dawn/Dusk/Midnight reference lines
#' and all of the time-of-night (\code{mins2.noon.min}/\code{max},
#' sunrise/sunset) math that supported them - per Josh's explicit
#' instruction ("There are no DAWN, DUSK or Midnight variables to be
#' ploted"). Each bar is simply one \code{fig.list}-matched data row's own
#' \code{$obs} value, plotted at its \code{$date} - no per-night aggregation
#' happens inside this function (a night's "All detections" bar is
#' \code{data}'s own pre-computed \code{$spp.id = "All Detections"} row for
#' that night/ARU, exactly parallel to how \code{batz.plotdetections_first.last()}
#' uses a pre-computed \code{"All detections"} row rather than summing
#' individual species rows itself).
#'
#' \strong{\code{$plot.type = "call.observations"}} was chosen for this
#' function's own \code{fig.list} rows (a judgment call, since Josh's spec
#' didn't name one) - \code{fig.list.csv} is shared across every \code{batz}
#' plotting function, each reading only the rows matching its own
#' \code{$plot.type} value and skipping (with a console \code{NOTE}, not an
#' error) any row belonging to another function, exactly like
#' \code{batz.plotdetections_first.last()} already does for
#' \code{"bat.detection"}. \strong{Josh's real \code{fig.list.csv} does not
#' yet have a row with this \code{$plot.type} value} - add one (with
#' \code{$Yaxe.trans}/\code{$y.scale}/\code{$y.custom}/\code{$ymax} filled
#' in as needed - those four columns already exist as blank headers in the
#' delivered file) before calling this function against real data. If a
#' different \code{$plot.type} string is wanted instead, it only needs to be
#' changed in one place (the \code{PLOT.TYPE} constant near the top of this
#' function's code).
#'
#' \strong{\code{suntimes} is accepted and header-checked but not otherwise
#' used in this iteration.} Josh's own spec listed it as an input alongside
#' \code{data}/\code{fig.list}/\code{aes.default}, but with no Dawn/Dusk/
#' Midnight lines to compute, nothing in this first pass actually needs
#' sunrise/sunset data - kept in the signature (rather than dropped) so
#' existing call sites built against the same four core inputs as the
#' sibling function keep working, and so a future iteration (e.g. excluding
#' nights with no suntimes coverage, or a twilight-shading feature) can add
#' real use of it without an interface change. \strong{Flagging this
#' explicitly for Josh}: if \code{suntimes} should actually do something in
#' this plot (e.g. gray out or omit un-monitored nights), say so and it can
#' be wired in.
#'
#' \strong{Y-axis resolution - the newest, most detailed part of this
#' function - implements Josh's spec as follows:} \code{$Yaxe.trans}
#' (\code{"none"} default / \code{"log"} / \code{"log10"}) picks a transform
#' applied to \code{$obs} before it's used as a bar's plotted height;
#' \strong{\code{"log"}/\code{"log10"} are implemented as \code{log1p(obs)}/
#' \code{log10(obs + 1)}, not a bare \code{log(obs)}/\code{log10(obs)}} - a
#' deliberate choice (Josh's spec didn't say how to handle a night with 0
#' observations, but \code{data} can and does have real detections that
#' would sum to a whole-number 0 for some species/night combinations
#' logically, and a bare \code{log(0)} is \code{-Inf}, which would break the
#' axis) - verified against Josh's own target image
#' (\code{"Number of bat calls detected.png"}): its Y-axis breaks
#' (\code{0, 2, 8, 25, 75, 230}) are spaced almost exactly evenly once run
#' through \code{log1p()} (successive gaps of about 1.10, 1.10, 1.06, 1.07,
#' 1.11 log-units), confirming this is the transform that image's axis
#' actually used, not a coincidence. \code{$loglabels} (\code{FALSE}
#' default) then controls whether each break is LABELED with the real
#' \code{$obs}-scale number (\code{FALSE}) or the transformed value itself
#' (\code{TRUE}) - it only has any effect when \code{$Yaxe.trans} isn't
#' \code{"none"}, per Josh's spec.
#'
#' \code{$y.scale} picks which raw (pre-transform) values become breaks:
#' \code{"regular"} places them at the exact 0/25/50/75/100\% points of
#' \code{0}-\code{$ymax}; \code{"rounded"} places them at those same four
#' points but rounds each to the nearest whole number first (matters once
#' \code{$ymax} isn't a multiple of 4, e.g. \code{$ymax = 230} gives a raw
#' 25\% point of \code{57.5}, which \code{"rounded"} shows as \code{58});
#' \code{"custom"} ignores the 0/25/50/75/100\% computation entirely and
#' uses whatever numbers are in \code{$y.custom} instead (semicolon-
#' separated, e.g. \code{"0;2;8;25;75;230"} - the same convention as
#' \code{$yaxe.break.labels} in \code{batz.plotdetections_first.last()}),
#' matching Josh's target image exactly. \strong{Josh's own spec text marked
#' BOTH \code{"regular"} and \code{"rounded"} as \code{"(default)"} - almost
#' certainly a copy/paste slip, since only one can be the actual default.}
#' \code{"regular"} was picked as the real default here (it's the option
#' listed first, and is the more literal/simpler reading of "0,0.25,0.5,
#' 0.75,1"); \strong{Josh: please confirm this is the one you meant as the
#' default}, since the two only visibly differ when \code{$ymax} isn't
#' evenly divisible by 4.
#'
#' \code{$ymax} sets the top of the Y axis. When a \code{fig.list} row
#' leaves it blank (or gives something that isn't a usable positive
#' number), it's auto-computed as \code{max($obs)} across that plot's own
#' filtered data, with a console \code{NOTE} reporting the value used -
#' \strong{a placeholder for this first iteration, flagged for Josh}: this
#' gives the axis exactly enough headroom to fit the tallest bar and no
#' more, which may look visually tight; a fixed padding percentage (e.g.
#' 10\% above the max) could be added in a later iteration if wanted. The
#' actual plotted axis upper limit is never allowed to clip a real bar or a
#' user-supplied \code{$y.custom} value even if \code{$ymax} itself is
#' smaller than one of those (\code{max($ymax, $y.custom values, $obs)} is
#' used as the true limit) - this protects against a \code{$ymax}/
#' \code{$y.custom} mismatch silently cutting off part of the plot.
#'
#' \strong{A real data/reference-table mismatch was found and worked around
#' while building this against Josh's real \code{plfr.bats.csv}:} its 40kHz-
#' Myotis rows use \code{$spp.id = "40kMyo"}, but
#' \code{batz.batusa_recode.names()}'s own reference table's matching
#' non-species category label is \code{"40KHzMyo"} - these do NOT match
#' after that function's own case/dash/underscore-folding normalization
#' (folding case doesn't add the missing \code{"Hz"}), so running
#' \code{"40kMyo"} through \code{batz.batusa_recode.names()} alone leaves it
#' unmatched (passed through unchanged, plus a console \code{WARNING} from
#' that function). Worked around, scoped to this function only (the shared
#' recode reference table itself was not touched): every data row's raw
#' \code{$spp.id} is checked directly (case/punctuation-insensitively)
#' against both \code{"40khzmyo"} and \code{"40kmyo"} before recoding, and
#' either spelling is treated as the 40kHzMyo category for the bar-
#' overlay/legend logic, regardless of what \code{batz.batusa_recode.names()}
#' itself would have matched. \strong{Flagging for Josh}: worth deciding
#' whether \code{"40kMyo"} should be renamed to \code{"40KHzMyo"} at the
#' source (in whatever produces \code{plfr.bats.csv}), or whether
#' \code{"40kMyo"} should be added as a recognized alias directly in
#' \code{batz.batusa_recode.names()}'s own reference table (\code{NAbat.names.csv})
#' - either fix would make this function's own local workaround
#' unnecessary, but isn't required for this function to work correctly as
#' delivered.
#'
#' \strong{\code{$output.filename.pattern}'s default value} in
#' \code{plotopts_callobs.csv} (\code{"Number of bat calls detected at
#' <ARU> between <date.start> and <date.end> <timestamp>.png"}) was chosen
#' to match the literal file name of Josh's own target output image
#' (\code{"Number of bat calls detected.png"}), with the same \code{<ARU>}/
#' \code{<date.start>}/\code{<date.end>}/\code{<timestamp>} placeholders
#' \code{batz.plotdetections_first.last()} already uses.
#'
#' \strong{Real bug found and fixed while visually comparing a render
#' against Josh's target image: the 40kHzMyo bar overlay was invisible.}
#' The first version of this function plotted \code{"All detections"} and
#' \code{"40kHzMyo"} bars for the same night in ONE \code{geom_col()} call
#' (\code{aes(fill = bar.type)}) - the data was correct (verified
#' separately via \code{pd$bar.type}), but ggplot2 draws a single layer's
#' rows in the fill aesthetic's own factor-level order, which defaults to
#' alphabetical: \code{"40kHzMyo"} sorts before \code{"All detections"},
#' so the black bar was drawn FIRST/underneath and the gray bar (same
#' night, same width/x-position, fully opaque) completely covered it every
#' time - invisible in every render even though the underlying data was
#' right. Only caught by actually rendering and looking at the image, not
#' from data-level checks alone (mirrors the same "verify by rendering, not
#' just reading the code" lesson found repeatedly while building
#' \code{batz.plotdetections_first.last()}). Fixed by splitting into two
#' explicit \code{geom_col()} layers, each pre-filtered to one
#' \code{bar.type} and added to the plot in bottom-to-top order (all-
#' detections layer first, 40kHzMyo layer second) - this guarantees the
#' draw order regardless of the fill factor's own alphabetical sort.
#' Re-verified by rendering: the black segment now shows correctly at the
#' base of the gray bar on every night with a real 40kHzMyo observation.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.plotactivity_observations()}: family = "plotactivity" (the
#' verb "plot" is baked into the family name), subject = "observations"
#' (the number of observations/calls per night). \strong{Requested as
#' \code{batz.plotactivlty_observations()} - "plotactivlty" was a typo for
#' "plotactivity" (transposed letters), fixed here; nothing else about the
#' name changed. Please flag if a different family/subject split was
#' actually intended.}
#'
#' @examples
#' \dontrun{
#' result <- batz.plotactivity_observations(
#'   data = plfr.bats,
#'   fig.list = fig.list,
#'   suntimes = aru.suntimes,
#'   aes.default = plotopts.callobs
#' )
#' result$ggplots[[1]]
#' }
#'
#' @export
batz.plotactivity_observations <- function(data, fig.list, suntimes,
                                            aes.default, project.name = "",
                                            dir.save = getwd()) {

  # See @details above for why this value was chosen and how to change it.
  PLOT.TYPE <- "call.observations"

  DATA.REQUIRED <- c("spp.id", "date", "aru.groupby", "obs")
  SUNTIMES.REQUIRED <- c("aru", "date", "date.mon", "sunregion", "time.zone",
                          "sunregion.type", "schedual1", "schedual2", "suns",
                          "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
  FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                          "Alldect", "facet.panel", "40khzmyo", "facet.label",
                          "plot.set", "date.format", "date.start", "date.end",
                          "xaxe.interval")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
    "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
    "panel.border.linewidth", "legend.position",
    "xaxe.interval", "xaxe.title", "xaxe.date.buffer.days", "yaxe.title",
    "Yaxe.trans", "loglabels", "y.scale", "ymax", "bar.width",
    "bar.alldetections.fill", "bar.40khzmyo.fill", "bar.fill.legend.title",
    "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
    "output.filename.pattern", "plot.width", "plot.height"
  )

  check.headers <- function(df, required, label) {
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
    }
    NULL
  }

  check.parameters <- function(df, required, label) {
    if (!("parameter" %in% names(df))) return(NULL)  # already reported by check.headers above
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
    check.headers(data, DATA.REQUIRED, "data"),
    check.headers(suntimes, SUNTIMES.REQUIRED, "suntimes"),
    check.headers(fig.list, FIG.LIST.REQUIRED, "fig.list"),
    check.headers(aes.default, AES.DEFAULT.REQUIRED, "aes.default"),
    check.parameters(aes.default, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
    check.duplicates(data, "data"),
    check.duplicates(suntimes, "suntimes"),
    check.duplicates(fig.list, "fig.list"),
    check.duplicates(aes.default, "aes.default")
  )
  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n"))
  }

  unquote <- function(x) {
    x <- trimws(as.character(x))
    gsub('^"(.*)"$', "\\1", x)
  }

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

  get.default <- function(param) {
    row.idx <- which(aes.default$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (nzchar(project.name) && project.name %in% names(aes.default)) {
      override <- aes.default[[project.name]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  get.setting <- function(job, param) {
    if (param %in% names(job)) {
      v <- job[[param]]
      if (!is.null(v) && !is.na(v) && nzchar(trimws(as.character(v)))) {
        return(as.character(v))
      }
    }
    get.default(param)
  }

  NE.ALIASES <- c("new england", "ne")
  SPECIAL.FACPAN <- c("Big brown bat", "Eastern red bat", "Hoary bat", "Silver-haired bat",
                       "Eastern small-footed myotis", "Little brown bat",
                       "Northern long-eared bat", "Tri-colored bat")

  # See @details above ("A real data/reference-table mismatch...") for why
  # this exists: Josh's real data uses "40kMyo", batz.batusa_recode.names()'s
  # own reference table uses "40KHzMyo" - the two don't match after that
  # function's own normalization, so this function checks the RAW $spp.id
  # itself, independent of recode().
  norm.simple <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
  KHZ.ALIASES <- c("40khzmyo", "40kmyo")

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("fig.list has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

  # Exact full-row fig.list duplicates are collapsed to their first
  # occurrence before any plotting happens - same fix as
  # batz.plotdetections_first.last() (see that function's own @details for
  # the real bug that led to it); included here from the start.
  n.jobs.before.dedup <- nrow(jobs)
  jobs <- jobs[!duplicated(jobs), , drop = FALSE]
  n.fig.list.duplicates.removed <- n.jobs.before.dedup - nrow(jobs)
  if (n.fig.list.duplicates.removed > 0) {
    cat(sprintf("NOTE: removed %d exact duplicate row(s) from fig.list before plotting (%d distinct row(s) remain).\n",
                 n.fig.list.duplicates.removed, nrow(jobs)))
  }

  plots <- list()

  for (j in seq_len(nrow(jobs))) {
    job <- jobs[j, ]
    job.label <- if (nzchar(trimws(job$plot.name))) job$plot.name else sprintf("row %d", j)
    job.key <- as.character(j)

    if (!identical(tolower(trimws(job$plot.type)), tolower(PLOT.TYPE))) {
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped (this function only handles plot.type = '%s').\n",
                   job.label, job$plot.type, PLOT.TYPE))
      next
    }

    facet.kind <- tolower(trimws(job$facet))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: fig.list row for '%s' has facet = '%s' - skipped ($facet = \"sppid\" is the only value implemented so far).\n",
                   job.label, job$facet))
      next
    }

    facet.set.val <- tolower(trimws(job$facet.set))
    if (facet.set.val %in% NE.ALIASES) {
      facpan <- SPECIAL.FACPAN
    } else {
      facpan <- strsplit(get.setting(job, "facpan"), ";", fixed = TRUE)[[1]]
    }

    spp.plot <- facpan
    myso.flag <- isTRUE(as.logical(job$MYSO))
    if (myso.flag) spp.plot <- c(spp.plot, "Indiana Bat")
    alldect.flag <- isTRUE(as.logical(job$Alldect))
    if (alldect.flag) {
      spp.plot <- c(spp.plot, "All detections")
      facpan <- c(facpan, "All detections")
    }
    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) {
      spp.plot <- c(spp.plot, "40khzmyo")
      if (!alldect.flag) facpan <- c(facpan, "40khzmyo")
    }
    spp.plot <- unique(trimws(spp.plot))
    facpan <- unique(trimws(facpan))

    spp.plot <- batz.batusa_recode.names(spp.plot, output.format = "common")
    facpan <- batz.batusa_recode.names(facpan, output.format = "common")

    pd <- data
    is.khz.raw <- norm.simple(pd$spp.id) %in% KHZ.ALIASES
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, output.format = "common")
    pd$spp.common[is.khz.raw] <- "40khzmyo"

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$aru.groupby)) == tolower(plot.set.val), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated. Check that $aru.groupby/$date in data actually overlap this row's $plot.set/$date.start/$date.end.\n",
                   job.label, plot.set.val, date.start, date.end))
      next
    }

    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel, "All detections", pd$spp.common)
    pd$bar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")

    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (is.na(facet.label.fmt) || !nzchar(facet.label.fmt)) facet.label.fmt <- "common"
    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, output.format = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw
    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels, labels = panel.labels[ordered.levels])

    # --- Y-axis resolution: see @details above for the full explanation ---
    yaxe.trans <- tolower(trimws(get.setting(job, "Yaxe.trans")))
    if (!yaxe.trans %in% c("none", "log", "log10")) {
      cat(sprintf("NOTE: '%s' - $Yaxe.trans = '%s' is not one of none/log/log10 - defaulting to 'none'.\n",
                   job.label, get.setting(job, "Yaxe.trans")))
      yaxe.trans <- "none"
    }
    loglabels <- isTRUE(as.logical(get.setting(job, "loglabels")))

    y.scale.mode <- tolower(trimws(get.setting(job, "y.scale")))
    if (!y.scale.mode %in% c("regular", "rounded", "custom")) {
      cat(sprintf("NOTE: '%s' - $y.scale = '%s' is not one of regular/rounded/custom - defaulting to 'regular'.\n",
                   job.label, get.setting(job, "y.scale")))
      y.scale.mode <- "regular"
    }

    ymax.raw <- suppressWarnings(as.numeric(get.setting(job, "ymax")))
    if (is.na(ymax.raw) || ymax.raw <= 0) {
      ymax.raw <- max(pd$obs, na.rm = TRUE)
      cat(sprintf("NOTE: '%s' - $ymax not given (or not a usable positive number) - defaulting to the max observed count in this plot's data (%s).\n",
                   job.label, ymax.raw))
    }

    trans.fn <- switch(yaxe.trans,
      none  = function(x) x,
      log   = function(x) log1p(x),
      log10 = function(x) log10(x + 1)
    )

    if (y.scale.mode == "custom") {
      y.custom.raw <- suppressWarnings(as.numeric(strsplit(get.setting(job, "y.custom"), ";", fixed = TRUE)[[1]]))
      y.custom.raw <- sort(unique(y.custom.raw[!is.na(y.custom.raw)]))
      if (length(y.custom.raw) == 0) {
        cat(sprintf("NOTE: '%s' - $y.scale = 'custom' but $y.custom has no usable numbers - falling back to 'regular'.\n", job.label))
        y.scale.mode <- "regular"
      }
    }
    if (y.scale.mode != "custom") {
      frac <- c(0, 0.25, 0.5, 0.75, 1)
      raw.breaks <- frac * ymax.raw
      if (y.scale.mode == "rounded") raw.breaks <- round(raw.breaks)
    } else {
      raw.breaks <- y.custom.raw
    }
    raw.breaks <- sort(unique(raw.breaks))
    # never let $ymax/$y.custom clip a real bar or a user-declared break
    y.upper <- max(c(ymax.raw, raw.breaks, pd$obs), na.rm = TRUE)

    pd$obs.plot <- trans.fn(pd$obs)
    break.pos <- trans.fn(raw.breaks)
    break.labels <- if (loglabels) {
      format(round(break.pos, 2))
    } else {
      format(raw.breaks, big.mark = ",", trim = TRUE, scientific = FALSE)
    }

    plots[[job.key]] <- list(
      job.label = job.label, job = job, pd = pd, panel.labels = panel.labels,
      facpan = facpan, spp.plot = spp.plot, date.start = date.start, date.end = date.end,
      khz.flag = khz.flag, break.pos = break.pos, break.labels = break.labels,
      y.upper.plot = trans.fn(y.upper)
    )
    cat(sprintf("Prepared plot data for '%s': %d observation row(s) across %d panel(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw)))
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

      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        cat(sprintf("NOTE: '%s' - $xaxe.interval = '%s' is not a usable number of x-axis labels - defaulting to 2 (just date.start/date.end).\n",
                     job.label, get.setting(p$job, "xaxe.interval")))
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))
      xaxe.buffer <- suppressWarnings(as.numeric(get.default("xaxe.date.buffer.days")))
      if (is.na(xaxe.buffer)) xaxe.buffer <- 0.5

      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      # Drawn as TWO separate geom_col() layers (all-detections first/bottom,
      # 40kHzMyo second/on top), rather than one geom_col() call with
      # aes(fill = bar.type) - a real rendering bug found and fixed while
      # visually comparing a render to Josh's target image: a single
      # geom_col() call groups by the fill aesthetic's OWN factor-level
      # order (alphabetical, since no explicit levels were set), which put
      # "40kHzMyo" (before "All detections" alphabetically) UNDERNEATH the
      # gray all-detections bar for the same night - same width, same x
      # position, same day - completely hiding the black overlay every
      # time, even though both bars' data rows were present and correct.
      # Two explicit layers, added to the plot in bottom-to-top order,
      # guarantee 40kHzMyo always draws on top regardless of factor order.
      bar.width.val <- as.numeric(get.default("bar.width"))
      g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed)) +
        ggplot2::geom_col(data = p$pd[p$pd$bar.type == "All detections", , drop = FALSE],
                           ggplot2::aes(y = obs.plot, fill = bar.type),
                           width = bar.width.val, position = "identity") +
        ggplot2::geom_col(data = p$pd[p$pd$bar.type == "40kHzMyo", , drop = FALSE],
                           ggplot2::aes(y = obs.plot, fill = bar.type),
                           width = bar.width.val, position = "identity") +
        ggplot2::scale_fill_manual(name = get.default("bar.fill.legend.title"),
          breaks = fill.legend.breaks, limits = fill.legend.limits,
          values = c(`All detections` = get.default("bar.alldetections.fill"),
                     `40kHzMyo` = get.default("bar.40khzmyo.fill"))) +
        ggplot2::scale_y_continuous(limits = c(0, p$y.upper.plot), breaks = p$break.pos, labels = p$break.labels,
          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        ggplot2::scale_x_date(limits = c(p$date.start - xaxe.buffer, p$date.end + xaxe.buffer),
          breaks = xaxe.breaks, date_labels = xaxe.date.labels.fmt,
          name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        ggplot2::facet_wrap(~facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(linewidth = as.numeric(get.default("panel.border.linewidth")), colour = "grey20", fill = NA),
          legend.position = get.default("legend.position"),
          plot.title = ggplot2::element_text(hjust = as.numeric(get.default("plot.title.hjust")), size = as.numeric(get.default("plot.title.size"))),
          axis.title = ggplot2::element_text(size = as.numeric(get.default("axis.title.size"))),
          axis.text = ggplot2::element_text(size = as.numeric(get.default("axis.text.size"))),
          legend.text = ggplot2::element_text(size = as.numeric(get.default("legend.text.size"))),
          legend.title = ggplot2::element_text(size = as.numeric(get.default("legend.title.size"))),
          panel.spacing.x = grid::unit(as.numeric(get.default("panel.spacing.x")), "pt")
        )

      ggplots[[job.key]] <- g

      pattern <- get.default("output.filename.pattern")
      fname <- pattern
      fname <- gsub("<ARU>", trimws(p$job$plot.set), fname, fixed = TRUE)
      fname <- gsub("<date.start>", as.character(min(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<date.end>", as.character(max(p$pd$date.parsed)), fname, fixed = TRUE)
      fname <- gsub("<timestamp>", format(Sys.time(), "%Y%m%d%H%M%S"), fname, fixed = TRUE)
      fname <- file.path(dir.save, fname)

      ggplot2::ggsave(fname, plot = g,
        width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
        height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
        units = get.default("ggsave.units"), dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

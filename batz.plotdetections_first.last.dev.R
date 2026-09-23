# =============================================================================
# batz.plotdetections_first.last.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.plotdetections_first.last().
#
# ROUND NINETEEN, per Josh's 2026-09-16 follow-up ("reorder the headings in
# all plotopts files to be $category $parameter $default.value $overide.value
# $notes ... Update all plot functions in Batz ... project.name = "new.project"
# ... this will now be used to create the first part of any file saved ...
# add arguments dir.save = getwd() ... aes.style = "overide.value" ...
# Function logic will first look in the column with the header = aes.style
# ... then if that element is blank use $default.value ... Print the save
# names for all"):
#
#   - project.name no longer selects an aes.default override column - it
#     ONLY builds the saved file name now:
#     "<project.name>_<ARU>_<timestamp>.png". Default changed from ""
#     to "new.project".
#   - New aes.style argument (default "overide.value") names a FIXED column
#     aes.default is checked against first, falling back to $default.value
#     when that column is absent or blank for a row. This replaces the old
#     project.name-matches-a-column-name mechanism ("gome", etc.) entirely.
#   - New dir.save argument - unchanged from the round it was added in
#     (2026-08-27) - still controls where every PNG lands.
#   - $output.filename.pattern is DEPRECATED and no longer read at all -
#     every saved file name is fixed: "<project.name>_<plot.set>_<timestamp>.png".
#
# **JUDGMENT CALL / LIMITATION, flagged to Josh:** this dev script previously
# read four of Josh's own real device test-data files off disk
# (vetted.processed.csv -> data, fig.list.csv -> fig.list, suntimes.csv ->
# suntimes, plotopts_first.last.csv -> aes.default) via the device bridge.
# That bridge is not available in this session/environment, so those real
# files could not be re-fetched. This round's dev script instead builds its
# own SYNTHETIC stand-ins for all four inputs directly in R code below,
# using the exact same column layouts documented for the real files (see
# DATA.REQUIRED/SUNTIMES.REQUIRED/FIG.LIST.REQUIRED/AES.DEFAULT.REQUIRED
# below). This fully exercises the settings-resolution/file-naming logic
# that actually changed this round; it does NOT re-validate against Josh's
# own real data quirks documented in this function's dated history further
# down (those entries are left as-is, historical). Please re-run this
# function against your own real files once you have a chance, and let me
# know if anything looks different.
# =============================================================================

source("batz.batusa_recode.names.R")

# -----------------------------------------------------------------------------
# SYNTHETIC test data - built directly in R (see limitation note above),
# using the exact column layouts documented for the real device files.
# -----------------------------------------------------------------------------

make.default.plotaesthetics <- function(overide.col = "overide.value") {
  rows <- list(
    c("Layout",         "facpan.numcol",                "3",        ""),
    c("Text",           "plot.title.size",               "12",       ""),
    c("Text",           "plot.title.hjust",               "0.5",      ""),
    c("Text",           "axis.title.size",                "10",       ""),
    c("Text",           "axis.text.size",                  "6",        ""),
    c("Text",           "legend.text.size",                "8",        ""),
    c("Text",           "legend.title.size",               "9",        ""),
    c("Layout",         "panel.spacing.x",                 "5.5",      ""),
    c("Theme",          "panel.border.linewidth",          "0.5",      ""),
    c("Theme",          "legend.position",                 "bottom",   ""),
    c("Axes",           "xaxe.interval",                   "4",        ""),
    c("Axes",           "yaxe.break.interval",             "4 hours",  ""),
    c("Axes",           "yaxe.labelformat",                "%H:%M",    ""),
    c("Axes",           "yaxe.break.labels",               "Noon;4pm;8pm;Midnight;4am;8am;Noon", ""),
    c("Reference lines","midnight.linetype",               "solid",    ""),
    c("Reference lines","midnight.color",                  "black",    ""),
    c("Reference lines","midnight.dots.color",             "grey50",   ""),
    c("Reference lines","midnight.dots.size",              "1.5",      ""),
    c("Reference lines","dawn.linetype",                   "dashed",   ""),
    c("Reference lines","dawn.color",                      "blue",     ""),
    c("Reference lines","dusk.linetype",                   "dashed",   ""),
    c("Reference lines","dusk.color",                      "red",      ""),
    c("Reference lines","reference.line.legend.title",     "Reference lines", ""),
    c("Crossbar",       "crossbar.alldetections.fill",     "grey70",   ""),
    c("Crossbar",       "crossbar.40khzmyo.fill",          "black",    ""),
    c("Crossbar",       "crossbar.linewidth",              "0.3",      ""),
    c("Crossbar",       "crossbar.fill.legend.title",      "Detections", ""),
    c("Save",           "ggsave.dpi",                      "150",      ""),
    c("Save",           "ggsave.units",                    "in",       ""),
    c("Save",           "ggsave.width.pad",                "0",        ""),
    c("Save",           "ggsave.height.pad",               "0",        ""),
    c("Save",           "plot.width",                      "6",        ""),
    c("Save",           "plot.height",                     "4",        ""),
    # extra params this function reads via get.default()/get.setting() that
    # are NOT in AES.DEFAULT.REQUIRED.PARAMETERS (optional/job-overridable):
    c("Misc",           "time.zone",                       "UTC",      ""),
    c("Axes",           "yaxe.limit.min",                  "12:00",    ""),
    c("Axes",           "yaxe.limit.max",                  "12:00",    ""),
    c("Reference lines","midnight",                        "short",    ""),
    c("Layout",         "facpan",                          "",         ""),
    c("Layout",         "plot.order",                      "",         ""),
    c("Axes",           "yaxe.title",                      "Hour of Monitoring", ""),
    # DEPRECATED (round nineteen) - left in place, harmless, never read.
    c("Save",           "output.filename.pattern",         "<ARU>_<date.start>_<timestamp>_Earliest and latest bat.png", "")
  )
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("category", "parameter", "default.value", "notes")
  df[[overide.col]] <- ""
  df <- df[, c("category", "parameter", "default.value", overide.col, "notes")]
  # mark the deprecated row's notes
  df$notes[df$parameter == "output.filename.pattern"] <-
    "DEPRECATED as of Josh's nineteenth follow-up (2026-09-16) - no longer read; the saved file name is now always \"<project.name>_<ARU>_<timestamp>.png\"."
  df
}

default.plotaesthetics.synth <- make.default.plotaesthetics()

date.start.synth <- as.Date("2026-05-01")
date.end.synth   <- as.Date("2026-05-05")
test.dates       <- seq(date.start.synth, date.end.synth, by = "day")

plot.data.synth <- do.call(rbind, lapply(test.dates, function(d) {
  data.frame(
    spp.id          = c("Hoary bat", "Big brown bat"),
    date            = format(d, "%m/%d/%Y"),
    aru.groupby     = "WTG-GOM102",
    obs             = 1,
    mins2.noon.min  = c(540, 560),
    mins2.noon.max  = c(545, 565),
    vetting.type    = "manid.sb",
    stringsAsFactors = FALSE
  )
}))

suntimes.synth <- do.call(rbind, lapply(seq_along(test.dates), function(i) {
  d <- test.dates[i]
  d.next <- test.dates[i] + 1
  data.frame(
    aru.name        = "WTG-GOM102",
    date            = format(d, "%m/%d/%Y"),
    date.mon        = format(d.next, "%m/%d/%Y"),
    sunregion       = "WTG",
    time.zone       = "UTC",
    sunregion.type  = "coordinates",
    schedual1       = "civil",
    schedual2       = "civil",
    suns            = format(as.POSIXct(paste(d, "20:00:00"), tz = "UTC"), "%m/%d/%Y %H:%M"),
    suns.unix       = 0,
    sunr            = format(as.POSIXct(paste(d, "06:00:00"), tz = "UTC"), "%m/%d/%Y %H:%M"),
    sunr.unix       = 0,
    sunr.mon        = format(as.POSIXct(paste(d.next, "06:00:00"), tz = "UTC"), "%m/%d/%Y %H:%M"),
    sunr.mon.unix   = 0,
    stringsAsFactors = FALSE
  )
}))

aru.metadata.db.synth <- data.frame(
  plot.type      = "bat.detection",
  plot.name      = "Test Site",
  facet          = "sppid",
  facet.set      = "NE",
  MYSO           = FALSE,
  Alldect        = TRUE,
  facet.panel    = "",
  "40khzmyo"     = TRUE,
  facet.label    = "common",
  plot.set       = "WTG-GOM102",
  date.format    = "%b-%d/n%Y",
  date.start     = format(date.start.synth, "%m/%d/%Y"),
  date.end       = format(date.end.synth, "%m/%d/%Y"),
  xaxe.interval  = 4,
  xaxe.title     = "Date",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cat("=== synthetic data ===\n"); str(plot.data.synth)
cat("\n=== synthetic fig.list ===\n"); str(aru.metadata.db.synth)
cat("\n=== synthetic suntimes ===\n"); str(suntimes.synth)
cat("\n=== synthetic aes.default (round nineteen: category/parameter/default.value/overide.value/notes) ===\n")
print(head(default.plotaesthetics.synth))

# -----------------------------------------------------------------------------
# header + duplicate-name + missing-parameter-row checks
# -----------------------------------------------------------------------------
check.headers <- function(df, required, label) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
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

DATA.REQUIRED <- c("spp.id", "date", "aru.groupby", "obs",
                         "mins2.noon.min", "mins2.noon.max", "vetting.type")
# Renamed 2026-09-22 (per Josh's request, "change all functions that have
# aru as an header to \"aru.name\"", found via the project's own reference
# workbook and cross-checked against this script's own R/ counterpart):
# matches batz.generate_suntimes.arulist()'s own renamed output column.
SUNTIMES.REQUIRED <- c("aru.name", "date", "date.mon", "sunregion", "time.zone",
                           "sunregion.type", "schedual1", "schedual2", "suns",
                           "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                               "Alldect", "facet.panel", "40khzmyo", "facet.label",
                               "plot.set", "date.format", "date.start", "date.end",
                               "xaxe.interval", "xaxe.title")
AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

## "output.filename.pattern" deliberately removed from this required list per
## Josh's nineteenth follow-up (2026-09-16) - the saved file name is now
## always "<project.name>_<ARU>_<timestamp>.png"; no longer read at all.
AES.DEFAULT.REQUIRED.PARAMETERS <- c(
  "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
  "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
  "panel.border.linewidth", "legend.position", "xaxe.interval",
  "yaxe.break.interval", "yaxe.labelformat", "yaxe.break.labels",
  "midnight.linetype", "midnight.color", "midnight.dots.color",
  "midnight.dots.size", "dawn.linetype", "dawn.color", "dusk.linetype",
  "dusk.color", "reference.line.legend.title", "crossbar.alldetections.fill",
  "crossbar.40khzmyo.fill", "crossbar.linewidth", "crossbar.fill.legend.title",
  "ggsave.dpi", "ggsave.units", "ggsave.width.pad", "ggsave.height.pad",
  "plot.width", "plot.height"
)

check.parameters <- function(df, required, label) {
  if (!("parameter" %in% names(df))) return(NULL)  # already reported by check.headers above
  missing <- setdiff(required, df$parameter)
  if (length(missing) > 0) {
    return(sprintf("%s is missing these required $parameter rows: %s - it may be an older copy missing settings added since it was last saved",
                    label, paste(missing, collapse = ", ")))
  }
  NULL
}

# -----------------------------------------------------------------------------
# batz.plotdetections_first.last() - dev copy, mirrors the package .R file
# -----------------------------------------------------------------------------
batz.plotdetections_first.last <- function(data, fig.list, suntimes,
                                            aes.default, project.name = "new.project",
                                            aes.style = "overide.value",
                                            dir.save = getwd()) {

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

  parse.flex.datetime <- function(x, tz) {
    out <- as.POSIXct(rep(NA_character_, length(x)), tz = tz)
    for (fmt in c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M")) {
      still.na <- is.na(out) & nzchar(trimws(as.character(x)))
      if (!any(still.na)) break
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      out[still.na] <- parsed[still.na]
    }
    out
  }

  ## Settings resolution (round nineteen, per Josh's 2026-09-16 follow-up):
  ## $aes.style names a FIXED column to check first (default "overide.value"
  ## - a blank column the user fills in directly on their own copy of the
  ## CSV), falling back to $default.value when that column doesn't exist or
  ## is blank for this row. Replaces the old project.name-matches-a-
  ## column-name mechanism entirely - project.name no longer participates in
  ## settings resolution, only in the saved file name (see the main loop
  ## below).
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

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) {
    stop("fig.list has no plot rows (every row's $plot.type is blank) - nothing to plot.")
  }

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

    if (!identical(tolower(trimws(job$plot.type)), "bat.detection")) {
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped (only 'bat.detection' is implemented so far).\n",
                   job.label, job$plot.type))
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
      facpan   <- c(facpan, "All detections")
    }

    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) {
      spp.plot <- c(spp.plot, "40khzmyo")
      if (!alldect.flag) facpan <- c(facpan, "40khzmyo")
    }

    spp.plot <- unique(trimws(spp.plot))
    facpan   <- unique(trimws(facpan))

    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common")
    facpan   <- batz.batusa_recode.names(facpan, batname.format.out = "common")

    pd <- data
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common")

    plot.set.val <- trimws(job$plot.set)
    if (nzchar(plot.set.val)) {
      pd <- pd[tolower(trimws(pd$aru.groupby)) == tolower(plot.set.val), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end   <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    tz <- get.setting(job, "time.zone")

    if (nrow(pd) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' (plot.set = '%s', %s to %s) matched 0 rows of data - no plot generated.\n",
                   job.label, plot.set.val, date.start, date.end))
      next
    }

    y.ref.date <- as.Date("1970-01-02")
    y.ref.noon <- as.POSIXct(paste(y.ref.date, "12:00:00"), tz = tz)
    pd$time.min <- y.ref.noon + pd$mins2.noon.min * 60
    pd$time.max <- y.ref.noon + pd$mins2.noon.max * 60

    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel,
                                    "All detections", pd$spp.common)
    pd$crossbar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")

    sdb <- suntimes
    sdb$date.parsed <- parse.flex.date(sdb$date)
    if (nzchar(plot.set.val)) {
      sdb <- sdb[tolower(trimws(sdb$aru.name)) == tolower(plot.set.val), , drop = FALSE]
    }
    sdb <- sdb[!is.na(sdb$date.parsed) & sdb$date.parsed >= date.start & sdb$date.parsed <= date.end, , drop = FALSE]

    if (nrow(sdb) == 0) {
      cat(sprintf("NOTE: fig.list row for '%s' matched 0 rows of suntimes for plot.set = '%s' between %s and %s.\n",
                   job.label, plot.set.val, date.start, date.end))
    }

    dusk.real <- parse.flex.datetime(sdb$suns, tz)
    dawn.real <- parse.flex.datetime(sdb$sunr.mon, tz)
    local.noon <- as.POSIXct(paste(sdb$date.parsed, "12:00:00"), tz = tz)
    sdb$dusk.time     <- y.ref.noon + as.numeric(difftime(dusk.real, local.noon, units = "secs"))
    sdb$dawn.time     <- y.ref.noon + as.numeric(difftime(dawn.real, local.noon, units = "secs"))
    sdb$midnight.time <- y.ref.noon + 12 * 3600

    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (!nzchar(facet.label.fmt)) facet.label.fmt <- "common"

    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, batname.format.out = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw

    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels,
                                    labels = panel.labels[ordered.levels])

    yaxe.limit.min <- get.setting(job, "yaxe.limit.min")
    yaxe.limit.max <- get.setting(job, "yaxe.limit.max")
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.min) || !grepl("^[0-9]{1,2}:[0-9]{2}$", yaxe.limit.max)) {
      stop(sprintf(paste("$yaxe.limit.min/$yaxe.limit.max ('%s'/'%s') don't look like HH:MM time-of-day",
                          "values - aes.default may be an old, numeric-minutes-based copy of",
                          "plotopts_first.last.csv."),
                    yaxe.limit.min, yaxe.limit.max))
    }
    y.start <- as.POSIXct(paste(y.ref.date, yaxe.limit.min), tz = tz)
    y.end   <- y.start + 24 * 3600

    plots[[job.key]] <- list(
      job.label = job.label,
      job = job,
      pd = pd,
      sdb = sdb,
      panel.labels = panel.labels,
      facpan = facpan,
      spp.plot = spp.plot,
      y.start = y.start,
      y.end = y.end,
      tz = tz,
      date.start = date.start,
      date.end = date.end,
      khz.flag = khz.flag,
      resolved.legend.position = get.default("legend.position"),  # exposed for testing the aes.style resolver
      resolved.dawn.color = get.default("dawn.color")              # exposed for testing the fall-through-to-default case
    )

    cat(sprintf("Prepared plot data for '%s': %d detection rows across %d panel(s), %d suntimes row(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw), nrow(sdb)))
  }

  if (length(plots) == 0) {
    cat("No plots were generated - see NOTE messages above.\n")
    return(invisible(list()))
  }

  ggplots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    for (job.key in names(plots)) {
      p <- plots[[job.key]]
      job.label <- p$job.label

      y.breaks <- seq(p$y.start, p$y.end, by = get.default("yaxe.break.interval"))
      y.break.labels <- strsplit(get.default("yaxe.break.labels"), ";", fixed = TRUE)[[1]]
      if (length(y.break.labels) != length(y.breaks)) {
        y.break.labels <- format(y.breaks, get.default("yaxe.labelformat"))
      }

      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)

      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) {
        xaxe.n.labels <- 2
      }
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))

      panel.border.lw <- as.numeric(get.default("panel.border.linewidth"))
      midnight.render.lw <- panel.border.lw / 2

      midnight.mode <- tolower(trimws(get.setting(p$job, "midnight")))
      if (!midnight.mode %in% c("none", "long", "short", "dots")) {
        midnight.mode <- "short"
      }
      midnight.const <- p$y.start + 12 * 3600
      midnight.legend.color <- if (midnight.mode == "dots") get.default("midnight.dots.color") else get.default("midnight.color")
      midnight.layer <- NULL
      if (midnight.mode == "short") {
        midnight.layer <- geom_line(data = p$sdb, aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                      linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw,
                                      inherit.aes = FALSE)
      } else if (midnight.mode == "long") {
        midnight.layer <- geom_hline(data = data.frame(midnight.time = midnight.const),
                                       aes(yintercept = midnight.time, color = "Midnight"),
                                       linetype = get.default("midnight.linetype"), linewidth = midnight.render.lw)
      } else if (midnight.mode == "dots") {
        midnight.layer <- geom_point(data = p$sdb, aes(x = date.parsed, y = midnight.time, color = "Midnight"),
                                       shape = 45, size = as.numeric(get.default("midnight.dots.size")),
                                       inherit.aes = FALSE)
      }

      midnight.legend.shape <- if (midnight.mode == "dots") 45 else NA
      reference.line.override.shape <- if (midnight.mode == "none") c(NA, NA) else c(NA, NA, midnight.legend.shape)

      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      g <- ggplot(p$pd, aes(x = date.parsed)) +
        geom_line(data = p$sdb, aes(x = date.parsed, y = dusk.time, color = "Dusk"),
                   linetype = get.default("dusk.linetype"), inherit.aes = FALSE) +
        midnight.layer +
        geom_line(data = p$sdb, aes(x = date.parsed, y = dawn.time, color = "Dawn"),
                   linetype = get.default("dawn.linetype"), inherit.aes = FALSE) +
        geom_crossbar(aes(ymin = time.min, ymax = time.max, y = time.min, fill = crossbar.type),
                       linewidth = as.numeric(get.default("crossbar.linewidth")),
                       width = 0.9) +
        scale_color_manual(name = get.default("reference.line.legend.title"),
                            values = c("Dawn" = get.default("dawn.color"),
                                       "Midnight" = midnight.legend.color,
                                       "Dusk" = get.default("dusk.color"))) +
        guides(colour = guide_legend(override.aes = list(shape = reference.line.override.shape))) +
        scale_fill_manual(name = get.default("crossbar.fill.legend.title"),
                           breaks = fill.legend.breaks,
                           limits = fill.legend.limits,
                           values = c("All detections" = get.default("crossbar.alldetections.fill"),
                                      "40kHzMyo" = get.default("crossbar.40khzmyo.fill"))) +
        scale_y_datetime(limits = c(p$y.start, p$y.end),
                          breaks = y.breaks,
                          labels = y.break.labels,
                          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        scale_x_date(limits = c(p$date.start - 0.5, p$date.end + 0.5),
                      breaks = xaxe.breaks,
                      date_labels = xaxe.date.labels.fmt,
                      name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        facet_wrap(~ facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        labs(title = job.label) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(linewidth = panel.border.lw, colour = "grey20", fill = NA),
              legend.position = get.default("legend.position"),
              plot.title = element_text(hjust = as.numeric(get.default("plot.title.hjust")),
                                          size = as.numeric(get.default("plot.title.size"))),
              axis.title = element_text(size = as.numeric(get.default("axis.title.size"))),
              axis.text = element_text(size = as.numeric(get.default("axis.text.size"))),
              legend.text = element_text(size = as.numeric(get.default("legend.text.size"))),
              legend.title = element_text(size = as.numeric(get.default("legend.title.size"))),
              panel.spacing.x = unit(as.numeric(get.default("panel.spacing.x")), "pt"))

      ggplots[[job.key]] <- g

      ## Round nineteen, per Josh (2026-09-16): every saved file name is now
      ## always "<project.name>_<ARU>_<timestamp>.png" -
      ## $output.filename.pattern is DEPRECATED and no longer read.
      fname <- sprintf("%s_%s_%s.png", project.name, trimws(p$job$plot.set), format(Sys.time(), "%Y%m%d_%H%M%S"))
      fname <- file.path(dir.save, fname)

      ggsave(fname, plot = g,
             width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
             height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
             units = get.default("ggsave.units"),
             dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 is not installed in this environment - returning prepared data only, no plot object/PNG produced.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n\n########## TEST 1: header checks catch real problems ##########\n")
tryCatch(
  batz.plotdetections_first.last(
    data = plot.data.synth[, setdiff(names(plot.data.synth), "obs")],
    fig.list = aru.metadata.db.synth,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  ),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 2: duplicate column name in fig.list is caught ##########\n")
aru.metadata.db.dup <- aru.metadata.db.synth
names(aru.metadata.db.dup)[names(aru.metadata.db.dup) == "plot.name"] <- "plot.type"
tryCatch(
  batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.dup,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  ),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 3: full pipeline, default (NE) facet list, default project.name/aes.style ##########\n")
result3 <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.synth,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.synth
)
cat("Number of plots produced:", length(result3$plots), "(expected 1)\n")
if (length(result3$plots) > 0) {
  p <- result3$plots[[1]]
  cat("Panels:", paste(levels(p$pd$facet.panel.value), collapse = " | "), "\n")
}

cat("\n\n########## TEST 4: MYSO/40khzmyo-without-Alldect flag handling ##########\n")
aru.metadata.db.test4 <- aru.metadata.db.synth
aru.metadata.db.test4$MYSO <- TRUE
aru.metadata.db.test4$Alldect <- FALSE
result4 <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.test4,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.synth
)
if (length(result4$plots) > 0) {
  cat("spp.plot included Indiana bat (canonicalized):",
      "indiana bat" %in% tolower(result4$plots[[1]]$spp.plot), "\n")
  cat("facpan included 40kHzMyo as its own panel (canonicalized):",
      "40khzmyo" %in% tolower(result4$plots[[1]]$facpan), "\n")
}

cat("\n\n########## TEST 5: $midnight modes (none/long/short/dots/invalid) ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  for (mode in c("none", "long", "short", "dots", "bogus")) {
    aru.metadata.db.mid <- aru.metadata.db.synth
    aru.metadata.db.mid$midnight <- mode
    result.mid <- batz.plotdetections_first.last(
      data = plot.data.synth,
      fig.list = aru.metadata.db.mid,
      suntimes = suntimes.synth,
      aes.default = default.plotaesthetics.synth
    )
    if (length(result.mid$ggplots) > 0) {
      g <- result.mid$ggplots[[1]]
      geom.classes <- sapply(g$layers, function(l) class(l$geom)[1])
      cat(sprintf("  %s: geoms present = %s\n", mode, paste(geom.classes, collapse = ", ")))
    }
  }
} else {
  cat("ggplot2 not available - skipping TEST 5\n")
}

cat("\n\n########## TEST 6: aes.style override (round nineteen - replaces the old project.name-column mechanism) ##########\n")
# 2026-09-16, per Josh's nineteenth follow-up: aes.default now has a fixed
# $overide.value column (default aes.style target) instead of a
# project.name-matched column. Filling in $overide.value for a parameter
# should override $default.value for every caller, regardless of
# project.name.
default.plotaesthetics.override <- default.plotaesthetics.synth
default.plotaesthetics.override$overide.value[default.plotaesthetics.override$parameter == "legend.position"] <- "top"
result6 <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.synth,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.override
)
if (length(result6$plots) > 0) {
  cat("legend.position resolved with $overide.value filled in:",
      result6$plots[[1]]$resolved.legend.position, "(expected 'top')\n")
}
# project.name itself should now have ZERO effect on settings resolution -
# confirm passing an arbitrary project.name doesn't change the resolved
# legend.position at all (it only affects the saved file name).
result6b <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.synth,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.override,
  project.name = "some.other.project"
)
if (length(result6b$plots) > 0) {
  cat("legend.position with a DIFFERENT project.name, same $overide.value:",
      result6b$plots[[1]]$resolved.legend.position, "(expected 'top' - project.name no longer matters for settings)\n")
}

cat("\n\n########## TEST 7: settings-resolution precedence - fig.list row > aes.style column > $default.value ##########\n")
default.plotaesthetics.prec <- default.plotaesthetics.synth
default.plotaesthetics.prec$overide.value[default.plotaesthetics.prec$parameter == "yaxe.title"] <- "Hour of Monitoring (overide)"
default.plotaesthetics.prec$overide.value[default.plotaesthetics.prec$parameter == "legend.position"] <- "top"
# dawn.color is left blank in $overide.value, so it should fall through to
# $default.value ("blue").

# Case (a): fig.list's OWN $yaxe.title is non-blank -> wins over both the
# overide.value column and the default.
aru.metadata.db.yt <- aru.metadata.db.synth
aru.metadata.db.yt$yaxe.title <- "Hour of mointoring (job's own value)"
result7a <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.yt,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.prec
)
cat("Case (a) fig.list's own value present -> y-axis label:",
    if (length(result7a$ggplots) > 0) result7a$ggplots[[1]]$scales$get_scales("y")$name else "(no plot)",
    "(expected: '\\nHour of mointoring (job's own value)' - job's own value wins)\n")

# Case (b): fig.list has no $yaxe.title value for this row (blank) -
# overide.value's value should now win over the default.
aru.metadata.db.noyt <- aru.metadata.db.synth
aru.metadata.db.noyt$yaxe.title <- ""
result7b <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.noyt,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.prec
)
cat("Case (b) fig.list's own value BLANK -> y-axis label:",
    if (length(result7b$ggplots) > 0) result7b$ggplots[[1]]$scales$get_scales("y")$name else "(no plot)",
    "(expected: '\\nHour of Monitoring (overide)' - overide.value column wins over default)\n")
cat("Case (b) legend.position (no fig.list column at all):",
    if (length(result7b$plots) > 0) result7b$plots[[1]]$resolved.legend.position else "(no plot)",
    "(expected: 'top' - overide.value column wins over default 'bottom')\n")

# Case (c): neither fig.list nor overide.value has a value for dawn.color -
# should fall all the way through to default.value.
cat("Case (c) dawn.color (blank in both fig.list and $overide.value):",
    if (length(result7b$plots) > 0) result7b$plots[[1]]$resolved.dawn.color else "(no plot)",
    "(expected: 'blue' - default.value, since neither fig.list nor overide.value define it)\n")

cat("\n\n########## TEST 8: backward compatibility - an aes.default sheet with NO $overide.value column at all still works ##########\n")
default.plotaesthetics.nocol <- default.plotaesthetics.synth
default.plotaesthetics.nocol$overide.value <- NULL
result8 <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.synth,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.nocol
)
cat("legend.position with no $overide.value column present at all:",
    if (length(result8$plots) > 0) result8$plots[[1]]$resolved.legend.position else "(no plot)",
    "(expected: 'bottom' - falls straight through to $default.value)\n")

cat("\n\n########## TEST 9: custom aes.style column name ##########\n")
default.plotaesthetics.custom <- default.plotaesthetics.synth
default.plotaesthetics.custom$overide.value <- NULL
default.plotaesthetics.custom$my.custom.col <- ""
default.plotaesthetics.custom$my.custom.col[default.plotaesthetics.custom$parameter == "legend.position"] <- "top"
result9 <- batz.plotdetections_first.last(
  data = plot.data.synth,
  fig.list = aru.metadata.db.synth,
  suntimes = suntimes.synth,
  aes.default = default.plotaesthetics.custom,
  aes.style = "my.custom.col"
)
cat("legend.position resolved with aes.style = 'my.custom.col':",
    if (length(result9$plots) > 0) result9$plots[[1]]$resolved.legend.position else "(no plot)",
    "(expected: 'top')\n")

cat("\n\n########## TEST 10: project.name drives the saved file name; $output.filename.pattern is ignored entirely ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  dir.save.test <- file.path(tempdir(), paste0("plotdetections_dirsave_test_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
  dir.create(dir.save.test)

  # default project.name = "new.project"
  result10a <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.synth,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth,
    dir.save = dir.save.test
  )
  pngs.a <- list.files(dir.save.test, pattern = "\\.png$")
  cat("default project.name -> saved file name(s):", paste(pngs.a, collapse = ", "), "\n")
  cat("  starts with 'new.project_WTG-GOM102_' (expected TRUE):",
      any(grepl("^new\\.project_WTG-GOM102_", pngs.a)), "\n")

  # explicit project.name = "acme"
  result10b <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.synth,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth,
    project.name = "acme",
    dir.save = dir.save.test
  )
  pngs.b <- setdiff(list.files(dir.save.test, pattern = "\\.png$"), pngs.a)
  cat("project.name = 'acme' -> saved file name(s):", paste(pngs.b, collapse = ", "), "\n")
  cat("  starts with 'acme_WTG-GOM102_' (expected TRUE):",
      any(grepl("^acme_WTG-GOM102_", pngs.b)), "\n")

  # $output.filename.pattern is present in default.plotaesthetics.synth
  # (a DEPRECATED row, left in place) but should have zero effect on the
  # saved name - confirm neither file name contains any token from it.
  cat("  neither saved file used the deprecated $output.filename.pattern token '<ARU>' literally (expected TRUE):",
      !any(grepl("<ARU>", c(pngs.a, pngs.b), fixed = TRUE)), "\n")
} else {
  cat("ggplot2 not available - skipping TEST 10\n")
}

cat("\n\n########## TEST 11: an aes.default missing a required $parameter row stops with a clear message ##########\n")
default.plotaesthetics.missingparam <- default.plotaesthetics.synth[
  default.plotaesthetics.synth$parameter != "panel.spacing.x", , drop = FALSE]
result.missingparam <- tryCatch({
  batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.synth,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.missingparam
  )
  "NO ERROR - this should have stopped"
}, error = function(e) conditionMessage(e))
cat("Result with $panel.spacing.x row removed:\n", result.missingparam, "\n")

cat("\n\n########## TEST 12: dir.save controls where the PNG is written; default dir.save = getwd() ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  pngs.before.default <- list.files(getwd(), pattern = "\\.png$")
  result.dirsave.default <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.synth,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  )
  pngs.after.default <- list.files(getwd(), pattern = "\\.png$")
  cat("omitting dir.save still writes into getwd() (expected >= 1 new PNG there):",
      length(pngs.after.default) - length(pngs.before.default), "\n")
  # clean up
  file.remove(setdiff(pngs.after.default, pngs.before.default))
} else {
  cat("ggplot2 not available - skipping TEST 12\n")
}

cat("\n\n########## TEST 13: fig.list rows sharing the SAME $plot.name but otherwise DIFFERENT still each produce their own plot ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  row.base <- aru.metadata.db.synth[1, , drop = FALSE]
  row.a <- row.base; row.a$xaxe.interval <- 4
  row.b <- row.base; row.b$xaxe.interval <- 5
  row.c <- row.base; row.c$xaxe.interval <- 6
  aru.metadata.db.dupname <- rbind(row.a, row.b, row.c)
  result.dupname <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.dupname,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  )
  cat("$plots entries produced (expected 3):", length(result.dupname$plots), "\n")
} else {
  cat("ggplot2 not available - skipping TEST 13\n")
}

cat("\n\n########## TEST 14: exact full-row duplicates in fig.list are removed before plotting ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  row.for.dup <- aru.metadata.db.synth[1, , drop = FALSE]
  aru.metadata.db.exactdup <- rbind(row.for.dup, row.for.dup, row.for.dup)
  result.exactdup <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.exactdup,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  )
  cat("$plots entries produced (expected 1 - 2 duplicates removed):", length(result.exactdup$plots), "\n")

  row.b.for.dup <- row.for.dup
  row.b.for.dup$xaxe.interval <- 7
  aru.metadata.db.mixeddup <- rbind(row.for.dup, row.for.dup, row.b.for.dup)
  result.mixeddup <- batz.plotdetections_first.last(
    data = plot.data.synth,
    fig.list = aru.metadata.db.mixeddup,
    suntimes = suntimes.synth,
    aes.default = default.plotaesthetics.synth
  )
  cat("mixed set (2 exact duplicates + 1 distinct row) -> $plots entries (expected 2):",
      length(result.mixeddup$plots), "\n")
} else {
  cat("ggplot2 not available - skipping TEST 14\n")
}

cat("\n\n########## ALL TESTS COMPLETED ##########\n")

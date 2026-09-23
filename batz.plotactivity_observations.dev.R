# =============================================================================
# batz.plotactivity_observations.dev.R
# -----------------------------------------------------------------------------
# ROUND NINETEEN, per Josh's 2026-09-16 follow-up (see
# batz.plotdetections_first.last.dev.R's own header comment for the full
# text of the request - identical mechanism applied here):
#   - project.name no longer selects an aes.default override column - it
#     ONLY builds the saved file name now: "<project.name>_<ARU>_<timestamp>.png".
#     Default changed from "" to "new.project".
#   - New aes.style argument (default "overide.value") drives settings
#     resolution instead.
#   - $output.filename.pattern is DEPRECATED and no longer read at all.
#
# **JUDGMENT CALL / LIMITATION, flagged to Josh** (same as
# batz.plotdetections_first.last.dev.R): this dev script previously read
# Josh's own real device test-data files (plfr.bats.csv, processed.csv,
# fig.list.csv, plotopts_callobs.csv). The device bridge is not available in
# this session, so this round's dev script builds its own SYNTHETIC
# stand-ins for all four inputs directly in R code below, using the exact
# column layouts documented for the real files. This fully exercises the
# settings-resolution/file-naming logic that actually changed this round;
# please re-run against your own real files when you have a chance.
#
# Follow-up, 2026-09-22, per Josh's request ("change all functions that have
# aru as an header to \"aru.name\"", found via the project's own reference
# workbook and cross-checked against this script's own R/ counterpart):
# SUNTIMES.REQUIRED's "aru" entry is now "aru.name", matching
# batz.generate_suntimes.arulist()'s own renamed output column; the
# synthetic suntimes.synth stand-in below is updated to match. No function-
# body reference needed changing - suntimes is accepted and header-checked
# here but not otherwise used, exactly as in the shipped .R file.
# =============================================================================

source("batz.batusa_recode.names.R")

make.default.plotaesthetics <- function(overide.col = "overide.value") {
  rows <- list(
    c("Layout", "facpan.numcol", "3", ""),
    c("Text", "plot.title.size", "12", ""),
    c("Text", "plot.title.hjust", "0.5", ""),
    c("Text", "axis.title.size", "10", ""),
    c("Text", "axis.text.size", "8", ""),
    c("Text", "legend.text.size", "8", ""),
    c("Text", "legend.title.size", "9", ""),
    c("Layout", "panel.spacing.x", "5.5", ""),
    c("Theme", "panel.border.linewidth", "0.5", ""),
    c("Theme", "legend.position", "bottom", ""),
    c("Axes", "xaxe.interval", "4", ""),
    c("Axes", "xaxe.title", "Date", ""),
    c("Axes", "xaxe.date.buffer.days", "0.5", ""),
    c("Axes", "yaxe.title", "Number of observations", ""),
    c("Axes", "Yaxe.trans", "none", ""),
    c("Axes", "loglabels", "FALSE", ""),
    c("Axes", "y.scale", "regular", ""),
    c("Axes", "ymax", "", ""),
    c("Bars", "bar.width", "0.8", ""),
    c("Bars", "bar.alldetections.fill", "grey70", ""),
    c("Bars", "bar.40khzmyo.fill", "black", ""),
    c("Bars", "bar.fill.legend.title", "Detections", ""),
    c("Legend", "legend", "TRUE", ""),
    c("Legend", "legend.groupval.title", "Detector", ""),
    c("Legend", "legend.groupval.colors", "#1b9e77;#d95f02;#7570b3", ""),
    c("Save", "ggsave.dpi", "150", ""),
    c("Save", "ggsave.units", "in", ""),
    c("Save", "ggsave.width.pad", "0", ""),
    c("Save", "ggsave.height.pad", "0", ""),
    c("Save", "plot.width", "6", ""),
    c("Save", "plot.height", "4", ""),
    c("Layout", "facpan", "", ""),
    c("Layout", "plot.order", "", ""),
    # DEPRECATED (round nineteen) - left in place, harmless, never read.
    c("Save", "output.filename.pattern",
      "Number of bat calls detected at <ARU> between <date.start> and <date.end> <timestamp>.png", "")
  )
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("category", "parameter", "default.value", "notes")
  df[[overide.col]] <- ""
  df <- df[, c("category", "parameter", "default.value", overide.col, "notes")]
  df$notes[df$parameter == "output.filename.pattern"] <-
    "DEPRECATED as of Josh's nineteenth follow-up (2026-09-16) - no longer read; the saved file name is now always \"<project.name>_<ARU>_<timestamp>.png\"."
  df
}

default.plotaesthetics.synth <- make.default.plotaesthetics()

date.start.synth <- as.Date("2026-06-01")
date.end.synth   <- as.Date("2026-06-05")
test.dates       <- seq(date.start.synth, date.end.synth, by = "day")

plot.data.synth <- do.call(rbind, lapply(test.dates, function(d) {
  data.frame(
    spp.id      = c("Hoary bat", "Big brown bat", "40kMyo"),
    date        = format(d, "%m/%d/%Y"),
    aru.groupby = "WTG-GOM102",
    obs         = c(3, 5, 1),
    stringsAsFactors = FALSE
  )
}))

suntimes.synth <- data.frame(
  aru.name = "WTG-GOM102", date = "06/01/2026", date.mon = "06/02/2026",
  sunregion = "WTG", time.zone = "UTC", sunregion.type = "coordinates",
  schedual1 = "civil", schedual2 = "civil",
  suns = "06/01/2026 20:00", suns.unix = 0,
  sunr = "06/01/2026 06:00", sunr.unix = 0,
  sunr.mon = "06/02/2026 06:00", sunr.mon.unix = 0,
  stringsAsFactors = FALSE
)

aru.metadata.db.synth <- data.frame(
  plot.type      = "call.observations",
  plot.name      = "Test Site - Call Observations",
  facet          = "sppid",
  facet.set      = "NE",
  MYSO           = FALSE,
  Alldect        = TRUE,
  facet.panel    = "",
  "40khzmyo"     = TRUE,
  facet.label    = "common",
  plot.group     = "aru.groupby",
  plot.sets      = "WTG-GOM102",
  pool           = FALSE,
  date.format    = "%b-%d/n%Y",
  date.start     = format(date.start.synth, "%m/%d/%Y"),
  date.end       = format(date.end.synth, "%m/%d/%Y"),
  xaxe.interval  = 4,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cat("=== synthetic aes.default (round nineteen: category/parameter/default.value/overide.value/notes) ===\n")
print(head(default.plotaesthetics.synth))

# -----------------------------------------------------------------------------
# batz.plotactivity_observations() - dev copy, mirrors the package .R file
# -----------------------------------------------------------------------------
PLOT.TYPE <- "call.observations"

DATA.REQUIRED <- c("spp.id", "date", "obs")
# Renamed 2026-09-22 (per Josh, see the header comment above): matches
# batz.generate_suntimes.arulist()'s own renamed output column.
SUNTIMES.REQUIRED <- c("aru.name", "date", "date.mon", "sunregion", "time.zone",
                        "sunregion.type", "schedual1", "schedual2", "suns",
                        "suns.unix", "sunr", "sunr.unix", "sunr.mon", "sunr.mon.unix")
FIG.LIST.REQUIRED <- c("plot.type", "plot.name", "facet", "facet.set", "MYSO",
                        "Alldect", "facet.panel", "40khzmyo", "facet.label",
                        "plot.group", "plot.sets", "pool", "date.format",
                        "date.start", "date.end", "xaxe.interval")
AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")

AES.DEFAULT.REQUIRED.PARAMETERS <- c(
  "facpan.numcol", "plot.title.size", "plot.title.hjust", "axis.title.size",
  "axis.text.size", "legend.text.size", "legend.title.size", "panel.spacing.x",
  "panel.border.linewidth", "legend.position",
  "xaxe.interval", "xaxe.title", "xaxe.date.buffer.days", "yaxe.title",
  "Yaxe.trans", "loglabels", "y.scale", "ymax", "bar.width",
  "bar.alldetections.fill", "bar.40khzmyo.fill", "bar.fill.legend.title",
  "legend", "legend.groupval.title", "legend.groupval.colors",
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
    return(sprintf("%s is missing these required $parameter rows: %s",
                    label, paste(missing, collapse = ", ")))
  }
  NULL
}
check.duplicates <- function(df, label) {
  nm <- names(df)
  dups <- unique(nm[duplicated(nm)])
  if (length(dups) > 0) {
    return(sprintf("%s has duplicate column name(s): %s", label, paste(dups, collapse = ", ")))
  }
  NULL
}

batz.plotactivity_observations <- function(data, fig.list, suntimes,
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
  if (length(problems) > 0) stop(paste(problems, collapse = "\n"))

  unquote <- function(x) {
    x <- trimws(as.character(x))
    gsub('^"(.*)"$', "\\1", x)
  }
  parse.plot.sets <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) == 0 || is.na(x) || !nzchar(x)) return(character(0))
    x <- gsub('"', " ", x, fixed = TRUE)
    vals <- strsplit(trimws(x), "\\s+")[[1]]
    vals[nzchar(vals)]
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
  norm.simple <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
  KHZ.ALIASES <- c("40khzmyo", "40kmyo")

  jobs <- fig.list[!is.na(fig.list$plot.type) & nzchar(trimws(fig.list$plot.type)), , drop = FALSE]
  if (nrow(jobs) == 0) stop("fig.list has no plot rows - nothing to plot.")

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
      cat(sprintf("NOTE: fig.list row for '%s' has plot.type = '%s' - skipped.\n", job.label, job$plot.type))
      next
    }
    facet.kind <- tolower(trimws(job$facet))
    if (!identical(facet.kind, "sppid")) {
      cat(sprintf("NOTE: fig.list row for '%s' has facet = '%s' - skipped.\n", job.label, job$facet))
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
    if (alldect.flag) { spp.plot <- c(spp.plot, "All detections"); facpan <- c(facpan, "All detections") }
    khz.flag <- isTRUE(as.logical(job[["40khzmyo"]]))
    if (khz.flag) { spp.plot <- c(spp.plot, "40khzmyo"); if (!alldect.flag) facpan <- c(facpan, "40khzmyo") }
    spp.plot <- unique(trimws(spp.plot)); facpan <- unique(trimws(facpan))
    spp.plot <- batz.batusa_recode.names(spp.plot, batname.format.out = "common")
    facpan <- batz.batusa_recode.names(facpan, batname.format.out = "common")

    pd <- data
    is.khz.raw <- norm.simple(pd$spp.id) %in% KHZ.ALIASES
    pd$spp.common <- batz.batusa_recode.names(pd$spp.id, batname.format.out = "common")
    pd$spp.common[is.khz.raw] <- "40khzmyo"

    group.col <- trimws(job$plot.group)
    if (!nzchar(group.col)) { cat(sprintf("NOTE: '%s' blank $plot.group - skipped.\n", job.label)); next }
    if (!(group.col %in% names(pd))) { cat(sprintf("NOTE: '%s' $plot.group='%s' not a column - skipped.\n", job.label, group.col)); next }

    plot.sets.vals <- parse.plot.sets(job$plot.sets)
    if (length(plot.sets.vals) > 0) {
      pd <- pd[tolower(trimws(as.character(pd[[group.col]]))) %in% tolower(plot.sets.vals), , drop = FALSE]
    }
    pd <- pd[tolower(trimws(pd$spp.common)) %in% tolower(spp.plot), , drop = FALSE]

    date.start <- parse.flex.date(get.setting(job, "date.start"))
    date.end <- parse.flex.date(get.setting(job, "date.end"))
    pd$date.parsed <- parse.flex.date(pd$date)
    pd <- pd[!is.na(pd$date.parsed) & pd$date.parsed >= date.start & pd$date.parsed <= date.end, , drop = FALSE]

    if (nrow(pd) == 0) { cat(sprintf("NOTE: '%s' matched 0 rows.\n", job.label)); next }

    khz.own.panel <- khz.flag && !alldect.flag
    pd$facet.panel.value <- ifelse(tolower(pd$spp.common) == "40khzmyo" & !khz.own.panel, "All detections", pd$spp.common)
    pd$bar.type <- ifelse(tolower(pd$spp.common) == "40khzmyo", "40kHzMyo", "All detections")
    pd$group.val <- as.character(pd[[group.col]])

    pool.flag <- isTRUE(as.logical(job$pool))
    if (pool.flag) {
      pd <- stats::aggregate(obs ~ spp.common + facet.panel.value + bar.type + date.parsed, data = pd, FUN = sum)
      pd$group.val <- "pooled"
    }

    legend.flag <- isTRUE(as.logical(get.setting(job, "legend")))

    facet.label.fmt <- unquote(get.setting(job, "facet.label"))
    if (is.na(facet.label.fmt) || !nzchar(facet.label.fmt)) facet.label.fmt <- "common"
    panel.levels.raw <- facpan
    panel.labels <- batz.batusa_recode.names(panel.levels.raw, batname.format.out = facet.label.fmt)
    names(panel.labels) <- panel.levels.raw
    plot.order.raw <- strsplit(get.setting(job, "plot.order"), ";", fixed = TRUE)[[1]]
    ordered.levels <- intersect(trimws(plot.order.raw), panel.levels.raw)
    ordered.levels <- c(ordered.levels, setdiff(panel.levels.raw, ordered.levels))
    pd$facet.panel.value <- factor(pd$facet.panel.value, levels = ordered.levels, labels = panel.labels[ordered.levels])

    yaxe.trans <- tolower(trimws(get.setting(job, "Yaxe.trans")))
    if (!yaxe.trans %in% c("none", "log", "log10")) yaxe.trans <- "none"
    loglabels <- isTRUE(as.logical(get.setting(job, "loglabels")))
    y.scale.mode <- tolower(trimws(get.setting(job, "y.scale")))
    if (!y.scale.mode %in% c("regular", "rounded", "custom")) y.scale.mode <- "regular"

    ymax.raw <- suppressWarnings(as.numeric(get.setting(job, "ymax")))
    if (is.na(ymax.raw) || ymax.raw <= 0) {
      ymax.raw <- max(pd$obs, na.rm = TRUE)
    }

    trans.fn <- switch(yaxe.trans, none = function(x) x, log = function(x) log1p(x), log10 = function(x) log10(x + 1))
    inv.trans.fn <- switch(yaxe.trans, none = function(x) x, log = function(x) expm1(x), log10 = function(x) 10^x - 1)

    if (y.scale.mode == "custom") {
      y.custom.raw <- suppressWarnings(as.numeric(strsplit(get.setting(job, "y.custom"), ";", fixed = TRUE)[[1]]))
      y.custom.raw <- sort(unique(y.custom.raw[!is.na(y.custom.raw)]))
      if (length(y.custom.raw) == 0) y.scale.mode <- "regular"
    }
    if (y.scale.mode != "custom") {
      frac <- c(0, 0.25, 0.5, 0.75, 1)
      trans.lo <- trans.fn(0); trans.hi <- trans.fn(ymax.raw)
      raw.breaks <- inv.trans.fn(trans.lo + frac * (trans.hi - trans.lo))
      if (y.scale.mode == "rounded") raw.breaks <- round(raw.breaks)
    } else {
      raw.breaks <- y.custom.raw
    }
    raw.breaks <- sort(unique(raw.breaks))
    y.upper <- max(c(ymax.raw, raw.breaks, pd$obs), na.rm = TRUE)

    pd$obs.plot <- trans.fn(pd$obs)
    break.pos <- trans.fn(raw.breaks)
    label.breaks <- if (y.scale.mode == "custom") raw.breaks else round(raw.breaks, 1)
    break.labels <- if (loglabels) format(round(break.pos, 2)) else format(label.breaks, big.mark = ",", trim = TRUE, scientific = FALSE)

    plots[[job.key]] <- list(
      job.label = job.label, job = job, pd = pd, panel.labels = panel.labels,
      facpan = facpan, spp.plot = spp.plot, date.start = date.start, date.end = date.end,
      khz.flag = khz.flag, break.pos = break.pos, break.labels = break.labels,
      y.upper.plot = trans.fn(y.upper), group.col = group.col,
      plot.sets.vals = plot.sets.vals, pool.flag = pool.flag, legend.flag = legend.flag,
      resolved.legend.position = get.default("legend.position")
    )
    cat(sprintf("Prepared plot data for '%s': %d observation row(s) across %d panel(s).\n",
                 job.label, nrow(pd), length(panel.levels.raw)))
  }

  if (length(plots) == 0) { cat("No plots were generated.\n"); return(invisible(list(plots = list(), ggplots = list()))) }

  ggplots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (job.key in names(plots)) {
      p <- plots[[job.key]]
      job.label <- p$job.label

      xaxe.date.labels.fmt <- gsub("/n", "\n", get.setting(p$job, "date.format"), fixed = TRUE)
      xaxe.n.labels <- suppressWarnings(as.numeric(get.setting(p$job, "xaxe.interval")))
      if (is.na(xaxe.n.labels) || xaxe.n.labels < 1) xaxe.n.labels <- 2
      xaxe.breaks <- seq(p$date.start, p$date.end, length.out = round(xaxe.n.labels))
      xaxe.buffer <- suppressWarnings(as.numeric(get.default("xaxe.date.buffer.days")))
      if (is.na(xaxe.buffer)) xaxe.buffer <- 0.5

      fill.legend.limits <- if (isTRUE(p$khz.flag)) c("All detections", "40kHzMyo") else "All detections"
      fill.legend.breaks <- if (isTRUE(p$khz.flag)) "40kHzMyo" else character(0)

      dodge.flag <- !isTRUE(p$pool.flag) && length(unique(p$pd$group.val)) > 1
      bar.width.val <- as.numeric(get.default("bar.width"))
      bar.position <- if (dodge.flag) ggplot2::position_dodge2(width = bar.width.val, padding = 0.1, preserve = "single") else "identity"

      show.groupval.color <- isTRUE(p$legend.flag) && dodge.flag

      if (show.groupval.color) {
        groupval.levels <- sort(unique(p$pd$group.val))
        groupval.palette <- strsplit(get.default("legend.groupval.colors"), ";", fixed = TRUE)[[1]]
        groupval.palette <- trimws(groupval.palette[nzchar(trimws(groupval.palette))])
        if (length(groupval.palette) == 0) groupval.palette <- c("#1b9e77", "#d95f02", "#7570b3")
        groupval.colors <- groupval.palette[((seq_along(groupval.levels) - 1) %% length(groupval.palette)) + 1]
        names(groupval.colors) <- groupval.levels
        pd.fill <- p$pd
        pd.fill$fill.val <- ifelse(pd.fill$bar.type == "40kHzMyo", "40kHzMyo", pd.fill$group.val)
        fill.values <- c(groupval.colors, `40kHzMyo` = get.default("bar.40khzmyo.fill"))
        fill.breaks <- if (isTRUE(p$khz.flag)) c(groupval.levels, "40kHzMyo") else groupval.levels

        g <- ggplot2::ggplot(pd.fill, ggplot2::aes(x = date.parsed)) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.val, group = group.val),
                             width = bar.width.val, position = bar.position) +
          ggplot2::geom_col(data = pd.fill[pd.fill$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = fill.val, group = group.val),
                             width = bar.width.val, position = bar.position) +
          ggplot2::scale_fill_manual(name = get.default("legend.groupval.title"),
                                      breaks = fill.breaks, limits = names(fill.values), values = fill.values)
      } else {
        g <- ggplot2::ggplot(p$pd, ggplot2::aes(x = date.parsed)) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "All detections", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val),
                             width = bar.width.val, position = bar.position) +
          ggplot2::geom_col(data = p$pd[p$pd$bar.type == "40kHzMyo", , drop = FALSE],
                             ggplot2::aes(y = obs.plot, fill = bar.type, group = group.val),
                             width = bar.width.val, position = bar.position) +
          ggplot2::scale_fill_manual(name = get.default("bar.fill.legend.title"),
            breaks = fill.legend.breaks, limits = fill.legend.limits,
            values = c(`All detections` = get.default("bar.alldetections.fill"),
                       `40kHzMyo` = get.default("bar.40khzmyo.fill")))
      }

      g <- g +
        ggplot2::scale_y_continuous(limits = c(0, p$y.upper.plot), breaks = p$break.pos, labels = p$break.labels,
          name = paste0("\n", get.setting(p$job, "yaxe.title"))) +
        ggplot2::scale_x_date(limits = c(p$date.start - xaxe.buffer, p$date.end + xaxe.buffer),
          breaks = xaxe.breaks, date_labels = xaxe.date.labels.fmt,
          name = paste0("\n", get.setting(p$job, "xaxe.title"))) +
        ggplot2::facet_wrap(~facet.panel.value, ncol = as.numeric(get.default("facpan.numcol")), drop = FALSE) +
        ggplot2::labs(title = job.label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
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

      aru.token <- paste(p$plot.sets.vals, collapse = "+")
      if (!nzchar(aru.token)) aru.token <- p$group.col
      if (isTRUE(p$pool.flag)) aru.token <- paste0(aru.token, "-pooled")
      fname <- sprintf("%s_%s_%s.png", project.name, aru.token, format(Sys.time(), "%Y%m%d_%H%M%S"))
      fname <- file.path(dir.save, fname)

      ggplot2::ggsave(fname, plot = g,
        width = as.numeric(get.default("plot.width")) + as.numeric(get.default("ggsave.width.pad")),
        height = as.numeric(get.default("plot.height")) + as.numeric(get.default("ggsave.height.pad")),
        units = get.default("ggsave.units"), dpi = as.numeric(get.default("ggsave.dpi")))
      cat("Saved:", fname, "\n")
    }
  } else {
    cat("ggplot2 not installed.\n")
  }

  invisible(list(plots = plots, ggplots = ggplots))
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n\n########## TEST 1: header checks catch real problems ##########\n")
tryCatch(
  batz.plotactivity_observations(plot.data.synth[, setdiff(names(plot.data.synth), "obs")],
                                  aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.synth),
  error = function(e) cat("Got expected error:\n", conditionMessage(e), "\n")
)

cat("\n\n########## TEST 2: full pipeline, default project.name/aes.style ##########\n")
result2 <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.synth)
cat("$plots entries (expected 1):", length(result2$plots), "\n")

cat("\n\n########## TEST 3: aes.style override (round nineteen) ##########\n")
default.plotaesthetics.override <- default.plotaesthetics.synth
default.plotaesthetics.override$overide.value[default.plotaesthetics.override$parameter == "legend.position"] <- "top"
result3 <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.override)
cat("legend.position resolved with $overide.value filled in:",
    result3$plots[[1]]$resolved.legend.position, "(expected 'top')\n")
result3b <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.override,
                                            project.name = "some.other.project")
cat("legend.position with a DIFFERENT project.name, same $overide.value:",
    result3b$plots[[1]]$resolved.legend.position, "(expected 'top' - project.name no longer matters for settings)\n")

cat("\n\n########## TEST 4: backward compatibility - no $overide.value column at all ##########\n")
default.plotaesthetics.nocol <- default.plotaesthetics.synth
default.plotaesthetics.nocol$overide.value <- NULL
result4 <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.nocol)
cat("legend.position with no $overide.value column present at all:",
    result4$plots[[1]]$resolved.legend.position, "(expected 'bottom')\n")

cat("\n\n########## TEST 5: custom aes.style column name ##########\n")
default.plotaesthetics.custom <- default.plotaesthetics.synth
default.plotaesthetics.custom$overide.value <- NULL
default.plotaesthetics.custom$my.custom.col <- ""
default.plotaesthetics.custom$my.custom.col[default.plotaesthetics.custom$parameter == "legend.position"] <- "top"
result5 <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.custom,
                                           aes.style = "my.custom.col")
cat("legend.position resolved with aes.style = 'my.custom.col':",
    result5$plots[[1]]$resolved.legend.position, "(expected 'top')\n")

cat("\n\n########## TEST 6: project.name drives the saved file name; $output.filename.pattern is ignored ##########\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  dir.save.test <- file.path(tempdir(), paste0("plotactivity_dirsave_test_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
  dir.create(dir.save.test)
  result6a <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.synth,
                                              dir.save = dir.save.test)
  pngs.a <- list.files(dir.save.test, pattern = "\\.png$")
  cat("default project.name -> file name(s):", paste(pngs.a, collapse = ", "), "\n")
  cat("  starts with 'new.project_WTG-GOM102_' (expected TRUE):", any(grepl("^new\\.project_WTG-GOM102_", pngs.a)), "\n")

  result6b <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.synth,
                                              project.name = "acme", dir.save = dir.save.test)
  pngs.b <- setdiff(list.files(dir.save.test, pattern = "\\.png$"), pngs.a)
  cat("project.name = 'acme' -> file name(s):", paste(pngs.b, collapse = ", "), "\n")
  cat("  starts with 'acme_WTG-GOM102_' (expected TRUE):", any(grepl("^acme_WTG-GOM102_", pngs.b)), "\n")
  cat("  neither file used the deprecated pattern's literal '<ARU>' token (expected TRUE):",
      !any(grepl("<ARU>", c(pngs.a, pngs.b), fixed = TRUE)), "\n")
} else {
  cat("ggplot2 not available - skipping TEST 6\n")
}

cat("\n\n########## TEST 7: an aes.default missing a required $parameter row stops with a clear message ##########\n")
default.plotaesthetics.missing <- default.plotaesthetics.synth[default.plotaesthetics.synth$parameter != "bar.width", , drop = FALSE]
result.missing <- tryCatch({
  batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.missing)
  "NO ERROR"
}, error = function(e) conditionMessage(e))
cat("Result with $bar.width row removed:\n", result.missing, "\n")

cat("\n\n########## TEST 8: 40kHzMyo bar overlay via the '40kMyo' real-data alias ##########\n")
result8 <- batz.plotactivity_observations(plot.data.synth, aru.metadata.db.synth, suntimes.synth, default.plotaesthetics.synth)
n.khz.rows <- sum(result8$plots[[1]]$pd$bar.type == "40kHzMyo")
cat("Rows recognized as 40kHzMyo bar.type (expected 5, one per test date):", n.khz.rows, "\n")

cat("\n\n########## TEST 9: exact full-row fig.list duplicates are removed before plotting ##########\n")
jobs.dup <- rbind(aru.metadata.db.synth, aru.metadata.db.synth, aru.metadata.db.synth)
result9 <- batz.plotactivity_observations(plot.data.synth, jobs.dup, suntimes.synth, default.plotaesthetics.synth)
cat("$plots entries produced (expected 1 - 2 duplicates removed):", length(result9$plots), "\n")

cat("\n\nALL TESTS COMPLETED\n")

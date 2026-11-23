#' Mean nightly calls per ARU with evening temperature and a rain strip
#'
#' For each sun region, draws the mean number of observations per stationary
#' ARU per night (red bars) and the region's mobile detector (blue bars) as
#' species facets, like \code{\link{batz.plot.bar_activity.site}}, and adds
#' the night's maximum and minimum temperature between \code{temp.window}
#' (default 20:00 to midnight) on a secondary Y axis and a strip along the
#' top of every panel showing when it rained.
#'
#' @param data Data frame of per-ARU, per-night counts, normally the output
#'   of \code{\link{batz.generate_plotframe.bat}} (one ARU per group):
#'   \code{$spp.id}, \code{$date} (the monitoring night; m/d/Y, Y-m-d or
#'   m/d/y), \code{$observations.count} and \code{$aru.name} (or
#'   \code{$group}). Headers are matched ignoring case and "." / "_" / spaces.
#' @param weather Data frame of weather records, normally
#'   \code{$weatherstation.data} from
#'   \code{\link{batz.generate_weatherstation.data}}: \code{$site} (the sun
#'   region; several separated by ";"), \code{$date.time} (station local
#'   time, \code{"YYYY-MM-DD HH:MM:SS"}), \code{$temperature.c} (or
#'   \code{$temperature.f}) and \code{$precipitation.mm_interval} (or
#'   \code{$precipitation.in_interval}; else \code{$precipitation.mm} /
#'   \code{$precipitation.in} as a running daily total).
#' @param arulist Data frame of the ARU deployments (an \verb{*arulist.csv}):
#'   \code{$aru.name}, \code{$aru.label}, \code{$sunregion},
#'   \code{$date.start}, \code{$date.end}. It says which ARUs are the
#'   stationary detectors of each sun region and sets the default date
#'   window (first \code{$date.start} to the night before the last
#'   \code{$date.end}).
#' @param fig.list Optional, default \code{NULL}: one plot per sun region in
#'   \code{arulist} that has data. Otherwise a data frame (or a \code{.csv}
#'   / \code{.xlsx} path, or a list holding \code{$fig.list}), one row per
#'   plot; only rows whose \code{$plot.type} is
#'   \code{"barline_meancalls.weather"} and whose \code{$plot} is TRUE are
#'   plotted. \code{$plot.set}: the sun region. \code{$plot.name}: plot
#'   title (blank: "<sun region> mean calls per ARU and weather").
#'   \code{$date.start}, \code{$date.end}: the window (blank: from
#'   \code{arulist}). \code{$break.start}, \code{$break.end}: nights one
#'   detector was out (the mean is divided by one fewer; not shaded).
#'   Other \code{\link{batz.plot.bar_activity.site}} columns (e.g.
#'   \code{$facet.set}, \code{$y.custom}, \code{$date.interval}) are passed
#'   on.
#' @param mobile.nights Optional data frame, default \code{NULL}:
#'   \code{$sunregion} and \code{$date} (m/d/Y, Y-m-d, Excel serial, several
#'   separated by ";"). Gives the sun region of a mobile detector whose name
#'   has no number (e.g. \code{"MOB"}, \code{"MOBILE"}): the region whose
#'   list holds that night.
#' @param aes.plotopts Optional, default \code{NULL} (the built-in
#'   \code{pltop.barline_meancalls.weather} settings). A data frame, or the
#'   path of a settings \code{.csv} or a pooled \code{.xlsx} (tab
#'   \verb{pltop.barline_meancalls.weather}), with \code{$parameter},
#'   \code{$default.value} and the \code{aes.style} column. See Details for
#'   the settings.
#' @param aes.style Character, default \code{"override.value"}. The
#'   \code{aes.plotopts} column read before \code{$default.value}.
#' @param project.name Character, default \code{"new.project"}. First part
#'   of the file name; blank leaves it out.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in. \code{NULL}: nothing saved.
#' @param save.auto Default \code{TRUE}: the files are saved in
#'   \code{dir.save}. \code{FALSE}: no files are saved; each plot is only
#'   printed (drawn). \code{"BOTH"}: each plot is saved and printed.
#'   \code{"print"}: nothing is saved; each plot is printed instead.
#'   \code{"generate <number>"}: nothing is saved; up to \code{<number>}
#'   plots are created as objects in the R global environment, named after
#'   the file they would have been saved as (without extension and time
#'   stamp); \code{"generate"} alone creates every plot.
#' @param ... Settings typed into the call, named as the \code{aes.plotopts}
#'   \code{$parameter} rows (e.g. \code{n.detectors = 3},
#'   \code{temp.range = c(5, 30)}), or as settings of
#'   \code{\link{batz.plot.bar_activity.site}} (e.g.
#'   \code{facet.set = "AllDet;NE"}, \code{legend.position = "bottom"}),
#'   which are passed on. A vector is joined with ";".
#'
#' @details
#' \strong{Settings} are read from the call (\code{...}), then the fig.list
#' row (a column of the same name), then \code{aes.plotopts}
#' (\code{aes.style} column, then \code{$default.value}):
#' \describe{
#'   \item{\code{n.detectors}}{Stationary detectors per sun region, default
#'     \code{3}; \code{"arulist"} = the number of different
#'     \code{$aru.label} of the region in \code{arulist}.}
#'   \item{\code{temp.window}}{Clock times, default \code{"20:00;24:00"}:
#'     the night's max and min temperature are taken from readings in this
#'     window on the night's date.}
#'   \item{\code{temp.range}}{Secondary Y axis limits (degrees C), e.g.
#'     \code{"5;30"}. Blank (default): from the smallest to the largest value
#'     of all plots in this call, rounded to 5, so they share one scale.}
#'   \item{\code{rain.windows}}{Default
#'     \code{"12:00-18:00;18:00-24:00;00:00-08:00"}, in night order. A window
#'     starting before 12:00 is on the next morning.}
#'   \item{\code{rain.min.mm}}{Default \code{1}: a window counts as rain when
#'     its total is more than this.}
#'   \item{\code{rain.colors}}{One per window, default
#'     \code{"lightblue;black;lightblue"}; \code{rain.none.color}, default
#'     \code{"white"}: no rain.}
#'   \item{\code{rain.multiple}}{Rain in more than one window: \code{"second"}
#'     (default; the second wet window in night order), \code{"first"} or
#'     \code{"last"}.}
#'   \item{\code{rain.caption}}{TRUE (default): a note under the plot
#'     explains the strip colours.}
#'   \item{\code{bar.mean.fill}, \code{bar.mobile.fill}}{Bar colours, default
#'     \code{"red"} and \code{"#1f78b4"}.}
#'   \item{\code{temp.line.color}, \code{temp.max.linetype},
#'     \code{temp.min.linetype}}{Default \code{"grey20"}, \code{"solid"},
#'     \code{"dashed"}.}
#'   \item{\code{yaxe.title}, \code{yaxe.title.right}, \code{xaxe.title}}{Axis
#'     titles; \code{yaxe.title.right} blank = "Temperature <temp.window>
#'     (degrees C)".}
#'   \item{\code{plot.width}, \code{plot.height}}{PNG size (inches), default
#'     9 x 7.}
#'   \item{Bar plot settings}{\code{facet.set}, \code{facet.label},
#'     \code{40khzmyo}, \code{yaxe.trans}, \code{y.scale}, \code{y.custom},
#'     \code{ymax}, \code{date.format}, \code{legend.40khzmyo}: passed to
#'     \code{\link{batz.plot.bar_activity.site}}. Defaults: the nine
#'     New England species facets with "All detections" first, log axis
#'     with labels 0, 5, 20, 70, 300, 1,000 (top 1,000), dates "\%b \%d" every
#'     7 days, 40kHzMyo key first.}
#' }
#'
#' \strong{Mean per ARU.} For each night in the window and each species, the
#' counts of the region's stationary ARUs (\code{data} rows whose ARU is in
#' \code{arulist} for that sun region; no row = 0) are added and divided by
#' \code{n.detectors}, or \code{n.detectors - 1} on the nights from
#' \code{$break.start} to \code{$break.end}.
#'
#' \strong{Mobile.} A detector whose name contains "MOB" (any case, e.g.
#' "105059-MOB", "MOBILE105059", "MOB") is a mobile detector. Its sun region
#' is the number in its name, else the region from \code{mobile.nights}; a
#' mobile with no region is left out (NOTE). Its counts are drawn as they
#' are, as \verb{<sun region>-MOB}.
#'
#' \strong{Weather.} \code{weather} rows whose \code{$site} lists the sun
#' region are used. Temperature in F is converted to C, precipitation in
#' inches to mm. A night with no temperature readings in \code{temp.window}
#' has no temperature point.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]barline_meancalls.weather_<sun region>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#'
#' @return Invisibly, a list: \code{$plots} (named by sun region, the ggplot
#'   objects), \code{$data} (the plotted bars: \code{$sunregion},
#'   \code{$spp.id}, \code{$date}, \code{$observations.count} (mean, or the
#'   mobile's count), \code{$group}, \code{$detectors}), \code{$nights} (one
#'   row per sun region and night: \code{$sunregion}, \code{$date},
#'   \code{$temperature.max}, \code{$temperature.min}, the rain total of
#'   each window (\verb{$rain.mm_<start>to<end>}, e.g.
#'   \code{$rain.mm_1200to1800}), \code{$rain} (the window shown, or
#'   "none") and \code{$rain.window} (its number)), and
#'   \code{$files}.
#'
#' @seealso \code{\link{batz.plot.bar_activity.site}},
#'   \code{\link{batz.plot.line_weather.average}}
#'
#' @examples
#' \dontrun{
#' bats <- read.csv("plfr.NA.bat.csv")
#' weather <- read.csv("weather.csv")
#' arulist <- read.csv("NA-Bat_20261118173904.arulist.csv")
#' batz.plot.barline_meancalls.weather(bats, weather, arulist, project.name = "NA-Bat")
#'
#' # outages and mobile nights from a fig.list
#' fl <- data.frame(plot = TRUE, plot.type = "barline_meancalls.weather", plot.set = "1079",
#'                  date.start = "7/17/2026", date.end = "8/17/2026",
#'                  break.start = "7/19/2026", break.end = "7/30/2026")
#' batz.plot.barline_meancalls.weather(bats, weather, arulist, fig.list = fl,
#'                                     mobile.nights = data.frame(sunregion = "107802", date = "7/20/2026"))
#' }
#' @export
batz.plot.barline_meancalls.weather <- function(data, weather, arulist, fig.list = NULL, mobile.nights = NULL,
                                                aes.plotopts = NULL, aes.style = "override.value",
                                                project.name = "new.project", dir.save = getwd(), save.auto = TRUE, ...) {
  FN <- "batz.plot.barline_meancalls.weather"
  PLOT.TYPE <- "barline_meancalls.weather"
  FIG.LIST.REQUIRED <- c("plot", "plot.type", "plot.set")
  FIG.LIST.OPTIONAL <- c("plot.name", "date.start", "date.end", "date.interval", "break.start", "break.end")
  sv <- util_saver(save.auto, FN, plot = TRUE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop(FN, ' needs the ggplot2 package: install.packages("ggplot2")', call. = FALSE)
  dots <- plotutil_dots(...)
  aes.plotopts <- plotutil_read_plotopts(aes.plotopts, FN)
  po <- if (is.data.frame(aes.plotopts)) aes.plotopts else tryCatch(batz.db(paste0("plotopts.", FN)), error = function(e) NULL)
  own <- if (is.data.frame(po)) as.character(po$parameter) else character(0)
  blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) || all(!nzchar(trimws(as.character(x)))) || identical(toupper(trimws(as.character(x[1]))), "NA")
  setting <- function(job, k) {                                   # call > fig.list > plotopts
    if (!is.null(dots[[k]])) return(paste(as.character(dots[[k]]), collapse = ";"))
    if (!is.null(job) && k %in% names(job) && !blank(job[[k]])) return(as.character(job[[k]][1]))
    v <- plotutil_po_value(po, aes.style, k); if (is.null(v)) NA_character_ else v
  }
  splt <- function(v) { x <- trimws(unlist(strsplit(as.character(v), ";", fixed = TRUE))); x[nzchar(x)] }
  key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  pick <- function(df, cands) { k <- key(names(df)); for (cc in cands) { i <- match(key(cc), k); if (!is.na(i)) return(names(df)[i]) }; NA_character_ }
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  rdate <- function(v) {                                          # Excel serial, Y-m-d, m/d/Y, m/d/y
    v <- trimws(as.character(v)); out <- as.Date(rep(NA_character_, length(v)))
    n <- suppressWarnings(as.numeric(v)); i <- !is.na(n) & n > 20000 & n < 80000
    out[i] <- as.Date(n[i], origin = "1899-12-30")
    for (f in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d")) { j <- is.na(out) & nzchar(v); if (any(j)) out[j] <- as.Date(sub(" .*$", "", v[j]), format = f) }
    out
  }
  clock <- function(s) { p <- num(strsplit(trimws(s), ":", fixed = TRUE)[[1]]); p[1] + if (length(p) > 1) p[2] / 60 else 0 }

  ## ---- inputs ------------------------------------------------------------------
  if (!is.data.frame(data) || !nrow(data)) stop(FN, ": data must be a data frame with rows.", call. = FALSE)
  if (!is.data.frame(weather) || !nrow(weather)) stop(FN, ": weather must be a data frame with rows.", call. = FALSE)
  if (!is.data.frame(arulist) || !nrow(arulist)) stop(FN, ": arulist must be a data frame with rows.", call. = FALSE)
  cd <- list(spp = pick(data, c("spp.id", "sppid")), date = pick(data, c("date", "date.monitoringnight")),
             obs = pick(data, c("observations.count", "obs")), aru = pick(data, c("aru.name", "aru", "group")))
  cw <- list(site = pick(weather, "site"), dt = pick(weather, "date.time"), tc = pick(weather, "temperature.c"),
             tf = pick(weather, "temperature.f"), pmi = pick(weather, "precipitation.mm_interval"),
             pii = pick(weather, "precipitation.in_interval"), pm = pick(weather, "precipitation.mm"), pin = pick(weather, "precipitation.in"))
  ca <- list(aru = pick(arulist, "aru.name"), lab = pick(arulist, "aru.label"), sr = pick(arulist, "sunregion"),
             d0 = pick(arulist, "date.start"), d1 = pick(arulist, "date.end"))
  mh <- function(pre, nm, cols) { m <- nm[is.na(unlist(cols))]; if (length(m)) paste0(pre, " $", m) else character(0) }
  miss <- c(mh("data", c("spp.id", "date", "observations.count", "aru.name"), cd),
            mh("weather", c("site", "date.time"), list(cw$site, cw$dt)),
            if (is.na(cw$tc) && is.na(cw$tf)) "weather $temperature.c (or $temperature.f)",
            if (all(is.na(c(cw$pmi, cw$pii, cw$pm, cw$pin)))) "weather $precipitation.mm_interval (or $precipitation.mm)",
            mh("arulist", c("aru.name", "aru.label", "sunregion", "date.start", "date.end"), ca))
  if (length(miss)) stop(FN, ": missing headers: ", paste(miss, collapse = ", "), call. = FALSE)
  b <- data.frame(spp = as.character(data[[cd$spp]]), date = rdate(data[[cd$date]]), obs = num(data[[cd$obs]]),
                  aru = trimws(as.character(data[[cd$aru]])), stringsAsFactors = FALSE)
  if (any(is.na(b$date))) { cat(sprintf("NOTE: %s - %d data row(s) with an unreadable $date dropped.\n", FN, sum(is.na(b$date)))); b <- b[!is.na(b$date), ] }
  b$obs[is.na(b$obs)] <- 0
  al <- data.frame(aru = trimws(as.character(arulist[[ca$aru]])), lab = as.character(arulist[[ca$lab]]),
                   sr = trimws(as.character(arulist[[ca$sr]])), d0 = rdate(arulist[[ca$d0]]), d1 = rdate(arulist[[ca$d1]]) - 1,
                   stringsAsFactors = FALSE)
  ## weather: local clock time read from the text, values in C and mm (interval)
  wt <- as.character(weather[[cw$dt]])
  wd <- data.frame(site = as.character(weather[[cw$site]]), date = as.Date(substr(wt, 1, 10)),
                   hour = num(substr(wt, 12, 13)) + num(substr(wt, 15, 16)) / 60,
                   temp = if (!is.na(cw$tc)) num(weather[[cw$tc]]) else (num(weather[[cw$tf]]) - 32) * 5 / 9, stringsAsFactors = FALSE)
  wd$rain <- if (!is.na(cw$pmi)) num(weather[[cw$pmi]]) else if (!is.na(cw$pii)) num(weather[[cw$pii]]) * 25.4 else {
    tot <- if (!is.na(cw$pm)) num(weather[[cw$pm]]) else num(weather[[cw$pin]]) * 25.4
    o <- order(wd$site, wt); r <- tot
    r[o] <- stats::ave(tot[o], wd$site[o], wd$date[o], FUN = function(p) { ok <- !is.na(p); if (any(ok)) p[ok] <- pmax(0, c(p[ok][1], diff(p[ok]))); p })
    r
  }
  wd <- wd[!is.na(wd$date), , drop = FALSE]

  ## ---- mobile detectors -------------------------------------------------------------
  b$mob <- grepl("mob", b$aru, ignore.case = TRUE)
  b$region <- al$sr[match(b$aru, al$aru)]
  b$region[b$mob] <- ifelse(grepl("[0-9]", b$aru[b$mob]), gsub("[^0-9]", "", b$aru[b$mob]), NA)
  mn <- list()
  if (is.data.frame(mobile.nights) && nrow(mobile.nights)) {
    ms <- pick(mobile.nights, "sunregion"); md <- pick(mobile.nights, "date")
    if (is.na(ms) || is.na(md)) stop(FN, ": mobile.nights needs $sunregion and $date.", call. = FALSE)
    for (i in seq_len(nrow(mobile.nights))) {
      r <- trimws(as.character(mobile.nights[[ms]][i]))
      mn[[r]] <- c(mn[[r]], format(rdate(splt(mobile.nights[[md]][i]))))
    }
  }
  nonum <- which(b$mob & is.na(b$region))
  for (i in nonum) { hit <- names(mn)[vapply(mn, function(d) format(b$date[i]) %in% d, logical(1))]; if (length(hit) == 1) b$region[i] <- hit }
  lost <- unique(b$aru[b$mob & is.na(b$region) & b$obs > 0])
  if (length(lost)) cat(sprintf("NOTE: %s - mobile detector(s) with no sun region (no number in the name and no matching mobile.nights): %s - left out.\n",
                                FN, paste(lost, collapse = ", ")))
  unk <- unique(b$aru[!b$mob & is.na(b$region)])
  if (length(unk)) cat(sprintf("NOTE: %s - ARU(s) in data but not in arulist - left out: %s\n", FN, paste(unk, collapse = ", ")))

  ## ---- fig.list rows ----------------------------------------------------------------
  if (is.null(fig.list)) {
    regs <- unique(al$sr[al$sr %in% b$region[!b$mob]])
    jobs <- lapply(regs, function(r) list(plot.set = r))
    cat(sprintf("NOTE: %s - no fig.list: one plot per sun region with data (%s).\n", FN, paste(regs, collapse = ", ")))
  } else {
    fl <- plotutil_figlist_na(fig.list)
    if (!"plot.type" %in% names(fl) || !"plot.set" %in% names(fl)) stop(FN, ": fig.list needs $plot.type and $plot.set.", call. = FALSE)
    keep <- trimws(fl$plot.type) %in% c(PLOT.TYPE, FN) & (if ("plot" %in% names(fl)) toupper(trimws(as.character(fl$plot))) %in% c("TRUE", "1") else TRUE)
    if (!any(keep)) cat(sprintf("NOTE: %s - no fig.list row with $plot = TRUE and $plot.type = \"%s\".\n", FN, PLOT.TYPE))
    jobs <- lapply(which(keep), function(i) as.list(fl[i, , drop = FALSE]))
  }

  ## ---- one plot per row: mean per ARU + mobile bars ----------------------------------
  out <- list(); nights.all <- list(); data.all <- list()
  for (job in jobs) {
    r <- trimws(as.character(job$plot.set)[1])
    ar <- al[al$sr == r, , drop = FALSE]
    if (!nrow(ar)) { cat(sprintf("NOTE: %s - sun region '%s' is not in arulist - skipped.\n", FN, r)); next }
    ds <- if (!blank(job$date.start)) rdate(job$date.start) else min(ar$d0, na.rm = TRUE)
    de <- if (!blank(job$date.end)) rdate(job$date.end) else max(ar$d1, na.rm = TRUE)
    if (is.na(ds) || is.na(de) || de < ds) { cat(sprintf("NOTE: %s - sun region %s: no usable date window - skipped.\n", FN, r)); next }
    nights <- seq(ds, de, by = "day")
    nd <- setting(job, "n.detectors")
    n.det <- if (!blank(nd) && tolower(nd) == "arulist") length(unique(ar$lab)) else { v <- num(nd); if (is.na(v)) 3 else v }
    den <- rep(n.det, length(nights))
    bs <- if (!blank(job$break.start)) rdate(splt(job$break.start)) else as.Date(character(0))
    be <- if (!blank(job$break.end)) rdate(splt(job$break.end)) else as.Date(character(0))
    if (length(bs) && length(bs) == length(be)) for (k in seq_along(bs)) den[nights >= bs[k] & nights <= be[k]] <- n.det - 1
    sta <- b[!b$mob & b$aru %in% ar$aru & b$date %in% nights, , drop = FALSE]
    if (!nrow(sta)) { cat(sprintf("NOTE: %s - sun region %s: no data in the window - skipped.\n", FN, r)); next }
    tot <- stats::aggregate(obs ~ date + spp, sta, sum)
    grid <- expand.grid(date = nights, spp = unique(sta$spp), stringsAsFactors = FALSE)
    grid$obs <- tot$obs[match(paste(grid$date, grid$spp), paste(tot$date, tot$spp))]; grid$obs[is.na(grid$obs)] <- 0
    grid$det <- den[match(grid$date, nights)]
    mob <- b[b$mob & b$region %in% r & b$date %in% nights & b$obs > 0, , drop = FALSE]
    if (nrow(mob)) mob <- stats::aggregate(obs ~ date + spp, mob, sum)
    d2 <- rbind(data.frame(spp.id = grid$spp, date = format(grid$date), observations.count = grid$obs / grid$det, group = r,
                           detectors = grid$det, stringsAsFactors = FALSE),
                if (nrow(mob)) data.frame(spp.id = mob$spp, date = format(mob$date), observations.count = mob$obs,
                                          group = paste0(r, "-MOB"), detectors = 1, stringsAsFactors = FALSE))
    data.all[[r]] <- cbind(sunregion = r, d2)

    ## weather per night
    ws <- wd[vapply(strsplit(wd$site, ";"), function(s) r %in% trimws(s), logical(1)), , drop = FALSE]
    if (!nrow(ws)) cat(sprintf("NOTE: %s - sun region %s: no weather rows whose $site lists it.\n", FN, r))
    tw <- vapply(splt(setting(job, "temp.window")), clock, numeric(1)); if (length(tw) != 2 || anyNA(tw)) tw <- c(20, 24)
    rw <- lapply(splt(setting(job, "rain.windows")), function(x) vapply(strsplit(x, "-", fixed = TRUE)[[1]], clock, numeric(1)))
    rmin <- num(setting(job, "rain.min.mm")); if (is.na(rmin)) rmin <- 1
    rlab <- vapply(splt(setting(job, "rain.windows")), function(x) x, "");
    nt <- do.call(rbind, lapply(nights, function(n) {
      tv <- ws$temp[ws$date == n & ws$hour >= tw[1] & ws$hour < tw[2]]
      tots <- vapply(rw, function(w) {
        dd <- if (w[1] < 12) n + 1 else n                              # a window starting before noon: next morning
        sum(ws$rain[ws$date == dd & ws$hour >= w[1] & ws$hour < w[2]], na.rm = TRUE)
      }, numeric(1))
      wet <- which(tots > rmin)
      how <- tolower(setting(job, "rain.multiple"))
      pickw <- if (!length(wet)) NA else if (length(wet) == 1) wet else switch(how, first = wet[1], last = wet[length(wet)], wet[2])
      data.frame(sunregion = r, date = n, temperature.max = if (any(!is.na(tv))) max(tv, na.rm = TRUE) else NA_real_,
                 temperature.min = if (any(!is.na(tv))) min(tv, na.rm = TRUE) else NA_real_,
                 t(stats::setNames(round(tots, 2), paste0("rain.mm_", gsub(":", "", gsub("-", "to", rlab))))),
                 rain = if (is.na(pickw)) "none" else rlab[pickw], rain.window = pickw, check.names = FALSE, stringsAsFactors = FALSE)
    }))
    nights.all[[r]] <- nt
    out[[r]] <- list(job = job, d2 = d2, nights = nt, ds = ds, de = de, mob = nrow(mob) > 0)
  }
  if (!length(out)) return(invisible(list(plots = list(), data = data.frame(), nights = data.frame(), files = character(0))))

  ## ---- one temperature scale for every plot of this call ---------------------------------
  t.all <- unlist(lapply(out, function(o) c(o$nights$temperature.max, o$nights$temperature.min)))
  tr0 <- setting(NULL, "temp.range")
  t.sc <- if (!blank(tr0)) range(num(splt(tr0))) else if (any(!is.na(t.all)))
    c(floor(min(t.all, na.rm = TRUE) / 5) * 5, ceiling(max(t.all, na.rm = TRUE) / 5) * 5) else c(0, 30)
  if (diff(t.sc) <= 0) t.sc <- t.sc + c(-5, 5)
  cat(sprintf("NOTE: %s - temperature axis %s to %s C (all plots).\n", FN, t.sc[1], t.sc[2]))

  ## ---- draw ------------------------------------------------------------------------------
  pass <- setdiff(names(dots), own)                               # other settings go to batz.plot.bar_activity.site
  plots <- list(); files <- character(0)
  for (r in names(out)) {
    o <- out[[r]]; job <- o$job
    g1 <- function(k) setting(job, k)
    fl1 <- data.frame(plot = TRUE, plot.type = "bar_activity.site",
                      plot.name = if (!blank(job$plot.name)) as.character(job$plot.name) else paste(r, "mean calls per ARU and weather"),
                      plot.set = paste(c(r, if (o$mob) paste0(r, "-MOB")), collapse = ";"), plot.group = "group",
                      date.start = format(o$ds, "%m/%d/%Y"), date.end = format(o$de, "%m/%d/%Y"),
                      date.interval = if (!blank(job$date.interval)) as.character(job$date.interval) else
                        paste(format(seq(o$ds, o$de, by = "7 days"), "%m/%d/%Y"), collapse = ";"),
                      pool = FALSE, facet.header = "sppid", facet.plot = TRUE, facet.set = g1("facet.set"),
                      facet.label_format = "common_name", facet.label = g1("facet.label"), all.detections = TRUE, myso = FALSE,
                      `40khzmyo` = g1("40khzmyo"), date.format = g1("date.format"), xaxe.interval = "7",
                      y.scale = g1("y.scale"), yaxe.trans = g1("yaxe.trans"), loglabels = FALSE, y.custom = g1("y.custom"),
                      ymax = g1("ymax"), legend = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    args <- c(list(data = o$d2, fig.list = fl1, project.name = "", dir.save = NULL, save.auto = TRUE,
                   legend.groupval.colors = paste(g1("bar.mean.fill"), g1("bar.mobile.fill"), sep = ";"),
                   legend.40khzmyo = g1("legend.40khzmyo"), xaxe.title = g1("xaxe.title"), yaxe.title = g1("yaxe.title")),
              dots[pass])
    res <- utils::capture.output(rb <- do.call(batz.plot.bar_activity.site, args))
    if (!length(rb$ggplots)) { cat(sprintf("NOTE: %s - sun region %s: no bar plot made.\n", FN, r)); next }
    p <- rb$plots[[1]]; g <- rb$ggplots[[1]]
    top <- p$y.upper.plot
    y.t <- function(v) (pmin(pmax(v, t.sc[1]), t.sc[2]) - t.sc[1]) / diff(t.sc) * top
    nt <- o$nights
    lab.max <- "Max temp"; lab.min <- "Min temp"
    tl <- rbind(data.frame(date = nt$date, y = y.t(nt$temperature.max), what = lab.max),
                data.frame(date = nt$date, y = y.t(nt$temperature.min), what = lab.min))
    rc <- splt(g1("rain.colors")); nonec <- g1("rain.none.color"); if (blank(nonec)) nonec <- "white"
    fillc <- ifelse(is.na(nt$rain.window), nonec, rc[pmin(nt$rain.window, length(rc))])
    rs <- data.frame(xmin = nt$date - 0.5, xmax = nt$date + 0.5, ymin = top * 1.04, ymax = top * 1.12, fill = fillc)
    for (fc in unique(rs$fill))
      g <- g + ggplot2::geom_rect(data = rs[rs$fill == fc, , drop = FALSE], ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                  fill = fc, colour = "grey60", linewidth = 0.1, inherit.aes = FALSE)
    tcol <- g1("temp.line.color"); if (blank(tcol)) tcol <- "grey20"
    tw.lab <- gsub(";", "-", g1("temp.window"))
    ttl <- g1("yaxe.title.right"); if (blank(ttl)) ttl <- sprintf("Temperature %s (\u00b0C)", tw.lab)
    brk <- pretty(t.sc); brk <- brk[brk >= t.sc[1] & brk <= t.sc[2]]
    g <- g +
      ggplot2::geom_line(data = tl, ggplot2::aes(x = date, y = y, linetype = what), colour = tcol, linewidth = 0.5, inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::geom_point(data = tl, ggplot2::aes(x = date, y = y), colour = tcol, size = 0.6, inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::scale_linetype_manual(name = NULL, values = stats::setNames(c(g1("temp.max.linetype"), g1("temp.min.linetype")), c(lab.max, lab.min)),
                                     guide = ggplot2::guide_legend(order = 3))
    g <- suppressMessages(g + ggplot2::scale_y_continuous(limits = c(0, top * 1.12), breaks = p$break.pos, labels = p$break.labels,
                                                          name = paste0("\n", g1("yaxe.title")),
                                                          sec.axis = ggplot2::sec_axis(~ t.sc[1] + . / top * diff(t.sc), name = ttl, breaks = brk)))
    if (!isFALSE(as.logical(g1("rain.caption")))) {
      rl <- splt(g1("rain.windows"))
      g <- g + ggplot2::labs(caption = sprintf("Top strip, rain > %s mm: %s = none; %s (rain in several windows: the %s)",
                                               g1("rain.min.mm"), nonec,
                                               paste(sprintf("%s = %s", rc[seq_along(rl)], rl), collapse = "; "), tolower(g1("rain.multiple")))) +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, hjust = 0))
    }
    g <- g + plotutil_text_theme(function(k) setting(job, k))
    plots[[r]] <- g
    clean <- function(x) gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(as.character(x))))
    parts <- c(if (!blank(project.name)) clean(project.name), PLOT.TYPE, clean(r),
               paste0(format(o$ds, "%Y%m%d"), "to", format(o$de, "%Y%m%d")), format(Sys.time(), "%Y%m%d%H%M%S"))
    if (!is.null(dir.save)) {
      if (sv$save && !dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
      f <- file.path(dir.save, paste0(paste(parts, collapse = "_"), ".png"))
      wdt <- num(g1("plot.width")); hgt <- num(g1("plot.height")); if (is.na(wdt)) wdt <- 9; if (is.na(hgt)) hgt <- 7
      f <- sv$out(g, f, function(.f) plotutil_ggsave(.f, g, width = wdt, height = hgt, units = "in", dpi = 300))
      if (sv$save) files <- c(files, f)
    }
  }
  dd <- do.call(rbind, data.all); rownames(dd) <- NULL
  nn <- do.call(rbind, nights.all); rownames(nn) <- NULL
  invisible(list(plots = plots, data = dd, nights = nn, files = files))
}

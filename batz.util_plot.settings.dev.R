# =============================================================================
# batz.util_plot.settings.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for the settings order shared by every Batz plot function:
#   1. settings typed into the function call (e.g. legend.position = "top")
#   2. the fig.list row (functions that use a fig.list)
#   3. plotopts / aes.default (override.value, then default.value)
# A setting given in the call does not need a row in plotopts.
# Uses the built-in plotopts (Batz_reference_db), the fig.list_*.csv files in
# the repo root and made-up data. Set dir.repo, then run the whole script.
# =============================================================================

library(Batz)

dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
dir.out  <- file.path(tempdir(), "plot_settings_test"); dir.create(dir.out, showWarnings = FALSE)
rd <- function(f) read.csv(file.path(dir.repo, f), check.names = FALSE, stringsAsFactors = FALSE)
th <- function(g, el) g$theme[[el]]
opts <- function(fn) Batz:::batz.db(paste0("plotopts.", fn))   # built-in plotopts (Batz_reference_db)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## ---- batz.plot_bar.bat.group / batz.plot_heat.site.species -----------------------
set.seed(1); n <- 400
raw <- data.frame(SITE = sample(c("OLMN3", "OLMN7", "KA01", "Basecamp KA 6"), n, TRUE),
                  `MANUAL ID` = sample(c("Otomops harrisoni", "Coleura afra", "NoID"), n, TRUE),
                  `DATE-12` = format(as.Date("2025-07-01") + sample(0:3, n, TRUE), "%m/%d/%Y"),
                  TIME = sprintf("%02d:%02d:00", sample(c(19:23, 0:5), n, TRUE), sample(0:59, n, TRUE)),
                  LATITUDE = 2.5, YEAR = 2025, check.names = FALSE)
v <- batz.datawrangler_format.vetted(raw, aru.recode = data.frame(o = "Basecamp KA 6", n = "KABC_06"),
                                     project = "South", latitude.sign = "south", write.output = FALSE)
pf <- batz.generate_plotframe.bat.group(v, groupby.date = "date-12", write.output = FALSE)
fl <- rd("fig.list_bar.bat.group.csv"); fl$plot.set <- "South"
po <- opts("batz.plot_bar.bat.group")
fl$legend.position <- "top"                                                   # fig.list sets it
b0 <- batz.plot_bar.bat.group(pf, fl, po, dir.save = dir.out)
b1 <- batz.plot_bar.bat.group(pf, fl, po, dir.save = dir.out, legend.position = "left", yaxe.title = "INPUT")
ok(th(b0$ggplots[[1]], "legend.position") == "top", "bar: fig.list before plotopts")
ok(th(b1$ggplots[[1]], "legend.position") == "left" && b1$ggplots[[1]]$labels$y == "INPUT", "bar: function call before fig.list")
fl$legend.position <- NULL
b2 <- batz.plot_bar.bat.group(pf, fl, po[po$parameter != "legend.position", ], dir.save = dir.out, legend.position = "right")
ok(th(b2$ggplots[[1]], "legend.position") == "right", "bar: a setting in the call replaces a missing plotopts row")
b3 <- batz.plot_bar.bat.group(pf, fl, po, dir.save = dir.out, legend.positon = "top")   # misspelt: NOTE, not used

fl2 <- rd("fig.list_heat.site.species.csv"); fl2$plot.set <- "OLMN;KA"
po2 <- opts("batz.plot_heat.site.species")
h1 <- batz.plot_heat.site.species(v, fl2, po2, groupby.date = "date-12", dir.save = dir.out,
                                  xaxe.title = "INPUT X", legend.position = "top")
ok(h1$ggplots[[1]]$labels$x == "INPUT X", "heat.site.species: function call before fig.list")

## ---- batz.plotactivity_observations / batz.plotdetections_first.last ------------
dates <- seq(as.Date("2026-07-01"), as.Date("2026-07-05"), by = "day")
d <- do.call(rbind, lapply(c("EPFU", "MYLU"), function(s) data.frame(spp.id = s, date = format(dates), group = "A1",
  observations.count = 3, mins2.noon.min = 500, mins2.noon.max = 900, vetting.type = "bat.detection")))
st <- data.frame(aru.name = "A1", date = format(dates), date.monitoringnight = format(dates), sunregion = "x",
  time.zone = "America/New_York", sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "", sunset = "20:20",
  sunset.unix = 0, sunrise = "05:10", sunrise.unix = 0, sunrise.monitoringnight = "05:11", sunrise.monitoringnight.unix = 0)
flx <- function(type) data.frame(plot.type = type, plot.name = "t", facet.header = "sppid", facet.plot = TRUE,
  facet.set = "EPFU;MYLU", myso = FALSE, all.detections = FALSE, `40khzmyo` = FALSE, facet.label = "code4", plot.set = "A1",
  pool = FALSE, date.format = "%b-%d", date.start = "2026-07-01", date.end = "2026-07-05", xaxe.interval = "1 day",
  xaxe.title = "Date", check.names = FALSE)
o1 <- batz.plotactivity_observations(d, flx("call.observations"), st, opts("batz.plotactivity_observations"), dir.save = dir.out,
                                     legend.position = "left")
ok(th(o1$ggplots[[1]], "legend.position") == "left", "observations: function call used")
f1 <- batz.plotdetections_first.last(d, flx("bat.detection"), st, opts("batz.plotdetections_first.last"), dir.save = dir.out,
                                     legend.position = "left", plot.width = 11, plot.height = 8.5)
ok(th(f1$ggplots[[1]], "legend.position") == "left", "first.last: function call used")

## ---- batz.plotactivity_heatmap2 (fig.list optional) ----------------------------
hm <- data.frame(date.monitoringnight = rep(format(dates), each = 4), pool.start = rep(c("21:00", "22:00", "01:00", "03:00"), 5),
                 observations.count = rpois(20, 5), group = "A1", spp.id = "EPFU")
flh <- rd("heat2.figlist.csv")[1, , drop = FALSE]
flh$plot.set <- "A1"; flh$facet.set <- "EPFU"; flh$date.start <- "2026-07-01"; flh$date.end <- "2026-07-05"
flh$facet.plot <- FALSE; flh$legend.position <- "left"
gg <- function(r) { x <- r$ggplots; if (is.null(x) || !length(x)) x <- r$plots; while (is.list(x) && !inherits(x, "gg")) x <- x[[1]]; x }
k2 <- batz.plotactivity_heatmap2(hm, fig.list = flh, dir.save = dir.out)
k3 <- batz.plotactivity_heatmap2(hm, fig.list = flh, dir.save = dir.out, legend.position = "top")
ok(th(gg(k2), "legend.position") == "left" && th(gg(k3), "legend.position") == "top", "heatmap2: function call before fig.list")

## ---- batz.plotactivity_daily.count / batz.plotactivity_heatmap (no fig.list) ----
set.seed(2); n <- 300
dd <- data.frame(kpauto = sample(c("EPFU", "MYLU", ""), n, TRUE),
                 filename = sprintf("A1_202607%02d_%02d%02d00_000.wav", sample(1:6, n, TRUE), sample(c(20:23, 0:4), n, TRUE), sample(0:59, n, TRUE)),
                 monnight = format(as.Date("2026-07-01") + sample(0:6, n, TRUE)),
                 time = sprintf("%02d:%02d:00", sample(c(20:23, 0:4), n, TRUE), sample(0:59, n, TRUE)))
c1 <- batz.plotactivity_daily.count(dd, aes.default = opts("batz.plotactivity_daily.count"), dir.save = dir.out, bar.fill = "red")
ok("red" %in% ggplot2::ggplot_build(c1$ggplot)$data[[1]]$fill, "daily.count: function call before plotopts")
h2 <- batz.plotactivity_heatmap(c1$data, aes.default = opts("batz.plotactivity_heatmap"), dir.save = dir.out, date.col = "monnight",
                                plot.title.size = 30)
ok(h2$ggplot$theme$plot.title$size == 30, "heatmap: function call before plotopts")

## ---- batz.plotcover_bullseye / batz.plotsm4_heatmap ---------------------------
## same pattern on real data, e.g.:
# batz.plotcover_bullseye(cover, mic, opts("batz.plotcover_bullseye"), canopy.title = "Canopy")
# batz.plotsm4_heatmap(sm4, fig.list, suntimes, opts("batz.plotsm4_heatmap"), xaxe.title = "Night", legend.position = "top")

## ---- no aes.default: the built-in plotopts (inst/plotopts/*.csv) are used ------
## each call prints "NOTE: <function> - no aes.default given: the built-in settings (...) are used."
b4 <- batz.plot_bar.bat.group(pf, fl, dir.save = dir.out)
ok(length(b4$ggplots) == 1, "bar: runs with no aes.default")
o4 <- batz.plotactivity_observations(d, flx("call.observations"), st, dir.save = dir.out)
ok(length(o4$ggplots) >= 1, "observations: runs with no aes.default")
k4 <- batz.plotactivity_heatmap2(hm, dir.save = dir.out)
c4 <- batz.plotactivity_daily.count(dd, dir.save = dir.out)
h4 <- batz.plotactivity_heatmap(c4$data, dir.save = dir.out, date.col = "monnight")
ok(!is.null(h4$ggplot), "daily.count + heatmap: run with no aes.default")
## a given aes.default is used and the built-in one ignored
po.h <- opts("batz.plotactivity_heatmap"); po.h$default.value[po.h$parameter == "plot.title.size"] <- "31"
h5 <- batz.plotactivity_heatmap(c4$data, aes.default = po.h, dir.save = dir.out, date.col = "monnight")
ok(h5$ggplot$theme$plot.title$size == 31, "given aes.default replaces the built-in settings")

## a trailing comma in the call (empty argument) is skipped, not an error
h6 <- batz.plotactivity_heatmap(c4$data, dir.save = dir.out, date.col = "monnight", )
ok(!is.null(h6$ggplot), "trailing comma in the call is ignored")

## aes.plotopts (2026-11-10): aes.default renamed; the old name is still read (WARNING)
h7 <- batz.plotactivity_heatmap(c4$data, aes.plotopts = po.h, dir.save = dir.out, date.col = "monnight")
ok(h7$ggplot$theme$plot.title$size == 31, "aes.plotopts is used")
h8 <- batz.plotactivity_heatmap(c4$data, aes.default = po.h, dir.save = dir.out, date.col = "monnight")   # WARNING: ... now called aes.plotopts
ok(h8$ggplot$theme$plot.title$size == 31, "old argument name aes.default still read")
## aes.style = a project column (as written by batz.generate_plotopts)
po.p <- po.h; names(po.p)[names(po.p) == "override.value"] <- "acme"; po.p$acme[po.p$parameter == "plot.title.size"] <- "27"
h9 <- batz.plotactivity_heatmap(c4$data, aes.plotopts = po.p, aes.style = "acme", dir.save = dir.out, date.col = "monnight")
ok(h9$ggplot$theme$plot.title$size == 27, "aes.style names the project column")

cat("\nALL TESTS PASSED - PNGs in", dir.out, "\n")

## ---------------------------------------------------------------------------
## 2026-11-13: a top/bottom legend wider than the saved image is wrapped over
## 2+ lines and the saved height grows by the extra legend height (all plot
## functions save through Batz:::plotutil_ggsave())
## ---------------------------------------------------------------------------
lg <- data.frame(x = 1:8, y = 1:8, site = paste("A long detector name", 1:8))
g.lg <- ggplot2::ggplot(lg, ggplot2::aes(x, y, fill = site)) + ggplot2::geom_col() +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
f.lg <- file.path(tempdir(), "legend_wrap.png")
s.lg <- Batz:::plotutil_ggsave(f.lg, g.lg, width = 6, height = 4, units = "in", dpi = 100)
cat("legend lines:", s.lg$legend.rows, " saved height:", round(s.lg$height, 2), "in\n")
ok(s.lg$legend.rows >= 2 && s.lg$height > 4, "wide legend wrapped and height increased")
s.ok <- Batz:::plotutil_ggsave(f.lg, g.lg, width = 30, height = 4, units = "in", dpi = 50)
ok(is.na(s.ok$legend.rows) && s.ok$height == 4, "legend that fits is left alone")

## 2026-11-13: a facet with no bars in a dodged layer no longer prints
## "no non-missing arguments to max; returning -Inf" when the PNG is saved
em <- data.frame(x = c("a", "b"), y = 1:2, s = c("A", "B"), f = factor(c("p1", "p1"), levels = c("p1", "p2")))
g.em <- ggplot2::ggplot(em, ggplot2::aes(x, y, fill = s)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single")) +
  ggplot2::facet_wrap(~f, drop = FALSE)
w.em <- character(0)
withCallingHandlers(Batz:::plotutil_ggsave(file.path(tempdir(), "empty_facet.png"), g.em, 6, 4, dpi = 50),
                    warning = function(w) { w.em <<- c(w.em, conditionMessage(w)); invokeRestart("muffleWarning") })
ok(length(w.em) == 0, "empty facet saves without the max/-Inf warning")

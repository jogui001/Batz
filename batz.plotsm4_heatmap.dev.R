# =============================================================================
# batz.plotsm4_heatmap.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plotsm4_heatmap() (Batz package).
# Made-up SM4 system checks -> batz.generate_plotframe.sm4() -> heatmap,
# with and without suntimes (suntimes is optional: no Dawn / Dusk lines).
# Run the whole script; the PNGs are saved in dir.out.
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "sm4_heatmap_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## checks every 60 s, 20:02:59 - 05:44:59, 4 nights; the unit stops 22:00 - 00:00 on night 2
t <- as.POSIXct("2026-06-26 20:02:59", tz = "UTC") + c(outer(60 * 0:581, 86400 * 0:3, "+"))
sm4 <- data.frame(aru.name = "AYERS", date = format(t, "%Y-%m-%d"), time = format(t, "%H:%M:%S"),
                  mic0.type = "U2", power.v = 6.4, temperature.c = 21.5, files = 1, scrubbed = 0)
gap <- t >= as.POSIXct("2026-06-27 22:00:00", tz = "UTC") & t < as.POSIXct("2026-06-28 00:00:00", tz = "UTC")
sm4 <- sm4[!gap, ]
pf <- batz.generate_plotframe.sm4(sm4, dir.save = NULL)            # NOTE: most likely schedule ... observed 20:03-05:40
ok(sum(pf$mins.oper == 0) == 8, "2-hour gap -> 8 intervals of 0")

fl <- batz.generate_figlist(functions = "batz.plotsm4_heatmap", dir.save = NULL)$fig.list
fl$plot <- TRUE                                                     # the model row comes with $plot = FALSE
fl$plot.set <- "AYERS"; fl$plot.name <- "Test AYERS"
fl$date.start <- "6/26/2026"; fl$date.end <- "6/29/2026"; fl$date.format <- "%b-%d"; fl$xaxe.interval <- "4"
fl$xaxe.title <- "Monitoring Night"; fl$yaxe.title <- "Hour of Monitoring"

## 1. no suntimes: Midnight line only
r1 <- batz.plotsm4_heatmap(data = pf, fig.list = fl, project.name = "nosun", dir.save = dir.out)
lines1 <- vapply(r1$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines1 == "GeomLine") == 1, "no suntimes: one reference line (Midnight)")

## 2. with suntimes: Dawn, Dusk and Midnight
st <- data.frame(aru.name = "AYERS", date = format(as.Date("2026-06-26") + 0:3), date.monitoringnight = format(as.Date("2026-06-26") + 0:3),
                 sunregion = "AYERS", time.zone = "America/New_York", sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "",
                 sunset = paste(as.Date("2026-06-26") + 0:3, "20:33:00"), sunset.unix = NA,
                 sunrise = paste(as.Date("2026-06-26") + 0:3, "05:08:00"), sunrise.unix = NA,
                 sunrise.monitoringnight = paste(as.Date("2026-06-27") + 0:3, "05:08:00"), sunrise.monitoringnight.unix = NA)
r2 <- batz.plotsm4_heatmap(data = pf, fig.list = fl, suntimes = st, project.name = "sun", dir.save = dir.out)
lines2 <- vapply(r2$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines2 == "GeomLine") == 3, "with suntimes: Dawn, Dusk and Midnight")

## 3. $plot (2026-11-09): only rows with TRUE or 1 are plotted
fl3 <- rbind(fl, fl, fl); fl3$plot <- c("FALSE", "1", ""); fl3$plot.name <- c("skip", "redo", "blank")
r3 <- batz.plotsm4_heatmap(data = pf, fig.list = fl3, project.name = "plot3", dir.save = dir.out)   # NOTE: 1 of 3 ...
ok(length(r3$ggplots) == 1, "$plot: only the row with 1 (\"redo\") is plotted")
fl4 <- fl; fl4$plot <- FALSE
r4 <- batz.plotsm4_heatmap(data = pf, fig.list = fl4, dir.save = dir.out)
ok(length(r4$ggplots) == 0, "$plot = FALSE everywhere: nothing plotted, no error")
fl5 <- fl[, names(fl) != "plot"]
r5 <- batz.plotsm4_heatmap(data = pf, fig.list = fl5, project.name = "noplotcol", dir.save = dir.out)   # NOTE: no $plot column
ok(length(r5$ggplots) == 1, "no $plot column: every row plotted")

## 4. plot.set with several ARUs separated by ";" (one panel each), "2 day" x labels, blank date.format (2026-11-09)
pf2 <- rbind(pf, transform(pf, aru.name = "SUGARHILL"))
fl6 <- fl; fl6$plot.set <- "AYERS ; sugar_hill.  "; fl6$plot.name <- "two ARUs"
fl6$xaxe.interval <- "2 day"; fl6$date.format <- ""
r6 <- batz.plotsm4_heatmap(data = pf2, fig.list = fl6, suntimes = st, project.name = "multi", dir.save = dir.out)   # NOTE: $date.format is blank
g6 <- r6$ggplots[[1]]
ok(inherits(g6$facet, "FacetWrap") && identical(levels(g6$data$facet), c("AYERS", "SUGARHILL")), "two ARUs: one panel each, in plot.set order")
ok(length(ggplot2::ggplot_build(g6)$layout$panel_params[[1]]$x$breaks[!is.na(ggplot2::ggplot_build(g6)$layout$panel_params[[1]]$x$breaks)]) == 2,
   "\"2 day\" from 6/26 to 6/29: labels on 6/26 and 6/28")

## 5. $facet.label: alternative panel names (2026-11-09)
fl7 <- fl6; fl7$facet.label <- "sugarhill=Sugar Hill"; fl7$date.format <- "%b-%d"
r7 <- batz.plotsm4_heatmap(data = pf2, fig.list = fl7, project.name = "labels", dir.save = dir.out)
ok(identical(levels(r7$ggplots[[1]]$data$facet), c("AYERS", "Sugar Hill")), "$facet.label: SUGARHILL shown as 'Sugar Hill'")
fl8 <- fl7; fl8$facet.label <- "Ayers Rd; Sugar Hill"
r8 <- batz.plotsm4_heatmap(data = pf2, fig.list = fl8, project.name = "labels", dir.save = dir.out)
ok(identical(levels(r8$ggplots[[1]]$data$facet), c("Ayers Rd", "Sugar Hill")), "$facet.label in plot.set order")

## ---- 6. Y axis: time.start / time.end / time.interval (2026-11-09) -----------
hm <- function(x) format(x, "%H:%M")
yb <- function(r) { b <- ggplot2::ggplot_build(r$ggplots[[1]])$layout$panel_params[[1]]$y$breaks; hm(as.POSIXct(b[!is.na(b)], origin = "1970-01-01", tz = r$plots[[1]]$tz)) }
## given: used as they are
fl9 <- fl; fl9$time.start <- "19:00"; fl9$time.end <- "07:00"; fl9$time.interval <- "120"
r9 <- batz.plotsm4_heatmap(data = pf, fig.list = fl9, suntimes = st, project.name = "ygiven", dir.save = dir.out)
ok(hm(r9$plots[[1]]$y.start) == "19:00" && hm(r9$plots[[1]]$y.end) == "07:00", "time.start / time.end given: 19:00 to 07:00")
print(yb(r9))
ok(identical(yb(r9), c("20:00", "22:00", "00:00", "02:00", "04:00", "06:00")), "time.interval 120: labels every second hour from midnight")
## blank: from $aru.schedule (USFWS.compliance = sunset-30 to sunrise+30), rounded to time.interval
pfu <- pf; pfu$aru.schedule <- "USFWS.compliance"
stu <- st; stu$sunset <- paste(as.Date("2026-06-26") + 0:3, "21:33:00")                       # earliest sunset 21:33 -> 21:03
stu$sunrise.monitoringnight <- paste(as.Date("2026-06-27") + 0:3, c("06:10:00", "06:05:00", "06:00:00", "05:55:00"))   # latest 06:10 -> 06:40
y.of <- function(iv) { f <- fl; f$time.start <- ""; f$time.end <- ""; f$time.interval <- iv
  r <- batz.plotsm4_heatmap(data = pfu, fig.list = f, suntimes = stu, project.name = paste0("yiv", iv), dir.save = dir.out)
  c(hm(r$plots[[1]]$y.start), hm(r$plots[[1]]$y.end)) }
ok(identical(y.of("60"), c("21:00", "07:00")), "time.interval 60: 21:03 -> 21:00, 06:40 -> 07:00")
ok(identical(y.of("30"), c("21:00", "07:00")), "time.interval 30: 21:00 to 07:00")
ok(identical(y.of("120"), c("20:00", "08:00")), "time.interval 120: 20:00 to 08:00")
ok(identical(y.of("15 min"), c("21:00", "06:45")), "time.interval \"15 min\": 21:00 to 06:45")
ok(identical(y.of(""), c("21:00", "07:00")), "time.interval blank: 60")
## one given, one blank
fl10 <- fl; fl10$time.start <- "18:00"; fl10$time.end <- ""
r10 <- batz.plotsm4_heatmap(data = pfu, fig.list = fl10, suntimes = stu, project.name = "yhalf", dir.save = dir.out)
ok(hm(r10$plots[[1]]$y.start) == "18:00" && hm(r10$plots[[1]]$y.end) == "07:00", "time.start given, time.end from the schedule")
## typed into the call (call > fig.list)
r11 <- batz.plotsm4_heatmap(data = pfu, fig.list = fl10, suntimes = stu, project.name = "ycall", dir.save = dir.out, time.start = "22:00")
ok(hm(r11$plots[[1]]$y.start) == "22:00", "time.start typed into the call wins over the fig.list")
## inferred schedule from batz.generate_plotframe.sm4 ("observed HH:MM-HH:MM"), no suntimes needed
fl12 <- fl; fl12$time.start <- ""; fl12$time.end <- ""
r12 <- batz.plotsm4_heatmap(data = pf, fig.list = fl12, project.name = "yobs", dir.save = dir.out)
print(unique(pf$aru.schedule))
ok(hm(r12$plots[[1]]$y.start) == "20:00" && hm(r12$plots[[1]]$y.end) == "06:00", "observed 20:03-05:44 -> 20:00 to 06:00")
## sun-based schedule without suntimes, or no $aru.schedule column: range of the plotted intervals (NOTE)
r13 <- batz.plotsm4_heatmap(data = pfu, fig.list = fl12, project.name = "ynosun", dir.save = dir.out)
r14 <- batz.plotsm4_heatmap(data = pf[, names(pf) != "aru.schedule"], fig.list = fl12, project.name = "ynocol", dir.save = dir.out)
ok(hm(r14$plots[[1]]$y.start) == "20:00" && hm(r14$plots[[1]]$y.end) == "06:00", "no $aru.schedule: range of the plotted intervals")

## ---- 7. $plot.dimensions: saved size in inches (2026-11-09) -------------------
px <- function(f) dim(png::readPNG(f))[2:1]                                            # width, height in pixels
last.png <- function(tag) { f <- list.files(dir.out, paste0("^", tag, "_.*\\.png$"), full.names = TRUE); f[which.max(file.mtime(f))] }
fl15 <- fl; fl15$plot.dimensions <- "1 ppt"
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dim1ppt", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dim1ppt")), c(1200L, 600L)), "\"1 ppt\" = 12 x 6 in")
fl15$plot.dimensions <- " 1/3 PAGE "
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dimthird", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimthird")), c(600L, 350L)), "\"1/3 page\" (case and spaces ignored) = 6 x 3.5 in")
fl15$plot.dimensions <- "7 x 4"
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dimcustom", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimcustom")), c(700L, 400L)), "\"7 x 4\" = 7 x 4 in")
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dimcall", dir.save = dir.out, ggsave.dpi = 100, plot.dimensions = "2/3 ppt")
ok(identical(px(last.png("dimcall")), c(800L, 600L)), "typed into the call wins: \"2/3 ppt\" = 8 x 6 in")
fl15$plot.dimensions <- ""
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dimblank", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimblank")), c(1050L, 750L)), "blank: plotopts plot.width + pad x plot.height + pad (10.5 x 7.5)")
fl15$plot.dimensions <- "half page"
batz.plotsm4_heatmap(data = pf, fig.list = fl15, project.name = "dimbad", dir.save = dir.out, ggsave.dpi = 100)   # NOTE: not a size
ok(identical(px(last.png("dimbad")), c(1050L, 750L)), "unknown entry: NOTE and the plotopts size")

## ---- 8. default text: labels 10 pt, titles 10 pt bold (2026-11-12) -----------
th <- r1$ggplots[[1]]$theme
ok(th$axis.text$size == 10 && th$legend.text$size == 10, "axis and legend labels 10 pt")
ok(all(sapply(c("axis.title", "legend.title", "plot.title"), function(e) th[[e]]$size == 10 && th[[e]]$face == "bold")),
   "axis, legend and plot titles 10 pt bold")
th6 <- r6$ggplots[[1]]$theme
ok(th6$strip.text$size == 10, "facet strip text 10 pt")

## ---- 9. plot.zero (2026-11-13): intervals with 0 minutes not drawn unless plot.zero = TRUE ----
n0 <- sum(pf$mins.oper == 0)
r9a <- batz.plotsm4_heatmap(data = pf, fig.list = fl, project.name = "zero.no", dir.save = dir.out)
r9b <- batz.plotsm4_heatmap(data = pf, fig.list = fl, project.name = "zero.yes", dir.save = dir.out, plot.zero = TRUE)
ok(nrow(r9a$ggplots[[1]]$data) == nrow(r9b$ggplots[[1]]$data) - n0 && n0 > 0, "plot.zero = FALSE (default): 0-minute tiles left out")

cat("\nALL TESTS PASSED - PNGs in", dir.out, "\n")

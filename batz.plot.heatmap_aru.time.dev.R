# =============================================================================
# batz.plot.heatmap_aru.time.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot.heatmap_aru.time() (Batz package).
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

fl <- batz.generate_figlist(functions = "batz.plot.heatmap_aru.time", dir.save = NULL)$fig.list
fl$plot <- TRUE                                                     # the model row comes with $plot = FALSE
fl$plot.set <- "AYERS"; fl$plot.name <- "Test AYERS"
fl$date.start <- "6/26/2026"; fl$date.end <- "6/29/2026"; fl$date.format <- "%b-%d"; fl$xaxe.interval <- "4"
fl$xaxe.title <- "Monitoring Night"; fl$yaxe.title <- "Hour of Monitoring"

## 1. no suntimes: Midnight line only
r1 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, project.name = "nosun", dir.save = dir.out)
lines1 <- vapply(r1$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines1 == "GeomLine") == 1, "no suntimes: one reference line (Midnight)")

## 2. with suntimes: Dawn, Dusk and Midnight
st <- data.frame(aru.name = "AYERS", date = format(as.Date("2026-06-26") + 0:3), date.monitoringnight = format(as.Date("2026-06-26") + 0:3),
                 sunregion = "AYERS", time.zone = "America/New_York", sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "",
                 sunset = paste(as.Date("2026-06-26") + 0:3, "20:33:00"), sunset.unix = NA,
                 sunrise = paste(as.Date("2026-06-26") + 0:3, "05:08:00"), sunrise.unix = NA,
                 sunrise.monitoringnight = paste(as.Date("2026-06-27") + 0:3, "05:08:00"), sunrise.monitoringnight.unix = NA)
r2 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, suntimes = st, project.name = "sun", dir.save = dir.out)
lines2 <- vapply(r2$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines2 == "GeomLine") == 3, "with suntimes: Dawn, Dusk and Midnight")

## 3. $plot (2026-11-09): only rows with TRUE or 1 are plotted
fl3 <- rbind(fl, fl, fl); fl3$plot <- c("FALSE", "1", ""); fl3$plot.name <- c("skip", "redo", "blank")
r3 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl3, project.name = "plot3", dir.save = dir.out)   # NOTE: 1 of 3 ...
ok(length(r3$ggplots) == 1, "$plot: only the row with 1 (\"redo\") is plotted")
fl4 <- fl; fl4$plot <- FALSE
r4 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl4, dir.save = dir.out)
ok(length(r4$ggplots) == 0, "$plot = FALSE everywhere: nothing plotted, no error")
fl5 <- fl[, names(fl) != "plot"]
r5 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl5, project.name = "noplotcol", dir.save = dir.out)   # NOTE: no $plot column
ok(length(r5$ggplots) == 1, "no $plot column: every row plotted")

## 4. plot.set with several ARUs separated by ";" (one panel each), "2 day" x labels, blank date.format (2026-11-09)
pf2 <- rbind(pf, transform(pf, aru.name = "SUGARHILL"))
fl6 <- fl; fl6$plot.set <- "AYERS ; sugar_hill.  "; fl6$plot.name <- "two ARUs"
fl6$xaxe.interval <- "2 day"; fl6$date.format <- ""
r6 <- batz.plot.heatmap_aru.time(data = pf2, fig.list = fl6, suntimes = st, project.name = "multi", dir.save = dir.out)   # NOTE: $date.format is blank
g6 <- r6$ggplots[[1]]
ok(inherits(g6$facet, "FacetWrap") && identical(levels(g6$data$facet), c("AYERS", "SUGARHILL")), "two ARUs: one panel each, in plot.set order")
ok(length(ggplot2::ggplot_build(g6)$layout$panel_params[[1]]$x$breaks[!is.na(ggplot2::ggplot_build(g6)$layout$panel_params[[1]]$x$breaks)]) == 2,
   "\"2 day\" from 6/26 to 6/29: labels on 6/26 and 6/28")

## 5. $facet.label: alternative panel names (2026-11-09)
fl7 <- fl6; fl7$facet.label <- "sugarhill=Sugar Hill"; fl7$date.format <- "%b-%d"
r7 <- batz.plot.heatmap_aru.time(data = pf2, fig.list = fl7, project.name = "labels", dir.save = dir.out)
ok(identical(levels(r7$ggplots[[1]]$data$facet), c("AYERS", "Sugar Hill")), "$facet.label: SUGARHILL shown as 'Sugar Hill'")
fl8 <- fl7; fl8$facet.label <- "Ayers Rd; Sugar Hill"
r8 <- batz.plot.heatmap_aru.time(data = pf2, fig.list = fl8, project.name = "labels", dir.save = dir.out)
ok(identical(levels(r8$ggplots[[1]]$data$facet), c("Ayers Rd", "Sugar Hill")), "$facet.label in plot.set order")

## ---- 6. Y axis: time.start / time.end / time.interval (2026-11-09) -----------
hm <- function(x) format(x, "%H:%M")
yb <- function(r) { b <- ggplot2::ggplot_build(r$ggplots[[1]])$layout$panel_params[[1]]$y$breaks; hm(as.POSIXct(b[!is.na(b)], origin = "1970-01-01", tz = r$plots[[1]]$tz)) }
## given: used as they are
fl9 <- fl; fl9$time.start <- "19:00"; fl9$time.end <- "07:00"; fl9$time.interval <- "120"
r9 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl9, suntimes = st, project.name = "ygiven", dir.save = dir.out)
ok(hm(r9$plots[[1]]$y.start) == "19:00" && hm(r9$plots[[1]]$y.end) == "07:00", "time.start / time.end given: 19:00 to 07:00")
print(yb(r9))
ok(identical(yb(r9), c("20:00", "22:00", "00:00", "02:00", "04:00", "06:00")), "time.interval 120: labels every second hour from midnight")
## blank: from $aru.schedule (USFWS.compliance = sunset-30 to sunrise+30), rounded to time.interval
pfu <- pf; pfu$aru.schedule <- "USFWS.compliance"
stu <- st; stu$sunset <- paste(as.Date("2026-06-26") + 0:3, "21:33:00")                       # earliest sunset 21:33 -> 21:03
stu$sunrise.monitoringnight <- paste(as.Date("2026-06-27") + 0:3, c("06:10:00", "06:05:00", "06:00:00", "05:55:00"))   # latest 06:10 -> 06:40
y.of <- function(iv) { f <- fl; f$time.start <- ""; f$time.end <- ""; f$time.interval <- iv
  r <- batz.plot.heatmap_aru.time(data = pfu, fig.list = f, suntimes = stu, project.name = paste0("yiv", iv), dir.save = dir.out)
  c(hm(r$plots[[1]]$y.start), hm(r$plots[[1]]$y.end)) }
## time.start blank: 1 hour before the earliest Dusk (21:33 -> 20:33), rounded down to the full hour = 20:00
ok(identical(y.of("60"), c("20:00", "07:00")), "time.interval 60: start 20:00 (Dusk - 1 h), end 06:40 -> 07:00")
ok(identical(y.of("30"), c("20:00", "07:00")), "time.interval 30: 20:00 to 07:00")
ok(identical(y.of("120"), c("20:00", "08:00")), "time.interval 120: 20:00 to 08:00")
ok(identical(y.of("15 min"), c("20:00", "06:45")), "time.interval \"15 min\": 20:00 to 06:45")
ok(identical(y.of(""), c("20:00", "07:00")), "time.interval blank: 60")
## one given, one blank
fl10 <- fl; fl10$time.start <- "18:00"; fl10$time.end <- ""
r10 <- batz.plot.heatmap_aru.time(data = pfu, fig.list = fl10, suntimes = stu, project.name = "yhalf", dir.save = dir.out)
ok(hm(r10$plots[[1]]$y.start) == "18:00" && hm(r10$plots[[1]]$y.end) == "07:00", "time.start given, time.end from the schedule")
## typed into the call (call > fig.list)
r11 <- batz.plot.heatmap_aru.time(data = pfu, fig.list = fl10, suntimes = stu, project.name = "ycall", dir.save = dir.out, time.start = "22:00")
ok(hm(r11$plots[[1]]$y.start) == "22:00", "time.start typed into the call wins over the fig.list")
## inferred schedule from batz.generate_plotframe.sm4 ("observed HH:MM-HH:MM"), no suntimes needed
fl12 <- fl; fl12$time.start <- ""; fl12$time.end <- ""
r12 <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl12, project.name = "yobs", dir.save = dir.out)
print(unique(pf$aru.schedule))
ok(hm(r12$plots[[1]]$y.start) == "20:00" && hm(r12$plots[[1]]$y.end) == "06:00", "observed 20:03-05:44 -> 20:00 to 06:00")
## sun-based schedule without suntimes, or no $aru.schedule column: range of the plotted intervals (NOTE)
r13 <- batz.plot.heatmap_aru.time(data = pfu, fig.list = fl12, project.name = "ynosun", dir.save = dir.out)
r14 <- batz.plot.heatmap_aru.time(data = pf[, names(pf) != "aru.schedule"], fig.list = fl12, project.name = "ynocol", dir.save = dir.out)
ok(hm(r14$plots[[1]]$y.start) == "20:00" && hm(r14$plots[[1]]$y.end) == "06:00", "no $aru.schedule: range of the plotted intervals")

## ---- 7. $plot.dimensions: saved size in inches (2026-11-09) -------------------
px <- function(f) dim(png::readPNG(f))[2:1]                                            # width, height in pixels
last.png <- function(tag) { f <- list.files(dir.out, paste0("^", tag, "_.*\\.png$"), full.names = TRUE); f[which.max(file.mtime(f))] }
fl15 <- fl; fl15$plot.dimensions <- "1 ppt"
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dim1ppt", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dim1ppt")), c(1200L, 600L)), "\"1 ppt\" = 12 x 6 in")
fl15$plot.dimensions <- " 1/3 PAGE "
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dimthird", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimthird")), c(600L, 350L)), "\"1/3 page\" (case and spaces ignored) = 6 x 3.5 in")
fl15$plot.dimensions <- "7 x 4"
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dimcustom", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimcustom")), c(700L, 400L)), "\"7 x 4\" = 7 x 4 in")
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dimcall", dir.save = dir.out, ggsave.dpi = 100, plot.dimensions = "2/3 ppt")
ok(identical(px(last.png("dimcall")), c(800L, 600L)), "typed into the call wins: \"2/3 ppt\" = 8 x 6 in")
fl15$plot.dimensions <- ""
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dimblank", dir.save = dir.out, ggsave.dpi = 100)
ok(identical(px(last.png("dimblank")), c(1050L, 750L)), "blank: plotopts plot.width + pad x plot.height + pad (10.5 x 7.5)")
fl15$plot.dimensions <- "half page"
batz.plot.heatmap_aru.time(data = pf, fig.list = fl15, project.name = "dimbad", dir.save = dir.out, ggsave.dpi = 100)   # NOTE: not a size
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
r9a <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, project.name = "zero.no", dir.save = dir.out)
r9b <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, project.name = "zero.yes", dir.save = dir.out, plot.zero = TRUE)
ok(nrow(r9a$ggplots[[1]]$data) == nrow(r9b$ggplots[[1]]$data) - n0 && n0 > 0, "plot.zero = FALSE (default): 0-minute tiles left out")

## ---- 10. Y axis start from Dusk, legend.position, legend.scalebar, font.family ----
## Dusk 20:33 (st) -> 19:33 -> 19:00 (Bristol example: 20:35 -> 19:35 -> 19:00)
ok(hm(r2$plots[[1]]$y.start) == "19:00", "time.start blank: Dusk 20:33 - 1 h = 19:33, rounded down to 19:00")
std <- st; std$sunset <- paste(as.Date("2026-06-26") + 0:3, c("20:35:00", "20:36:00", "20:37:00", "20:38:00"))
r10a <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl12, suntimes = std, project.name = "ydusk", dir.save = dir.out)
ok(hm(r10a$plots[[1]]$y.start) == "19:00", "earliest Dusk 20:35 -> 19:00")
std$sunset <- paste(as.Date("2026-06-26") + 0:3, "21:00:00")
r10b <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl12, suntimes = std, project.name = "ydusk2", dir.save = dir.out)
ok(hm(r10b$plots[[1]]$y.start) == "20:00", "Dusk 21:00 exactly -> 20:00")
r10c <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl12, suntimes = std, project.name = "ydusk3", dir.save = dir.out, time.start = "18:00")
ok(hm(r10c$plots[[1]]$y.start) == "18:00", "time.start given still wins over the Dusk rule")
gd <- function(r) r$ggplots[[1]]$scales$get_scales("fill")$guide
for (pos in c("bottom", "top", "left", "right")) {
  r <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, suntimes = st, project.name = paste0("leg.", pos), dir.save = dir.out, legend.position = pos)
  ok(identical(r$ggplots[[1]]$theme$legend.position, pos) &&
       identical(gd(r)$direction, if (pos %in% c("bottom", "top")) "horizontal" else "vertical"),
     paste0("legend.position = \"", pos, "\": colorbar ", if (pos %in% c("bottom", "top")) "left to right" else "bottom to top"))
}
rbad <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, project.name = "leg.bad", dir.save = dir.out, legend.position = "middle")   # NOTE
ok(identical(rbad$ggplots[[1]]$theme$legend.position, "bottom"), "legend.position not bottom/top/left/right: NOTE and bottom")
ok(identical(gd(r2)$title.position, "top"), "legend.scalebar default \"below\": bar under the words")
rsr <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, suntimes = st, project.name = "scalebar.right", dir.save = dir.out, legend.scalebar = "right")
ok(identical(gd(rsr)$title.position, "left"), "legend.scalebar = \"right\": bar to the right of the words")
fls <- fl; fls$legend.scalebar <- "right"
rsf <- batz.plot.heatmap_aru.time(data = pf, fig.list = fls, project.name = "scalebar.figlist", dir.save = dir.out)
ok(identical(gd(rsf)$title.position, "left"), "legend.scalebar read from the fig.list")
rsb <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, project.name = "scalebar.bad", dir.save = dir.out, legend.scalebar = "above")   # NOTE
ok(identical(gd(rsb)$title.position, "top"), "legend.scalebar not below/right: NOTE and below")
ok(identical(r2$ggplots[[1]]$theme$text$family, "Arial"), "font.family default Arial")
po <- read.csv(system.file("plotopts", "pltop.heatmap_aru.time.csv", package = "Batz"), check.names = FALSE, colClasses = "character")
po <- po[!po$parameter %in% c("font.family", "legend.scalebar"), ]
rold <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, aes.plotopts = po, project.name = "oldplotopts", dir.save = dir.out)
ok(identical(rold$ggplots[[1]]$theme$text$family, "Arial") && identical(gd(rold)$title.position, "top"),
   "older plotopts without the font.family / legend.scalebar rows: Arial and below")

## ---- 11. $plot.type = the function name without batz.plot. ----
ok(identical(fl$plot.type, "heatmap_aru.time"), "batz.generate_figlist model row: $plot.type = \"heatmap_aru.time\"")
for (pt in c("heatmap_aru.time", "batz.plot.heatmap_aru.time", " Heatmap_ARU.time ", "sm4.heatmap")) {   # old value: NOTE, still read
  f <- fl; f$plot.type <- pt
  r <- batz.plot.heatmap_aru.time(data = pf, fig.list = f, project.name = "ptype", dir.save = dir.out)
  ok(length(r$ggplots) == 1, paste0("$plot.type \"", pt, "\" plotted"))
}
f <- fl; f$plot.type <- "heatmap_activity.species"
r <- batz.plot.heatmap_aru.time(data = pf, fig.list = f, project.name = "ptype.other", dir.save = dir.out)   # NOTE: skipped
ok(length(r$ggplots) == 0, "another function's $plot.type: skipped")

## ---- 12. aes.plotopts as a CSV path or the pooled workbook ----
fx <- batz.generate_plotopts(project.name = "acme", dir.save = dir.out)
po.csv <- file.path(dir.out, "acme_pltop.heatmap_aru.time.csv")
po <- read.csv(po.csv, check.names = FALSE, colClasses = "character", na.strings = character(0))
po$acme[po$parameter == "dawn.color"] <- "orange"; write.csv(po, po.csv, row.names = FALSE)
rc <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, suntimes = st, aes.plotopts = po.csv, aes.style = "acme", project.name = "po.csv", dir.save = dir.out)
ok(identical(unname(rc$ggplots[[1]]$scales$get_scales("colour")$palette(3)[1]), "orange"), "aes.plotopts = CSV path, aes.style = project column")
rx <- batz.plot.heatmap_aru.time(data = pf, fig.list = fl, aes.plotopts = file.path(dir.out, "acme_pltop.pooled.xlsx"), aes.style = "acme",
                                 project.name = "po.xlsx", dir.save = dir.out)   # NOTE: tab pltop.heatmap_aru.time
ok(length(rx$ggplots) == 1, "aes.plotopts = pooled .xlsx path: own tab read")
e <- try(batz.plot.heatmap_aru.time(data = pf, fig.list = fl, aes.plotopts = c(file.path(dir.out, "acme_pltop.pooled.xlsx"), "nope"), dir.save = dir.out), silent = TRUE)
ok(inherits(e, "try-error") && grepl("no tab 'nope'", e), "pooled .xlsx with a missing tab: error lists the tabs")

## ---- 13. $date.interval: the X-axis labels (replaces $xaxe.interval) ----
xb <- function(r) { b <- ggplot2::ggplot_build(r$ggplots[[1]])$layout$panel_params[[1]]$x$breaks; as.Date(b[!is.na(b)], origin = "1970-01-01") }
ok("date.interval" %in% names(fl) && match("date.interval", names(fl)) == match("date.end", names(fl)) + 1,
   "fig.list template: $date.interval right after $date.end")
fdi <- fl; fdi$xaxe.interval <- "4"; fdi$date.interval <- "6/27/2026; 2026-06-29; 7/15/2026; 6/40/2026"   # NOTE: outside / not a date
rdi <- batz.plot.heatmap_aru.time(data = pf, fig.list = fdi, project.name = "dint", dir.save = dir.out, save.auto = FALSE)
ok(identical(xb(rdi), as.Date(c("2026-06-27", "2026-06-29"))), "$date.interval: listed dates in the window are the labels")
fdi$date.interval <- ""
rdb <- batz.plot.heatmap_aru.time(data = pf, fig.list = fdi, project.name = "dint", dir.save = dir.out, save.auto = FALSE)
ok(length(xb(rdb)) == 4, "$date.interval blank: $xaxe.interval (4 labels)")
fdi$date.interval <- "7/15/2026"                                                                     # NOTE: none in the window
rdn <- batz.plot.heatmap_aru.time(data = pf, fig.list = fdi, project.name = "dint", dir.save = dir.out, save.auto = FALSE)
ok(length(xb(rdn)) == 4, "$date.interval with no date in the window: $xaxe.interval")

cat("\nALL TESTS PASSED - PNGs in", dir.out, "\n")

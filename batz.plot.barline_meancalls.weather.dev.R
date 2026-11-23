# =============================================================================
# batz.plot.barline_meancalls.weather.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot.barline_meancalls.weather() (Batz package).
# Section 1: made-up data, run the whole script. Section 2 (commented): the
# NA-Bat files.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
dir.out <- file.path(tempdir(), "barline_meancalls"); unlink(dir.out, recursive = TRUE)

## ---- 1. made-up data: sun region "R1", 3 ARUs, a mobile, 10 nights -----------------
nights <- seq(as.Date("2026-07-01"), as.Date("2026-07-10"), by = "day")
arulist <- data.frame(aru.name = c("R1-A", "R1-B", "R1-C"), aru.label = c("R1-North", "R1-South", "R1-West"),
                      sunregion = "R1", date.start = "7/1/2026", date.end = "7/11/2026")
bats <- do.call(rbind, lapply(arulist$aru.name, function(a)
  rbind(data.frame(spp.id = "All Detections", date = format(nights), observations.count = 30, aru.name = a),
        data.frame(spp.id = "epfu", date = format(nights), observations.count = 6, aru.name = a))))
bats <- bats[!(bats$aru.name == "R1-C" & bats$date %in% c("2026-07-04", "2026-07-05")), ]   # R1-C out Jul 4-5
bats <- rbind(bats, data.frame(spp.id = c("All Detections", "epfu"), date = "2026-07-03", observations.count = c(12, 4), aru.name = "MOB"))
t <- seq(as.POSIXct("2026-07-01 00:00", tz = "UTC"), as.POSIXct("2026-07-11 23:55", tz = "UTC"), by = "15 min")
h <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
weather <- data.frame(site = "R1;R2", date.time = format(t, "%Y-%m-%d %H:%M:%S"),
                      temperature.c = round(15 + 6 * cos((h - 15) / 24 * 2 * pi), 2),
                      precipitation.mm_interval = 0)
wet <- function(d, h0, h1, mm) weather$precipitation.mm_interval[as.Date(t) == as.Date(d) & h >= h0 & h < h1] <<- mm
wet("2026-07-02", 19, 21, 0.5)                              # 4 mm 18:00-24:00            -> black
wet("2026-07-06", 13, 15, 0.5); wet("2026-07-07", 2, 4, 0.5) # 12-18 and 00-08 next day  -> the second: 00:00-08:00
wet("2026-07-08", 13, 14, 0.1)                              # 0.4 mm only                 -> none
fl <- data.frame(plot = TRUE, plot.type = "barline_meancalls.weather", plot.set = "R1",
                 date.start = "7/1/2026", date.end = "7/10/2026", break.start = "7/4/2026", break.end = "7/5/2026")
mn <- data.frame(sunregion = "R1", date = "7/3/2026")

r <- batz.plot.barline_meancalls.weather(bats, weather, arulist, fig.list = fl, mobile.nights = mn, project.name = "acme", dir.save = dir.out)
m <- r$data[r$data$spp.id == "All Detections", ]
ok(all(m$observations.count[m$group == "R1" & !m$date %in% c("2026-07-04", "2026-07-05")] == 30), "mean = night total / 3 (90 / 3 = 30)")
ok(all(m$observations.count[m$group == "R1" & m$date %in% c("2026-07-04", "2026-07-05")] == 30) &&
   all(m$detectors[m$group == "R1" & m$date %in% c("2026-07-04", "2026-07-05")] == 2), "outage nights: total / 2 (60 / 2 = 30)")
ok(identical(m$observations.count[m$group == "R1-MOB"], 12) && identical(m$date[m$group == "R1-MOB"], "2026-07-03"),
   "mobile named \"MOB\": sun region from mobile.nights, count as is")
n <- r$nights
ok(n$rain[n$date == as.Date("2026-07-02")] == "18:00-24:00", "rain 18:00-24:00 > 1 mm")
ok(n$rain[n$date == as.Date("2026-07-06")] == "00:00-08:00", "rain in two windows: the second")
ok(n$rain[n$date == as.Date("2026-07-08")] == "none", "0.4 mm: none")
x <- weather[as.Date(t) == as.Date("2026-07-03") & h >= 20, ]
ok(isTRUE(all.equal(n$temperature.max[n$date == as.Date("2026-07-03")], max(x$temperature.c))) &&
   isTRUE(all.equal(n$temperature.min[n$date == as.Date("2026-07-03")], min(x$temperature.c))), "temperature max / min 20:00-24:00")
ok(length(r$files) == 1 && grepl("^acme_barline_meancalls\\.weather_R1_20260701to20260710_[0-9]{14}\\.png$", basename(r$files)), "file name")
g <- ggplot2::ggplot_build(r$plots[["R1"]])
ok(any(vapply(r$plots[["R1"]]$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))), "rain strip drawn")

## settings in the call: n.detectors, rain.multiple, temp.range
r2 <- batz.plot.barline_meancalls.weather(bats, weather, arulist, fig.list = fl, mobile.nights = mn, dir.save = NULL,
                                          n.detectors = 2, rain.multiple = "first", temp.range = c(0, 40))
ok(r2$data$observations.count[r2$data$group == "R1" & r2$data$spp.id == "All Detections" & r2$data$date == "2026-07-01"] == 45,
   "n.detectors = 2: 90 / 2")
ok(r2$nights$rain[r2$nights$date == as.Date("2026-07-06")] == "12:00-18:00", "rain.multiple = \"first\"")
## no fig.list: one plot per sun region, window from arulist
r3 <- batz.plot.barline_meancalls.weather(bats, weather, arulist, dir.save = NULL)
ok(names(r3$plots) == "R1" && max(r3$nights$date) == as.Date("2026-07-10"), "no fig.list: arulist window (to the night before date.end)")
e <- try(batz.plot.barline_meancalls.weather(bats[, -1], weather, arulist, dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl("missing headers: data \\$spp.id", e), "missing header: error")

## ---- 2. NA-Bat ---------------------------------------------------------------------------
# bats <- read.csv("plfr.NA.bat.csv")
# bats$aru.name <- sub("^5157-", "5175-", bats$aru.name)                 # typo in that file
# bats$spp.id[toupper(bats$spp.id) %in% c("LUSO", "MYSO")] <- "mylu"
# weather <- read.csv("weather.csv"); arulist <- read.csv("NA-Bat_20261118173904.arulist.csv")
# fl <- data.frame(plot = TRUE, plot.type = "barline_meancalls.weather", plot.set = c("105059", "107802", "108325", "1079", "5175", "55"),
#                  date.start = rep(c("7/7/2026", "7/17/2026"), each = 3), date.end = rep(c("8/5/2026", "8/17/2026"), each = 3),
#                  break.start = c("", "", "", "7/19/2026", "7/25/2026", "7/21/2026"), break.end = c("", "", "", "7/30/2026", "7/31/2026", "7/31/2026"))
# mn <- data.frame(sunregion = c("105059", "107802", "1079", "108325", "5175", "55"),
#                  date = c("7/16/2026;8/4/2026", "7/20/2026", "7/17/2026", "7/15/2026;8/5/2026", "7/19/2026", "7/31/2026"))
# batz.plot.barline_meancalls.weather(bats, weather, arulist, fig.list = fl, mobile.nights = mn, project.name = "NA-Bat")

cat("\nALL TESTS PASSED\n")

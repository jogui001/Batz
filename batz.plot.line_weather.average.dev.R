# =============================================================================
# batz.plot.line_weather.average.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot.line_weather.average() (Batz package).
# Section 1 uses made-up weather and sun times (run the whole script);
# section 2 (commented) runs it on real files.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
dir.out <- file.path(tempdir(), "weather_average_test"); unlink(dir.out, recursive = TRUE)

## ---- made-up data: 2 stations, 5-minute readings, 2026-07-01 to 2026-07-21 ----
tz <- "America/New_York"
t <- seq(as.POSIXct("2026-07-01 00:00", tz = tz), as.POSIXct("2026-07-21 23:55", tz = tz), by = "5 min")
t <- t[format(t, "%Y-%m-%d") != "2026-07-10"]                      # no readings on 7/10
h <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
mk <- function(code, site, dT) {
  p.int <- ifelse(format(t, "%Y-%m-%d") == "2026-07-05" & h >= 22 & h < 23, 0.5, 0)   # 6 mm, 22:00-23:00 on 7/5
  data.frame(site = site, station.name = paste("Station", code), station.code = code,
             station.latitude = 43.6, station.longitude = -71.7,
             date.time = format(t, "%Y-%m-%d %H:%M:%S"),
             temperature.c = round(18 + dT + 6 * cos((h - 15) / 24 * 2 * pi), 2),
             wind.speed.kmh = round(10 + 5 * sin(h / 24 * 2 * pi), 2), wind.gust.kmh = round(20 + 5 * sin(h / 24 * 2 * pi), 2),
             precipitation.mm_interval = p.int, stringsAsFactors = FALSE)
}
weather <- rbind(mk("KNHA1", "Clerk", 0), mk("KNHB2", "SugarHill", -3))
d <- seq(as.Date("2026-06-30"), as.Date("2026-07-22"), by = "day")
suntimes <- data.frame(aru.name = "Clerk", date = format(d), latitude = 43.59, longitude = -71.73, time.zone = tz,
                       sunset = paste(format(d), "20:30:00"), sunrise = paste(format(d), "05:15:00"),
                       sunrise.monitoringnight = paste(format(d + 1), "05:15:00"))

## ---- 1. period = "night" (default): sunset to the next sunrise ----------------
r1 <- batz.plot.line_weather.average(weather, suntimes, dir.save = dir.out, project.name = "acme")
print(head(r1$data[r1$data$station == "KNHA1", ], 6))
x <- weather[weather$station.code == "KNHA1", ]; tt <- as.POSIXct(x$date.time, tz = tz)
inw <- tt >= as.POSIXct("2026-07-03 20:30", tz = tz) & tt < as.POSIXct("2026-07-04 05:15", tz = tz)
row <- r1$data[r1$data$station == "KNHA1" & r1$data$date == as.Date("2026-07-03"), ]
ok(row$readings == sum(inw) && row$temperature.min == round(min(x$temperature.c[inw]), 2) &&
   row$temperature.max == round(max(x$temperature.c[inw]), 2) && row$temperature.mean == round(mean(x$temperature.c[inw]), 2),
   "night 7/3: readings, min / mean / max temperature (sunset 20:30 to 05:15)")
ok(isTRUE(all.equal(row$wind.max, round(max(x$wind.speed.kmh[inw]) / 3.6, 2))), "wind in m/s (km/h / 3.6)")
ok(r1$data$precipitation.total[r1$data$station == "KNHA1" & r1$data$date == as.Date("2026-07-05")] == 6, "night 7/5: precipitation total 6 mm")
ok(r1$data$readings[r1$data$station == "KNHA1" & r1$data$date == as.Date("2026-07-10")] == 63,
   "night 7/10: only the 7/11 readings up to sunrise (none on 7/10)")
ok(length(r1$files) == 2 && all(grepl("^acme_weather\\.average_(Clerk|SugarHill)_KNH.._night_20260701to20260721_[0-9]{14}\\.png$", basename(r1$files))),
   "one PNG per station: acme_weather.average_<site>_<station>_night_<dates>_<timestamp>.png")
ok(all(r1$data$aru.name == "Clerk"), "nearest suntimes site used")

## ---- 2. period = "24hr", date.type = "calendar.day" (00:00 to 24:00) ----------
r2 <- batz.plot.line_weather.average(weather, period = "24hr", dir.save = NULL)
in.d <- format(tt, "%Y-%m-%d") == "2026-07-03"
row <- r2$data[r2$data$station == "KNHA1" & r2$data$date == as.Date("2026-07-03"), ]
ok(row$readings == 288 && row$temperature.min == round(min(x$temperature.c[in.d]), 2) && row$temperature.max == round(max(x$temperature.c[in.d]), 2),
   "24hr calendar.day: 288 readings, min / max of the day (no suntimes needed)")
ok(row$period.start == "2026-07-03 00:00:00" && row$period.end == "2026-07-04 00:00:00", "calendar.day period.start / period.end")
ok(r2$data$readings[r2$data$station == "KNHA1" & r2$data$date == as.Date("2026-07-10")] == 0 &&
   is.na(r2$data$temperature.mean[r2$data$station == "KNHA1" & r2$data$date == as.Date("2026-07-10")]), "7/10 no readings: NA (line broken)")

## ---- 3. date.type = "monitoring.night" (12:00 to 12:00) ----------------------
r3 <- batz.plot.line_weather.average(weather, period = "24hr", date.type = "monitoring.night", dir.save = NULL)
row <- r3$data[r3$data$station == "KNHA1" & r3$data$date == as.Date("2026-07-03"), ]
ok(row$period.start == "2026-07-03 12:00:00" && row$period.end == "2026-07-04 12:00:00" && row$readings == 288, "monitoring.night: 12:00 to 12:00")

## ---- 4. night window by clock time / length: no suntimes needed ----------------
r4 <- batz.plot.line_weather.average(weather, time.start_monitoring = "21:00", time.end_monitoring = "5 hours", dir.save = NULL)
row <- r4$data[r4$data$station == "KNHA1" & r4$data$date == as.Date("2026-07-03"), ]
ok(row$period.start == "2026-07-03 21:00:00" && row$period.end == "2026-07-04 02:00:00" && row$readings == 60, "21:00 for 5 hours")
e <- try(batz.plot.line_weather.average(weather, dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl("needs suntimes", e), "Sunset / Sunrise without suntimes: error")

## ---- 5. errors ---------------------------------------------------------------------
e <- try(batz.plot.line_weather.average(weather, suntimes, period = "week", dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl('period must be "night" or "24hr"', e), "bad period: error")
e <- try(batz.plot.line_weather.average(weather[, -grep("wind", names(weather))], suntimes, dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl("missing.*wind.speed.ms", e), "no wind column: error")

## ---- 6. wind gust, date range, settings from a pooled plotopts workbook ----------
r6 <- batz.plot.line_weather.average(weather, suntimes, wind.type = "gust", date.start = "2026-07-05", date.end = "2026-07-08", dir.save = NULL)
ok(nrow(r6$data) == 8 && min(r6$data$wind.min) > 4, "wind.type = \"gust\"; date.start / date.end: 4 dates x 2 stations")
po <- batz.generate_plotopts("batz.plot.line_weather.average", project.name = "acme", dir.save = dir.out)
pooled <- file.path(dir.out, "acme_pltop.pooled.xlsx")
wb <- openxlsx::loadWorkbook(pooled); tab <- openxlsx::read.xlsx(wb, "pltop.line_weather.average")
tab$acme[tab$parameter == "period"] <- "24hr"; tab$acme[tab$parameter == "temp.range"] <- "-5;30"
openxlsx::writeData(wb, "pltop.line_weather.average", tab); openxlsx::saveWorkbook(wb, pooled, overwrite = TRUE)
r7 <- batz.plot.line_weather.average(weather, dir.save = NULL, aes.plotopts = pooled, aes.style = "acme")
ok(all(r7$data$period == "24hr") && r7$axes$min[r7$axes$axis == "y"][1] == -5, "pooled workbook: period and temp.range from the acme column")

## ---- 6b. panel.layout = "separate": wind, temperature, precipitation panels --------
r8 <- batz.plot.line_weather.average(weather, suntimes, panel.layout = "separate", date.start = "2026-07-01", date.end = "2026-07-14", dir.save = NULL)
g <- r8$plots[["KNHA1"]]
ok(inherits(g$facet, "FacetWrap") && identical(levels(g$layers[[1]]$data$panel), c("Wind speed (m/s)", "Temperature (\u00b0C)", "Precipitation total (mm)")),
   "separate: 3 panels, wind / temperature / precipitation (panel.order default)")
r9 <- batz.plot.line_weather.average(weather, suntimes, panel.layout = "separate", stack.y = "Temperature", stack.y2 = "Precipitation",
                                     panel.order = c("Precipitation", "Temperature", "Wind"), dir.save = NULL)
ok(identical(levels(r9$plots[["KNHA1"]]$layers[[1]]$data$panel), c("Precipitation total (mm)", "Temperature (\u00b0C)")),
   "separate: only the types in stack.y / stack.y2, in panel.order")
r10 <- batz.plot.line_weather.average(weather, suntimes, panel.layout = "separate", temp.range = c(0, 30), wind.range = c(0, 8),
                                      precip.range = c(0, 4), dir.save = NULL)
b10 <- ggplot2::ggplot_build(r10$plots[["KNHA1"]])
yr <- lapply(b10$layout$panel_params, function(pp) pp$y.range)
ok(isTRUE(all.equal(yr[[1]], c(0, 8))) && isTRUE(all.equal(yr[[2]], c(0, 30))) && isTRUE(all.equal(yr[[3]], c(0, 4))),
   "separate: wind / temp / precipitation axes as given (scale.auto not used; precip.range)")
e <- try(batz.plot.line_weather.average(weather, suntimes, panel.layout = "stacked", dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl("panel.layout must be", e), "bad panel.layout: error")

## ---- 7. save.auto --------------------------------------------------------------------
suppressWarnings(rm(list = ls(globalenv(), pattern = "^acme_weather"), envir = globalenv()))
batz.plot.line_weather.average(weather, suntimes, dir.save = dir.out, project.name = "acme", save.auto = "generate 1")
ok(length(ls(globalenv(), pattern = "^acme_weather\\.average")) == 1, "save.auto = \"generate 1\": one ggplot object")

## ---- 2. real files -------------------------------------------------------------------
# weather  <- read.csv("weather.csv")                       # $weatherstation.data from batz.generate_weatherstation.data
# suntimes <- read.csv("MAMM_IN25092_2suntimes.csv")         # from batz.generate_suntimes.arulist
# batz.plot.line_weather.average(weather, suntimes, project.name = "IN25092")                    # nights
# batz.plot.line_weather.average(weather, period = "24hr", project.name = "IN25092")             # calendar days

cat("\nALL TESTS PASSED\n")

# =============================================================================
# batz.generate_weatherstation.data.dev.R  -  dev/test script (2026-11-04, per Josh)
# Keep this file OUTSIDE the package's R/ folder.
#
# Picks the nearest Weather Underground stations that record the needed
# weather.type(s) and downloads their data for date.start - date.end.
# Needs: jsonlite, and an API key set once per session (never saved in a
# script that goes to GitHub):  Sys.setenv(WU_API_KEY = "<key>")
# Every API call is spaced 2 s apart - keep the date range short while testing.
# =============================================================================
library(Batz)                    # install the current version first: pak::pak("jogui001/Batz")

test.data <- data.frame(site = c("bri", "cumberland"),
                        site.latitude  = c(43.687683883908406, 43.809682493532925),
                        site.longitude = c(-70.28145797545827, -70.32403651749544))

## ---- TEST 1: 2 stations per site with every default weather.type, 3 days ----
w1 <- batz.generate_weatherstation.data(test.data, station.max = 2,
                                        date.start = "2026-07-01", date.end = "2026-07-03")
names(w1)                                   # weatherstation.list, weatherstation.data
w1$weatherstation.list
head(w1$weatherstation.data)
stopifnot(identical(names(w1$weatherstation.data)[1:6],
                    c("site", "station.name", "station.code", "station.area", "station.latitude", "station.longitude")))

## ---- TEST 2: temperature + humidity only, patched from several stations -----
w2 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 5,
                                        weather.type = c("Temperature", "Humidity"), weather.patched = TRUE,
                                        date.start = "2026-07-01", date.end = "2026-07-02")
table(w2$weatherstation.data$station.code)

## ---- TEST 3: airport data (Condition / Visibility are airport-only) --------
w3 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 1, station.type = "airport",
                                        weather.type = c("Temperature", "Wind Speed", "Condition"),
                                        date.start = "2026-07-01", date.end = "2026-07-02")
head(w3$weatherstation.data)

## ---- TEST 4: errors ---------------------------------------------------------
try(batz.generate_weatherstation.data(test.data, date.start = "2026-07-01"))                                  # no date.end
try(batz.generate_weatherstation.data(test.data, weather.type = "Rain", date.start = "2026-07-01", date.end = "2026-07-02"))
try(batz.generate_weatherstation.data(test.data, date.start = "2026-07-05", date.end = "2026-07-01"))         # end before start

## ---- TEST 5: recording rate (2026-11-04) -----------------------------------
w5 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 2, distance.max = 25, sample.interval_min = 10,
                                        date.start = "2026-07-01", date.end = "2026-07-02")
w5$weatherstation.list[, c("site", "station.code", "sample.interval")]     # measured over the dates, <= 10

## ---- TEST 6: units (2026-11-04) ----------------------------------------------
## metric = Weather Underground's metric units (C, km/h, hPa, mm, km)
for (u in list("metric", "imperial", "both", c("metric", "imperial"))) {
  w6 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 1, units = u,
                                          date.start = "2026-07-01", date.end = "2026-07-01")
  cat("\nunits =", paste(u, collapse = ", "), "\n"); print(names(w6$weatherstation.data)[-(1:8)])
}
## "both": temperature.c, temperature.f, dewpoint.c, dewpoint.f, humidity.percent, ...
## c("metric", "imperial"): every metric column, then the imperial ones

## ---- TEST 7: precipitation since the previous record (2026-11-05) -------------
## PWS: $precipitation.mm is the day's running total; $precipitation.mm_interval
## is the amount since the previous record (resets at local midnight).
## Airport: the interval column is the reported hourly amount.
w7 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 1,
                                        date.start = "2026-06-26", date.end = "2026-06-27")
p7 <- w7$weatherstation.data[, c("station.code", "date.time", "precipitation.mm", "precipitation.mm_interval")]
print(head(p7[p7$precipitation.mm_interval > 0 & !is.na(p7$precipitation.mm_interval), ], 10))
## check: per station and day, the intervals add up to the day's last running total
p7$day <- substr(p7$date.time, 1, 10)
chk <- do.call(rbind, lapply(split(p7, list(p7$station.code, p7$day), drop = TRUE), function(x)
  data.frame(station.code = x$station.code[1], day = x$day[1],
             last.total = round(utils::tail(x$precipitation.mm[!is.na(x$precipitation.mm)], 1), 2),
             sum.interval = round(sum(x$precipitation.mm_interval, na.rm = TRUE), 2))))
print(chk)                                    # last.total and sum.interval should match

## ---- TEST 8: alternative headers $aru.name / $latitude / $longitude (2026-11-06) ----
## e.g. the ARU list written by batz.merge_sitevisit.acoustics(generate.arulist = TRUE)
alt.data <- data.frame(aru.name = c("bri", "cumberland"),
                       latitude  = c(43.687683883908406, 43.809682493532925),
                       longitude = c(-70.28145797545827, -70.32403651749544))
w8 <- batz.generate_weatherstation.data(alt.data, station.max = 1,
                                        date.start = "2026-07-01", date.end = "2026-07-01")
stopifnot(identical(sort(unique(w8$weatherstation.list$site)), c("bri", "cumberland")),
          all(c("site", "site.latitude", "site.longitude") %in% names(w8$weatherstation.list)))
## both present: $site / $site.latitude / $site.longitude are used
both <- cbind(test.data, aru.name = "other", latitude = 0, longitude = 0)
w8b <- batz.generate_weatherstation.list(both, station.max = 1)
stopifnot(identical(unique(w8b$site), test.data$site))
# arulist <- read.csv("bristol.arulist.csv"); batz.generate_weatherstation.list(arulist)

## ---- TEST 9: a station shared by several sites (2026-11-06) ------------------
## Clerk and SugarHill are ~0.5 km apart, so they pick the same stations:
## each record is listed once, $site = "Clerk;SugarHill"
arulist <- data.frame(aru.name = c("Clerk", "SugarHill"),
                      latitude = c(43.59224283, 43.59525033), longitude = c(-71.7364705, -71.73053717))
w9 <- batz.generate_weatherstation.data(arulist, station.max = 1,
                                        date.start = "2026-06-26", date.end = "2026-06-26")   # NOTE: station ... is shared by sites Clerk, SugarHill
table(w9$weatherstation.data$site)
stopifnot(!anyDuplicated(w9$weatherstation.data[, c("station.code", "date.time_utc")]),
          any(w9$weatherstation.data$site == "Clerk;SugarHill"))
w9$weatherstation.list[, c("site", "station.code")]          # still one row per site and station

## ---- TEST 10: save the weather data (2026-11-09) -----------------------------
## save = TRUE (default): <project.name>_weatherdata_<first date>to<last date>_<timestamp>.csv in dir.save
## (the tests above also save, into the working directory)
dir.w <- file.path(tempdir(), "weather_test")
w10 <- batz.generate_weatherstation.data(test.data[1, ], station.max = 1, date.start = "2026-07-01", date.end = "2026-07-02",
                                         dir.save = dir.w, project.name = "Bristol NH")
print(basename(w10$file))                   # Bristol-NH_weatherdata_20260701to20260702_<YYYYMMDDHHMMSS>.csv
stopifnot(grepl("^Bristol-NH_weatherdata_\\d{8}to\\d{8}_\\d{14}\\.csv$", basename(w10$file)),
          nrow(read.csv(w10$file)) == nrow(w10$weatherstation.data))
w10b <- batz.generate_weatherstation.data(test.data[1, ], station.max = 1, date.start = "2026-07-01", date.end = "2026-07-01",
                                          save = FALSE)
stopifnot(is.na(w10b$file))                 # save = FALSE: no file

## ---- save ------------------------------------------------------------------
# write.csv(w1$weatherstation.data, "weatherstation.data.csv", row.names = FALSE)

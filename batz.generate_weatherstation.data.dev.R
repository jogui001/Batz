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
dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
for (f in c("batz.util_standardize.headers.R", "batz.generate_weatherstation.list.R",
            "batz.generate_weatherstation.data.R")) source(file.path(dir.repo, "R", f))

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

## ---- save ------------------------------------------------------------------
# write.csv(w1$weatherstation.data, "weatherstation.data.csv", row.names = FALSE)

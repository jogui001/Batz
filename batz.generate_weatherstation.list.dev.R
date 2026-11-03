# =============================================================================
# batz.generate_weatherstation.list.dev.R  -  dev/test script (2026-11-03, per Josh)
# Keep this file OUTSIDE the package's R/ folder.
#
# Lists the Weather Underground stations (PWS + airports) near each site by
# distance interval (km). Needs: jsonlite, and an API key set once per session
# (never saved in a script that goes to GitHub):
#   Sys.setenv(WU_API_KEY = "<key>")
# =============================================================================
dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
for (f in c("batz.util_standardize.headers.R", "batz.generate_weatherstation.list.R")) source(file.path(dir.repo, "R", f))

## test data as sent 2026-11-03 had latitude/longitude swapped - corrected here
test.data <- data.frame(site = c("bri", "cumberland"),
                        site.latitude  = c(43.687683883908406, 43.809682493532925),
                        site.longitude = c(-70.28145797545827, -70.32403651749544))

## ---- TEST 1: counts at each interval (km) -----------------------------------
t1 <- batz.generate_weatherstation.list(test.data, distance.max = c(5, 10, 15, 25, 50, 100), station.max = Inf)
aggregate(station.total ~ site + distance.max, t1, max)      # one total per site and distance

## ---- TEST 2: the 25 nearest stations per site -------------------------------
t2 <- batz.generate_weatherstation.list(test.data, station.max = 25)
stopifnot(all(table(t2$site) <= 25))

## ---- TEST 3: swapped coordinates -> NOTE ------------------------------------
sw <- test.data; names(sw)[2:3] <- c("site.longitude", "site.latitude")
batz.generate_weatherstation.list(sw, station.max = 2)        # NOTE: latitude ... in Antarctica

## ---- TEST 4: errors ---------------------------------------------------------
try(batz.generate_weatherstation.list(test.data[, 1:2]))                   # missing $site.longitude
try(batz.generate_weatherstation.list(test.data, distance.max = -5))         # bad distance.max
try(batz.generate_weatherstation.list(test.data, api.key = ""))             # no key

## ---- TEST 5: station name + data types (2026-11-03) -------------------------
t5 <- batz.generate_weatherstation.list(test.data, station.max = 5)
t5[, c("site", "station.name", "station.type", "station.datatypes")]

## ---- TEST 6: station name / code / area (2026-11-04) ------------------------
t6 <- batz.generate_weatherstation.list(test.data)              # station.max now defaults to 10
t6[, c("site", "station.name", "station.code", "station.area")]  # e.g. Ocean Avenue Station | FB28 - KMEPORTL81 | Portland

## ---- TEST 7: distance.max / sample.interval_min (2026-11-04) ----------------
t7 <- batz.generate_weatherstation.list(test.data)                         # 50 km, 10 stations, records at least hourly
t7[, c("site", "station.code", "station.distance", "sample.interval")]
t7b <- batz.generate_weatherstation.list(test.data, sample.interval_min = 10)  # only stations recording every <= 10 min
stopifnot(all(t7b$sample.interval <= 10, na.rm = TRUE))

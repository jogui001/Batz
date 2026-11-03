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
t1 <- batz.generate_weatherstation.list(test.data, interval = c(5, 10, 15, 25, 50, 100))
aggregate(station.total ~ site + interval, t1, max)          # one total per site and interval

## ---- TEST 2: the 25 nearest stations per site -------------------------------
t2 <- batz.generate_weatherstation.list(test.data, station.max = 25)
stopifnot(all(table(t2$site) <= 25))

## ---- TEST 3: swapped coordinates -> NOTE ------------------------------------
sw <- test.data; names(sw)[2:3] <- c("site.longitude", "site.latitude")
batz.generate_weatherstation.list(sw, station.max = 2)        # NOTE: latitude ... in Antarctica

## ---- TEST 4: errors ---------------------------------------------------------
try(batz.generate_weatherstation.list(test.data[, 1:2]))                   # missing $site.longitude
try(batz.generate_weatherstation.list(test.data, interval = c(-5, 10)))     # bad interval
try(batz.generate_weatherstation.list(test.data, api.key = ""))             # no key

## ---- TEST 5: station name + data types (2026-11-03) -------------------------
t5 <- batz.generate_weatherstation.list(test.data, station.max = 5)
t5[, c("site", "station.name", "station.type", "station.datatypes")]
batz.generate_weatherstation.list(test.data, station.max = 5, datatypes = FALSE)   # skip the extra look-ups

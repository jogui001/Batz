## =============================================================================
## wunderground_download.R  -  2026-11-03, per Josh
##
## Finds the Weather Underground station closest to the centre of all Seven
## Islands ARU sites that has every "Daily Observations" column on the
## wunderground.com history page (Time, Temperature, Dew Point, Humidity, Wind,
## Wind Speed, Wind Gust, Pressure, Precip., Condition), downloads one month of
## its observations, and saves them as a .csv.
##
## Only airport (ICAO) stations have the full Daily Observations table - personal
## weather stations (PWS) have no "Condition" - so airports are searched, nearest
## first, and the first one with data in all ten columns is used.
##
## Needs: jsonlite  (install.packages("jsonlite"))
## API key: never saved in this file. Run once per R session:
##   Sys.setenv(WU_API_KEY = "<your key>")
## =============================================================================

api.key    <- Sys.getenv("WU_API_KEY")
site.lat   <- 45.7746        # centre of all ARU sites (mean of the vetted-file coordinates)
site.lon   <- -69.7806
date.start <- as.Date("2026-07-01")
date.end   <- as.Date("2026-07-31")
time.zone  <- "America/New_York"
units      <- "e"            # "e" = imperial (°F, mph, in), "m" = metric (°C, km/h, mm)
dir.save   <- getwd()

if (!nzchar(api.key)) stop('Set the key first: Sys.setenv(WU_API_KEY = "<your key>")', call. = FALSE)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop('install.packages("jsonlite")', call. = FALSE)
if (as.numeric(date.end - date.start) > 30) stop("The history API returns at most 31 days per request.", call. = FALSE)

wu.get <- function(url) {
  tryCatch(jsonlite::fromJSON(url), error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("401|403", msg)) stop("The API key was refused (", msg, ").", call. = FALSE)
    NULL                                                     # e.g. 204/404: no data for that station
  })
}

## ---- 1. airports near the sites, nearest first --------------------------------
near <- wu.get(sprintf("https://api.weather.com/v3/location/near?geocode=%.4f,%.4f&product=airport&format=json&apiKey=%s",
                       site.lat, site.lon, api.key))
if (is.null(near)) stop("No airports returned for ", site.lat, ", ", site.lon, call. = FALSE)
ap <- as.data.frame(near$location[c("icaoCode", "airportName", "latitude", "longitude", "distanceKm")], stringsAsFactors = FALSE)
ap$countryCode <- if (!is.null(near$location$countryCode)) near$location$countryCode else "US"
ap <- ap[!is.na(ap$icaoCode) & nzchar(ap$icaoCode), ]
ap <- ap[order(as.numeric(ap$distanceKm)), ]
cat("Airports checked (nearest first):\n"); print(ap[, c("icaoCode", "airportName", "distanceKm")], row.names = FALSE)

## the wunderground.com "Daily Observations" columns and the API field behind each
cols <- c("Temperature" = "temp", "Dew Point" = "dewPt", "Humidity" = "rh", "Wind" = "wdir_cardinal",
          "Wind Speed" = "wspd", "Wind Gust" = "gust", "Pressure" = "pressure", "Precip." = "precip_hrly",
          "Condition" = "wx_phrase")

## ---- 2. first airport with data in every column ------------------------------
found <- NULL
for (i in seq_len(nrow(ap))) {
  url <- sprintf("https://api.weather.com/v1/location/%s:9:%s/observations/historical.json?apiKey=%s&units=%s&startDate=%s&endDate=%s",
                 ap$icaoCode[i], ap$countryCode[i], api.key, units, format(date.start, "%Y%m%d"), format(date.end, "%Y%m%d"))
  obs <- wu.get(url)$observations
  if (is.null(obs) || NROW(obs) == 0) { cat(sprintf("  %s: no observations\n", ap$icaoCode[i])); next }
  empty <- names(cols)[!vapply(cols, function(f) f %in% names(obs) && any(!is.na(obs[[f]]) & nzchar(as.character(obs[[f]]))), logical(1))]
  if (length(empty) > 0) { cat(sprintf("  %s: no data for %s\n", ap$icaoCode[i], paste(empty, collapse = ", "))); next }
  cat(sprintf("  %s: all Daily Observations columns present (%d observations)\n", ap$icaoCode[i], NROW(obs)))
  found <- list(station = ap[i, ], obs = obs)
  break
}
if (is.null(found)) stop("None of the nearby airports has data in every Daily Observations column for this month.", call. = FALSE)

## ---- 3. table with the wunderground.com headers, then save --------------------
o  <- found$obs
dt <- as.POSIXct(o$valid_time_gmt, origin = "1970-01-01", tz = time.zone)
u  <- if (units == "e") c(t = "°F", s = "mph", p = "in", r = "in") else c(t = "°C", s = "km/h", p = "mb", r = "mm")
out <- data.frame(
  station.icao = found$station$icaoCode, station.name = found$station$airportName,
  station.distance.km = found$station$distanceKm,
  Date = format(dt, "%Y-%m-%d"), Time = format(dt, "%I:%M %p"),
  Temperature = o$temp, `Dew Point` = o$dewPt, Humidity = o$rh, Wind = o$wdir_cardinal,
  `Wind Speed` = o$wspd, `Wind Gust` = o$gust, Pressure = o$pressure, `Precip.` = o$precip_hrly,
  Condition = o$wx_phrase, check.names = FALSE, stringsAsFactors = FALSE)
f <- file.path(dir.save, sprintf("wunderground_%s_%s_%s.csv", found$station$icaoCode,
                                 format(date.start, "%Y%m%d"), format(date.end, "%Y%m%d")))
write.csv(out, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("\nSaved: %s\nStation: %s %s (%.0f km from the sites), %d observations. Units: Temperature/Dew Point %s, Humidity %%, Wind Speed/Gust %s, Pressure %s, Precip. %s.\n",
            f, found$station$icaoCode, found$station$airportName, as.numeric(found$station$distanceKm), nrow(out),
            u["t"], u["s"], u["p"], u["r"]))

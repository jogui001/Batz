#' Download Weather Underground data from the nearest suitable stations
#'
#' For each site, picks the nearest Weather Underground stations that record
#' every \code{weather.type} (or, with \code{weather.patched = TRUE}, that add
#' data the stations already picked are missing) at least every
#' \code{sample.interval_min} minutes, up to \code{station.max} stations
#' within \code{distance.max} km, and downloads their observations
#' from \code{date.start} to \code{date.end}. Built 2026-11-04, per Josh
#' (replaces the paused \code{wunderground_download.R} script).
#'
#' @inheritParams batz.generate_weatherstation.list
#' @param weather.type Character, default \code{c("Temperature", "Dew Point",
#'   "Humidity", "Wind", "Wind Speed", "Wind Gust", "Pressure", "Precip.",
#'   "UV", "Solar")}. The kinds of data required (wunderground.com table
#'   headers; \code{"Condition"} and \code{"Visibility"} - airports only - may
#'   also be used). A station that doesn't record all of them over the dates
#'   is not counted (unless \code{weather.patched = TRUE}).
#' @param weather.patched Logical, default \code{FALSE}. \code{TRUE}: a station
#'   counts when it adds data the stations already picked don't have - an hour
#'   (station local time) with no value yet for one of the \code{weather.type}s - even if it doesn't
#'   record every type. Use it to fill gaps from several stations.
#' @param units Character, default \code{"metric"}. Added 2026-11-04, per Josh.
#'   Weather Underground gives imperial units; metric columns are converted to
#'   the units Weather Underground itself uses for metric (°C, km/h, hPa, mm,
#'   km). \code{"metric"}: metric columns only. \code{"imperial"}: imperial
#'   only. \code{"both"}: each metric column with its imperial column next to
#'   it. \code{c("metric", "imperial")}: all the metric columns, then all the
#'   imperial ones (in the order given). Columns whose unit doesn't change
#'   (humidity, wind direction, UV, solar, condition) appear once. Column names
#'   are \code{name.standard_suggested.metric} /
#'   \code{name.standard_suggested.imperial} from Josh's
#'   \code{wunderground_headers_20261104.csv} (see Value).
#' @param date.start,date.end Character \code{"YYYY-MM-DD"} (or Date). First and
#'   last day downloaded (station local time).
#'
#' @return Invisibly, a list of two data frames:
#'   \code{$weatherstation.list} - the stations picked, in the same layout as
#'   \code{batz.generate_weatherstation.list()} (\code{$station.total} counts
#'   only picked stations; \code{$sample.interval} is measured over the
#'   downloaded dates), and \code{$weatherstation.data} - their
#'   observations: \code{$site}, \code{$station.name}, \code{$station.code},
#'   \code{$station.area}, \code{$station.latitude}, \code{$station.longitude},
#'   then \code{$date.time} (station local time), \code{$date.time_utc} and one
#'   column per weather type (2026-11-04, named for \code{units}):
#'   \tabular{lll}{
#'   weather.type \tab metric \tab imperial \cr
#'   Temperature \tab \code{temperature.c} (°C) \tab \code{temperature.f} (°F) \cr
#'   Dew Point \tab \code{dewpoint.c} (°C) \tab \code{dewpoint.f} (°F) \cr
#'   Humidity \tab \code{humidity.percent} \tab (same) \cr
#'   Wind \tab \code{wind.direction.degrees} \tab (same) \cr
#'   Wind Speed \tab \code{wind.speed.kmh} (km/h) \tab \code{wind.speed.mph} (mph) \cr
#'   Wind Gust \tab \code{wind.gust.kmh} (km/h) \tab \code{wind.gust.mph} (mph) \cr
#'   Pressure \tab \code{pressure.hpa} (hPa = mbar) \tab \code{pressure.inhg} (inHg) \cr
#'   Precip. \tab \code{precipitation.mm} (mm) \tab \code{precipitation.in} (in) \cr
#'   UV \tab \code{uv.index} \tab (same) \cr
#'   Solar \tab \code{solar.radiation.wm2} (W/m2) \tab (same) \cr
#'   Condition \tab \code{condition} \tab (same) \cr
#'   Visibility \tab \code{visibility.km} (km) \tab \code{visibility.mi} (mi)
#'   }
#'   Visibility isn't in \code{wunderground_headers_20261104.csv}; its names
#'   follow the same pattern. \code{weather.type} still uses the
#'   wunderground.com names (first column).
#'
#' @details
#' \strong{Which stations count.} Stations are tried nearest first. A quick
#' check of the station's current data types (see
#' \code{batz.generate_weatherstation.list()}) skips one that clearly lacks a
#' \code{weather.type} or records less often than \code{sample.interval_min};
#' the rest are downloaded and checked over the requested dates: a type counts
#' when it has at least one value, and the average minutes between records
#' over the dates (\code{$sample.interval}) must be at most
#' \code{sample.interval_min} (2026-11-04, per Josh). Each skipped station is
#' listed in a NOTE with the reason. Airports have no Solar (and seldom UV), so
#' with the default \code{weather.type} only personal weather stations
#' qualify.
#'
#' \strong{What is downloaded.} Personal weather stations: every observation
#' (usually 5-minute) from the wunderground.com history, one API call per
#' station per day - for a PWS, Temperature/Dew Point/Humidity/Wind Speed are
#' the period averages, Wind Gust/UV/Solar the period highs, Pressure the
#' period high and Precip. the day's running total. Airports: every reported
#' observation (usually hourly), one call per station per 31 days. Every API
#' call is spaced 2 seconds apart, so a month for 10 stations takes about 10
#' minutes per site; a station listed for several sites is downloaded once.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WU_API_KEY = "<key>")
#' test.data <- data.frame(site = c("bri", "cumberland"),
#'                         site.latitude  = c(43.687683883908406, 43.809682493532925),
#'                         site.longitude = c(-70.28145797545827, -70.32403651749544))
#' w <- batz.generate_weatherstation.data(test.data, station.max = 3,
#'                                        date.start = "2026-07-01", date.end = "2026-07-31")
#' w$weatherstation.list
#' head(w$weatherstation.data)
#' }
#'
#' @export
batz.generate_weatherstation.data <- function(data,
                                              distance.max = 50,
                                              station.max = 10,
                                              station.type = c("pws", "airport"),
                                              api.key = Sys.getenv("WU_API_KEY"),
                                              query.max = 60,
                                              sample.interval_min = 60,
                                              units = c("metric", "imperial", "both"),
                                              weather.type = c("Temperature", "Dew Point", "Humidity", "Wind", "Wind Speed",
                                                               "Wind Gust", "Pressure", "Precip.", "UV", "Solar"),
                                              weather.patched = FALSE,
                                              date.start = NULL,
                                              date.end = NULL) {
  FN <- "batz.generate_weatherstation.data"
  if (missing(units)) units <- "metric"
  units <- unique(match.arg(units, c("metric", "imperial", "both"), several.ok = TRUE))
  if ("both" %in% units) units <- "both"
  chk <- wu.check.inputs(data, distance.max, station.max, station.type, api.key, FN, sample.interval_min)
  types.all <- c("Temperature", "Dew Point", "Humidity", "Wind", "Wind Speed", "Wind Gust", "Pressure",
                 "Precip.", "UV", "Solar", "Condition", "Visibility")
  wt <- types.all[match(tolower(trimws(weather.type)), tolower(types.all))]
  if (length(weather.type) == 0 || anyNA(wt)) stop(FN, ": unknown weather.type: ",
    paste(weather.type[is.na(wt)], collapse = ", "), " - use any of: ", paste(types.all, collapse = ", "), call. = FALSE)
  weather.type <- unique(wt)
  if (is.null(date.start) || is.null(date.end)) stop(FN, ': give date.start and date.end as "YYYY-MM-DD".', call. = FALSE)
  d0 <- tryCatch(as.Date(date.start), error = function(e) NA); d1 <- tryCatch(as.Date(date.end), error = function(e) NA)
  if (is.na(d0) || is.na(d1)) stop(FN, ': date.start / date.end must be "YYYY-MM-DD".', call. = FALSE)
  if (d1 < d0) stop(FN, ": date.end is before date.start.", call. = FALSE)
  cat(sprintf("%s: %s to %s (%d day(s)); stations must record: %s%s\n", FN, d0, d1, as.numeric(d1 - d0) + 1,
              paste(weather.type, collapse = ", "), if (isTRUE(weather.patched)) " (patched from several stations)" else ""))

  info.cache <- new.env(); data.cache <- new.env()
  lists <- list(); obs <- list()
  for (i in seq_len(nrow(chk$data))) {
    site <- chk$data$site[i]
    cand <- wu.candidates(chk$lat[i], chk$lon[i], chk$station.type, max(chk$distance),
                          if (is.finite(station.max)) station.max * 5 else Inf, api.key, query.max, site)
    picked <- integer(0); skipped <- character(0); covered <- character(0); all.cells <- NULL; full <- FALSE
    for (k in seq_len(nrow(cand))) {
      if (length(picked) >= station.max) break
      if (!is.null(all.cells) && length(setdiff(all.cells, covered)) == 0) {
        cat(sprintf("  %s: every hour already has every weather.type - no more stations needed.\n", site)); full <- TRUE; break
      }
      s <- wu.add.info(cand[k, , drop = FALSE], api.key, info.cache)
      cand[k, names(s)] <- s[1, ]
      slow <- wu.too.slow(s$sample.interval, sample.interval_min)
      if (!is.na(slow)) { skipped <- c(skipped, sprintf("%s (%s)", s$station.code, slow)); next }
      has.now <- trimws(strsplit(s$station.datatypes, ";")[[1]])
      lacks <- setdiff(weather.type, has.now)
      if (identical(s$station.datatypes, "not reporting") ||
          (!isTRUE(weather.patched) && length(lacks) > 0) || (isTRUE(weather.patched) && length(lacks) == length(weather.type))) {
        skipped <- c(skipped, sprintf("%s (%s)", s$station.code,
                                      if (identical(s$station.datatypes, "not reporting")) "not reporting" else paste("no", paste(lacks, collapse = ", "))))
        next
      }
      id <- s$station.id
      if (is.null(data.cache[[id]])) data.cache[[id]] <- wu.download(id, s$station.type, s$station.country, s$station.tz, d0, d1, api.key)
      d <- data.cache[[id]]
      if (!is.null(d)) d <- d[substr(d$date.time, 1, 10) >= format(d0) & substr(d$date.time, 1, 10) <= format(d1), , drop = FALSE]
      if (is.null(d) || nrow(d) == 0) { skipped <- c(skipped, sprintf("%s (no data for the dates)", s$station.code)); next }
      si <- wu.mean.gap(as.numeric(as.POSIXct(d$date.time_utc, tz = "UTC")))
      cand$sample.interval[k] <- si
      slow <- wu.too.slow(si, sample.interval_min)
      if (!is.na(slow)) { skipped <- c(skipped, sprintf("%s (%s over the dates)", s$station.code, slow)); next }
      present <- weather.type[vapply(weather.type, function(t) t %in% names(d) && any(!is.na(d[[t]])), logical(1))]
      if (!isTRUE(weather.patched)) {
        if (length(present) < length(weather.type)) {
          skipped <- c(skipped, sprintf("%s (no %s for the dates)", s$station.code, paste(setdiff(weather.type, present), collapse = ", ")))
          next
        }
      } else {
        hr <- substr(d$date.time, 1, 13)                    # local clock hour
        if (is.null(all.cells)) {                           # every hour of the dates
          h <- as.vector(outer(sprintf("%02d", 0:23), format(seq(d0, d1, by = "day")), function(x, y) paste(y, x)))
          all.cells <- as.vector(outer(h, weather.type, paste))
        }
        cells <- unique(unlist(lapply(present, function(t) paste(hr[!is.na(d[[t]])], t))))
        new <- setdiff(cells, covered)
        if (length(new) == 0) { skipped <- c(skipped, sprintf("%s (adds no new hours)", s$station.code)); next }
        covered <- c(covered, new)
      }
      picked <- c(picked, k)
      obs[[length(obs) + 1]] <- data.frame(site = site, station.name = s$station.name, station.code = s$station.code,
                                           station.area = s$station.area, station.latitude = s$station.latitude,
                                           station.longitude = s$station.longitude, d, check.names = FALSE,
                                           stringsAsFactors = FALSE)
      cat(sprintf("  %s: picked %s, %.1f km (%d observations)\n", site, s$station.code, s$station.distance, nrow(d)))
    }
    if (length(skipped) > 0) cat(sprintf("NOTE: %s - %d station(s) not counted: %s\n", site, length(skipped), paste(skipped, collapse = "; ")))
    if (!full && length(picked) < station.max && is.finite(station.max))
      cat(sprintf("NOTE: %s - only %d of station.max = %d suitable station(s) found within %s km.\n",
                  site, length(picked), station.max, max(chk$distance)))
    lists[[i]] <- wu.rows(site, chk$lat[i], chk$lon[i], cand[picked, , drop = FALSE], chk$distance)
  }
  wl <- do.call(rbind, lists); rownames(wl) <- NULL
  wd <- if (length(obs) > 0) do.call(rbind, obs) else data.frame()
  wd <- wu.units(wd, units)
  rownames(wd) <- NULL
  print(wl[, setdiff(names(wl), c("station.type", "station.id"))], row.names = FALSE)
  cat(sprintf("weatherstation.data: %d observation(s) from %d station(s).\n", nrow(wd), length(unique(wd$station.code))))
  invisible(list(weatherstation.list = wl, weatherstation.data = wd))
}

#' Download one station's observations, d0 to d1, as wunderground.com headers (internal)
#' @noRd
wu.download <- function(id, type, country, tz, d0, d1, api.key) {
  col <- function(x, n) if (is.null(x)) rep(NA, n) else x
  days <- seq(d0, d1, by = "day")
  if (identical(type, "pws")) {
    parts <- list()
    for (dy in as.list(days)) {
      j <- wu.fetch(sprintf("https://api.weather.com/v2/pws/history/all?stationId=%s&format=json&units=e&numericPrecision=decimal&date=%s&apiKey=%s",
                            utils::URLencode(id, reserved = TRUE), format(dy, "%Y%m%d"), api.key))
      o <- j$observations
      if (is.null(o) || NROW(o) == 0) next
      n <- NROW(o); im <- o$imperial
      parts[[length(parts) + 1]] <- data.frame(
        date.time = sub("T", " ", substr(as.character(o$obsTimeLocal), 1, 19)),
        date.time_utc = sub("T", " ", substr(as.character(o$obsTimeUtc), 1, 19)),
        Temperature = col(im$tempAvg, n), `Dew Point` = col(im$dewptAvg, n), Humidity = col(o$humidityAvg, n),
        Wind = col(o$winddirAvg, n), `Wind Speed` = col(im$windspeedAvg, n), `Wind Gust` = col(im$windgustHigh, n),
        Pressure = col(im$pressureMax, n), `Precip.` = col(im$precipTotal, n), UV = col(o$uvHigh, n),
        Solar = col(o$solarRadiationHigh, n), Condition = NA_character_, Visibility = NA_real_,
        check.names = FALSE, stringsAsFactors = FALSE)
    }
    return(if (length(parts) > 0) do.call(rbind, parts) else NULL)
  }
  if (is.na(tz) || !nzchar(tz)) tz <- "UTC"
  parts <- list()
  starts <- seq(d0, d1, by = "31 days")
  for (k in seq_along(starts)) {
    e <- min(d1, starts[k] + 30)
    j <- wu.fetch(sprintf("https://api.weather.com/v1/location/%s:9:%s/observations/historical.json?apiKey=%s&units=e&startDate=%s&endDate=%s",
                          id, if (is.na(country) || !nzchar(country)) "US" else country, api.key,
                          format(starts[k], "%Y%m%d"), format(e, "%Y%m%d")))
    o <- j$observations
    if (is.null(o) || NROW(o) == 0) next
    n <- NROW(o)
    t <- as.POSIXct(as.numeric(o$valid_time_gmt), origin = "1970-01-01", tz = "UTC")
    parts[[length(parts) + 1]] <- data.frame(
      date.time = format(t, "%Y-%m-%d %H:%M:%S", tz = tz), date.time_utc = format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      Temperature = col(o$temp, n), `Dew Point` = col(o$dewPt, n), Humidity = col(o$rh, n),
      Wind = col(o$wdir, n), `Wind Speed` = col(o$wspd, n), `Wind Gust` = col(o$gust, n),
      Pressure = col(o$pressure, n), `Precip.` = col(o$precip_hrly, n), UV = col(o$uv_index, n),
      Solar = NA_real_, Condition = col(o$wx_phrase, n), Visibility = col(o$vis, n),
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (length(parts) > 0) do.call(rbind, parts) else NULL
}

#' Name the weather columns for the units, converting to metric (internal)
#'
#' Names from Josh's wunderground_headers_20261104.csv
#' ($name.standard_suggested.metric / .imperial). Data arrive imperial;
#' metric = the units Weather Underground uses for metric (°C, km/h, hPa, mm, km).
#' Columns with no unit change (humidity, wind direction, UV, solar, condition)
#' appear once.
#' @noRd
wu.units <- function(d, units) {
  if (nrow(d) == 0) return(d)
  num <- function(x) suppressWarnings(as.numeric(x))
  same <- function(x) x
  f2c <- function(x) round((num(x) - 32) * 5 / 9, 2)
  k <- function(f, r) function(x) round(num(x) * f, r)
  map <- list(
    list(wu = "Temperature", metric = "temperature.c",          imperial = "temperature.f",          conv = f2c),
    list(wu = "Dew Point",   metric = "dewpoint.c",             imperial = "dewpoint.f",             conv = f2c),
    list(wu = "Humidity",    metric = "humidity.percent",       imperial = "humidity.percent",       conv = same),
    list(wu = "Wind",        metric = "wind.direction.degrees", imperial = "wind.direction.degrees", conv = same),
    list(wu = "Wind Speed",  metric = "wind.speed.kmh",         imperial = "wind.speed.mph",         conv = k(1.609344, 2)),
    list(wu = "Wind Gust",   metric = "wind.gust.kmh",          imperial = "wind.gust.mph",          conv = k(1.609344, 2)),
    list(wu = "Pressure",    metric = "pressure.hpa",           imperial = "pressure.inhg",          conv = k(33.8639, 1)),
    list(wu = "Precip.",     metric = "precipitation.mm",       imperial = "precipitation.in",       conv = k(25.4, 2)),
    list(wu = "UV",          metric = "uv.index",               imperial = "uv.index",               conv = same),
    list(wu = "Solar",       metric = "solar.radiation.wm2",    imperial = "solar.radiation.wm2",    conv = same),
    list(wu = "Condition",   metric = "condition",              imperial = "condition",              conv = same),
    list(wu = "Visibility",  metric = "visibility.km",          imperial = "visibility.mi",          conv = k(1.609344, 2)))
  base <- d[, setdiff(names(d), vapply(map, `[[`, "", "wu")), drop = FALSE]
  col <- function(m, u) {                                   # one weather column, in unit u
    v <- d[[m$wu]]
    stats::setNames(data.frame(if (u == "metric") m$conv(v) else v, stringsAsFactors = FALSE), m[[u]])
  }
  pieces <- list()
  if (identical(units, "both")) {                           # metric, imperial next to it
    for (m in map) { pieces[[length(pieces) + 1]] <- col(m, "metric")
                     if (m$imperial != m$metric) pieces[[length(pieces) + 1]] <- col(m, "imperial") }
  } else {                                                  # one block per unit, in the order given
    for (u in units) for (m in map) {
      if (u != units[1] && m$imperial == m$metric) next     # unit-free columns only once
      pieces[[length(pieces) + 1]] <- col(m, u)
    }
  }
  cbind(base, do.call(cbind, pieces))
}

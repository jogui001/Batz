#' List the Weather Underground stations near each site, within a distance
#'
#' For each site (latitude/longitude), finds the Weather Underground
#' (\url{https://www.wunderground.com/}) weather stations - personal weather
#' stations (PWS) and airports - within \code{distance.max} km, up to
#' \code{station.max} stations that record at least every
#' \code{sample.interval_min} minutes, and prints one row per station. No weather
#' data is downloaded (see \code{\link{batz.generate_weatherstation.data}}).
#'
#' @param data A data frame with one row per site: \code{$site},
#'   \code{$site.latitude}, \code{$site.longitude} (decimal degrees). Headers
#'   are matched ignoring case and "." / "_" / spaces.
#' @param distance.max Numeric, default \code{50}. How far from the site to
#'   look, in km. Several distances (e.g. \code{c(5, 10, 25, 50)}) also count
#'   the stations within each one.
#' @param station.max Numeric, default \code{10}. Stop once this many stations
#'   (nearest first) are listed for a site. \code{Inf} lists every station out
#'   to \code{max(distance.max)}.
#' @param station.type Character, default \code{c("pws", "airport")}. Which
#'   Weather Underground station types to include.
#' @param api.key Character, default \code{Sys.getenv("WU_API_KEY")}. A
#'   weather.com / Weather Underground API key. Keep it out of scripts that go
#'   to GitHub: set it once per session with
#'   \code{Sys.setenv(WU_API_KEY = "<key>")}.
#' @param query.max Numeric, default \code{60}. Most station-search look-ups per
#'   site and station type (see Details).
#' @param sample.interval_min Numeric, default \code{60}. The longest average
#'   time, in minutes, between a station's records: a station that records less
#'   often (or whose recording rate can't be read, e.g. not reporting) is not
#'   used and not counted (see Details).
#'
#' @return Invisibly, a data frame with one row per station (closest first):
#'   \code{$site}, \code{$site.latitude}, \code{$site.longitude},
#'   \code{$distance.max} (the smallest \code{distance.max} the station falls
#'   within), \code{$station.total} (number of stations within that distance),
#'   \code{$station.distance} (km, 2 decimals), \code{$station.latitude},
#'   \code{$station.longitude}, \code{$station.name}, \code{$station.code},
#'   \code{$station.area}, \code{$sample.interval} (average minutes between
#'   records), \code{$station.datatypes}, \code{$station.type},
#'   \code{$station.id}. A distance with no new stations gets one row with
#'   blank station columns, so every distance's total is shown. All but
#'   \code{$station.type} and \code{$station.id} are printed.
#'
#' @details
#' \strong{Station names.} For a PWS, \code{$station.name} is the name its
#' owner gave it (e.g. "Ocean Avenue Station"), \code{$station.code} is its
#' hardware/software and ID as on the wunderground.com dashboard (e.g. "FB28 -
#' KMEPORTL81"), and \code{$station.area} is the place it is in (e.g.
#' "Portland"). For an airport, \code{$station.name} is the airport name,
#' \code{$station.code} its ICAO code (e.g. "KPWM") and \code{$station.area} is
#' blank. A PWS that isn't currently reporting has no name or hardware to read:
#' \code{$station.code} is its ID.
#'
#' \strong{How stations are found.} The weather.com "location near" service
#' returns only the 10 stations nearest a point. So the area around each site
#' (out to \code{max(distance.max)}, or out to the \code{station.max}-th station)
#' is covered with look-ups: the site first, then the nearest point not yet
#' covered, and so on. A look-up "covers" the circle out to its 10th station,
#' since no closer station can be missing there. Every API call is spaced 2
#' seconds apart (the API allows about 30 per minute).
#'
#' \strong{Dense areas.} Where there are many stations (cities), covering
#' 100 km can take more than \code{query.max} look-ups. The function then stops
#' and prints a NOTE giving the distance up to which the list is complete;
#' totals for distances beyond it are minimums.
#'
#' \strong{Data types.} \code{$station.datatypes} lists what the station
#' recorded, using the wunderground.com table headers: Temperature, Dew Point,
#' Humidity, Wind (direction), Wind Speed, Wind Gust, Pressure, Precip., UV,
#' Solar, Condition, Visibility. A PWS is checked from its current
#' observation: a sensor it doesn't have comes back blank (a reading of 0, e.g.
#' no rain, still counts). An airport is checked over yesterday's
#' observations: a value counts when it was recorded at least once. A station
#' with nothing to report gets \code{"not reporting"}. Each station is looked
#' up once, even when it is listed for several sites.
#'
#' \strong{Recording rate.} \code{$sample.interval} is the average number of
#' minutes between a station's records (time from the first to the last
#' record, divided by the number of gaps), so gaps when the station was off
#' raise it. It is read from the station's last 24 hours of records (a PWS) or
#' yesterday's observations (an airport). Stations are tried nearest first, and
#' one with \code{$sample.interval} above \code{sample.interval_min} (or unknown)
#' is skipped and listed in a NOTE. Most PWS record every 5 minutes; airports
#' about hourly (a little more often when the weather changes).
#'
#' \strong{Coordinates.} A NOTE is printed for a site whose latitude is south
#' of -60 (Antarctica) - usually latitude and longitude swapped. Unreadable or
#' out-of-range coordinates stop the function.
#'
#' @seealso \code{\link{batz.generate_weatherstation.data}}
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WU_API_KEY = "<key>")
#' test.data <- data.frame(site = c("bri", "cumberland"),
#'                         site.latitude  = c(43.687683883908406, 43.809682493532925),
#'                         site.longitude = c(-70.28145797545827, -70.32403651749544))
#' batz.generate_weatherstation.list(test.data, distance.max = c(5, 10, 15, 25, 50))
#' batz.generate_weatherstation.list(test.data, station.max = 25)
#' }
#'
#' @export
batz.generate_weatherstation.list <- function(data,
                                              distance.max = 50,
                                              station.max = 10,
                                              station.type = c("pws", "airport"),
                                              api.key = Sys.getenv("WU_API_KEY"),
                                              query.max = 60,
                                              sample.interval_min = 60) {
  FN <- "batz.generate_weatherstation.list"
  chk <- wu.check.inputs(data, distance.max, station.max, station.type, api.key, FN, sample.interval_min)
  info.cache <- new.env()
  out <- list()
  for (i in seq_len(nrow(chk$data))) {
    site <- chk$data$site[i]
    cand <- wu.candidates(chk$lat[i], chk$lon[i], chk$station.type, max(chk$distance), 
                          if (is.finite(station.max)) station.max * 3 else Inf, api.key, query.max, site)
    keep <- integer(0); skipped <- character(0)
    for (k in seq_len(nrow(cand))) {
      if (length(keep) >= station.max) break
      s <- wu.add.info(cand[k, , drop = FALSE], api.key, info.cache)
      cand[k, names(s)] <- s[1, ]
      why <- wu.too.slow(s$sample.interval, sample.interval_min)
      if (!is.na(why)) { skipped <- c(skipped, sprintf("%s (%s)", s$station.code, why)); next }
      keep <- c(keep, k)
    }
    if (length(skipped) > 0) cat(sprintf("NOTE: %s - %d station(s) not used: %s\n", site, length(skipped), paste(skipped, collapse = "; ")))
    out[[i]] <- wu.rows(site, chk$lat[i], chk$lon[i], cand[keep, , drop = FALSE], chk$distance)
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  print(out[, setdiff(names(out), c("station.type", "station.id"))], row.names = FALSE)
  invisible(out)
}

## ---- shared internal helpers (also used by batz.generate_weatherstation.data) ----

#' Check the shared inputs (internal)
#' @noRd
wu.check.inputs <- function(data, distance.max, station.max, station.type, api.key, FN, sample.interval_min) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop(FN, ' needs the jsonlite package: install.packages("jsonlite")', call. = FALSE)
  if (!is.data.frame(data)) stop(FN, ": data must be a data frame with $site, $site.latitude, $site.longitude.", call. = FALSE)
  ch <- canonicalize.headers(data, c("site", "site.latitude", "site.longitude"))
  if (length(ch$missing) > 0) stop(FN, ": data is missing ", paste0("$", ch$missing, collapse = ", "), call. = FALSE)
  data <- ch$df
  if (!nzchar(api.key)) stop(FN, ': no API key - set one with Sys.setenv(WU_API_KEY = "<key>") or pass api.key.', call. = FALSE)
  distance <- sort(unique(as.numeric(distance.max)))
  if (length(distance) == 0 || any(is.na(distance) | distance <= 0)) stop(FN, ": distance.max must be a positive number (km).", call. = FALSE)
  if (!is.numeric(sample.interval_min) || length(sample.interval_min) != 1 || is.na(sample.interval_min) || sample.interval_min <= 0)
    stop(FN, ": sample.interval_min must be a number of minutes > 0.", call. = FALSE)
  if (!is.numeric(station.max) || length(station.max) != 1 || is.na(station.max) || station.max < 1)
    stop(FN, ": station.max must be a number >= 1 (or Inf).", call. = FALSE)
  station.type <- match.arg(station.type, c("pws", "airport"), several.ok = TRUE)
  lat <- suppressWarnings(as.numeric(data$site.latitude))
  lon <- suppressWarnings(as.numeric(data$site.longitude))
  bad <- is.na(lat) | is.na(lon) | abs(lat) > 90 | abs(lon) > 180
  if (any(bad)) stop(FN, ": unreadable or out-of-range coordinates for site(s): ", paste(data$site[bad], collapse = ", "), call. = FALSE)
  south <- lat < -60
  if (any(south)) cat(sprintf("NOTE: %s - latitude %s is in Antarctica - are $site.latitude and $site.longitude swapped?\n",
                              data$site[south], lat[south]), sep = "")
  list(data = data, lat = lat, lon = lon, distance = distance, station.type = station.type)
}

#' Every API call, spaced 2 seconds apart; NULL when there is no data (internal)
#' @noRd
wu.fetch <- function(url) {
  pause <- getOption("batz.wu.pause", 2)
  last <- getOption("batz.wu.last", 0)
  wait <- pause - (as.numeric(Sys.time()) - last)
  if (wait > 0) Sys.sleep(wait)
  on.exit(options(batz.wu.last = as.numeric(Sys.time())))
  tryCatch(jsonlite::fromJSON(url), error = function(e) {
    if (grepl("401|403", conditionMessage(e))) stop("The API key was refused: ", conditionMessage(e), call. = FALSE)
    NULL                                                    # 204 / 404: no data
  })
}

#' Great-circle distance in km (internal)
#' @noRd
wu.km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  6371.0088 * 2 * asin(pmin(1, sqrt(a)))
}

#' The (up to 10) Weather Underground stations nearest a point (internal)
#' @noRd
wu.near <- function(lat, lon, product, api.key) {
  j <- wu.fetch(sprintf("https://api.weather.com/v3/location/near?geocode=%.5f,%.5f&product=%s&format=json&apiKey=%s",
                        lat, lon, product, api.key))
  loc <- j$location
  if (is.null(loc) || length(loc$latitude) == 0) return(data.frame())
  n <- length(loc$latitude)
  pick <- function(x) if (is.null(x)) rep(NA_character_, n) else as.character(x)
  pws <- product == "pws"
  data.frame(station.id = if (pws) pick(loc$stationId) else pick(loc$icaoCode),
             station.name = if (pws) NA_character_ else pick(loc$airportName),
             station.area = if (pws) pick(loc$stationName) else NA_character_,
             station.type = product,
             station.country = if (is.null(loc$countryCode)) "US" else pick(loc$countryCode),
             station.tz = pick(loc$ianaTimeZone),
             station.latitude = as.numeric(loc$latitude), station.longitude = as.numeric(loc$longitude),
             stringsAsFactors = FALSE)
}

#' Cover the circle around a site with "near" look-ups (internal)
#'
#' The circle is split into square cells (km, centred on the site), nearest
#' first. A cell is done when it lies wholly inside a covered circle (a look-up
#' point out to its farthest returned station). Otherwise its centre is looked
#' up, and if that still doesn't cover it, it is split into four. Returns the
#' stations found and the distance (km) out to which the list is complete.
#' @noRd
wu.cover <- function(lat, lon, product, r.max, station.max, api.key, query.max) {
  kx <- 111.32 * cos(lat * pi / 180); ky <- 111.32
  h0 <- r.max / 4
  ctr <- seq(-r.max + h0, r.max - h0, by = 2 * h0)
  cells <- expand.grid(x = ctr, y = ctr); cells$h <- h0; cells$asked <- FALSE
  discs <- data.frame(x = numeric(0), y = numeric(0), r = numeric(0))
  st <- data.frame(); n <- 0; need <- r.max
  near.edge <- function(c) pmax(0, sqrt(c$x^2 + c$y^2) - c$h * sqrt(2))
  done <- function(c) nrow(discs) > 0 && any(sqrt((discs$x - c$x)^2 + (discs$y - c$y)^2) + c$h * sqrt(2) <= discs$r * 0.98)
  repeat {
    cells <- cells[near.edge(cells) <= need, , drop = FALSE]
    if (nrow(cells) == 0) return(list(stations = st, covered = need))
    cells <- cells[order(near.edge(cells)), , drop = FALSE]
    c <- cells[1, ]
    if (done(c)) { cells <- cells[-1, , drop = FALSE]; next }
    if (!c$asked) {
      if (n >= query.max) return(list(stations = st, covered = min(need, near.edge(c))))
      s <- wu.near(lat + c$y / ky, lon + c$x / kx, product, api.key)
      n <- n + 1
      cells$asked[1] <- TRUE
      if (nrow(s) > 0) {
        sx <- (s$station.longitude - lon) * kx; sy <- (s$station.latitude - lat) * ky
        ## fewer than 10 back = there are no more stations of this type nearby
        rr <- if (nrow(s) < 10) Inf else max(sqrt((sx - c$x)^2 + (sy - c$y)^2))
        discs <- rbind(discs, data.frame(x = c$x, y = c$y, r = rr))
        st <- rbind(st, s); st <- st[!duplicated(st$station.id), , drop = FALSE]
        if (is.finite(station.max) && nrow(st) >= station.max) {
          need <- min(r.max, sort(wu.km(lat, lon, st$station.latitude, st$station.longitude))[station.max])
        }
      } else {
        discs <- rbind(discs, data.frame(x = c$x, y = c$y, r = c$h * sqrt(2) / 0.98))
      }
      next
    }
    h <- c$h / 2
    kids <- data.frame(x = c$x + c(-h, h, -h, h), y = c$y + c(-h, -h, h, h), h = h, asked = FALSE)
    cells <- rbind(kids, cells[-1, , drop = FALSE])
  }
}

#' Stations near one site, closest first, out to r.max (internal)
#' @noRd
wu.candidates <- function(lat, lon, station.type, r.max, station.max, api.key, query.max, site) {
  st <- NULL; covered <- r.max
  for (p in station.type) {
    res <- wu.cover(lat, lon, p, r.max, station.max, api.key, query.max)
    st <- rbind(st, res$stations)
    covered <- min(covered, res$covered)
  }
  if (is.null(st) || nrow(st) == 0) return(data.frame())
  st <- st[!duplicated(st$station.id), , drop = FALSE]
  st$station.distance <- wu.km(lat, lon, st$station.latitude, st$station.longitude)
  st <- st[st$station.distance <= r.max, , drop = FALSE]
  st <- st[order(st$station.distance), , drop = FALSE]
  if (covered < r.max && (nrow(st) < station.max || max(c(0, st$station.distance[seq_len(min(nrow(st), station.max))])) > covered)) {
    cat(sprintf("NOTE: %s - the station list is complete to %.1f km only (query.max = %d reached); totals beyond that are minimums.\n",
                site, covered, query.max))
  }
  rownames(st) <- NULL
  st
}

#' Station name, code and data types, once per station (internal)
#'
#' PWS: from its current observation (owner's name = "neighborhood",
#' hardware = "softwareType"; a missing sensor comes back NULL). Airport: any
#' value over yesterday's observations.
#' @noRd
wu.info <- function(id, type, country, api.key) {
  has <- function(x) !is.null(x) && any(!is.na(x) & nzchar(as.character(x)))
  first <- function(x) if (is.null(x) || all(is.na(x))) NA_character_ else as.character(x[!is.na(x)][1])
  if (identical(type, "pws")) {
    j <- wu.fetch(sprintf("https://api.weather.com/v2/pws/observations/current?stationId=%s&format=json&units=e&numericPrecision=decimal&apiKey=%s",
                          utils::URLencode(id, reserved = TRUE), api.key))
    o <- j$observations
    if (is.null(o) || NROW(o) == 0) return(list(name = NA_character_, code = id, datatypes = "not reporting", sample.interval = NA_real_))
    a <- wu.fetch(sprintf("https://api.weather.com/v2/pws/observations/all/1day?stationId=%s&format=json&units=e&numericPrecision=decimal&apiKey=%s",
                          utils::URLencode(id, reserved = TRUE), api.key))
    si <- wu.mean.gap(a$observations$epoch)
    im <- o$imperial
    keep <- c("Temperature" = has(im$temp), "Dew Point" = has(im$dewpt), "Humidity" = has(o$humidity),
              "Wind" = has(o$winddir), "Wind Speed" = has(im$windSpeed), "Wind Gust" = has(im$windGust),
              "Pressure" = has(im$pressure), "Precip." = has(im$precipRate) || has(im$precipTotal),
              "UV" = has(o$uv), "Solar" = has(o$solarRadiation))
    sw <- first(o$softwareType)
    return(list(name = first(o$neighborhood), code = if (is.na(sw) || !nzchar(sw)) id else paste(sw, "-", id),
                datatypes = if (any(keep)) paste(names(keep)[keep], collapse = "; ") else "not reporting",
                sample.interval = si))
  }
  day <- format(Sys.Date() - 1, "%Y%m%d")
  j <- wu.fetch(sprintf("https://api.weather.com/v1/location/%s:9:%s/observations/historical.json?apiKey=%s&units=e&startDate=%s&endDate=%s",
                        id, if (is.na(country) || !nzchar(country)) "US" else country, api.key, day, day))
  o <- j$observations
  if (is.null(o) || NROW(o) == 0) return(list(name = NA_character_, code = id, datatypes = "not reporting", sample.interval = NA_real_))
  keep <- c("Temperature" = has(o$temp), "Dew Point" = has(o$dewPt), "Humidity" = has(o$rh),
            "Wind" = has(o$wdir_cardinal) || has(o$wdir), "Wind Speed" = has(o$wspd), "Wind Gust" = has(o$gust),
            "Pressure" = has(o$pressure), "Precip." = has(o$precip_hrly) || has(o$precip_total),
            "UV" = has(o$uv_index), "Condition" = has(o$wx_phrase), "Visibility" = has(o$vis))
  list(name = NA_character_, code = id, datatypes = if (any(keep)) paste(names(keep)[keep], collapse = "; ") else "not reporting",
       sample.interval = wu.mean.gap(o$valid_time_gmt))
}

#' Add $station.name / $station.code / $station.datatypes to stations (internal)
#' @noRd
wu.add.info <- function(st, api.key, cache) {
  if (nrow(st) == 0) return(st)
  st$station.code <- st$station.datatypes <- NA_character_
  st$sample.interval <- NA_real_
  for (k in seq_len(nrow(st))) {
    id <- st$station.id[k]
    if (is.null(cache[[id]])) cache[[id]] <- wu.info(id, st$station.type[k], st$station.country[k], api.key)
    inf <- cache[[id]]
    if (!is.na(inf$name)) st$station.name[k] <- inf$name
    st$station.code[k] <- inf$code
    st$station.datatypes[k] <- inf$datatypes
    st$sample.interval[k] <- inf$sample.interval
  }
  st
}

#' One site's output rows, with distance band and running total (internal)
#' @noRd
wu.rows <- function(site, lat, lon, st, distance) {
  cols <- c("station.distance", "station.latitude", "station.longitude", "station.name", "station.code",
            "station.area", "sample.interval", "station.datatypes", "station.type", "station.id")
  if (nrow(st) == 0) st <- as.data.frame(setNames(replicate(length(cols), logical(0), simplify = FALSE), cols))
  if (!"sample.interval" %in% names(st)) st$sample.interval <- NA_real_
  st$band <- vapply(st$station.distance, function(x) distance[which(distance >= x)[1]], numeric(1))
  reached <- if (nrow(st) > 0) max(st$band) else max(distance)
  rows <- list()
  for (k in distance[distance <= reached]) {
    s <- st[st$band %in% k, cols, drop = FALSE]
    if (nrow(s) == 0) { s <- s[NA_integer_, , drop = FALSE] }
    s$band <- k
    s$station.total <- sum(st$station.distance <= k)
    rows[[length(rows) + 1]] <- s
  }
  s <- do.call(rbind, rows)
  data.frame(site = site, site.latitude = lat, site.longitude = lon, distance.max = s$band,
             station.total = s$station.total, station.distance = round(s$station.distance, 2),
             station.latitude = s$station.latitude, station.longitude = s$station.longitude,
             station.name = s$station.name, station.code = s$station.code, station.area = s$station.area,
             sample.interval = round(s$sample.interval, 1),
             station.datatypes = s$station.datatypes, station.type = s$station.type, station.id = s$station.id,
             stringsAsFactors = FALSE)
}

#' Average minutes between records (internal)
#' @noRd
wu.mean.gap <- function(epoch) {
  e <- sort(unique(suppressWarnings(as.numeric(epoch))))
  if (length(e) < 2) return(NA_real_)
  (max(e) - min(e)) / (length(e) - 1) / 60
}

#' Why a station records too seldom, or NA when it is fine (internal)
#' @noRd
wu.too.slow <- function(si, limit) {
  if (is.na(si)) return("recording rate unknown - not reporting")
  if (si > limit) return(sprintf("records every %.0f min on average", si))
  NA_character_
}

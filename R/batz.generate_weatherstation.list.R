#' List the Weather Underground stations near each site, by distance interval
#'
#' For each site (latitude/longitude), finds the Weather Underground
#' (\url{https://www.wunderground.com/}) weather stations - personal weather
#' stations (PWS) and airports - within each distance \code{interval}, or the
#' nearest \code{station.max} stations, and prints one row per station. No
#' weather data is downloaded. Built 2026-11-03, per Josh.
#'
#' @param data A data frame with one row per site: \code{$site},
#'   \code{$site.latitude}, \code{$site.longitude} (decimal degrees). Headers
#'   are matched ignoring case and "." / "_" / spaces.
#' @param interval Numeric, default \code{c(5, 10, 15, 25, 50, 100)}.
#'   Distances from the site, in km, at which stations are counted.
#' @param station.max Numeric, default \code{Inf}. Stop once this many
#'   stations (nearest first) have been listed for a site.
#' @param station.type Character, default \code{c("pws", "airport")}. Which
#'   Weather Underground station types to include.
#' @param api.key Character, default \code{Sys.getenv("WU_API_KEY")}. A
#'   weather.com / Weather Underground API key. Keep it out of scripts that go
#'   to GitHub: set it once per session with
#'   \code{Sys.setenv(WU_API_KEY = "<key>")}.
#' @param query.max Numeric, default \code{60}. Most API look-ups per site and
#'   station type (see Details).
#' @param datatypes Logical, default \code{TRUE}. Added 2026-11-03, per Josh.
#'   Look up which kinds of data each listed station records
#'   (\code{$station.datatypes}, see Details) - one extra look-up per station.
#'
#' @return Invisibly, a data frame with one row per station (closest first):
#'   \code{$site}, \code{$site.latitude}, \code{$site.longitude},
#'   \code{$interval} (the smallest interval the station falls in),
#'   \code{$station.total} (number of stations within that interval),
#'   \code{$station.distance} (km), \code{$station.latitude},
#'   \code{$station.longitude}, plus \code{$station.id}, \code{$station.name}
#'   and \code{$station.type}. An interval with no new stations gets one row
#'   with blank station columns, so every interval's total is shown.
#'   \strong{2026-11-03:} \code{$station.name} and \code{$station.datatypes}
#'   (a "; "-separated list) are printed after the first eight columns.
#'
#' @details
#' \strong{How stations are found.} The weather.com "location near" service
#' returns only the 10 stations nearest a point. So the area around each site
#' (out to \code{max(interval)}, or out to the \code{station.max}-th station)
#' is covered with look-ups: the site first, then the nearest point not yet
#' covered, and so on. A look-up "covers" the circle out to its 10th
#' station, since no closer station can be missing there. Look-ups are
#' spaced 2 seconds apart (the API allows about 30 per minute).
#'
#' \strong{Dense areas.} Where there are many stations (cities), covering
#' 100 km can take more than \code{query.max} look-ups. The function then stops
#' and prints a NOTE giving the distance up to which the list is complete;
#' totals for intervals beyond it are minimums. Raise \code{query.max}, lower
#' \code{interval}, or use \code{station.max} instead.
#'
#' \strong{Data types (2026-11-03, per Josh).} \code{$station.datatypes} lists
#' what the station recorded, using the wunderground.com table headers:
#' Temperature, Dew Point, Humidity, Wind (direction), Wind Speed, Wind Gust,
#' Pressure, Precip., UV, Solar, Condition, Visibility. A personal weather
#' station is checked from its current observation: a sensor it doesn't have
#' comes back blank (a reading of 0, e.g. no rain, still counts). An airport is
#' checked over yesterday's observations: a value counts when it was recorded
#' at least once, so Wind Gust or Precip. can be missing after a calm, dry day.
#' A station with nothing to report gets \code{"not reporting"}. Each station
#' is looked up once, even when it is listed for several sites.
#'
#' \strong{Coordinates.} A NOTE is printed for a site whose latitude is south
#' of -60 (Antarctica) - usually latitude and longitude swapped.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(WU_API_KEY = "<key>")
#' test.data <- data.frame(site = c("bri", "cumberland"),
#'                         site.latitude  = c(43.687683883908406, 43.809682493532925),
#'                         site.longitude = c(-70.28145797545827, -70.32403651749544))
#' batz.generate_weatherstation.list(test.data, interval = c(5, 10, 15, 25, 50, 100))
#' batz.generate_weatherstation.list(test.data, station.max = 25)
#' }
#'
#' @export
batz.generate_weatherstation.list <- function(data,
                                              interval = c(5, 10, 15, 25, 50, 100),
                                              station.max = Inf,
                                              station.type = c("pws", "airport"),
                                              api.key = Sys.getenv("WU_API_KEY"),
                                              query.max = 60,
                                              datatypes = TRUE) {
  FN <- "batz.generate_weatherstation.list"
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop(FN, ' needs the jsonlite package: install.packages("jsonlite")', call. = FALSE)
  if (!is.data.frame(data)) stop(FN, ": data must be a data frame with $site, $site.latitude, $site.longitude.", call. = FALSE)
  ch <- canonicalize.headers(data, c("site", "site.latitude", "site.longitude"))
  if (length(ch$missing) > 0) stop(FN, ": data is missing ", paste0("$", ch$missing, collapse = ", "), call. = FALSE)
  data <- ch$df
  if (!nzchar(api.key)) stop(FN, ': no API key - set one with Sys.setenv(WU_API_KEY = "<key>") or pass api.key.', call. = FALSE)
  interval <- sort(unique(as.numeric(interval)))
  if (length(interval) == 0 || any(is.na(interval) | interval <= 0)) stop(FN, ": interval must be positive numbers (km).", call. = FALSE)
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

  r.max <- max(interval)
  out <- list()
  for (i in seq_len(nrow(data))) {
    st <- NULL; covered <- r.max
    for (p in station.type) {
      res <- wu.cover(lat[i], lon[i], p, r.max, station.max, api.key, query.max)
      st <- rbind(st, res$stations)
      covered <- min(covered, res$covered)
    }
    st <- if (is.null(st)) data.frame() else st[!duplicated(st$station.id), , drop = FALSE]
    if (nrow(st) > 0) {
      st$station.distance <- wu.km(lat[i], lon[i], st$station.latitude, st$station.longitude)
      st <- st[st$station.distance <= r.max, , drop = FALSE]
      st <- st[order(st$station.distance), , drop = FALSE]
      if (nrow(st) > station.max) st <- st[seq_len(station.max), , drop = FALSE]
    }
    if (covered < r.max && (nrow(st) < station.max || max(c(0, st$station.distance)) > covered)) {
      cat(sprintf("NOTE: %s - the list is complete to %.1f km only (query.max = %d reached); totals beyond that are minimums.\n",
                  data$site[i], covered, query.max))
    }
    st$interval <- vapply(st$station.distance, function(x) interval[which(interval >= x)[1]], numeric(1))
    reached <- if (nrow(st) > 0) max(st$interval) else max(interval)
    rows <- list()
    for (k in interval[interval <= reached]) {
      s <- st[st$interval == k, , drop = FALSE]
      tot <- sum(st$station.distance <= k)
      if (nrow(s) == 0) s <- data.frame(station.id = NA, station.name = NA, station.type = NA, station.latitude = NA,
                                        station.longitude = NA, station.distance = NA, interval = k, station.country = NA)
      s$station.total <- tot
      rows[[length(rows) + 1]] <- s
    }
    s <- do.call(rbind, rows)
    out[[i]] <- data.frame(site = data$site[i], site.latitude = lat[i], site.longitude = lon[i],
                           interval = s$interval, station.total = s$station.total,
                           station.distance = round(s$station.distance, 2),
                           station.latitude = s$station.latitude, station.longitude = s$station.longitude,
                           station.id = s$station.id, station.name = s$station.name, station.type = s$station.type,
                           station.country = s$station.country,
                           stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL

  ## what each listed station records (2026-11-03, per Josh) - once per station
  out$station.datatypes <- NA_character_
  if (isTRUE(datatypes)) {
    ids <- unique(out[!is.na(out$station.id), c("station.id", "station.type", "station.country")])
    cat(sprintf("Checking the data types of %d station(s)...\n", nrow(ids)))
    for (j in seq_len(nrow(ids))) {
      if (j > 1) Sys.sleep(2)
      dt <- wu.datatypes(ids$station.id[j], ids$station.type[j], ids$station.country[j], api.key)
      out$station.datatypes[out$station.id %in% ids$station.id[j]] <- dt
    }
  }
  out$station.country <- NULL
  print(out[, c(1:8, match(c("station.name", "station.datatypes"), names(out)))], row.names = FALSE)
  invisible(out)
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
  url <- sprintf("https://api.weather.com/v3/location/near?geocode=%.5f,%.5f&product=%s&format=json&apiKey=%s",
                 lat, lon, product, api.key)
  j <- tryCatch(jsonlite::fromJSON(url), error = function(e) {
    if (grepl("401|403", conditionMessage(e))) stop("The API key was refused: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  loc <- j$location
  if (is.null(loc) || length(loc$latitude) == 0) return(data.frame())
  data.frame(station.id = if (product == "pws") loc$stationId else loc$icaoCode,
             station.name = if (product == "pws") loc$stationName else loc$airportName,
             station.type = product,
             station.country = if (!is.null(loc$countryCode)) loc$countryCode else "US",
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
wu.cover <- function(lat, lon, product, r.max, station.max, api.key, query.max, pause = 2) {
  kx <- 111.32 * cos(lat * pi / 180); ky <- 111.32
  h0 <- r.max / 4                                         # starting cells: 8 x 8 over the circle
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
      if (n > 0) Sys.sleep(pause)
      s <- wu.near(lat + c$y / ky, lon + c$x / kx, product, api.key)
      n <- n + 1
      cells$asked[1] <- TRUE
      if (nrow(s) > 0) {
        sx <- (s$station.longitude - lon) * kx; sy <- (s$station.latitude - lat) * ky
        discs <- rbind(discs, data.frame(x = c$x, y = c$y, r = max(sqrt((sx - c$x)^2 + (sy - c$y)^2))))
        st <- rbind(st, s); st <- st[!duplicated(st$station.id), , drop = FALSE]
        if (is.finite(station.max) && nrow(st) >= station.max) {
          need <- min(r.max, sort(wu.km(lat, lon, st$station.latitude, st$station.longitude))[station.max])
        }
      } else {
        ## nothing returned: treat the cell as empty
        discs <- rbind(discs, data.frame(x = c$x, y = c$y, r = c$h * sqrt(2) / 0.98))
      }
      next
    }
    ## looked up but still not covered: split into four
    h <- c$h / 2
    kids <- data.frame(x = c$x + c(-h, h, -h, h), y = c$y + c(-h, -h, h, h), h = h, asked = FALSE)
    cells <- rbind(kids, cells[-1, , drop = FALSE])
  }
}

#' Which kinds of data a station records (internal)
#'
#' PWS: from its current observation (a missing sensor comes back NULL).
#' Airport: any value over yesterday's observations. Returns a "; "-separated
#' list of wunderground.com table headers, or "not reporting".
#' @noRd
wu.datatypes <- function(id, type, country, api.key,
                         get = function(url) tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)) {
  has <- function(x) !is.null(x) && any(!is.na(x) & nzchar(as.character(x)))
  if (identical(type, "pws")) {
    j <- get(sprintf("https://api.weather.com/v2/pws/observations/current?stationId=%s&format=json&units=e&numericPrecision=decimal&apiKey=%s",
                     utils::URLencode(id, reserved = TRUE), api.key))
    o <- j$observations
    if (is.null(o) || NROW(o) == 0) return("not reporting")
    im <- if (is.data.frame(o$imperial)) o$imperial else as.data.frame(o$imperial)
    keep <- c("Temperature" = has(im$temp), "Dew Point" = has(im$dewpt), "Humidity" = has(o$humidity),
              "Wind" = has(o$winddir), "Wind Speed" = has(im$windSpeed), "Wind Gust" = has(im$windGust),
              "Pressure" = has(im$pressure), "Precip." = has(im$precipRate) || has(im$precipTotal),
              "UV" = has(o$uv), "Solar" = has(o$solarRadiation))
  } else {
    day <- format(Sys.Date() - 1, "%Y%m%d")
    j <- get(sprintf("https://api.weather.com/v1/location/%s:9:%s/observations/historical.json?apiKey=%s&units=e&startDate=%s&endDate=%s",
                     id, if (is.na(country) || !nzchar(country)) "US" else country, api.key, day, day))
    o <- j$observations
    if (is.null(o) || NROW(o) == 0) return("not reporting")
    keep <- c("Temperature" = has(o$temp), "Dew Point" = has(o$dewPt), "Humidity" = has(o$rh),
              "Wind" = has(o$wdir_cardinal) || has(o$wdir), "Wind Speed" = has(o$wspd), "Wind Gust" = has(o$gust),
              "Pressure" = has(o$pressure), "Precip." = has(o$precip_hrly) || has(o$precip_total),
              "UV" = has(o$uv_index), "Condition" = has(o$wx_phrase), "Visibility" = has(o$vis))
  }
  if (!any(keep)) return("not reporting")
  paste(names(keep)[keep], collapse = "; ")
}

#' Download Weather Underground data from the nearest suitable stations
#'
#' For each site, picks the nearest Weather Underground stations that record
#' every \code{weather.type} (or, with \code{weather.patched = TRUE}, that add
#' data the stations already picked are missing) at least every
#' \code{sample.interval_min} minutes, up to \code{station.max} stations
#' within \code{distance.max} km, and downloads their observations
#' from \code{date.start} to \code{date.end}.
#'
#' @inheritParams batz.generate_weatherstation.list
#' @param data A data frame of locations, normally an ARU list
#'   (\verb{*arulist.csv}) with \code{$aru.name}, \code{$aru.label},
#'   \code{$longitude}, \code{$latitude}, \code{$sunregion},
#'   \code{$sunregion.longitude}, \code{$sunregion.latitude}. Which columns
#'   give the locations is set by \code{sunregion} (see Details,
#'   "Locations"). A table with \code{$site}, \code{$site.latitude},
#'   \code{$site.longitude} (one row per site) is also accepted. Headers are
#'   matched ignoring case and "." / "_" / spaces.
#' @param sunregion Logical, default \code{TRUE}. \code{TRUE}: stations are
#'   found for each \code{$sunregion} at its \code{$sunregion.latitude} /
#'   \code{$sunregion.longitude}. \code{FALSE}: for each \code{$aru.name} at
#'   its \code{$latitude} / \code{$longitude}.
#' @param units Character, default \code{"metric"} (when not given). Weather
#'   Underground gives imperial units; metric columns are converted to the
#'   units Weather Underground itself uses for metric (°C, km/h, hPa, mm, km).
#'   \code{"metric"}: metric columns only. \code{"imperial"}: imperial only.
#'   \code{"both"}: each metric column with its imperial column next to it.
#'   \code{c("metric", "imperial")}: all the metric columns, then all the
#'   imperial ones (in the order given). Columns whose unit doesn't change
#'   (humidity, wind direction, UV, solar, condition) appear once. Column names
#'   follow \code{name.standard_suggested.metric} /
#'   \code{name.standard_suggested.imperial} in
#'   \code{wunderground_headers_20261104.csv} (see Value).
#' @param weather.type Character, default \code{c("Temperature", "Dew Point",
#'   "Humidity", "Wind", "Wind Speed", "Wind Gust", "Pressure", "Precip.",
#'   "UV", "Solar")}. The kinds of data required (wunderground.com table
#'   headers, matched ignoring case; \code{"Condition"} and
#'   \code{"Visibility"} - airports only - may also be used). A station that
#'   doesn't record all of them over the dates is not counted (unless
#'   \code{weather.patched = TRUE}). An unknown type stops the function.
#' @param weather.patched Logical, default \code{FALSE}. \code{TRUE}: a station
#'   counts when it adds data the stations already picked don't have - an hour
#'   (station local time) with no value yet for one of the
#'   \code{weather.type}s - even if it doesn't record every type. Use it to fill
#'   gaps from several stations.
#' @param date.start,date.end Character \code{"YYYY-MM-DD"} (or Date), default
#'   \code{NULL} (both are required). First and last day downloaded (station
#'   local time).
#' @param dir.save Character, default \code{getwd()}. Folder the Excel
#'   workbook is saved in (created if needed), named
#'   \verb{<project.name>_weatherdata_<YYYYMMDD>to<YYYYMMDD>_<YYYYMMDDHHMMSS>.xlsx}
#'   (\code{date.start} and \code{date.end}, then the time of the run;
#'   \verb{weatherdata_...} when \code{project.name} is blank). Tabs: see
#'   Value. It is saved also when no data was downloaded (the log says why).
#' @param save.auto Default \code{TRUE}: the files are saved in \code{dir.save}.
#'   \code{FALSE}: no files are saved. \code{"BOTH"}: saved and printed.
#'   \code{"print"}: nothing is saved; each
#'   output (plot or table) is printed instead. \code{"generate <number>"}
#'   (e.g. \code{"generate 5"}): nothing is saved; up to \code{<number>}
#'   outputs are created as objects in the R global environment, each named
#'   after the file it would have been saved as, without the extension and the
#'   time stamp (e.g. \code{acme_vegetation.dbh}); \code{"generate"} alone
#'   creates every output.
#' @param project.name Character, default \code{""}. First part of the file
#'   name (spaces become "-").
#' @param ... The old argument \code{save} is still read as \code{save.auto}
#'   (WARNING); when \code{save.auto} is also given, \code{save} is ignored.
#'
#' @return Invisibly, a list: \code{$weatherstation.list},
#'   \code{$weatherstation.data}, \code{$settings}, \code{$log} and
#'   \code{$file} (the workbook saved, or \code{NA}). The workbook has three
#'   tabs: \code{"Weatherdata"} (\code{$weatherstation.data}),
#'   \code{"settings"} (\code{$settings}: a copy of \code{data} with the
#'   columns \code{$sunregion} (TRUE/FALSE), \code{$distance.max},
#'   \code{$station.max}, \code{$station.type}, \code{$query.max},
#'   \code{$sample.interval_min}, \code{$weather.type},
#'   \code{$weather.patched}, \code{$date.start_weather} and
#'   \code{$date.end_weather} added; lists joined with ";"; the API key is
#'   never written) and \code{"log"} (\code{$log}: one row per location -
#'   \code{$site}, \code{$aru.name}, \code{$aru.label},
#'   \code{$site.latitude}, \code{$site.longitude}, \code{$status}
#'   ("downloaded", "partial" when a \code{weather.type} or a day is
#'   missing, or "no data"), \code{$stations.picked} (code and km),
#'   \code{$stations.skipped} (code and reason), \code{$observations},
#'   \code{$record.first}, \code{$record.last}, \code{$days.no.data}
#'   (count), \code{$dates.no.data} (list), and one
#'   \verb{$hours.with.data_<weather.type>} column per \code{weather.type}:
#'   the percent of the hours from \code{date.start} to \code{date.end}
#'   with at least one value).
#'   \code{$weatherstation.list} - the stations picked, in the same layout as
#'   \code{batz.generate_weatherstation.list()} (\code{$station.total} counts
#'   only picked stations; \code{$sample.interval} is measured over the
#'   downloaded dates), and \code{$weatherstation.data} - their
#'   observations: \code{$site} (every site the record was picked for,
#'   separated by ";", e.g. \code{"Clerk;SugarHill"} - see Details),
#'   \code{$aru.name} and \code{$aru.label} (every ARU of those sites,
#'   separated by ";"),
#'   \code{$station.name}, \code{$station.code},
#'   \code{$station.area}, \code{$station.latitude}, \code{$station.longitude},
#'   then \code{$date.time} (station local time), \code{$date.time_utc} and one
#'   column per weather type (all 12 types, named for \code{units}), with the
#'   precipitation since the previous record after the precipitation column:
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
#'   (from Precip.) \tab \code{precipitation.mm_interval} (mm) \tab \code{precipitation.in_interval} (in) \cr
#'   UV \tab \code{uv.index} \tab (same) \cr
#'   Solar \tab \code{solar.radiation.wm2} (W/m2) \tab (same) \cr
#'   Condition \tab \code{condition} \tab (same) \cr
#'   Visibility \tab \code{visibility.km} (km) \tab \code{visibility.mi} (mi)
#'   }
#'   Visibility isn't in \code{wunderground_headers_20261104.csv}; its names
#'   follow the same pattern. \code{weather.type} uses the wunderground.com
#'   names (first column). The list is printed (without \code{$station.type}
#'   and \code{$station.id}), with the number of observations.
#'
#' @details
#' \strong{Locations.} \code{sunregion = TRUE}: one location per
#' \code{$sunregion}, at its \code{$sunregion.latitude} /
#' \code{$sunregion.longitude} (a sun region with more than one pair gets
#' the central point, as in \code{\link{batz.generate_unique.sunregion}});
#' \code{$site} is the sun region and \code{$aru.name} / \code{$aru.label}
#' list its ARUs. \code{sunregion = FALSE}: one location per
#' \code{$aru.name}, at its \code{$latitude} / \code{$longitude} (an ARU
#' listed with more than one pair: the first, NOTE); \code{$site} is the
#' \code{$aru.name}. When the columns for the chosen setting are missing,
#' the others are used (NOTE): \code{$aru.name} / \code{$latitude} /
#' \code{$longitude}, else \code{$site} / \code{$site.latitude} /
#' \code{$site.longitude}, else the sun-region columns.
#'
#' \strong{Stations shared by several sites.} A station picked for more than
#' one site is downloaded once. Its records (same station and date-time) are
#' listed once in \code{$weatherstation.data}, with all those sites in
#' \code{$site} separated by ";" (in the order of \code{data}), and a NOTE
#' names the shared stations. \code{$weatherstation.list} still has one row
#' per site and station.
#'
#' \strong{Which stations count.} Stations are tried nearest first. A quick
#' check of the station's current data types (see
#' \code{batz.generate_weatherstation.list()}) skips one that clearly lacks a
#' \code{weather.type} (with \code{weather.patched = TRUE}: lacks all of them),
#' is not reporting, or records less often than \code{sample.interval_min}.
#' The rest are downloaded and checked over the requested dates: a type counts
#' when it has at least one value, and the average minutes between records
#' over the dates (\code{$sample.interval}) must be at most
#' \code{sample.interval_min}. Each skipped station is listed in a NOTE with
#' the reason, and a NOTE is printed when fewer than \code{station.max}
#' stations were found. Airports have no Solar (and seldom UV), so with the
#' default \code{weather.type} only personal weather stations qualify.
#'
#' \strong{Patching.} With \code{weather.patched = TRUE}, a station is picked
#' only if it adds at least one new (hour, type) pair. Once every hour of the
#' dates has every \code{weather.type}, no more stations are tried.
#'
#' \strong{What is downloaded.} Personal weather stations: every observation
#' (usually 5-minute) from the wunderground.com history, one API call per
#' station per day - for a PWS, Temperature/Dew Point/Humidity/Wind
#' (direction)/Wind Speed are the period averages, Wind Gust/UV/Solar the
#' period highs, Pressure the period high and Precip. the day's running total.
#' Airports: every reported observation (usually hourly), one call per station
#' per 31 days; Precip. is the hourly amount.
#'
#' \strong{Precipitation per record.} \code{$precipitation.mm_interval} (and
#' \code{$precipitation.in_interval}) is the precipitation since the
#' station's previous record. For a personal weather station it is the
#' running total minus the previous record's total on the same local date
#' (the first record of a date keeps its total; a drop in the total gives 0).
#' For an airport it is the reported hourly amount. Every API call is spaced 2
#' seconds apart, so a month for 10 stations takes about 10 minutes per site;
#' a station listed for several sites is downloaded once.
#'
#' @seealso \code{\link{batz.generate_weatherstation.list}}
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
                                              sunregion = TRUE,
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
                                              date.end = NULL,
                                              dir.save = getwd(), save.auto = TRUE,
                                              project.name = "", ...) {
  FN <- "batz.generate_weatherstation.data"
  dots <- list(...)
  if ("save" %in% c(names(sys.call()), names(dots))) {    # old argument, replaced by save.auto
    old <- if ("save" %in% names(dots)) dots$save else save.auto   # R matches save = to save.auto when save.auto isn't given
    cat("WARNING: ", FN, " - argument 'save' has been replaced by save.auto",
        if ("save" %in% names(dots)) " - save ignored, save.auto used" else sprintf(" - used as save.auto = %s", format(old)), ".\n", sep = "")
    dots$save <- NULL
  }
  if (length(dots)) stop(FN, ": unknown argument(s): ", paste(names(dots), collapse = ", "), call. = FALSE)
  sv <- util_saver(save.auto, FN)   # TRUE save / FALSE / "BOTH" / "print" / "generate <n>"
  if (missing(units)) units <- "metric"
  units <- unique(match.arg(units, c("metric", "imperial", "both"), several.ok = TRUE))
  if ("both" %in% units) units <- "both"
  if (!is.data.frame(data) || !nrow(data)) stop(FN, ": data must be a data frame with rows.", call. = FALSE)
  loc <- wu.locations(data, sunregion, FN)                         # $site / $site.latitude / $site.longitude + ARU lists
  chk <- wu.check.inputs(loc$data, distance.max, station.max, station.type, api.key, FN, sample.interval_min)
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
  lists <- list(); obs <- list(); logs <- list()
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
      d <- wu.precip.interval(d, s$station.type)
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
    logs[[i]] <- data.frame(site = site,
                            stations.picked = paste(sprintf("%s (%.1f km)", cand$station.code[picked], cand$station.distance[picked]), collapse = "; "),
                            stations.skipped = paste(skipped, collapse = "; "),
                            stations.found = nrow(cand), stringsAsFactors = FALSE)
  }
  wl <- do.call(rbind, lists); rownames(wl) <- NULL
  wd <- if (length(obs) > 0) do.call(rbind, obs) else data.frame()
  cover <- wu.coverage(obs, chk$data$site, weather.type, d0, d1)  # per site: days and hours with data
  wd <- wu.merge.sites(wd)                                    # shared stations: one record, sites joined with ";"
  wd <- wu.units(wd, units)
  if (nrow(wd)) {                                             # every ARU of the record's site(s), ";"-separated
    lst <- function(m) vapply(strsplit(as.character(wd$site), ";", fixed = TRUE), function(ss) paste(unique(unlist(m[ss])), collapse = ";"), "")
    wd <- cbind(wd[1], aru.name = lst(loc$aru.name), aru.label = lst(loc$aru.label), wd[-1], stringsAsFactors = FALSE)
  }
  rownames(wd) <- NULL
  ## log: one row per location
  lg <- data.frame(site = chk$data$site,
                   aru.name = vapply(chk$data$site, function(x) paste(loc$aru.name[[x]], collapse = ";"), ""),
                   aru.label = vapply(chk$data$site, function(x) paste(loc$aru.label[[x]], collapse = ";"), ""),
                   site.latitude = chk$lat, site.longitude = chk$lon, stringsAsFactors = FALSE, row.names = NULL)
  lg <- cbind(lg, do.call(rbind, logs)[-1], cover[match(lg$site, cover$site), -1, drop = FALSE])
  hcols <- grep("^hours.with.data_", names(lg), value = TRUE)
  lg$status <- ifelse(lg$observations == 0, "no data",
                      ifelse(lg$days.no.data > 0 | apply(lg[hcols], 1, function(v) any(v == 0)), "partial", "downloaded"))
  lg <- lg[c("site", "aru.name", "aru.label", "site.latitude", "site.longitude", "status", "stations.picked", "stations.skipped",
             "stations.found", "observations", "record.first", "record.last", "days.no.data", "dates.no.data", hcols)]
  rownames(lg) <- NULL
  ## settings: the input with the run's settings
  st <- data
  extra <- list(sunregion = loc$mode == "sunregion", distance.max = paste(distance.max, collapse = ";"), station.max = station.max,
                station.type = paste(chk$station.type, collapse = ";"), query.max = query.max, sample.interval_min = sample.interval_min,
                weather.type = paste(weather.type, collapse = ";"), weather.patched = isTRUE(weather.patched),
                date.start_weather = format(d0), date.end_weather = format(d1))
  if ("sunregion" %in% names(st) && !is.logical(st$sunregion)) {   # the ARU list's own $sunregion column is kept as is
    names(extra)[1] <- "sunregion_setting"
    cat("NOTE: ", FN, " - data has a $sunregion column, so the setting is written as $sunregion_setting in the settings tab.\n", sep = "")
  }
  for (k in names(extra)) st[[k]] <- extra[[k]]
  print(wl[, setdiff(names(wl), c("station.type", "station.id"))], row.names = FALSE)
  cat(sprintf("weatherstation.data: %d observation(s) from %d station(s).\n", nrow(wd), length(unique(wd$station.code))))
  if (nrow(wd) == 0) cat("NOTE: ", FN, " - no weather data downloaded - see the log tab.\n", sep = "")
  print(lg[c("site", "status", "observations", "days.no.data")], row.names = FALSE)
  pj <- if (is.null(project.name) || !length(project.name) || is.na(project.name[1]) || !nzchar(trimws(project.name[1]))) "" else
    paste0(gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(project.name[1]))), "_")
  file <- NA_character_
  if (!is.null(dir.save)) {
    if (sv$save && !dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    file <- file.path(dir.save, sprintf("%sweatherdata_%sto%s_%s.xlsx", pj, format(d0, "%Y%m%d"), format(d1, "%Y%m%d"),
                                        format(Sys.time(), "%Y%m%d%H%M%S")))
    tabs <- list(Weatherdata = wd, settings = st, log = lg)
    file <- sv$out(tabs, file, function(.f) {
      wb <- openxlsx::createWorkbook()
      for (tb in names(tabs)) {
        openxlsx::addWorksheet(wb, tb)
        openxlsx::writeData(wb, tb, tabs[[tb]], headerStyle = openxlsx::createStyle(textDecoration = "bold"))
        openxlsx::freezePane(wb, tb, firstRow = TRUE)
      }
      openxlsx::saveWorkbook(wb, .f, overwrite = TRUE)
    })
  }
  invisible(list(weatherstation.list = wl, weatherstation.data = wd, settings = st, log = lg, file = file))
}

#' One row per station record; sites sharing it joined with ";" (internal)
#'
#' Records with the same \code{$station.code}, \code{$date.time} and
#' \code{$date.time_utc} (a station picked for several sites) are kept once,
#' with \code{$site} = the sites in order of first appearance, separated by
#' ";". Prints a NOTE naming the shared stations.
#' @noRd
wu.merge.sites <- function(wd) {
  if (nrow(wd) == 0 || !all(c("site", "station.code", "date.time", "date.time_utc") %in% names(wd))) return(wd)
  k <- paste(wd$station.code, wd$date.time, wd$date.time_utc, sep = "|")
  if (!anyDuplicated(k)) return(wd)
  f <- factor(k, levels = unique(k))
  joined <- vapply(split(as.character(wd$site), f), function(x) paste(unique(x), collapse = ";"), character(1))
  keep <- !duplicated(k)
  out <- wd[keep, , drop = FALSE]
  out$site <- unname(joined[k[keep]])
  shared <- unique(out[grepl(";", out$site), c("station.code", "site"), drop = FALSE])
  if (nrow(shared) > 0)
    cat(sprintf("NOTE: station %s is shared by sites %s - its records are listed once, $site = \"%s\".\n",
                shared$station.code, gsub(";", ", ", shared$site), shared$site), sep = "")
  rownames(out) <- NULL
  out
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
#' Names from wunderground_headers_20261104.csv
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
    list(wu = "Precip. interval", metric = "precipitation.mm_interval", imperial = "precipitation.in_interval", conv = k(25.4, 2)),
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

#' Precipitation since the previous record (internal)
#'
#' Adds \code{"Precip. interval"} next to \code{"Precip."} (inches, as
#' downloaded). For a personal weather station \code{"Precip."} is the day's
#' running total, so the interval is the total minus the previous record's
#' total on the same local date (the first record of a date keeps its total;
#' a drop in the total gives 0). For an airport \code{"Precip."} is already
#' the amount for the reporting period and is copied. Records are taken in
#' time order; a missing total gives a missing interval.
#' @param d One station's downloaded records (\code{wu.download()} output).
#' @param type \code{"pws"} or \code{"airport"}.
#' @return \code{d} with the new column.
#' @keywords internal
#' @noRd
wu.precip.interval <- function(d, type) {
  if (is.null(d) || nrow(d) == 0 || !"Precip." %in% names(d)) return(d)
  p <- suppressWarnings(as.numeric(d[["Precip."]]))
  if (identical(type, "pws")) {
    o <- order(d$date.time); out <- rep(NA_real_, length(p))
    day <- substr(d$date.time[o], 1, 10); po <- p[o]
    for (dd in unique(day)) {
      i <- which(day == dd); prev <- NA_real_
      for (j in i) {
        if (is.na(po[j])) next
        out[o[j]] <- if (is.na(prev)) po[j] else max(0, po[j] - prev)
        prev <- po[j]
      }
    }
    out <- round(out, 4)
  } else out <- p
  pos <- match("Precip.", names(d))
  d <- cbind(d[seq_len(pos)], `Precip. interval` = out, d[-seq_len(pos)])
  names(d)[pos + 1] <- "Precip. interval"
  d
}


#' $sunregion / $sunregion.latitude / $sunregion.longitude as the site headers (internal)
#'
#' When \code{data} has neither all of \code{$site}, \code{$site.latitude},
#' \code{$site.longitude} nor all of \code{$aru.name}, \code{$latitude},
#' \code{$longitude}, but has all three sun-region headers, they are renamed
#' to \code{$site}, \code{$site.latitude}, \code{$site.longitude}. Headers
#' are matched ignoring case and "." / "_" / spaces.
#' @noRd
wu.sunregion.headers <- function(data) {
  if (!is.data.frame(data)) return(data)
  std <- standardize.headers(names(data))
  has <- function(h) all(standardize.headers(h) %in% std)
  if (has(c("site", "site.latitude", "site.longitude")) || has(c("aru.name", "latitude", "longitude"))) return(data)
  sr <- c(site = "sunregion", site.latitude = "sunregion.latitude", site.longitude = "sunregion.longitude")
  if (!has(sr)) return(data)
  for (h in names(sr)) {
    if (standardize.headers(h) %in% std) names(data)[std == standardize.headers(h)] <- paste0(h, ".old")   # a lone $site etc. set aside
    names(data)[match(standardize.headers(sr[[h]]), std)] <- h
  }
  data
}


#' Locations to find stations for (internal)
#'
#' Returns \code{$data} (\code{$site}, \code{$site.latitude},
#' \code{$site.longitude}), \code{$aru.name} and \code{$aru.label} (named
#' lists, site -> ARU names / labels) and \code{$mode} ("sunregion", "aru"
#' or "site"). See batz.generate_weatherstation.data, Details, "Locations".
#' @noRd
wu.locations <- function(data, sunregion, FN) {
  sr.h <- c("sunregion", "sunregion.latitude", "sunregion.longitude"); aru.h <- c("aru.name", "latitude", "longitude")
  site.h <- c("site", "site.latitude", "site.longitude")
  has <- function(h) !length(canonicalize.headers(data, h)$missing)
  d <- canonicalize.headers(data, unique(c(sr.h, aru.h, site.h, "aru.label")))$df
  mode <- if (isTRUE(sunregion) && has(sr.h)) "sunregion" else if (!isTRUE(sunregion) && has(aru.h)) "aru" else
    if (has(aru.h)) "aru" else if (has(site.h)) "site" else if (has(sr.h)) "sunregion" else NA
  if (is.na(mode)) stop(FN, ": data needs $sunregion / $sunregion.latitude / $sunregion.longitude, $aru.name / $latitude / $longitude, ",
                        "or $site / $site.latitude / $site.longitude.", call. = FALSE)
  if (isTRUE(sunregion) && mode != "sunregion")
    cat(sprintf("NOTE: %s - sunregion = TRUE but data has no $sunregion / $sunregion.latitude / $sunregion.longitude - using %s.\n",
                FN, if (mode == "aru") "$aru.name / $latitude / $longitude" else "$site / $site.latitude / $site.longitude"))
  if (!isTRUE(sunregion) && mode == "site")
    cat(sprintf("NOTE: %s - sunregion = FALSE but data has no $aru.name / $latitude / $longitude - using $site / $site.latitude / $site.longitude.\n", FN))
  grp <- function(key) {
    k <- trimws(as.character(d[[key]]))
    an <- if ("aru.name" %in% names(d)) trimws(as.character(d$aru.name)) else k
    al <- if ("aru.label" %in% names(d)) trimws(as.character(d$aru.label)) else an
    list(aru.name = lapply(split(an, factor(k, levels = unique(k))), function(x) unique(x[!is.na(x) & nzchar(x)])),
         aru.label = lapply(split(al, factor(k, levels = unique(k))), function(x) unique(x[!is.na(x) & nzchar(x)])))
  }
  if (mode == "sunregion") {
    u <- suppressWarnings(utils::capture.output(sr <- batz.generate_unique.sunregion(d, dir.save = NULL)))
    if (length(u)) cat(u, sep = "\n")
    loc <- data.frame(site = sr$sunregion, site.latitude = sr$sunregion.latitude, site.longitude = sr$sunregion.longitude, stringsAsFactors = FALSE)
    g <- grp("sunregion")
  } else if (mode == "aru") {
    k <- trimws(as.character(d$aru.name)); ok <- !is.na(k) & nzchar(k)
    first <- !duplicated(k[ok]); dd <- d[ok, , drop = FALSE]
    two <- unique(k[ok][duplicated(k[ok]) & !duplicated(paste(k[ok], d$latitude[ok], d$longitude[ok]))])
    if (length(two)) cat(sprintf("NOTE: %s - ARU(s) listed with more than one $latitude / $longitude - the first is used: %s\n", FN, paste(two, collapse = ", ")))
    loc <- data.frame(site = k[ok][first], site.latitude = dd$latitude[first], site.longitude = dd$longitude[first], stringsAsFactors = FALSE)
    g <- grp("aru.name")
  } else {
    loc <- data.frame(site = trimws(as.character(d$site)), site.latitude = d$site.latitude, site.longitude = d$site.longitude, stringsAsFactors = FALSE)
    loc <- loc[!duplicated(loc$site), , drop = FALSE]
    g <- grp("site")
  }
  list(data = loc, aru.name = g$aru.name, aru.label = g$aru.label, mode = mode)
}

#' Days and hours with data per site (internal)
#'
#' From the per-station records (before sites are merged): observations,
#' first and last record, the dates from d0 to d1 with no record, and per
#' weather.type the percent of hours with at least one value.
#' @noRd
wu.coverage <- function(obs, sites, weather.type, d0, d1) {
  all.d <- format(seq(d0, d1, by = "day")); n.h <- length(all.d) * 24
  do.call(rbind, lapply(sites, function(s) {
    d <- do.call(rbind, lapply(obs, function(o) if (identical(o$site[1], s)) o[c("date.time", intersect(weather.type, names(o)))] else NULL))
    out <- data.frame(site = s, observations = if (is.null(d)) 0L else nrow(d),
                      record.first = if (is.null(d)) NA_character_ else min(d$date.time),
                      record.last = if (is.null(d)) NA_character_ else max(d$date.time), stringsAsFactors = FALSE)
    miss <- setdiff(all.d, if (is.null(d)) character(0) else unique(substr(d$date.time, 1, 10)))
    out$days.no.data <- length(miss)
    out$dates.no.data <- paste(miss, collapse = ";")
    for (t in weather.type) {
      h <- if (is.null(d) || !t %in% names(d)) character(0) else unique(substr(d$date.time[!is.na(d[[t]])], 1, 13))
      out[[paste0("hours.with.data_", gsub("[^A-Za-z]", "", t))]] <- round(100 * length(h) / n.h, 1)
    }
    out
  }))
}

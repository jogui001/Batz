#' Nightly weather plot: temperature, wind speed and precipitation
#'
#' Draws one panel per night for each weather station in \code{data}, with
#' temperature (red line), wind speed (dark red line) and precipitation
#' (blue bars) on the primary and secondary Y axes as set by \code{stack.y}
#' and \code{stack.y2}. Horizontal black lines mark the cut-offs
#' (\code{temperature} and \code{wind.ms}) and two dashed vertical lines mark
#' the monitoring window (\code{time.start_monitoring} to
#' \code{time.end_monitoring}). Nights can be chosen automatically (weather
#' within the cut-offs for the whole monitoring window), by date, or both.
#' \code{panels} nights go on each PNG.
#'
#' @param data Data frame of weather records, normally
#'   \code{$weatherstation.data} from
#'   \code{\link{batz.generate_weatherstation.data}} (or the same table saved
#'   as a CSV). Required columns: \code{$date.time} (local time),
#'   \code{$station.latitude}, \code{$station.longitude}, a temperature
#'   column (\code{$temperature.c}, or \code{$temperature.f}), a wind speed
#'   column (\code{$wind.speed.ms}, \code{$wind.speed.kmh} or
#'   \code{$wind.speed.mph}) and a precipitation column
#'   (\code{$precipitation.mm} or \code{$precipitation.in}). Stations are told
#'   apart by \code{$station.code} (or \code{$station.name}, or the
#'   coordinates); \code{$site} and \code{$station.code} are used in the file
#'   name when present. Headers are matched ignoring case and "." / "_" /
#'   space differences.
#' @param suntimes Data frame of sun times, normally from
#'   \code{\link{batz.generate_suntimes.arulist}}. Needed when a monitoring
#'   time refers to sunset or sunrise. Columns: a site column
#'   (\code{$aru.name} or \code{$aru}), \code{$date}, latitude
#'   (\code{$latitude} or \code{$lat}), longitude (\code{$longitude} or
#'   \code{$long}), local sunset (\code{$sunset} or \code{$suns}) and local
#'   sunrise (\code{$sunrise.monitoringnight} or \code{$sunr.mon}, otherwise
#'   the next day's \code{$sunrise} or \code{$sunr}), as
#'   \code{"YYYY-MM-DD HH:MM:SS"}. \code{$time.zone} (or \code{$time_zone})
#'   sets the time zone \code{$date.time} is read in. Default \code{NULL}.
#' @param date.start,date.end Date (or text such as \code{"2026-06-25"},
#'   \code{"6/25/2026"}) of the first and last night. Default \code{NULL} (or
#'   blank): the earliest / latest calendar date of \code{$date.time}.
#' @param stack.y Character, default \code{c("Temperature")}. Data drawn on
#'   the primary (left) Y axis: any of \code{"Temperature"}, \code{"Wind"},
#'   \code{"Precipitation"} (case ignored; \code{"temp"} and \code{"precip"}
#'   also work).
#' @param stack.y2 Character, default \code{c("Wind", "Precipitation")}. Data
#'   drawn on the secondary (right) Y axis. Each type can be on one axis
#'   only; a type left out of both is not drawn.
#' @param wind.ms Numeric, default \code{4}. Wind cut-off (m/s). Blank
#'   (\code{""}) means no wind cut-off: no wind line is drawn and wind is not
#'   used to choose nights.
#' @param temperature Numeric, default \code{10}. Temperature cut-off
#'   (degrees C). Blank (\code{""}) means no temperature cut-off, as for
#'   \code{wind.ms}. With both blank, \code{nights.auto = TRUE} keeps every
#'   night.
#' @param wind.ms_buffer Numeric, default \code{10}. Minutes in the
#'   monitoring window that wind may be at or above \code{wind.ms} before the
#'   night no longer counts. Blank = 0.
#' @param temperature.c_buffer Numeric, default \code{10}. Minutes in the
#'   monitoring window that temperature may be at or below
#'   \code{temperature} before the night no longer counts. Blank = 0.
#' @param nights.auto \code{TRUE} (default), \code{FALSE} or \code{"AND"}.
#'   \code{TRUE}: only nights where, between \code{time.start_monitoring}
#'   and \code{time.end_monitoring}, temperature is above \code{temperature}
#'   and wind below \code{wind.ms}, apart from at most
#'   \code{temperature.c_buffer} / \code{wind.ms_buffer} minutes.
#'   \code{FALSE}: the nights in \code{nights.manual} (all nights when it is
#'   blank). \code{"AND"}: nights that pass the \code{TRUE} test and are in
#'   \code{nights.manual}.
#' @param nights.manual Dates of nights to plot, e.g.
#'   \code{c("2026-06-25", "2026-06-27")} or one text value such as
#'   \code{"c(2026-06-25, 2026-06-27)"} or \code{"2026-06-25; 2026-06-27"}.
#'   Default \code{""} (all nights). Used when \code{nights.auto} is
#'   \code{FALSE} or \code{"AND"}.
#' @param panels Numeric, default \code{8}. Nights (panels) on each PNG; more
#'   nights make more PNGs.
#' @param scale.auto Logical, default \code{TRUE}. TRUE lines the two axes
#'   up so \code{temperature} and \code{wind.ms} are at the same height (see
#'   Details). FALSE uses \code{temp.range} and \code{wind.range} exactly as
#'   given.
#' @param time.start_monitoring Character, default \code{"-30 Sunset"}. Start
#'   of the monitoring window (first dashed line): a clock time
#'   (\code{"21:00"}, \code{"21:00:00"}), or minutes before (\code{-}) or
#'   after (\code{+}) \code{Sunset} or \code{Sunrise} (\code{"-30 Sunset"},
#'   \code{"+15 sunrise"}, \code{"Sunset"}).
#' @param time.end_monitoring Character, default \code{"5 hours"}. End of the
#'   monitoring window (second dashed line): a length of time after the start
#'   (\code{"5 hours"}, \code{"five hours"}, \code{"90 minutes"}), a clock
#'   time (\code{"01:00"}), or a time relative to \code{Sunset} /
#'   \code{Sunrise} as above.
#' @param time.start_xaxis,time.end_xaxis Character, defaults \code{"18:00"}
#'   and \code{"08:00"}. First and last time on the X axis; the part of each
#'   night shown.
#' @param time.interval Numeric, default \code{120}. Minutes between X axis
#'   labels.
#' @param temp.range Numeric, default \code{c(0, 25)}. Temperature axis
#'   limits.
#' @param wind.range Numeric, default \code{c(0, 10)}. Wind axis limits. Its
#'   maximum is also the maximum drawn precipitation.
#' @param wind.absent \code{"show"} (default) or \code{"hide"}. \code{"show"}
#'   draws readings with zero wind (the wind line runs along 0);
#'   \code{"hide"} leaves them out, so the wind line is broken where there is
#'   no wind (a lone reading between breaks is drawn as a dot).
#' @param precipitation.absent \code{"hide"} (default) or \code{"show"}.
#'   \code{"hide"} draws no bar for zero precipitation; \code{"show"} draws
#'   zero precipitation as a blue line along 0. When both zero wind and zero
#'   precipitation are shown, zero wind is drawn just above the
#'   zero-precipitation line so both can be seen.
#' @param precip.cumulative Logical, default \code{TRUE}. TRUE when
#'   precipitation is a running total for the day (as Weather Underground
#'   station history is): each reading becomes the amount since the reading
#'   before it (the first reading of a day keeps its value). FALSE uses the
#'   values as they are.
#' @param facet.ncol Numeric, default \code{2}. Panels per row.
#' @param project.name Character, default \code{""}. When not blank, every
#'   saved file name starts with it.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in (created if missing). \code{NULL} saves nothing.
#' @param plot.width Numeric, default \code{9}. PNG width (inches). The
#'   height is 1.9 inches per row of panels plus 1 inch.
#'
#' @details
#' \strong{Axes.} The axes are not fitted to the data: they come from
#' \code{temp.range} (default 0-25 C) and \code{wind.range} (default 0-10
#' m/s, also used for precipitation in mm).
#' \itemize{
#'   \item \code{scale.auto = TRUE} with temperature and wind on different
#'     axes and both cut-offs given: the bottoms of the two axes are level and
#'     \code{temperature} is level with \code{wind.ms}, so one solid black
#'     line crosses both. If \code{temp.range} is changed, the wind axis is
#'     set to match, with its top rounded up to a whole number (to 2 above
#'     10, to 5 above 20) and the temperature top moved to suit; e.g.
#'     \code{temp.range = c(-5, 30)} gives -5 to 32.5 C against 0 to 10
#'     m/s. If only \code{wind.range} is changed, the temperature axis is
#'     set to match. A NOTE gives the ranges used.
#'   \item \code{scale.auto = FALSE}, or a blank cut-off: both ranges are
#'     used as given and each cut-off gets its own line.
#'   \item Temperature and wind on the same axis: one range covering
#'     \code{temp.range} and \code{wind.range}.
#'   \item Where there are two cut-off lines, the first (in \code{stack.y},
#'     then \code{stack.y2}) is solid and the other dotted.
#'   \item Precipitation goes from 0 to the top of the wind axis.
#' }
#'
#' \strong{Values outside the axes} are drawn at the axis limit, so both
#' lines can always be seen. Zero wind and zero precipitation are drawn or
#' left out as set by \code{wind.absent} and \code{precipitation.absent};
#' missing values are never drawn. Precipitation above the maximum is drawn
#' at the maximum. Where readings
#' are missing for more than 30 minutes (or three reading intervals, if
#' longer) the lines are broken.
#'
#' \strong{Nights.} A reading belongs to the night that starts on its date
#' when it falls between \code{time.start_xaxis} and \code{time.end_xaxis}
#' the next morning. For \code{nights.auto} the readings between the two
#' monitoring times are used; a night with no readings there does not pass.
#' Each reading counts for the minutes until the next reading (at most 30);
#' the minutes with temperature at or below \code{temperature}, and with wind
#' at or above \code{wind.ms}, are added up and compared with the buffers.
#' A NOTE lists the nights that pass.
#'
#' \strong{Sun times.} For each station the \code{suntimes} site with the
#' smallest great-circle distance to (\code{$station.latitude},
#' \code{$station.longitude}) is used (a NOTE names it). A night with no sun
#' time for that site has no window lines and does not pass
#' \code{nights.auto}.
#'
#' \strong{Units.} Wind in km/h is divided by 3.6 and in mph multiplied by
#' 0.44704; temperature in F is converted to C; precipitation in inches is
#' multiplied by 25.4.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]weather.daily_<site>_<station.code>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png},
#' with the first and last night on that PNG.
#'
#' @return Invisibly, a list: \code{$plots} (named list, one entry per
#'   station, each a list of ggplot objects, one per PNG), \code{$data} (the
#'   plotted readings, with \code{$night}, \code{$hours} from
#'   \code{time.start_xaxis} and the values as drawn), \code{$nights} (one
#'   row per station and night: \code{$station}, \code{$aru.name},
#'   \code{$night}, \code{$monitoring.start}, \code{$monitoring.end},
#'   \code{$readings}, \code{$temperature.min}, \code{$wind.max},
#'   \code{$minutes.temperature.out}, \code{$minutes.wind.out},
#'   \code{$pass.auto}, \code{$plotted}), \code{$axes} (axis ranges per
#'   station) and \code{$files}.
#'
#' @examples
#' \dontrun{
#' weather <- read.csv("weather.csv")
#' suntimes <- read.csv("MAMM_IN25092_2suntimes.csv")
#' # nights with good weather for the whole window, 8 per PNG
#' batz.plot_weather.daily(weather, suntimes, project.name = "IN25092")
#' # all nights, wind on the left with temperature, fixed axes
#' batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
#'                         stack.y = c("Temperature", "Wind"), stack.y2 = "Precipitation",
#'                         scale.auto = FALSE)
#' # two chosen nights, window 21:00 to 01:00
#' batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
#'                         nights.manual = c("2026-06-25", "2026-06-29"),
#'                         time.start_monitoring = "21:00", time.end_monitoring = "01:00")
#' }
#' @export
batz.plot_weather.daily <- function(data, suntimes = NULL, date.start = NULL, date.end = NULL,
                                    stack.y = c("Temperature"), stack.y2 = c("Wind", "Precipitation"),
                                    wind.ms = 4, temperature = 10,
                                    wind.ms_buffer = 10, temperature.c_buffer = 10,
                                    nights.auto = TRUE, nights.manual = "",
                                    panels = 8, scale.auto = TRUE,
                                    time.start_monitoring = "-30 Sunset", time.end_monitoring = "5 hours",
                                    time.start_xaxis = "18:00", time.end_xaxis = "08:00", time.interval = 120,
                                    temp.range = c(0, 25), wind.range = c(0, 10),
                                    wind.absent = c("show", "hide"), precipitation.absent = c("hide", "show"),
                                    precip.cumulative = TRUE, facet.ncol = 2,
                                    project.name = "", dir.save = getwd(), plot.width = 9) {
  FN <- "batz.plot_weather.daily"
  temp.given <- !missing(temp.range); wind.given <- !missing(wind.range)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop(FN, ' needs the ggplot2 package: install.packages("ggplot2")', call. = FALSE)
  if (!is.data.frame(data) || nrow(data) == 0) stop(FN, ": data must be a data frame with rows.", call. = FALSE)

  key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  pick <- function(df, cands) { k <- key(names(df)); for (cc in cands) { i <- match(key(cc), k); if (!is.na(i)) return(names(df)[i]) }; NA_character_ }
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) || all(!nzchar(trimws(as.character(x))))
  ## cut-offs: blank ("" / NULL / NA) = no cut-off for that variable
  temperature <- if (blank(temperature)) NA_real_ else num(temperature)[1]
  wind.ms <- if (blank(wind.ms)) NA_real_ else num(wind.ms)[1]
  w.buf <- if (blank(wind.ms_buffer)) 0 else num(wind.ms_buffer)[1]
  absent.opt <- function(x, nm) {
    v <- tolower(trimws(as.character(x)[1]))
    if (!v %in% c("show", "hide")) stop(FN, ": ", nm, ' must be "show" or "hide".', call. = FALSE)
    v
  }
  wind.absent <- absent.opt(wind.absent, "wind.absent")
  precipitation.absent <- absent.opt(precipitation.absent, "precipitation.absent")
  t.buf <- if (blank(temperature.c_buffer)) 0 else num(temperature.c_buffer)[1]

  ## ---- stacks ---------------------------------------------------------------
  type.of <- function(x) {
    k <- key(x)
    out <- ifelse(k %in% c("temperature", "temp", "temperaturec"), "Temperature",
           ifelse(k %in% c("wind", "windspeed", "windspeedms"), "Wind",
           ifelse(k %in% c("precipitation", "precip", "rain", "precipitationmm"), "Precipitation", NA_character_)))
    if (any(is.na(out))) stop(FN, ": stack.y / stack.y2 can only hold \"Temperature\", \"Wind\" and \"Precipitation\" (got ",
                               paste0('"', x[is.na(out)], '"', collapse = ", "), ").", call. = FALSE)
    out
  }
  sy <- if (blank(stack.y)) character(0) else unique(type.of(trimws(unlist(strsplit(as.character(stack.y), "[;,]")))))
  sy2 <- if (blank(stack.y2)) character(0) else unique(type.of(trimws(unlist(strsplit(as.character(stack.y2), "[;,]")))))
  if (length(intersect(sy, sy2))) stop(FN, ": ", paste(intersect(sy, sy2), collapse = ", "), " is in both stack.y and stack.y2.", call. = FALSE)
  if (!length(sy)) stop(FN, ": stack.y needs at least one of \"Temperature\", \"Wind\", \"Precipitation\".", call. = FALSE)
  axis.of <- function(v) if (v %in% sy) "y" else if (v %in% sy2) "y2" else NA_character_

  ## ---- nights.auto -------------------------------------------------------------
  na.mode <- toupper(trimws(as.character(nights.auto)[1]))
  if (na.mode %in% c("T", "TRUE")) na.mode <- "TRUE"
  if (na.mode %in% c("F", "FALSE")) na.mode <- "FALSE"
  if (!na.mode %in% c("TRUE", "FALSE", "AND")) stop(FN, ': nights.auto must be TRUE, FALSE or "AND".', call. = FALSE)

  ## ---- time parsers ---------------------------------------------------------
  clock <- function(s) {                         # hours after midnight, or NA
    s <- trimws(as.character(s))
    if (grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", s)) { p <- as.numeric(strsplit(s, ":")[[1]]); return(p[1] + p[2] / 60 + if (length(p) > 2) p[3] / 3600 else 0) }
    if (grepl("^[0-9]{3,4}$", s)) { n <- as.numeric(s); return(n %/% 100 + (n %% 100) / 60) }
    if (grepl("^[0-9]{1,2}$", s)) return(as.numeric(s))
    NA_real_
  }
  sunref <- function(s) {                        # list(ref, offset.min) or NULL
    s <- tolower(gsub("\\s+", " ", trimws(as.character(s))))
    m <- regmatches(s, regexec("^([+-]? ?[0-9.]+)? ?(min|mins|minutes?)? ?(sunset|sunrise) ?(([+-]) ?([0-9.]+))? ?(min|mins|minutes?)?$", s))[[1]]
    if (!length(m)) return(NULL)
    off <- 0
    if (nzchar(m[2])) off <- as.numeric(gsub(" ", "", m[2]))
    if (nzchar(m[5])) off <- off + as.numeric(paste0(m[6], m[7]))
    list(ref = m[4], offset = off)
  }
  words <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9, ten = 10,
             eleven = 11, twelve = 12, half = 0.5)
  duration <- function(s) {                      # minutes, or NA
    s <- tolower(trimws(as.character(s)))
    m <- regmatches(s, regexec("^([0-9.]+|[a-z]+) ?(hours?|hrs?|h|minutes?|mins?|m)$", s))[[1]]
    if (!length(m)) return(NA_real_)
    n <- suppressWarnings(as.numeric(m[2])); if (is.na(n)) n <- unname(words[m[2]])
    if (is.na(n)) return(NA_real_)
    if (substr(m[3], 1, 1) == "h") n * 60 else n
  }
  t.start <- list(clock = clock(time.start_monitoring), sun = sunref(time.start_monitoring))
  t.end <- list(clock = clock(time.end_monitoring), sun = sunref(time.end_monitoring), dur = duration(time.end_monitoring))
  if (is.na(t.start$clock) && is.null(t.start$sun))
    stop(FN, ': time.start_monitoring must be a clock time ("21:00") or minutes from Sunset / Sunrise ("-30 Sunset"); got "', time.start_monitoring, '".', call. = FALSE)
  if (is.na(t.end$clock) && is.null(t.end$sun) && is.na(t.end$dur))
    stop(FN, ': time.end_monitoring must be a length of time ("5 hours"), a clock time ("01:00") or minutes from Sunset / Sunrise; got "', time.end_monitoring, '".', call. = FALSE)
  xs <- clock(time.start_xaxis); xe <- clock(time.end_xaxis)
  if (is.na(xs)) stop(FN, ': time.start_xaxis must be a clock time such as "18:00"; got "', time.start_xaxis, '".', call. = FALSE)
  if (is.na(xe)) stop(FN, ': time.end_xaxis must be a clock time such as "08:00"; got "', time.end_xaxis, '".', call. = FALSE)
  span <- (xe - xs) %% 24; if (span == 0) span <- 24
  need.sun <- !is.null(t.start$sun) || !is.null(t.end$sun)

  ## ---- data columns --------------------------------------------------------
  col <- list(dt = pick(data, "date.time"), lat = pick(data, "station.latitude"), lon = pick(data, "station.longitude"),
              tc = pick(data, "temperature.c"), tf = pick(data, "temperature.f"),
              wms = pick(data, c("wind.speed.ms", "wind.speed.m.s")), wkmh = pick(data, "wind.speed.kmh"), wmph = pick(data, "wind.speed.mph"),
              pmm = pick(data, "precipitation.mm"), pin = pick(data, "precipitation.in"),
              code = pick(data, "station.code"), name = pick(data, "station.name"), site = pick(data, "site"))
  miss <- c(if (is.na(col$dt)) "$date.time", if (is.na(col$lat)) "$station.latitude", if (is.na(col$lon)) "$station.longitude",
            if (is.na(col$tc) && is.na(col$tf)) "$temperature.c (or $temperature.f)",
            if (all(is.na(c(col$wms, col$wkmh, col$wmph)))) "$wind.speed.ms (or $wind.speed.kmh / $wind.speed.mph)",
            if (is.na(col$pmm) && is.na(col$pin)) "$precipitation.mm (or $precipitation.in)")
  if (length(miss)) stop(FN, ": data is missing ", paste(miss, collapse = ", "), call. = FALSE)
  w <- data.frame(dt.text = as.character(data[[col$dt]]), lat = num(data[[col$lat]]), lon = num(data[[col$lon]]), stringsAsFactors = FALSE)
  w$temperature.c <- if (!is.na(col$tc)) num(data[[col$tc]]) else (num(data[[col$tf]]) - 32) * 5 / 9
  w$wind.speed.ms <- if (!is.na(col$wms)) num(data[[col$wms]]) else if (!is.na(col$wkmh)) num(data[[col$wkmh]]) / 3.6 else num(data[[col$wmph]]) * 0.44704
  w$precipitation.raw <- if (!is.na(col$pmm)) num(data[[col$pmm]]) else num(data[[col$pin]]) * 25.4
  w$site <- if (!is.na(col$site)) as.character(data[[col$site]]) else ""
  w$station.name <- if (!is.na(col$name)) as.character(data[[col$name]]) else ""
  w$station.code <- if (!is.na(col$code)) as.character(data[[col$code]]) else ""
  w$station <- if (!is.na(col$code)) w$station.code else if (!is.na(col$name)) w$station.name else paste(w$lat, w$lon)

  ## ---- suntimes ---------------------------------------------------------------
  st <- NULL
  if (is.data.frame(suntimes) && nrow(suntimes) > 0) {
    sc <- list(aru = pick(suntimes, c("aru.name", "aru")), date = pick(suntimes, "date"),
               lat = pick(suntimes, c("latitude", "lat", "site.latitude")), lon = pick(suntimes, c("longitude", "long", "lon", "site.longitude")),
               sunset = pick(suntimes, c("sunset", "suns")), sunrise.mon = pick(suntimes, c("sunrise.monitoringnight", "sunr.mon")),
               sunrise = pick(suntimes, c("sunrise", "sunr")), tz = pick(suntimes, c("time.zone", "time_zone")))
    miss <- c(if (is.na(sc$aru)) "$aru.name (or $aru)", if (is.na(sc$date)) "$date", if (is.na(sc$lat)) "$latitude (or $lat)",
              if (is.na(sc$lon)) "$longitude (or $long)")
    if (length(miss)) stop(FN, ": suntimes is missing ", paste(miss, collapse = ", "), call. = FALSE)
    g <- function(cn) if (!is.na(cn)) as.character(suntimes[[cn]]) else rep(NA_character_, nrow(suntimes))
    st <- data.frame(aru = g(sc$aru), date = g(sc$date), lat = num(suntimes[[sc$lat]]), lon = num(suntimes[[sc$lon]]),
                     sunset = g(sc$sunset), sunrise.mon = g(sc$sunrise.mon), sunrise = g(sc$sunrise),
                     tz = if (!is.na(sc$tz)) as.character(suntimes[[sc$tz]]) else "", stringsAsFactors = FALSE)
  }
  if (need.sun && is.null(st)) stop(FN, ": a monitoring time refers to Sunset / Sunrise, so suntimes is needed.", call. = FALSE)

  parse.date <- function(x) {
    x <- trimws(as.character(x)); out <- as.Date(rep(NA_character_, length(x)))
    for (f in c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d", "%d-%b-%Y")) {
      todo <- is.na(out) & nzchar(x); if (!any(todo)) break
      out[todo] <- as.Date(sub(" .*$", "", x[todo]), format = f)
    }
    out
  }
  parse.dt <- function(x, tz) {
    x <- trimws(as.character(x)); out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = tz)
    for (f in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M")) {
      todo <- is.na(out) & !is.na(x) & nzchar(x); if (!any(todo)) break
      out[todo] <- as.POSIXct(x[todo], format = f, tz = tz)
    }
    out
  }
  manual <- if (blank(nights.manual)) as.Date(character(0)) else {
    tx <- unlist(regmatches(paste(as.character(nights.manual), collapse = " "),
                            gregexpr("[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}|[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", paste(as.character(nights.manual), collapse = " "))))
    d <- parse.date(tx)
    if (!length(d) || any(is.na(d))) stop(FN, ': nights.manual dates could not be read - use "YYYY-MM-DD" (got "', paste(nights.manual, collapse = ", "), '").', call. = FALSE)
    sort(unique(d))
  }
  if (na.mode == "TRUE" && length(manual)) cat(sprintf('NOTE: %s - nights.auto = TRUE, so nights.manual is not used (use nights.auto = "AND" to use both).\n', FN))


  plots <- list(); all.data <- list(); all.nights <- list(); all.axes <- list(); files <- character(0)
  stations <- unique(w$station[!is.na(w$lat) & !is.na(w$lon)])
  if (length(stations) == 0) stop(FN, ": no rows with $station.latitude and $station.longitude.", call. = FALSE)

  for (s in stations) {
    ws <- w[w$station == s, , drop = FALSE]
    lab <- if (nzchar(ws$station.name[1])) sprintf("%s (%s)", ws$station.name[1], s) else s
    ## nearest suntimes site
    near <- NA_character_; sn <- NULL; tz <- ""
    if (!is.null(st)) {
      lat0 <- stats::median(ws$lat, na.rm = TRUE); lon0 <- stats::median(ws$lon, na.rm = TRUE)
      sites <- unique(st[!is.na(st$lat) & !is.na(st$lon), c("aru", "lat", "lon")]); sites <- sites[!duplicated(sites$aru), , drop = FALSE]
      d.km <- 6371.0088 * 2 * asin(pmin(1, sqrt(sin((sites$lat - lat0) * pi / 360)^2 +
                cos(lat0 * pi / 180) * cos(sites$lat * pi / 180) * sin((sites$lon - lon0) * pi / 360)^2)))
      near <- sites$aru[which.min(d.km)]; sn <- st[st$aru == near, , drop = FALSE]
      tz <- sn$tz[!is.na(sn$tz) & nzchar(sn$tz)][1]
      cat(sprintf("NOTE: %s - station %s: sun times from suntimes site '%s' (%.2f km away).\n", FN, lab, near, min(d.km)))
    }
    if (is.na(tz) || !nzchar(tz)) tz <- Sys.timezone()

    ## times
    ws$t <- parse.dt(ws$dt.text, tz)
    if (any(is.na(ws$t))) cat(sprintf("NOTE: %s - station %s: %d row(s) with an unreadable $date.time dropped.\n", FN, lab, sum(is.na(ws$t))))
    ws <- ws[!is.na(ws$t), , drop = FALSE]; ws <- ws[order(ws$t), , drop = FALSE]
    if (isTRUE(precip.cumulative)) {
      ws$precipitation.mm <- stats::ave(ws$precipitation.raw, format(ws$t, "%Y-%m-%d"), FUN = function(p) {
        ok <- !is.na(p); if (any(ok)) p[ok] <- pmax(0, c(p[ok][1], diff(p[ok]))); p })
    } else ws$precipitation.mm <- ws$precipitation.raw
    cal <- as.Date(format(ws$t, "%Y-%m-%d"))
    ds <- if (blank(date.start)) min(cal) else parse.date(date.start)
    de <- if (blank(date.end)) max(cal) else parse.date(date.end)
    if (is.na(ds) || is.na(de)) stop(FN, ": date.start / date.end could not be read as dates.", call. = FALSE)

    ## night of each reading on the X axis
    hr <- as.numeric(format(ws$t, "%H")) + as.numeric(format(ws$t, "%M")) / 60
    ws$night <- as.Date(ifelse(hr >= xs, cal, cal - 1), origin = "1970-01-01")
    base.of <- function(n) as.POSIXct(paste(format(n), sprintf("%02d:%02d", floor(xs), round((xs %% 1) * 60))), format = "%Y-%m-%d %H:%M", tz = tz)
    ws$hours <- as.numeric(difftime(ws$t, base.of(ws$night), units = "hours"))
    shown <- ws$hours >= 0 & ws$hours <= span

    ## monitoring window for every night
    nights.all <- sort(unique(ws$night[shown & ws$night >= ds & ws$night <= de]))
    if (!length(nights.all)) { cat(sprintf("NOTE: %s - station %s has no readings between %s and %s - no plot.\n", FN, lab, ds, de)); next }
    sun.time <- function(n, ref) {
      if (is.null(sn)) return(as.POSIXct(NA))
      if (ref == "sunset") { v <- sn$sunset[parse.date(sn$date) == n][1] }
      else { v <- sn$sunrise.mon[parse.date(sn$date) == n][1]
             if (is.na(v) || !nzchar(v)) v <- sn$sunrise[parse.date(sn$date) == n + 1][1] }
      if (is.na(v) || !nzchar(v)) as.POSIXct(NA) else parse.dt(v, tz)
    }
    at.clock <- function(n, h) { d <- if (h >= 12) n else n + 1
      as.POSIXct(paste(format(d), sprintf("%02d:%02d:%02d", floor(h), floor((h %% 1) * 60), round(((h * 60) %% 1) * 60))), format = "%Y-%m-%d %H:%M:%S", tz = tz) }
    win <- do.call(rbind, lapply(nights.all, function(n) {
      w0 <- if (!is.null(t.start$sun)) sun.time(n, t.start$sun$ref) + t.start$sun$offset * 60 else at.clock(n, t.start$clock)
      w1 <- if (!is.na(t.end$dur)) w0 + t.end$dur * 60 else if (!is.null(t.end$sun)) sun.time(n, t.end$sun$ref) + t.end$sun$offset * 60 else at.clock(n, t.end$clock)
      if (!is.na(w0) && !is.na(w1) && w1 <= w0) w1 <- w1 + 86400
      inw <- !is.na(w0) & !is.na(w1) & ws$t >= w0 & ws$t <= w1
      tv <- ws$temperature.c[inw]; wv <- ws$wind.speed.ms[inw]
      ## minutes each reading stands for: time to the next reading (the last uses the usual interval), at most 30 min
      tt <- ws$t[inw]
      dur <- if (length(tt)) { d <- c(as.numeric(diff(tt), units = "mins"), NA); med <- stats::median(d, na.rm = TRUE)
                               if (!is.finite(med)) med <- 5; d[is.na(d)] <- med; pmin(d, max(30, med)) } else numeric(0)
      t.out <- if (is.na(temperature)) 0 else sum(dur[!is.na(tv) & tv <= temperature])
      w.out <- if (is.na(wind.ms)) 0 else sum(dur[!is.na(wv) & wv >= wind.ms])
      t.ok <- is.na(temperature) || (any(!is.na(tv)) && t.out <= t.buf)
      w.ok <- is.na(wind.ms) || (any(!is.na(wv)) && w.out <= w.buf)
      pass <- if (is.na(temperature) && is.na(wind.ms)) TRUE else sum(inw) > 0 && t.ok && w.ok
      data.frame(station = s, aru.name = near, night = n,
                 monitoring.start = format(w0, "%Y-%m-%d %H:%M:%S"), monitoring.end = format(w1, "%Y-%m-%d %H:%M:%S"),
                 readings = sum(inw),
                 temperature.min = if (any(!is.na(tv))) min(tv, na.rm = TRUE) else NA_real_,
                 wind.max = if (any(!is.na(wv))) max(wv, na.rm = TRUE) else NA_real_,
                 minutes.temperature.out = t.out, minutes.wind.out = w.out,
                 pass.auto = pass,
                 x0 = as.numeric(difftime(w0, base.of(n), units = "hours")), x1 = as.numeric(difftime(w1, base.of(n), units = "hours")),
                 stringsAsFactors = FALSE)
    }))
    if (any(is.na(win$x0))) cat(sprintf("NOTE: %s - station %s: no sun time for %s at site '%s' - no window lines, and the night does not pass nights.auto.\n",
                                       FN, lab, paste(format(win$night[is.na(win$x0)]), collapse = ", "), near))
    keep <- switch(na.mode,
                   "TRUE" = win$pass.auto,
                   "FALSE" = if (length(manual)) win$night %in% manual else rep(TRUE, nrow(win)),
                   "AND" = win$pass.auto & (if (length(manual)) win$night %in% manual else TRUE))
    win$plotted <- keep
    if (na.mode != "FALSE") {
      crit <- c(if (!is.na(temperature)) sprintf("temperature above %s C (up to %s min outside)", temperature, t.buf),
                if (!is.na(wind.ms)) sprintf("wind below %s m/s (up to %s min outside)", wind.ms, w.buf))
      if (!length(crit)) cat(sprintf("NOTE: %s - station %s: no cut-offs (temperature and wind.ms blank) - every night passes nights.auto.\n", FN, lab))
      else cat(sprintf("NOTE: %s - station %s: %d of %d night(s) have %s for the whole monitoring window: %s\n",
                       FN, lab, sum(win$pass.auto), nrow(win), paste(crit, collapse = " and "),
                       if (any(win$pass.auto)) paste(format(win$night[win$pass.auto]), collapse = ", ") else "none"))
    }
    if (length(manual) && na.mode != "TRUE" && any(!manual %in% win$night))
      cat(sprintf("NOTE: %s - station %s: nights.manual date(s) with no readings: %s\n", FN, lab, paste(format(manual[!manual %in% win$night]), collapse = ", ")))
    all.nights[[s]] <- win[c("station", "aru.name", "night", "monitoring.start", "monitoring.end", "readings",
                             "temperature.min", "wind.max", "minutes.temperature.out", "minutes.wind.out", "pass.auto", "plotted")]
    nights <- win$night[keep]
    if (!length(nights)) { cat(sprintf("NOTE: %s - station %s: no nights to plot.\n", FN, lab)); next }
    ws <- ws[shown & ws$night %in% nights, , drop = FALSE]

    ## ---- axis ranges -------------------------------------------------------
    has <- c(Temperature = !is.na(axis.of("Temperature")), Wind = !is.na(axis.of("Wind")), Precipitation = !is.na(axis.of("Precipitation")))
    split.axes <- has["Temperature"] && has["Wind"] && axis.of("Temperature") != axis.of("Wind")
    ## scale.auto: the cut-offs are lined up (temp.range min level with wind.range min, temperature level with wind.ms)
    aligned <- split.axes && isTRUE(scale.auto) && !is.na(temperature) && !is.na(wind.ms)
    tr <- as.numeric(temp.range); wr <- as.numeric(wind.range)
    if (aligned) {
      if (temperature <= tr[1]) stop(FN, ": temperature (", temperature, ") must be above the bottom of temp.range (", tr[1], ").", call. = FALSE)
      if (wind.ms <= wr[1]) stop(FN, ": wind.ms (", wind.ms, ") must be above the bottom of wind.range (", wr[1], ").", call. = FALSE)
      k <- (temperature - tr[1]) / (wind.ms - wr[1])           # degrees C per m/s
      if (wind.given && !temp.given) {
        tr <- c(tr[1], tr[1] + k * (wr[2] - wr[1]))
      } else {
        need <- (tr[2] - tr[1]) / k                            # wind span that reaches the top of temp.range
        step <- if (need <= 10) 1 else if (need <= 20) 2 else 5
        wr <- c(wr[1], wr[1] + step * ceiling(need / step - 1e-9))
        tr <- c(tr[1], tr[1] + k * (wr[2] - wr[1]))
        if (wind.given && temp.given) cat(sprintf("NOTE: %s - temp.range is kept and wind.range is changed so %s C lines up with %s m/s.\n", FN, temperature, wind.ms))
      }
      if (temp.given || wind.given)
        cat(sprintf("NOTE: %s - axes: temperature %s to %s C, wind and precipitation %s to %s (%s C level with %s m/s).\n", FN,
                    format(round(tr[1], 2)), format(round(tr[2], 2)), format(round(wr[1], 2)), format(round(wr[2], 2)), temperature, wind.ms))
    }
    pr <- c(0, wr[2])
    var.range <- list(Temperature = tr, Wind = wr, Precipitation = pr)
    axis.range <- function(a) {
      v <- if (a == "y") sy else sy2
      if (!length(v)) return(NULL)
      if (split.axes) {                          # the temperature axis and the wind axis keep their own ranges
        if ("Temperature" %in% v) return(tr)
        if ("Wind" %in% v) return(wr)
      }
      range(unlist(var.range[v]))
    }
    ry <- axis.range("y"); ry2 <- axis.range("y2")
    to.y <- function(v, a) if (a == "y") v else ry[1] + (v - ry2[1]) * diff(ry) / diff(ry2)
    clampv <- function(v, a) { r <- if (a == "y") ry else ry2; pmin(pmax(v, r[1]), r[2]) }
    all.axes[[s]] <- data.frame(station = s, axis = c("y", if (length(sy2)) "y2"),
                                contents = c(paste(sy, collapse = "; "), if (length(sy2)) paste(sy2, collapse = "; ")),
                                min = c(ry[1], if (length(sy2)) ry2[1]), max = c(ry[2], if (length(sy2)) ry2[2]), stringsAsFactors = FALSE)

    ## values as drawn (in primary-axis units)
    ws$temp.plot <- if (has["Temperature"]) to.y(clampv(ws$temperature.c, axis.of("Temperature")), axis.of("Temperature")) else NA
    ws$wind.plot <- if (has["Wind"]) to.y(clampv(ws$wind.speed.ms, axis.of("Wind")), axis.of("Wind")) else NA
    p <- ws$precipitation.mm; p[!is.na(p) & p <= 0] <- NA
    ws$precip.plot <- if (has["Precipitation"]) to.y(clampv(pmin(p, pr[2]), axis.of("Precipitation")), axis.of("Precipitation")) else NA
    ws$night.label <- format(ws$night)
    bar.w <- stats::median(diff(sort(unique(ws$hours))), na.rm = TRUE); if (!is.finite(bar.w)) bar.w <- 5 / 60
    ws <- ws[order(ws$night, ws$hours), , drop = FALSE]
    ws$grp <- paste(ws$night.label, stats::ave(ws$hours, ws$night.label, FUN = function(h) cumsum(c(TRUE, diff(h) > max(3 * bar.w, 0.5)))))

    ## zero wind / zero precipitation ("absent")
    z.p <- if (has["Precipitation"]) to.y(clampv(0, axis.of("Precipitation")), axis.of("Precipitation")) else NA
    w0 <- !is.na(ws$wind.speed.ms) & ws$wind.speed.ms <= 0
    if (has["Wind"] && wind.absent == "hide") ws$wind.plot[w0] <- NA
    ws$precip.zero <- if (has["Precipitation"] && precipitation.absent == "show")
      ifelse(!is.na(ws$precipitation.mm) & ws$precipitation.mm <= 0, z.p, NA) else NA
    if (has["Wind"] && has["Precipitation"] && wind.absent == "show" && precipitation.absent == "show") {
      lift <- 0.015 * diff(ry)                     # zero wind drawn just above the zero-precipitation line
      low <- !is.na(ws$wind.plot) & ws$wind.plot >= z.p - 1e-9 & ws$wind.plot < z.p + lift
      ws$wind.plot[low] <- z.p + lift
    }
    ws$wgrp <- paste(ws$grp, stats::ave(as.numeric(is.na(ws$wind.plot)), ws$grp, FUN = cumsum))

    ## cut-off lines
    cut.lines <- NULL
    cuts <- c(Temperature = temperature, Wind = wind.ms); cuts <- cuts[!is.na(cuts) & names(cuts) %in% c(sy, sy2)]
    if (aligned) cut.lines <- data.frame(y = to.y(temperature, axis.of("Temperature")), lt = "solid")
    else {                                       # first cut-off solid, any other dotted
      for (a in c("y", "y2")) {
        v <- intersect(if (a == "y") sy else sy2, names(cuts)); if (!length(v)) next
        cut.lines <- rbind(cut.lines, data.frame(y = to.y(cuts[v], a), lt = "dotted"))
      }
      if (!is.null(cut.lines)) cut.lines$lt[1] <- "solid"
    }
    if (!is.null(cut.lines)) cut.lines <- cut.lines[!duplicated(round(cut.lines$y, 9)), , drop = FALSE]

    ## axis titles and breaks
    unit.lab <- c(Temperature = "Temperature (°C)", Wind = "Wind speed (m/s)", Precipitation = "Precipitation (mm)")
    title.of <- function(v) { l <- unit.lab[v]; if (length(l) > 1) l[-1] <- paste0(tolower(substr(l[-1], 1, 1)), substring(l[-1], 2)); paste(l, collapse = " and ") }
    brks <- function(r) { b <- pretty(r, n = 6); b[b >= r[1] - 1e-9 & b <= r[2] + 1e-9] }
    xb <- seq(0, span, by = time.interval / 60)
    xl <- format(as.POSIXct("2000-01-01 00:00", tz = "UTC") + (xs + xb) * 3600, "%H:%M")
    sec <- if (length(sy2)) ggplot2::sec_axis(~ ry2[1] + (. - ry[1]) * diff(ry2) / diff(ry), name = title.of(sy2), breaks = brks(ry2)) else ggplot2::waiver()

    vl <- rbind(data.frame(night.label = format(win$night), x = win$x0), data.frame(night.label = format(win$night), x = win$x1))
    vl <- vl[!is.na(vl$x) & vl$night.label %in% format(nights), , drop = FALSE]

    chunks <- split(nights, ceiling(seq_along(nights) / max(1, panels)))
    plots[[s]] <- list()
    for (ci in seq_along(chunks)) {
      nn <- format(chunks[[ci]])
      d <- ws[ws$night.label %in% nn, , drop = FALSE]
      d$night.label <- factor(d$night.label, levels = nn)
      pd <- d[!is.na(d$precip.plot), , drop = FALSE]
      pz <- d[!is.na(d$precip.zero), , drop = FALSE]
      v <- vl[vl$night.label %in% nn, , drop = FALSE]; v$night.label <- factor(v$night.label, levels = nn)
      g <- ggplot2::ggplot(d, ggplot2::aes(x = hours))
      if (!is.null(cut.lines)) for (i in seq_len(nrow(cut.lines)))
        g <- g + ggplot2::geom_hline(yintercept = cut.lines$y[i], colour = "black", linewidth = 0.6, linetype = cut.lines$lt[i])
      if (has["Precipitation"] && nrow(pd))
        g <- g + ggplot2::geom_rect(data = pd, ggplot2::aes(xmin = hours - bar.w / 2, xmax = hours + bar.w / 2,
                                                            ymin = to.y(clampv(0, axis.of("Precipitation")), axis.of("Precipitation")),
                                                            ymax = precip.plot), fill = "blue", colour = NA, inherit.aes = FALSE)
      if (nrow(pz))
        g <- g + ggplot2::geom_segment(data = pz, ggplot2::aes(x = hours - bar.w / 2, xend = hours + bar.w / 2, y = precip.zero, yend = precip.zero),
                                       colour = "blue", linewidth = 0.6, inherit.aes = FALSE)
      if (has["Wind"]) {
        dw <- d[!is.na(d$wind.plot), , drop = FALSE]
        one <- dw$wgrp %in% names(which(table(dw$wgrp) == 1))   # a lone reading between gaps is drawn as a dot
        if (any(!one)) g <- g + ggplot2::geom_line(data = dw[!one, , drop = FALSE], ggplot2::aes(y = wind.plot, group = wgrp), colour = "brown", linewidth = 0.5)
        if (any(one)) g <- g + ggplot2::geom_point(data = dw[one, , drop = FALSE], ggplot2::aes(y = wind.plot), colour = "brown", size = 0.6)
      }
      if (has["Temperature"]) g <- g + ggplot2::geom_line(ggplot2::aes(y = temp.plot, group = grp), colour = "red", linewidth = 0.6, na.rm = TRUE)
      g <- g + ggplot2::geom_vline(data = v, ggplot2::aes(xintercept = x), linetype = "dashed", linewidth = 0.6) +
        ggplot2::facet_wrap(~night.label, ncol = facet.ncol, drop = TRUE) +
        ggplot2::scale_x_continuous(breaks = xb, labels = xl) +
        ggplot2::scale_y_continuous(name = title.of(sy), breaks = brks(ry), sec.axis = sec) +
        ggplot2::coord_cartesian(xlim = c(0, span), ylim = ry, expand = TRUE) +
        ggplot2::labs(x = "Local Time") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(size = 10))
      plots[[s]][[ci]] <- g
      if (!is.null(dir.save)) {
        if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
        clean <- function(x) { x <- gsub('[\\\\/:*?"<>|]+', "", trimws(as.character(x))); x <- gsub("\\s*-\\s*", "-", x); gsub("\\s+", "-", x) }
        parts <- c(if (!blank(project.name)) clean(project.name), "weather.daily", if (nzchar(ws$site[1])) clean(ws$site[1]), clean(s),
                   paste0(gsub("-", "", nn[1]), "to", gsub("-", "", nn[length(nn)])), format(Sys.time(), "%Y%m%d%H%M%S"))
        f <- file.path(dir.save, paste0(paste(parts[nzchar(parts)], collapse = "_"), ".png"))
        ggplot2::ggsave(f, g, width = plot.width, height = 1.9 * ceiling(length(nn) / facet.ncol) + 1, units = "in", dpi = 300)
        cat("Saved:", f, "\n"); files <- c(files, f)
      }
    }
    all.data[[s]] <- ws[c("site", "station.name", "station.code", "night", "t", "hours", "temperature.c", "wind.speed.ms",
                          "precipitation.mm", "temp.plot", "wind.plot", "precip.plot")]
  }
  out.data <- if (length(all.data)) do.call(rbind, all.data) else data.frame()
  if (nrow(out.data)) { names(out.data)[names(out.data) == "t"] <- "date.time"; out.data$date.time <- format(out.data$date.time, "%Y-%m-%d %H:%M:%S") }
  rownames(out.data) <- NULL
  out.n <- if (length(all.nights)) do.call(rbind, all.nights) else data.frame(); rownames(out.n) <- NULL
  out.a <- if (length(all.axes)) do.call(rbind, all.axes) else data.frame(); rownames(out.a) <- NULL
  invisible(list(plots = plots, data = out.data, nights = out.n, axes = out.a, files = files))
}

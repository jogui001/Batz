#' Weather summary plot: minimum, mean and maximum per night or per day
#'
#' For each weather station in \code{data}, works out per night (dusk to
#' dawn) or per 24 hours (see \code{period}) the minimum, mean and maximum
#' temperature, the minimum, mean and maximum wind (speed or gust, see
#' \code{wind.type}) and the total precipitation, and draws them against the
#' date: temperature as red lines, wind as brown lines (maximum solid, mean
#' dashed, minimum dotted) and precipitation as blue bars, on the primary
#' and secondary Y axes as set by \code{stack.y} and \code{stack.y2}.
#' Horizontal black lines mark the cut-offs (\code{temperature} and
#' \code{wind.ms}). One PNG per station.
#'
#' @param data Data frame of weather records, normally
#'   \code{$weatherstation.data} from
#'   \code{\link{batz.generate_weatherstation.data}} (or the same table saved
#'   as a CSV); the same columns as for
#'   \code{\link{batz.plot.line_weather.daily}}: \code{$date.time} (local
#'   time), \code{$station.latitude}, \code{$station.longitude}, a
#'   temperature column (\code{$temperature.c}, or \code{$temperature.f}), a
#'   wind column (\code{$wind.speed.ms}, \code{$wind.speed.kmh} or
#'   \code{$wind.speed.mph}; for \code{wind.type = "gust"},
#'   \code{$wind.gust.ms}, \code{$wind.gust.kmh} or \code{$wind.gust.mph})
#'   and a precipitation column (\code{$precipitation.mm_interval},
#'   \code{$precipitation.in_interval}, \code{$precipitation.mm} or
#'   \code{$precipitation.in}, used in that order). Stations are told apart by
#'   \code{$station.code} (or \code{$station.name}, or the coordinates);
#'   \code{$site} and \code{$station.code} are used in the file name when
#'   present. Headers are matched ignoring case and "." / "_" / space
#'   differences.
#' @param suntimes Data frame of sun times, normally from
#'   \code{\link{batz.generate_suntimes.arulist}}, as for
#'   \code{\link{batz.plot.line_weather.daily}}. Needed for
#'   \code{period = "night"} when a monitoring time refers to sunset or
#'   sunrise. \code{$time.zone} (or \code{$time_zone}) sets the time zone
#'   \code{$date.time} is read in. Default \code{NULL}.
#' @param period \code{"night"} (default) or \code{"24hr"}. \code{"night"}:
#'   each night from \code{time.start_monitoring} to
#'   \code{time.end_monitoring} (default sunset to the next sunrise), labelled
#'   with the date it starts on. \code{"24hr"}: 24 hours, as set by
#'   \code{date.type}.
#' @param date.type \code{"calendar.day"} (default) or
#'   \code{"monitoring.night"}. Used with \code{period = "24hr"}:
#'   \code{"calendar.day"} = 00:00 to 24:00 of each date;
#'   \code{"monitoring.night"} = 12:00 on the date to 12:00 the next day, so a
#'   whole night is in one period. Not used with \code{period = "night"}.
#' @param date.start,date.end Date (or text such as \code{"2026-06-25"},
#'   \code{"6/25/2026"}) of the first and last date. Default \code{NULL} (or
#'   blank): the earliest / latest calendar date of \code{$date.time}.
#' @param stack.y Character, default \code{c("Temperature")}. Data drawn on
#'   the primary (left) Y axis: any of \code{"Temperature"}, \code{"Wind"},
#'   \code{"Precipitation"} (case ignored; \code{"temp"} and \code{"precip"}
#'   also work).
#' @param stack.y2 Character, default \code{c("Wind", "Precipitation")}. Data
#'   drawn on the secondary (right) Y axis. Each type can be on one axis
#'   only; a type left out of both is not drawn.
#' @param wind.ms Numeric, default \code{4}. Wind cut-off line (m/s). Blank
#'   (\code{""}) = no wind line.
#' @param temperature Numeric, default \code{10}. Temperature cut-off line
#'   (degrees C). Blank (\code{""}) = no temperature line.
#' @param wind.type \code{"speed"} (default) or \code{"gust"}: the wind speed
#'   or the wind gust column is summarized. The wind axis title says which.
#' @param time.start_monitoring Character, default \code{"Sunset"}. Start of
#'   each night (\code{period = "night"}): a clock time (\code{"21:00"}), or
#'   minutes before (\code{-}) or after (\code{+}) \code{Sunset} or
#'   \code{Sunrise} (\code{"-30 Sunset"}, \code{"Sunset"}).
#' @param time.end_monitoring Character, default \code{"Sunrise"}. End of each
#'   night: \code{Sunrise} / \code{Sunset} with minutes as above (sunrise is
#'   the next morning's), a length of time after the start
#'   (\code{"5 hours"}, \code{"90 minutes"}) or a clock time
#'   (\code{"06:00"}).
#' @param xaxe.interval Numeric, default \code{7}. Days between X axis labels.
#' @param scale.auto Logical, default \code{TRUE}. TRUE lines the two axes
#'   up so \code{temperature} and \code{wind.ms} are at the same height, as
#'   in \code{\link{batz.plot.line_weather.daily}}. FALSE uses
#'   \code{temp.range} and \code{wind.range} exactly as given.
#' @param temp.range Numeric, default \code{c(0, 25)}. Temperature axis
#'   limits.
#' @param wind.range Numeric, default \code{c(0, 10)}. Wind axis limits. Its
#'   maximum is also the maximum drawn precipitation.
#' @param precip.range Numeric, default \code{NULL}. Precipitation axis
#'   limits (mm) of the precipitation panel with
#'   \code{panel.layout = "separate"}, e.g. \code{c(0, 20)}, so plots can
#'   share one scale; larger totals are drawn at the limit (NOTE).
#'   \code{NULL}: 0 to just above the largest total. Not used with
#'   \code{"combined"} (precipitation uses the wind axis).
#' @param precip.cumulative Logical, default \code{TRUE}. Not used when
#'   \code{data} has \code{$precipitation.mm_interval} (or
#'   \code{$precipitation.in_interval}). TRUE when precipitation is a running
#'   total for the day: each reading becomes the amount since the reading
#'   before it. FALSE uses the values as they are.
#' @param project.name Character, default \code{""}. When not blank, every
#'   saved file name starts with it.
#' @param dir.save Character, default \code{getwd()}. Folder the PNGs are
#'   saved in (created if missing). \code{NULL} saves nothing.
#' @param save.auto Default \code{TRUE}: the files are saved in \code{dir.save}.
#'   \code{FALSE}: no files are saved; each plot is only printed (drawn).
#'   \code{"BOTH"}: each plot is saved and printed.
#'   \code{"print"}: nothing is saved; each plot is printed instead.
#'   \code{"generate <number>"} (e.g. \code{"generate 5"}): nothing is saved;
#'   up to \code{<number>} plots are created as objects in the R global
#'   environment, each named after the file it would have been saved as,
#'   without the extension and the time stamp; \code{"generate"} alone
#'   creates every plot.
#' @param panel.layout \code{"combined"} (default) or \code{"separate"}.
#'   \code{"combined"}: one panel with two Y axes (\code{stack.y},
#'   \code{stack.y2}). \code{"separate"}: one panel per data type, stacked
#'   in \code{panel.order} with the same date axis, each with its own Y axis
#'   (see Details); \code{stack.y} / \code{stack.y2} only choose which
#'   types are drawn.
#' @param panel.order Character, default \code{c("Wind", "Temperature",
#'   "Precipitation")}. Top-to-bottom order of the panels with
#'   \code{panel.layout = "separate"}.
#' @param plot.width,plot.height Numeric. PNG size (inches). Defaults:
#'   width \code{9}; height \code{NULL} = 4 for \code{"combined"}, 2 per
#'   panel plus 1 for \code{"separate"}.
#' @param aes.plotopts Optional, default \code{NULL}. Plot settings table: a
#'   data frame, or the path of a settings \code{.csv} or of a pooled
#'   \code{.xlsx} workbook (its tab \verb{pltop.line_weather.average} is
#'   read), laid out like
#'   \code{batz.generate_plotopts("batz.plot.line_weather.average")} writes it
#'   (columns \code{$category}, \code{$parameter}, \code{$default.value},
#'   \code{$override.value}, \code{$notes}). Each \code{$parameter} row is one
#'   of this function's arguments (e.g. \code{period}, \code{temp.range});
#'   lists are separated by ";" (e.g. \code{"0;25"}). Settings are taken from
#'   the function call, then \code{aes.plotopts}, then the defaults shown
#'   here. A blank cell keeps the default. Text sizes and faces (rows
#'   \code{plot.title.size}, \code{plot.title.face}, \code{axis.title.size},
#'   \code{axis.title.face}, \code{axis.title.x.size},
#'   \code{axis.title.y.size}, \code{axis.text.size}, \code{axis.text.x.size},
#'   \code{axis.text.y.size}, \code{legend.title.size},
#'   \code{legend.title.face}, \code{legend.text.size}): a blank row keeps the
#'   function's own value (10 pt; titles bold).
#' @param aes.style Character, default \code{"override.value"}. Name of the
#'   \code{aes.plotopts} column read before \code{$default.value} (e.g. the
#'   project-name column written by \code{\link{batz.generate_plotopts}}).
#'
#' @details
#' \strong{Periods.} A reading belongs to a period when its local time is at
#' or after the period's start and before its end. For each period:
#' \code{$temperature.min}, \code{$temperature.mean},
#' \code{$temperature.max} (degrees C), \code{$wind.min},
#' \code{$wind.mean}, \code{$wind.max} (m/s) - the mean is the mean of the
#' readings - and \code{$precipitation.total} (mm, the sum of the
#' precipitation since the previous reading). A period with no readings has
#' \code{NA} values and breaks the lines; a NOTE lists such dates.
#'
#' \strong{Sun times} (\code{period = "night"}). For each station the
#' \code{suntimes} site nearest to (\code{$station.latitude},
#' \code{$station.longitude}) is used (a NOTE names it). A night with no sun
#' time for that site has no values (NOTE).
#'
#' \strong{Axes.} As in \code{\link{batz.plot.line_weather.daily}}: the axes
#' come from \code{temp.range} and \code{wind.range}; with
#' \code{scale.auto = TRUE}, temperature and wind on different axes and both
#' cut-offs given, \code{temperature} is level with \code{wind.ms} and one
#' solid black line crosses both (a NOTE gives the ranges used); otherwise
#' each cut-off gets its own line (the first solid, the other dotted).
#' Precipitation goes from 0 to the top of the wind axis. Values outside the
#' axes are drawn at the axis limit (NOTE for precipitation).
#'
#' \strong{Separate panels} (\code{panel.layout = "separate"}). The
#' temperature panel uses \code{temp.range}, the wind panel
#' \code{wind.range} (values outside are drawn at the limit), and the
#' precipitation panel runs from 0 to just above the largest total, or
#' \code{precip.range} when given. The
#' \code{temperature} / \code{wind.ms} cut-off line is drawn in its own
#' panel; \code{scale.auto} is not used. The panel titles give the units.
#'
#' \strong{Units.} Wind in km/h is divided by 3.6 and in mph multiplied by
#' 0.44704; temperature in F is converted to C; precipitation in inches is
#' multiplied by 25.4.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]weather.average_<site>_<station.code>_<night|24hr>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png},
#' with the first and last date.
#'
#' @return Invisibly, a list: \code{$plots} (named list, one ggplot per
#'   station), \code{$data} (one row per station and date: \code{$station},
#'   \code{$site}, \code{$station.name}, \code{$station.code},
#'   \code{$aru.name} (the suntimes site), \code{$period}, \code{$date},
#'   \code{$period.start}, \code{$period.end}, \code{$readings},
#'   \code{$temperature.min}, \code{$temperature.mean},
#'   \code{$temperature.max}, \code{$wind.min}, \code{$wind.mean},
#'   \code{$wind.max}, \code{$precipitation.total}), \code{$axes} (axis
#'   ranges per station) and \code{$files}.
#'
#' @seealso \code{\link{batz.plot.line_weather.daily}},
#'   \code{\link{batz.generate_weatherstation.data}}
#'
#' @examples
#' \dontrun{
#' weather <- read.csv("weather.csv")
#' suntimes <- read.csv("MAMM_IN25092_2suntimes.csv")
#' # each night, sunset to sunrise
#' batz.plot.line_weather.average(weather, suntimes, project.name = "IN25092")
#' # full calendar days
#' batz.plot.line_weather.average(weather, period = "24hr")
#' # noon to noon
#' batz.plot.line_weather.average(weather, period = "24hr", date.type = "monitoring.night")
#' }
#' @export
batz.plot.line_weather.average <- function(data, suntimes = NULL,
                                           period = c("night", "24hr"),
                                           date.type = c("calendar.day", "monitoring.night"),
                                           date.start = NULL, date.end = NULL,
                                           stack.y = c("Temperature"), stack.y2 = c("Wind", "Precipitation"),
                                           wind.ms = 4, temperature = 10, wind.type = c("speed", "gust"),
                                           time.start_monitoring = "Sunset", time.end_monitoring = "Sunrise",
                                           xaxe.interval = 7, scale.auto = TRUE,
                                           temp.range = c(0, 25), wind.range = c(0, 10), precip.range = NULL,
                                           precip.cumulative = TRUE,
                                           project.name = "", dir.save = getwd(), save.auto = TRUE,
                                           panel.layout = c("combined", "separate"),
                                           panel.order = c("Wind", "Temperature", "Precipitation"),
                                           plot.width = 9, plot.height = NULL,
                                           aes.plotopts = NULL, aes.style = "override.value") {
  FN <- "batz.plot.line_weather.average"
  sv <- util_saver(save.auto, FN, plot = TRUE)
  ## settings: function call > aes.plotopts > built-in defaults
  given <- setdiff(names(match.call())[-1], "")
  aes.plotopts <- plotutil_read_plotopts(aes.plotopts, FN)
  po.vals <- weather.daily.plotopts(aes.plotopts, aes.style, given, FN, fm = formals(batz.plot.line_weather.average),
                                    skip = c("data", "suntimes", "date.start", "date.end", "project.name", "dir.save",
                                             "save.auto", "aes.plotopts", "aes.style"))
  po.text <- if (is.data.frame(aes.plotopts)) aes.plotopts else tryCatch(batz.db("plotopts.batz.plot.line_weather.average"), error = function(e) NULL)
  for (nm in names(po.vals)) assign(nm, po.vals[[nm]])
  temp.given <- !missing(temp.range) || "temp.range" %in% names(po.vals)
  wind.given <- !missing(wind.range) || "wind.range" %in% names(po.vals)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop(FN, ' needs the ggplot2 package: install.packages("ggplot2")', call. = FALSE)
  if (!is.data.frame(data) || nrow(data) == 0) stop(FN, ": data must be a data frame with rows.", call. = FALSE)

  key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  pick <- function(df, cands) { k <- key(names(df)); for (cc in cands) { i <- match(key(cc), k); if (!is.na(i)) return(names(df)[i]) }; NA_character_ }
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) || all(!nzchar(trimws(as.character(x))))
  choice <- function(x, ok, nm) {
    v <- tolower(trimws(as.character(x)[1]))
    if (!v %in% ok) stop(FN, ": ", nm, " must be ", paste0('"', ok, '"', collapse = " or "), ' - not "', x[1], '".', call. = FALSE)
    v
  }
  period <- choice(period, c("night", "24hr"), "period")
  date.type <- choice(date.type, c("calendar.day", "monitoring.night"), "date.type")
  if (period == "night" && "date.type" %in% c(given, names(po.vals)))
    cat(sprintf('NOTE: %s - date.type is only used with period = "24hr" - ignored.\n', FN))
  wind.type <- choice(wind.type, c("speed", "gust"), "wind.type")
  panel.layout <- choice(panel.layout, c("combined", "separate"), "panel.layout")
  if (blank(plot.height)) plot.height <- NA_real_ else plot.height <- num(plot.height)[1]
  temperature <- if (blank(temperature)) NA_real_ else num(temperature)[1]
  wind.ms <- if (blank(wind.ms)) NA_real_ else num(wind.ms)[1]
  xint <- num(xaxe.interval)[1]
  if (is.na(xint) || xint <= 0) stop(FN, ": xaxe.interval must be a number of days > 0.", call. = FALSE)

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

  ## ---- night window settings --------------------------------------------------
  clock <- function(s) {
    s <- trimws(as.character(s))
    if (grepl("^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", s)) { p <- as.numeric(strsplit(s, ":")[[1]]); return(p[1] + p[2] / 60 + if (length(p) > 2) p[3] / 3600 else 0) }
    if (grepl("^[0-9]{3,4}$", s)) { n <- as.numeric(s); return(n %/% 100 + (n %% 100) / 60) }
    if (grepl("^[0-9]{1,2}$", s)) return(as.numeric(s))
    NA_real_
  }
  sunref <- function(s) {
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
  duration <- function(s) {
    s <- tolower(trimws(as.character(s)))
    m <- regmatches(s, regexec("^([0-9.]+|[a-z]+) ?(hours?|hrs?|h|minutes?|mins?|m)$", s))[[1]]
    if (!length(m)) return(NA_real_)
    n <- suppressWarnings(as.numeric(m[2])); if (is.na(n)) n <- unname(words[m[2]])
    if (is.na(n)) return(NA_real_)
    if (substr(m[3], 1, 1) == "h") n * 60 else n
  }
  t.start <- list(clock = clock(time.start_monitoring), sun = sunref(time.start_monitoring))
  t.end <- list(clock = clock(time.end_monitoring), sun = sunref(time.end_monitoring), dur = duration(time.end_monitoring))
  need.sun <- FALSE
  if (period == "night") {
    if (is.na(t.start$clock) && is.null(t.start$sun))
      stop(FN, ': time.start_monitoring must be a clock time ("21:00") or minutes from Sunset / Sunrise ("-30 Sunset"); got "', time.start_monitoring, '".', call. = FALSE)
    if (is.na(t.end$clock) && is.null(t.end$sun) && is.na(t.end$dur))
      stop(FN, ': time.end_monitoring must be Sunrise / Sunset (with minutes), a length of time ("5 hours") or a clock time ("06:00"); got "', time.end_monitoring, '".', call. = FALSE)
    need.sun <- !is.null(t.start$sun) || !is.null(t.end$sun)
  }

  ## ---- data columns --------------------------------------------------------
  col <- list(dt = pick(data, "date.time"), lat = pick(data, "station.latitude"), lon = pick(data, "station.longitude"),
              tc = pick(data, "temperature.c"), tf = pick(data, "temperature.f"),
              wms = pick(data, paste0("wind.", wind.type, c(".ms", ".m.s"))), wkmh = pick(data, paste0("wind.", wind.type, ".kmh")),
              wmph = pick(data, paste0("wind.", wind.type, ".mph")),
              pmm = pick(data, "precipitation.mm"), pin = pick(data, "precipitation.in"),
              pmmi = pick(data, "precipitation.mm_interval"), pini = pick(data, "precipitation.in_interval"),
              code = pick(data, "station.code"), name = pick(data, "station.name"), site = pick(data, "site"))
  miss <- c(if (is.na(col$dt)) "$date.time", if (is.na(col$lat)) "$station.latitude", if (is.na(col$lon)) "$station.longitude",
            if (is.na(col$tc) && is.na(col$tf)) "$temperature.c (or $temperature.f)",
            if (all(is.na(c(col$wms, col$wkmh, col$wmph))))
              sprintf("$wind.%1$s.ms (or $wind.%1$s.kmh / $wind.%1$s.mph) for wind.type = \"%1$s\"", wind.type),
            if (all(is.na(c(col$pmm, col$pin, col$pmmi, col$pini)))) "$precipitation.mm (or $precipitation.mm_interval / $precipitation.in)")
  if (length(miss)) stop(FN, ": data is missing ", paste(miss, collapse = ", "), call. = FALSE)
  w <- data.frame(dt.text = as.character(data[[col$dt]]), lat = num(data[[col$lat]]), lon = num(data[[col$lon]]), stringsAsFactors = FALSE)
  w$temperature.c <- if (!is.na(col$tc)) num(data[[col$tc]]) else (num(data[[col$tf]]) - 32) * 5 / 9
  w$wind.ms <- if (!is.na(col$wms)) num(data[[col$wms]]) else if (!is.na(col$wkmh)) num(data[[col$wkmh]]) / 3.6 else num(data[[col$wmph]]) * 0.44704
  p.interval <- !is.na(col$pmmi) || !is.na(col$pini)
  w$precipitation.raw <- if (!is.na(col$pmmi)) num(data[[col$pmmi]]) else if (!is.na(col$pini)) num(data[[col$pini]]) * 25.4 else
    if (!is.na(col$pmm)) num(data[[col$pmm]]) else num(data[[col$pin]]) * 25.4
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
  if (need.sun && is.null(st)) stop(FN, ': period = "night" with a time from Sunset / Sunrise needs suntimes.', call. = FALSE)

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
  stat <- function(v, f) if (any(!is.na(v))) round(f(v, na.rm = TRUE), 2) else NA_real_
  period.lab <- if (period == "night") sprintf("night (%s to %s)", time.start_monitoring, time.end_monitoring) else
    if (date.type == "calendar.day") "24 hr (00:00 to 24:00)" else "24 hr (12:00 to 12:00)"

  plots <- list(); all.data <- list(); all.axes <- list(); files <- character(0)
  stations <- unique(w$station[!is.na(w$lat) & !is.na(w$lon)])
  if (length(stations) == 0) stop(FN, ": no rows with $station.latitude and $station.longitude.", call. = FALSE)

  for (s in stations) {
    ws <- w[w$station == s, , drop = FALSE]
    lab <- if (nzchar(ws$station.name[1])) sprintf("%s (%s)", ws$station.name[1], s) else s
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

    ws$t <- parse.dt(ws$dt.text, tz)
    if (any(is.na(ws$t))) cat(sprintf("NOTE: %s - station %s: %d row(s) with an unreadable $date.time dropped.\n", FN, lab, sum(is.na(ws$t))))
    ws <- ws[!is.na(ws$t), , drop = FALSE]; ws <- ws[order(ws$t), , drop = FALSE]
    if (!nrow(ws)) next
    if (isTRUE(precip.cumulative) && !p.interval) {
      ws$precipitation.mm <- stats::ave(ws$precipitation.raw, format(ws$t, "%Y-%m-%d"), FUN = function(p) {
        ok <- !is.na(p); if (any(ok)) p[ok] <- pmax(0, c(p[ok][1], diff(p[ok]))); p })
    } else ws$precipitation.mm <- ws$precipitation.raw
    cal <- as.Date(format(ws$t, "%Y-%m-%d"))
    ds <- if (blank(date.start)) min(cal) else parse.date(date.start)
    de <- if (blank(date.end)) max(cal) else parse.date(date.end)
    if (is.na(ds) || is.na(de)) stop(FN, ": date.start / date.end could not be read as dates.", call. = FALSE)
    if (de < ds) stop(FN, ": date.end is before date.start.", call. = FALSE)
    dates <- seq(ds, de, by = "day")

    ## ---- period of each date -----------------------------------------------
    at <- function(d, h) as.POSIXct(sprintf("%s %02d:%02d:%02d", format(d), floor(h), floor((h %% 1) * 60), round(((h * 60) %% 1) * 60)),
                                    format = "%Y-%m-%d %H:%M:%S", tz = tz)
    sun.time <- function(n, ref) {
      if (is.null(sn)) return(as.POSIXct(NA))
      if (ref == "sunset") { v <- sn$sunset[parse.date(sn$date) == n][1] }
      else { v <- sn$sunrise.mon[parse.date(sn$date) == n][1]
             if (is.na(v) || !nzchar(v)) v <- sn$sunrise[parse.date(sn$date) == n + 1][1] }
      if (is.na(v) || !nzchar(v)) as.POSIXct(NA) else parse.dt(v, tz)
    }
    win <- function(n) {
      if (period == "24hr") {
        if (date.type == "calendar.day") return(list(at(n, 0), at(n + 1, 0)))
        return(list(at(n, 12), at(n + 1, 12)))
      }
      w0 <- if (!is.null(t.start$sun)) sun.time(n, t.start$sun$ref) + t.start$sun$offset * 60 else at(if (t.start$clock >= 12) n else n + 1, t.start$clock)
      w1 <- if (!is.na(t.end$dur)) w0 + t.end$dur * 60 else if (!is.null(t.end$sun)) sun.time(n, t.end$sun$ref) + t.end$sun$offset * 60 else
        at(if (t.end$clock >= 12) n else n + 1, t.end$clock)
      if (!is.na(w0) && !is.na(w1) && w1 <= w0) w1 <- w1 + 86400
      list(w0, w1)
    }
    rows <- lapply(dates, function(n) {
      ww <- win(n); w0 <- ww[[1]]; w1 <- ww[[2]]
      inw <- !is.na(w0) & !is.na(w1) & ws$t >= w0 & ws$t < w1
      tv <- ws$temperature.c[inw]; wv <- ws$wind.ms[inw]; pv <- ws$precipitation.mm[inw]
      data.frame(station = s, site = ws$site[1], station.name = ws$station.name[1], station.code = ws$station.code[1],
                 aru.name = near, period = period, date = n,
                 period.start = if (is.na(w0)) NA_character_ else format(w0, "%Y-%m-%d %H:%M:%S"),
                 period.end = if (is.na(w1)) NA_character_ else format(w1, "%Y-%m-%d %H:%M:%S"),
                 readings = sum(inw),
                 temperature.min = stat(tv, min), temperature.mean = stat(tv, mean), temperature.max = stat(tv, max),
                 wind.min = stat(wv, min), wind.mean = stat(wv, mean), wind.max = stat(wv, max),
                 precipitation.total = if (any(!is.na(pv))) round(sum(pv, na.rm = TRUE), 2) else NA_real_,
                 stringsAsFactors = FALSE)
    })
    pt <- do.call(rbind, rows)
    no.sun <- is.na(pt$period.start) | is.na(pt$period.end)
    if (any(no.sun)) cat(sprintf("NOTE: %s - station %s: no sun time at site '%s' for %s - no values.\n",
                                 FN, lab, near, paste(format(pt$date[no.sun]), collapse = ", ")))
    empty <- pt$readings == 0 & !no.sun
    if (any(empty)) cat(sprintf("NOTE: %s - station %s: no readings for %s - no values.\n",
                                FN, lab, paste(format(pt$date[empty]), collapse = ", ")))
    all.data[[s]] <- pt
    if (all(pt$readings == 0)) { cat(sprintf("NOTE: %s - station %s: no readings between %s and %s - no plot.\n", FN, lab, ds, de)); next }

    ## ---- axis ranges (as batz.plot.line_weather.daily) ---------------------
    has <- c(Temperature = !is.na(axis.of("Temperature")), Wind = !is.na(axis.of("Wind")), Precipitation = !is.na(axis.of("Precipitation")))
    split.axes <- has["Temperature"] && has["Wind"] && axis.of("Temperature") != axis.of("Wind")
    aligned <- split.axes && isTRUE(scale.auto) && !is.na(temperature) && !is.na(wind.ms) && panel.layout == "combined"   # separate panels: ranges as given
    tr <- as.numeric(temp.range); wr <- as.numeric(wind.range); tr.in <- tr; wr.in <- wr
    if (aligned) {
      if (temperature <= tr[1]) stop(FN, ": temperature (", temperature, ") must be above the bottom of temp.range (", tr[1], ").", call. = FALSE)
      if (wind.ms <= wr[1]) stop(FN, ": wind.ms (", wind.ms, ") must be above the bottom of wind.range (", wr[1], ").", call. = FALSE)
      k <- (temperature - tr[1]) / (wind.ms - wr[1])
      if (wind.given && !temp.given) {
        tr <- c(tr[1], tr[1] + k * (wr[2] - wr[1]))
      } else {
        need <- (tr[2] - tr[1]) / k
        step <- if (need <= 10) 1 else if (need <= 20) 2 else 5
        wr <- c(wr[1], wr[1] + step * ceiling(need / step - 1e-9))
        tr <- c(tr[1], tr[1] + k * (wr[2] - wr[1]))
        if (wind.given && temp.given && any(abs(wr - wr.in) > 1e-9))
          cat(sprintf("NOTE: %s - temp.range is kept and wind.range is changed so %s C lines up with %s m/s.\n", FN, temperature, wind.ms))
      }
      if (any(abs(c(tr, wr) - c(tr.in, wr.in)) > 1e-9))
        cat(sprintf("NOTE: %s - axes: temperature %s to %s C, wind and precipitation %s to %s (%s C level with %s m/s).\n", FN,
                    format(round(tr[1], 2)), format(round(tr[2], 2)), format(round(wr[1], 2)), format(round(wr[2], 2)), temperature, wind.ms))
    }
    pr <- if (panel.layout == "separate" && !blank(precip.range)) range(num(precip.range)) else c(0, wr[2])
    var.range <- list(Temperature = tr, Wind = wr, Precipitation = pr)
    axis.range <- function(a) {
      v <- if (a == "y") sy else sy2
      if (!length(v)) return(NULL)
      if (split.axes) { if ("Temperature" %in% v) return(tr); if ("Wind" %in% v) return(wr) }
      range(unlist(var.range[v]))
    }
    ry <- axis.range("y"); ry2 <- axis.range("y2")
    to.y <- function(v, a) if (a == "y") v else ry[1] + (v - ry2[1]) * diff(ry) / diff(ry2)
    clampv <- function(v, a) { r <- if (a == "y") ry else ry2; pmin(pmax(v, r[1]), r[2]) }
    all.axes[[s]] <- data.frame(station = s, axis = c("y", if (length(sy2)) "y2"),
                                contents = c(paste(sy, collapse = "; "), if (length(sy2)) paste(sy2, collapse = "; ")),
                                min = c(ry[1], if (length(sy2)) ry2[1]), max = c(ry[2], if (length(sy2)) ry2[2]), stringsAsFactors = FALSE)
    big <- !is.na(pt$precipitation.total) & pt$precipitation.total > pr[2]
    if (has["Precipitation"] && any(big) && !(panel.layout == "separate" && blank(precip.range)))
      cat(sprintf("NOTE: %s - station %s: precipitation above %s mm drawn at %s mm for %s.\n", FN, lab, pr[2], pr[2],
                  paste(sprintf("%s (%s mm)", format(pt$date[big]), pt$precipitation.total[big]), collapse = ", ")))

    ## ---- long table of the lines ---------------------------------------------
    stat.lab <- c(max = "Maximum", mean = "Mean", min = "Minimum")
    ln <- NULL
    for (v in c("Temperature", "Wind")) {
      if (!has[v]) next
      pre <- if (v == "Temperature") "temperature" else "wind"
      for (k in names(stat.lab))
        ln <- rbind(ln, data.frame(date = pt$date, variable = v, statistic = stat.lab[[k]],
                                   y = to.y(clampv(pt[[paste0(pre, ".", k)]], axis.of(v)), axis.of(v)), stringsAsFactors = FALSE))
    }
    if (!is.null(ln)) {
      ln$grp <- paste(ln$variable, ln$statistic)
      ln$variable <- factor(ln$variable, levels = c("Temperature", "Wind"))
      ln$statistic <- factor(ln$statistic, levels = unname(stat.lab))
    }
    pb <- pt[!is.na(pt$precipitation.total) & pt$precipitation.total > 0, , drop = FALSE]

    ## cut-off lines
    cut.lines <- NULL
    cuts <- c(Temperature = temperature, Wind = wind.ms); cuts <- cuts[!is.na(cuts) & names(cuts) %in% c(sy, sy2)]
    if (aligned) cut.lines <- data.frame(y = to.y(temperature, axis.of("Temperature")), lt = "solid")
    else {
      for (a in c("y", "y2")) {
        v <- intersect(if (a == "y") sy else sy2, names(cuts)); if (!length(v)) next
        cut.lines <- rbind(cut.lines, data.frame(y = to.y(cuts[v], a), lt = "dotted"))
      }
      if (!is.null(cut.lines)) cut.lines$lt[1] <- "solid"
    }
    if (!is.null(cut.lines)) cut.lines <- cut.lines[!duplicated(round(cut.lines$y, 9)), , drop = FALSE]

    unit.lab <- c(Temperature = "Temperature (\u00b0C)", Wind = if (wind.type == "gust") "Wind gust (m/s)" else "Wind speed (m/s)",
                  Precipitation = "Precipitation total (mm)")
    title.of <- function(v) { l <- unit.lab[v]; if (length(l) > 1) l[-1] <- paste0(tolower(substr(l[-1], 1, 1)), substring(l[-1], 2)); paste(l, collapse = " and ") }
    brks <- function(r) { b <- pretty(r, n = 6); b[b >= r[1] - 1e-9 & b <= r[2] + 1e-9] }
    sec <- if (length(sy2)) ggplot2::sec_axis(~ ry2[1] + (. - ry[1]) * diff(ry2) / diff(ry), name = title.of(sy2), breaks = brks(ry2)) else ggplot2::waiver()
    xb <- seq(ds, de, by = paste(xint, "days"))

    if (panel.layout == "combined") {
    g <- ggplot2::ggplot()
    if (!is.null(cut.lines)) for (i in seq_len(nrow(cut.lines)))
      g <- g + ggplot2::geom_hline(yintercept = cut.lines$y[i], colour = "black", linewidth = 0.6, linetype = cut.lines$lt[i])
    if (has["Precipitation"] && nrow(pb)) {
      pb$y0 <- to.y(clampv(0, axis.of("Precipitation")), axis.of("Precipitation"))
      pb$y1 <- to.y(clampv(pmin(pb$precipitation.total, pr[2]), axis.of("Precipitation")), axis.of("Precipitation"))
      pb$fill <- "Precipitation total"
      g <- g + ggplot2::geom_rect(data = pb, ggplot2::aes(xmin = date - 0.35, xmax = date + 0.35, ymin = y0, ymax = y1, fill = fill), colour = NA) +
        ggplot2::scale_fill_manual(name = NULL, values = c("Precipitation total" = "blue"))
    }
    if (!is.null(ln)) {
      g <- g + ggplot2::geom_line(data = ln, ggplot2::aes(x = date, y = y, colour = variable, linetype = statistic, group = grp),
                                  linewidth = 0.6, na.rm = TRUE) +
        ggplot2::geom_point(data = ln, ggplot2::aes(x = date, y = y, colour = variable, group = grp), size = 0.8, na.rm = TRUE) +
        ggplot2::scale_colour_manual(name = NULL, values = c(Temperature = "red", Wind = "brown"), drop = TRUE) +
        ggplot2::scale_linetype_manual(name = NULL, values = c(Maximum = "solid", Mean = "dashed", Minimum = "dotted"))
    }
    g <- g + ggplot2::scale_x_date(breaks = xb, labels = format(xb, "%b %d")) +
      ggplot2::scale_y_continuous(name = title.of(sy), breaks = brks(ry), sec.axis = sec) +
      ggplot2::coord_cartesian(xlim = c(ds - 0.5, de + 0.5), ylim = ry, expand = FALSE) +
      ggplot2::labs(x = if (period == "night") "Night (date it starts)" else "Date", title = sprintf("%s - %s", lab, period.lab)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
                     plot.title = ggplot2::element_text(size = 10, face = "bold"),
                     axis.text = ggplot2::element_text(size = 10),
                     axis.title = ggplot2::element_text(size = 10, face = "bold"),
                     legend.text = ggplot2::element_text(size = 10))
    height <- if (is.na(plot.height)) 4 else plot.height
    } else {
      g <- weather.average.separate(pt, intersect(sapply(panel.order, type.of), c(sy, sy2)), unit.lab, tr, wr, temperature, wind.ms,
                                    if (blank(precip.range)) NULL else pr,
                                    ds, de, xb, if (period == "night") "Monitoring night" else "Date", sprintf("%s - %s", lab, period.lab))
      height <- if (is.na(plot.height)) 2 * length(intersect(sapply(panel.order, type.of), c(sy, sy2))) + 1 else plot.height
    }
    g <- g + plotutil_text_theme(function(k) plotutil_po_value(po.text, aes.style, k))
    plots[[s]] <- g
    if (!is.null(dir.save)) {
      if (sv$save && !dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
      clean <- function(x) { x <- gsub('[\\\\/:*?"<>|]+', "", trimws(as.character(x))); x <- gsub("\\s*-\\s*", "-", x); gsub("\\s+", "-", x) }
      parts <- c(if (!blank(project.name)) clean(project.name), "weather.average", if (nzchar(ws$site[1])) clean(ws$site[1]), clean(s), period,
                 paste0(format(ds, "%Y%m%d"), "to", format(de, "%Y%m%d")), format(Sys.time(), "%Y%m%d%H%M%S"))
      f <- file.path(dir.save, paste0(paste(parts[nzchar(parts)], collapse = "_"), ".png"))
      sv$out(g, f, function(.f) plotutil_ggsave(.f, g, width = plot.width, height = height, units = "in", dpi = 300))
      if (sv$save) files <- c(files, f)
    }
  }
  out.data <- if (length(all.data)) do.call(rbind, all.data) else data.frame(); rownames(out.data) <- NULL
  out.a <- if (length(all.axes)) do.call(rbind, all.axes) else data.frame(); rownames(out.a) <- NULL
  invisible(list(plots = plots, data = out.data, axes = out.a, files = files))
}


#' Weather summary, one panel per data type (internal)
#'
#' Panels stacked in \code{vars} order, shared date axis, free Y axes:
#' temperature clamped to \code{tr}, wind to \code{wr}, precipitation 0 to
#' just above the largest total. Max / mean / min as solid / dashed / dotted
#' lines; precipitation as blue bars; the cut-off line in its own panel.
#' @noRd
weather.average.separate <- function(pt, vars, unit.lab, tr, wr, temperature, wind.ms, pr, ds, de, xb, xlab, title) {
  vars <- unique(vars)
  if (!length(vars)) stop("batz.plot.line_weather.average: no data type to draw - check stack.y / stack.y2 / panel.order.", call. = FALSE)
  lev <- unname(unit.lab[vars])
  stat.lab <- c(max = "Maximum", mean = "Mean", min = "Minimum")
  ln <- NULL; lim <- NULL; cut <- NULL
  for (v in intersect(vars, c("Temperature", "Wind"))) {
    pre <- if (v == "Temperature") "temperature" else "wind"
    r <- if (v == "Temperature") tr else wr
    for (k in names(stat.lab))
      ln <- rbind(ln, data.frame(date = pt$date, panel = unit.lab[[v]], variable = v, statistic = stat.lab[[k]],
                                 y = pmin(pmax(pt[[paste0(pre, ".", k)]], r[1]), r[2]), stringsAsFactors = FALSE))
    lim <- rbind(lim, data.frame(date = ds, panel = unit.lab[[v]], y = r, stringsAsFactors = FALSE))
    cv <- if (v == "Temperature") temperature else wind.ms
    if (!is.na(cv)) cut <- rbind(cut, data.frame(panel = unit.lab[[v]], y = cv, stringsAsFactors = FALSE))
  }
  pb <- NULL
  if ("Precipitation" %in% vars) {
    if (is.null(pr)) {
      top <- max(c(pt$precipitation.total, 0), na.rm = TRUE)
      pr <- c(0, if (top > 0) max(pretty(c(0, top * 1.05))) else 1)
    }
    lim <- rbind(lim, data.frame(date = ds, panel = unit.lab[["Precipitation"]], y = pr, stringsAsFactors = FALSE))
    pb <- pt[!is.na(pt$precipitation.total) & pt$precipitation.total > 0, , drop = FALSE]
    pb$precipitation.total <- pmin(pb$precipitation.total, pr[2])
    pb$panel <- rep(unit.lab[["Precipitation"]], nrow(pb))
  }
  fac <- function(d) { if (!is.null(d) && nrow(d)) d$panel <- factor(d$panel, levels = lev); d }
  ln <- fac(ln); lim <- fac(lim); cut <- fac(cut); pb <- fac(pb)
  g <- ggplot2::ggplot() + ggplot2::geom_blank(data = lim, ggplot2::aes(x = date, y = y))
  if (!is.null(cut) && nrow(cut)) g <- g + ggplot2::geom_hline(data = cut, ggplot2::aes(yintercept = y), colour = "black", linewidth = 0.5)
  if (!is.null(pb) && nrow(pb))
    g <- g + ggplot2::geom_rect(data = pb, ggplot2::aes(xmin = date - 0.35, xmax = date + 0.35, ymin = 0, ymax = precipitation.total),
                                fill = "blue", colour = NA)
  if (!is.null(ln) && nrow(ln)) {
    ln$grp <- paste(ln$variable, ln$statistic)
    ln$statistic <- factor(ln$statistic, levels = unname(stat.lab))
    g <- g + ggplot2::geom_line(data = ln, ggplot2::aes(x = date, y = y, colour = variable, linetype = statistic, group = grp),
                                linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(data = ln, ggplot2::aes(x = date, y = y, colour = variable, group = grp), size = 0.8, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = c(Temperature = "red", Wind = "brown"), guide = "none") +
      ggplot2::scale_linetype_manual(name = NULL, values = c(Maximum = "solid", Mean = "dashed", Minimum = "dotted"))
  }
  g + ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", drop = FALSE) +
    ggplot2::scale_x_date(breaks = xb, labels = format(xb, "%b %d")) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.03))) +
    ggplot2::coord_cartesian(xlim = c(ds - 0.5, de + 0.5), expand = FALSE) +
    ggplot2::labs(x = xlab, y = NULL, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
                   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 10),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text = ggplot2::element_text(size = 10), axis.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 10))
}

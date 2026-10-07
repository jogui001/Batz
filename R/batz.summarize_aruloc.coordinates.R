#' Summarize ARU locations and calculate median locations for sets of ARUs
#'
#' Added 2026-09-30, per Josh. Splits each ARU's records into separate
#' location records (one row per continuous stay at one
#' \code{longitude}/\code{latitude}), counts the records in each, and
#' calculates the median location for each set of ARUs listed in
#' \code{aru.combo}.
#'
#' @param data Data frame of ARU log records - e.g. the output of
#'   \code{\link{batz.merge_sm.logfiles}}. Must have \code{$aru.name},
#'   \code{$date}, \code{$time} (see "Dates and times" below),
#'   \code{$longitude} and \code{$latitude} (signed decimal degrees). Header
#'   spelling is matched loosely (\code{"."}/\code{"_"}/case ignored). Other
#'   columns are ignored.
#' @param aru.combo Data frame of ARU sets. The \strong{first column} is
#'   used, whatever it is called (e.g. \code{$Sets}); each row lists one set
#'   of ARU names separated by \code{";"} (e.g. \code{"NWS01; NWS02;
#'   NWS03"}). Blank rows are dropped. Names are matched to
#'   \code{data$aru.name} ignoring case and surrounding spaces.
#' @param gap.days Numeric, default \code{1}. A gap longer than this many
#'   days between two consecutive records of the same ARU at the same
#'   location starts a new location record.
#' @param zero.remove Logical, default \code{TRUE}. Drop records where
#'   \code{longitude} and \code{latitude} are both \code{0} - what SM units
#'   log before they get a GPS fix - before summarizing.
#'
#' @param group.by Character, default \code{"aru.name"}. The column in
#'   \code{data} that identifies each ARU: \code{"aru.name"} or
#'   \code{"aru.label"} (any other single column name also works). Location
#'   records are split by this column, \code{aru.combo} names are matched
#'   against it, and it is the first column of \code{coordinate.summary}
#'   (named after \code{group.by}).
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplifies accented letters and
#'   symbols (\code{é} -> \code{e}, \code{×} -> \code{X}) and removes other non-ASCII special characters (e.g. \code{°}, \code{µ},
#'   \code{™}) from \code{data} and \code{aru.combo} before
#'   anything else, so e.g. a latitude of \code{"42.036373°"} is read as
#'   the number \code{42.036373} instead of being dropped as unreadable.
#'
#' @return Invisibly, a named list with two data frames, also auto-assigned
#'   into the calling environment:
#'
#'   \code{coordinate.summary} - one row per location record, ordered by
#'   the \code{group.by} column then \code{$datetime.start}:
#'   \code{$aru.name} (or whichever column \code{group.by} names), \code{$datetime.start} and \code{$datetime.end}
#'   (earliest/latest record, character \code{"YYYY-MM-DD HH:MM:SS"} in the
#'   unit's own clock time), \code{$longitude}, \code{$latitude},
#'   \code{$records} (number of records).
#'
#'   \code{coordinate.average} - one row per row of \code{aru.combo}:
#'   \code{$aru.names} (the set as written in \code{aru.combo}),
#'   \code{$longitude}, \code{$latitude} (median of the unique locations of
#'   the ARUs in that set).
#'
#' @details
#' \strong{Location records.} Records are sorted by ARU and date/time. A new
#' location record starts whenever (1) the ARU's \code{longitude}/
#' \code{latitude} differs from its previous record - so an ARU that moves
#' A -> B -> A gets three records, not two - or (2) the gap since its
#' previous record at that location is more than \code{gap.days}.
#' Coordinates must match exactly; any GPS jitter counts as a new location.
#'
#' \strong{Median location.} For each set, the median \code{longitude} and
#' the median \code{latitude} are taken separately over the \emph{unique}
#' \code{aru.name}/\code{longitude}/\code{latitude} combinations of the ARUs
#' in that set - each distinct location counts once, however many records
#' or location records it has. An ARU with many distinct GPS fixes
#' therefore pulls the median toward itself more than an ARU with one.
#'
#' \strong{Unmatched names.} An \code{aru.combo} name not found in
#' \code{data$aru.name} gives a warning naming the closest match (e.g.
#' \code{"NWSO1"} with a letter O vs \code{"NWS01"} with a zero) and is left
#' out of that set's median. A set with no matched names gets \code{NA}.
#'
#' \strong{Header check.} \code{data} must have \code{aru.name}, \code{date},
#' \code{time}, \code{longitude}, \code{latitude}; missing ones stop the
#' function with \code{"data is missing these headers: ..."}. A record whose
#' \code{date}/\code{time} can't be read, or with a missing coordinate, is
#' dropped with a console NOTE.
#'
#' \strong{Dates and times} (follow-up 2026-09-30, per Josh's real run,
#' where every record failed to read): \code{$date} can be a \code{Date}
#' column or text as \code{YYYY-MM-DD}, \code{YYYY/MM/DD}, \code{M/D/YYYY}
#' (US order, as Excel saves it), \code{M-D-YYYY} or \code{YYYY-Mon-DD};
#' \code{$time} can be text \code{HH:MM:SS} or \code{HH:MM}, an Excel time
#' read by \code{readxl} (\code{POSIXct} on 1899-12-31), an
#' \code{hms}/\code{difftime} column, or an Excel fraction of a day.
#' Day-first dates (\code{D/M/YYYY}) aren't supported, since they can't be
#' told apart from US dates. The NOTE for unreadable records now shows an
#' example value.
#'
#' \strong{Follow-up, 2026-09-30, per Josh - group.by and special
#' characters.} New input \code{group.by} (default \code{"aru.name"}) to
#' summarize by \code{"aru.label"} instead - useful when one ARU was
#' redeployed under several labels (e.g. \code{NWS03-1}, \code{NWS03-2}).
#' The required-header check, the location-record breaks, the
#' \code{aru.combo} matching and the first output column all follow it. New
#' input \code{strip.special} (default \code{TRUE}) removes non-ASCII
#' characters from both inputs first. This function has no log, so there is
#' no \code{$strip.special} column.
#'
#' \strong{Follow-up, 2026-10-02, per Josh - accents simplified.} With
#' \code{strip.special = TRUE}, accented letters and common symbols are
#' now simplified instead of dropped (\code{café} -> \code{cafe},
#' \code{×} -> \code{X}, curly quotes -> straight quotes); characters
#' with no plain equivalent (e.g. \code{°}, \code{µ}, \code{™}) are still
#' removed. Column names are cleaned the same way.
#'
#' @seealso \code{\link{batz.merge_sm.logfiles}}
#'
#' @examples
#' \dontrun{
#' data <- read.csv("norcross2.csv")
#' aru.combo <- read.csv("aru.combos.csv")
#' batz.summarize_aruloc.coordinates(data, aru.combo)
#' # coordinate.summary and coordinate.average are now in your workspace
#' }
#'
#' @export
batz.summarize_aruloc.coordinates <- function(data,
                                              aru.combo,
                                              gap.days    = 1,
                                              zero.remove = TRUE,
                                              group.by    = "aru.name",
                                              strip.special = TRUE) {

  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.data.frame(aru.combo) || ncol(aru.combo) < 1) stop("aru.combo must be a data frame with at least one column")
  if (!is.character(group.by) || length(group.by) != 1 || !nzchar(group.by)) {
    stop("group.by must be one column name, e.g. \"aru.name\" or \"aru.label\"")
  }

  ## ---- special characters (per Josh, 2026-09-30) ---------------------------
  data      <- special.strip(data, strip.special)$df
  aru.combo <- special.strip(aru.combo, strip.special)$df

  ## ---- header check ------------------------------------------------------
  ## group.by (per Josh, 2026-09-30) names the ARU identifier column. Inside
  ## the function it is always worked on as aru.name; the output column is
  ## renamed back to group.by at the end.
  required <- c(group.by, "date", "time", "longitude", "latitude")
  ch <- canonicalize.headers(data, required)
  if (length(ch$missing) > 0) {
    stop("data is missing these headers: ", paste(ch$missing, collapse = ", "))
  }
  d <- ch$df[required]
  names(d)[1] <- "aru.name"
  d$aru.name  <- trimws(as.character(d$aru.name))
  d$longitude <- suppressWarnings(as.numeric(d$longitude))
  d$latitude  <- suppressWarnings(as.numeric(d$latitude))

  ## ---- clean ---------------------------------------------------------------
  bad.xy <- is.na(d$longitude) | is.na(d$latitude)
  if (any(bad.xy)) {
    cat("NOTE:", sum(bad.xy), "record(s) with a missing longitude/latitude dropped.\n")
    d <- d[!bad.xy, , drop = FALSE]
  }
  if (zero.remove) {
    zero <- d$longitude == 0 & d$latitude == 0
    if (any(zero)) {
      cat("NOTE:", sum(zero), "record(s) at 0,0 (no GPS fix) dropped.\n")
      d <- d[!zero, , drop = FALSE]
    }
  }
  d$datetime <- aruloc.parse.datetime(d$date, d$time)
  bad.dt <- is.na(d$datetime)
  if (any(bad.dt)) {
    i <- which(bad.dt)[1]
    cat("NOTE:", sum(bad.dt), "record(s) with an unreadable date/time dropped",
        paste0("(e.g. date = '", format(d$date[i]), "', time = '", format(d$time[i]), "').\n"))
    d <- d[!bad.dt, , drop = FALSE]
  }
  if (nrow(d) == 0) stop("no usable records left in data")

  ## ---- location records ----------------------------------------------------
  d <- d[order(d$aru.name, d$datetime), , drop = FALSE]
  n <- nrow(d)
  loc.key   <- paste(d$longitude, d$latitude)
  new.aru   <- c(TRUE, d$aru.name[-1] != d$aru.name[-n])
  new.loc   <- c(TRUE, loc.key[-1] != loc.key[-n])
  gap.secs  <- c(Inf, diff(as.numeric(d$datetime)))
  new.gap   <- gap.secs > gap.days * 86400
  d$loc.id  <- cumsum(new.aru | new.loc | new.gap)

  fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  first <- !duplicated(d$loc.id)
  last  <- !duplicated(d$loc.id, fromLast = TRUE)
  coordinate.summary <- data.frame(
    aru.name       = d$aru.name[first],
    datetime.start = fmt(d$datetime[first]),
    datetime.end   = fmt(d$datetime[last]),
    longitude      = d$longitude[first],
    latitude       = d$latitude[first],
    records        = as.integer(tabulate(d$loc.id)),
    stringsAsFactors = FALSE
  )
  rownames(coordinate.summary) <- NULL
  cat("coordinate.summary:", nrow(coordinate.summary), "location record(s) across",
      length(unique(d$aru.name)), "ARU(s).\n")

  ## ---- median location per set ----------------------------------------------
  sets <- trimws(as.character(aru.combo[[1]]))
  sets <- sets[!is.na(sets) & nzchar(sets)]
  unique.locs <- unique(d[c("aru.name", "longitude", "latitude")])
  aru.all <- unique(d$aru.name)

  med <- lapply(sets, function(s) {
    members <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
    members <- members[nzchar(members)]
    hit <- match(tolower(members), tolower(aru.all))
    if (any(is.na(hit))) {
      for (m in members[is.na(hit)]) {
        close <- aru.all[which.min(utils::adist(tolower(m), tolower(aru.all)))]
        warning("aru.combo name '", m, "' not found in data$", group.by, " (closest: '",
                close, "') - left out of set '", s, "'", call. = FALSE)
      }
    }
    locs <- unique.locs[unique.locs$aru.name %in% aru.all[hit[!is.na(hit)]], ]
    if (nrow(locs) == 0) return(c(NA_real_, NA_real_))
    c(stats::median(locs$longitude), stats::median(locs$latitude))
  })
  coordinate.average <- data.frame(
    aru.names = sets,
    longitude = vapply(med, `[`, numeric(1), 1),
    latitude  = vapply(med, `[`, numeric(1), 2),
    stringsAsFactors = FALSE
  )

  names(coordinate.summary)[1] <- group.by

  result <- list(coordinate.summary = coordinate.summary,
                 coordinate.average = coordinate.average)
  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)
  invisible(result)
}

#' Parse date + time columns into POSIXct (internal helper)
#'
#' Added 2026-09-30 for \code{batz.summarize_aruloc.coordinates()}, after a
#' real file failed because its dates/times weren't in the
#' \code{YYYY-MM-DD}/\code{HH:MM:SS} layout. Accepts:
#' \itemize{
#'   \item date: a \code{Date}/\code{POSIXct} column, or text as
#'     \code{YYYY-MM-DD}, \code{YYYY/MM/DD}, \code{M/D/YYYY} (US order,
#'     as Excel writes it on a US machine), \code{M-D-YYYY} or
#'     \code{YYYY-Mon-DD} (raw SM log layout).
#'   \item time: text as \code{HH:MM:SS} or \code{HH:MM}; a
#'     \code{POSIXct} column (how \code{readxl} reads Excel times, dated
#'     1899-12-31); an \code{hms}/\code{difftime} column; or an Excel time
#'     fraction of a day (e.g. \code{0.8229}).
#' }
#' The first date layout that reads every non-blank value is used. Day-first
#' dates (\code{D/M/YYYY}) are not tried, since they can't be told apart
#' from US month-first dates.
#'
#' @keywords internal
#' @noRd
aruloc.parse.datetime <- function(date, time) {
  ## ---- date -> "YYYY-MM-DD" --------------------------------------------
  if (inherits(date, c("Date", "POSIXt"))) {
    date.txt <- format(as.Date(date), "%Y-%m-%d")
  } else {
    txt <- trimws(as.character(date))
    formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%Y-%b-%d")
    ok <- !is.na(txt) & nzchar(txt)
    date.txt <- rep(NA_character_, length(txt))
    for (f in formats) {
      parsed <- as.Date(txt, format = f)
      yr <- as.integer(format(parsed, "%Y"))
      if (all(!is.na(parsed[ok])) && all(yr[ok] > 1900)) {
        date.txt <- format(parsed, "%Y-%m-%d"); break
      }
    }
    if (all(is.na(date.txt))) {                 # no single layout fits: per value
      for (f in formats) {
        parsed <- as.Date(txt, format = f)
        fill <- is.na(date.txt) & !is.na(parsed) & as.integer(format(parsed, "%Y")) > 1900
        date.txt[fill] <- format(parsed[fill], "%Y-%m-%d")
      }
    }
  }
  ## ---- time -> seconds after midnight -----------------------------------
  if (inherits(time, "POSIXt")) {
    lt <- as.POSIXlt(time)
    secs <- lt$hour * 3600 + lt$min * 60 + floor(lt$sec)
  } else if (inherits(time, "difftime")) {
    secs <- as.numeric(time, units = "secs")
  } else if (is.numeric(time)) {
    secs <- round(ifelse(time >= 0 & time < 1, time * 86400, NA))
  } else {
    txt <- trimws(as.character(time))
    m <- regmatches(txt, regexec("^([0-9]{1,2}):([0-9]{2})(:([0-9]{2}))?", txt))
    secs <- vapply(m, function(p) {
      if (length(p) == 0) return(NA_real_)
      h <- as.numeric(p[2]); mi <- as.numeric(p[3])
      se <- if (nzchar(p[5])) as.numeric(p[5]) else 0
      if (h > 23 || mi > 59 || se > 59) NA_real_ else h * 3600 + mi * 60 + se
    }, numeric(1))
  }
  as.POSIXct(date.txt, tz = "UTC", format = "%Y-%m-%d") + secs
}

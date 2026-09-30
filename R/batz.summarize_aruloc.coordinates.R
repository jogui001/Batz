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
#'   \code{$date} (\code{YYYY-MM-DD}), \code{$time} (\code{HH:MM:SS}),
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
#' @return Invisibly, a named list with two data frames, also auto-assigned
#'   into the calling environment:
#'
#'   \code{coordinate.summary} - one row per location record, ordered by
#'   \code{$aru.name} then \code{$datetime.start}:
#'   \code{$aru.name}, \code{$datetime.start} and \code{$datetime.end}
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
                                              zero.remove = TRUE) {

  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.data.frame(aru.combo) || ncol(aru.combo) < 1) stop("aru.combo must be a data frame with at least one column")

  ## ---- header check ------------------------------------------------------
  required <- c("aru.name", "date", "time", "longitude", "latitude")
  ch <- canonicalize.headers(data, required)
  if (length(ch$missing) > 0) {
    stop("data is missing these headers: ", paste(ch$missing, collapse = ", "))
  }
  d <- ch$df[required]
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
  d$datetime <- as.POSIXct(paste(trimws(d$date), trimws(d$time)),
                           tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  bad.dt <- is.na(d$datetime)
  if (any(bad.dt)) {
    cat("NOTE:", sum(bad.dt), "record(s) with an unreadable date/time dropped.\n")
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
        warning("aru.combo name '", m, "' not found in data$aru.name (closest: '",
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

  result <- list(coordinate.summary = coordinate.summary,
                 coordinate.average = coordinate.average)
  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)
  invisible(result)
}

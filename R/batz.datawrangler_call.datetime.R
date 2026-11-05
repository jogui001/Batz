#' Combine separate date and time vectors into one call date-time
#'
#' Takes two parallel vectors, \code{date} and \code{time} (for example the
#' \code{$date} and \code{$time} columns parsed from recording file names),
#' detects the format of each, parses them, and returns one combined
#' date-time string per element, formatted with \code{time.format.out}.
#' Here "call" is a noun: the time of a bat call (recording).
#'
#' @details
#' \strong{Built-in formats.} Dates: \verb{\%Y\%m\%d}, \verb{\%Y-\%m-\%d},
#' \verb{\%Y/\%m/\%d}, \verb{\%m/\%d/\%Y}, \verb{\%d/\%m/\%Y},
#' \verb{\%m-\%d-\%Y}, \verb{\%d-\%m-\%Y}, \verb{\%m/\%d/\%y},
#' \verb{\%d/\%m/\%y}. Times: \verb{\%H\%M\%S}, \verb{\%H:\%M:\%S},
#' \verb{\%H\%M}, \verb{\%H:\%M}, \verb{\%I:\%M:\%S \%p}, \verb{\%I:\%M \%p}.
#'
#' \strong{Format detection} (done once for \code{date}, once for \code{time}):
#' \itemize{
#'   \item Values are converted to character and trimmed. Blank and NA values
#'     are ignored for detection. If no value is left, the function stops.
#'   \item One format is assumed for the whole vector, not row by row.
#'   \item A format is viable only if its regular expression matches the
#'     WHOLE string of every value and every value parses to a valid
#'     date/time. The full-string check stops a short format (e.g.
#'     \verb{\%H\%M}) from silently reading only the start of a longer value
#'     (e.g. \code{"212144"}).
#'   \item No viable format: the function stops and lists the formats tried.
#'   \item One viable format, or several that give identical values: the
#'     first viable format (in the order above) is used.
#'   \item Several viable formats that disagree on any value (e.g.
#'     \code{"01/02/2025"} as \verb{\%m/\%d/\%Y} vs \verb{\%d/\%m/\%Y}): the
#'     first entry of \code{date.format.in} / \code{time.format.in} that is
#'     viable is used. If none is, the viable formats are printed and the
#'     function stops.
#' }
#' \code{date.format.in} and \code{time.format.in} only break ties between
#' built-in formats; they do not add new formats. Their defaults count the
#' same as values passed by the caller.
#'
#' \strong{Combining.} Parsed dates are formatted as \verb{\%Y-\%m-\%d} and
#' times as \verb{\%H:\%M:\%S}, pasted together, parsed as a UTC date-time and
#' formatted with \code{time.format.out}. Blank or NA elements in \code{date}
#' or \code{time} give NA in the result.
#'
#' @param date Character (or coercible) vector of recorded dates, e.g.
#'   \code{$date}.
#' @param time Character (or coercible) vector of recorded times, e.g.
#'   \code{$time}. Must be the same length as \code{date}, otherwise the
#'   function stops.
#' @param date.format.in Character vector, default \code{c("\%Y\%m\%d")}.
#'   Preferred date format(s), used only when several built-in date formats
#'   fit the data but disagree. See Details.
#' @param time.format.in Character vector, default \code{c("\%H\%M\%S")}.
#'   Same role as \code{date.format.in}, for \code{time}.
#' @param time.format.out Character, default
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%S"}. \code{strftime()}-style format of the
#'   returned date-time strings.
#'
#' @return A character vector the same length as \code{date} and \code{time},
#'   with one combined date-time per element formatted with
#'   \code{time.format.out} (NA where \code{date} or \code{time} is blank or
#'   NA). Nothing is written to disk.
#'
#' @examples
#' \dontrun{
#' vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
#'   vetted.merged$date, vetted.merged$time)
#'
#' ## day-first dates
#' batz.datawrangler_call.datetime(c("01/02/2025", "13/02/2025"),
#'   c("21:21:44", "22:00:00"), date.format.in = "\%d/\%m/\%Y")
#' }
#'
#' @seealso \code{\link{batz.merge_vetted.acoustics}}
#'
#' @export
batz.datawrangler_call.datetime <- function(date, time,
                                             date.format.in = c("%Y%m%d"),
                                             time.format.in = c("%H%M%S"),
                                             time.format.out = "%Y-%m-%d %H:%M:%S") {

  if (length(date) != length(time)) {
    stop("`date` and `time` must be the same length (", length(date), " vs ", length(time), ").")
  }

  ## candidate formats, each paired with a hand-built exact-match regex -
  ## see the @details WARNING above for why the regex check matters
  date.candidates <- list(
    list(fmt = "%Y%m%d",   rx = "^[0-9]{4}[0-9]{2}[0-9]{2}$"),
    list(fmt = "%Y-%m-%d", rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$"),
    list(fmt = "%Y/%m/%d", rx = "^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"),
    list(fmt = "%d/%m/%Y", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"),
    list(fmt = "%m-%d-%Y", rx = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$"),
    list(fmt = "%d-%m-%Y", rx = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$"),
    list(fmt = "%m/%d/%y", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$"),
    list(fmt = "%d/%m/%y", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$")
  )

  time.candidates <- list(
    list(fmt = "%H%M%S",      rx = "^[0-9]{2}[0-9]{2}[0-9]{2}$"),
    list(fmt = "%H:%M:%S",    rx = "^[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%H%M",        rx = "^[0-9]{4}$"),
    list(fmt = "%H:%M",       rx = "^[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%I:%M:%S %p", rx = "^[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2} (AM|PM|am|pm)$"),
    list(fmt = "%I:%M %p",    rx = "^[0-9]{1,2}:[0-9]{1,2} (AM|PM|am|pm)$")
  )

  detect.format <- function(x, label, candidates, requested.format, parse.fun) {
    x <- trimws(as.character(x))
    present <- x[!is.na(x) & nzchar(x)]
    if (length(present) == 0) {
      stop("`", label, "` has no non-blank values to detect a format from.")
    }

    viable <- character(0)
    parsed.by.fmt <- list()
    for (cand in candidates) {
      if (!all(grepl(cand$rx, present))) next
      parsed <- parse.fun(present, cand$fmt)
      if (any(is.na(parsed))) next
      viable <- c(viable, cand$fmt)
      parsed.by.fmt[[cand$fmt]] <- parsed
    }

    if (length(viable) == 0) {
      tried <- vapply(candidates, function(c) c$fmt, character(1))
      stop("Could not detect a `", label, "` format - none of the built-in candidate ",
           "formats matched every value of `", label, "`. Candidates tried: ",
           paste(tried, collapse = ", "), ". Pass ", label,
           ".format.in explicitly if your data uses a different format.")
    }

    chosen <- viable[1]
    if (length(viable) > 1) {
      ref <- parsed.by.fmt[[viable[1]]]
      agree <- all(vapply(viable[-1], function(f) {
        identical(as.character(parsed.by.fmt[[f]]), as.character(ref))
      }, logical(1)))

      if (!agree) {
        match.idx <- which(requested.format %in% viable)
        if (length(match.idx) > 0) {
          chosen <- requested.format[match.idx[1]]
        } else {
          cat("Possible `", label, "` formats (ambiguous - all fit your data but disagree ",
              "on at least one value):\n", sep = "")
          print(viable)
          stop("`", label, "` format is ambiguous - set ", label, ".format.in to one of the ",
               "formats printed above to disambiguate.")
        }
      }
    }

    full.parsed <- parse.fun(x, chosen)
    list(format = chosen, parsed = full.parsed)
  }

  date.detect <- detect.format(date, "date", date.candidates, date.format.in,
                                function(x, fmt) as.Date(x, format = fmt))
  time.detect <- detect.format(time, "time", time.candidates, time.format.in,
                                function(x, fmt) strptime(x, format = fmt, tz = "UTC"))

  date.str <- format(date.detect$parsed, "%Y-%m-%d")
  time.str <- format(time.detect$parsed, "%H:%M:%S")

  combined  <- as.POSIXct(paste(date.str, time.str), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  date.time <- format(combined, time.format.out)

  date.time
}

#' Calculate a combined call date-time from separate $date and $time vectors
#'
#' Given two parallel vectors \code{date} and \code{time} (e.g. the
#' \code{$date}/\code{$time} columns \code{batz.vettedacoustics_merge.format()}
#' parses out of a recording's file name), auto-detects the format each is
#' recorded in, parses them, pastes them back together, and returns one
#' combined date-time character vector formatted per \code{output.format}.
#'
#' \strong{How format detection works} (the spec described the goal but not
#' the mechanics, so a concrete rule had to be built - see the dev script's
#' header comment for the full assumptions list):
#' \itemize{
#'   \item A small, fixed list of common date formats (and, separately,
#'     time formats) is tried against the WHOLE vector at once - one
#'     format is assumed to describe every non-blank value, not a mix
#'     row-by-row.
#'   \item A candidate format is "viable" only if it matches the FULL
#'     string for every value (not just a lenient prefix match - see the
#'     WARNING below) and parses to a real, valid date/time.
#'   \item If exactly one candidate is viable, that's the detected format.
#'   \item If more than one is viable and they parse the data to
#'     DIFFERENT actual values (e.g. \code{"01/02/2025"} as
#'     \verb{\%m/\%d/\%Y} vs \verb{\%d/\%m/\%Y}) - genuine ambiguity - the
#'     viable candidates are printed and the function stops, UNLESS the
#'     currently-set \code{date.format}/\code{time.format} value (its
#'     default counts the same as an explicit override - see the dev
#'     script's assumption #4) is itself one of the viable candidates, in
#'     which case that one is used silently instead.
#'   \item If more than one candidate is viable but they all agree on
#'     every value, there's no real ambiguity - the first viable one is
#'     used.
#'   \item If NO candidate matches at all, that's a separate, always-fatal
#'     error (added as a safety net, not literally specified).
#' }
#'
#' \strong{WARNING - a real correctness trap this avoids:}
#' \code{strptime()}/\code{as.Date()} only require a format to match the
#' BEGINNING of a string, not consume all of it - naively trying
#' \verb{\%H\%M} against a real 6-digit time like \code{"212144"} would
#' "succeed" by reading just the first 4 digits (21:21) and silently
#' discarding \code{"44"}, a wrong answer with no warning. Every candidate
#' format is paired internally with a hand-built regex requiring an EXACT
#' full-string match before it's even considered, specifically to prevent
#' this.
#'
#' Blank/NA elements in \code{date} or \code{time} pass through as
#' \code{NA} in the result.
#'
#' Naming convention (per project preferences):
#' \code{package.family_action.subject()}. This function is
#' \code{batz.datawrangler_call.datetime()}: family = "datawrangler"
#' (general-purpose data-cleanup helpers), action/subject = "call.datetime"
#' - "call" here is a noun (the time of a bat call/recording, standard
#' bioacoustics usage), not a verb.
#'
#' @param date Character or coercible-to-character vector of recorded
#'   dates (e.g. \code{$date}).
#' @param time Character or coercible-to-character vector of recorded
#'   times, the same length as \code{date} (e.g. \code{$time}).
#' @param date.format Character vector, default \code{c("\%Y\%m\%d")}. Format
#'   string(s) (\code{strptime()}/\code{as.Date()} style) to prefer if
#'   \code{date}'s format is genuinely ambiguous between more than one
#'   built-in candidate - see Details.
#' @param time.format Character vector, default \code{c("\%H\%M\%S")}. Same
#'   idea as \code{date.format}, for \code{time}.
#' @param output.format Character, default
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%S"}. \code{strftime()}-style format used to
#'   build the returned combined date-time strings.
#'
#' @return A character vector the same length as \code{date}/\code{time}:
#'   the combined date-time for every element, formatted per
#'   \code{output.format} (\code{NA} for any blank/NA input element).
#'
#' @examples
#' \dontrun{
#' date.time <- batz.datawrangler_call.datetime(vetted.merged$date, vetted.merged$time)
#' vetted.merged$date.time <- date.time
#' }
#'
#' @export
batz.datawrangler_call.datetime <- function(date, time,
                                             date.format = c("%Y%m%d"),
                                             time.format = c("%H%M%S"),
                                             output.format = "%Y-%m-%d %H:%M:%S") {

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
           ".format explicitly if your data uses a different format.")
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
          stop("`", label, "` format is ambiguous - set ", label, ".format to one of the ",
               "formats printed above to disambiguate.")
        }
      }
    }

    full.parsed <- parse.fun(x, chosen)
    list(format = chosen, parsed = full.parsed)
  }

  date.detect <- detect.format(date, "date", date.candidates, date.format,
                                function(x, fmt) as.Date(x, format = fmt))
  time.detect <- detect.format(time, "time", time.candidates, time.format,
                                function(x, fmt) strptime(x, format = fmt, tz = "UTC"))

  date.str <- format(date.detect$parsed, "%Y-%m-%d")
  time.str <- format(time.detect$parsed, "%H:%M:%S")

  combined  <- as.POSIXct(paste(date.str, time.str), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  date.time <- format(combined, output.format)

  date.time
}

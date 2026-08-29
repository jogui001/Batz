# batz.datawrangler_call.datetime.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.datawrangler_*   (general-purpose data-cleanup helpers)
# Action:  call                 (per Josh's own name - "call" here means
#                                 the datetime of a bat CALL/recording, not
#                                 an action verb - see the function name
#                                 itself: batz.datawrangler_call.datetime())
# Subject: datetime
#
# Purpose: given two parallel vectors $date and $time (e.g. the $date/$time
# columns batz.vettedacoustics_merge.format() parses out of a recording's
# file name), auto-detect the format each is recorded in, parse them, paste
# them back together, and return one combined date-time vector formatted
# per `time.format.out`.
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS MADE (spec was open-ended on these - flag for review):
#
#  1. "call" in the function name is read as a NOUN (the time of a bat
#     call/recording - standard bioacoustics usage), not a verb - the
#     spec's own purpose line ("Calculates the call time from the file
#     name from a vetted sheet") and the fact that $date/$time already
#     exist as columns (not something parsed fresh from a filename here)
#     both support this. No rename needed either way - "call.datetime"
#     already fits the package's own action.subject naming slot.
#
#  2. **Format detection is per-VECTOR, not per-element.** "Determine the
#     format of how $date is recorded" is read as: the whole $date column
#     follows ONE consistent format (as real data confirms - see below),
#     not a mix of different formats row-by-row. Every candidate format is
#     checked against every non-blank value in the vector at once.
#
#  3. **What "ambiguous" means, precisely** (the spec doesn't define this,
#     so a concrete rule had to be built): a small, fixed list of common
#     date/time formats is tried against the whole vector. A format is
#     "viable" only if (a) it matches the FULL string for every value (a
#     hand-built exact-match regex per format, not just strptime()'s
#     lenient prefix-match - see the WARNING below) and (b) it parses to a
#     real, valid date/time (rejects e.g. month 13). If exactly one
#     candidate is viable, that's the detected format, no ambiguity. If
#     more than one is viable AND they'd parse the data to DIFFERENT
#     actual dates/times (e.g. "01/02/2025" as %m/%d/%Y vs %d/%m/%Y), that
#     is genuine ambiguity - the viable candidates are printed and the
#     script stops, UNLESS the currently-set date.format.in/time.format.in value
#     is itself one of the viable candidates, in which case that one wins
#     silently. (If more than one candidate is viable but they all agree
#     on every value - which can happen incidentally - there's no real
#     ambiguity and the first viable one is just used.)
#
#  4. **"Unless user defined date.format matches" is read loosely: it
#     checks whatever value date.format.in/time.format.in currently holds -
#     the built-in default (c("%Y%m%d")/c("%H%M%S")) counts as much as an
#     explicit override.** A stricter reading (only count it if the caller
#     literally passed the argument this call, via missing()) was
#     considered and rejected as needless extra complexity here - flagging
#     this reading in case Josh wants the stricter version instead.
#     date.format.in/time.format.in may be a vector of more than one acceptable
#     format (matches the c(...) style Josh gave for the defaults) - the
#     first element that's actually one of the viable candidates wins.
#
#  5. **WARNING - a real correctness trap avoided here:** strptime()/
#     as.Date() only require the format to match the BEGINNING of the
#     string, not consume all of it. Naively trying "%H%M" against a real
#     6-digit time like "212144" would "succeed" by reading just the first
#     4 digits (21:21) and silently discarding "44" - a wrong answer with
#     no warning. Every candidate format below is paired with a hand-built
#     regex that requires an EXACT full-string match before it's even
#     considered, specifically to prevent this.
#
#  6. No format for $date/$time parsed as entirely unrecognizable (matches
#     none of the built-in candidates at all) is a separate, always-fatal
#     error (not literally specified, added as a safety net) - distinct
#     from "ambiguous" (multiple candidates fit); this case is "zero
#     candidates fit."
#
#  7. Blank/NA elements in $date or $time pass through as NA in the
#     returned $date.time (not specified either way - the safest default
#     for a "don't silently misrepresent missing data" utility).
#
#  8. Returns a plain character vector (matches "Returns the merged date
#     time for every element" literally) - not a list/data frame, and no
#     auto-assign-into-caller's-environment side effect, following the
#     same "plain utility" precedent as batz.datawrangler_rename() rather
#     than the list-returning convention used by the merge/load functions.
# ---------------------------------------------------------------------------

## base R only - no package dependencies required

## ===========================================================================
## SECTION 1: format-detection helpers
## ===========================================================================

## Candidate formats are hand-paired with an exact-match regex (see
## WARNING #5 above) rather than derived from a generic token->regex
## mapper - the candidate list is short and fixed, so a hand-built table
## is simpler to read/audit than a general-purpose format parser.
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

## Detects the single format that best describes every non-blank value in
## `x`, among `candidates`. `requested.format` is whatever value the
## caller's date.format.in/time.format.in argument currently holds (default or
## explicit - see assumption #4 above); it's only consulted if genuine
## ambiguity is found. `parse.fun(x, fmt)` must return NA for any element
## that fails to parse under `fmt`. Returns list(format = <chosen fmt>,
## parsed = <parsed values for every element of the ORIGINAL x, NA for
## blanks>).
detect.format <- function(x, label, candidates, requested.format, parse.fun) {
  x <- trimws(as.character(x))
  present <- x[!is.na(x) & nzchar(x)]
  if (length(present) == 0) {
    stop("$", label, " has no non-blank values to detect a format from.")
  }

  viable <- character(0)
  parsed.by.fmt <- list()
  for (cand in candidates) {
    if (!all(grepl(cand$rx, present))) next            # wrong shape entirely
    parsed <- parse.fun(present, cand$fmt)
    if (any(is.na(parsed))) next                        # right shape, not a real date/time
    viable <- c(viable, cand$fmt)
    parsed.by.fmt[[cand$fmt]] <- parsed
  }

  if (length(viable) == 0) {
    tried <- vapply(candidates, function(c) c$fmt, character(1))
    stop("Could not detect a $", label, " format - none of the built-in candidate ",
         "formats matched every value of $", label, ". Candidates tried: ",
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
        cat("Possible $", label, " formats (ambiguous - all fit your data but disagree ",
            "on at least one value):\n", sep = "")
        print(viable)
        stop("$", label, " format is ambiguous - set ", label, ".format.in to one of the ",
             "formats printed above to disambiguate.")
      }
    }
  }

  full.parsed <- parse.fun(x, chosen)   # re-run on the FULL vector; blanks -> NA
  list(format = chosen, parsed = full.parsed)
}

## ===========================================================================
## SECTION 2: config for this test run
## ===========================================================================
dir.load         <- getwd()
load.pattern     <- "vetted.merged.csv"
date.format.in   <- c("%Y%m%d")
time.format.in   <- c("%H%M%S")
time.format.out  <- "%Y-%m-%d %H:%M:%S"

## ===========================================================================
## SECTION 3: load test data
## ===========================================================================
test.file <- list.files(dir.load, pattern = load.pattern, full.names = TRUE)
if (length(test.file) == 0) {
  stop("No file matching \"", load.pattern, "\" found in dir.load (\"", dir.load, "\").")
}
vetted.merged <- read.csv(test.file[1], stringsAsFactors = FALSE, colClasses = "character",
                           check.names = FALSE)
cat("Loaded", nrow(vetted.merged), "row(s) from", basename(test.file[1]), "\n\n")

date <- vetted.merged$date
time <- vetted.merged$time

## ===========================================================================
## SECTION 4: detect + parse $date and $time
## ===========================================================================
if (length(date) != length(time)) {
  stop("$date and $time must be the same length (", length(date), " vs ", length(time), ").")
}

date.detect <- detect.format(date, "date", date.candidates, date.format.in,
                              function(x, fmt) as.Date(x, format = fmt))
cat("Detected $date format:", date.detect$format, "\n")

time.detect <- detect.format(time, "time", time.candidates, time.format.in,
                              function(x, fmt) strptime(x, format = fmt, tz = "UTC"))
cat("Detected $time format:", time.detect$format, "\n\n")

## ===========================================================================
## SECTION 5: paste date + time together per time.format.out
## ===========================================================================
date.str <- format(date.detect$parsed, "%Y-%m-%d")
time.str <- format(time.detect$parsed, "%H:%M:%S")

combined  <- as.POSIXct(paste(date.str, time.str), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
date.time <- format(combined, time.format.out)

cat("--- date.time (first 10) ---\n")
print(head(date.time, 10))
cat("\nany NA?", any(is.na(date.time)), "\n")

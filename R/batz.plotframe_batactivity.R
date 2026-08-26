#' Summarize vetted bat-acoustic detections into a plotting-ready frame
#'
#' Given a fully-assembled data frame of vetted bat-acoustic detections
#' (the output of \code{\link{batz.vettedacoustics_merge.format}}, further
#' joined with a call-datetime column and a \code{$sunregion} column - see
#' Details), builds a summary table of detection counts and the
#' earliest/latest call time (in minutes since the start of that
#' monitoring night) per species, monitoring night, and detector - the
#' shape you'd feed straight into a plotting function.
#'
#' \strong{Required input columns.} `data` must have every one of:
#' \code{filename}, \code{date.mon}, \code{manid}, \code{autoid.kp},
#' \code{autoid.sb}, \code{lat}, \code{serial}, \code{lon},
#' \code{aru.name}, \code{date}, \code{time}, \code{call.datetime},
#' \code{sunregion}. The first eleven, plus \code{$call.datetime}, come
#' straight out of \code{\link{batz.vettedacoustics_merge.format}} - no
#' renaming needed (an earlier version of this function required
#' \code{$call.time} instead, which didn't match that function's own
#' \code{$call.datetime} output column; standardized on
#' \code{$call.datetime} across both, per Josh, 2026-08-26). \code{
#' $sunregion} isn't produced by any existing \code{batz} function at
#' all - it has to be joined in separately (e.g. from an
#' \code{*arulist.csv} via \code{$aru.name}). This function expects a
#' fully-assembled input; it does no loading or joining itself.
#'
#' \strong{Steps.} If any required header is missing, stops immediately
#' and lists every missing header by name. If \code{duplicates.remove =
#' TRUE} (default), exact duplicate rows are dropped from \code{data}.
#' Every row gets a helper \code{$obs = 1}. \code{$mins2.noon} is computed
#' per row as the number of minutes from noon on the date named by
#' \code{date.groupby} (the start of that monitoring night, since these
#' are nocturnal-animal records - a night starting at noon on
#' \code{$date.mon} and ending at noon the next calendar day) to
#' \code{$call.datetime} for that row - both columns' formats are
#' auto-detected against a small built-in candidate list rather than
#' assumed fixed (see below).
#'
#' A per-\code{spp.id}/\code{date.groupby}/\code{aru.groupby} summary
#' (\code{plfr.batsummary}) is always built first, with columns
#' \code{$spp.id} (the VALUES of whichever column \code{spp.id} names),
#' \code{$date} (the values of the \code{date.groupby} column),
#' \code{$aru.groupby} (the values of the \code{aru.groupby} column),
#' \code{$obs} (count of detections in that group), and
#' \code{$mins2.noon.min}/\code{$mins2.noon.max} (the smallest/largest
#' \code{$mins2.noon} in that group).
#'
#' If \code{alldetections = FALSE}, \code{$vetting.type} (the literal
#' NAME of the column \code{spp.id} points at, e.g. \code{"manid.sb"}) is
#' added to every row and this per-species table is returned as-is.
#'
#' If \code{alldetections = TRUE} (default), rows where the \code{spp.id}
#' column is \code{"noise"} (if \code{trim.noise = TRUE}, default) or
#' \code{"NoID"} (if \code{trim.noid = TRUE}, default) - case-insensitive -
#' are additionally removed from a COPY of the data, and the SAME
#' summarization is repeated on that trimmed copy, grouped only by
#' \code{date.groupby}/\code{aru.groupby} (species collapsed), with
#' \code{$spp.id} forced to the literal string \code{"All Detections"}.
#' That table is row-bound onto the per-species table from above, then
#' \code{$vetting.type} is added to every row and the combined table is
#' returned. Because the trim only happens for this second, collapsed
#' table, \code{"noise"}/\code{"NoID"} still appear as their own rows in
#' the per-species breakdown (so you can see how much noise/unidentified
#' activity there was per night/detector) while the \code{"All
#' Detections"} total reflects only genuine wildlife activity. This
#' "build both, row-bind them" behavior was the most open-ended part of
#' the original spec and is flagged as a real interpretive call - please
#' confirm it's what's wanted (see the dev script's header comment for
#' the alternative readings that were considered and set aside).
#'
#' \strong{Date/time format auto-detection.} Neither \code{date.groupby}
#' (e.g. raw \code{$date.mon}/\code{$monitoringnight} values like
#' \code{"6/26/2026"}) nor \code{call.datetime}'s own values (which may
#' already be in \code{\link{batz.datawrangler_call.datetime}}-style
#' format OR a raw recorder \code{Timestamp} string like
#' \code{"5/17/2026 21:21"}, confirmed against real data) can be assumed
#' to already be in one fixed format. \code{date.groupby} is parsed by
#' reusing \code{\link{batz.datawrangler_call.datetime}}'s own format
#' auto-detection (fed alongside a constant noon time). \code{
#' call.datetime} is parsed via a small internal regex-guarded candidate
#' list covering both shapes above - same exact-full-string-match-before-
#' parsing pattern used elsewhere in this package, to avoid a silent
#' partial-string parse.
#'
#' Unlike the merge/load-style \code{batz} functions, this one has no
#' bare-call auto-assign side effect and writes nothing to disk - the
#' original spec's own wording just says "return plfr.batsummary" (one
#' data frame), so it's returned directly.
#'
#' @param data A data frame with every column listed above already
#'   present (see Details for how to assemble one).
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from \code{data} before summarizing.
#' @param spp.id Character, default \code{"manid.sb"}. Name of the column
#'   in \code{data} holding the species identifier to summarize by.
#' @param date.groupby Character, default \code{"date.mon"}. Name of the
#'   column in \code{data} holding the date/interval to summarize by.
#'   (Named \code{"date.groupby"} rather than the originally-specced
#'   \code{"date"}, to match the sibling \code{aru.groupby} parameter and
#'   avoid colliding with \code{data}'s own separate \code{$date} column -
#'   told Josh about the rename.)
#' @param aru.groupby Character, default \code{"aru.name"}. Name of the
#'   column in \code{data} holding the detector unit to summarize by.
#' @param alldetections Logical, default \code{TRUE}. See Details.
#' @param trim.noise Logical, default \code{TRUE}. When \code{alldetections
#'   = TRUE}, exclude rows where the \code{spp.id} column is
#'   \code{"noise"} (case-insensitive) from the \code{"All Detections"}
#'   summary.
#' @param trim.noid Logical, default \code{TRUE}. When \code{alldetections
#'   = TRUE}, exclude rows where the \code{spp.id} column is
#'   \code{"NoID"} (case-insensitive) from the \code{"All Detections"}
#'   summary. (Named \code{"trim.noid"} rather than the originally-specced
#'   \code{"trim.noID"}, to match the identically-purposed parameter
#'   already shipped in \code{\link{batz.vettedacoustics_merge.format}}.)
#'
#' @return A data frame, \code{plfr.batsummary}, with columns
#'   \code{$spp.id}, \code{$date}, \code{$aru.groupby}, \code{$obs},
#'   \code{$mins2.noon.min}, \code{$mins2.noon.max}, \code{$vetting.type}.
#'
#' @examples
#' \dontrun{
#' plfr.batsummary <- batz.plotframe_batactivity(vetted.merged)
#' plfr.batsummary <- batz.plotframe_batactivity(vetted.merged, alldetections = FALSE)
#' }
#'
#' @export
batz.plotframe_batactivity <- function(data,
                                        duplicates.remove = TRUE,
                                        spp.id = "manid.sb",
                                        date.groupby = "date.mon",
                                        aru.groupby = "aru.name",
                                        alldetections = TRUE,
                                        trim.noise = TRUE,
                                        trim.noid = TRUE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  required.headers <- c("filename", "date.mon", "manid", "autoid.kp",
                         "autoid.sb", "lat", "serial", "lon", "aru.name",
                         "date", "time", "call.datetime", "sunregion")
  missing.headers <- setdiff(required.headers, names(data))
  if (length(missing.headers) > 0) {
    stop("`data` is missing required header(s): ",
         paste(missing.headers, collapse = ", "))
  }

  for (colname in c(spp.id, date.groupby, aru.groupby)) {
    if (!(colname %in% names(data))) {
      stop("`", colname, "` (from spp.id/date.groupby/aru.groupby) is not ",
           "a column of `data`.")
    }
  }

  if (duplicates.remove) {
    data <- data[!duplicated(data), , drop = FALSE]
  }

  data$obs <- 1

  ## noon-of-monitoring-night anchor, reusing batz.datawrangler_call.datetime()'s
  ## own date-format auto-detection
  noon.anchor <- as.POSIXct(
    batz.datawrangler_call.datetime(date = as.character(data[[date.groupby]]),
                                     time = rep("120000", nrow(data))),
    tz = "UTC")

  ## $call.datetime format auto-detection - regex-guarded candidate list, same
  ## exact-full-string-match pattern already used in
  ## batz.datawrangler_call.datetime to avoid a silent partial-match
  call.datetime.candidates <- list(
    list(fmt = "%Y-%m-%d %H:%M:%S", rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%Y-%m-%d %H:%M",    rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}$")
  )
  parse.call.datetime <- function(x) {
    x <- trimws(as.character(x))
    present <- x[!is.na(x) & nzchar(x)]
    chosen <- NULL
    if (length(present) > 0) {
      for (cand in call.datetime.candidates) {
        if (!all(grepl(cand$rx, present))) next
        parsed <- as.POSIXct(present, format = cand$fmt, tz = "UTC")
        if (any(is.na(parsed))) next
        chosen <- cand$fmt
        break
      }
      if (is.null(chosen)) {
        stop("Could not auto-detect the format of $call.datetime - none of the ",
             "built-in candidate formats (",
             paste(vapply(call.datetime.candidates, function(c) c$fmt, character(1)), collapse = ", "),
             ") matched every non-blank value.")
      }
    }
    as.POSIXct(x, format = chosen, tz = "UTC")
  }
  call.dt <- parse.call.datetime(data$call.datetime)
  data$.mins2.noon <- as.numeric(difftime(call.dt, noon.anchor, units = "mins"))

  safe.min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe.max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  build.summary <- function(df, spp.override = NULL) {
    spp.vals <- if (!is.null(spp.override)) rep(spp.override, nrow(df)) else as.character(df[[spp.id]])
    date.vals <- as.character(df[[date.groupby]])
    aru.vals  <- as.character(df[[aru.groupby]])

    key <- paste(spp.vals, date.vals, aru.vals, sep = "\r")
    ag.obs <- tapply(df$obs, key, sum)
    ag.min <- tapply(df$.mins2.noon, key, safe.min)
    ag.max <- tapply(df$.mins2.noon, key, safe.max)

    keys  <- names(ag.obs)
    parts <- strsplit(keys, "\r", fixed = TRUE)

    out <- data.frame(
      spp.id         = vapply(parts, `[`, character(1), 1),
      date           = vapply(parts, `[`, character(1), 2),
      aru.groupby    = vapply(parts, `[`, character(1), 3),
      obs            = as.numeric(ag.obs[keys]),
      mins2.noon.min = as.numeric(ag.min[keys]),
      mins2.noon.max = as.numeric(ag.max[keys]),
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    out
  }

  if (!alldetections) {
    plfr.batsummary <- build.summary(data)
    plfr.batsummary$vetting.type <- spp.id
    return(plfr.batsummary)
  }

  ## alldetections = TRUE: per-species breakdown (on all rows, pre-trim)
  ## + a collapsed "All Detections" summary on the noise/NoID-trimmed
  ## data, combined into one table - see Details
  species.summary <- build.summary(data)

  trimmed <- data
  if (trim.noise) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noise"), , drop = FALSE]
  }
  if (trim.noid) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noid"), , drop = FALSE]
  }

  all.summary <- build.summary(trimmed, spp.override = "All Detections")

  plfr.batsummary <- rbind(species.summary, all.summary)
  plfr.batsummary$vetting.type <- spp.id
  plfr.batsummary <- plfr.batsummary[order(plfr.batsummary$aru.groupby,
                                            plfr.batsummary$date,
                                            plfr.batsummary$spp.id), ]
  rownames(plfr.batsummary) <- NULL

  plfr.batsummary
}

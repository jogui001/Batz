## Shared internal helpers for the batz plot functions.
## Added 2026-10-02, per Josh: species panel labels from fig.list
## $facet.label, and plotting data made by batz.generate_plotframe.bat()
## with pool.interval other than "1day".

#' Species facet-panel labels from fig.list $facet.label
#'
#' Used when fig.list $facet = "sppid". \code{fmt} is the row's
#' $facet.label: any value \code{batz.batusa_recode.names()} accepts for
#' \code{batname.format.out} ("common_name", "scientific_name", "code4",
#' "code6", "both", or the older "common"/"latin"/"scientific"). Blank or NA
#' means "common_name". A panel whose recoded label comes back blank (e.g.
#' "All detections" with a species-only format) keeps its original name.
#' An unusable value stops with an error naming the fig.list row.
#' @return Named character vector: labels, named by \code{levels}.
#' @keywords internal
#' @noRd
plot.facet.labels <- function(levels, fmt, job.label, fn.name) {
  fmt <- if (length(fmt) == 0 || is.na(fmt)) "" else trimws(gsub('"', "", as.character(fmt)))
  if (!nzchar(fmt)) fmt <- "common_name"
  res <- tryCatch(
    batz.batusa_recode.names(levels, batname.format.out = fmt),
    error = function(e) {
      stop(sprintf("%s: fig.list row '%s' has $facet.label = '%s', which can't be used as a species name format.\n%s",
                   fn.name, job.label, fmt, conditionMessage(e)), call. = FALSE)
    })
  res <- as.character(res)
  blank <- is.na(res) | !nzchar(trimws(res))
  res[blank] <- levels[blank]
  names(res) <- levels
  res
}

#' Collapse pooled plot-frame rows to one row per date
#'
#' \code{batz.generate_plotframe.bat(pool.interval = )} below one day gives
#' several rows per species/group/date (one per time bin). Plots that draw
#' one bar or crossbar per date need one row per date, so rows that are
#' identical in every column except the counts/times and pool times are
#' combined: \code{$obs} summed, \code{$mins2.noon.min} min,
#' \code{$mins2.noon.max} max; \code{$pool.start}/\code{$pool.end} become
#' the earliest start / latest end. Data without \code{$pool.start}, or
#' with nothing to combine (the default "1day"), is returned unchanged.
#' @keywords internal
#' @noRd
plot.collapse.pools <- function(data, fn.name) {
  if (!all(c("pool.start", "pool.end") %in% names(data)) || nrow(data) == 0) return(data)
  value.cols <- intersect(c("obs", "mins2.noon.min", "mins2.noon.max", "pool.start", "pool.end"),
                          names(data))
  key.cols <- setdiff(names(data), value.cols)
  key <- do.call(paste, c(lapply(data[key.cols], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
                          sep = "\r"))
  if (!anyDuplicated(key)) return(data)
  first <- !duplicated(key)
  out <- data[first, , drop = FALSE]
  g <- factor(key, levels = unique(key))
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  if ("obs" %in% names(data)) out$obs <- as.vector(tapply(num(data$obs), g, sum, na.rm = TRUE))
  if ("mins2.noon.min" %in% names(data)) {
    out$mins2.noon.min <- as.vector(tapply(num(data$mins2.noon.min), g, function(v) if (all(is.na(v))) NA else min(v, na.rm = TRUE)))
  }
  if ("mins2.noon.max" %in% names(data)) {
    out$mins2.noon.max <- as.vector(tapply(num(data$mins2.noon.max), g, function(v) if (all(is.na(v))) NA else max(v, na.rm = TRUE)))
  }
  ## pool.start / pool.end are text ("YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS"),
  ## which sort correctly as text
  if ("pool.start" %in% names(data)) out$pool.start <- as.vector(tapply(as.character(data$pool.start), g, min))
  if ("pool.end" %in% names(data))   out$pool.end   <- as.vector(tapply(as.character(data$pool.end), g, max))
  cat(sprintf("NOTE: %s - data has %s pools shorter than one day; combined %d row(s) into %d (one per species/group/date) for plotting.\n",
              fn.name, if ("pool.interval" %in% names(data)) paste0("'", data$pool.interval[1], "'") else "",
              nrow(data), nrow(out)))
  rownames(out) <- NULL
  out
}

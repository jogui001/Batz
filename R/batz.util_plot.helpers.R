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
plotutil_facet_labels <- function(levels, fmt, job.label, fn.name) {
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
#' combined: \code{$observations.count} (or old \code{$obs}) summed, \code{$mins2.noon.min} min,
#' \code{$mins2.noon.max} max; \code{$pool.start}/\code{$pool.end} become
#' the earliest start / latest end. Data without \code{$pool.start}, or
#' with nothing to combine (the default "1day"), is returned unchanged.
#' @keywords internal
#' @noRd
plotutil_collapse_pools <- function(data, fn.name) {
  if (!all(c("pool.start", "pool.end") %in% names(data)) || nrow(data) == 0) return(data)
  value.cols <- intersect(c("observations.count", "obs", "mins2.noon.min", "mins2.noon.max", "pool.start", "pool.end"),
                          names(data))
  key.cols <- setdiff(names(data), value.cols)
  key <- do.call(paste, c(lapply(data[key.cols], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
                          sep = "\r"))
  if (!anyDuplicated(key)) return(data)
  first <- !duplicated(key)
  out <- data[first, , drop = FALSE]
  g <- factor(key, levels = unique(key))
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  if ("observations.count" %in% names(data)) {
    out$observations.count <- as.vector(tapply(num(data$observations.count), g, sum, na.rm = TRUE))
  }
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


## ---------------------------------------------------------------------------
## fig.list format, 2026-10-09 (per Josh): $facet -> $facet.header,
## new $facet.plot, $facet.set = preset or ordered list, new file names.
## Shared by batz.plotactivity_observations(), batz.plotdetections_first.last()
## and batz.plotactivity_heatmap2().
## ---------------------------------------------------------------------------

#' Preset facet sets for fig.list $facet.set (internal)
#'
#' Named list; each name is a preset's short hand (matched case-
#' insensitively) and each value the species in plotting order. Add a new
#' preset by adding one more entry. \code{plotutil_facet_preset_aliases} maps
#' other spellings onto a preset name.
#' @keywords internal
#' @noRd
plotutil_facet_presets <- list(
  NE = c("Big brown bat", "Eastern red bat", "Eastern small-footed myotis", "Hoary bat",
         "Little brown bat", "Northern long-eared bat", "Silver-haired bat", "Tri-colored bat")
)
plotutil_facet_preset_aliases <- c("new england" = "NE")

#' Old fig.list headers -> current ones (internal)
#'
#' \code{$facet} -> \code{$facet.header}, \code{$Alldect} ->
#' \code{$all.dectections}; a missing \code{$facet.plot} is added as TRUE
#' (the old behaviour - one faceted plot). Old \code{$facet.panel},
#' \code{$facpan} and \code{$plot.order} columns are ignored. Prints a
#' WARNING for each change.
#' @keywords internal
#' @noRd
plotutil_figlist_legacy <- function(fig.list, fn.name) {
  if (!is.data.frame(fig.list)) return(fig.list)
  std <- standardize.headers(names(fig.list))
  rename.old <- function(old, new) {
    if (old %in% std && !(standardize.headers(new) %in% std)) {
      old <- names(fig.list)[std == old][1]
      names(fig.list)[standardize.headers(names(fig.list)) == standardize.headers(old)] <<- new
      std[std == standardize.headers(old)] <<- standardize.headers(new)
      cat(sprintf("WARNING: %s - fig.list has the old header $%s - used as $%s.\n", fn.name, old, new))
    }
  }
  rename.old("facet", "facet.header")
  rename.old("alldect", "all.dectections")
  if (!("facet_plot" %in% std)) {
    fig.list$facet.plot <- rep(TRUE, nrow(fig.list))
    cat(sprintf("WARNING: %s - fig.list has no $facet.plot column - every row is drawn as one faceted plot (facet.plot = TRUE).\n", fn.name))
  }
  ignored <- names(fig.list)[std %in% c("facet_panel", "facpan", "plot_order")]
  if (length(ignored) > 0) {
    cat(sprintf("NOTE: %s - fig.list column(s) %s are no longer used - $facet.set sets the facets and their order.\n",
                fn.name, paste0("$", ignored, collapse = ", ")))
  }
  fig.list
}

#' Resolve a fig.list $facet.set value into ordered facets (internal)
#'
#' \code{x} is a preset short hand (e.g. \code{"NE"}) or a list separated
#' by \code{";"} (or \code{","}) of species in any format
#' \code{batz.batusa_recode.names()} reads, \code{"AllDet"} / \code{"All
#' detections"}, \code{"40khzmyo"}, or preset names - mixed freely, e.g.
#' \code{"AllDet;NE"} or \code{"AllDet;epfu;labo;lano"}. The order written
#' is the plotting order. Blank means the \code{"NE"} preset.
#' @return Character vector of facet names (common names, plus
#'   \code{"All detections"} / \code{"40khzmyo"}), in order, no duplicates.
#' @keywords internal
#' @noRd
plotutil_resolve_facet_set <- function(x, job.label, fn.name) {
  x <- if (length(x) == 0 || is.na(x)) "" else trimws(as.character(x))
  if (!nzchar(x)) {
    cat(sprintf("NOTE: %s - '%s' has a blank $facet.set - using the \"NE\" preset.\n", fn.name, job.label))
    x <- "NE"
  }
  tokens <- trimws(strsplit(x, "[;,]")[[1]])
  tokens <- tokens[nzchar(tokens)]
  simple <- function(v) gsub("[^a-z0-9]", "", tolower(v))
  preset.names <- names(plotutil_facet_presets)
  alias.map <- c(stats::setNames(preset.names, simple(preset.names)),
                 stats::setNames(unname(plotutil_facet_preset_aliases), simple(names(plotutil_facet_preset_aliases))))
  out <- character(0)
  for (tk in tokens) {
    st <- simple(tk)
    if (st %in% names(alias.map)) {
      out <- c(out, plotutil_facet_presets[[alias.map[[st]]]])
    } else if (st %in% c("alldet", "alldetections", "alldetection", "alldect")) {
      out <- c(out, "All detections")
    } else if (st %in% c("40khzmyo", "40kmyo")) {
      out <- c(out, "40khzmyo")
    } else {
      rec <- suppressWarnings(utils::capture.output(
        v <- batz.batusa_recode.names(tk, batname.format.out = "common_name")))
      if (any(grepl("did not match", rec))) {
        ## unmatched - recode passes the input back unchanged
        cat(sprintf("NOTE: %s - '%s' $facet.set entry '%s' is not a preset or a recognized species - kept as written.\n",
                    fn.name, job.label, tk))
      }
      out <- c(out, v)
    }
  }
  out[!duplicated(simple(out))]
}

#' Short facet ID for file names (internal)
#'
#' \code{$facet.header = "sppid"}: species -> four-letter code
#' (\code{code4}), "All detections" -> \code{"AllDet"}. Anything without a
#' code is kept, with characters unsafe in file names removed.
#' @keywords internal
#' @noRd
plotutil_facet_id <- function(x, facet.header = "sppid") {
  x <- as.character(x)
  simple <- gsub("[^a-z0-9]", "", tolower(x))
  out <- x
  if (identical(tolower(trimws(facet.header)), "sppid")) {
    rec <- utils::capture.output(code <- batz.batusa_recode.names(x, batname.format.out = "code4"))
    out <- as.character(code)
  }
  out[simple %in% c("alldetections", "alldet")] <- "AllDet"
  out[simple %in% c("40khzmyo", "40kmyo")] <- "40kMyo"
  out
}

#' Save name for a fig.list plot (internal)
#'
#' Faceted (\code{facet.plot = TRUE}):
#' \verb{<plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png};
#' one plot per facet (\code{FALSE}):
#' \verb{<plot.name>_<facet>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#' Spaces become "-" and characters Windows doesn't allow are removed. If
#' the name is longer than \code{max.len} characters it is shortened, in
#' this order, stopping as soon as it fits: (1) same-year date range drops
#' the year from the last date; (2) each word in the facet ID(s) cut to 6
#' characters; (3) each word in plot.set cut to 6; (4) each word in
#' plot.name cut to 6. A "word" is a run of letters/digits.
#' @param facet.ids Facet IDs in plotted order (first and last are used
#'   when \code{faceted = TRUE}; the single ID otherwise).
#' @return File name (no folder).
#' @keywords internal
#' @noRd
plotutil_figlist_filename <- function(plot.name, facet.ids, faceted, plot.set, date.start, date.end,
                                  max.len = 100, timestamp = format(Sys.time(), "%Y%m%d%H%M%S"),
                                  fn.name = "batz") {
  clean <- function(x) {
    x <- paste(trimws(as.character(x)), collapse = "+")
    x <- gsub('[\\\\/:*?"<>|]+', "", x)
    x <- gsub("\\s+", "-", x)
    if (!nzchar(x)) "all" else x
  }
  cut.words <- function(x, n = 6) {
    m <- gregexpr("[A-Za-z0-9]+", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(w) substr(w, 1, n))
    x
  }
  name.tok  <- clean(plot.name)
  set.tok   <- clean(plot.set)
  ids       <- vapply(facet.ids, clean, character(1))
  first.tok <- ids[1]
  last.tok  <- ids[length(ids)]
  d1 <- format(as.Date(date.start), "%Y%m%d")
  d2 <- format(as.Date(date.end), "%Y%m%d")
  build <- function() {
    facet.tok <- if (isTRUE(faceted)) paste0("F_", first.tok, "to", last.tok) else first.tok
    paste0(paste(name.tok, facet.tok, set.tok, paste0(d1, "to", d2), timestamp, sep = "_"), ".png")
  }
  f <- build()
  if (nchar(f) > max.len && identical(substr(d1, 1, 4), substr(d2, 1, 4))) {
    d2 <- substr(d2, 5, 8); f <- build()
  }
  if (nchar(f) > max.len) { first.tok <- cut.words(first.tok); last.tok <- cut.words(last.tok); f <- build() }
  if (nchar(f) > max.len) { set.tok <- cut.words(set.tok); f <- build() }
  if (nchar(f) > max.len) { name.tok <- cut.words(name.tok); f <- build() }
  if (nchar(f) > max.len) {
    cat(sprintf("NOTE: %s - file name is still %d characters after shortening: %s\n", fn.name, nchar(f), f))
  }
  f
}

#' Split a faceted plot entry into one entry per facet (internal)
#'
#' For \code{$facet.plot = FALSE}. \code{entry$pd$facet.panel.value} is a
#' factor whose levels are the facet labels in plotted order;
#' \code{facet.raw} are the matching raw facet names. Returns a list of
#' entries, each with \code{pd} cut to one facet, \code{facet.ids} set to
#' that facet's ID and \code{facet.plot = FALSE}. Facets with no data are
#' skipped with a NOTE.
#' @keywords internal
#' @noRd
plotutil_split_facets <- function(entry, facet.raw, facet.header, fn.name) {
  labs <- levels(entry$pd$facet.panel.value)
  out <- list()
  for (i in seq_along(labs)) {
    sub <- entry$pd[as.character(entry$pd$facet.panel.value) == labs[i], , drop = FALSE]
    if (nrow(sub) == 0) {
      cat(sprintf("NOTE: %s - '%s' facet '%s' has no data in range - no plot for it.\n",
                  fn.name, entry$job.label, labs[i]))
      next
    }
    sub$facet.panel.value <- factor(as.character(sub$facet.panel.value), levels = labs[i])
    e <- entry
    e$pd <- sub
    e$facet.ids <- plotutil_facet_id(facet.raw[i], facet.header)
    e$facet.plot <- FALSE
    out[[length(out) + 1]] <- e
  }
  out
}

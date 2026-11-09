## Shared internal helpers for the batz plot functions.
## Added 2026-10-02, per Josh: species panel labels from fig.list
## $facet.label, and plotting data made by batz.generate_plotframe.bat()
## with pool.interval other than "1day".

#' Species facet-panel labels from fig.list $facet.label_format (internal)
#'
#' Used when fig.list \code{$facet.header = "sppid"}. Recodes each species name
#' in \code{levels} with \code{batz.batusa_recode.names()} to the format in
#' \code{fmt}.
#' @param levels Species names (the facet levels).
#' @param fmt The fig.list row's \code{$facet.label_format}: any value
#'   \code{batz.batusa_recode.names()} accepts for \code{batname.format.out}
#'   (\code{"common_name"}, \code{"scientific_name"}, \code{"code4"},
#'   \code{"code6"}, \code{"both"}, ...). Quotes are dropped; blank or
#'   \code{NA} means \code{"common_name"}.
#' @param job.label,fn.name fig.list row label and calling function, used in
#'   messages.
#' @param alt The fig.list row's \code{$facet.label} (alternative labels;
#'   see \code{plotutil_facet_relabel()}).
#' @details A panel whose recoded label is blank (e.g. \code{"All detections"}
#'   with a species-only format) keeps its original name. A \code{fmt} that
#'   cannot be used stops with an error naming the fig.list row.
#' @return Named character vector of labels, named by \code{levels}.
#' @keywords internal
#' @noRd
plotutil_facet_labels <- function(levels, fmt, job.label, fn.name, alt = "") {
  fmt <- if (length(fmt) == 0 || is.na(fmt)) "" else trimws(gsub('"', "", as.character(fmt)))
  if (!nzchar(fmt)) fmt <- "common_name"
  res <- tryCatch(
    batz.batusa_recode.names(levels, batname.format.out = fmt),
    error = function(e) {
      stop(sprintf("%s: fig.list row '%s' has $facet.label_format = '%s', which can't be used as a species name format.\n%s",
                   fn.name, job.label, fmt, conditionMessage(e)), call. = FALSE)
    })
  res <- as.character(res)
  blank <- is.na(res) | !nzchar(trimws(res))
  res[blank] <- levels[blank]
  names(res) <- levels
  ## other names a $facet.label pair may use for a panel: species codes and names, AllDet, 40kMyo
  aka <- lapply(levels, function(l) l)
  for (f in c("code4", "code6", "common_name", "scientific_name")) {
    v <- tryCatch(as.character(suppressMessages(suppressWarnings(batz.batusa_recode.names(levels, batname.format.out = f)))),
                  error = function(e) rep(NA_character_, length(levels)))
    if (length(v) == length(levels)) for (i in seq_along(levels)) if (!is.na(v[i]) && nzchar(v[i])) aka[[i]] <- c(aka[[i]], v[i])
  }
  k <- gsub("[^a-z0-9]", "", tolower(levels))
  for (i in seq_along(levels)) {
    if (k[i] %in% c("alldetections", "alldet")) aka[[i]] <- c(aka[[i]], "AllDet", "All detections")
    if (grepl("40k", k[i])) aka[[i]] <- c(aka[[i]], "40khzmyo", "40kMyo")
  }
  plotutil_facet_relabel(res, alt, job.label, fn.name, aka = aka)
}

#' Collapse pooled plot-frame rows to one row per date (internal)
#'
#' \code{batz.generate_plotframe.bat()} with a \code{pool.interval} shorter than
#' one day gives several rows per species/group/date (one per time bin). Plots
#' that draw one bar or crossbar per date need one row per date.
#' @param data Plot-frame data frame.
#' @param fn.name Calling function, used in the NOTE.
#' @details Rows that are identical in every column except
#'   \code{$observations.count} (or old \code{$obs}), \code{$mins2.noon.min},
#'   \code{$mins2.noon.max}, \code{$pool.start} and \code{$pool.end} are
#'   combined: counts are summed, \code{$mins2.noon.min} takes the minimum,
#'   \code{$mins2.noon.max} the maximum, \code{$pool.start} the earliest and
#'   \code{$pool.end} the latest value. Prints a NOTE with the row counts.
#'   Data without \code{$pool.start}/\code{$pool.end}, with no rows, or with
#'   nothing to combine (e.g. \code{pool.interval = "1day"}) is returned
#'   unchanged.
#' @return \code{data}, with one row per combined group and row names reset.
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
#' Named list: each name is a preset short hand (matched ignoring case and
#' punctuation) and each value the species, as common names, in plotting
#' order. \code{NE} (New England) holds the eight New England species. Add a
#' preset by adding an entry. \code{plotutil_facet_preset_aliases} maps other
#' spellings (e.g. \code{"new england"}) to a preset name.
#' @keywords internal
#' @noRd
plotutil_facet_presets <- list(
  NE = c("Big brown bat", "Eastern red bat", "Eastern small-footed myotis", "Hoary bat",
         "Little brown bat", "Northern long-eared bat", "Silver-haired bat", "Tri-colored bat")
)
plotutil_facet_preset_aliases <- c("new england" = "NE")

#' Old fig.list headers to current ones (internal)
#'
#' Accepts fig.list tables that use old headers.
#' @param fig.list fig.list data frame (anything else is returned unchanged).
#' @param fn.name Calling function, used in messages.
#' @details
#' \itemize{
#'   \item \code{$facet} is used as \code{$facet.header};
#'   \item \code{$Alldect} and the misspelled \code{$all.dectections} are used
#'     as \code{$all.detections};
#'   \item a missing \code{$facet.plot} is added as \code{TRUE} (one faceted
#'     plot per row);
#'   \item old \code{$facet.panel}, \code{$facpan} and \code{$plot.order}
#'     columns are ignored (\code{$facet.set} sets the facets and their order).
#' }
#' Each rename or added column prints a WARNING; ignored columns print a NOTE.
#' A rename is skipped when the current header is already present. Headers
#' are matched after \code{standardize.headers()}.
#' @return \code{fig.list} with current headers.
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
  rename.old("alldect", "all.detections")
  rename.old("all_dectections", "all.detections")  # spelling fix 2026-11-04, per Josh
  fig.list <- plotutil_figlist_labelformat(fig.list, fn.name)
  std <- standardize.headers(names(fig.list))
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
#' @param x A preset short hand (e.g. \code{"NE"}) or a list separated by
#'   \code{";"} (or \code{","}) of species in any format
#'   \code{batz.batusa_recode.names()} reads, \code{"AllDet"} /
#'   \code{"All detections"}, \code{"40khzmyo"}, or preset names, mixed
#'   freely (e.g. \code{"AllDet;NE"} or \code{"AllDet;epfu;labo;lano"}). The
#'   order written is the plotting order. Blank means the \code{"NE"} preset
#'   (with a NOTE).
#' @param job.label,fn.name fig.list row label and calling function, used in
#'   messages.
#' @param alt The fig.list row's \code{$facet.label} (alternative labels;
#'   see \code{plotutil_facet_relabel()}).
#' @details Species are recoded to common names. An entry that is neither a
#'   preset nor a recognized species is kept as written, with a NOTE.
#'   Duplicates (ignoring case and punctuation) are dropped.
#' @return Character vector of facet names (common names, plus
#'   \code{"All detections"} / \code{"40khzmyo"}), in order.
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
#' @param x Facet names.
#' @param facet.header The fig.list \code{$facet.header}; default \code{"sppid"}.
#' @details With \code{facet.header = "sppid"} species are recoded to their
#'   four-letter code (\code{code4}). In every case \code{"All detections"}
#'   becomes \code{"AllDet"} and \code{"40khzmyo"} becomes \code{"40kMyo"};
#'   anything else is kept as is.
#' @return Character vector of IDs, same length as \code{x}.
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
#' Faceted (\code{faceted = TRUE}):
#' \verb{[<project.name>_]<plot.name>_F_<facet.first>to<facet.last>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png};
#' one plot per facet (\code{FALSE}):
#' \verb{[<project.name>_]<plot.name>_<facet>_<plot.set>_<YYYYMMDD>to<YYYYMMDD>_<TIMESTAMP>.png}.
#' @param plot.name,plot.set Name parts (several values are joined with
#'   \code{"+"}).
#' @param facet.ids Facet IDs in plotted order (first and last are used when
#'   \code{faceted = TRUE}; the first one otherwise).
#' @param faceted \code{TRUE} for a faceted plot.
#' @param date.start,date.end Date range of the plot.
#' @param max.len Maximum file-name length; default 100.
#' @param timestamp Default \code{format(Sys.time(), "\%Y\%m\%d\%H\%M\%S")}.
#' @param fn.name Calling function, used in the NOTE; default \code{"batz"}.
#' @param project.name Put first in the file name, followed by \code{"_"};
#'   default \code{""} (left out).
#' @details Spaces become \code{"-"}, characters Windows does not allow are
#'   removed, and an empty part becomes \code{"all"}. A name longer than
#'   \code{max.len} is shortened in this order, stopping as soon as it fits:
#'   (1) when both dates are in the same year, the year is dropped from the
#'   end date; (2) each word in the facet IDs is cut to 6 characters; (3) each
#'   word in \code{plot.set} is cut to 6; (4) each word in \code{plot.name} is
#'   cut to 6. A word is a run of letters and digits. If it is still too long
#'   a NOTE is printed.
#' @return File name (no folder).
#' @keywords internal
#' @noRd
plotutil_figlist_filename <- function(plot.name, facet.ids, faceted, plot.set, date.start, date.end,
                                  max.len = 100, timestamp = format(Sys.time(), "%Y%m%d%H%M%S"),
                                  fn.name = "batz", project.name = "") {
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
    parts <- c(name.tok, facet.tok, set.tok, paste0(d1, "to", d2), timestamp)
    if (length(project.name) == 1 && !is.na(project.name) && nzchar(trimws(project.name))) parts <- c(clean(project.name), parts)
    paste0(paste(parts, collapse = "_"), ".png")
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
#' Used for \code{$facet.plot = FALSE}.
#' @param entry Plot entry; \code{entry$pd$facet.panel.value} is a factor whose
#'   levels are the facet labels in plotted order.
#' @param facet.raw Raw facet names matching those levels.
#' @param facet.header The fig.list \code{$facet.header}, passed to
#'   \code{plotutil_facet_id()}.
#' @param fn.name Calling function, used in messages.
#' @return List of entries, each with \code{pd} cut to one facet,
#'   \code{facet.ids} set to that facet's ID and \code{facet.plot = FALSE}.
#'   Facets with no data are skipped with a NOTE.
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

#' Column of aes.default that holds the custom style (internal)
#'
#' @param aes.default Plot-options (aes.default) data frame.
#' @param aes.style Name of the custom-style column (the plot functions'
#'   default is \code{"override.value"}).
#' @return \code{aes.style} when that column exists. When
#'   \code{"override.value"} is asked for and only the old misspelled
#'   \code{"overide.value"} column exists, \code{"overide.value"}.
#'   \code{NA} when neither exists or \code{aes.style} is not a single value.
#' @keywords internal
#' @noRd
plotutil_aes_style_col <- function(aes.default, aes.style) {
  if (is.null(aes.style) || length(aes.style) != 1 || is.na(aes.style)) return(NA_character_)
  if (aes.style %in% names(aes.default)) return(aes.style)
  if (identical(aes.style, "override.value") && "overide.value" %in% names(aes.default)) return("overide.value")
  NA_character_
}

#' $myso = TRUE: Indiana bat facet, or MYLU and MYSO pooled as LUSO (internal)
#'
#' Called when a fig.list row has \code{$myso = TRUE}.
#' @param spp.common Common name of each record.
#' @param spp.plot Species to plot.
#' @param facpan Facets, in order.
#' @param present Logical, one per record: \code{TRUE} when the record is in
#'   the plotted data (plot set and date range).
#' @param fn.name,job.label Calling function and fig.list row label, used in
#'   the NOTE.
#' @details When little brown bat (MYLU) records are present, every MYLU and
#'   MYSO record is recoded to LUSO (little brown bat / Indiana bat), and LUSO
#'   replaces MYLU/MYSO in \code{spp.plot} and \code{facpan} (at the place of
#'   the first of them, or added at the end); a NOTE gives the number of
#'   records pooled. Otherwise Indiana bat is kept and added to
#'   \code{facpan} and \code{spp.plot} if it is not there already.
#' @return List with \code{spp.common}, \code{spp.plot} and \code{facpan}.
#' @keywords internal
#' @noRd
plotutil_myso_luso <- function(spp.common, spp.plot, facpan, present, fn.name, job.label) {
  nm <- batz.batusa_recode.names(c("MYLU", "MYSO", "LUSO"), batname.format.out = "common_name")
  lc <- tolower(trimws(spp.common))
  present <- !is.na(present) & present
  swap.in <- function(v, new, old) {
    l <- tolower(trimws(v))
    pos <- which(l %in% tolower(old))
    if (length(pos) == 0) return(unique(c(v, new)))
    v[pos[1]] <- new
    if (length(pos) > 1) v <- v[-pos[-1]]
    unique(v)
  }
  if (any(present & lc == tolower(nm[1]))) {
    hit <- lc %in% tolower(nm[1:2])
    spp.common[hit] <- nm[3]
    spp.plot <- swap.in(spp.plot, nm[3], nm)
    facpan <- swap.in(facpan, nm[3], nm)
    cat(sprintf("NOTE: %s - '%s': $myso = TRUE and little brown bat (MYLU) is present - %d MYLU/MYSO record(s) plotted together as LUSO (%s).\n",
                fn.name, job.label, sum(hit), nm[3]))
  } else {
    if (!(tolower(nm[2]) %in% tolower(facpan))) facpan <- c(facpan, nm[2])
    if (!(tolower(nm[2]) %in% tolower(spp.plot))) spp.plot <- c(spp.plot, nm[2])
  }
  list(spp.common = spp.common, spp.plot = spp.plot, facpan = facpan)
}

#' Key for matching plotopts parameter names (internal)
#'
#' Lower case with all spaces removed, so \code{"Yaxe.trans"},
#' \code{" yaxe.trans "} and \code{"yaxe. trans"} all match \code{"yaxe.trans"}.
#' Used both to check for required \code{$parameter} rows and to read them.
#' @param x Character vector of parameter names.
#' @return Character vector of keys.
#' @keywords internal
#' @noRd
plotutil_param_key <- function(x) tolower(gsub("\\s+", "", as.character(x)))


#' Collect the \code{...} settings of a plot function call (internal)
#'
#' Like \code{list(...)}, but empty arguments (e.g. from a trailing comma,
#' \code{f(x = 1, )}) are skipped instead of stopping with
#' "argument is missing, with no default".
#' @keywords internal
#' @noRd
plotutil_dots <- function(...) {
  n <- ...length()
  if (n == 0) return(list())
  ex <- as.list(substitute(list(...)))[-1]
  nm <- names(ex); if (is.null(nm)) nm <- rep("", n)
  out <- list()
  for (i in seq_len(n)) {
    if (identical(ex[[i]], quote(expr = ))) next          # empty argument (trailing comma)
    v <- list(...elt(i)); names(v) <- nm[i]
    out <- c(out, v)
  }
  if (all(!nzchar(names(out)))) names(out) <- NULL
  out
}


#' Settings typed into a plot function call (internal)
#'
#' Every Batz plot function takes plot settings in \code{...}
#' (e.g. \code{legend.position = "bottom"}). They are used before fig.list
#' and plotopts. \code{plotutil_plot_inputs()} turns \code{list(...)} into a
#' named list of text values (a vector becomes one ";"-separated string),
#' keyed by \code{plotutil_param_key()}; \code{plotutil_input_get()} returns
#' one value or \code{NULL}; \code{plotutil_plot_inputs_check()} prints a
#' NOTE naming the settings used and any the function does not know;
#' \code{plotutil_required_left()} drops required plotopts rows given in
#' the call.
#' @keywords internal
#' @noRd
plotutil_plot_inputs <- function(dots, fn) {
  if (!length(dots)) return(list())
  nm <- names(dots)
  if (is.null(nm) || any(is.na(nm) | !nzchar(nm)))
    stop(fn, ': settings given in the function call must be named, e.g. legend.position = "bottom".', call. = FALSE)
  out <- lapply(dots, function(v) if (is.null(v) || length(v) == 0) NA_character_ else paste(as.character(v), collapse = ";"))
  names(out) <- plotutil_param_key(nm)
  attr(out, "given") <- nm
  out
}

#' @rdname plotutil_plot_inputs
#' @keywords internal
#' @noRd
plotutil_input_get <- function(inputs, param) {
  if (!length(inputs)) return(NULL)
  v <- inputs[[plotutil_param_key(param)]]
  if (is.null(v) || is.na(v)) NULL else v
}

#' @keywords internal
#' @noRd
plotutil_plot_inputs_check <- function(inputs, known, fn) {
  if (!length(inputs)) return(invisible(NULL))
  nm <- attr(inputs, "given")
  bad <- nm[!plotutil_param_key(nm) %in% plotutil_param_key(known)]
  ok <- setdiff(nm, bad)
  if (length(ok)) cat(sprintf("NOTE: %s - settings from the function call (used before fig.list and plotopts): %s\n",
                              fn, paste(ok, collapse = ", ")))
  if (length(bad)) cat(sprintf("NOTE: %s - setting(s) in the function call that this function does not use: %s\n",
                               fn, paste(bad, collapse = ", ")))
  invisible(NULL)
}

#' @keywords internal
#' @noRd
plotutil_required_left <- function(required, inputs) required[!plotutil_param_key(required) %in% names(inputs)]


#' Built-in plot settings (plotopts) of a plot function (internal)
#'
#' Every plot function that takes \code{aes.default} uses its built-in
#' settings table when \code{aes.default} is not given (\code{NULL}). The
#' tables are kept in Batz_reference_db (tabs \code{PLOTOPTS_*}, listed in
#' its \code{database.names} tab as \code{plotopts.<function name>}) and
#' carried by the package in \verb{R/sysdata.rda}
#' (\code{\link{batz.util_refdb.sync}}). A NOTE says when they are used. A
#' data frame given as \code{aes.default} is used instead, and the built-in
#' table is not read.
#' @param fn Plot function name, e.g. \code{"batz.plotactivity_observations"}.
#' @keywords internal
#' @noRd
plotutil_default_plotopts <- function(fn) {
  d <- batz.db(paste0("plotopts.", fn))
  cat(sprintf("NOTE: %s - no aes.default given: the built-in settings (Batz_reference_db) are used.\n", fn))
  d
}


#' fig.list cells "NA" (heading not used by that plot type) become NA (internal)
#'
#' A fig.list made with \code{batz.generate_figlist()} has every heading for
#' every plot type, with \code{"NA"} where a heading is not used by that
#' row's plot type. Text cells that are \code{"NA"} (any case, spaces
#' trimmed) are set to \code{NA}, so they are treated as blank: the setting
#' then comes from plotopts.
#' @keywords internal
#' @noRd
plotutil_figlist_na <- function(fig.list) {
  if (!is.data.frame(fig.list)) return(fig.list)
  for (cl in names(fig.list)) {
    v <- fig.list[[cl]]
    if (is.character(v) || is.factor(v)) {
      v <- as.character(v); v[!is.na(v) & toupper(trimws(v)) == "NA"] <- NA
      fig.list[[cl]] <- v
    }
  }
  fig.list
}


#' fig.list rows to plot: $plot TRUE or 1 (internal)
#'
#' Keeps the rows whose $plot is TRUE or 1 (text, number or logical; case
#' and spaces ignored); FALSE, any other entry or a blank is skipped. With
#' no $plot column every row is kept. Prints a NOTE either way. Non-data-
#' frames (e.g. NULL) are returned unchanged.
#' @keywords internal
#' @noRd
plotutil_figlist_plot <- function(fig.list, fn) {
  if (!is.data.frame(fig.list)) return(fig.list)
  j <- match("plot", tolower(trimws(names(fig.list))))
  if (is.na(j)) {
    cat(sprintf("NOTE: %s - fig.list has no $plot column: every row is plotted.\n", fn))
    return(fig.list)
  }
  v <- toupper(trimws(as.character(fig.list[[j]])))
  keep <- !is.na(v) & v %in% c("TRUE", "1")
  cat(sprintf("NOTE: %s - %d of %d fig.list row(s) have $plot = TRUE or 1 and are plotted%s.\n", fn, sum(keep), length(keep),
              if (any(!keep)) "; the others are skipped" else ""))
  out <- fig.list[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Old fig.list $facet.label (a name format) read as $facet.label_format (internal)
#'
#' Until 2026-11-09 $facet.label held the species name format
#' (common_name, code4, ...). It is now $facet.label_format, and $facet.label
#' holds alternative labels. A fig.list with $facet.label but no
#' $facet.label_format, whose $facet.label cells are all name formats or
#' blank / "NA", has that column renamed (WARNING).
#' @keywords internal
#' @noRd
plotutil_figlist_labelformat <- function(fig.list, fn.name) {
  if (!is.data.frame(fig.list)) return(fig.list)
  std <- standardize.headers(names(fig.list))
  if (!"facet_label" %in% std || "facet_label_format" %in% std) return(fig.list)
  j <- which(std == "facet_label")[1]
  v <- tolower(trimws(gsub('"', "", as.character(fig.list[[j]]))))
  v <- v[!is.na(v) & nzchar(v) & v != "na"]
  fmts <- c("common_name", "scientific_name", "code4", "code6", "both", "common", "latin", "scientific")
  if (all(v %in% fmts)) {                                    # name formats, or all blank / "NA"
    names(fig.list)[j] <- "facet.label_format"
    cat(sprintf("WARNING: %s - fig.list has $facet.label but no $facet.label_format%s - used as $facet.label_format (the new name); $facet.label is now for alternative labels.\n",
                fn.name, if (length(v)) sprintf(" (it holds name formats: %s)", paste(unique(v), collapse = ", ")) else ""))
  }
  fig.list
}


#' Text of one fig.list cell, "" when absent / NA / "NA" (internal)
#' @keywords internal
#' @noRd
plotutil_job_text <- function(job, name) {
  if (is.null(job) || !name %in% names(job)) return("")
  v <- trimws(as.character(job[[name]])[1])
  if (is.na(v) || toupper(v) == "NA") "" else v
}


#' Alternative panel labels from fig.list $facet.label (internal)
#'
#' \code{alt} is a ";" list: either \code{name=label} pairs (a name is matched
#' to a panel's name, its current label or one of \code{aka} (e.g. species
#' codes), ignoring case, spaces, dots and underscores), or labels in panel
#' order. Panels not given a label keep theirs. A
#' NOTE names pairs that match no panel or a count that doesn't match.
#' @param labels Named character vector: current labels, named by panel.
#' @return \code{labels} with the alternative labels in place.
#' @keywords internal
#' @noRd
plotutil_facet_relabel <- function(labels, alt, job.label, fn.name, aka = NULL) {
  alt <- if (length(alt) == 0 || is.na(alt[1])) "" else trimws(as.character(alt[1]))
  if (!nzchar(alt) || toupper(alt) == "NA") return(labels)
  key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  parts <- trimws(strsplit(alt, ";", fixed = TRUE)[[1]]); parts <- parts[nzchar(parts)]
  if (any(grepl("=", parts, fixed = TRUE))) {
    for (pr in parts) {
      kv <- trimws(strsplit(pr, "=", fixed = TRUE)[[1]])
      if (length(kv) < 2 || !nzchar(kv[2])) next
      i <- which(key(names(labels)) == key(kv[1]) | key(labels) == key(kv[1]) |
                   vapply(seq_along(labels), function(j) !is.null(aka) && key(kv[1]) %in% key(aka[[j]]), logical(1)))
      if (!length(i)) cat(sprintf("NOTE: %s - '%s': $facet.label '%s' matches no panel.\n", fn.name, job.label, kv[1]))
      labels[i] <- paste(kv[-1], collapse = "=")
    }
  } else {
    if (length(parts) != length(labels))
      cat(sprintf("NOTE: %s - '%s': $facet.label has %d label(s) for %d panel(s) - %s.\n", fn.name, job.label, length(parts),
                  length(labels), if (length(parts) < length(labels)) "the rest keep their names" else "the extra labels are not used"))
    n <- min(length(parts), length(labels))
    if (n) labels[seq_len(n)] <- parts[seq_len(n)]
  }
  labels
}


#' Saved-chart size from $plot.dimensions (internal)
#'
#' The value is taken from the function call, then the fig.list row
#' ($plot.dimensions, or the old spelling $plot.dimentions). Blank or "NA":
#' NULL (the plotopts plot.width / plot.height are used). A size name
#' (case and spaces ignored) or "<width> x <height>" in inches gives
#' c(width, height); anything else gives NULL with a NOTE.
#' @keywords internal
#' @noRd
plotutil_plot_dimensions <- function(job, inputs, job.label, fn.name) {
  SIZES <- list("1/3 page" = c(6, 3.5), "1/2 page" = c(6, 4), "2/3 page" = c(6, 6), "1 page" = c(6, 7.5),
                "1/3 ppt" = c(6, 3), "1/2 ppt" = c(12, 3), "2/3 ppt" = c(8, 6), "1 ppt" = c(12, 6))
  v <- NULL
  for (nm in c("plot.dimensions", "plot.dimentions")) {
    if (is.null(v)) v <- plotutil_input_get(inputs, nm)
  }
  if (is.null(v)) for (nm in c("plot.dimensions", "plot.dimentions")) {
    t <- plotutil_job_text(job, nm)
    if (nzchar(t)) { v <- t; break }
  }
  v <- if (is.null(v)) "" else trimws(as.character(v)[1])
  if (is.na(v) || !nzchar(v) || toupper(v) == "NA") return(NULL)
  key <- function(x) gsub("\\s+", "", tolower(x))
  hit <- match(key(v), key(names(SIZES)))
  if (!is.na(hit)) return(SIZES[[hit]])
  m <- regmatches(key(v), regexec("^([0-9]*\\.?[0-9]+)(in)?x([0-9]*\\.?[0-9]+)(in)?$", key(v)))[[1]]
  if (length(m)) return(c(as.numeric(m[2]), as.numeric(m[4])))
  cat(sprintf("NOTE: %s - '%s': $plot.dimensions '%s' is not one of %s or \"<width> x <height>\" - the plotopts plot.width / plot.height are used.\n",
              fn.name, job.label, v, paste0('"', names(SIZES), '"', collapse = ", ")))
  NULL
}

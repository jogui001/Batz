#' Search, merge, and de-duplicate raw ARU/habitat-assessment data files
#'
#' Scans a folder (and, optionally, its sub-folders) for \code{.csv} and
#' \code{.xlsx} files, sorts them into three categories by file name (or, for
#' a workbook whose name ends in \code{"ARUdeployments.xlsx"}, by sheet name),
#' merges each category's files into a single data frame, removes duplicate
#' rows, and reports how many were removed.
#'
#' @param dir.load Character, default \code{getwd()}. Folder to search.
#' @param load.pattern Character vector, default \code{c("*.csv", "*.xlsx")}.
#'   File-name patterns (wildcards \code{*} and \code{?}, case ignored) of the
#'   files to scan. Only \code{.csv} and \code{.xlsx} files are read.
#' @param dir.sub Logical, default \code{FALSE}. \code{TRUE}: also search every
#'   sub-folder of \code{dir.load}.
#' @param log.file Logical, default \code{FALSE}. \code{TRUE}: the returned list
#'   also has \code{$arumeta.mergelog} (see Value).
#' @param strip.special Logical, default \code{TRUE}. \code{TRUE}: simplifies
#'   accented letters and common symbols and removes other non-ASCII characters
#'   from headers and text data after loading (see Details). Files are always
#'   read with a UTF-8/Latin-1 fallback, so a stray degree sign can't stop
#'   them loading.
#'
#' @return Invisibly, a named list with \code{$aru.visit}, \code{$aru.quad},
#'   \code{$aru.20m}: each a merged, de-duplicated data frame. A category with
#'   no matching files is left out of the list (so \code{result$aru.quad} is
#'   \code{NULL}). When a category has two or more sources, all its columns
#'   are character; a single source keeps the column types it was read with.
#'
#'   With \code{log.file = TRUE}, also \code{$arumeta.mergelog}: one row per
#'   duplicate-row or duplicate-column action, with \code{$filename} (the
#'   source file, or \code{"filename [sheet: sheetname]"} for a workbook
#'   sheet), \code{$event.type} (\code{"duplicated row"} or \code{"duplicated
#'   column"}), \code{$action} (\code{"deletion"} for identical copies dropped,
#'   \code{"merging"} for differing copies combined), \code{$count} (number of
#'   extra rows/columns, not counting the one kept), \code{$strip.special}
#'   (\code{"FALSE"} when not selected, \code{"TRUE NONE"} when nothing was
#'   found, else \code{"TRUE ; <header>; ..."}: the standardized headers of
#'   that file/sheet where characters were changed),
#'   \code{$accented.letters.header} and \code{$removed.symbols.header}
#'   (number of unique column names with a character simplified / removed),
#'   and \code{$accented.letters.data} and \code{$removed.symbols.data}
#'   (number of unique data values with a character simplified / removed -
#'   \code{cafe} with an accent in 500 rows counts once). The four counts are
#'   \code{NA} when \code{strip.special = FALSE}. Only actions that did
#'   something are logged, so a file with no duplicates has no log rows (even
#'   if special characters were changed in it).
#'
#'   Each non-\code{NULL} element is also assigned, under its own name, into
#'   the environment the function was called from.
#'
#' @details
#' \strong{Categories.} A file (or sheet) goes to the first category whose
#' short, case-insensitive text its name contains: \code{"sitevis"} for
#' \code{aru.visit} (this also catches a sheet misspelled \code{"sitevist"}),
#' \code{"quad"} for \code{aru.quad} (quadrat habitat assessment), \code{"20m"}
#' for \code{aru.20m} (20 m plot habitat assessment). Files that match none
#' are skipped. Only a workbook whose name ends in \code{"ARUdeployments.xlsx"}
#' (case ignored) is read sheet by sheet and sorted by sheet name; any other
#' \code{.xlsx} (first sheet) is sorted by its file name, like a \code{.csv}.
#'
#' \strong{Each file or sheet, in order:}
#' \enumerate{
#'   \item Read: a \code{.csv} with \code{special.read.csv()} (falls back to
#'     Latin-1 when a file isn't valid UTF-8); a workbook with \code{readxl}.
#'   \item With \code{strip.special = TRUE}, \code{special.strip()} cleans the
#'     headers and the text columns: accented letters and common symbols are
#'     simplified (an accented "e" becomes "e", the hybrid sign in
#'     \code{Quercus X bebbiana} becomes "X", curly quotes become straight
#'     quotes) and characters with no plain equivalent (e.g. degree and micro
#'     signs) are removed.
#'   \item A leading UTF-8 byte-order mark is removed from the column names,
#'     so a column from a CSV and from a workbook line up.
#'   \item Headers are standardized with \code{standardize.headers()}: trimmed,
#'     every run of non-alphanumeric characters becomes one underscore, lower
#'     case (\code{"Site Name"} becomes \code{site_name}).
#'   \item A column name that now appears more than once (e.g. "Personnel"
#'     twice on the site-visit form, or two names that differ only in case or
#'     punctuation) is compared row by row, treating \code{NA} and "" as the
#'     same: identical copies are dropped; otherwise they are merged into one
#'     column, taking the non-blank value and joining two different non-blank
#'     values with \code{"; "}.
#' }
#'
#' \strong{Merging and duplicates.} Within a category, the sources are bound
#' together as character columns; a column missing from a source is filled
#' with \code{NA}. Column names are never changed by R (\code{check.names =
#' FALSE}). Exact full-row duplicates (base \code{duplicated()}) are then
#' removed, and the number removed is printed for each category. Each row is
#' tagged internally with its source file (not returned) so removed rows can
#' be logged against that file.
#'
#' @examples
#' \dontrun{
#' result <- batz.merge_aru.meta("path/to/raw/data", dir.sub = TRUE)
#' result$aru.visit
#' result$aru.quad
#' result$aru.20m
#'
#' result <- batz.merge_aru.meta("path/to/raw/data", log.file = TRUE)
#' result$arumeta.mergelog
#' }
#'
#' @export
batz.merge_aru.meta <- function(dir.load = getwd(),
                                       load.pattern     = c("*.csv", "*.xlsx"),
                                       dir.sub          = FALSE,
                                       log.file         = FALSE,
                                       strip.special    = TRUE) {

  ## convert a plain wildcard/glob suffix pattern (or vector of them) into
  ## one combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  log.rows <- list()
  special.headers <- list()   # per source label: headers where special characters were removed
  special.counts <- list()    # per source label: simplified/removed counts (2026-10-02)
  add.log <- function(filename, event.type, action, count) {
    if (!log.file || count <= 0) return(invisible(NULL))
    hdrs <- special.headers[[filename]]
    if (is.null(hdrs)) hdrs <- character(0)
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filename = filename, event.type = event.type, action = action, count = as.integer(count),
      special.log.cols(strip.special, hdrs, special.counts[[filename]]),
      stringsAsFactors = FALSE)
  }

  resolve.dup.columns <- function(df, source.label = "") {
    nm <- names(df)
    dup.names <- unique(nm[duplicated(nm)])
    if (length(dup.names) == 0) return(df)

    keep <- rep(TRUE, ncol(df))
    for (dn in dup.names) {
      idx <- which(nm == dn)
      cols <- lapply(idx, function(i) {
        v <- as.character(df[[i]])
        ifelse(is.na(v) | v == "", NA_character_, v)
      })

      same <- all(sapply(cols[-1], function(v) identical(v, cols[[1]])))

      if (same) {
        cat("  [", source.label, "] duplicate column '", dn, "' (", length(idx),
            " copies) - identical content, dropped ", length(idx) - 1,
            " extra copy(ies)\n", sep = "")
        keep[idx[-1]] <- FALSE
        add.log(source.label, "duplicated column", "deletion", length(idx) - 1)
      } else {
        merged <- cols[[1]]
        for (i in 2:length(cols)) {
          this.col <- cols[[i]]
          conflict <- !is.na(merged) & !is.na(this.col) & merged != this.col
          fill     <- is.na(merged) & !is.na(this.col)
          merged[fill]     <- this.col[fill]
          merged[conflict] <- paste(merged[conflict], this.col[conflict], sep = "; ")
        }
        cat("  [", source.label, "] duplicate column '", dn, "' (", length(idx),
            " copies) - content differs, merged into one column\n", sep = "")
        df[[idx[1]]] <- merged
        keep[idx[-1]] <- FALSE
        add.log(source.label, "duplicated column", "merging", length(idx) - 1)
      }
    }
    df[keep]
  }

  bind.fill <- function(df.list) {
    df.list <- df.list[!sapply(df.list, is.null)]
    if (length(df.list) == 0) return(NULL)
    if (length(df.list) == 1) return(as.data.frame(df.list[[1]], stringsAsFactors = FALSE, check.names = FALSE))

    all.cols <- unique(unlist(lapply(df.list, names)))
    df.list <- lapply(df.list, function(d) {
      d <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE, check.names = FALSE)
      missing.cols <- setdiff(all.cols, names(d))
      for (mc in missing.cols) d[[mc]] <- NA_character_
      d[all.cols]
    })
    do.call(rbind, df.list)
  }

  strip.bom <- function(df) {
    names(df) <- gsub("\ufeff", "", names(df), fixed = TRUE)
    df
  }

  category.patterns <- c(aru.visit = "sitevis", aru.quad = "quad", aru.20m = "20m")

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  buckets <- setNames(vector("list", length(category.patterns)), names(category.patterns))

  add.to.bucket <- function(cat.name, df, source.label, stripped.headers = character(0),
                            stripped.counts = NULL) {
    special.counts[[source.label]] <<- stripped.counts
    special.headers[[source.label]] <<- standardize.headers(gsub("\ufeff", "", stripped.headers, fixed = TRUE))
    df <- strip.bom(df)
    ## header standardization (per Josh, 2026-09-14 project preference) -
    ## reverses this function's prior "preserve raw headers verbatim" design
    ## decision; see @details "Header standardization" above. Runs after BOM
    ## stripping but BEFORE within-source duplicate-column resolution, since
    ## standardizing can itself introduce new name collisions to resolve.
    names(df) <- standardize.headers(names(df))
    df <- resolve.dup.columns(df, source.label)
    df[[".source.file"]] <- source.label
    buckets[[cat.name]][[length(buckets[[cat.name]]) + 1]] <<- df
    cat("  matched", source.label, "->", cat.name, "\n")
  }

  classify <- function(name.lower) {
    hit <- names(category.patterns)[sapply(category.patterns, function(p) grepl(p, name.lower, ignore.case = TRUE))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  for (f in all.files) {
    fname <- basename(f)

    if (grepl("\\.csv$", fname, ignore.case = TRUE)) {
      cat.name <- classify(fname)
      if (!is.na(cat.name)) {
        df <- special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
        s.strip <- special.strip(df, strip.special); df <- s.strip$df
        add.to.bucket(cat.name, df, fname, s.strip$headers, s.strip$counts)
      }
    } else if (grepl("\\.xlsx$", fname, ignore.case = TRUE)) {
      if (grepl("ARUdeployments\\.xlsx$", fname, ignore.case = TRUE)) {
        sheets <- readxl::excel_sheets(f)
        for (s in sheets) {
          cat.name <- classify(s)
          if (!is.na(cat.name)) {
            df <- suppressMessages(as.data.frame(
              readxl::read_excel(f, sheet = s, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
            s.strip <- special.strip(df, strip.special); df <- s.strip$df
            add.to.bucket(cat.name, df, paste0(fname, " [sheet: ", s, "]"), s.strip$headers, s.strip$counts)
          }
        }
      } else {
        cat.name <- classify(fname)
        if (!is.na(cat.name)) {
          df <- suppressMessages(as.data.frame(
            readxl::read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
          s.strip <- special.strip(df, strip.special); df <- s.strip$df
          add.to.bucket(cat.name, df, fname, s.strip$headers, s.strip$counts)
        }
      }
    }
  }

  result <- list()
  for (cat.name in names(category.patterns)) {
    merged <- bind.fill(buckets[[cat.name]])
    if (is.null(merged)) {
      cat("\n", cat.name, ": no matching files found\n", sep = "")
      result[[cat.name]] <- NULL
      next
    }
    data.cols <- setdiff(names(merged), ".source.file")
    dup.mask <- duplicated(merged[data.cols])
    n.dup <- sum(dup.mask)

    if (n.dup > 0) {
      removed.by.file <- table(merged$.source.file[dup.mask])
      for (fn in names(removed.by.file)) {
        add.log(fn, "duplicated row", "deletion", as.integer(removed.by.file[[fn]]))
      }
    }

    cat("\n", cat.name, ": ", n.dup, " duplicate row(s) removed\n", sep = "")
    result[[cat.name]] <- merged[!dup.mask, data.cols, drop = FALSE]
  }

  if (log.file) {
    result$arumeta.mergelog <- if (length(log.rows) > 0) {
      do.call(rbind, log.rows)
    } else {
      data.frame(filename = character(0), event.type = character(0),
                 action = character(0), count = integer(0), special.log.cols.empty(),
                 stringsAsFactors = FALSE)
    }
  }

  caller.env <- parent.frame()
  for (nm in names(result)) {
    if (!is.null(result[[nm]])) assign(nm, result[[nm]], envir = caller.env)
  }

  invisible(result)
}

#' Search, merge, and de-duplicate raw ARU/habitat-assessment data files
#'
#' Scans a directory (and, optionally, its subdirectories) for \code{.csv} and
#' \code{.xlsx} files, sorts them into three categories by file name (or, for
#' a workbook whose name ends in \code{"ARUdeployments.xlsx"}, by sheet name),
#' merges each category's files into a single data frame, removes duplicate
#' rows, and reports how many were removed.
#'
#' @param dir.load Character. Directory to search. Defaults to the current
#'   working directory (\code{getwd()}).
#' @param load.pattern Character vector, default \code{c("*.csv", "*.xlsx")}:
#'   the file-name suffix pattern(s) (plain wildcard/glob style - \code{"*"}
#'   as a leading wildcard, everything else literal) that identify which
#'   files in \code{dir.load} get scanned/classified.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also searches
#'   every subdirectory of \code{dir.load}.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, an
#'   additional data frame named \code{arumeta.mergelog} is added to the
#'   returned list, with one row per deletion/merge action taken while
#'   cleaning up duplicate rows or columns. Columns: \code{$inputfile} (the
#'   source file - or \code{"filename [sheet: sheetname]"} for an xlsx sheet -
#'   the action happened to), \code{$event} (\code{"duplicated row"} or
#'   \code{"duplicated column"}), \code{$action} (\code{"deletion"} for
#'   identical duplicates that were just dropped, \code{"merging"} for
#'   differing duplicates combined into one), \code{$count} (how many extra
#'   rows/columns - not counting the surviving one - the action was done to).
#'   Only actions that actually did something are logged; nothing duplicated
#'   means no log rows at all for that file/category.
#'
#' @return Invisibly, a named list with three elements: \code{aru.visit},
#'   \code{aru.quad}, \code{aru.20m}. Each is a merged, de-duplicated data
#'   frame (character columns throughout), or \code{NULL} if no matching
#'   files were found for that category. If \code{log.file = TRUE}, a fourth
#'   element, \code{arumeta.mergelog}, is also included (see \code{log.file}
#'   above). In addition to being returned, each of these (whichever aren't
#'   \code{NULL}) is also assigned as its own object - \code{aru.visit},
#'   \code{aru.quad}, \code{aru.20m}, and (if requested) \code{arumeta.mergelog}
#'   - directly into the environment the function was called from, so calling
#'   \code{batz.merge&format_aru.meta(...)} on its own (with no assignment)
#'   creates those data frames right in your workspace. You can still do
#'   \code{result <- batz.merge&format_aru.meta(...)} and access them as
#'   \code{result$aru.visit} etc. if you'd rather work with the list.
#'
#' @details
#' Category matching uses a short, case-insensitive substring rather than an
#' exact file-name pattern: \code{"sitevis"} for the site-visit category,
#' \code{"quad"} for the quadrat habitat-assessment category, \code{"20m"} for
#' the 20m-plot habitat-assessment category. This was necessary because in the
#' real test workbook the site-visit sheet is named \code{"sitevist"} (missing
#' an "i") rather than containing "SiteVisitARU" - an exact-string match would
#' have silently missed it.
#'
#' Only a file whose name ends in \code{"ARUdeployments.xlsx"} (case
#' insensitive) is opened sheet-by-sheet and classified by SHEET name; any
#' other \code{.xlsx} file is classified as a single unit by its own file
#' name, the same as a \code{.csv}.
#'
#' Column names have a leading UTF-8 byte-order-mark stripped if present (some
#' exported CSVs carry one on their first column), so the same logical column
#' from a CSV and an xlsx sheet lines up correctly instead of appearing twice.
#'
#' If a single file or sheet has the same column name more than once (a real
#' issue in the source forms - e.g. "Personnel" appears twice on the site-visit
#' form), every duplicate is compared row-by-row (treating \code{NA} and ""
#' as equivalent): if all copies agree everywhere, the extra copy(ies) are
#' dropped; if they differ anywhere, they are merged into ONE column - taking
#' whichever copy is non-blank for a given row, and concatenating both with
#' \code{"; "} where they conflict (both non-blank and different). This
#' happens before columns are merged across sources.
#'
#' All columns are then coerced to character before merging multiple sources
#' within a category, and combining uses a fill-by-\code{NA} row bind (a
#' source missing a column the others have gets that column filled with
#' \code{NA} rather than the merge erroring) with raw column names preserved
#' exactly (no automatic renaming of names with spaces/punctuation). "Duplicate
#' rows" means exact full-row duplicates (base \code{duplicated()}) within a
#' category's merged data frame; the name of each category and the number of
#' duplicate rows removed from it is always printed to the console. Internally
#' every row is tagged with which source file it came from before this check,
#' so - when \code{log.file = TRUE} - removed duplicate rows can be attributed
#' back to the specific file(s) they came from.
#'
#' @section Known open items (not yet resolved - see project notes):
#' \itemize{
#'   \item No "format" step (column/date standardization, etc.) has been
#'     specified yet - only search/merge/de-duplicate is implemented.
#' }
#'
#' @examples
#' \dontrun{
#' result <- `batz.merge&format_aru.meta`("path/to/raw/data", dir.sub = TRUE)
#' result$aru.visit
#' result$aru.quad
#' result$aru.20m
#'
#' result <- `batz.merge&format_aru.meta`("path/to/raw/data", log.file = TRUE)
#' result$arumeta.mergelog
#' }
#'
#' @export
`batz.merge&format_aru.meta` <- function(dir.load = getwd(),
                                       load.pattern     = c("*.csv", "*.xlsx"),
                                       dir.sub          = FALSE,
                                       log.file         = FALSE) {

  ## convert a plain wildcard/glob suffix pattern (or vector of them) into
  ## one combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  log.rows <- list()
  add.log <- function(inputfile, event, action, count) {
    if (!log.file || count <= 0) return(invisible(NULL))
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      inputfile = inputfile, event = event, action = action, count = as.integer(count),
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
    names(df) <- gsub("﻿", "", names(df), fixed = TRUE)
    df
  }

  category.patterns <- c(aru.visit = "sitevis", aru.quad = "quad", aru.20m = "20m")

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  buckets <- setNames(vector("list", length(category.patterns)), names(category.patterns))

  add.to.bucket <- function(cat.name, df, source.label) {
    df <- strip.bom(df)
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
        df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
        add.to.bucket(cat.name, df, fname)
      }
    } else if (grepl("\\.xlsx$", fname, ignore.case = TRUE)) {
      if (grepl("ARUdeployments\\.xlsx$", fname, ignore.case = TRUE)) {
        sheets <- readxl::excel_sheets(f)
        for (s in sheets) {
          cat.name <- classify(s)
          if (!is.na(cat.name)) {
            df <- suppressMessages(as.data.frame(
              readxl::read_excel(f, sheet = s, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
            add.to.bucket(cat.name, df, paste0(fname, " [sheet: ", s, "]"))
          }
        }
      } else {
        cat.name <- classify(fname)
        if (!is.na(cat.name)) {
          df <- suppressMessages(as.data.frame(
            readxl::read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
          add.to.bucket(cat.name, df, fname)
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
      data.frame(inputfile = character(0), event = character(0),
                 action = character(0), count = integer(0), stringsAsFactors = FALSE)
    }
  }

  caller.env <- parent.frame()
  for (nm in names(result)) {
    if (!is.null(result[[nm]])) assign(nm, result[[nm]], envir = caller.env)
  }

  invisible(result)
}

#' Recode a set of values (or column headers) using a lookup table
#'
#' Recodes each value of a vector, or of every column of a data frame, by
#' looking it up in the first column of a two-column lookup table and
#' replacing it with the value in the second column. Values with no match are
#' left unchanged. With \code{headers.rename = TRUE} the column headers are
#' renamed instead of the contents. Optional console diagnostics report
#' unmatched values and duplicate keys.
#'
#' @param data A vector or a data frame to recode. For a data frame, every
#'   column is recoded with the same \code{recode.table}. Must be a data frame
#'   when \code{headers.rename = TRUE}.
#' @param recode.table A data frame (or tibble) with at least two columns.
#'   Column 1 holds the values to find; column 2 holds the replacements.
#'   Columns are read by position, so any column names work.
#' @param missing.count Logical, default \code{FALSE}. If \code{TRUE}, prints
#'   the number of values (every occurrence) with no match in
#'   \code{recode.table}, or \code{"all elements modified"} if every value
#'   matched. With \code{headers.rename = TRUE}, counts unmatched column
#'   headers instead.
#' @param missing.list Logical, default \code{FALSE}. If \code{TRUE}, prints a
#'   table of each unique unmatched value and its count (\code{$value},
#'   \code{$count}), or \code{"all elements modified"}. With
#'   \code{headers.rename = TRUE}, lists unmatched column headers instead.
#' @param duplicates.count Logical, default \code{FALSE}. If \code{TRUE},
#'   prints the total number of rows in \code{recode.table} whose first-column
#'   key repeats (all copies counted), or
#'   \code{"all reference elements are unique"}.
#' @param duplicates.list Logical, default \code{FALSE}. If \code{TRUE},
#'   prints a table of each repeated key and its count (\code{$value},
#'   \code{$count}), or \code{"all reference elements are unique"}. The first
#'   spelling found is shown.
#' @param match.first Logical, default \code{TRUE}. When a key appears more
#'   than once in \code{recode.table}, \code{TRUE} uses the first matching
#'   row's replacement and \code{FALSE} uses the last.
#' @param headers.rename Logical, default \code{FALSE}. If \code{TRUE}, looks
#'   up each column header of \code{data} and renames the matched ones; the
#'   data values are not recoded. Unmatched headers stay as they are. Stops
#'   with an error if \code{data} is not a data frame.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE}, accented
#'   letters and common symbols are simplified (accented letters to plain
#'   letters, the multiplication sign to \code{X}, curly quotes to straight quotes) and other
#'   non-ASCII characters (e.g. degree, micro and trademark signs) are removed from
#'   every data frame input (\code{data} when it is a data frame, and
#'   \code{recode.table}), in values and column names, before matching.
#'
#' @return
#' \itemize{
#'   \item \code{headers.rename = TRUE}: \code{data} with renamed column
#'     headers. Values are not recoded (but may have been cleaned by
#'     \code{strip.special}).
#'   \item \code{data} is a data frame: a data frame of the same size and
#'     column names, every value recoded, every column character.
#'   \item \code{data} is a vector: a character vector of the same length,
#'     values recoded.
#' }
#' The diagnostic arguments only print to the console; they do not change
#' what is returned. No files are read or written.
#'
#' @details
#' \strong{Matching.} Values are compared as text (\code{as.character}) and
#' letter case is ignored (\code{"Test1"} matches \code{"test1"}). Spaces and
#' punctuation are not ignored. The replacement is the column 2 value exactly
#' as written. An unmatched value (or header) is returned unchanged, never
#' \code{NA}, and never causes an error.
#'
#' \strong{Duplicate keys.} Keys that differ only in case count as the same
#' key, for \code{match.first} and for the duplicate diagnostics.
#'
#' \strong{Diagnostics.} For a data frame, \code{missing.count} and
#' \code{missing.list} look across all columns combined. With
#' \code{headers.rename = TRUE} they look at the column headers.
#'
#' \strong{Headers are not standardized.} \code{data}'s column names are not
#' passed through \code{standardize.headers()}, so a header rename matches the
#' headers as supplied (apart from case). To rename standardized headers, call
#' \code{standardize.headers()} first and write \code{recode.table} with the
#' standardized spellings.
#'
#' \strong{strip.special.} Cleaning happens at the start of the call. A text
#' column that changed is re-typed (\code{"42.5"} followed by a degree sign becomes 42.5); this also
#' applies to the contents of \code{data} when \code{headers.rename = TRUE}.
#' A vector \code{data} is not cleaned, so a vector value containing e.g.
#' a degree sign will not match a key that contained it (the key is cleaned); use
#' \code{strip.special = FALSE} if that matters.
#'
#' @examples
#' \dontrun{
#' recode.table <- data.frame(in_ = c("test1", "test2"),
#'                             out = c("out1", "banana"))
#' batz.datawrangler_rename(c("test1", "test2", "test6"), recode.table)
#' # "out1"   "banana" "test6"   (test6 has no match, stays unchanged)
#'
#' # matching is case-insensitive
#' batz.datawrangler_rename(c("Test1", "TEST2"), recode.table)
#' # "out1"   "banana"
#'
#' batz.datawrangler_rename(my.dataframe, recode.table,
#'                           missing.count = TRUE, duplicates.list = TRUE)
#'
#' # reference table with a duplicate key ("test1" -> "out1" AND "coconut")
#' dup.table <- data.frame(in_ = c("test1", "test1"), out = c("out1", "coconut"))
#' batz.datawrangler_rename("test1", dup.table)                    # "out1"
#' batz.datawrangler_rename("test1", dup.table, match.first = FALSE) # "coconut"
#'
#' # rename column headers instead of recoding values
#' header.table <- data.frame(old = c("A", "B"), new = c("Alpha", "Beta"))
#' batz.datawrangler_rename(my.dataframe, header.table, headers.rename = TRUE)
#' }
#'
#' @seealso \code{\link{batz.datawrangler_load.files}}
#'
#' @export
batz.datawrangler_rename <- function(data, recode.table,
                                      missing.count    = FALSE,
                                      missing.list     = FALSE,
                                      duplicates.count = FALSE,
                                      duplicates.list  = FALSE,
                                      match.first      = TRUE,
                                      headers.rename   = FALSE,
                                      strip.special    = TRUE) {

  ## special characters (per Josh, 2026-09-30): strip every data-frame input
  ## up front (vectors pass through unchanged).
  data         <- special.strip(data, strip.special)$df
  recode.table <- special.strip(recode.table, strip.special)$df

  # Case-insensitive-only normalization for comparison purposes (per Josh,
  # 2026-09-25) - deliberately does NOT fold whitespace/punctuation the way
  # standardize.headers() does; see @details above for why.
  normalize <- function(x) tolower(as.character(x))

  recode.vec <- function(x, recode.table, match.first = TRUE) {
    find.vals    <- as.character(recode.table[[1]])
    replace.vals <- as.character(recode.table[[2]])

    x.chr <- as.character(x)

    find.norm <- normalize(find.vals)
    x.norm    <- normalize(x.chr)

    if (match.first) {
      match.idx <- match(x.norm, find.norm)
    } else {
      n <- length(find.norm)
      rev.idx <- match(x.norm, rev(find.norm))
      match.idx <- ifelse(is.na(rev.idx), NA, n - rev.idx + 1)
    }
    found <- !is.na(match.idx)

    out <- x.chr
    out[found] <- replace.vals[match.idx[found]]
    out
  }

  if (headers.rename && !is.data.frame(data)) {
    stop("headers.rename = TRUE requires 'data' to be a data frame - it renames column headers, not vector elements.")
  }

  ref.find <- as.character(recode.table[[1]])
  ref.norm <- normalize(ref.find)

  # ---- missing-element diagnostics ----
  # In headers.rename mode, "elements" means the column headers being looked
  # up (not the data frame's contents); otherwise it's every data value.
  # Comparison against recode.table's first column is case-insensitive
  # (per Josh, 2026-09-25).
  if (missing.count || missing.list) {
    flat.chr <- if (headers.rename) {
      names(data)
    } else {
      as.character(if (is.data.frame(data)) unlist(data, use.names = FALSE) else data)
    }
    missing.vals <- flat.chr[!(normalize(flat.chr) %in% ref.norm)]

    if (missing.count) {
      if (length(missing.vals) == 0) {
        cat("all elements modified\n")
      } else {
        cat(length(missing.vals), "\n")
      }
    }

    if (missing.list) {
      if (length(missing.vals) == 0) {
        cat("all elements modified\n")
      } else {
        missing.tbl <- as.data.frame(table(missing.vals), stringsAsFactors = FALSE)
        names(missing.tbl) <- c("value", "count")
        print(missing.tbl)
      }
    }
  }

  # ---- duplicate-key diagnostics (reference table's first column) ----
  # Keys differing only by case are treated as the same key (per Josh,
  # 2026-09-25) - grouped by their case-folded form, displayed using the
  # first original-cased spelling encountered for that group.
  if (duplicates.count || duplicates.list) {
    norm.tbl <- as.data.frame(table(ref.norm), stringsAsFactors = FALSE)
    names(norm.tbl) <- c("value.normalized", "count")
    dup.tbl <- norm.tbl[norm.tbl$count > 1, , drop = FALSE]

    if (nrow(dup.tbl) > 0) {
      dup.tbl$value <- ref.find[match(dup.tbl$value.normalized, ref.norm)]
      dup.tbl <- dup.tbl[, c("value", "count")]
    }

    if (duplicates.count) {
      if (nrow(dup.tbl) == 0) {
        cat("all reference elements are unique\n")
      } else {
        cat(sum(dup.tbl$count), "\n")
      }
    }

    if (duplicates.list) {
      if (nrow(dup.tbl) == 0) {
        cat("all reference elements are unique\n")
      } else {
        print(dup.tbl)
      }
    }
  }

  # ---- actual rename/recode ----
  if (headers.rename) {
    names(data) <- recode.vec(names(data), recode.table, match.first = match.first)
    return(data)
  }

  if (is.data.frame(data)) {
    out <- as.data.frame(
      lapply(data, recode.vec, recode.table = recode.table, match.first = match.first),
      stringsAsFactors = FALSE
    )
    names(out) <- names(data)
    return(out)
  }
  recode.vec(data, recode.table, match.first = match.first)
}

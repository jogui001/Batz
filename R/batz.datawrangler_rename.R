#' Quickly rename a set of values (or column headers) using a reference data frame
#'
#' Recodes every element of a vector, or every element of every column of a
#' data frame, by looking each value up in a two-column reference table: if
#' the value is found in the reference table's first column, it is replaced
#' by the corresponding value in the reference table's second column. Values
#' with no match are left unchanged. Optionally reports diagnostics on
#' unmatched input elements and on duplicate keys in the reference table.
#' Alternatively, set \code{headers.rename = TRUE} to rename column HEADERS
#' instead of recoding the data frame's contents.
#'
#' @param data A vector or a data frame to be recoded. If a data frame is
#'   supplied and \code{headers.rename = FALSE} (the default), every column
#'   is recoded against the same \code{recode.table} (there is no
#'   column-selection argument). If \code{headers.rename = TRUE}, \code{data}
#'   must be a data frame (a vector has no headers to rename).
#' @param recode.table A data frame (or tibble) with at least two columns:
#'   the first column holds the values to search for, the second column
#'   holds the corresponding replacement values. Columns are read by
#'   position, not by name, so \code{recode.table} may use any column names.
#' @param missing.count Logical, default \code{FALSE}. If \code{TRUE}, print
#'   the number of instances (every occurrence, not just distinct values) in
#'   \code{data} that had no match anywhere in \code{recode.table}'s first
#'   column. If there are none, prints \code{"all elements modified"}. When
#'   \code{headers.rename = TRUE}, this counts unmatched COLUMN HEADERS
#'   instead of unmatched data values (see \code{headers.rename} below).
#' @param missing.list Logical, default \code{FALSE}. If \code{TRUE}, print a
#'   table of each unique unmatched value in \code{data} and how many
#'   instances of it were found. If there are none, prints
#'   \code{"all elements modified"}. When \code{headers.rename = TRUE}, this
#'   lists unmatched COLUMN HEADERS instead of unmatched data values.
#' @param duplicates.count Logical, default \code{FALSE}. If \code{TRUE},
#'   print the number of elements in \code{recode.table}'s first column that
#'   repeat (all instances of any repeated key, not just the extras). If none
#'   repeat, prints \code{"all reference elements are unique"}.
#' @param duplicates.list Logical, default \code{FALSE}. If \code{TRUE},
#'   print a table of the name and total count of each element in
#'   \code{recode.table}'s first column that repeats. If none repeat, prints
#'   \code{"all reference elements are unique"}.
#' @param match.first Logical, default \code{TRUE}. When
#'   \code{recode.table}'s first column has a duplicate key (e.g. it maps
#'   the same input value to two different replacements), \code{TRUE} uses
#'   the FIRST matching row's replacement value (matching R's own
#'   \code{match()} behavior); \code{FALSE} uses the LAST matching row's
#'   replacement value instead.
#' @param headers.rename Logical, default \code{FALSE}. If \code{TRUE}, the
#'   function does NOT touch the contents of \code{data} at all - instead it
#'   looks up each of \code{data}'s column HEADERS in \code{recode.table}'s
#'   first column, and renames any header found there to the matching second-
#'   column value. Headers with no match in \code{recode.table} are left
#'   unchanged. Requires \code{data} to be a data frame (errors otherwise).
#'
#' @return If \code{headers.rename = TRUE}, \code{data} unchanged except for
#'   its column names. Otherwise: if \code{data} is a data frame, a data
#'   frame of the same shape and column names, with every value recoded
#'   (columns are returned as character vectors); if \code{data} is a vector,
#'   a character vector of the same length, with values recoded. The four
#'   diagnostic arguments only print to the console - they never change what
#'   is returned.
#'
#' @details
#' Matching and replacement are done on the character representation of
#' values (\code{as.character}). A value in \code{data} (or, in
#' \code{headers.rename} mode, a column header) with no matching entry in
#' \code{recode.table[[1]]} is left unchanged in the output - it is not set
#' to \code{NA} and does not raise an error, regardless of whether
#' \code{missing.count}/\code{missing.list} are on.
#'
#' For a data frame input (with \code{headers.rename = FALSE}), the missing-
#' element diagnostics (\code{missing.count}/\code{missing.list}) are
#' computed across ALL columns combined, not per column - consistent with how
#' the recode itself treats every column the same way. With
#' \code{headers.rename = TRUE}, those same diagnostics instead look at the
#' column headers themselves (\code{names(data)}), since that's what's being
#' matched/renamed in that mode.
#'
#' @examples
#' \dontrun{
#' recode.table <- data.frame(in_ = c("test1", "test2"),
#'                             out = c("out1", "banana"))
#' batz.datawrangler_rename(c("test1", "test2", "test6"), recode.table)
#' # "out1"   "banana" "test6"   (test6 has no match, stays unchanged)
#'
#' batz.datawrangler_rename(my.dataframe, recode.table,
#'                           missing.count = TRUE, duplicates.list = TRUE)
#'
#' # reference table with a duplicate key ("test1" -> "out1" AND "coconut")
#' dup.table <- data.frame(in_ = c("test1", "test1"), out = c("out1", "coconut"))
#' batz.datawrangler_rename("test1", dup.table)                    # "out1"
#' batz.datawrangler_rename("test1", dup.table, match.first = FALSE) # "coconut"
#'
#' # rename column HEADERS instead of recoding values
#' header.table <- data.frame(old = c("A", "B"), new = c("Alpha", "Beta"))
#' batz.datawrangler_rename(my.dataframe, header.table, headers.rename = TRUE)
#' }
#'
#' @export
batz.datawrangler_rename <- function(data, recode.table,
                                      missing.count    = FALSE,
                                      missing.list     = FALSE,
                                      duplicates.count = FALSE,
                                      duplicates.list  = FALSE,
                                      match.first      = TRUE,
                                      headers.rename   = FALSE) {

  recode.vec <- function(x, recode.table, match.first = TRUE) {
    find.vals    <- as.character(recode.table[[1]])
    replace.vals <- as.character(recode.table[[2]])

    x.chr <- as.character(x)

    if (match.first) {
      match.idx <- match(x.chr, find.vals)
    } else {
      n <- length(find.vals)
      rev.idx <- match(x.chr, rev(find.vals))
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

  # ---- missing-element diagnostics ----
  # In headers.rename mode, "elements" means the column headers being looked
  # up (not the data frame's contents); otherwise it's every data value.
  if (missing.count || missing.list) {
    flat.chr <- if (headers.rename) {
      names(data)
    } else {
      as.character(if (is.data.frame(data)) unlist(data, use.names = FALSE) else data)
    }
    missing.vals <- flat.chr[!(flat.chr %in% ref.find)]

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
  if (duplicates.count || duplicates.list) {
    ref.tbl <- as.data.frame(table(ref.find), stringsAsFactors = FALSE)
    names(ref.tbl) <- c("value", "count")
    dup.tbl <- ref.tbl[ref.tbl$count > 1, ]

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

#' Quickly rename a set of values using a reference data frame
#'
#' Recodes every element of a vector, or every element of every column of a
#' data frame, by looking each value up in a two-column reference table: if
#' the value is found in the reference table's first column, it is replaced
#' by the corresponding value in the reference table's second column. Values
#' with no match are left unchanged. Optionally reports diagnostics on
#' unmatched input elements and on duplicate keys in the reference table.
#'
#' @param data A vector or a data frame to be recoded. If a data frame is
#'   supplied, every column is recoded against the same \code{recode.table}
#'   (there is no column-selection argument).
#' @param recode.table A data frame (or tibble) with at least two columns:
#'   the first column holds the values to search for, the second column
#'   holds the corresponding replacement values. Columns are read by
#'   position, not by name, so \code{recode.table} may use any column names.
#' @param count.missing Logical, default \code{FALSE}. If \code{TRUE}, print
#'   the number of instances (every occurrence, not just distinct values) in
#'   \code{data} that had no match anywhere in \code{recode.table}'s first
#'   column. If there are none, prints \code{"all elements modified"}.
#' @param list.missing Logical, default \code{FALSE}. If \code{TRUE}, print a
#'   table of each unique unmatched value in \code{data} and how many
#'   instances of it were found. If there are none, prints
#'   \code{"all elements modified"}.
#' @param count.duplicates Logical, default \code{FALSE}. If \code{TRUE},
#'   print the number of elements in \code{recode.table}'s first column that
#'   repeat (all instances of any repeated key, not just the extras). If none
#'   repeat, prints \code{"all reference elements are unique"}.
#' @param list.duplicates Logical, default \code{FALSE}. If \code{TRUE},
#'   print a table of the name and total count of each element in
#'   \code{recode.table}'s first column that repeats. If none repeat, prints
#'   \code{"all reference elements are unique"}.
#' @param first.match Logical, default \code{TRUE}. When
#'   \code{recode.table}'s first column has a duplicate key (e.g. it maps
#'   the same input value to two different replacements), \code{TRUE} uses
#'   the FIRST matching row's replacement value (matching R's own
#'   \code{match()} behavior); \code{FALSE} uses the LAST matching row's
#'   replacement value instead.
#'
#' @return If \code{data} is a data frame, a data frame of the same shape and
#'   column names, with every value recoded (columns are returned as
#'   character vectors). If \code{data} is a vector, a character vector of
#'   the same length, with values recoded. The four diagnostic arguments only
#'   print to the console - they never change what is returned.
#'
#' @details
#' Matching and replacement are done on the character representation of
#' values (\code{as.character}). A value in \code{data} with no matching
#' entry in \code{recode.table[[1]]} is left unchanged in the output - it is
#' not set to \code{NA} and does not raise an error, regardless of whether
#' \code{count.missing}/\code{list.missing} are on.
#'
#' For a data frame input, the missing-element diagnostics
#' (\code{count.missing}/\code{list.missing}) are computed across ALL columns
#' combined, not per column - consistent with how the recode itself treats
#' every column the same way.
#'
#' @examples
#' \dontrun{
#' recode.table <- data.frame(in_ = c("test1", "test2"),
#'                             out = c("out1", "banana"))
#' batz.datawrangler_rename(c("test1", "test2", "test6"), recode.table)
#' # "out1"   "banana" "test6"   (test6 has no match, stays unchanged)
#'
#' batz.datawrangler_rename(my.dataframe, recode.table,
#'                           count.missing = TRUE, list.duplicates = TRUE)
#'
#' # reference table with a duplicate key ("test1" -> "out1" AND "coconut")
#' dup.table <- data.frame(in_ = c("test1", "test1"), out = c("out1", "coconut"))
#' batz.datawrangler_rename("test1", dup.table)                    # "out1"
#' batz.datawrangler_rename("test1", dup.table, first.match = FALSE) # "coconut"
#' }
#'
#' @export
batz.datawrangler_rename <- function(data, recode.table,
                                      count.missing    = FALSE,
                                      list.missing     = FALSE,
                                      count.duplicates = FALSE,
                                      list.duplicates  = FALSE,
                                      first.match      = TRUE) {

  recode.vec <- function(x, recode.table, first.match = TRUE) {
    find.vals    <- as.character(recode.table[[1]])
    replace.vals <- as.character(recode.table[[2]])

    x.chr <- as.character(x)

    if (first.match) {
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

  ref.find <- as.character(recode.table[[1]])

  # ---- missing-element diagnostics (input elements with no match in ref) ----
  if (count.missing || list.missing) {
    flat.chr <- as.character(if (is.data.frame(data)) unlist(data, use.names = FALSE) else data)
    missing.vals <- flat.chr[!(flat.chr %in% ref.find)]

    if (count.missing) {
      if (length(missing.vals) == 0) {
        cat("all elements modified\n")
      } else {
        cat(length(missing.vals), "\n")
      }
    }

    if (list.missing) {
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
  if (count.duplicates || list.duplicates) {
    ref.tbl <- as.data.frame(table(ref.find), stringsAsFactors = FALSE)
    names(ref.tbl) <- c("value", "count")
    dup.tbl <- ref.tbl[ref.tbl$count > 1, ]

    if (count.duplicates) {
      if (nrow(dup.tbl) == 0) {
        cat("all reference elements are unique\n")
      } else {
        cat(sum(dup.tbl$count), "\n")
      }
    }

    if (list.duplicates) {
      if (nrow(dup.tbl) == 0) {
        cat("all reference elements are unique\n")
      } else {
        print(dup.tbl)
      }
    }
  }

  # ---- actual recode ----
  if (is.data.frame(data)) {
    out <- as.data.frame(
      lapply(data, recode.vec, recode.table = recode.table, first.match = first.match),
      stringsAsFactors = FALSE
    )
    names(out) <- names(data)
    return(out)
  }
  recode.vec(data, recode.table, first.match = first.match)
}

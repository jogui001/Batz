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
#'   repeat, prints \code{"all reference elements are unique"}. Since
#'   2026-09-25, two keys that differ only in case (e.g. \code{"Test1"} and
#'   \code{"test1"}) count as a repeat (see \code{@details}).
#' @param duplicates.list Logical, default \code{FALSE}. If \code{TRUE},
#'   print a table of the name and total count of each element in
#'   \code{recode.table}'s first column that repeats. If none repeat, prints
#'   \code{"all reference elements are unique"}. Since 2026-09-25, keys
#'   differing only in case are grouped together (see \code{@details}).
#' @param match.first Logical, default \code{TRUE}. When
#'   \code{recode.table}'s first column has a duplicate key (e.g. it maps
#'   the same input value to two different replacements), \code{TRUE} uses
#'   the FIRST matching row's replacement value (matching R's own
#'   \code{match()} behavior); \code{FALSE} uses the LAST matching row's
#'   replacement value instead. Since 2026-09-25, "duplicate key" and
#'   "matching row" are both determined case-insensitively (see
#'   \code{@details}).
#' @param headers.rename Logical, default \code{FALSE}. If \code{TRUE}, the
#'   function does NOT touch the contents of \code{data} at all - instead it
#'   looks up each of \code{data}'s column HEADERS in \code{recode.table}'s
#'   first column, and renames any header found there to the matching second-
#'   column value. Headers with no match in \code{recode.table} are left
#'   unchanged. Requires \code{data} to be a data frame (errors otherwise).
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplifies accented letters and
#'   symbols (\code{é} -> \code{e}, \code{×} -> \code{X}) and removes other non-ASCII special characters (e.g. \code{°}, \code{µ},
#'   \code{™}) from every data frame input (\code{data}, when
#'   it is a data frame, and \code{recode.table}) at the start of the call -
#'   see \code{@details}. A vector \code{data} passes through unchanged.
#'   Files are always read with a UTF-8/Latin-1 fallback elsewhere in
#'   \code{batz}, so a stray \code{°} can't stop them loading, whatever this
#'   is set to (this function itself reads no files).
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
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - deliberately NOT applied automatically here, flagged for review.} The
#' project-wide preference is that headers coming from a loaded file or an
#' externally-supplied data frame are run through \code{standardize.headers()}
#' (trim whitespace, collapse non-alphanumeric runs to underscores,
#' lowercase) - but this function's whole job in \code{headers.rename = TRUE}
#' mode is already an explicit, user-directed header rename, driven by
#' whatever \code{recode.table} the caller supplies (e.g. \code{"A" ->
#' "Alpha"}). Auto-standardizing \code{names(data)} before doing that lookup
#' would actively break it: a raw header like \code{"A"} would already have
#' become \code{"a"} by the time it's compared against \code{recode.table}'s
#' literal \code{"A"} entry, so the intended rename would silently fail to
#' match. For that reason \code{batz.datawrangler_rename()} does NOT run
#' \code{standardize.headers()} on \code{data} (in either mode) - it is a
#' generic recode/rename utility that trusts its caller's own
#' \code{recode.table}, not a file/data-loading step the standardization
#' preference is aimed at. In practice, headers reaching this function are
#' typically already standardized upstream (e.g. by
#' \code{batz.datawrangler_load.files()}, which now standardizes on load -
#' see its own documentation); if you need standardized headers AND a custom
#' rename on top, call \code{standardize.headers()} yourself first and build
#' \code{recode.table}'s first column against the standardized spellings.
#'
#' \strong{Case-insensitive matching against \code{recode.table} (per Josh,
#' 2026-09-25) - narrower than, and not in conflict with, the header-
#' standardization note above.} Every comparison this function makes against
#' \code{recode.table}'s first column - the main value/header recode lookup,
#' the missing-element diagnostics, and the duplicate-key diagnostics - now
#' folds case before comparing (\code{"Test1"} in \code{data} now matches a
#' \code{"test1"} entry in \code{recode.table}, and vice versa). This is
#' case-folding ONLY: unlike \code{standardize.headers()}, it does not touch
#' whitespace or punctuation, so it does not reopen the problem described
#' above - a literal, caller-supplied rename table still means what it says
#' beyond letter case. The value actually substituted is always
#' \code{recode.table}'s second-column entry exactly as supplied (its
#' original casing, never lowercased), matching the pattern already used in
#' \code{batz.batusa_recode.names()}, \code{batz.batusa_list.species()}, and
#' \code{batz.treeusa_recode.names()}. One consequence: if
#' \code{recode.table}'s first column contains two keys that differ only by
#' case (e.g. \code{"Test1"} and \code{"test1"}), they are now treated as a
#' duplicate key for \code{match.first}/\code{duplicates.count}/
#' \code{duplicates.list} purposes, exactly as if they were spelled
#' identically - this was not previously the case.
#'
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
#' \strong{Follow-up, 2026-09-30, per Josh - special characters.} New last
#' argument \code{strip.special = TRUE}. At the very start of the call,
#' \code{data} (if it is a data frame) and \code{recode.table} are passed
#' through \code{special.strip()}, removing non-ASCII characters (e.g.
#' \code{°}) from their text columns before any matching, so a lookup key
#' and a data value that differ only by such a character now match, and
#' replacement values are substituted without them. Column names are not
#' changed. Note this means that with \code{headers.rename = TRUE} the
#' contents of a data-frame \code{data} are also stripped (and any column
#' that changed is re-typed, e.g. \code{"42.5°"} becomes \code{42.5});
#' pass \code{strip.special = FALSE} to leave contents exactly as supplied.
#' A vector \code{data} is NOT stripped, so a vector value containing e.g.
#' \code{°} will no longer match a \code{recode.table} key that contained
#' the same character (the key is stripped) - use \code{strip.special =
#' FALSE} if that matters. No log.
#'
#' \strong{Follow-up, 2026-10-02, per Josh - accents simplified.} With
#' \code{strip.special = TRUE}, accented letters and common symbols are
#' now simplified instead of dropped (\code{café} -> \code{cafe},
#' \code{×} -> \code{X}, curly quotes -> straight quotes); characters
#' with no plain equivalent (e.g. \code{°}, \code{µ}, \code{™}) are still
#' removed. Column names are cleaned the same way.
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

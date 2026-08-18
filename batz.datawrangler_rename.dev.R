# =============================================================================
# batz.datawrangler_rename.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.datawrangler_rename() - tested against the real test
# data before being wrapped into the final function (batz.datawrangler_rename.R).
#
# Purpose: quickly rename a set of values in a vector or data frame, using a
# second data frame as the recode reference (col 1 = value to find, col 2 =
# replacement value). Optionally reports diagnostics on unmatched input
# elements and on duplicate keys in the reference table.
#
# Test data: recode.xlsx (recode reference table), recode.test.xlsx (data to
# be recoded).
#
# ASSUMPTIONS MADE (spec was silent on these - flagging per project convention):
#   1. The recode reference table is read by POSITION, not by column name -
#      "first column" / "second column" in the spec, so this works no matter
#      what the two columns are named (recode.xlsx happens to use "in"/"out").
#   2. A value in the data with NO match in the recode table's first column is
#      left UNCHANGED (the spec only says what to do when a match IS found).
#      recode.test.xlsx deliberately includes "test6", which has no entry in
#      recode.xlsx, to exercise this case.
#   3. When the input is a data frame, EVERY column is recoded against the
#      same single recode table (the spec says "changing every element in"
#      the input) - there's no column-selection argument.
#   4. Matching/replacement is done on the character representation of values
#      (as.character) - values are compared and replaced as text. A returned
#      data frame's columns come back as character vectors (not re-cast to
#      factor).
#   5. "Instances" (count.missing / count.duplicates) = every occurrence, not
#      just distinct values - e.g. if "test6" appears 3 times unmatched, that
#      counts as 3 instances, and if a reference key repeats 3 times, all 3
#      rows count toward the duplicate total (not just the 2 "extra" ones).
#      For a data frame input, missing-element counts/tables are computed
#      across ALL columns combined (flattened), not per column - there's no
#      column-selection argument, consistent with how the recoding itself
#      treats every column the same way.
#   6. These four flags are pure reporting side effects (printed via cat()/
#      print()) - they never change the returned recoded vector/data frame,
#      and unmatched values are still passed through unchanged regardless of
#      whether count.missing/list.missing are on.
#   7. first.match = TRUE (default) - when the reference table's first column
#      has a duplicate key (e.g. real test data recode.csv has two rows for
#      "test1": ->out1 and ->coconut), the FIRST matching row's replacement is
#      used, matching R's own match() behavior. Setting first.match = FALSE
#      uses the LAST matching row's replacement instead.
# =============================================================================

suppressMessages(library(readxl))

recode.table <- read_excel("/home/claude/recode_work/recode.xlsx")
test.data    <- read_excel("/home/claude/recode_work/recode.test.xlsx")

cat("=== recode.table ===\n"); print(recode.table)
cat("\n=== test.data ===\n"); print(test.data)

# -----------------------------------------------------------------------------
# core: recode a single vector against a 2-column recode table (by position).
# When the reference table's first column has a duplicate key, first.match
# picks whether the FIRST or LAST matching row's replacement value is used.
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# batz.datawrangler_rename(data, recode.table,
#                           count.missing = FALSE, list.missing = FALSE,
#                           count.duplicates = FALSE, list.duplicates = FALSE,
#                           first.match = TRUE)
# -----------------------------------------------------------------------------
batz.datawrangler_rename <- function(data, recode.table,
                                      count.missing    = FALSE,
                                      list.missing     = FALSE,
                                      count.duplicates = FALSE,
                                      list.duplicates  = FALSE,
                                      first.match      = TRUE) {

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

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n=== basic recode (unchanged behavior) ===\n")
recoded.df <- batz.datawrangler_rename(test.data, recode.table)
print(recoded.df)

cat("\n=== count.missing = TRUE (test6 unmatched, appears twice in test.data) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, count.missing = TRUE))

cat("\n=== list.missing = TRUE ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, list.missing = TRUE))

cat("\n=== count.missing/list.missing when nothing is missing ('all elements modified') ===\n")
full.coverage.table <- rbind(recode.table, data.frame(`in` = "test6", out = "renamed6", check.names = FALSE))
invisible(batz.datawrangler_rename(test.data, full.coverage.table, count.missing = TRUE))
invisible(batz.datawrangler_rename(test.data, full.coverage.table, list.missing = TRUE))

cat("\n=== count.duplicates = TRUE (real recode.xlsx has no duplicate keys) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, count.duplicates = TRUE))

cat("\n=== list.duplicates = TRUE (real recode.xlsx has no duplicate keys) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, list.duplicates = TRUE))

cat("\n=== count.duplicates/list.duplicates with a synthetic duplicate-key table ===\n")
dup.table <- rbind(recode.table, recode.table[1, ], recode.table[1, ])  # duplicate "test1" 2 extra times
invisible(batz.datawrangler_rename(test.data, dup.table, count.duplicates = TRUE))
invisible(batz.datawrangler_rename(test.data, dup.table, list.duplicates = TRUE))

# -----------------------------------------------------------------------------
# first.match test - real duplicate-key data (recode.csv has TWO rows for
# "test1": test1->out1 (row 1, first) and test1->coconut (row 5, last))
# -----------------------------------------------------------------------------
recode.table.realdup <- read.csv("/home/claude/recode_work/recode.csv", stringsAsFactors = FALSE)
test.data.csv        <- read.csv("/home/claude/recode_work/recode.test.csv", stringsAsFactors = FALSE)

cat("\n=== real duplicate-key reference table ===\n"); print(recode.table.realdup)

cat("\n=== first.match = TRUE (default) - test1 should recode to 'out1' ===\n")
print(batz.datawrangler_rename(test.data.csv, recode.table.realdup))

cat("\n=== first.match = FALSE - test1 should recode to 'coconut' instead ===\n")
print(batz.datawrangler_rename(test.data.csv, recode.table.realdup, first.match = FALSE))

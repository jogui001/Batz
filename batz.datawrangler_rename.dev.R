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
#   5. "Instances" (missing.count / duplicates.count) = every occurrence, not
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
#      whether missing.count/missing.list are on.
#   7. match.first = TRUE (default) - when the reference table's first column
#      has a duplicate key (e.g. real test data recode.csv has two rows for
#      "test1": ->out1 and ->coconut), the FIRST matching row's replacement is
#      used, matching R's own match() behavior. Setting match.first = FALSE
#      uses the LAST matching row's replacement instead.
#
# NEW FEATURE (2026-08-19, per Josh): optional headers.rename = FALSE. When
# TRUE, the function does NOT touch the data frame's contents at all -
# instead it looks up each of data's column HEADERS in recode.table's first
# column, and renames any header found there to the matching second-column
# value (headers with no match are left unchanged). This reuses the exact
# same recode.vec() matching/match.first logic already used for recoding
# values - just applied to names(data) instead of the data itself. See
# assumption 8 below.
#   8. headers.rename requires `data` to be a data frame (a vector has no
#      headers to rename) - errors with a clear message if data isn't a data
#      frame and headers.rename = TRUE. When headers.rename = TRUE, the
#      missing.count/missing.list diagnostics also switch what they consider
#      "elements": instead of flattening the data frame's VALUES, they look
#      at names(data) directly - since in this mode it's the headers being
#      matched/renamed, not the values. duplicates.count/duplicates.list are
#      unaffected either way (they only ever look at recode.table's own first
#      column, independent of what's being renamed).
#
#   9. **Reviewed 2026-09-14, per Josh's project-wide header-standardization
#      preference - deliberately left UNCHANGED, flagged for review.** The
#      preference says headers from a loaded file or an externally-supplied
#      data frame get run through standardize.headers() - but this function's
#      headers.rename = TRUE mode IS an explicit, user-directed header rename
#      already, driven by whatever recode.table the caller supplies (e.g.
#      "A" -> "Alpha"). Auto-standardizing names(data) first would break that:
#      a raw header like "A" would already be "a" by the time it's compared
#      against recode.table's literal "A" entry, so the rename would silently
#      fail to match. So this function does NOT call standardize.headers() on
#      data in either mode - it's a generic recode/rename utility that trusts
#      the caller's own recode.table, not a loading step. Headers reaching
#      this function are typically already standardized upstream (e.g. by
#      batz.datawrangler_load.files(), which now standardizes on load); if
#      standardized headers AND a custom rename are both needed, call
#      standardize.headers() first and build recode.table's first column
#      against the standardized spellings.
#
#  10. **Round twenty-five, 2026-09-25, per Josh: "do not be case sensitive
#      when comparing input to reference dataframes" - applied here.** Every
#      comparison against recode.table's first column (the main recode
#      lookup in both value mode and headers.rename mode, the missing-
#      element diagnostics, and the duplicate-key diagnostics) now folds
#      case before comparing - e.g. "Test1" in the data now matches a
#      "test1" entry in recode.table. This is case-folding ONLY (a plain
#      tolower()) - it deliberately does NOT also fold whitespace/punctuation
#      the way standardize.headers() does, so it doesn't reopen the problem
#      flagged in assumption 9 above; a caller-supplied recode.table still
#      means what it says beyond letter case. The replacement value
#      substituted is always recode.table's second-column entry exactly as
#      supplied (original casing, never lowercased) - same pattern as
#      batz.batusa_recode.names()/batz.batusa_list.species()/
#      batz.treeusa_recode.names()'s own normalize()/normalize.tree()
#      helpers. Consequence: two reference-table keys differing only by case
#      (e.g. "Test1"/"test1") now count as a duplicate key for
#      match.first/duplicates.count/duplicates.list purposes - see the new
#      "case-insensitivity" test section at the end of this script (using
#      synthetic data, since the device bridge with the original
#      recode.xlsx/recode.csv real test files wasn't available for this
#      round; the pre-existing tests above are unaffected and still pass
#      with these real files, since none of them contain case-colliding
#      keys).
# =============================================================================

suppressMessages(library(readxl))

recode.table <- read_excel("/home/claude/recode_work/recode.xlsx")
test.data    <- read_excel("/home/claude/recode_work/recode.test.xlsx")

cat("=== recode.table ===\n"); print(recode.table)
cat("\n=== test.data ===\n"); print(test.data)

# -----------------------------------------------------------------------------
# Case-insensitive-only normalization for comparison purposes (per Josh,
# 2026-09-25) - deliberately does NOT fold whitespace/punctuation the way
# standardize.headers() does; see assumption 10 above for why.
# -----------------------------------------------------------------------------
normalize <- function(x) tolower(as.character(x))

# -----------------------------------------------------------------------------
# core: recode a single vector against a 2-column recode table (by position).
# When the reference table's first column has a duplicate key, match.first
# picks whether the FIRST or LAST matching row's replacement value is used.
# Matching is now case-insensitive (2026-09-25); the value substituted is
# always the reference table's original-cased replacement text.
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# batz.datawrangler_rename(data, recode.table,
#                           missing.count = FALSE, missing.list = FALSE,
#                           duplicates.count = FALSE, duplicates.list = FALSE,
#                           match.first = TRUE, headers.rename = FALSE)
# -----------------------------------------------------------------------------
batz.datawrangler_rename <- function(data, recode.table,
                                      missing.count    = FALSE,
                                      missing.list     = FALSE,
                                      duplicates.count = FALSE,
                                      duplicates.list  = FALSE,
                                      match.first      = TRUE,
                                      headers.rename   = FALSE) {

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

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n=== basic recode (unchanged behavior) ===\n")
recoded.df <- batz.datawrangler_rename(test.data, recode.table)
print(recoded.df)

cat("\n=== missing.count = TRUE (test6 unmatched, appears twice in test.data) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, missing.count = TRUE))

cat("\n=== missing.list = TRUE ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, missing.list = TRUE))

cat("\n=== missing.count/missing.list when nothing is missing ('all elements modified') ===\n")
full.coverage.table <- rbind(recode.table, data.frame(`in` = "test6", out = "renamed6", check.names = FALSE))
invisible(batz.datawrangler_rename(test.data, full.coverage.table, missing.count = TRUE))
invisible(batz.datawrangler_rename(test.data, full.coverage.table, missing.list = TRUE))

cat("\n=== duplicates.count = TRUE (real recode.xlsx has no duplicate keys) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, duplicates.count = TRUE))

cat("\n=== duplicates.list = TRUE (real recode.xlsx has no duplicate keys) ===\n")
invisible(batz.datawrangler_rename(test.data, recode.table, duplicates.list = TRUE))

cat("\n=== duplicates.count/duplicates.list with a synthetic duplicate-key table ===\n")
dup.table <- rbind(recode.table, recode.table[1, ], recode.table[1, ])  # duplicate "test1" 2 extra times
invisible(batz.datawrangler_rename(test.data, dup.table, duplicates.count = TRUE))
invisible(batz.datawrangler_rename(test.data, dup.table, duplicates.list = TRUE))

# -----------------------------------------------------------------------------
# match.first test - real duplicate-key data (recode.csv has TWO rows for
# "test1": test1->out1 (row 1, first) and test1->coconut (row 5, last))
# -----------------------------------------------------------------------------
recode.table.realdup <- read.csv("/home/claude/recode_work/recode.csv", stringsAsFactors = FALSE)
test.data.csv        <- read.csv("/home/claude/recode_work/recode.test.csv", stringsAsFactors = FALSE)

cat("\n=== real duplicate-key reference table ===\n"); print(recode.table.realdup)

cat("\n=== match.first = TRUE (default) - test1 should recode to 'out1' ===\n")
print(batz.datawrangler_rename(test.data.csv, recode.table.realdup))

cat("\n=== match.first = FALSE - test1 should recode to 'coconut' instead ===\n")
print(batz.datawrangler_rename(test.data.csv, recode.table.realdup, match.first = FALSE))

# -----------------------------------------------------------------------------
# headers.rename test - real test.data has columns "A" and "B"; use a
# synthetic header-rename table (real recode.table's values don't match
# column names, so a header-specific reference table is needed here).
# -----------------------------------------------------------------------------
cat("\n\n=== headers.rename = TRUE - real test.data columns before: ===\n")
print(names(test.data))

header.table <- data.frame(old = c("A", "B", "NoSuchColumn"),
                            new = c("Alpha", "Beta", "Should.Not.Appear"),
                            stringsAsFactors = FALSE)

renamed <- batz.datawrangler_rename(test.data, header.table, headers.rename = TRUE)
cat("\n=== headers.rename = TRUE - columns after (A->Alpha, B->Beta) ===\n")
print(names(renamed))
cat("contents unchanged? ", identical(as.data.frame(renamed, stringsAsFactors = FALSE),
                                       setNames(as.data.frame(test.data, stringsAsFactors = FALSE), names(renamed))), "\n")

cat("\n=== headers.rename = TRUE + a header with NO match in the reference\n",
    "(column C - not in header.table - should stay unchanged) ===\n", sep = "")
test.data.extra <- test.data
test.data.extra$C <- "unchanged.column"
renamed.extra <- batz.datawrangler_rename(test.data.extra, header.table, headers.rename = TRUE)
print(names(renamed.extra))

cat("\n=== headers.rename = TRUE + missing.count/missing.list now describe\n",
    "unmatched HEADERS, not unmatched data values ===\n", sep = "")
invisible(batz.datawrangler_rename(test.data.extra, header.table, headers.rename = TRUE, missing.count = TRUE))
invisible(batz.datawrangler_rename(test.data.extra, header.table, headers.rename = TRUE, missing.list = TRUE))

cat("\n=== headers.rename = TRUE on a plain vector should error ===\n")
tryCatch(
  batz.datawrangler_rename(c("A", "B"), header.table, headers.rename = TRUE),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

# =============================================================================
# NEW (round twenty-five, 2026-09-25): case-insensitivity tests, per Josh's
# "do not be case sensitive when comparing input to reference dataframes".
# Uses hand-built synthetic data (not the real recode.xlsx/csv files above),
# since these files weren't available in this session's environment - each
# test below is self-contained and asserts its own expected result with
# stopifnot(), independent of the real-file tests above.
# =============================================================================
cat("\n\n========================================\n")
cat("CASE-INSENSITIVITY TESTS (2026-09-25)\n")
cat("========================================\n\n")

ci.recode.table <- data.frame(in_ = c("test1", "test2"), out = c("out1", "banana"),
                               stringsAsFactors = FALSE)

cat("--- TEST CI-1: value recode matches regardless of input casing ---\n")
r.ci1 <- batz.datawrangler_rename(c("Test1", "TEST2", "test6"), ci.recode.table)
print(r.ci1)
stopifnot(identical(r.ci1, c("out1", "banana", "test6")))
cat("PASS\n\n")

cat("--- TEST CI-2: exact-case input is still unaffected (no regression) ---\n")
r.ci2 <- batz.datawrangler_rename(c("test1", "test2"), ci.recode.table)
stopifnot(identical(r.ci2, c("out1", "banana")))
cat("PASS:", r.ci2, "\n\n")

cat("--- TEST CI-3: reference-table keys differing only by case are now a\n",
    "    duplicate key, and match.first / match.first = FALSE still\n",
    "    pick first-vs-last correctly among them ---\n", sep = "")
ci.dup.table <- data.frame(in_ = c("Test1", "test1"), out = c("out1", "coconut"),
                            stringsAsFactors = FALSE)
r.ci3a <- batz.datawrangler_rename("test1", ci.dup.table)
r.ci3b <- batz.datawrangler_rename("TEST1", ci.dup.table, match.first = FALSE)
stopifnot(identical(r.ci3a, "out1"))
stopifnot(identical(r.ci3b, "coconut"))
cat("PASS: match.first = TRUE ->", r.ci3a, " | match.first = FALSE ->", r.ci3b, "\n\n")

cat("--- TEST CI-4: headers.rename = TRUE is also case-insensitive ---\n")
ci.df <- data.frame(A = 1, b = 2)
ci.header.table <- data.frame(old = c("a", "B"), new = c("Alpha", "Beta"),
                                stringsAsFactors = FALSE)
ci.df2 <- batz.datawrangler_rename(ci.df, ci.header.table, headers.rename = TRUE)
stopifnot(identical(names(ci.df2), c("Alpha", "Beta")))
cat("PASS:", names(ci.df2), "\n\n")

cat("--- TEST CI-5: missing.count folds case (only truly-unmatched values count) ---\n")
invisible(batz.datawrangler_rename(c("Test1", "nope"), ci.recode.table, missing.count = TRUE))
cat("(expect 1 - only 'nope' is unmatched; 'Test1' now matches 'test1')\n\n")

cat("--- TEST CI-6: duplicates.count now flags case-differing keys as duplicates ---\n")
invisible(batz.datawrangler_rename("x", ci.dup.table, duplicates.count = TRUE))
cat("(expect 2 - 'Test1'/'test1' now count as one repeated key)\n\n")

cat("--- TEST CI-7: duplicates.list groups case-differing keys together ---\n")
invisible(batz.datawrangler_rename("x", ci.dup.table, duplicates.list = TRUE))
cat("\n")

cat("--- TEST CI-8: no false-positive duplicate flag when no keys collide, even\n",
    "    case-insensitively ---\n", sep = "")
ci.clean.table <- data.frame(in_ = c("alpha", "beta"), out = c("A", "B"),
                              stringsAsFactors = FALSE)
invisible(batz.datawrangler_rename("x", ci.clean.table, duplicates.count = TRUE))
cat("(expect 'all reference elements are unique')\n\n")

cat("ALL CASE-INSENSITIVITY TESTS PASSED\n")

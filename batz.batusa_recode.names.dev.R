# =============================================================================
# batz.batusa_recode.names.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.batusa_recode.names() - tested here before being wrapped
# into the final function (batz.batusa_recode.names.R).
#
# Purpose: given a vector or data frame of US bat species identifiers in ANY
# mix of common name, latin (scientific) name, 4-letter species code, or
# 6-letter species code, look each one up in a reference database and return
# it re-expressed in a single chosen format (output.format).
#
# NAME NORMALIZATION (per Josh's naming conventions - see project preferences):
#   - Requested name was "batz.batusa_names.recode()". Per the
#     batz.<family>_<action>.<subject>() convention, the part before the "."
#     is the ACTION (verb) and the part after is the SUBJECT (noun) - e.g.
#     arumeta_generate.eventlog (generate = action, eventlog = subject),
#     datawrangler_load.files (load = action, files = subject). Here the verb
#     is "recode" and the noun is "names", so this was normalized to
#     batz.batusa_recode.names() (family = batusa, action = recode,
#     subject = names). Flagging this rename to Josh per project convention -
#     not silently renamed.
#   - "gramma-dash" (optional input) normalized to "grammar.dash" (typo fix +
#     "." separator per Josh's own column/parameter naming convention).
#
# TEST DATA STATUS:
#   - "NAbat.names.csv" (the real reference database) was supplied directly
#     by Josh (uploaded to the conversation) - real data, originally 54
#     species x 12 columns: latin, common, code4, code6, fedstatus,
#     iucnstatus, states.listed, states.present, states.end, states.the,
#     state.soc, fed.proposed. No blank rows, no duplicate keys in latin/
#     common/code4/code6 (checked programmatically before building this).
#     On 2026-08-25 three more columns were added (hibernation.strat,
#     phonic.group, notes - see add_bat_traits.R), making it 54 x 15. On
#     2026-08-27, 8 more ROWS were added (non-species detection/category
#     labels, see point 10 below), making it 62 x 15. This dev script reads
#     the CSV from disk each run, so it always reflects whatever's currently
#     in NAbat.names.csv - only new *test* coverage needed to be added below,
#     no data-loading change was required.
#   - "NAbat.namestest" (the actual test-input file named in the spec) was
#     NOT supplied and could not be found anywhere in this session or via
#     the connected-device folders (the "hildas" folder-access prompt timed
#     out unanswered). A SYNTHETIC test vector is used below instead, built
#     from real species drawn from the real reference file, deliberately
#     mixing case/underscores/dashes/whitespace and including values with no
#     match at all. **Josh: please send the real NAbat.namestest (or connect
#     the test-data folder) so this can be re-verified against it.**
#
# STEPS / ASSUMPTIONS (spec was silent on some of these - flagging per
# project convention):
#   1. The reference database's matching columns are exactly the four named
#      in the spec: latin, common, code4, code6. All leading/trailing
#      whitespace is stripped from EVERY column (not just the 4 match
#      columns) on load, per spec step 1 - some of the real file's other
#      columns (e.g. state lists) could plausibly pick up stray whitespace
#      too, and output.format can return any of them (see next point).
#   2. output.format can be ANY header found in the reference database - the
#      spec says so explicitly ("possible outputs are any one of the headers
#      found in reference database"), so with the real file this means
#      output.format also accepts e.g. "fedstatus", "iucnstatus",
#      "states.present", etc. - not just latin/common/code4/code6. Matching
#      (finding the right row) is still restricted to just latin/common/
#      code4/code6 per the spec's Steps section. An output.format that isn't
#      one of the reference table's headers is an error, not a silent
#      fallback. Default remains "common".
#   3. Matching ignores case and treats underscores/dashes as equivalent to
#      spaces, and collapses/strips whitespace, on BOTH sides (input and
#      reference table) before comparing - e.g. "Silver-Haired_Bat",
#      "silver haired bat", and " Silver  Haired Bat " all match the same
#      reference row (real value: "Silver-haired bat"). This is applied only
#      for the purpose of finding a match; the VALUE returned always comes
#      from the reference table's original (untouched, just
#      whitespace-trimmed) text for the requested output.format column -
#      input formatting is never echoed back for a matched element.
#   4. A given input element is searched against all four match columns
#      (latin, common, code4, code6) - whichever column it matches in, the
#      same row's output.format column is returned. The real reference file
#      has no duplicate keys in any of the four match columns (confirmed
#      programmatically), so first-vs-last-match tie-breaking never actually
#      comes up on this data - if it ever did, the first matching row wins
#      (same convention as batz.datawrangler_rename's match.first = TRUE).
#   5. grammar.dash = TRUE (default) leaves hyphens in the output value as
#      given in the reference table. FALSE replaces every "-" in the
#      returned value with a space (spec: "replace the hyphens in the result
#      with a space") - this only touches the OUTPUT value, never the
#      matching step (assumption 3 already treats hyphens as space-equivalent
#      for matching regardless of this flag).
#   6. data may be a plain vector or a data frame. A data frame is recoded
#      element-wise across EVERY column (no column-selection argument, same
#      convention as batz.datawrangler_rename) and comes back as a data
#      frame of the same dimensions with columns as character vectors.
#   7. An input element with no match anywhere in the reference table is
#      returned UNCHANGED (exactly as given, not normalized) - same
#      pass-through convention as batz.datawrangler_rename.
#   8. Unmatched-element reporting: X = every unmatched INSTANCE (not just
#      distinct values, matching the "instances" convention already used in
#      batz.datawrangler_rename's missing.count). The warning vector itself
#      is the UNIQUE unmatched values. Per spec ("Print the following
#      warning with the 25 elements of the warning vector"), only the first
#      25 unique unmatched values are shown in the printed warning; if there
#      are more than 25, the message notes how many were omitted (a small
#      addition on top of the literal spec, flagged here).
#   9. For a data frame input, unmatched-instance counting/reporting is done
#      once across the WHOLE data frame (all columns flattened together),
#      not per column - consistent with how batz.datawrangler_rename's
#      missing.count/missing.list behave for data frame input.
#   10. ADDED 2026-08-27, per Josh's request ("update batz.batusa_recode.names
#      with new entries of All detections, 40KHzMyo, HiF, LoF, HiFrag,
#      LoFrag, Multiple, Social - ignore case when matching and use these
#      formats"): these 8 non-species detection/category labels are now
#      recognized. They are NOT a separate lookup mechanism - they're 8
#      ordinary new ROWS appended to the same nabat.names reference table,
#      so they go through the exact same match.cols/normalize() logic as
#      every species row (no new code path needed). For each new row,
#      latin/common/code4/code6 are all set to the identical literal string
#      (e.g. all four = "40KHzMyo") - this means (a) matching works no
#      matter which of the four "kinds" of identifier an input looks like,
#      and (b) output.format = any of latin/common/code4/code6 all return
#      the exact literal casing Josh gave, never a re-cased variant. The
#      other 11 output.format columns (fedstatus, iucnstatus, states.*,
#      state.soc, fed.proposed, hibernation.strat, phonic.group) are set to
#      "" for these 8 rows, since none of them semantically apply to a
#      non-species label; notes carries a short explanatory string instead
#      of "" for these rows only. These values live in NAbat.names.csv
#      itself now (updated 2026-08-27), not just in the function - see the
#      csv's own row count (62, was 54) and the 8 new rows appended at the
#      end. Matching is case-insensitive by the SAME mechanism already used
#      for species (normalize()'s tolower()) - no separate case-insensitivity
#      logic was needed or added. Internal dashes/underscores in an input
#      (e.g. "hi-f", "hi_f") do NOT match "HiF", since "HiF" itself has no
#      internal separator to normalize away - consistent with how e.g.
#      "silverhairedbat" (no separator) would not match "Silver-haired bat"
#      either; only whitespace/dash/underscore *equivalence* is assumed, not
#      their removal.
# =============================================================================

# -----------------------------------------------------------------------------
# Real reference database, as supplied by Josh (NAbat.names.csv), loaded from
# disk here for dev/testing. In the final function this same table is
# embedded directly in the function body (see batz.batusa_recode.names.R) -
# there's no reference-file-path input in the spec, so it's baked in.
# -----------------------------------------------------------------------------
nabat.names <- read.csv("NAbat.names.csv", stringsAsFactors = FALSE, check.names = FALSE)
nabat.names[] <- lapply(nabat.names, function(col) trimws(as.character(col)))

cat("=== nabat.names (real reference database) ===\n")
cat("dim:", dim(nabat.names), "\n")
print(head(nabat.names[, c("latin", "common", "code4", "code6")], 10))
print(tail(nabat.names[, c("latin", "common", "code4", "code6")], 8))

# SYNTHETIC test vector (NAbat.namestest stand-in - see caveat above) built
# from real species in the reference file, mixing formats/case/punctuation,
# plus values with no match at all.
nabat.namestest <- c(
  "epfu", "Myotis Lucifugus", "Hoary_Bat", "labo", "silver haired bat",
  "  TRI-COLORED BAT  ", "corynorhinus townsendii", "Big Free Tailed Bat",
  "not.a.real.bat", "myse", "gray_bat", "unmatched_species_2",
  "not.a.real.bat", "COTOIN", "eastern small-footed myotis"
)

# ADDED 2026-08-27: synthetic test vector covering the 8 new category-label
# rows, mixing case (and, for a couple, dash/underscore where the label has
# no internal separator to normalize - deliberately included as a
# should-NOT-match check per assumption 10 above).
nabat.categorytest <- c(
  "all detections", "ALL DETECTIONS", "40khzmyo", "40KHZMYO", "hif", "HIF",
  "lof", "LOF", "hifrag", "HIFRAG", "lofrag", "LOFRAG", "multiple", "MULTIPLE",
  "social", "SOCIAL", "  Hif  ", "hi-f", "hi_f"
)

cat("\n=== nabat.namestest (synthetic species input vector) ===\n"); print(nabat.namestest)
cat("\n=== nabat.categorytest (synthetic category-label input vector) ===\n"); print(nabat.categorytest)

# -----------------------------------------------------------------------------
# normalize(): matching-only normalization - trims, folds case, and treats
# underscores/dashes as spaces (then collapses runs of whitespace).
# -----------------------------------------------------------------------------
normalize <- function(x) {
  x <- as.character(x)
  x <- gsub("[-_]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  tolower(x)
}

# -----------------------------------------------------------------------------
# recode.vec(): core element-wise lookup against the reference table.
# Returns list(values = <recoded vector>, unmatched = <original unmatched
# values, in input order, WITH duplicates - i.e. every unmatched instance>).
# -----------------------------------------------------------------------------
recode.vec <- function(x, reference, output.format, grammar.dash = TRUE) {

  match.cols <- c("latin", "common", "code4", "code6")

  lookup.values <- unlist(lapply(match.cols, function(cn) normalize(reference[[cn]])),
                           use.names = FALSE)
  lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

  x.chr  <- as.character(x)
  x.norm <- normalize(x.chr)

  match.idx <- match(x.norm, lookup.values)
  row.idx   <- lookup.rowidx[match.idx]   # NA where match.idx is NA
  found     <- !is.na(row.idx)

  out <- x.chr
  out[found] <- as.character(reference[[output.format]][row.idx[found]])

  if (!grammar.dash) {
    out[found] <- gsub("-", " ", out[found])
  }

  list(values = out, unmatched = x.chr[!found])
}

# -----------------------------------------------------------------------------
# batz.batusa_recode.names(data, output.format = "common", grammar.dash = TRUE)
# -----------------------------------------------------------------------------
batz.batusa_recode.names <- function(data, output.format = "common", grammar.dash = TRUE) {

  match.cols <- c("latin", "common", "code4", "code6")

  reference <- nabat.names
  reference[] <- lapply(reference, function(col) trimws(as.character(col)))

  if (!(output.format %in% names(reference))) {
    stop(sprintf("output.format must be one of the reference database's headers: %s (got '%s')",
                  paste(names(reference), collapse = ", "), output.format))
  }

  if (is.data.frame(data)) {
    results <- lapply(data, recode.vec, reference = reference,
                       output.format = output.format, grammar.dash = grammar.dash)
    out <- as.data.frame(lapply(results, function(r) r$values),
                          stringsAsFactors = FALSE)
    names(out) <- names(data)
    unmatched.all <- unlist(lapply(results, function(r) r$unmatched), use.names = FALSE)
  } else {
    result <- recode.vec(data, reference = reference, output.format = output.format,
                          grammar.dash = grammar.dash)
    out <- result$values
    unmatched.all <- result$unmatched
  }

  if (length(unmatched.all) > 0) {
    warning.vector <- unique(unmatched.all)
    shown <- head(warning.vector, 25)
    omitted.note <- if (length(warning.vector) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(warning.vector))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match: %s%s\n",
                 length(unmatched.all), paste(shown, collapse = ", "), omitted.note))
  }

  out
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n=== default output.format = 'common' ===\n")
print(batz.batusa_recode.names(nabat.namestest))

cat("\n=== output.format = 'latin' ===\n")
print(batz.batusa_recode.names(nabat.namestest, output.format = "latin"))

cat("\n=== output.format = 'code4' ===\n")
print(batz.batusa_recode.names(nabat.namestest, output.format = "code4"))

cat("\n=== output.format = 'code6' ===\n")
print(batz.batusa_recode.names(nabat.namestest, output.format = "code6"))

cat("\n=== output.format beyond the 4 match columns - 'fedstatus' ===\n")
print(batz.batusa_recode.names(nabat.namestest, output.format = "fedstatus"))

cat("\n=== output.format = 'states.present' ===\n")
print(batz.batusa_recode.names(c("epfu", "myse"), output.format = "states.present"))

cat("\n=== grammar.dash = FALSE (hyphens -> spaces in output only) ===\n")
print(batz.batusa_recode.names(c("epfu", "lano", "coto"), output.format = "common",
                                grammar.dash = FALSE))
cat("(compare to grammar.dash = TRUE, default, hyphens kept):\n")
print(batz.batusa_recode.names(c("epfu", "lano", "coto"), output.format = "common"))

cat("\n=== data frame input (every column recoded, same dims back) ===\n")
test.df <- data.frame(
  col.a = c("epfu", "mylu", "not.a.real.bat"),
  col.b = c("Hoary_Bat", "labo", "another.fake.bat"),
  stringsAsFactors = FALSE
)
print(test.df)
print(batz.batusa_recode.names(test.df, output.format = "code4"))

cat("\n=== invalid output.format should error ===\n")
tryCatch(
  batz.batusa_recode.names(nabat.namestest, output.format = "family"),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== single unmatched element, no data frame, sanity check on pass-through ===\n")
print(batz.batusa_recode.names("totally_unknown_bat"))

cat("\n=== disambiguating similarly-spelled species (Corynorhinus townsendii vs.\n",
    "its two subspecies coti/cotv) still resolve to the right row ===\n", sep = "")
print(batz.batusa_recode.names(c("coto", "coti", "cotv"), output.format = "common"))

cat("\n=== output.format = 'hibernation.strat' (added 2026-08-25, 15-column\n",
    "NAbat.names.csv) - 'tabr' has both migratory and resident populations ===\n", sep = "")
print(batz.batusa_recode.names("tabr", output.format = "hibernation.strat"))

cat("\n=== output.format = 'phonic.group' (added 2026-08-25) - 'mylu' calls\n",
    "above 35 kHz ===\n", sep = "")
print(batz.batusa_recode.names("mylu", output.format = "phonic.group"))

cat("\n=== output.format = 'notes' - flagged lower-confidence/caveat species ===\n")
print(batz.batusa_recode.names(c("nole", "maca"), output.format = "notes"))

cat("\n=== data frame input recoded to 'hibernation.strat' (regression check\n",
    "that the new columns work through the data-frame path too) ===\n", sep = "")
print(batz.batusa_recode.names(test.df, output.format = "hibernation.strat"))

# -----------------------------------------------------------------------------
# NEW tests, added 2026-08-27, for the 8 non-species category-label rows
# -----------------------------------------------------------------------------
cat("\n=== NEW 2026-08-27: category labels match case-insensitively, default\n",
    "output.format = 'common' ===\n", sep = "")
print(batz.batusa_recode.names(nabat.categorytest))

cat("\n=== NEW 2026-08-27: 'hi-f'/'hi_f' do NOT match 'HiF' (no internal\n",
    "separator in the reference value to normalize away - see assumption 10)\n",
    "=> both should print unchanged in the tail of the vector above, and this\n",
    "single-element call should trigger the WARNING path ===\n", sep = "")
print(batz.batusa_recode.names("hi-f"))

cat("\n=== NEW 2026-08-27: output.format = latin/code4/code6 for a category\n",
    "label all return the identical literal casing given ===\n", sep = "")
print(batz.batusa_recode.names("hifrag", output.format = "latin"))
print(batz.batusa_recode.names("hifrag", output.format = "code4"))
print(batz.batusa_recode.names("hifrag", output.format = "code6"))

cat("\n=== NEW 2026-08-27: species-only output.format columns return '' for\n",
    "category-label rows ===\n", sep = "")
print(batz.batusa_recode.names(c("hif", "social", "multiple"), output.format = "fedstatus"))
print(batz.batusa_recode.names(c("hif", "social", "multiple"), output.format = "phonic.group"))

cat("\n=== NEW 2026-08-27: species and category labels mixed in one call ===\n")
print(batz.batusa_recode.names(c("epfu", "LoF", "Hoary_Bat", "Social", "not.a.real.bat")))

cat("\n=== NEW 2026-08-27: species and category labels mixed in a data frame ===\n")
test.df2 <- data.frame(
  col.a = c("epfu", "HIF", "not.a.real.bat"),
  col.b = c("Hoary_Bat", "lofrag", "Social"),
  stringsAsFactors = FALSE
)
print(test.df2)
print(batz.batusa_recode.names(test.df2, output.format = "code4"))

# =============================================================================
# batz.treeusa_recode.names.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.treeusa_recode.names() - tested here against the real
# reference database before being wrapped into the final function
# (batz.treeusa_recode.names.R).
#
# Purpose (per Josh's spec): given a vector of Maine tree/shrub identifiers
# (any mix of scientific name or either of two common names), look each one
# up in a reference database loaded from disk and return it re-expressed in
# one or more requested output headers (head.out). Reuses the same core
# lookup/pass-through/reporting logic already built for
# batz.batusa_recode.names(), generalized to (a) load its reference table
# from a file on disk instead of an embedded table, and (b) support more than
# one output header at once.
#
# NAME NORMALIZATION (per Josh's naming conventions - see preferences.md):
#   - Requested name was "bats.treeusa_recode.names()" - "bats." normalized
#     to "batz." (the package prefix used by every other function here; read
#     as a typo, not an intentional new prefix). The family/action/subject
#     split (treeusa_recode.names) already matches the
#     batz.<family>_<action>.<subject>() convention exactly - it's the
#     direct tree/shrub-database sibling of batz.batusa_recode.names()
#     (family = treeusa, action = recode, subject = names). Flagging the
#     "bats"->"batz" normalization to Josh per project convention, not
#     silently renamed.
#
# REFERENCE FILE - REAL NAMING MISMATCH FOUND, NOT SILENTLY FIXED:
#   - Josh's spec says the pattern is "maine_tree_species_and_shrubs" and the
#     file lives in "reference database files". Checked the real "4 Current
#     test data" folder on Josh's machine (connected mid-session): a file
#     named EXACTLY "maine_tree_species_and_shrubs.csv" does exist, but it
#     sits at the TOP LEVEL of "4 Current  test data", not inside "reference
#     database files". The copy actually inside "reference database files"
#     is named "tree_species_and_shrubs.csv" - missing the "maine_" prefix.
#     Confirmed (byte-for-byte diff) the two files are IDENTICAL content, so
#     this is just an inconsistent filename between the two copies, not a
#     different/older dataset.
#   - 2026-09-14 update (per Josh): the internal reference data frame is now
#     called reference.plants (was reference), and the canonical file name
#     is "USA.treeshrub_recode.names.csv" (matching NAbat.names.csv/
#     USAstates.names.csv naming elsewhere in this package) - a THIRD name
#     for what appears to be the same table. load.pattern's default is now
#     a vector matching either the new canonical name or the previously-
#     established real file name(s), so the function still finds whichever
#     copy Josh actually has. **Flagging for Josh: please confirm which
#     single name should be canonical going forward.**
#   - Real reference file structure (verified directly, 121 species/shrub
#     rows x 11 columns): $species, $common_one, $common_two, $genus,
#     $family, $native_status, $growth_habit, $wood_type, $grouping_one,
#     $grouping_two, $grouping_three. $common_two is blank for 43 of the 121
#     rows (not every species has a second common name) - blanks never
#     match anything (by design - see normalize.tree() below) and are only
#     ever a problem if $common_two is itself the requested head.out for a
#     row with no second name, in which case that head.out column is just
#     "" for that row (same "value straight from the reference table" idea
#     as everywhere else in this project - not a bug).
#   - Real duplicate-key check across species/common_one/common_two (case/
#     punctuation-insensitive): 3 genuine collisions exist in the real data
#     - "Juneberry" ($common_two) is shared by three different species
#     (Amelanchier arborea/canadensis/laevis), and "Filbert" ($common_two)
#     by two (the two Corylus species). No parameter was requested to
#     control tie-breaking for this function (unlike
#     batz.datawrangler_rename's match.first) - defaulted to FIRST match in
#     species/common_one/common_two column order, then file row order (same
#     "first match wins" convention already used elsewhere in this project
#     when no tie-break rule is specified). Flagging this for Josh since an
#     input of "Juneberry" alone is genuinely ambiguous in the source data.
#
# STEPS / ASSUMPTIONS (spec was silent or ambiguous on some of these -
# flagging per project convention):
#   1. Matching columns are exactly the three named in the spec: $species,
#      $common_one, $common_two - hardcoded (not parameterized), same as
#      batz.batusa_recode.names' fixed match.cols. All columns are
#      whitespace-trimmed on load.
#   2. "ignore missing spaces and special characters" (Josh's wording) is
#      read as a STRONGER normalization than batz.batusa_recode.names' -
#      that function only treats dashes/underscores as space-equivalent and
#      collapses whitespace; this one strips ALL whitespace and punctuation
#      entirely (not just collapsing to a single space) before comparing,
#      so e.g. "Ash-leaved Maple", "ash leaved maple", "Ashleaved_Maple",
#      and "ASHLEAVEDMAPLE" all match the same real reference row. Case is
#      folded too. This normalization is matching-only - the VALUE returned
#      always comes from the reference table's original (trimmed, not
#      de-punctuated) text.
#   3. `data` is a plain vector only, per the spec's literal
#      "Input: data = vector()" (unlike batz.batusa_recode.names, which
#      also accepts a data frame) - passing a data frame is an error here,
#      a deliberately narrower scope than the bat version, not an
#      oversight.
#   4. `head.out` selects one or more of the reference database's OWN
#      column names as output - same "any header in the reference table"
#      idea as batz.batusa_recode.names' batname.format.out, but supporting
#      more than one at once: length(head.out) == 1 returns a plain vector
#      (same shape as the bat version); length(head.out) > 1 returns a data
#      frame with one column per requested head.out entry, same row order/
#      length as `data`. An unrecognized head.out entry is an error (same
#      convention as the bat version). Default head.out = "common_one".
#      Matching a data element to a reference row still only ever uses
#      species/common_one/common_two, regardless of what's requested in
#      head.out.
#   5. An input element with no match anywhere in the reference table is
#      passed through UNCHANGED in every requested head.out column (same
#      pass-through convention as batz.batusa_recode.names/
#      batz.datawrangler_rename) - never NA, never an error.
#   6. `treesmismatch.log` (per spec's literal column names/name, kept as
#      given): built only when at least one input didn't match, with
#      columns $input (each unique unmatched value), $missmatch_count (how
#      many times that exact value occurs in THIS call's `data`, i.e.
#      per-call instance count, same "instances" convention used elsewhere
#      in this project), and $closest.match (the single nearest reference
#      entry by string edit distance - see point 7 below). Auto-assigned
#      into the caller's environment (same bare-call-populates-workspace
#      convention already used by batz.generate_arumeta.eventlog/
#      batz.merge_aru.meta/etc.) rather than returned as part of a wrapped
#      list, since the spec's primary output is the recoded vector/data
#      frame itself, not a list.
#   7. $closest.match: computed with base R's `utils::adist()` (Levenshtein
#      edit distance) between the unmatched input's normalized form (same
#      normalize.tree() as matching) and every reference row's normalized
#      species/common_one/common_two values pooled together; the reference
#      entry with the smallest distance wins (first one, in
#      species/common_one/common_two column order, on an exact tie) and its
#      ORIGINAL (trimmed, not de-punctuated) text is what's stored in
#      $closest.match. Not explicitly specified in the spec (which only
#      says "closest match in the reference database") - a standard
#      string-distance interpretation, flagged in case Josh meant something
#      more specific (e.g. restricted to a single column).
#   8. Print step ("These inpusts were missing" - obvious typo, corrected to
#      "These inputs were missing:" and flagged here since it reads as a
#      one-off typo, not an intentional literal string) runs under the same
#      condition as building `treesmismatch.log` and prints the unique
#      unmatched values (same vector as the log's $input column).
#   9. **Standardized (2026-09-14, per Josh - project-wide header
#      standardization preference).** The reference database's own column
#      names are a literal, uninvented copy of that file's real header
#      text, so they're run through standardize.headers() (trim whitespace,
#      collapse every run of non-alphanumeric characters to a single
#      underscore, lowercase) right after loading - inlined near the top of
#      this script since it's a standalone dev script (the package .R file
#      calls the shared internal helper of the same name directly instead).
#      This is a no-op against the current real reference file (its columns
#      are already exactly this snake_case shape - see the "Real reference
#      file structure" note above), but guards against a future copy of the
#      file picking up stray whitespace/case/punctuation differences.
#      match.cols and the default head.out are already written as their
#      standardized spellings, so nothing else needed to change.
#  10. **Added 2026-09-14, per Josh: optional supplemental reference data
#      (reference.data/pattern).** New params: dir.load/dir.sub are reused
#      (dir.sub's default flips to TRUE - see below); `pattern` (default
#      "plant.names.csv", vector-friendly) names supplemental CSV(s) to
#      search for; `reference.data` ("default"/"append"/"overwrite") turns
#      the feature on. Only when reference.data != "default": dir.load (and
#      subdirectories, if dir.sub) is searched for `pattern`; each matched
#      file's headers are standardized then matched to reference.plants'
#      10 non-wood_type columns by (1) exact standardized name, (2) a
#      keyword match on the header text, (3) a content-signature fallback
#      (binomial-looking text -> species; small controlled vocabularies ->
#      native_status/growth_habit) - flagged as a best-effort heuristic,
#      not a guarantee, since "match using header names AND content" has no
#      single obvious algorithm. A file missing species, every common name
#      column, or every grouping column entirely is skipped (message
#      printed, not added). A file that's missing only one of
#      common_one/common_two, or one or two of grouping_one/two/three, has
#      the missing slot(s) filled by RECYCLING the other/first-present
#      value (flagged: recycling from "the first present grouping column"
#      specifically is an arbitrary tie-break, since the spec doesn't say
#      which level to prefer). Missing $genus is derived from the first
#      word of $species. Missing $family/$native_status/$growth_habit are
#      padded with NA. If nothing had to be recycled/derived/padded, a
#      "complete success very nice!" message prints instead of the "loaded
#      but padded..." one (per Josh's literal wording). All matched files'
#      reconciled rows are folded into one reference.plants.temp, and then:
#      reference.plants.all <- reference.plants (copy); "append" adds
#      reference.plants.temp's rows onto it (column union, so wood_type is
#      NA for the new rows); "overwrite" replaces it entirely with
#      reference.plants.temp (falling back to the unmodified
#      reference.plants if nothing was actually loaded - flagged, since an
#      empty reference table would break the rest of the function).
#      reference.plants.all (not reference.plants) is what matching/
#      head.out validation actually use from here on.
#  11. **dir.sub default changed 2026-09-14, per Josh:** dir.sub now
#      defaults to TRUE (was FALSE) - both the base reference file and any
#      supplemental files are searched recursively by default now.
# =============================================================================

# -----------------------------------------------------------------------------
# real reference database, copied locally for this dev run (verified against
# Josh's real "4 Current  test data" folder - see naming-mismatch note above)
# -----------------------------------------------------------------------------
test.dir <- "treeusa_testdata"
cat("=== files in test.dir ===\n"); print(list.files(test.dir))

reference.preview <- read.csv(file.path(test.dir, "maine_tree_species_and_shrubs.csv"),
                               stringsAsFactors = FALSE, check.names = FALSE)
cat("=== real reference database ===\n")
cat("dim:", dim(reference.preview), "\n")
cat("columns:", paste(names(reference.preview), collapse = ", "), "\n")
print(head(reference.preview, 6))

# -----------------------------------------------------------------------------
# real-data test input vector - exact species/common_one/common_two values,
# case/punctuation/whitespace variants, the Juneberry/Filbert ambiguity, a
# blank-common_two species requested via head.out = "common_two", and values
# with no match at all.
# -----------------------------------------------------------------------------
tree.test <- c(
  "Sugar Maple", "acer rubrum", "NORTHERN RED OAK", "paper-birch",
  "  eastern white pine  ", "Ash_Leaved_Maple", "ASHLEAVEDMAPLE",
  "not.a.real.tree", "Balsam Fir", "ironwood", "not.a.real.tree",
  "Trembling Aspen", "sugarmaple", "Quaking Aspen", "swamp maple",
  "Juneberry", "American Mountain-ash", "americanmountainash",
  "not.a.real.tree.either"
)
cat("\n=== tree.test (real-data input vector) ===\n"); print(tree.test)

# -----------------------------------------------------------------------------
# helper: matching-only normalization - fold case, strip ALL whitespace and
# punctuation entirely (stronger than batz.batusa_recode.names' normalize(),
# per assumption 2 above - "ignore missing spaces and special characters").
# -----------------------------------------------------------------------------
normalize.tree <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

## ---- helper: standardize.headers (per Josh, 2026-09-14 project
## preference) - inlined here since this is a standalone dev script, not
## part of the package (package .R files call the shared internal helper of
## the same name directly instead). Trims whitespace, collapses every run of
## non-alphanumeric characters to a single underscore, strips a
## leading/trailing underscore, and lowercases. -----------------------------
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

# -----------------------------------------------------------------------------
# helper: read one .csv/.xlsx file into a trimmed character data frame.
# -----------------------------------------------------------------------------
read.ref.file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "csv") {
    df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reference file '", basename(f), "' is an Excel file, but the 'readxl' package is not installed.")
    }
    df <- as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
  } else {
    stop("Reference file '", basename(f), "' has an unsupported extension (expected .csv/.xlsx/.xls).")
  }
  df[] <- lapply(df, function(col) trimws(as.character(col)))
  df
}

# -----------------------------------------------------------------------------
# helper: load the base reference database from disk (dir.load/load.pattern/
# dir.sub) - supports .csv and .xlsx by extension.
# -----------------------------------------------------------------------------
load.tree.reference <- function(dir.load, load.pattern, dir.sub) {
  matches <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                         recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)
  if (length(matches) == 0) {
    stop(sprintf("No reference database file found in '%s' matching load.pattern '%s'.",
                  dir.load, paste(load.pattern, collapse = "', '")))
  }
  if (length(matches) > 1) {
    cat("NOTE: more than one file matched load.pattern - using the first: ",
        basename(matches[1]), "\n", sep = "")
  }
  ref <- read.ref.file(matches[1])

  ## header standardization (per Josh, 2026-09-14 project preference): the
  ## reference database's own column names are a literal, uninvented copy
  ## of that file's real header text, so they're run through
  ## standardize.headers() the same as any other loaded file's headers - a
  ## no-op against the current real reference file (already exactly this
  ## snake_case shape), but guards against future whitespace/case/
  ## punctuation drift in the file. See assumption 9 above.
  names(ref) <- standardize.headers(names(ref))
  ref
}

match.cols <- c("species", "common_one", "common_two")
plant.schema.cols <- c("species", "common_one", "common_two", "genus", "family",
                        "native_status", "growth_habit",
                        "grouping_one", "grouping_two", "grouping_three")

## ---- content-signature helpers for supplemental-file column matching
## (assumption 10 above) ------------------------------------------------
looks.like.binomial <- function(x) {
  x <- x[nzchar(x)]
  if (length(x) == 0) return(FALSE)
  mean(grepl("^[A-Z][a-z]+[ _][a-z]+", x)) > 0.7
}
native.status.vocab <- c("native", "introduced", "nonnative", "non native",
                          "invasive", "naturalized", "exotic", "adventive")
growth.habit.vocab   <- c("tree", "shrub", "vine", "tree shrub", "treeshrub",
                           "groundcover", "herb", "graminoid")
looks.like.vocab <- function(x, vocab) {
  x <- normalize.tree(x)
  x <- x[nzchar(x)]
  if (length(x) == 0) return(FALSE)
  mean(x %in% normalize.tree(vocab)) > 0.6
}

match.file.headers <- function(df, canonical.cols) {
  std <- standardize.headers(names(df))
  names(df) <- std

  mapped <- rep(NA_character_, length(std))

  exact <- std %in% canonical.cols
  mapped[exact] <- std[exact]
  already.used <- unique(mapped[exact])

  keyword.map <- list(
    species        = c("species", "latin", "scientific", "sciname"),
    genus          = c("genus"),
    family         = c("family"),
    native_status  = c("native", "status"),
    growth_habit   = c("growth", "habit"),
    common_one     = c("common"),
    common_two     = c("common"),
    grouping_one   = c("group", "grouping"),
    grouping_two   = c("group", "grouping"),
    grouping_three = c("group", "grouping")
  )
  for (i in which(!exact)) {
    h <- std[i]
    hit <- NA_character_
    for (cc in canonical.cols) {
      if (cc %in% already.used) next
      kws <- keyword.map[[cc]]
      if (!is.null(kws) && any(vapply(kws, function(k) grepl(k, h, fixed = TRUE), logical(1)))) {
        hit <- cc
        break
      }
    }
    if (!is.na(hit)) {
      mapped[i] <- hit
      already.used <- c(already.used, hit)
    }
  }

  for (i in which(is.na(mapped))) {
    col.vals <- df[[i]]
    if (!("species" %in% already.used) && looks.like.binomial(col.vals)) {
      mapped[i] <- "species"; already.used <- c(already.used, "species")
    } else if (!("native_status" %in% already.used) && looks.like.vocab(col.vals, native.status.vocab)) {
      mapped[i] <- "native_status"; already.used <- c(already.used, "native_status")
    } else if (!("growth_habit" %in% already.used) && looks.like.vocab(col.vals, growth.habit.vocab)) {
      mapped[i] <- "growth_habit"; already.used <- c(already.used, "growth_habit")
    }
  }

  names(df) <- ifelse(is.na(mapped), std, mapped)
  df <- df[names(df) %in% canonical.cols]
  df[!duplicated(names(df))]
}

build.plant.row.set <- function(df, canonical.cols) {
  padded <- character(0)

  has.c1 <- "common_one" %in% names(df)
  has.c2 <- "common_two" %in% names(df)
  if (has.c1 && !has.c2) { df$common_two <- df$common_one; padded <- c(padded, "common_two") }
  if (has.c2 && !has.c1) { df$common_one <- df$common_two; padded <- c(padded, "common_one") }

  grp.cols     <- c("grouping_one", "grouping_two", "grouping_three")
  present.grp  <- grp.cols[grp.cols %in% names(df)]
  if (length(present.grp) > 0 && length(present.grp) < 3) {
    source.col  <- present.grp[1]
    missing.grp <- setdiff(grp.cols, present.grp)
    for (gc in missing.grp) df[[gc]] <- df[[source.col]]
    padded <- c(padded, missing.grp)
  }

  if (!"genus" %in% names(df)) {
    df$genus <- vapply(strsplit(df$species, "\\s+"),
                        function(w) if (length(w) >= 1) w[1] else NA_character_,
                        character(1))
    padded <- c(padded, "genus")
  }

  for (cc in c("family", "native_status", "growth_habit")) {
    if (!cc %in% names(df)) {
      df[[cc]] <- NA_character_
      padded <- c(padded, cc)
    }
  }

  df <- df[canonical.cols]
  list(data = df, padded = padded)
}

rbind.fill <- function(a, b) {
  all.cols <- union(names(a), names(b))
  for (cc in setdiff(all.cols, names(a))) a[[cc]] <- NA
  for (cc in setdiff(all.cols, names(b))) b[[cc]] <- NA
  rbind(a[all.cols], b[all.cols])
}

# -----------------------------------------------------------------------------
# helper: core element-wise lookup for ONE requested head.out column.
# Returns list(values = <recoded vector, same length as x>,
#              found = <logical vector, same length as x>).
# -----------------------------------------------------------------------------
recode.one <- function(x, reference, head.col, lookup.values, lookup.rowidx) {
  x.chr  <- as.character(x)
  x.norm <- normalize.tree(x.chr)

  match.idx <- match(x.norm, lookup.values)
  row.idx   <- lookup.rowidx[match.idx]
  found     <- !is.na(row.idx)

  out <- x.chr
  out[found] <- as.character(reference[[head.col]][row.idx[found]])

  list(values = out, found = found)
}

# -----------------------------------------------------------------------------
# helper: closest-match lookup for the mismatch log (assumption 7 above).
# -----------------------------------------------------------------------------
closest.match.for <- function(x.norm.one, ref.pool.norm, ref.pool.raw) {
  d <- utils::adist(x.norm.one, ref.pool.norm)[1, ]
  ref.pool.raw[which.min(d)]
}

# -----------------------------------------------------------------------------
# batz.treeusa_recode.names(data, head.out, dir.load, load.pattern, dir.sub,
#                            pattern, reference.data)
# -----------------------------------------------------------------------------
batz.treeusa_recode.names <- function(data,
                                       head.out       = "common_one",
                                       dir.load       = getwd(),
                                       load.pattern   = c("*USA.treeshrub_recode.names*",
                                                           "*tree_species_and_shrubs*"),
                                       dir.sub        = TRUE,
                                       pattern        = "plant.names.csv",
                                       reference.data = "default") {

  if (!(is.character(reference.data) && length(reference.data) == 1 &&
        reference.data %in% c("default", "append", "overwrite"))) {
    stop("`reference.data` must be one of \"default\", \"append\", or \"overwrite\" (got: \"",
         paste(reference.data, collapse = ", "), "\").")
  }

  if (is.data.frame(data)) {
    stop("`data` must be a plain vector for batz.treeusa_recode.names() (not a data frame).")
  }

  reference.plants <- load.tree.reference(dir.load, load.pattern, dir.sub)
  reference.plants.all <- reference.plants

  if (reference.data %in% c("append", "overwrite")) {
    supp.matches <- list.files(dir.load, pattern = pattern.regex(pattern),
                                recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

    if (length(supp.matches) == 0) {
      cat(sprintf("NOTE: no files matching pattern '%s' were found in '%s' (dir.sub = %s) - %s using only reference.plants.\n",
                   paste(pattern, collapse = "', '"), dir.load, dir.sub,
                   if (reference.data == "overwrite") "nothing to overwrite with;" else "nothing to append;"))
    }

    reference.plants.temp <- NULL

    for (f in supp.matches) {
      df <- tryCatch(read.ref.file(f), error = function(e) {
        cat(sprintf("%s %s skipped as unreadable: %s\n", basename(f), f, conditionMessage(e)))
        NULL
      })
      if (is.null(df)) next

      df <- match.file.headers(df, plant.schema.cols)

      has.species <- "species" %in% names(df)
      has.common  <- any(c("common_one", "common_two") %in% names(df))
      has.group   <- any(c("grouping_one", "grouping_two", "grouping_three") %in% names(df))

      if (!(has.species && has.common && has.group)) {
        missing.req <- c(
          if (!has.species) "species (latin name)",
          if (!has.common)  "a common name column",
          if (!has.group)   "a grouping column"
        )
        cat(sprintf("%s %s skipped as missing required headers: %s\n",
                    basename(f), f, paste(missing.req, collapse = ", ")))
        next
      }

      built <- build.plant.row.set(df, plant.schema.cols)

      if (length(built$padded) == 0) {
        cat(sprintf("%s %s complete success very nice!\n", basename(f), f))
      } else {
        cat(sprintf("%s %s loaded but padded as missing these headers: %s\n",
                    basename(f), f, paste(built$padded, collapse = ", ")))
      }

      reference.plants.temp <- if (is.null(reference.plants.temp)) {
        built$data
      } else {
        rbind.fill(reference.plants.temp, built$data)
      }
    }

    if (!is.null(reference.plants.temp)) {
      if (reference.data == "append") {
        reference.plants.all <- rbind.fill(reference.plants.all, reference.plants.temp)
      } else if (reference.data == "overwrite") {
        reference.plants.all <- reference.plants.temp
      }
    }
  }

  reference <- reference.plants.all

  bad.head <- setdiff(head.out, names(reference))
  if (length(bad.head) > 0) {
    stop(sprintf("head.out must be (a) header(s) from the reference database's own columns: %s (got unrecognized: %s)",
                  paste(names(reference), collapse = ", "), paste(bad.head, collapse = ", ")))
  }

  lookup.values <- unlist(lapply(match.cols, function(cn) normalize.tree(reference[[cn]])),
                           use.names = FALSE)
  lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

  x.chr <- as.character(data)

  per.head <- lapply(head.out, function(hc) {
    recode.one(x.chr, reference, hc, lookup.values, lookup.rowidx)
  })
  names(per.head) <- head.out

  # "found" is identical across every head.out column (same match step) -
  # take it from the first.
  found <- per.head[[1]]$found

  if (length(head.out) == 1) {
    out <- per.head[[1]]$values
  } else {
    out <- as.data.frame(lapply(per.head, function(r) r$values), stringsAsFactors = FALSE)
    names(out) <- head.out
  }

  # ---- treesmismatch.log + console notice for unmatched inputs ----
  if (any(!found)) {
    unmatched.instances <- x.chr[!found]
    unmatched.unique    <- unique(unmatched.instances)

    ref.pool.raw  <- unlist(lapply(match.cols, function(cn) reference[[cn]]), use.names = FALSE)
    ref.pool.norm <- normalize.tree(ref.pool.raw)
    keep.pool     <- ref.pool.norm != ""   # blank $common_two cells contribute nothing to match
    ref.pool.raw  <- ref.pool.raw[keep.pool]
    ref.pool.norm <- ref.pool.norm[keep.pool]

    closest <- vapply(unmatched.unique, function(v) {
      v.norm <- normalize.tree(v)
      closest.match.for(v.norm, ref.pool.norm, ref.pool.raw)
    }, character(1))

    treesmismatch.log <- data.frame(
      input           = unmatched.unique,
      missmatch_count = as.integer(vapply(unmatched.unique, function(v) sum(unmatched.instances == v), integer(1))),
      closest.match   = closest,
      stringsAsFactors = FALSE
    )

    cat("These inputs were missing:\n")
    print(unmatched.unique)

    assign("treesmismatch.log", treesmismatch.log, envir = parent.frame())
  }

  out
}

# -----------------------------------------------------------------------------
# tests (against the REAL reference database)
# -----------------------------------------------------------------------------
cat("\n=== default head.out = 'common_one' ===\n")
print(batz.treeusa_recode.names(tree.test, dir.load = test.dir))
cat("\n=== treesmismatch.log ===\n"); print(treesmismatch.log)

cat("\n=== head.out = 'species' ===\n")
print(batz.treeusa_recode.names(tree.test, head.out = "species", dir.load = test.dir))

cat("\n=== head.out = 'common_two' (note: blanks pass through as '' for species\n",
    "with no second common name) ===\n", sep = "")
print(batz.treeusa_recode.names(c("Sugar Maple", "Balsam Fir", "Quaking Aspen"),
                                 head.out = "common_two", dir.load = test.dir))

cat("\n=== multi-column head.out -> data frame ===\n")
print(batz.treeusa_recode.names(tree.test, head.out = c("species", "common_one", "family", "growth_habit"),
                                 dir.load = test.dir))

cat("\n=== Juneberry ambiguity - first match in file row order wins ===\n")
print(batz.treeusa_recode.names("Juneberry", head.out = "species", dir.load = test.dir))
cat("(reference rows sharing 'Juneberry' as common_two, in file order:)\n")
print(reference.preview[normalize.tree(reference.preview$common_two) == "juneberry",
                         c("species", "common_one", "common_two")])

cat("\n=== fully-matched input (no treesmismatch.log expected) ===\n")
if (exists("treesmismatch.log")) rm(treesmismatch.log)
print(batz.treeusa_recode.names(c("Sugar Maple", "Northern Red Oak", "Eastern White Pine"), dir.load = test.dir))
cat("exists('treesmismatch.log') after a fully-matched call:", exists("treesmismatch.log"), "\n")

cat("\n=== invalid head.out should error ===\n")
tryCatch(
  batz.treeusa_recode.names(tree.test, head.out = "not.a.real.column", dir.load = test.dir),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== data frame input should error (spec says vector only) ===\n")
tryCatch(
  batz.treeusa_recode.names(data.frame(a = "Sugar Maple"), dir.load = test.dir),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== no file matching load.pattern should error clearly ===\n")
empty.dir <- file.path(tempdir(), "treeusa_empty"); dir.create(empty.dir, showWarnings = FALSE)
tryCatch(
  batz.treeusa_recode.names(tree.test, dir.load = empty.dir),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== literal spec pattern 'maine_tree_species_and_shrubs' also still works\n",
    "when pointed at a copy that DOES carry the 'maine_' prefix ===\n", sep = "")
print(batz.treeusa_recode.names("Sugar Maple", dir.load = test.dir,
                                 load.pattern = "*maine_tree_species_and_shrubs*"))

cat("\n=== duplicate unmatched inputs - missmatch_count should reflect instance count ===\n")
dup.test <- c("not.a.real.tree", "Sugar Maple", "not.a.real.tree", "not.a.real.tree", "also.fake")
print(batz.treeusa_recode.names(dup.test, dir.load = test.dir))
print(treesmismatch.log)

# =============================================================================
# NEW 2026-09-14 tests: reference.data / pattern (supplemental reference data)
# Uses a synthetic scratch folder (not the real reference data) so these
# tests are self-contained and don't depend on any specific file Josh may or
# may not have under test.dir - mirrors the same synthetic-fixture approach
# already used above for the "no file matching load.pattern" test.
# =============================================================================
supp.dir <- file.path(tempdir(), "treeusa_supp_test")
supp.sub <- file.path(supp.dir, "sub")
dir.create(supp.sub, recursive = TRUE, showWarnings = FALSE)

## copy the real base reference file into the scratch folder so load.pattern
## still finds a base table there
file.copy(file.path(test.dir, "maine_tree_species_and_shrubs.csv"),
          file.path(supp.dir, "maine_tree_species_and_shrubs.csv"), overwrite = TRUE)

## file 1 (top level): all 10 canonical headers present natively -> expect
## the "complete success very nice!" message
writeLines(c(
  "species,common_one,common_two,genus,family,native_status,growth_habit,grouping_one,grouping_two,grouping_three",
  "Betula papyrifera,Paper Birch,White Birch,Betula,Betulaceae,Native,Tree,Birch,Softwood Hardwood,Common"
), file.path(supp.dir, "plant.names.csv"))

## file 2 (in a subdirectory - exercises dir.sub): only Scientific Name/
## Common Name/Group One - missing common_two, grouping_two, grouping_three,
## genus, family, native_status, growth_habit -> expect recycling + genus
## derivation + NA-padding, and the "loaded but padded..." message
writeLines(c(
  "Scientific Name,Common Name,Group One",
  "Cornus sericea,Red-osier Dogwood,Dogwood"
), file.path(supp.sub, "shrub_extra.csv"))

## file 3 (top level): missing species/common/grouping entirely -> expect
## the "skipped as missing required headers" message
writeLines(c(
  "Genus,Family",
  "Betula,Betulaceae"
), file.path(supp.dir, "missing_required.csv"))

cat("\n=== reference.data = 'default' (new params present but inert) ===\n")
print(batz.treeusa_recode.names(c("sugar maple", "paper birch"), dir.load = supp.dir, dir.sub = TRUE))
cat("(\"paper birch\" should NOT match here - it only exists in the supplemental file)\n")

cat("\n=== reference.data = 'append', pattern matches all 3 synthetic files ===\n")
out.append <- batz.treeusa_recode.names(
  c("sugar maple", "paper birch", "red-osier dogwood"),
  dir.load = supp.dir, dir.sub = TRUE,
  pattern = c("plant.names.csv", "shrub_extra.csv", "missing_required.csv"),
  reference.data = "append",
  head.out = c("common_one", "genus", "family", "native_status", "growth_habit", "grouping_one")
)
print(out.append)
cat("(expect: missing_required.csv skipped; plant.names.csv 'complete success';\n",
    "shrub_extra.csv 'loaded but padded...'; all three test inputs matched)\n", sep = "")

cat("\n=== reference.data = 'overwrite' - only supplemental rows used ===\n")
out.overwrite <- batz.treeusa_recode.names(
  c("paper birch", "sugar maple"),
  dir.load = supp.dir, dir.sub = TRUE,
  pattern = c("plant.names.csv", "shrub_extra.csv"),
  reference.data = "overwrite",
  head.out = "common_one"
)
print(out.overwrite)
cat("(expect: 'paper birch' matches; 'sugar maple' does NOT - base table was discarded)\n")
print(treesmismatch.log)

cat("\n=== reference.data = 'overwrite' with no matching supplemental files - falls back\n",
    "to reference.plants unmodified ===\n", sep = "")
print(batz.treeusa_recode.names("sugar maple", dir.load = supp.dir, dir.sub = TRUE,
                                 pattern = "no_such_file_xyz.csv", reference.data = "overwrite"))

cat("\n=== invalid reference.data value should error ===\n")
tryCatch(
  batz.treeusa_recode.names("sugar maple", dir.load = supp.dir, reference.data = "bogus"),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== dir.sub now defaults to TRUE - confirm the subdirectory file is found\n",
    "without passing dir.sub explicitly ===\n", sep = "")
out.default.dirsub <- batz.treeusa_recode.names(
  "red-osier dogwood", dir.load = supp.dir,
  pattern = "shrub_extra.csv", reference.data = "append"
)
print(out.default.dirsub)

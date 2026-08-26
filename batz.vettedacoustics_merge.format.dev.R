# =============================================================================
# batz.vettedacoustics_merge.format.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.vettedacoustics_merge.format() - tested here before
# being wrapped into the final function (batz.vettedacoustics_merge.format.R).
#
# Purpose: search a directory (and its subdirectories, optionally) for vetted
# bat-acoustic-call files exported from k-Pro/Kaleidoscope or SonoBat vetting
# software, merge every file that has the expected columns into one master
# data frame, and log every file that gets skipped and why.
#
# NAME NORMALIZATION (per Josh's naming conventions - see project
# preferences.md): Josh's spec named it "batz.vettedacoustics_merge&format()"
# - normalized "&" to "." (same normalization already applied to
# batz.arumeta.merge&format -> batz.arumeta_merge.format and
# batz.sm4logfile_merge&format -> batz.sm4logfile_merge.format - this is now
# an established, repeated pattern in Josh's own spec template, not a
# one-off typo).
#
# TEST DATA STATUS - REAL DATA USED:
#   - The real "4 Current  test data" folder (device folder access granted
#     earlier this session) has a real file called "FinalVetted.csv"
#     (6,487 rows x 35 columns) - this is a real k-Pro/Kaleidoscope vetting
#     export, not the "*vetted.csv" name given in the spec's load.pattern,
#     but it DOES end in "...Vetted.csv" (case differs: "Vetted" vs
#     "vetted") - the spec's default load.pattern only matches it at all if
#     file-pattern matching is done CASE-INSENSITIVELY. Implemented
#     case-insensitive matching (same convention already used in
#     batz.arumeta_merge.format for its own real-world case-inconsistent
#     filenames) - confirmed necessary by this exact real file, not just a
#     defensive guess.
#   - This real file's 35 raw headers were checked against the spec's 7
#     expected (standardized) headers - all 7 are genuinely present, which
#     is what let a real bug in the SPEC's own header-standardization
#     snippet get caught (see below) rather than being missed by a
#     synthetic-only test.
#   - To exercise the multi-file/multi-directory merge, duplicate-removal,
#     and both skip-reason branches, the real file was split into pieces
#     (real data throughout, not resynthesized) plus two purely-synthetic
#     edge-case files:
#       testdata/site1_vetted.csv       - real rows 1-50, top level
#       testdata/sub1/site2_Vetted.csv  - real rows 51-100 + 3 rows
#                                          duplicated from site1 (tests
#                                          duplicates.remove), in a
#                                          SUBDIRECTORY (tests dir.sub),
#                                          filename case "Vetted" varied on
#                                          purpose (tests case-insensitive
#                                          load.pattern matching again)
#       testdata/notes.csv              - real rows 101-105, but named so
#                                          it does NOT match load.pattern at
#                                          all (should be silently ignored,
#                                          not logged)
#       testdata/badheaders_vetted.csv  - real rows 106-110 with the
#                                          "Species Manual ID" column
#                                          deliberately dropped (SYNTHETIC
#                                          edit - tests the "mismatched
#                                          headers" skip branch)
#       testdata/empty_vetted.csv       - real headers, zero data rows
#                                          (SYNTHETIC - tests the "no
#                                          records" skip branch)
#
# REAL BUG CAUGHT IN THE SPEC'S OWN CODE SNIPPET (found via the real data,
# not a synthetic test):
#   Josh's spec gives the header-standardization line literally as
#   `tolower(gsub("[[:punct:]]", "", names(temp)))` - but `[[:punct:]]`
#   matches PUNCTUATION only, not whitespace. Applied literally, the real
#   header "Species Manual ID" standardizes to "species manual id" (spaces
#   survive) - which would NEVER match the spec's own expected header
#   "$speciesmanualid" (no spaces), and "WA|Kaleidoscope|Auto ID" would
#   standardize to "wakaleidoscope auto id", not "$wakaleidoscopeautoid".
#   Every real file would therefore get skipped as "mismatched headers",
#   permanently, which cannot be the intent. Fixed by stripping ALL
#   non-alphanumeric characters (punctuation AND whitespace):
#   `tolower(gsub("[^[:alnum:]]", "", names(tmp)))` - verified this exact
#   substitution turns "Species Manual ID" -> "speciesmanualid" and
#   "WA|Kaleidoscope|Auto ID" -> "wakaleidoscopeautoid", both real headers
#   in FinalVetted.csv, matching the spec's expected list exactly. This is
#   the single most important correction in this build - without it, the
#   function could never successfully merge a single real file.
#
# WHAT THE REAL DATA CONFIRMED ABOUT THE OTHER AMBIGUOUS PARTS OF THE SPEC:
#   - $lat (after standardization) really does hold BOTH coordinates in one
#     field, exactly as the spec says - real format confirmed as
#     "<lat> <lon>" (a single space between two decimal-degree numbers,
#     longitude negative for the western hemisphere, e.g.
#     "43.59303 -71.73640"). Split on whitespace into $lat/$lon.
#   - $filename really does follow "<ARU NAME>_<YYYYMMDD>_<HHMMSS>_<junk>"
#     exactly as described (e.g. "CLERK_20260627_001635_000.wav") - every
#     one of the 6,487 real rows matches this pattern. The spec calls the
#     time part "HHMMDD" - read as a typo for "HHMMSS" (military time, per
#     the spec's own words "military time"; there is no sensible reading
#     where a day-of-month belongs after an 8-digit YYYYMMDD date).
#   - Confirmed real header "$Serial" (capitalized in the raw file) does
#     standardize down to "serial" as expected once case-folded.
#   - The raw file also has a "MonitoringNight_1" column (a near-duplicate
#     of "MonitoringNight") and many acoustic-detail columns (Fc mean, Dur
#     mean, calls/sec, etc.) not in the spec's expected-header list - these
#     are correctly dropped by the "trim tmp to only have expected headers"
#     step, not an error or a sign anything is wrong.
#
# STEPS / ASSUMPTIONS (spec was silent or inconsistent on a few of these -
# flagging per project convention):
#   1. `duplicates.remove` is used in the Steps section ("if
#      duplicates.remove = TRUE then remove any duplicated rows") but is
#      MISSING from the Optional Inputs list entirely - same "spec-template
#      leftover" issue flagged repeatedly elsewhere in this project. Added
#      it as a real parameter, default TRUE (matching every other batz
#      dedup-flag default) - please confirm.
#   2. `load.pattern` matching is done CASE-INSENSITIVELY (see above - the
#      real test file itself only matches this way).
#   3. A file matching `load.pattern` that fails to read at all (corrupt/
#      unreadable) is skipped with `$reason = "could not read file"` - not
#      literally specified, added as a safety fallback consistent with
#      other batz functions' "file fails to load" handling.
#   4. Header-standardization order matches the spec's literal Steps order:
#      check for missing expected headers FIRST (skip + log if any are
#      missing), THEN check for zero rows (skip + log if empty) - a file
#      missing headers is never also reported as "no records" even if it
#      happens to have 0 rows.
#   5. "$headers.missing" in the log is a single comma-joined string of the
#      missing (standardized) header names, e.g. "speciesmanualid"; for the
#      "no records" reason it's the literal string "none", exactly as
#      given in the spec.
#   6. `data frame "tmp"` in the spec's prose Steps is called `temp` once
#      later in the same section (`names(temp)`) - read as the same
#      variable, just an inconsistent name in Josh's own notes; used `tmp`
#      throughout for consistency with the rest of the Steps text.
#   7. `$temp` (spec: "make a copy of $filename called $temp... extract
#      into separate headers") is treated as a purely intermediate/working
#      value, not a column that survives into the final `vetted.merged`
#      output - only the three EXTRACTED columns ($aru.name, $date, $time)
#      are added to the output, per the spec's own worked example, which
#      only ever refers to the three extracted pieces afterward.
#   8. $date is kept as an 8-character string in "YYYYMMDD" form (not
#      converted to an R Date) and $time as a 6-character string in
#      "HHMMSS" form (not converted to a time object) - the spec describes
#      them as extracted TEXT fields, not calculated values, and doesn't
#      ask for any date arithmetic later in this function.
#   9. Same auto-assign-into-caller's-environment pattern already used in
#      `batz.arumeta_merge.format`/`batz.datawrangler_load.files` (and
#      implied by the spec's own wording, "create a new object in R called
#      vetted.merged_log.file") - a bare call with no assignment creates
#      `vetted.merged` (and `vetted.merged_log.file`, if `log.file = TRUE`)
#      directly in the calling environment; the function also returns
#      (invisibly) a list of both for anyone who prefers `result$...`
#      access.
#  10. The expected-header list is compared using EXACT (not substring)
#      matching against the standardized real headers.
# =============================================================================

setwd("/home/claude/vettedacoustics_work")

normalize.header <- function(x) tolower(gsub("[^[:alnum:]]", "", x))

expected.headers <- c("filename", "monitoringnight", "speciesmanualid",
                       "wakaleidoscopeautoid", "sppaccp", "lat", "serial")

cat("=== sanity check: does the real FinalVetted.csv standardize to include\n",
    "all 7 expected headers? ===\n", sep = "")
raw.names <- names(read.csv("FinalVetted.csv", check.names = FALSE, nrows = 1))
std.names <- normalize.header(raw.names)
print(data.frame(raw = raw.names, standardized = std.names))
cat("All expected headers present:", all(expected.headers %in% std.names), "\n")

# -----------------------------------------------------------------------------
# batz.vettedacoustics_merge.format(dir.load = getwd(), load.pattern =
#   c("*vetted.csv"), dir.sub = FALSE, duplicates.remove = TRUE, log.file = FALSE)
# -----------------------------------------------------------------------------
batz.vettedacoustics_merge.format <- function(dir.load = getwd(),
                                               load.pattern = c("*vetted.csv"),
                                               dir.sub = FALSE,
                                               duplicates.remove = TRUE,
                                               log.file = FALSE) {

  expected.headers <- c("filename", "monitoringnight", "speciesmanualid",
                         "wakaleidoscopeautoid", "sppaccp", "lat", "serial")

  regex.pattern <- paste(utils::glob2rx(load.pattern), collapse = "|")
  files <- list.files(dir.load, pattern = regex.pattern, recursive = dir.sub,
                       full.names = TRUE, ignore.case = TRUE)

  vetted.merged <- data.frame()
  log.rows <- list()

  add.log <- function(filepath, reason, headers.missing) {
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filepath = filepath, reason = reason, headers.missing = headers.missing,
      stringsAsFactors = FALSE
    )
  }

  for (f in files) {

    tmp <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) {
      add.log(f, "could not read file", "none")
      next
    }

    names(tmp) <- normalize.header(names(tmp))

    missing.headers <- setdiff(expected.headers, names(tmp))
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", "))
      next
    }

    if (nrow(tmp) == 0) {
      add.log(f, "no records", "none")
      next
    }

    tmp <- tmp[, expected.headers, drop = FALSE]
    vetted.merged <- rbind(vetted.merged, tmp)
  }

  if (duplicates.remove && nrow(vetted.merged) > 0) {
    vetted.merged <- vetted.merged[!duplicated(vetted.merged), ]
  }

  if (nrow(vetted.merged) > 0) {

    # $lat holds "<lat> <lon>" - split into two numeric columns
    latlon <- strsplit(trimws(vetted.merged$lat), "\\s+")
    vetted.merged$lat <- vapply(latlon, function(x) as.numeric(x[1]), numeric(1))
    vetted.merged$lon <- vapply(latlon, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA_real_, numeric(1))

    # $filename = "<ARU>_<YYYYMMDD>_<HHMMSS>_<junk>" -> $aru.name/$date/$time
    m <- regmatches(vetted.merged$filename,
                     regexec("^([^_]+)_(\\d{8})_(\\d{6})_", vetted.merged$filename))
    vetted.merged$aru.name <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
    vetted.merged$date     <- vapply(m, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
    vetted.merged$time     <- vapply(m, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1))
  }

  rownames(vetted.merged) <- NULL

  result <- list(vetted.merged = vetted.merged)

  if (log.file) {
    vetted.merged_log.file <- if (length(log.rows) > 0) do.call(rbind, log.rows) else
      data.frame(filepath = character(0), reason = character(0), headers.missing = character(0))
    rownames(vetted.merged_log.file) <- NULL
    result$vetted.merged_log.file <- vetted.merged_log.file
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())

  invisible(result)
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("\n=== basic merge, dir.sub = FALSE (misses the subfolder file) ===\n")
r1 <- batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = FALSE, log.file = TRUE)
cat("vetted.merged dim:", dim(vetted.merged), "\n")
print(head(vetted.merged, 3))
cat("\nvetted.merged_log.file:\n")
print(vetted.merged_log.file)

cat("\n=== dir.sub = TRUE (picks up sub1/site2_Vetted.csv too, dedups the 3 shared rows) ===\n")
r2 <- batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = TRUE, log.file = TRUE, duplicates.remove = TRUE)
cat("vetted.merged dim (should be 50 + 50 unique from site2, since 3 were dupes of site1):", dim(vetted.merged), "\n")
cat("\nvetted.merged_log.file:\n")
print(vetted.merged_log.file)

cat("\n=== duplicates.remove = FALSE (keeps all rows, including the 3 dupes) ===\n")
batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = TRUE, duplicates.remove = FALSE)
cat("vetted.merged dim (should be 50 + 53 = 103):", dim(vetted.merged), "\n")

cat("\n=== extracted columns spot check ===\n")
batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = TRUE)
print(head(vetted.merged[, c("filename", "aru.name", "date", "time", "lat", "lon")], 5))

cat("\n=== bare call with no assignment auto-creates vetted.merged in this environment ===\n")
rm(list = c("vetted.merged"))
batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = TRUE)
cat("exists('vetted.merged') after bare call:", exists("vetted.merged"), " nrow:", nrow(vetted.merged), "\n")

cat("\n=== log.file = FALSE (default) - no vetted.merged_log.file object created ===\n")
rm(list = "vetted.merged_log.file")
batz.vettedacoustics_merge.format(dir.load = "testdata", dir.sub = TRUE, log.file = FALSE)
cat("exists('vetted.merged_log.file') after log.file = FALSE:", exists("vetted.merged_log.file"), "\n")

cat("\n=== load.pattern only matches the real full-size file (sanity check against\n",
    "the un-split original, case-insensitive match on 'FinalVetted.csv') ===\n", sep = "")
batz.vettedacoustics_merge.format(dir.load = ".", load.pattern = "*vetted.csv", dir.sub = FALSE, log.file = TRUE)
cat("matched + merged rows from FinalVetted.csv:", nrow(vetted.merged), "(should be 6487)\n")
print(vetted.merged_log.file)

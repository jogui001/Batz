# =============================================================================
# batz.merge_sm4.logfile.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.merge_sm4.logfile() - tested against real test
# data before being wrapped into the final function
# (batz.merge_sm4.logfile.R).
#
# Purpose (per spec): merge all SM4 ARU activity-log summary files
# ("*_A_Summary.txt"/"*_B_Summary.txt") in a directory (and, optionally, its
# subdirectories) into one master summary data frame in a standardized
# format (ARU name extracted, date normalized, lat/long converted to signed
# decimal degrees).
#
# NAME: Josh's given name was "batz.sm4logfile_merge&format" - normalized to
# "batz.merge_sm4.logfile" ("&" isn't one of the two separator
# characters Josh's own convention defines - "_" between family/action, "."
# within the action - same normalization already applied to
# "batz.arumeta.merge&format" -> "batz.arumeta_merge.format" earlier in this
# project). Told Josh about the rename, per standing project convention.
#
# =============================================================================
# FLAGGED SPEC ISSUES (original 2026-09-14 build):
# =============================================================================
#  1. `duplicates.remove` is used in the Steps section ("if duplicates.remove
#     = TRUE then remove any duplicated rows") but is NOT listed in
#     "Optional inputs" at all. Added it as a real parameter, default TRUE
#     (matching every other batz function's dedup-flag default) - please
#     confirm TRUE is the right default here too.
#  2. Returned object names `sm4logs.merged`/`sm4logs.merged_log.file` mix
#     "_" into an otherwise dot-separated name - this one is NOT a naming
#     leak from a different family this time (this IS the sm4logfile family
#     function), so used them exactly as given rather than renaming.
#  3. DATE format: every real file seen so far uses "YYYY-Mon-DD" (e.g.
#     "2026-Jun-26"), not "Mon-DD-YYYY" or any other order. Implemented the
#     "three-letter month" conversion narrowly for this exact real format.
#  4. LAT/LON in the real data are already plain decimal degrees (not
#     degrees-minutes-seconds), so "convert into decimal degrees" is
#     implemented as just applying the correct sign from the NS/EW
#     hemisphere letter - not a DMS parse.
#  5. Header matching for the "missing headers" check is exact-string,
#     case-insensitive, after trimming - extra/unexpected columns beyond the
#     given 11 wouldn't fail a file (only a MISSING expected column does).
#
#  6. **Standardized (2026-09-14, per Josh - project-wide header
#     standardization preference) - real, documented output-schema change.**
#     The 11-column expected-header list is a literal, uninvented copy of
#     the SM4 device's own real export column text (not a batz-invented
#     shorthand), so both it and every raw file's own headers are now run
#     through standardize.headers(). Old -> new: DATE -> date, TIME -> time,
#     LAT -> lat, NS -> ns, LON -> lon, EW -> ew, POWER(V) -> power_v,
#     TEMP(C) -> temp_c, #FILES -> files, #SCRUBBED -> scrubbed,
#     MIC0 TYPE -> mic0_type. Does NOT affect aru.name, X, or Y.
#
# =============================================================================
# FOLLOW-UP, 2026-09-22, per Josh - log.file COMPLETELY REDESIGNED:
# =============================================================================
#  Previously, sm4logs.merged_log.file only had a row for a SKIPPED file
#  ($filepath/$reason only, "mismatched headers (missing: ...)"/"no
#  records"/"could not read file"). Josh's new spec: build a row for EVERY
#  file examined (success or failure), with columns $aru.name, $file.name,
#  $date.start, $date.end, $date.unique, $date.range, $records,
#  $load.status ("Success"/"Failure"), $reason, $filepath.
#
#  - $aru.name/$file.name/$filepath: always set, regardless of success.
#  - Success (all 11 headers present AND >=1 data row): $date is converted
#    to YYYY-MM-DD (same convert.date() step sm4logs.merged itself uses),
#    then $date.start/$date.end (earliest/latest date IN THAT ONE FILE,
#    not across all files), $date.unique (count of distinct $date values in
#    that file), $date.range (calendar-day span from $date.start to
#    $date.end inclusive - lets you compare against $date.unique to spot
#    gap days with no records), and $records (row count of that one file)
#    are all filled in; $load.status = "Success"; $reason = Josh's exact
#    literal text, "All headers present and observation in file".
#  - Failure (headers missing and/or no data rows): $load.status =
#    "Failure"; $reason = "no data" (headers fine, zero rows), "These
#    headers are missing: <list>" (has data, headers missing), or "no data
#    and These headers are missing: <list>" (both) - Josh's exact literal
#    text/format in all three cases. Per Josh's explicit "all other headers
#    = NA": $date.start/$date.end/$date.unique/$date.range/$records are all
#    NA on a Failure row.
#  - "could not read file" (a file read.csv() itself errored on) is kept as
#    a fourth Failure reason, outside Josh's given list - an edge case the
#    OLD log.file scheme already handled and this redesign preserves for
#    parity, since the new spec doesn't say what should happen to a file
#    that can't be read at all.
#  - FLAGGED, not in Josh's spec: a $date value convert.date() doesn't
#    recognize (see flagged issue 3 above) is still counted in
#    $date.unique but excluded from the $date.start/$date.end/$date.range
#    calculation via an ISO-format ("^\d{4}-\d{2}-\d{2}$") validity check,
#    so one malformed date can't corrupt the file's chronological summary.
#    No real file has been seen to trigger this.
#  - Test data note: this session's device bridge does not have the "3 All
#    test data" folder connected (only "reference database files" and the
#    GitHub\Batz repo are connected this round), so - rather than request a
#    new folder grant just to re-verify the UNCHANGED base merge/convert
#    logic - this round's tests use fresh SYNTHETIC fixtures exercising
#    every log.file branch (Success x2 with different date-range/unique
#    shapes, all three Failure reason texts, and a dir.sub=TRUE subfolder
#    file). The base per-row merge/date/coordinate logic itself is
#    unchanged from the 2026-09-14 build, which WAS verified against real
#    data at the time (see git history / earlier revisions of this file).
# =============================================================================

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

pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

## header standardization (per Josh, 2026-09-14 project preference).
expected.headers <- standardize.headers(c("DATE", "TIME", "LAT", "NS", "LON", "EW",
                                           "POWER(V)", "TEMP(C)", "#FILES", "#SCRUBBED", "MIC0 TYPE"))

month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                   jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

# -----------------------------------------------------------------------------
# helper: "YYYY-Mon-DD" (3-letter month) -> "YYYY-MM-DD"; anything else is
# left untouched (see flagged issue 3).
# -----------------------------------------------------------------------------
convert.date <- function(x) {
  m <- regmatches(x, regexpr("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x))
  out <- x
  has.match <- nzchar(m)
  if (any(has.match)) {
    parts <- regmatches(x[has.match], regexec("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x[has.match]))
    converted <- vapply(parts, function(p) {
      yr <- p[2]; mon <- tolower(p[3]); day <- p[4]
      mm <- month.lookup[mon]
      if (is.na(mm)) return(NA_character_)
      paste(yr, mm, day, sep = "-")
    }, character(1))
    out[has.match] <- ifelse(is.na(converted), x[has.match], converted)
  }
  out
}

# -----------------------------------------------------------------------------
# helper: build one sm4logs.merged_log.file row (per Josh, 2026-09-22
# log.file redesign - see FOLLOW-UP note above).
# -----------------------------------------------------------------------------
make.log.row <- function(aru.name, file.name, filepath, load.status, reason,
                          date.start = NA_character_, date.end = NA_character_,
                          date.unique = NA_integer_, date.range = NA_integer_,
                          records = NA_integer_) {
  data.frame(aru.name = aru.name, file.name = file.name,
             date.start = date.start, date.end = date.end,
             date.unique = date.unique, date.range = date.range,
             records = records, load.status = load.status, reason = reason,
             filepath = filepath, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# helper: process one summary file -> validated/converted data frame (or
# NULL) + a full log row (per Josh, 2026-09-22 log.file redesign).
# -----------------------------------------------------------------------------
process.one.file <- function(f) {
  base.name <- basename(f)
  file.aru.name <- sub("_.*$", "", base.name)

  raw <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(list(data = NULL,
                 log = make.log.row(file.aru.name, base.name, f, "Failure", "could not read file")))
  }

  ## header standardization (per Josh, 2026-09-14 project preference).
  names(raw) <- standardize.headers(names(raw))
  present <- expected.headers %in% names(raw)
  headers.missing <- !all(present)
  no.data <- nrow(raw) == 0

  if (headers.missing || no.data) {
    missing.list <- paste(expected.headers[!present], collapse = ", ")
    reason <- if (headers.missing && no.data) {
      paste0("no data and These headers are missing: ", missing.list)
    } else if (headers.missing) {
      paste0("These headers are missing: ", missing.list)
    } else {
      "no data"
    }
    return(list(data = NULL,
                 log = make.log.row(file.aru.name, base.name, f, "Failure", reason)))
  }

  tmp <- raw[expected.headers]
  for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

  tmp$aru.name <- file.aru.name
  tmp$date <- convert.date(tmp$date)

  ns <- tolower(trimws(tmp$ns))
  ew <- tolower(trimws(tmp$ew))
  tmp$Y <- ifelse(ns == "s", -as.numeric(tmp$lat), as.numeric(tmp$lat))
  tmp$X <- ifelse(ew == "w", -as.numeric(tmp$lon), as.numeric(tmp$lon))

  tmp <- tmp[c("aru.name", expected.headers, "X", "Y")]

  ## per-file date summary (per Josh, 2026-09-22 log.file redesign).
  date.vals <- tmp$date
  date.unique.n <- length(unique(date.vals))
  iso.ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date.vals) &
    !is.na(suppressWarnings(as.Date(date.vals, format = "%Y-%m-%d")))
  if (any(iso.ok)) {
    date.start.val <- min(date.vals[iso.ok])
    date.end.val   <- max(date.vals[iso.ok])
    date.range.val <- as.integer(as.Date(date.end.val) - as.Date(date.start.val)) + 1L
  } else {
    date.start.val <- NA_character_
    date.end.val   <- NA_character_
    date.range.val <- NA_integer_
  }

  list(data = tmp,
       log = make.log.row(file.aru.name, base.name, f, "Success",
                           "All headers present and observation in file",
                           date.start = date.start.val, date.end = date.end.val,
                           date.unique = date.unique.n, date.range = date.range.val,
                           records = nrow(tmp)))
}

# -----------------------------------------------------------------------------
# batz.merge_sm4.logfile(dir.load, dir.sub, load.pattern,
#                               duplicates.remove, log.file)
# -----------------------------------------------------------------------------
batz.merge_sm4.logfile <- function(dir.load = getwd(),
                                          dir.sub           = FALSE,
                                          load.pattern      = c("*_A_Summary.txt", "*_B_Summary.txt"),
                                          duplicates.remove = TRUE,
                                          log.file          = FALSE) {

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  sm4logs.merged <- NULL
  log.rows <- list()

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f)
      log.rows[[length(log.rows) + 1]] <- r$log
      if (is.null(r$data)) {
        cat("  [skipped] ", f, " - ", r$log$reason, "\n", sep = "")
      } else {
        cat("  loaded ", f, " (", nrow(r$data), " rows)\n", sep = "")
        sm4logs.merged <- if (is.null(sm4logs.merged)) r$data else rbind(sm4logs.merged, r$data)
      }
    }
  }

  sm4logs.merged_log.file <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    data.frame(aru.name = character(0), file.name = character(0),
               date.start = character(0), date.end = character(0),
               date.unique = integer(0), date.range = integer(0),
               records = integer(0), load.status = character(0),
               reason = character(0), filepath = character(0),
               stringsAsFactors = FALSE)
  }

  if (is.null(sm4logs.merged)) {
    sm4logs.merged <- data.frame()
    cat("\nNo files were successfully loaded - sm4logs.merged is empty.\n")
  } else if (duplicates.remove) {
    dup.mask <- duplicated(sm4logs.merged)
    n.dup <- sum(dup.mask)
    if (n.dup > 0) {
      cat("\n", n.dup, " duplicate row(s) removed from sm4logs.merged.\n", sep = "")
      sm4logs.merged <- sm4logs.merged[!dup.mask, ]
    }
    rownames(sm4logs.merged) <- NULL
  }

  result <- list(sm4logs.merged = sm4logs.merged)
  if (log.file) result$sm4logs.merged_log.file <- sm4logs.merged_log.file

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

# =============================================================================
# tests
# =============================================================================
# Synthetic fixtures (see FOLLOW-UP note above for why synthetic this round):
#   sm4_test/AYERS_A_Summary.txt      - 3 rows, 2 unique dates, Jun01+Jun03
#                                        (date.range = 3, a 1-day gap)
#   sm4_test/CEMETERY_A_Summary.txt   - 1 row, 1 date (date.range = 1)
#   sm4_test/BADHEADER_A_Summary.txt  - missing TEMP(C), has 1 data row
#   sm4_test/EMPTY_A_Summary.txt      - all headers present, 0 data rows
#   sm4_test/BOTHBAD_A_Summary.txt    - missing TEMP(C) AND 0 data rows
#   sm4_test/subdir/SUBSITE_A_Summary.txt - 1 row (dir.sub = TRUE test)

test.dir <- "/home/claude/refdb/sm4_test"

cat("=== log.file = TRUE, dir.sub = TRUE (all 6 fixtures) ===\n")
res1 <- batz.merge_sm4.logfile(test.dir, dir.sub = TRUE, log.file = TRUE)
cat("\ndim sm4logs.merged:", paste(dim(sm4logs.merged), collapse = " x "), "\n")
print(sm4logs.merged[, c("aru.name", "date", "time", "lat", "lon")])
cat("\nsm4logs.merged_log.file:\n")
print(sm4logs.merged_log.file)

stopifnot(nrow(sm4logs.merged_log.file) == 6)
stopifnot(nrow(sm4logs.merged) == 5)

get.row <- function(fname) sm4logs.merged_log.file[sm4logs.merged_log.file$file.name == fname, ]

r <- get.row("AYERS_A_Summary.txt")
stopifnot(r$load.status == "Success", r$records == 3, r$date.unique == 2,
          r$date.start == "2026-06-01", r$date.end == "2026-06-03", r$date.range == 3)
cat("[PASS] AYERS Success row: records=3, date.unique=2, date.range=3\n")

r <- get.row("CEMETERY_A_Summary.txt")
stopifnot(r$load.status == "Success", r$records == 1, r$date.unique == 1, r$date.range == 1)
cat("[PASS] CEMETERY Success row: records=1, date.unique=1, date.range=1\n")

r <- get.row("BADHEADER_A_Summary.txt")
stopifnot(r$load.status == "Failure", r$reason == "These headers are missing: temp_c", is.na(r$records))
cat("[PASS] BADHEADER Failure row:", r$reason, "\n")

r <- get.row("EMPTY_A_Summary.txt")
stopifnot(r$load.status == "Failure", r$reason == "no data", is.na(r$date.unique))
cat("[PASS] EMPTY Failure row:", r$reason, "\n")

r <- get.row("BOTHBAD_A_Summary.txt")
stopifnot(r$load.status == "Failure",
          r$reason == "no data and These headers are missing: temp_c")
cat("[PASS] BOTHBAD Failure row:", r$reason, "\n")

r <- get.row("SUBSITE_A_Summary.txt")
stopifnot(r$load.status == "Success", r$aru.name == "SUBSITE")
cat("[PASS] SUBSITE (dir.sub = TRUE) Success row\n")

stopifnot(all(!is.na(sm4logs.merged_log.file$aru.name)),
          all(!is.na(sm4logs.merged_log.file$file.name)),
          all(!is.na(sm4logs.merged_log.file$filepath)))
cat("[PASS] aru.name/file.name/filepath always populated, even on Failure\n")

cat("\n=== log.file = FALSE (sm4logs.merged_log.file should not be created) ===\n")
if (exists("sm4logs.merged_log.file", envir = .GlobalEnv, inherits = FALSE)) {
  rm(sm4logs.merged_log.file, envir = .GlobalEnv)
}
res2 <- batz.merge_sm4.logfile(test.dir, dir.sub = TRUE, log.file = FALSE)
stopifnot(is.null(res2$sm4logs.merged_log.file))
stopifnot(!exists("sm4logs.merged_log.file", envir = .GlobalEnv, inherits = FALSE))
cat("[PASS] log.file = FALSE correctly omits sm4logs.merged_log.file\n")

cat("\n=== empty directory ===\n")
empty.dir <- tempfile(); dir.create(empty.dir)
res3 <- batz.merge_sm4.logfile(empty.dir, log.file = TRUE)
stopifnot(nrow(sm4logs.merged) == 0, nrow(sm4logs.merged_log.file) == 0)
cat("[PASS] empty directory: 0 rows, 0 log rows, no error\n")

cat("\nALL TESTS PASSED\n")

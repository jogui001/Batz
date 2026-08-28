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
# FLAGGED SPEC ISSUES:
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
#     Flagging only the minor style inconsistency in case you'd rather have
#     it as `sm4logs.merged.log.file` for consistency with your own
#     `$collum.name` dot convention - not changed without asking.
#  3. "Test data location" names the "4 Current  test data" folder, but that
#     folder has no `*_A_Summary.txt`/`*_B_Summary.txt` files in it - found
#     them instead in the sibling "3 All test data" folder (which also has
#     good real edge cases: a byte-identical duplicate file in a subfolder
#     for dir.sub testing, and a much smaller/larger A/B pair for a second
#     real ARU). Used "3 All test data" for testing; flagging in case "4
#     Current test data" was supposed to have these and doesn't yet.
#  4. No real file in the test data exercises "mismatched headers" or "no
#     records" - every real file's header matched the given 11-column list
#     exactly. Built two small SYNTHETIC files (not from Josh's real data,
#     kept in their own "synthetic/" subfolder) to exercise those two
#     branches: BADHEADER_A_Summary.txt (missing the TEMP(C) column) and
#     EMPTY_A_Summary.txt (header row only, zero data rows).
#  5. DATE format: every real file uses "YYYY-Mon-DD" (e.g. "2026-Jun-26"),
#     not "Mon-DD-YYYY" or any other order. Implemented the "three-letter
#     month" conversion narrowly for this exact real format (regex-swap the
#     3-letter month abbreviation for its zero-padded number, using a fixed
#     locale-independent lookup table rather than R's locale-dependent
#     %b/strptime) - if a file ever uses a different date layout, this will
#     need to be revisited; flagging since the spec was silent on the exact
#     input layout beyond "uses three letters for the month".
#  6. LAT/LON in the real data are already plain decimal degrees (not
#     degrees-minutes-seconds), so "convert into decimal degrees" is
#     implemented as just applying the correct sign from the NS/EW
#     hemisphere letter (n/N -> positive, s/S -> negative for $Y; e/E ->
#     positive, w/W -> negative for $X) - not a DMS parse. All real NS/EW
#     values seen are lowercase ("n"/"w"); matching is case-insensitive
#     regardless.
#  7. Real MIC0 TYPE values have INCONSISTENT trailing whitespace within the
#     same file (e.g. "U2" on some rows, "U2 " on others) - trimmed with
#     trimws() on read so this doesn't create spurious near-duplicate values
#     or block exact-duplicate-row detection.
#  8. Header matching for the "missing headers" check is exact-string,
#     case-insensitive, after trimming - extra/unexpected columns beyond the
#     given 11 wouldn't fail a file (only a MISSING expected column does);
#     no real file exercises this either way, since the "MIC0 TYPE " header
#     field's trailing space is already stripped by read.csv itself before
#     comparison.
#
# Test data (staged from "3 All test data", NOT modifying Josh's real
# files):
#   /home/claude/sm4_work/AYERS_A_Summary.txt          (real, 3459 rows)
#   /home/claude/sm4_work/CEMETERY_A_Summary.txt        (real, 3460 rows)
#   /home/claude/sm4_work/CLERK_B_Summary.txt           (real, 3460 rows)
#   /home/claude/sm4_work/SUGARHILL_A_Summary.txt       (real, 3460 rows)
#   /home/claude/sm4_work/Second level used to test subfolders/
#     CEMETERY_A_Summary.txt   (real - byte-identical dup of the top-level
#     CEMETERY_A_Summary.txt, for dir.sub=TRUE duplicate-row testing)
#   /home/claude/sm4_work/UMaineWTG_07.17.2026_07.31.2026/
#     WTG-GOM101_A_Summary.txt (real, 198 rows) and
#     WTG-GOM101_B_Summary.txt (real, 19813 rows)
#   /home/claude/sm4_work/synthetic/BADHEADER_A_Summary.txt (SYNTHETIC - missing header)
#   /home/claude/sm4_work/synthetic/EMPTY_A_Summary.txt     (SYNTHETIC - no data rows)
# =============================================================================

pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

expected.headers <- c("DATE", "TIME", "LAT", "NS", "LON", "EW",
                       "POWER(V)", "TEMP(C)", "#FILES", "#SCRUBBED", "MIC0 TYPE")

month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                   jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

# -----------------------------------------------------------------------------
# helper: "YYYY-Mon-DD" (3-letter month) -> "YYYY-MM-DD"; anything else is
# left untouched (see flagged issue 5).
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
# helper: process one summary file -> validated/converted data frame, or
# NULL + a skip reason.
# -----------------------------------------------------------------------------
process.one.file <- function(f) {
  raw <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(list(data = NULL, reason = "could not read file"))

  names(raw) <- trimws(names(raw))
  present <- expected.headers %in% names(raw)
  if (!all(present)) {
    return(list(data = NULL, reason = paste0("mismatched headers (missing: ",
                                              paste(expected.headers[!present], collapse = ", "), ")")))
  }

  if (nrow(raw) == 0) {
    return(list(data = NULL, reason = "no records"))
  }

  tmp <- raw[expected.headers]
  for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

  base.name <- basename(f)
  tmp$aru.name <- sub("_.*$", "", base.name)

  tmp$DATE <- convert.date(tmp$DATE)

  ns <- tolower(trimws(tmp$NS))
  ew <- tolower(trimws(tmp$EW))
  tmp$Y <- ifelse(ns == "s", -as.numeric(tmp$LAT), as.numeric(tmp$LAT))
  tmp$X <- ifelse(ew == "w", -as.numeric(tmp$LON), as.numeric(tmp$LON))

  # aru.name first, then the 11 standardized raw columns, then derived X/Y
  tmp <- tmp[c("aru.name", expected.headers, "X", "Y")]

  list(data = tmp, reason = NA_character_)
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
      if (is.null(r$data)) {
        cat("  [skipped] ", f, " - ", r$reason, "\n", sep = "")
        log.rows[[length(log.rows) + 1]] <- data.frame(filepath = f, reason = r$reason, stringsAsFactors = FALSE)
      } else {
        cat("  loaded ", f, " (", nrow(r$data), " rows)\n", sep = "")
        sm4logs.merged <- if (is.null(sm4logs.merged)) r$data else rbind(sm4logs.merged, r$data)
      }
    }
  }

  sm4logs.merged_log.file <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    data.frame(filepath = character(0), reason = character(0), stringsAsFactors = FALSE)
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

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("=== dir.sub = FALSE, log.file = TRUE (real data, top-level only) ===\n")
res1 <- batz.merge_sm4.logfile("/home/claude/sm4_work", dir.sub = FALSE, log.file = TRUE)
cat("\ndim sm4logs.merged:", paste(dim(sm4logs.merged), collapse = " x "), "\n")
cat("aru.name values:", paste(unique(sm4logs.merged$aru.name), collapse = ", "), "\n")
print(head(sm4logs.merged[, c("aru.name", "DATE", "TIME", "LAT", "NS", "Y", "LON", "EW", "X")], 3))
print(sm4logs.merged_log.file)

cat("\n\n=== dir.sub = TRUE (should also pick up subfolders, incl. the real\n",
    " duplicate file and the two synthetic edge-case files) ===\n", sep = "")
res2 <- batz.merge_sm4.logfile("/home/claude/sm4_work", dir.sub = TRUE, log.file = TRUE)
cat("\ndim sm4logs.merged:", paste(dim(sm4logs.merged), collapse = " x "), "\n")
print(sm4logs.merged_log.file)

cat("\n\n=== duplicates.remove = FALSE (dup CEMETERY rows should survive) ===\n")
res3 <- batz.merge_sm4.logfile("/home/claude/sm4_work", dir.sub = TRUE, duplicates.remove = FALSE)
cat("dim with duplicates.remove = FALSE:", paste(dim(sm4logs.merged), collapse = " x "), "\n")

cat("\n\n=== empty directory ===\n")
empty.dir <- tempfile(); dir.create(empty.dir)
res4 <- batz.merge_sm4.logfile(empty.dir, log.file = TRUE)
cat("dim:", paste(dim(sm4logs.merged), collapse = " x "), " log rows:", nrow(sm4logs.merged_log.file), "\n")

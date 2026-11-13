# =============================================================================
# batz.merge_sm4.logfiles.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.merge_sm4.logfiles() - tested against real test
# data before being wrapped into the final function
# (batz.merge_sm4.logfiles.R).
#
# Purpose (per spec): merge all SM4 ARU activity-log summary files
# ("*_A_Summary*.txt"/"*_B_Summary*.txt") in a directory (and, optionally,
# its subdirectories) into one master summary data frame in a standardized
# format (ARU name extracted, date normalized, lat/long converted to signed
# decimal degrees).
#
# NAME: Josh's given name was "batz.sm4logfile_merge&format" - normalized to
# "batz.merge_sm4.logfiles" ("&" isn't one of the two separator
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
#  2. Returned object names `sm4logs.merged`/`log.file_sm4` mix
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
#  Previously, log.file_sm4 only had a row for a SKIPPED file
#  ($filepath/$reason only, "mismatched headers (missing: ...)"/"no
#  records"/"could not read file"). Josh's new spec: build a row for EVERY
#  file examined (success or failure), with columns $aru.name, $filename,
#  $date.start, $date.end, $date.unique, $date.range, $records,
#  $load.status ("Success"/"Failure"), $reason, $filepath. (Renamed from
#  $file.name to $filename on 2026-09-27 - see the RENAMED note below; this
#  header comment already uses the current name.)
#
#  - $aru.name/$filename/$filepath: always set, regardless of success.
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
# RENAMED, 2026-09-27, per Josh's reference-workbook "Change.to" column:
# =============================================================================
#  In log.file_sm4 (log.file = TRUE): $file.name -> $filename.
#  In sm4logs.merged (the merged master data frame): the standardized input
#  column $lon -> $longitude. The expected.headers list used to VALIDATE a
#  raw file's headers is unchanged ("lon" is still what the SM4 device's own
#  real "LON" export text standardizes to via standardize.headers(), and
#  that real device text is not changing) - a separate output.headers
#  vector renames just the "lon" -> "longitude" spelling once a file has
#  already been matched/subset, so only the OUTPUT-facing column name
#  changes, not the input-matching logic.
# =============================================================================
# FOLLOW-UP, 2026-09-29, per Josh - wider default load.pattern:
# =============================================================================
#  load.pattern default: c("*_A_Summary.txt", "*_B_Summary.txt") ->
#  c("*_A_Summary*.txt", "*_B_Summary*.txt"). The extra "*" lets any text
#  sit between "_Summary" and ".txt", so e.g. "WTG-GOM102_A_Summary -
#  Copy.txt" (a real file in "4 Current  test data") and
#  "AYERS_B_Summary_2026.txt" are now picked up. Still case-insensitive;
#  ".csv", "_C_Summary" and ".txt.bak" files are still ignored. ARU name
#  parsing is unchanged (everything before the first "_"). An exact
#  " - Copy" duplicate adds identical rows - duplicates.remove = TRUE drops
#  them from sm4logs.merged, but both files still get a log row. See TEST
#  "load.pattern" at the bottom of this script.
# =============================================================================

# =============================================================================
# FOLLOW-UP, 2026-09-29, per Josh - SM4/SM5 detection + $version:
# =============================================================================
#  Headers are now checked to tell SM4 from SM5 (firmware 1.5 or 1.6) files.
#  Only SM4 files merge into sm4logs.merged; SM5 files are logged as
#  "Failure" with a reason pointing to batz.merge_sm5.logfiles() /
#  batz.merge_sm.logfiles(). log.file_sm4 gains $version
#  ("SM4"/"SM5.1.5"/"SM5.1.6"/"unknown") after $filename. Reading,
#  checking and converting now live in the shared engine
#  batz.util_sm.logfile.R (inlined below). sm4logs.merged is unchanged.
#  RENAMED, same day, per Josh: the log object sm4logs.merged_log.file is
#  now log.file_sm4 (SM5: log.file_sm5; all-units: log.file_sm), so no
#  batz log overwrites another. Earlier notes in this header use the new
#  name.
#  RENAMED, same day, per Josh: function batz.merge_sm4.logfile() ->
#  batz.merge_sm4.logfiles() (this file was batz.merge_sm4.logfile.dev.R).
#  Earlier notes use the new name.
# =============================================================================

## ---- helper: standardize.headers (per Josh, 2026-09-14) - inlined, since
## this is a standalone dev script, not part of the package.
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

sm.logfile.schemas <- function() {
  common <- c("date", "time", "lat", "ns", "lon", "ew", "power_v", "temp_c")
  list(
    "SM4"     = c(common, "files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c(common, "acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c(common, "acfiles", "acl", "acr")
  )
}

sm.logfile.detect.version <- function(hdrs) {
  schemas <- sm.logfile.schemas()
  full <- vapply(schemas, function(s) all(s %in% hdrs), logical(1))
  if (any(full)) {
    cand <- names(schemas)[full]
    return(cand[which.max(lengths(schemas[cand]))])
  }
  distinct <- list(
    "SM4"     = c("files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c("acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c("acfiles", "acl", "acr")
  )
  ## acfiles alone can't tell SM5.1.5 from SM5.1.6 - score it for neither
  ## on its own; it only tips the balance toward "SM5" when paired with
  ## that version's other distinguishing headers.
  score <- vapply(distinct, function(d) {
    hits <- d %in% hdrs
    if (sum(hits[d != "acfiles"]) == 0) 0 else sum(hits)
  }, numeric(1))
  if (max(score) == 0 || sum(score == max(score)) > 1) return("unknown")
  names(score)[which.max(score)]
}

sm.logfile.merge <- function(dir.load, dir.sub, load.pattern, duplicates.remove,
                             versions.keep, caller.name) {

  schemas <- sm.logfile.schemas()

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                     jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

  convert.date <- function(x) {
    m <- regmatches(x, regexpr("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x))
    out <- x
    has.match <- nzchar(m)
    if (any(has.match)) {
      parts <- regmatches(x[has.match], regexec("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x[has.match]))
      converted <- vapply(parts, function(p) {
        mm <- month.lookup[tolower(p[3])]
        if (is.na(mm)) return(NA_character_)
        paste(p[2], mm, p[4], sep = "-")
      }, character(1))
      out[has.match] <- ifelse(is.na(converted), x[has.match], converted)
    }
    out
  }

  make.log.row <- function(aru.name, filename, filepath, version, load.status, reason,
                            date.start = NA_character_, date.end = NA_character_,
                            date.unique = NA_integer_, date.range = NA_integer_,
                            records = NA_integer_) {
    data.frame(aru.name = aru.name, filename = filename, version = version,
               date.start = date.start, date.end = date.end,
               date.unique = date.unique, date.range = date.range,
               records = records, load.status = load.status, reason = reason,
               filepath = filepath, stringsAsFactors = FALSE)
  }

  other.fn <- function(v) {
    if (v == "SM4") "batz.merge_sm4.logfiles() or batz.merge_sm.logfiles()"
    else "batz.merge_sm5.logfiles() or batz.merge_sm.logfiles()"
  }

  process.one.file <- function(f) {
    base.name <- basename(f)
    file.aru.name <- sub("_.*$", "", base.name)

    raw <- tryCatch(
      utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      return(list(data = NULL, version = "unknown",
                  log = make.log.row(file.aru.name, base.name, f, "unknown",
                                     "Failure", "could not read file")))
    }

    names(raw) <- standardize.headers(names(raw))
    version <- sm.logfile.detect.version(names(raw))

    if (version == "unknown") {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     "could not identify SM version from headers")))
    }
    if (!(version %in% versions.keep)) {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     paste0(version, " file - not loaded by ", caller.name,
                                            " (use ", other.fn(version), ")"))))
    }

    expected <- schemas[[version]]
    present <- expected %in% names(raw)
    headers.missing <- !all(present)
    no.data <- nrow(raw) == 0

    if (headers.missing || no.data) {
      missing.list <- paste(expected[!present], collapse = ", ")
      reason <- if (headers.missing && no.data) {
        paste0("no data and These headers are missing: ", missing.list)
      } else if (headers.missing) {
        paste0("These headers are missing: ", missing.list)
      } else {
        "no data"
      }
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure", reason)))
    }

    output <- expected
    output[output == "lon"] <- "longitude"
    tmp <- raw[expected]
    names(tmp) <- output
    for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

    tmp$aru.name <- file.aru.name
    tmp$date <- convert.date(tmp$date)

    ns <- tolower(trimws(tmp$ns))
    ew <- tolower(trimws(tmp$ew))
    tmp$Y <- ifelse(ns == "s", -as.numeric(tmp$lat), as.numeric(tmp$lat))
    tmp$X <- ifelse(ew == "w", -as.numeric(tmp$longitude), as.numeric(tmp$longitude))

    tmp <- tmp[c("aru.name", output, "X", "Y")]

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

    list(data = tmp, version = version,
         log = make.log.row(file.aru.name, base.name, f, version, "Success",
                            "All headers present and observation in file",
                            date.start = date.start.val, date.end = date.end.val,
                            date.unique = date.unique.n, date.range = date.range.val,
                            records = nrow(tmp)))
  }

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                          recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  pieces   <- stats::setNames(lapply(versions.keep, function(v) list()), versions.keep)
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
        cat("  loaded ", f, " [", r$version, "] (", nrow(r$data), " rows)\n", sep = "")
        pieces[[r$version]][[length(pieces[[r$version]]) + 1]] <- r$data
      }
    }
  }

  merged <- lapply(versions.keep, function(v) {
    p <- pieces[[v]]
    if (length(p) == 0) return(data.frame())
    df <- do.call(rbind, p)
    if (duplicates.remove) {
      dup.mask <- duplicated(df)
      if (any(dup.mask)) {
        cat("\n", sum(dup.mask), " duplicate row(s) removed from ", v, " data.\n", sep = "")
        df <- df[!dup.mask, , drop = FALSE]
      }
    }
    rownames(df) <- NULL
    df
  })
  names(merged) <- versions.keep

  log.df <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    make.log.row(character(0), character(0), character(0), character(0),
                 character(0), character(0), character(0), character(0),
                 integer(0), integer(0), integer(0))
  }

  list(data = merged, log = log.df)
}

batz.merge_sm4.logfiles <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE) {

  ## 2026-09-29: shared engine (batz.util_sm.logfile.R) - detects each
  ## file's SM version from its headers and only merges SM4 files here.
  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = "SM4", caller.name = "batz.merge_sm4.logfiles()")

  sm4logs.merged <- out$data[["SM4"]]
  if (nrow(sm4logs.merged) == 0) {
    cat("\nNo files were successfully loaded - sm4logs.merged is empty.\n")
  }

  result <- list(sm4logs.merged = sm4logs.merged)
  if (log.file) result$log.file_sm4 <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

# =============================================================================
# TEST DATA: point test.dir at a folder of SM summary files. The tests
# below were run 2026-09-29 against Josh's real NWS01/NWS02/NWS03
# (SM5 firmware 1.5) files plus synthetic SM4 / SM5.1.6 / bad-header /
# empty / unrecognised files (NWS03 in a subfolder, to test dir.sub).
# =============================================================================
test.dir <- "/home/claude/sm/testdata"
## ---- tests (shared by all three dev scripts) ------------------------------
if (!exists("test.dir")) test.dir <- "/home/claude/sm/testdata"
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg); cat("[PASS]", msg, "\n") }
lg <- function(L, f) L[L$filename == f, ]

if (exists("batz.merge_sm.logfiles")) {
  cat("\n=== batz.merge_sm.logfiles(): real NWS01-03 + synthetic ===\n")
  r <- batz.merge_sm.logfiles(test.dir, dir.sub = TRUE, log.file = TRUE)
  print(log.file_sm[, c("aru.name","filename","version","date.start","date.end","date.unique","date.range","records","load.status")])
  ok(setequal(names(r), c("SM4","SM5_1.5","SM5_1.6","log.file_sm")), "returns SM4, SM5_1.5, SM5_1.6, log.file_sm")
  ok(all(c("SM4","SM5_1.5","SM5_1.6","log.file_sm") %in% ls(globalenv())), "all four auto-assigned")
  ok(nrow(log.file_sm) == 8, "one log row per matched file (8)")
  ok(all(lg(log.file_sm,"NWS01_A_Summary_1.txt")$version == "SM5.1.5",
         lg(log.file_sm,"NWS03_A_Summary_1.txt")$version == "SM5.1.5"), "real NWS files detected as SM5.1.5")
  n.real <- sum(log.file_sm$records[log.file_sm$version == "SM5.1.5"])
  ok(n.real == 27279 + 27283 + 23681, "SM5.1.5 records = all real data rows (78243)")
  ok(nrow(SM5_1.5) <= n.real && nrow(SM5_1.5) > 78000, "SM5_1.5 merged (after duplicate removal)")
  ok(identical(names(SM5_1.5), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c",
       "acfiles","fs1files","fs2files","zc1files","zc2files","scrub1","scrub2","X","Y")), "SM5_1.5 column names")
  ok(is.numeric(SM5_1.5$scrub2) && is.numeric(SM5_1.5$power_v), "trailing-space numeric columns read as numeric")
  ok(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", SM5_1.5$date)), "SM5 dates converted to YYYY-MM-DD")
  ok(all(SM5_1.5$X[SM5_1.5$longitude != 0] < 0), "W longitudes -> negative X")
  ok(setequal(unique(SM5_1.5$aru.name), c("NWS01","NWS02","NWS03")), "aru.name parsed from file names")
  n3 <- lg(log.file_sm,"NWS03_A_Summary_1.txt")
  ok(n3$date.start == "2025-05-29" && n3$date.end == "2026-07-13" && n3$date.range > n3$date.unique, "NWS03 2025 test rows show as a date gap")
  ok(lg(log.file_sm,"AYERS_A_Summary.txt")$version == "SM4" && nrow(SM4) == 2, "SM4 file -> SM4 frame")
  ok(identical(names(SM4), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c","files","scrubbed","mic0_type","X","Y")), "SM4 column names match sm4logs.merged")
  ok(lg(log.file_sm,"NEW16_A_Summary.txt")$version == "SM5.1.6" && nrow(SM5_1.6) == 2 &&
       identical(names(SM5_1.6), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c","acfiles","acl","acr","X","Y")), "SM5.1.6 file -> SM5_1.6 frame")
  b <- lg(log.file_sm,"BAD16_A_Summary.txt")
  ok(b$version == "SM5.1.6" && b$load.status == "Failure" && b$reason == "These headers are missing: temp_c", "partial 1.6 file -> SM5.1.6 Failure, missing temp_c")
  e <- lg(log.file_sm,"EMPTY16_B_Summary.txt")
  ok(e$version == "SM5.1.6" && e$reason == "no data", "empty 1.6 file -> no data")
  o <- lg(log.file_sm,"ODD_A_Summary.txt")
  ok(o$version == "unknown" && o$reason == "could not identify SM version from headers", "unrecognised headers -> unknown")
}

if (exists("batz.merge_sm5.logfiles")) {
  cat("\n=== batz.merge_sm5.logfiles() ===\n")
  rm(list = intersect(c("SM4","SM5_1.5","SM5_1.6","log.file_sm5"), ls(globalenv())), envir = globalenv())
  r <- batz.merge_sm5.logfiles(test.dir, dir.sub = TRUE, log.file = TRUE)
  ok(setequal(names(r), c("SM5_1.5","SM5_1.6","log.file_sm5")) && !exists("SM4", envir = globalenv()), "returns SM5_1.5, SM5_1.6, log.file_sm5 only")
  a <- lg(log.file_sm5,"AYERS_A_Summary.txt")
  ok(a$version == "SM4" && a$load.status == "Failure" && grepl("not loaded by batz.merge_sm5.logfiles", a$reason), "SM4 file skipped with version SM4")
  ok(nrow(SM5_1.6) == 2 && nrow(SM5_1.5) > 78000, "both SM5 frames filled")
  r2 <- batz.merge_sm5.logfiles(test.dir, dir.sub = TRUE)
  ok(is.null(r2$log.file_sm5), "log.file = FALSE omits the log")
  if (exists("batz.merge_sm.logfiles"))
    ok(exists("log.file_sm") && nrow(log.file_sm) == 8, "log.file_sm (all-units log) not overwritten by the SM5 run")
}

if (exists("batz.merge_sm4.logfiles")) {
  cat("\n=== batz.merge_sm4.logfiles() ===\n")
  r <- batz.merge_sm4.logfiles(test.dir, dir.sub = TRUE, log.file = TRUE)
  L <- log.file_sm4
  ok(identical(names(L), c("aru.name","filename","version","date.start","date.end","date.unique","date.range","records","load.status","reason","filepath")), "log has new $version column after $filename")
  ok(nrow(sm4logs.merged) == 2 && all(sm4logs.merged$aru.name == "AYERS"), "only the SM4 file merged")
  n1 <- lg(L,"NWS01_A_Summary_1.txt")
  ok(n1$version == "SM5.1.5" && n1$load.status == "Failure" && grepl("use batz.merge_sm5.logfiles", n1$reason), "SM5 file skipped, version SM5.1.5, reason points to sm5 function")
  ## original SM4 failure-reason behaviour still intact
  d <- file.path(tempdir(), "sm4bad"); dir.create(d, showWarnings = FALSE)
  writeLines(c("DATE,TIME,LAT,NS,LON,EW,POWER(V),#FILES,#SCRUBBED,MIC0 TYPE","2026-Jun-05,20:00:00,44.5,N,70.6,W,12.1,10,0,U2"), file.path(d,"BADHEADER_A_Summary.txt"))
  writeLines("DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#FILES,#SCRUBBED,MIC0 TYPE", file.path(d,"EMPTY_A_Summary.txt"))
  batz.merge_sm4.logfiles(d, log.file = TRUE)
  ok(lg(log.file_sm4,"BADHEADER_A_Summary.txt")$reason == "These headers are missing: temp_c", "SM4 missing-header reason unchanged")
  ok(lg(log.file_sm4,"EMPTY_A_Summary.txt")$reason == "no data" && nrow(sm4logs.merged) == 0, "SM4 no-data reason unchanged; empty result ok")
  e <- tempfile(); dir.create(e); batz.merge_sm4.logfiles(e, log.file = TRUE)
  ok(nrow(log.file_sm4) == 0 && "version" %in% names(log.file_sm4), "empty folder: 0-row log keeps all columns")
}
if (exists("batz.merge_sm.logfiles") && exists("batz.merge_sm5.logfiles") && exists("batz.merge_sm4.logfiles"))
  ok(all(c("log.file_sm","log.file_sm5","log.file_sm4") %in% ls(globalenv())) &&
       nrow(log.file_sm) == 8 && nrow(log.file_sm5) == 8, "log.file_sm, log.file_sm5, log.file_sm4 all coexist")
cat("\nALL TESTS PASSED\n")

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
res1 <- batz.merge_sm4.logfiles(test.dir, dir.sub = TRUE, log.file = TRUE)
cat("\ndim sm4logs.merged:", paste(dim(sm4logs.merged), collapse = " x "), "\n")
print(sm4logs.merged[, c("aru.name", "date", "time", "lat", "longitude")])
cat("\nlog.file_sm4:\n")
print(log.file_sm4)

stopifnot(nrow(log.file_sm4) == 6)
stopifnot(nrow(sm4logs.merged) == 5)

get.row <- function(fname) log.file_sm4[log.file_sm4$filename == fname, ]

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

stopifnot(all(!is.na(log.file_sm4$aru.name)),
          all(!is.na(log.file_sm4$filename)),
          all(!is.na(log.file_sm4$filepath)))
cat("[PASS] aru.name/filename/filepath always populated, even on Failure\n")

cat("\n=== log.file = FALSE (log.file_sm4 should not be created) ===\n")
if (exists("log.file_sm4", envir = .GlobalEnv, inherits = FALSE)) {
  rm(log.file_sm4, envir = .GlobalEnv)
}
res2 <- batz.merge_sm4.logfiles(test.dir, dir.sub = TRUE, log.file = FALSE)
stopifnot(is.null(res2$log.file_sm4))
stopifnot(!exists("log.file_sm4", envir = .GlobalEnv, inherits = FALSE))
cat("[PASS] log.file = FALSE correctly omits log.file_sm4\n")

cat("\n=== empty directory ===\n")
empty.dir <- tempfile(); dir.create(empty.dir)
res3 <- batz.merge_sm4.logfiles(empty.dir, log.file = TRUE)
stopifnot(nrow(sm4logs.merged) == 0, nrow(log.file_sm4) == 0)
cat("[PASS] empty directory: 0 rows, 0 log rows, no error\n")

# -----------------------------------------------------------------------------
# TEST load.pattern (2026-09-29): wider default "*_A_Summary*.txt" /
# "*_B_Summary*.txt". Builds its own fixtures in a temp folder.
# -----------------------------------------------------------------------------
cat("\n=== load.pattern: suffixed/copied file names ===\n")
lp.dir <- file.path(tempdir(), "sm4_pattern_test")
unlink(lp.dir, recursive = TRUE); dir.create(file.path(lp.dir, "subdir"), recursive = TRUE)
lp.hdr <- "DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#FILES,#SCRUBBED,MIC0 TYPE"
lp.row <- function(dt) sprintf("%s,20:00:00,44.5,N,70.6,W,12.1,18.5,10,0,U2", dt)
lp.write <- function(f, dts) writeLines(c(lp.hdr, vapply(dts, lp.row, character(1))), file.path(lp.dir, f))
lp.write("WTG-GOM102_A_Summary.txt",        c("2026-Jun-01", "2026-Jun-02"))
lp.write("WTG-GOM102_A_Summary - Copy.txt", c("2026-Jun-01", "2026-Jun-02"))  # exact duplicate
lp.write("AYERS_B_Summary_2026.txt",        "2026-Jun-05")
lp.write("subdir/SUB_A_summary.TXT",        "2026-Jun-07")                    # case-insensitive
lp.write("NOPE_A_Summary.csv",              "2026-Jun-09")                    # wrong extension
lp.write("NOPE_C_Summary.txt",              "2026-Jun-09")                    # not A/B
lp.write("NOPE_A_Summary.txt.bak",          "2026-Jun-09")                    # not ending .txt

batz.merge_sm4.logfiles(lp.dir, dir.sub = TRUE, log.file = TRUE)
stopifnot(nrow(log.file_sm4) == 4,
          !any(grepl("NOPE", log.file_sm4$filename)),
          "WTG-GOM102_A_Summary - Copy.txt" %in% log.file_sm4$filename,
          "AYERS_B_Summary_2026.txt" %in% log.file_sm4$filename)
cat("[PASS] suffixed/' - Copy'/mixed-case files found; .csv/_C_/.bak ignored\n")
stopifnot(nrow(sm4logs.merged) == 4,
          sum(sm4logs.merged$aru.name == "WTG-GOM102") == 2)
cat("[PASS] ' - Copy' duplicate rows removed by duplicates.remove; aru.name = WTG-GOM102\n")

batz.merge_sm4.logfiles(lp.dir, dir.sub = TRUE, log.file = TRUE,
                       load.pattern = c("*_A_Summary.txt", "*_B_Summary.txt"))
stopifnot(nrow(log.file_sm4) == 2)
cat("[PASS] old pattern still works when passed explicitly (2 files)\n")

cat("\nORIGINAL SM4 TESTS PASSED\n")

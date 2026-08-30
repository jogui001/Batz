# batz.merge_vetted.acoustics.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.vettedacoustics_*
# Action:  merge.format
#
# UPDATE (2026-08-26, per Josh) - adds to the existing merge/lat-lon-split/
# filename-parse pipeline:
#   - new optional inputs: manid.kp = TRUE, manid.sb = TRUE,
#     trim.noise = TRUE, trim.noid = FALSE
#   - positional header rename (11 existing columns -> new names)
#   - header reorder
#   - $call.datetime via batz.datawrangler_call.datetime()
#   - $manid/$autoid.kp/$autoid.sb recoded via batz.batusa_recode.names()
#   - $manid.kp/$manid.sb "fill blank manual ID from the matching auto ID"
#     columns (optional)
#   - trim.noise/trim.noid row-removal (optional)
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS FLAGGED FOR JOSH (spec was open on these - see delivery note):
#
#  1. Josh's own rename list, verbatim, was missing a comma between
#     "autoid.sb" and "lat":
#       c("filename", "date.mon","manid","autoid.kp", "autoid.sb"   "lat", ...)
#     Read as a typo, not a real 10-element list - it's an 11-element
#     positional rename onto the CURRENT 11-column order coming out of the
#     existing merge/lat-lon-split/filename-parse steps:
#       filename, monitoringnight, speciesmanualid, wakaleidoscopeautoid,
#       sppaccp, lat, serial, lon, aru.name, date, time
#     ->
#       filename, date.mon, manid, autoid.kp, autoid.sb, lat, serial, lon,
#       aru.name, date, time
#     ("wakaleidoscopeautoid" -> "autoid.kp" and "sppaccp" -> "autoid.sb"
#     make sense once you notice the function merges files from either
#     k-Pro/Kaleidoscope [kp] or SonoBat [sb] vetting software - Kaleidoscope's
#     own auto ID column becomes $autoid.kp, SonoBat's "accepted species"
#     column becomes $autoid.sb.)
#
#  2. The fill-in prose says "overwrite that element with the entry for
#     $auto.kp"/"$auto.sb" - there is no column anywhere in this spec (or
#     any earlier one) literally called "$auto.kp"/"$auto.sb". Read as
#     shorthand for the just-renamed "$autoid.kp"/"$autoid.sb" columns,
#     matching the rest of the spec's own naming and the fact that
#     recode.names() (the step immediately before this one) is run on
#     exactly those two columns.
#
#  3. batz.batusa_recode.names() is called with output.format = bat.names
#     (a new optional input, default "code4") and default grammar.dash =
#     TRUE. Originally this was hardcoded to recode.names()'s own default
#     ("common"); per Josh's follow-up ("add bat.names = 'code4' / If
#     bat.names = 'code4' ... set the output name to default") a new
#     bat.names parameter now controls this, defaulting to "code4"
#     instead. Josh's own wording admits more than one reading - read here
#     as "bat.names IS the output.format value, and its own default is
#     'code4'" (a real behavior change: manid/autoid.kp/autoid.sb now come
#     back as 4-letter codes, not common names) rather than "bat.names ==
#     'code4' means fall back to recode.names()'s internal default of
#     'common'" (which would make the new parameter inert for its own
#     default value - seemed like a strange thing to add a parameter for).
#     FLAGGED for Josh to confirm.
#
#  4. New columns land in this order, appended at the very end (not
#     specified): ... date, time, call.datetime, manid.kp, manid.sb.
#
#  5. "Empty" for the manid.kp/manid.sb fill check means NA or a
#     blank/whitespace-only string (not specified either way - same
#     "don't silently misrepresent missing data" convention already used
#     in batz.datawrangler_call.datetime).
#
#  6. trim.noise/trim.noid check $manid (the manual ID column, AFTER
#     recode.names() has run on it) for the literal, case-insensitive
#     strings "noise"/"NoID" - NOT $manid.kp/$manid.sb. This is safe
#     precisely because recode.names() passes through anything that isn't
#     a recognized species name/code unchanged - "noise" and "NoID" don't
#     match anything in the bat reference table, so they survive the
#     recode step untouched and are still there to match against.
#     trim.noise/trim.noid run AFTER the manid.kp/manid.sb fill-in step
#     (matching the order Josh gave), so a "noise"-flagged row's
#     $manid.kp/$manid.sb would also have been filled from the auto ID
#     columns before being dropped - harmless, since the whole row is
#     removed anyway.
#
#  7. **Follow-up (2026-08-27, later still, per Josh: "update
#     batz.merge_vetted.acoustics() to include copying over
#     $sunregion from input data") - $sunregion is now copied through when
#     a raw input file already has it.** Read as OPTIONAL pass-through, not
#     a new required header: $sunregion is not added to expected.headers,
#     so a file that lacks it is still merged normally (its rows just get
#     NA for $sunregion), matching this function's existing tolerant,
#     skip-only-on-genuinely-missing-required-headers design. A file whose
#     (normalized) headers DO include "sunregion" has that column carried
#     straight through, unchanged, into the merged output - placed next to
#     $serial/$aru.name (the other detector-level columns) rather than at
#     the very end. This still doesn't make the function itself DO the
#     join described in batz.generate_plotframe.bat's own docs (matching
#     $aru.name against an *arulist.csv) - it only preserves $sunregion
#     when the raw per-file input already has it, e.g. if a future export
#     or a manually-augmented file already carries the column. See TEST 11
#     below.
#
#  8. **Follow-up (2026-08-30, per Josh bug report: a real Mobile-transect
#     vetted export was being silently skipped entirely) - $serial is now
#     OPTIONAL, handled exactly like $sunregion (assumption #7).** The
#     reported file (a Mobile-format export) has no Serial/similar header at
#     all - confirmed directly, and confirmed the load.pattern matching
#     itself was NOT the problem (both "*vetted.csv" and the exact file name
#     match it via glob2rx()/list.files()). $serial is removed from
#     expected.headers; a file lacking it is no longer skipped - it merges
#     with NA for $serial instead. See TEST 13 below.
# ---------------------------------------------------------------------------

## ===========================================================================
## SECTION 0: load dependency functions (same package, sourced here only
## because this is a standalone dev script - in the real package both are
## already in the same namespace, no explicit sourcing needed)
## ===========================================================================
source("/home/claude/datetime_work/batz.datawrangler_call.datetime.R")
source("/home/claude/vettedacoustics_work2/batz.batusa_recode.names.R")

## ===========================================================================
## SECTION 1: function under test (same body as the final .R file)
## ===========================================================================
batz.merge_vetted.acoustics <- function(dir.load = getwd(),
                                               load.pattern = c("*vetted.csv"),
                                               dir.sub = FALSE,
                                               duplicates.remove = TRUE,
                                               log.file = FALSE,
                                               bat.names.out = "code4",
                                               manid.kp = TRUE,
                                               manid.sb = TRUE,
                                               trim.noise = TRUE,
                                               trim.noid = FALSE) {

  expected.headers <- c("filename", "monitoringnight", "speciesmanualid",
                         "wakaleidoscopeautoid", "sppaccp", "lat")

  normalize.header <- function(x) tolower(gsub("[^[:alnum:]]", "", x))

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
    if (is.null(tmp)) { add.log(f, "could not read file", "none"); next }
    names(tmp) <- normalize.header(names(tmp))
    missing.headers <- setdiff(expected.headers, names(tmp))
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", ")); next
    }
    if (nrow(tmp) == 0) { add.log(f, "no records", "none"); next }
    ## $serial is OPTIONAL (2026-08-30 follow-up), not one of the required
    ## expected.headers - a file is never skipped for lacking it (e.g. a
    ## Mobile-transect export, which has no fixed detector serial number at
    ## all). Captured BEFORE trimming to expected.headers below, exactly like
    ## $sunregion, since that trim would otherwise silently drop it.
    serial.vals <- if ("serial" %in% names(tmp)) as.character(tmp$serial) else
      rep(NA_character_, nrow(tmp))
    ## $sunregion is OPTIONAL, not one of the required expected.headers - a
    ## file is never skipped for lacking it. If a file's own (normalized)
    ## headers happen to include it, copy those values straight through;
    ## otherwise this file's rows get NA for $sunregion (still needs to be
    ## joined in separately, exactly as before, for files that don't already
    ## carry it). Captured BEFORE trimming to expected.headers below, since
    ## that trim would otherwise silently drop it.
    sunregion.vals <- if ("sunregion" %in% names(tmp)) as.character(tmp$sunregion) else
      rep(NA_character_, nrow(tmp))
    tmp <- tmp[, expected.headers, drop = FALSE]
    tmp$serial <- serial.vals
    tmp$sunregion <- sunregion.vals
    vetted.merged <- rbind(vetted.merged, tmp)
  }

  if (duplicates.remove && nrow(vetted.merged) > 0) {
    vetted.merged <- vetted.merged[!duplicated(vetted.merged), ]
  }

  if (nrow(vetted.merged) > 0) {
    ## $lat holds "<lat> <lon>" - split into two numeric columns
    latlon <- strsplit(trimws(vetted.merged$lat), "\\s+")
    vetted.merged$lat <- vapply(latlon, function(x) as.numeric(x[1]), numeric(1))
    vetted.merged$lon <- vapply(latlon, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA_real_, numeric(1))

    ## $filename = "<ARU>_<YYYYMMDD>_<HHMMSS>_<junk>" -> $aru.name/$date/$time
    m <- regmatches(vetted.merged$filename,
                     regexec("^([^_]+)_(\\d{8})_(\\d{6})_", vetted.merged$filename))
    vetted.merged$aru.name <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
    vetted.merged$date     <- vapply(m, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
    vetted.merged$time     <- vapply(m, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1))

    ## --- NEW (2026-08-26): rename, reorder, call.datetime, recode.names,
    ## manid.kp/manid.sb fill-in, trim.noise/trim.noid -------------------

    ## positional rename - see assumption #1 above ($sunregion, appended
    ## right after $serial back in the per-file loop above, keeps its own
    ## name here - no rename needed - see assumption #7)
    names(vetted.merged) <- c("filename", "date.mon", "manid", "autoid.kp",
                               "autoid.sb", "lat", "serial", "sunregion", "lon",
                               "aru.name", "date", "time")

    ## reorder ($sunregion placed with the other detector-level columns,
    ## next to $serial/$aru.name - see assumption #7)
    vetted.merged <- vetted.merged[, c("filename", "date.mon", "aru.name",
                                        "serial", "sunregion", "lat", "lon",
                                        "manid", "autoid.kp", "autoid.sb",
                                        "date", "time")]

    ## $call.datetime
    vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = vetted.merged$date, time = vetted.merged$time)

    ## recode manid/autoid.kp/autoid.sb (batname.format.out = bat.names.out, default "code4")
    vetted.merged$manid     <- batz.batusa_recode.names(vetted.merged$manid, batname.format.out = bat.names.out)
    vetted.merged$autoid.kp <- batz.batusa_recode.names(vetted.merged$autoid.kp, batname.format.out = bat.names.out)
    vetted.merged$autoid.sb <- batz.batusa_recode.names(vetted.merged$autoid.sb, batname.format.out = bat.names.out)

    is.empty <- function(x) is.na(x) | !nzchar(trimws(x))

    if (manid.kp) {
      vetted.merged$manid.kp <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.kp)
      vetted.merged$manid.kp[blank] <- vetted.merged$autoid.kp[blank]
    }

    if (manid.sb) {
      vetted.merged$manid.sb <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.sb)
      vetted.merged$manid.sb[blank] <- vetted.merged$autoid.sb[blank]
    }

    if (trim.noise) {
      vetted.merged <- vetted.merged[!(tolower(trimws(vetted.merged$manid)) == "noise"), , drop = FALSE]
    }

    if (trim.noid) {
      vetted.merged <- vetted.merged[!(tolower(trimws(vetted.merged$manid)) == "noid"), , drop = FALSE]
    }
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

## ===========================================================================
## SECTION 2: build a small synthetic edge-case fixture (blank manid,
## NOISE, NoID, all-blank auto IDs) alongside the real data used below -
## the real FinalVetted.csv has real NOISE rows and real blank Species
## Manual ID rows, but no "NoID" literal anywhere, so trim.noid needs a
## synthetic exercise
## ===========================================================================
dir.create("testdata", showWarnings = FALSE)

synth <- data.frame(
  Filename = c("SYN-A_20260601_010101_000.wav",
               "SYN-A_20260601_020202_000.wav",
               "SYN-A_20260601_030303_000.wav",
               "SYN-A_20260601_040404_000.wav"),
  MonitoringNight = c("6/1/2026","6/1/2026","6/1/2026","6/1/2026"),
  `Species Manual ID` = c("", "NoID", "noise", "Epfu"),
  `WA|Kaleidoscope|Auto ID` = c("EPTFUS", "LASNOC", "LASNOC", "EPTFUS"),
  SppAccp = c("Laci", "", "", "Epfu"),
  Lat = rep("44.00000 -68.00000", 4),
  Serial = rep("S4U00001", 4),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth, "testdata/synth_vetted.csv", row.names = FALSE)

## ===========================================================================
## SECTION 3: real-data test - FinalVetted.csv (6,487 rows, real blank
## Species Manual IDs, real NOISE rows)
## ===========================================================================
dir.load <- "/mnt/user-data/uploads/4 Current  test data"

cat("=== TEST 1: real FinalVetted.csv, defaults ===\n")
res1 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                           load.pattern = "*FinalVetted.csv",
                                           dir.sub = FALSE)
cat("rows:", nrow(res1$vetted.merged), " cols:", ncol(res1$vetted.merged), "\n")
print(names(res1$vetted.merged))
print(head(res1$vetted.merged, 3))
cat("any NA in call.datetime?", any(is.na(res1$vetted.merged$call.datetime)), "\n")
cat("any 'noise' left in $manid (should be FALSE, trim.noise default TRUE)? ",
    any(tolower(trimws(res1$vetted.merged$manid)) == "noise"), "\n")
cat("any blank $manid.kp remaining where autoid.kp was also blank?\n")
blank.both <- with(res1$vetted.merged, (is.na(manid.kp) | !nzchar(trimws(manid.kp))))
cat(sum(blank.both), "\n\n")

cat("=== TEST 2: real data, trim.noise = FALSE (keep noise rows) ===\n")
res2 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                           load.pattern = "*FinalVetted.csv",
                                           dir.sub = FALSE,
                                           trim.noise = FALSE)
cat("rows:", nrow(res2$vetted.merged), "(should be", nrow(res1$vetted.merged) + sum(tolower(trimws(res1$vetted.merged$manid))=="noise", na.rm=TRUE) , "more than test 1's row count, i.e. more rows than test 1)\n")
cat("noise rows present:", sum(tolower(trimws(res2$vetted.merged$manid)) == "noise"), "\n\n")

cat("=== TEST 3: manid.kp/manid.sb spot check (real blank-manid rows) ===\n")
blank.rows <- which(nchar(trimws(res2$vetted.merged$manid)) == 0)
cat("n rows with blank $manid (post-recode):", length(blank.rows), "\n")
print(head(res2$vetted.merged[blank.rows, c("manid","autoid.kp","autoid.sb","manid.kp","manid.sb")], 5))

cat("\n=== TEST 4: manid.kp = FALSE / manid.sb = FALSE (columns should not exist) ===\n")
res4 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                           load.pattern = "*FinalVetted.csv",
                                           dir.sub = FALSE,
                                           manid.kp = FALSE, manid.sb = FALSE)
cat("has manid.kp col?", "manid.kp" %in% names(res4$vetted.merged), "\n")
cat("has manid.sb col?", "manid.sb" %in% names(res4$vetted.merged), "\n\n")

## ===========================================================================
## SECTION 4: synthetic-fixture test - exercises trim.noid, blank
## autoid.sb-driven manid.sb fill, and the "NoID" pass-through
## ===========================================================================
cat("=== TEST 5: synthetic fixture, trim.noid = TRUE, trim.noise = TRUE ===\n")
res5 <- batz.merge_vetted.acoustics(dir.load = "testdata",
                                           load.pattern = "*synth_vetted.csv",
                                           trim.noise = TRUE, trim.noid = TRUE,
                                           duplicates.remove = FALSE)
print(res5$vetted.merged[, c("manid","autoid.kp","autoid.sb","manid.kp","manid.sb")])
cat("rows remaining (started at 4, should drop the 'NoID' and 'noise' rows -> 2 left):",
    nrow(res5$vetted.merged), "\n\n")

cat("=== TEST 6: synthetic fixture, trim.noid = FALSE (default), trim.noise = TRUE (default) ===\n")
res6 <- batz.merge_vetted.acoustics(dir.load = "testdata",
                                           load.pattern = "*synth_vetted.csv",
                                           duplicates.remove = FALSE)
print(res6$vetted.merged[, c("manid","autoid.kp","autoid.sb","manid.kp","manid.sb")])
cat("rows remaining (should keep the 'NoID' row, drop only 'noise' -> 3 left):",
    nrow(res6$vetted.merged), "\n\n")

cat("=== TEST 7: bare call auto-assign into caller's environment ===\n")
rm(list = c("vetted.merged"), envir = .GlobalEnv)
suppressWarnings(rm(vetted.merged))
batz.merge_vetted.acoustics(dir.load = "testdata", load.pattern = "*synth_vetted.csv",
                                   duplicates.remove = FALSE)
cat("vetted.merged exists after bare call?", exists("vetted.merged"), "\n")
cat("rows:", nrow(vetted.merged), "\n\n")

cat("=== TEST 8: log.file = TRUE still works alongside the new pipeline ===\n")
res8 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                           load.pattern = "*FinalVetted.csv",
                                           log.file = TRUE)
cat("has vetted.merged_log.file?", "vetted.merged_log.file" %in% names(res8), "\n")
print(res8$vetted.merged_log.file)

cat("=== TEST 9: bat.names.out default ('code4') - real data spot check ===\n")
res9 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                           load.pattern = "*FinalVetted.csv",
                                           dir.sub = FALSE)
print(head(res9$vetted.merged[, c("manid", "autoid.kp", "autoid.sb")], 3))
cat("manid values look like 4-letter codes (lowercase)?",
    all(grepl("^[a-z]{4}$|^$", res9$vetted.merged$manid[nzchar(res9$vetted.merged$manid)] [1:5])), "\n\n")

cat("=== TEST 10: bat.names.out = 'common' explicitly (old behavior, still available) ===\n")
res10 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                            load.pattern = "*FinalVetted.csv",
                                            dir.sub = FALSE,
                                            bat.names.out = "common")
print(head(res10$vetted.merged[, c("manid", "autoid.kp", "autoid.sb")], 3))

## ===========================================================================
## SECTION 5: $sunregion pass-through (2026-08-27, per Josh - assumption #7)
## ===========================================================================
dir.create("testdata/sunregion", showWarnings = FALSE, recursive = TRUE)

## file A: HAS a $sunregion column (any-case/punctuation header variant,
## to confirm normalize.header() picks it up the same way it does every
## other expected header)
synth.with.sun <- data.frame(
  Filename = c("SYN-B_20260601_010101_000.wav", "SYN-B_20260601_020202_000.wav"),
  MonitoringNight = c("6/1/2026", "6/1/2026"),
  `Species Manual ID` = c("Epfu", "Laci"),
  `WA|Kaleidoscope|Auto ID` = c("EPTFUS", "LASNOC"),
  SppAccp = c("Epfu", "Laci"),
  Lat = rep("44.00000 -68.00000", 2),
  Serial = rep("S4U00002", 2),
  `Sun_Region` = c("penobscotbay", "penobscotbay"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth.with.sun, "testdata/sunregion/withsun_vetted.csv", row.names = FALSE)

## file B: does NOT have a $sunregion column at all (same shape as the
## original synth fixture)
synth.no.sun <- data.frame(
  Filename = c("SYN-C_20260601_010101_000.wav"),
  MonitoringNight = c("6/1/2026"),
  `Species Manual ID` = c("Epfu"),
  `WA|Kaleidoscope|Auto ID` = c("EPTFUS"),
  SppAccp = c("Epfu"),
  Lat = c("45.00000 -69.00000"),
  Serial = c("S4U00003"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth.no.sun, "testdata/sunregion/nosun_vetted.csv", row.names = FALSE)

cat("\n=== TEST 11: $sunregion pass-through - present in one file, absent in another, merged together ===\n")
res11 <- batz.merge_vetted.acoustics(dir.load = "testdata/sunregion",
                                            load.pattern = "*vetted.csv",
                                            duplicates.remove = FALSE)
print(res11$vetted.merged[, c("aru.name", "serial", "sunregion")])
cat("has $sunregion column at all?", "sunregion" %in% names(res11$vetted.merged), "\n")
cat("SYN-B rows (had a real $sunregion column) got 'penobscotbay'?",
    all(res11$vetted.merged$sunregion[res11$vetted.merged$aru.name == "SYN-B"] == "penobscotbay"), "\n")
cat("SYN-C row (no $sunregion column in its source file) got NA?",
    is.na(res11$vetted.merged$sunregion[res11$vetted.merged$aru.name == "SYN-C"]), "\n")
cat("column position - sunregion lands right after $serial?",
    which(names(res11$vetted.merged) == "sunregion") == which(names(res11$vetted.merged) == "serial") + 1, "\n\n")

cat("=== TEST 12: real FinalVetted.csv (no $sunregion column in the raw file) still merges fine, $sunregion all NA ===\n")
res12 <- batz.merge_vetted.acoustics(dir.load = dir.load,
                                            load.pattern = "*FinalVetted.csv",
                                            dir.sub = FALSE)
cat("has $sunregion column?", "sunregion" %in% names(res12$vetted.merged), "\n")
cat("all NA (no file supplied it)?", all(is.na(res12$vetted.merged$sunregion)), "\n")
cat("row count unaffected by this change?", nrow(res12$vetted.merged) == nrow(res1$vetted.merged), "\n\n")

## ===========================================================================
## SECTION 6: $serial optional (2026-08-30, per Josh bug report) - a real
## Mobile-transect vetted export with NO Serial column at all was being
## silently skipped entirely ("mismatched headers"). Reproduced here with a
## synthetic Mobile-style file (no Serial header) alongside a normal
## stationary-style file (has Serial), merged together in one call.
## ===========================================================================
dir.create("testdata/mobile", showWarnings = FALSE, recursive = TRUE)

## file A: stationary-style, HAS $serial
synth.stationary <- data.frame(
  Filename = c("SYN-D_20260601_010101_000.wav"),
  MonitoringNight = c("6/1/2026"),
  `Species Manual ID` = c("Epfu"),
  `WA|Kaleidoscope|Auto ID` = c("EPTFUS"),
  SppAccp = c("Epfu"),
  Lat = c("44.00000 -68.00000"),
  Serial = c("S4U00004"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth.stationary, "testdata/mobile/stationary_vetted.csv", row.names = FALSE)

## file B: Mobile-style, NO $serial column anywhere (matches the real
## reported file's shape - no Serial header at all)
synth.mobile <- data.frame(
  Filename = c("105059-MOB_20260716_220211_000.wav"),
  Filename2 = c("MOBILE_20260716_220211_000.wav"),
  MonitoringNight = c("7/16/2026"),
  Lat = c("44.76230 -70.89998"),
  `Species Manual ID` = c("2Bat"),
  `WA|Kaleidoscope|Auto ID` = c("LASNOC"),
  SppAccp = c("Lano"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth.mobile, "testdata/mobile/mobile_vetted.csv", row.names = FALSE)

cat("=== TEST 13: $serial optional - Mobile file (no Serial column) merges instead of being skipped ===\n")
res13 <- batz.merge_vetted.acoustics(dir.load = "testdata/mobile",
                                            load.pattern = "*vetted.csv",
                                            duplicates.remove = FALSE, log.file = TRUE)
cat("rows merged (should be 2 - both files kept, neither skipped for 'serial'):",
    nrow(res13$vetted.merged), "\n")
print(res13$vetted.merged[, c("aru.name", "serial")])
cat("stationary file kept its real $serial?",
    res13$vetted.merged$serial[res13$vetted.merged$aru.name == "SYN-D"] == "S4U00004", "\n")
cat("mobile file (no Serial column) got NA for $serial instead of being skipped?",
    is.na(res13$vetted.merged$serial[res13$vetted.merged$aru.name == "105059-MOB"]), "\n")
cat("log shows zero files skipped for 'mismatched headers' due to serial:\n")
print(res13$vetted.merged_log.file)
cat("\n")

cat("\nAll dev-script tests completed.\n")

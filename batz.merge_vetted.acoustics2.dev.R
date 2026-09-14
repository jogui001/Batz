# batz.merge_vetted.acoustics2.dev.R
#
# DEV / TEST VERSION - Batz project
#
# REVISION (2026-09-06, per Josh) to the existing batz.merge_vetted.acoustics2():
#   - output column names are now the SHORT abbreviated forms throughout
#     (mon.ngh, manid, auto.kp, auto.sb, aru.serial, sunregion, lat, lon,
#     filename) instead of the long canonical vetting-software header names
#     used previously
#   - header.rename table's job flips accordingly: it now maps EITHER a raw
#     abbreviated Josh-ism (file.name, serial, sunregions) OR the long
#     canonical name a real vetted-software export normalizes to
#     (monitoringnight, speciesmanualid, wakaleidoscopeautoid, sppaccp)
#     directly onto the short target name
#   - header categories reduced to THREE: required, results, optional - lat/
#     lon moved from their own blocking "location" category (v1 of this
#     function) to fully optional/non-blocking, and the X/Y location form is
#     no longer supported (not in this revision's spec)
#   - `log.file` removed as an INPUT parameter entirely - every attempted
#     file is now unconditionally logged (this matches the old log.file =
#     TRUE default, now just always-on)
#   - new post-merge step: an auto-id column (auto.kp/auto.sb) that is 100%
#     NA across the whole merge is dropped entirely
#
# REVISION (2026-09-06, later same day, per Josh):
#   - added back alternate location-header spellings ($long, X/Y) via the
#     header.rename table instead of restoring dedicated location-parsing
#     code (see TEST 10b below)
#   - added two brand-new results-header auto-ID columns, auto.bc (from a
#     raw BCID header) and auto.ec (from a raw EchoClass header), alongside
#     auto.kp/auto.sb - results headers briefly became "at least one of
#     auto.kp/auto.sb/auto.bc/auto.ec", and manid.kp/manid.sb fill-in was
#     extended with two analogous manid.bc/manid.ec columns
#
# REVISION (2026-09-08, per Josh) - REMOVES the auto.bc/auto.ec (BCID/
# EchoClass) work added in the second 2026-09-06 revision above:
#   - Josh's reason: BCID and EchoClass don't follow the same export format
#     as Kaleidoscope/SonoBat, so the simple "file has a literal BCID/
#     EchoClass header cell, rename it straight to auto.bc/auto.ec" model
#     this function uses for Kaleidoscope ($wakaleidoscopeautoid) and
#     SonoBat ($sppaccp) doesn't fit them. (Confirmed by research into how
#     BCID/EchoClass actually export: both are multi-section Excel reports
#     that need marker-string row-scanning just to locate a data table in
#     the first place, not a flat table with a clean header row - and
#     EchoClass's real species-result column is named "Prominent Species",
#     not "EchoClass".)
#   - results headers are back to auto.kp/auto.sb only (>= 1 required);
#     manid.bc/manid.ec and the BCID->auto.bc/EchoClass->auto.ec rows in
#     arumerge.headerrename.csv are removed.
#   - every test fixture/assertion below that referenced BCID/EchoClass/
#     auto.bc/auto.ec/manid.bc/manid.ec has been updated accordingly (see
#     inline notes at each changed fixture); TEST 11 (previously exercising
#     the auto.bc/auto.ec all-NA column drop) now exercises the same
#     all-NA-drop logic on auto.sb instead, since that behavior itself is
#     unchanged and still worth covering.
#
# REVISION (2026-09-14, per Josh's new project-wide header-standardization
# preference): batz.merge_vetted.acoustics2()'s own internal
# normalize.header() - which stripped all separators out entirely (deleting
# word boundaries) - was replaced with the shared package helper
# standardize.headers() (trim, collapse non-alphanumeric runs to a single
# underscore, lowercase), applied to both a file's own raw headers and to
# arumerge.headerrename.csv's "raw" column. This dev script is standalone
# (not part of the installed package), so standardize.headers() is inlined
# below rather than sourced. Real matching-behavior change: two raw headers
# that used to collapse to the same no-separator string ("Species Manual
# ID" -> "speciesmanualid") now keep their word boundaries as underscores
# ("species_manual_id"); arumerge.headerrename.csv was updated to match
# (its "Species Manual ID"/"WA|Kaleidoscope|Auto ID" rows now carry literal
# spelled-out text with real separators, and a new "Sun Region" -> sunregion
# row was added, since that header used to auto-match the canonical
# "sunregion" name by coincidence under the old delete-based normalization
# and no longer does). See batz.merge_vetted.acoustics2.R's own @details
# "Header standardization" paragraph for the full explanation.
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS FLAGGED FOR JOSH:
#
#  1. The header-rename table Josh supplied this round has each row as
#     "<in> <recode>" - read as "<raw header a file might actually have>
#     <target short column name>". Two rows (file.name/filename and
#     sunregions/sunregion) keep the same raw-then-target order as the
#     PREVIOUS version's table; the other rows (monitoringnight/mon.ngh,
#     speciesmanualid/manid, wakaleidoscopeautoid/auto.kp, sppaccp/auto.sb,
#     serial/serials) read at face value would rename a file's already-
#     correct canonical header INTO an abbreviated one and then fail
#     downstream validation (which now checks for the abbreviated names) -
#     that can't be what's intended. Resolved by treating every row
#     uniformly as raw-header-text -> short-target-name.
#  2. The "serial" row's target was given as "serials" - but the requested
#     blank output header is "$aru.serial", not "$serials". Read as a typo;
#     the reference table (`arumerge.headerrename.csv`) now maps
#     serial -> aru.serial instead.
#  3. "If rename = TRUE then add user defined headers to the 'headers'
#     dataframe by collum position, ignore conflicting headers" - read as:
#     the working header-rename table starts empty, and (only when
#     rename = TRUE) the CSV at header.rename.path is read positionally
#     (column 1 = raw, column 2 = target) and appended to it; "ignore
#     conflicting headers" is read as "if the CSV itself has two rows for
#     the same raw header, keep only the first" (first-occurrence wins,
#     matching this package's existing match.first convention elsewhere).
#  4. The Steps section says "three sets of headers: 1) required, 2)
#     results, 3) optional" and separately describes $lat/$lon under
#     "Optional headers" - read literally (a real change from the prior
#     version of this function, where location was its own blocking
#     category with three valid forms including X/Y). This revision drops
#     X/Y support - not mentioned anywhere in this round's spec - since
#     location no longer blocks loading at all. (X/Y support was later
#     added back the same day via the header-rename table - see TEST 10b.)
#  5. trim.noise/trim.noid/manid.kp/manid.sb (existing parameters/behavior
#     from the current function) are not mentioned in this round's
#     "add inputs"/"remove inputs" sections at all, so they're kept
#     unchanged from the current version - only `log.file` was explicitly
#     named for removal.
#  6. `log.file` the RETURNED DATA FRAME still exists (still one of the two
#     required outputs) - only the `log.file` INPUT PARAMETER is removed.
#     Every attempted file (success or failure) is now always logged.
#  7. (2026-09-08) auto.bc/auto.ec/manid.bc/manid.ec removed per Josh - see
#     the REVISION note above. No open question here; this was an explicit
#     instruction, not an inferred assumption.
#  8. (2026-09-14) arumerge.headerrename.csv's raw column was rewritten to
#     spell out real header text with its original separators, and a new
#     "Sun Region" -> sunregion row was added - both required by the switch
#     to standardize.headers(), not optional cleanup. See the REVISION note
#     above.
# ---------------------------------------------------------------------------

## standardize.headers() - inlined here because this dev script is
## standalone (not part of the installed package); in the real package
## every function in R/ is loaded together, so batz.merge_vetted.acoustics2()
## can call it directly without this. See batz.util_standardize.headers.R.
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

source("/home/claude/merge_vetted2/deps/batz.datawrangler_rename.R")
source("/home/claude/merge_vetted2/deps/batz.datawrangler_call.datetime.R")
source("/home/claude/merge_vetted2/deps/batz.batusa_recode.names.R")
source("/home/claude/merge_vetted2/batz.merge_vetted.acoustics2.R")

setwd("/home/claude/merge_vetted2")

dir.create("testdata2", showWarnings = FALSE)
dir.create("testdata2/rename", showWarnings = FALSE)
dir.create("testdata2/xlsx_out", showWarnings = FALSE)
dir.create("testdata2/dup", showWarnings = FALSE)

write.row <- function(path, ...) {
  df <- data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
  write.csv(df, path, row.names = FALSE)
}

## 0. every possible header present -> "success all headers", nothing missing
write.row("testdata2/file_complete_vetted.csv",
          Filename = "COMP-A_20260601_000000_000.wav",
          MonitoringNight = "6/1/2026",
          `Species Manual ID` = "Epfu",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          SppAccp = "Epfu",
          Lat = 44.9, Lon = -68.9,
          Serial = "S4U00099",
          `Sun Region` = "penobscotbay")

## 1. all headers present (canonical/raw software names), explicit lat/lon,
## has auto.kp+auto.sb but is missing the optional Sun Region header ->
## "success missing" (sunregion)
write.row("testdata2/file_full_vetted.csv",
          Filename = "FULL-A_20260601_010101_000.wav",
          MonitoringNight = "6/1/2026",
          `Species Manual ID` = "Epfu",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          SppAccp = "Epfu",
          Lat = 44.5, Lon = -68.5,
          Serial = "S4U00001")

## 2. combined "lat lon" string (only $lat present); missing aru.serial +
## sunregion (optional); has only sppaccp (one results header) ->
## "success missing"
write.row("testdata2/file_oneresult_vetted.csv",
          Filename = "ONERES_20260602_020202_000.wav",
          MonitoringNight = "6/2/2026",
          `Species Manual ID` = "Laci",
          SppAccp = "Laci",
          Lat = "44.20000 -68.90000")

## 3. no lat/lon at all (still merges - location is now optional, never
## blocks); has auto.kp only (changed 2026-09-08: previously used EchoClass
## as its sole results header - now that EchoClass is no longer recognized,
## this fixture uses Kaleidoscope's own auto-ID header instead so it still
## has >= 1 results header and continues to exercise the "no location"
## path rather than tripping the "missing results headers" failure path)
write.row("testdata2/file_noloc_vetted.csv",
          Filename = "NOLOC_20260603_030303_000.wav",
          MonitoringNight = "6/3/2026",
          `Species Manual ID` = "Mylu",
          `WA|Kaleidoscope|Auto ID` = "MYLU",
          Serial = "S4U00002")

## 4. missing a required header (no Species Manual ID at all) -> "failure"
write.row("testdata2/file_missingreq_vetted.csv",
          Filename = "MISSREQ_20260604_040404_000.wav",
          MonitoringNight = "6/4/2026",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          Lat = "44.00000 -68.00000")

## 5. missing BOTH results headers -> "failure"
write.row("testdata2/file_missingres_vetted.csv",
          Filename = "MISSRES_20260605_050505_000.wav",
          MonitoringNight = "6/5/2026",
          `Species Manual ID` = "Epfu",
          Lat = "44.00000 -68.00000")

## 6. noise/NoID/blank-manid rows, both auto-id columns present, for
## trim.noise/trim.noid + manid.kp/sb fill-in checks (changed 2026-09-08:
## dropped this fixture's BCID/EchoClass columns along with the feature)
synth.trim <- data.frame(
  Filename = c("TRIM-A_20260607_010101_000.wav",
               "TRIM-A_20260607_020202_000.wav",
               "TRIM-A_20260607_030303_000.wav",
               "TRIM-A_20260607_040404_000.wav"),
  MonitoringNight = rep("6/7/2026", 4),
  `Species Manual ID` = c("", "NoID", "noise", "Epfu"),
  `WA|Kaleidoscope|Auto ID` = c("EPTFUS", "LASNOC", "LASNOC", "EPTFUS"),
  SppAccp = c("Laci", "", "", "Epfu"),
  Lat = rep(44.0, 4), Lon = rep(-68.0, 4),
  Serial = rep("S4U00003", 4),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(synth.trim, "testdata2/file_trim_vetted.csv", row.names = FALSE)

## 7. raw/abbreviated headers (file.name/serial/sunregions), needs
## rename = TRUE to be recognized at all
write.row("testdata2/rename/file_rawheaders_vetted.csv",
          file.name = "RAW-A_20260608_070707_000.wav",
          MonitoringNight = "6/8/2026",
          `Species Manual ID` = "Epfu",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          Lat = 44.3, Lon = -68.3,
          serial = "S4U00004",
          sunregions = "penobscotbay")

cat("=== TEST 1: mixed fixture set, defaults ===\n")
res1 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "*vetted.csv",
                                      dir.sub = FALSE,
                                      save.xlsx = FALSE)
cat("rows in data:", nrow(res1$data), "\n")
print(res1$data[, c("filename", "aru.name", "aru.serial", "sunregion", "lat", "lon", "manid")])
cat("\ncolumn names in data:\n")
print(names(res1$data))
cat("\nlog.file:\n")
print(res1$log.file)
stopifnot(all(c("failure", "success all headers", "success missing") %in% res1$log.file$status))
stopifnot(res1$log.file$`missing headers`[res1$log.file$filename == file.path("testdata2", "file_complete_vetted.csv")] == "")
stopifnot(!("auto.bc" %in% names(res1$data)))
stopifnot(!("auto.ec" %in% names(res1$data)))
cat("PASS: all three status categories appear; auto.bc/auto.ec no longer exist as columns\n\n")

cat("=== TEST 2: file_full_vetted.csv alone - has auto.kp/auto.sb but is missing the optional sunregion header -> 'success missing' (sunregion) ===\n")
res2 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "file_full_vetted.csv",
                                      save.xlsx = FALSE)
print(res2$log.file)
print(res2$data[, c("manid", "auto.kp", "auto.sb")])
stopifnot(res2$log.file$status == "success missing")
stopifnot(grepl("sunregion", res2$log.file$`missing headers`))
stopifnot(res2$data$lat == 44.5 && res2$data$lon == -68.5)
cat("PASS\n\n")

cat("=== TEST 3: combined lat/lon string split correctly, one results header only ===\n")
res3 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "file_oneresult_vetted.csv",
                                      save.xlsx = FALSE)
print(res3$data[, c("lat", "lon", "manid", "auto.sb")])
print(res3$log.file)
stopifnot(res3$data$lat == 44.2 && res3$data$lon == -68.9)
stopifnot(res3$log.file$status == "success missing")
cat("PASS\n\n")

cat("=== TEST 4: no location headers at all - does NOT fail (location is optional now), lat/lon NA ===\n")
res4 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "file_noloc_vetted.csv",
                                      save.xlsx = FALSE)
print(res4$log.file)
print(res4$data[, c("lat", "lon", "auto.kp", "aru.serial")])
stopifnot(res4$log.file$status != "failure")
stopifnot(is.na(res4$data$lat) && is.na(res4$data$lon))
stopifnot(res4$data$aru.serial == "S4U00002")
cat("PASS: missing lat/lon does not block loading\n\n")

cat("=== TEST 5: missing required header AND missing both results headers -> both logged as failure with distinct reasons ===\n")
res5 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = c("file_missingreq_vetted.csv",
                                                        "file_missingres_vetted.csv"),
                                      save.xlsx = FALSE)
print(res5$log.file)
stopifnot(nrow(res5$data) == 0)
stopifnot(all(res5$log.file$status == "failure"))
stopifnot(any(grepl("required", res5$log.file$reason)))
stopifnot(any(grepl("results", res5$log.file$reason)))
cat("PASS\n\n")

cat("=== TEST 6: trim.noise = TRUE (default), trim.noid = FALSE (default); manid.kp/sb fill-in ===\n")
res6 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "file_trim_vetted.csv",
                                      duplicates.remove = FALSE,
                                      save.xlsx = FALSE)
print(res6$data[, c("manid", "auto.kp", "auto.sb", "manid.kp", "manid.sb")])
cat("rows remaining (started at 4, drop only 'noise' -> 3 left):", nrow(res6$data), "\n")
stopifnot(nrow(res6$data) == 3)
stopifnot(!any(tolower(trimws(res6$data$manid)) == "noise"))
stopifnot(all(c("manid.kp", "manid.sb") %in% names(res6$data)))
stopifnot(!("manid.bc" %in% names(res6$data)))
stopifnot(!("manid.ec" %in% names(res6$data)))
cat("PASS\n\n")

cat("=== TEST 7: trim.noid = TRUE too -> drops 'NoID' as well (2 left) ===\n")
res7 <- batz.merge_vetted.acoustics2(dir.load = "testdata2",
                                      load.pattern = "file_trim_vetted.csv",
                                      duplicates.remove = FALSE,
                                      trim.noid = TRUE,
                                      save.xlsx = FALSE)
cat("rows remaining:", nrow(res7$data), "\n")
stopifnot(nrow(res7$data) == 2)
cat("PASS\n\n")

cat("=== TEST 8: raw/abbreviated headers, rename = FALSE -> file fails (headers not recognized) ===\n")
res8 <- batz.merge_vetted.acoustics2(dir.load = "testdata2/rename",
                                      load.pattern = "*vetted.csv",
                                      rename = FALSE,
                                      save.xlsx = FALSE)
print(res8$log.file)
stopifnot(nrow(res8$data) == 0)
stopifnot(res8$log.file$status == "failure")
cat("PASS: without rename, file.name/serial/sunregions are not recognized\n\n")

cat("=== TEST 9: same raw-header file, rename = TRUE (default), header.rename.path pointed at the real reference table -> succeeds ===\n")
res9 <- batz.merge_vetted.acoustics2(dir.load = "testdata2/rename",
                                      load.pattern = "*vetted.csv",
                                      rename = TRUE,
                                      header.rename.path = "/home/claude/merge_vetted2/arumerge.headerrename.csv",
                                      save.xlsx = FALSE)
print(res9$log.file)
print(res9$data[, c("filename", "aru.name", "aru.serial", "sunregion", "manid", "auto.kp")])
stopifnot(nrow(res9$data) == 1)
stopifnot(res9$data$aru.serial == "S4U00004")
stopifnot(res9$data$sunregion == "penobscotbay")
cat("PASS: rename lookup bridges file.name/serial/sunregions into filename/aru.serial/sunregion\n\n")

cat("=== TEST 10: exact duplicate rows removed by default ===\n")
file.copy("testdata2/file_full_vetted.csv", "testdata2/dup/file_full_copy1_vetted.csv", overwrite = TRUE)
res10 <- batz.merge_vetted.acoustics2(dir.load = "testdata2/dup",
                                       load.pattern = "*vetted.csv",
                                       save.xlsx = FALSE)
cat("rows (should be 1):", nrow(res10$data), "\n")
stopifnot(nrow(res10$data) == 1)
cat("PASS\n\n")

cat("=== TEST 10b: alternate location spellings - $long (full spelling) and $X/$Y both resolve to lat/lon via the rename table (added 2026-09-06, per Josh) ===\n")
dir.create("testdata2/altloc", showWarnings = FALSE)
write.row("testdata2/altloc/file_xy_vetted.csv",
          Filename = "XYTEST_20260601_010101_000.wav",
          MonitoringNight = "6/1/2026",
          `Species Manual ID` = "Epfu",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          X = -68.5, Y = 44.5)
write.row("testdata2/altloc/file_long_vetted.csv",
          Filename = "LONGTEST_20260602_020202_000.wav",
          MonitoringNight = "6/2/2026",
          `Species Manual ID` = "Laci",
          SppAccp = "Laci",
          Lat = 45.1, Long = -69.1)
res10b <- batz.merge_vetted.acoustics2(dir.load = "testdata2/altloc",
                                        load.pattern = "*vetted.csv",
                                        save.xlsx = FALSE)
print(res10b$data[, c("aru.name", "lat", "lon")])
stopifnot(res10b$data$lat[res10b$data$aru.name == "XYTEST"] == 44.5)
stopifnot(res10b$data$lon[res10b$data$aru.name == "XYTEST"] == -68.5)
stopifnot(res10b$data$lat[res10b$data$aru.name == "LONGTEST"] == 45.1)
stopifnot(res10b$data$lon[res10b$data$aru.name == "LONGTEST"] == -69.1)
cat("PASS: X/Y -> lat/lon (via x->long then the long-is-an-alias-for-lon step) and Long -> lon both work\n\n")

cat("=== TEST 11 (changed 2026-09-08 - previously covered the now-removed auto.bc/auto.ec drop): dropped all-NA auto-id column - a fixture set where NOTHING supplies auto.sb should not have that column (or its manid.sb fill) at all ===\n")
dir.create("testdata2/noautosb", showWarnings = FALSE)
write.row("testdata2/noautosb/file_a_vetted.csv",
          Filename = "NASB-A_20260610_010101_000.wav",
          MonitoringNight = "6/10/2026",
          `Species Manual ID` = "Epfu",
          `WA|Kaleidoscope|Auto ID` = "EPTFUS",
          Lat = 44.1, Lon = -68.1)
res11 <- batz.merge_vetted.acoustics2(dir.load = "testdata2/noautosb",
                                       load.pattern = "*vetted.csv",
                                       save.xlsx = FALSE)
print(names(res11$data))
stopifnot(!("auto.sb" %in% names(res11$data)))
stopifnot(!("manid.sb" %in% names(res11$data)))
stopifnot(!("auto.bc" %in% names(res11$data)))
stopifnot(!("auto.ec" %in% names(res11$data)))
cat("PASS: auto.sb (and its manid.sb fill) is absent when no file ever supplies it; auto.bc/auto.ec don't exist at all\n\n")

cat("=== TEST 12: save.xlsx = TRUE writes one workbook with data/log.file sheets ===\n")
if (requireNamespace("openxlsx", quietly = TRUE)) {
  file.copy("testdata2/file_full_vetted.csv", "testdata2/xlsx_out/file_full_vetted.csv", overwrite = TRUE)
  res12 <- batz.merge_vetted.acoustics2(dir.load = "testdata2/xlsx_out",
                                         load.pattern = "*vetted.csv",
                                         project.name = "SevenIslands")
  saved <- list.files("testdata2/xlsx_out", pattern = "SevenIslands.*\\.xlsx$")
  cat("saved file(s):", paste(saved, collapse = ", "), "\n")
  stopifnot(length(saved) == 1)
  sheets <- openxlsx::getSheetNames(file.path("testdata2/xlsx_out", saved))
  cat("sheet names:", paste(sheets, collapse = ", "), "\n")
  stopifnot(all(c("data", "log.file") %in% sheets))
  cat("PASS\n\n")
} else {
  cat("openxlsx not installed in this sandbox - install.packages('openxlsx') to exercise this test.\n")
  cat("(the function falls back to a warning + skips saving, rather than erroring.)\n\n")
}

cat("=== TEST 13: bare call auto-assign into caller's environment ===\n")
suppressWarnings(rm(data, log.file))
batz.merge_vetted.acoustics2(dir.load = "testdata2", load.pattern = "file_full_vetted.csv",
                              save.xlsx = FALSE)
cat("`data` exists after bare call?", exists("data"), "\n")
cat("`log.file` exists after bare call?", exists("log.file"), "\n")
stopifnot(exists("data") && exists("log.file"))
cat("PASS\n\n")

cat("\nAll dev-script tests completed.\n")

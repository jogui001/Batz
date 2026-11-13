# =============================================================================
# batz.merge_sm.logfiles.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.merge_sm.logfiles() (Batz package).
# Merges Song Meter summary logs into SM4, SM5_1.5 and SM5_1.6, detected from
# each file's headers. A version with no data is not created.
# The tests write small SM4 and SM5 (firmware 1.6) summary files to a temp
# folder, so no real data is needed. To run on real data, set test.dir below.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## ---- test files: one SM4, one SM5 firmware 1.6, no SM5 firmware 1.5 ---------
test.dir <- file.path(tempdir(), "sm_logs_test"); dir.create(test.dir, showWarnings = FALSE)
writeLines(c("DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#FILES,#SCRUBBED,MIC0 TYPE",
             "2026-Jun-05,20:00:00,44.5,N,70.6,W,12.1,18.5,10,0,U2",
             "2026-Jun-06,20:00:00,44.5,N,70.6,W,12.0,17.0,12,1,U2"),
           file.path(test.dir, "AYERS_A_Summary.txt"))
writeLines(c("DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#ACFILES,#ACL,#ACR",
             "2026-Jun-05,20:00:00,43.6,N,71.7,W,5.1,19.0,30,15,15",
             "2026-Jun-06,20:00:00,43.6,N,71.7,W,5.0,18.0,28,14,14"),
           file.path(test.dir, "NEW16_A_Summary.txt"))

## ---- 1. no SM5.1.5 files: SM5_1.5 is not created --------------------------
rm(list = intersect(c("SM4", "SM5_1.5", "SM5_1.6", "log.file_sm"), ls()))
r1 <- batz.merge_sm.logfiles(test.dir, log.file = TRUE)       # NOTE names SM5_1.5
print(names(r1))
ok(setequal(names(r1), c("SM4", "SM5_1.6", "log.file_sm")), "returns SM4, SM5_1.6, log.file_sm only")
ok(!exists("SM5_1.5", inherits = FALSE), "SM5_1.5 not created in the workspace")
ok(nrow(SM4) == 2 && nrow(SM5_1.6) == 2, "SM4 and SM5_1.6 have 2 rows each")
print(log.file_sm[, c("aru.name", "version", "records", "load.status", "reason")])

## ---- 2. only SM4 files: both SM5 data frames left out -----------------------
d2 <- file.path(tempdir(), "sm_logs_sm4only"); dir.create(d2, showWarnings = FALSE)
invisible(file.copy(file.path(test.dir, "AYERS_A_Summary.txt"), d2, overwrite = TRUE))
r2 <- batz.merge_sm.logfiles(d2)
ok(identical(names(r2), "SM4"), "only SM4 returned")

## ---- 3. empty folder: nothing returned except the log -----------------------
d3 <- file.path(tempdir(), "sm_logs_empty"); dir.create(d3, showWarnings = FALSE)
r3 <- batz.merge_sm.logfiles(d3, log.file = TRUE)
ok(identical(names(r3), "log.file_sm"), "empty folder: only log.file_sm")

## ---- 4. output columns: $latitude / $longitude signed, no $X/$Y (2026-11-06) ----
print(names(SM4))      # aru.name, date, time, latitude, ns, longitude, ew, power.v, temperature.c, files, scrubbed, mic0.type
print(names(SM5_1.6))  # aru.name, date, time, latitude, ns, longitude, ew, power.v, temperature.c, acfiles, acl, acr
ok(identical(names(SM4), c("aru.name", "date", "time", "latitude", "ns", "longitude", "ew", "power.v",
                           "temperature.c", "files", "scrubbed", "mic0.type")), "SM4 headers")
ok(!any(c("X", "Y", "lat") %in% c(names(SM4), names(SM5_1.6))), "no $X, $Y or $lat")
ok(all(SM4$longitude == -70.6) && all(SM4$latitude == 44.5), "EW = W -> negative longitude; NS = N -> positive latitude")
ok(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", SM4$date)), "dates YYYY-MM-DD")

## ---- 5. south / east coordinates -------------------------------------------
d5 <- file.path(tempdir(), "sm_logs_se"); dir.create(d5, showWarnings = FALSE)
writeLines(c("DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#FILES,#SCRUBBED,MIC0 TYPE",
             "2026-Jun-05,20:00:00,33.9,S,151.2,E,12.1,18.5,10,0,U2"),
           file.path(d5, "SYD_A_Summary.txt"))
r5 <- batz.merge_sm.logfiles(d5)
ok(r5$SM4$latitude == -33.9 && r5$SM4$longitude == 151.2, "NS = S -> negative latitude; EW = E -> positive longitude")
## the merged logs go straight into batz.summarize_aruloc.coordinates() (needs $latitude/$longitude)

cat("\nALL TESTS PASSED\n")

## ---- real data (edit the path) ----------------------------------------------
# dir.logs <- "C:/path/to/SM summary files"
# batz.merge_sm.logfiles(dir.logs, dir.sub = TRUE, log.file = TRUE)
# table(log.file_sm$version, log.file_sm$load.status)

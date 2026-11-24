# =============================================================================
# batz.merge_vetted.acoustics.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.merge_vetted.acoustics() (Batz package).
# Section 1: made-up vetted files, run the whole script. Section 2
# (commented): real files.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
dir.t <- file.path(tempdir(), "merge_vetted"); unlink(dir.t, recursive = TRUE); dir.create(dir.t)
wr <- function(d, f) write.csv(d, file.path(dir.t, f), row.names = FALSE)

## ---- 1. made-up vetted files ------------------------------------------------------
## A: stationary SM4 file with Serial and a sunregion column
wr(data.frame(Filename = c("SYN-B_20260601_010101_000.wav", "SYN-B_20260601_020202_000.wav", "SYN-B_20260601_010101_000.wav"),
              MonitoringNight = "6/1/2026", `Species Manual ID` = c("Epfu", "", "Epfu"),  # rows 1 and 3 identical
              `WA|Kaleidoscope|Auto ID` = c("EPTFUS", "LASNOC", "EPTFUS"), SppAccp = c("Epfu", "Laci", "Epfu"),
              Lat = "44.00000 -68.00000", Serial = "S4U00002", sunregion = "penobscotbay",
              check.names = FALSE), "a_vetted.csv")
## B: mobile file, no Serial column
wr(data.frame(Filename = "105059-MOB_20260716_220211_000.wav", MonitoringNight = "7/16/2026",
              Lat = "44.76230 -70.89998", `Species Manual ID` = "Lano", `WA|Kaleidoscope|Auto ID` = "LASNOC",
              SppAccp = "Lano", check.names = FALSE), "b_vetted.csv")
## C: wav named by serial number, with a site column
wr(data.frame(Filename = "656346_20230506_180008_000.wav", MonitoringNight = "5/6/2023", Lat = "45.1 -69.1",
              `Species Manual ID` = "Mylu", SppAccp = "Mylu", Serial = "656346", site = "Clerk",
              check.names = FALSE), "c_vetted.csv")
## D: missing Lat -> skipped
wr(data.frame(Filename = "X_20260601_010101_000.wav", MonitoringNight = "6/1/2026", `Species Manual ID` = "Epfu",
              SppAccp = "Epfu", check.names = FALSE), "d_vetted.csv")

r <- batz.merge_vetted.acoustics(dir.load = dir.t, log.file = TRUE)
v <- r$vetted.merged
print(v)
ok(!"sunregion" %in% names(v), "no $sunregion in the output (even when a file has one)")
ok(identical(names(v)[1:11], c("filename", "date.monitoringnight", "aru.name", "aru.model", "aru.serial", "lat", "longitude",
                               "manid", "autoid.kp", "autoid.sb", "date")), "column order")
ok(nrow(v) == 4, "duplicate row removed (A has one exact duplicate), D skipped")
ok(is.na(v$aru.serial[v$aru.name == "105059-MOB"]), "no Serial column: $aru.serial NA, file kept")
ok(v$aru.name[v$filename == "656346_20230506_180008_000.wav"] == "Clerk", "wav named by serial: $aru.name from $site")
ok(all(c("manid.kp", "manid.sb") %in% names(v)) && isTRUE(v$manid.kp[v$filename == "SYN-B_20260601_020202_000.wav"] == "lano"),
   "blank $manid filled from the auto IDs in $manid.kp / $manid.sb")
ok(isTRUE(all.equal(v$longitude[v$aru.name == "SYN-B"][1], -68)), "$lat split into $lat / $longitude")
ok(nrow(r$vetted.merged_log.file) == 1 && grepl("lat", r$vetted.merged_log.file$headers.missing), "log: D skipped for mismatched headers")

r2 <- batz.merge_vetted.acoustics(dir.load = dir.t, snake_case = TRUE, manid.kp = FALSE, manid.sb = FALSE)
ok("date_monitoringnight" %in% names(r2$vetted.merged) && !any(c("manid_kp", "manid_sb") %in% names(r2$vetted.merged)),
   "snake_case = TRUE; manid.kp / manid.sb = FALSE")

## ---- 2. real files -------------------------------------------------------------------
# r <- batz.merge_vetted.acoustics(dir.load = "C:/.../vetted", dir.sub = TRUE, log.file = TRUE)
# head(r$vetted.merged)

cat("\nALL TESTS PASSED\n")

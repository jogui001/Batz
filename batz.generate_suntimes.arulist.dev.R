# =============================================================================
# batz.generate_suntimes.arulist.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_suntimes.arulist() (Batz package).
# Tests the three kinds of *arulist.csv:
#   A. the ARU list written by batz.merge_sitevisit.acoustics(generate.arulist = TRUE)
#      (no $sunregion.type / $schedule1 / $schedule2; has $sunregion_date.start/end)
#   B. the same list, pooled sun region
#   C. an older list with $sunregion.type and $schedule1/$schedule2, no sun-region dates
# Run the whole script; everything is written to a temporary folder.
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "suntimes_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
sub.dir <- function(x) { d <- file.path(dir.out, x); dir.create(d, showWarnings = FALSE); d }

base <- data.frame(aru.name = c("Clerk", "SugarHill", "Cemetery", "Ayers"),
                   longitude = c(-71.7364705, -71.73053717, -71.72612, -71.72153433),
                   latitude = c(43.59224283, 43.59525033, 43.598793, 43.59999983),
                   date.start = "6/26/2026", date.end = "7/2/2026", time.zone = "America/New_York")

## A. discrete, sun-region dates 6/1 - 7/31 (as bristol.arulist.csv)
a <- base; a$sunregion <- a$aru.name; a$sunregion.longitude <- a$longitude; a$sunregion.latitude <- a$latitude
a$sunregion_date.start <- "6/1/2026"; a$sunregion_date.end <- "7/31/2026"
write.csv(a, file.path(sub.dir("A"), "bristol.arulist.csv"), row.names = FALSE)
sa <- batz.generate_suntimes.arulist(dir.load = file.path(dir.out, "A"), dir.save = file.path(dir.out, "A"),
                                     project.name = "Bristol NH")$aru.suntimes      # NOTE: $sunregion.type set ...
ok(all(table(sa$aru.name) == 61), "A: one row per ARU per day, 6/1 to 7/31 (61 days)")
ok(identical(range(sa$date), as.Date(c("2026-06-01", "2026-07-31"))), "A: sun-region dates used")
ok(all(sa$sunregion.type == "fixed.unique"), "A: sunregion = aru.name gives fixed.unique")
ok(all(sa$date.start == as.Date("2026-06-26")), "A: output $date.start stays the deployment date")
ok(all(sa$schedule1 == "" & sa$schedule2 == ""), "A: schedules blank when absent")

## B. pooled: sunregion = project name at the central point; one blank sun-region date -> deployment dates
b <- base; b$sunregion <- "Bristol NH"
b$sunregion.longitude <- mean(b$longitude); b$sunregion.latitude <- mean(b$latitude)
b$sunregion_date.start <- c("", "6/1/2026", "6/1/2026", "6/1/2026"); b$sunregion_date.end <- "7/31/2026"
write.csv(b, file.path(sub.dir("B"), "pooled.arulist.csv"), row.names = FALSE)
sb <- batz.generate_suntimes.arulist(dir.load = file.path(dir.out, "B"), write.output = FALSE)$aru.suntimes
ok(all(sb$sunregion.type == "fixed.pooled"), "B: sunregion != aru.name gives fixed.pooled")
ok(identical(range(sb$date[sb$aru.name == "Clerk"]), as.Date(c("2026-06-26", "2026-07-02"))),
   "B: a blank sun-region date uses date.start/date.end")
ok(length(unique(sb$sunset[sb$date == as.Date("2026-07-01")])) == 1, "B: one sun time per pooled region and date")

## C. older list: $sunregion.type and $schedual1/$schedual2 (old spelling), no sun-region dates
cc <- a[, setdiff(names(a), c("sunregion_date.start", "sunregion_date.end"))]
cc$sunregion.type <- "fixed.unique"; cc$schedual1 <- "sunset-sunrise"; cc$schedual2 <- ""
write.csv(cc, file.path(sub.dir("C"), "old.arulist.csv"), row.names = FALSE)
sc <- batz.generate_suntimes.arulist(dir.load = file.path(dir.out, "C"), write.output = FALSE)$aru.suntimes
ok(identical(range(sc$date), as.Date(c("2026-06-26", "2026-07-02"))), "C: no sun-region dates - deployment dates")
ok(all(sc$schedule1 == "sunset-sunrise"), "C: old spelling schedual1 read as schedule1")

## the merged ARU list straight from batz.merge_sitevisit.acoustics():
# m <- batz.merge_sitevisit.acoustics(dir.load = wd.Raw, dir.save = wd.out, generate.arulist = TRUE)
# st <- batz.generate_suntimes.arulist(dir.load = wd.out, dir.save = wd.out, project.name = "Bristol NH")

## errors
d <- sub.dir("E"); write.csv(base[, -1], file.path(d, "bad.arulist.csv"), row.names = FALSE)
try(batz.generate_suntimes.arulist(dir.load = d, write.output = FALSE))   # missing headers: aru.name, sunregion, ...
cat("\nALL TESTS PASSED - output in", dir.out, "\n")

# =============================================================================
# batz.datawrangler_rename_arulist.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.datawrangler_rename_arulist() (Batz package).
# Section 1: made-up data, run the whole script. Section 2 (commented): the
# Norcross files.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## ---- 1. made-up data -------------------------------------------------------------------
## A1 at one location; B2 moved twice (Pond 4, Alden Brook, Pond 2)
arulist <- data.frame(aru.name = c("A1", "A1", "B2", "B2", "B2"),
                      aru.label = c("North", "North", "Pond 4", "Alden Brook", "Pond 2"),
                      date.start = c("5/28/2026", "5/28/2026", "6/3/2026", "6/17/2026", "6/29/2026"),
                      date.end = c("7/12/2026", "7/12/2026", "6/16/2026", "6/28/2026", "7/12/2026"))
data <- data.frame(filename = paste0("f", 1:9, ".wav"),
                   date.monitoringnight = c("6/1/2026", "8/1/2026", "6/16/2026", "6/17/2026", "6/28/2026", "6/29/2026",
                                            "5/28/2025", "6/20/2026", "6/20/2026"),
                   aru.name = c("A1", "a1 ", "B2", "B2", "B2", "B2", "B2", "C9", NA))
r <- batz.datawrangler_rename_arulist(data, arulist)
ok(identical(names(r)[1:4], c("filename", "date.monitoringnight", "aru.name", "aru.label")), "$aru.label right after $aru.name")
ok(identical(r$aru.label[1:2], c("North", "North")), "one label: used on every record (dates not checked; case / spaces ignored)")
ok(identical(r$aru.label[3:6], c("Pond 4", "Alden Brook", "Alden Brook", "Pond 2")), "several labels: picked by $date.monitoringnight, both ends included")
ok(is.na(r$aru.label[7]), "outside every window: NA")
ok(all(is.na(r$aru.label[8:9])), "not in aru.list / blank $aru.name: NA")

## no $date.monitoringnight: $call.datetime, calls before 12:00 count with the night before
d2 <- data.frame(aru.name = "B2", call.datetime = c("2026-06-17 01:30:00", "2026-06-17 21:30:00"))
ok(identical(batz.datawrangler_rename_arulist(d2, arulist)$aru.label, c("Pond 4", "Alden Brook")), "$call.datetime: 01:30 on 6/17 is the night of 6/16")

## overlapping windows: the first aru.list row
al3 <- arulist; al3$date.end[3] <- "6/20/2026"
ok(batz.datawrangler_rename_arulist(data[8, ], transform(al3, aru.name = sub("B2", "C9", aru.name)))$aru.label == "Pond 4", "overlap: first row in aru.list")

## an existing $aru.label is replaced; aru.list as a CSV path
f <- tempfile(fileext = ".csv"); write.csv(arulist, f, row.names = FALSE)
r4 <- batz.datawrangler_rename_arulist(cbind(data, aru.label = "old"), f)
ok(sum(names(r4) == "aru.label") == 1 && r4$aru.label[1] == "North", "old $aru.label replaced; aru.list read from a file")

## full dates required: the year must match; a date without a year gets NA
d5 <- data.frame(aru.name = "B2", date.monitoringnight = c("6/20/2026", "6/20/2025", "6/20", ""))
out5 <- capture.output(r5 <- batz.datawrangler_rename_arulist(d5, arulist))
ok(identical(r5$aru.label, c("Alden Brook", NA, NA, NA)), "full date: wrong year, no year or blank -> NA")
ok(any(grepl("without a full date.*Values: 6/20, blank", out5)) && any(grepl("outside every.*2025-06-20", out5)), "NOTEs: no full date / outside every window")
al6 <- arulist; al6$date.end[4] <- ""
e <- try(batz.datawrangler_rename_arulist(data, al6), silent = TRUE)
ok(inherits(e, "try-error") && grepl("need a full \\$date.start and \\$date.end.*B2 Alden Brook \\(6/17/2026 to blank\\)", e), "aru.list blank date.end: error")
al6$date.end[4] <- "6/28"
e <- try(batz.datawrangler_rename_arulist(data, al6), silent = TRUE)
ok(inherits(e, "try-error") && grepl("6/17/2026 to 6/28", e), "aru.list date without a year: error")
ok(identical(batz.datawrangler_rename_arulist(data[1, ], transform(arulist, date.end = ""))$aru.label, "North"), "one label: dates not needed")

## errors
e <- try(batz.datawrangler_rename_arulist(data, arulist[, 1:2]), silent = TRUE)
ok(inherits(e, "try-error") && grepl("needs \\$date.start and \\$date.end", e), "several labels without dates in aru.list: error")
e <- try(batz.datawrangler_rename_arulist(data[, -3], arulist), silent = TRUE)
ok(inherits(e, "try-error") && grepl("missing headers: data \\$aru.name", e), "no $aru.name: error")

## ---- 2. Norcross -------------------------------------------------------------------------
# merged <- read.csv("merged.csv")
# merged <- batz.datawrangler_rename_arulist(merged, "Norcross.arulist.csv")
# table(merged$aru.name, merged$aru.label, useNA = "ifany")

cat("\nALL TESTS PASSED\n")

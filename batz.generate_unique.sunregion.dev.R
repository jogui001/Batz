# =============================================================================
# batz.generate_unique.sunregion.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_unique.sunregion() (Batz package).
# Section 1 uses made-up data; section 2 uses Josh's NA-Bat arulist when the
# file is found. Run the whole script.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
d.out <- file.path(tempdir(), "unique.sunregion"); unlink(d.out, recursive = TRUE)

## ---- 1. made-up ARU list ---------------------------------------------------
arulist <- data.frame(
  aru.name            = c("A1", "A2", "A3", "B1", "B2", "C1", "D1", "E1"),
  Sunregion           = c("A", "A", "A ", "B", "B", "C", "", "E"),
  sunregion_longitude = c("-70.10", "-70.10", "-70.10", "-69.0", "-69.2", "-68.5", "-67", ""),
  "sunregion latitude" = c("44.10", "44.10", "44.10", "46.0", "46.4", "45.5", "45", ""),
  check.names = FALSE, stringsAsFactors = FALSE)

out <- capture.output(sr <- batz.generate_unique.sunregion(arulist, dir.save = d.out, project.name = "acme"))
print(sr); cat(out, sep = "\n")
ok(identical(names(sr), c("sunregion", "sunregion.longitude", "sunregion.latitude")), "output headers")
ok(identical(sr$sunregion, c("A", "B", "C", "E")), "one row per sun region, first-appearance order; \"A \" = \"A\"; blank dropped")
ok(sr$sunregion.longitude[1] == -70.1 && sr$sunregion.latitude[1] == 44.1, "A: one pair (repeated rows) kept")
ok(isTRUE(all.equal(sr$sunregion.longitude[2], -69.1)) && isTRUE(all.equal(sr$sunregion.latitude[2], 46.2)), "B: two pairs -> central point")
ok(is.na(sr$sunregion.longitude[4]) && is.na(sr$sunregion.latitude[4]), "E: no coordinates -> NA")
ok(any(grepl("central point.*B \\(2 pairs\\)", out)) && any(grepl("1 row\\(s\\) with a blank", out)) && any(grepl("\\(NA\\) for: E", out)),
   "NOTEs: central point, blank sunregion, NA")
f <- list.files(d.out, full.names = TRUE)
ok(length(f) == 1 && grepl("^acme_unique\\.sunregion_[0-9]{14}\\.csv$", basename(f)), "file acme_unique.sunregion_<timestamp>.csv")
ok(nrow(read.csv(f)) == 4, "CSV has the 4 sun regions")

e <- try(batz.generate_unique.sunregion(arulist["aru.name"], dir.save = NULL), silent = TRUE)
ok(inherits(e, "try-error") && grepl("missing these headers: sunregion, sunregion.longitude, sunregion.latitude", e), "missing headers: error")

sr2 <- batz.generate_unique.sunregion(arulist, dir.save = NULL)
ok(nrow(sr2) == 4, "dir.save = NULL: no file, table returned")
suppressWarnings(rm("acme_unique.sunregion", envir = globalenv()))
batz.generate_unique.sunregion(arulist, dir.save = d.out, project.name = "acme", save.auto = "generate 1")
ok(exists("acme_unique.sunregion", globalenv()) && length(list.files(d.out)) == 1, "save.auto = \"generate 1\": object made, no new file")

## ---- 2. Josh's NA-Bat arulist (path input) ----------------------------------
f.nabat <- "NA-Bat_20261118173904.arulist.csv"   # set to the file's folder if not in getwd()
if (file.exists(f.nabat)) {
  sr <- batz.generate_unique.sunregion(f.nabat, dir.save = NULL)
  print(sr)
  ok(nrow(sr) == 6 && setequal(sr$sunregion, c("105059", "107802", "1079", "108325", "5175", "55")), "NA-Bat: 6 sun regions")
} else cat("skip - NA-Bat arulist not found in", getwd(), "\n")

cat("\nALL TESTS PASSED\n")

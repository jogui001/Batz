## batz.generate_plotopts.dev.R
##
## Round nineteen, per Josh (2026-09-16). Dev/test script for the new
## batz.generate_plotopts() function: for each named plot function, copies
## its master plotopts_*.csv settings file into a new project-named file.
##
## Test data note: this uses the REAL master plotopts_bullseye.csv,
## plotopts_first.last.csv, and plotopts_callobs.csv files produced
## earlier in this same round nineteen pass (present in this same
## directory) - not synthetic stand-ins - since this function's whole job
## is to read/copy those exact files, so testing against the real ones is
## both possible and more meaningful here than for the plot functions'
## own dev scripts (which needed synthetic data due to the device bridge
## not being available this session).

## Run this script with the working directory set to the folder containing
## this file and the three master plotopts_*.csv files (e.g.
## `Rscript batz.generate_plotopts.dev.R` from inside that folder).
source("batz.generate_plotopts.R")

cat("========================================\n")
cat("TEST 1: default call - all three recognized plot functions\n")
cat("========================================\n")

out.dir1 <- file.path(tempdir(), "gp_test1")
if (dir.exists(out.dir1)) unlink(out.dir1, recursive = TRUE)

saved1 <- batz.generate_plotopts(project.name = "acme.wetlands.2026",
                                  dir.load = getwd(),
                                  dir.save = out.dir1)

stopifnot(length(saved1) == 3)
stopifnot(all(file.exists(file.path(out.dir1, saved1))))
stopifnot(identical(sort(saved1), sort(c(
  "acme.wetlands.2026_plotopts_batz.plotcover_bullseye.csv",
  "acme.wetlands.2026_plotopts_batz.plotdetections_first.last.csv",
  "acme.wetlands.2026_plotopts_batz.plotactivity_observations.csv"
))))
cat("PASS: all three files saved with expected names, all exist on disk.\n\n")

cat("========================================\n")
cat("TEST 2: generated bullseye file has the SAME columns/rows/values as the master\n")
cat("========================================\n")

master.bullseye <- utils::read.csv("plotopts_bullseye.csv", stringsAsFactors = FALSE,
                                    check.names = FALSE, colClasses = "character")
generated.bullseye <- utils::read.csv(
  file.path(out.dir1, "acme.wetlands.2026_plotopts_batz.plotcover_bullseye.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"
)

stopifnot(identical(names(master.bullseye), names(generated.bullseye)))
stopifnot(identical(dim(master.bullseye), dim(generated.bullseye)))
stopifnot(identical(master.bullseye, generated.bullseye))
stopifnot(identical(names(generated.bullseye), c("category", "parameter", "default.value",
                                                   "overide.value", "notes")))
cat("PASS: generated bullseye plotopts file is an exact copy of the master (columns, rows, values).\n\n")

cat("========================================\n")
cat("TEST 3: single function only\n")
cat("========================================\n")

out.dir3 <- file.path(tempdir(), "gp_test3")
if (dir.exists(out.dir3)) unlink(out.dir3, recursive = TRUE)

saved3 <- batz.generate_plotopts(generate.files = "batz.plotcover_bullseye",
                                  project.name = "solo.test",
                                  dir.load = getwd(),
                                  dir.save = out.dir3)

stopifnot(length(saved3) == 1)
stopifnot(saved3 == "solo.test_plotopts_batz.plotcover_bullseye.csv")
stopifnot(file.exists(file.path(out.dir3, saved3)))
stopifnot(!file.exists(file.path(out.dir3, "solo.test_plotopts_batz.plotdetections_first.last.csv")))
cat("PASS: only the requested function's settings file was created.\n\n")

cat("========================================\n")
cat("TEST 4: unrecognized function name errors clearly\n")
cat("========================================\n")

err4 <- tryCatch({
  batz.generate_plotopts(generate.files = c("batz.plotcover_bullseye", "batz.generate_plotframe.bat"),
                          dir.load = getwd(), dir.save = tempdir())
  NULL
}, error = function(e) conditionMessage(e))

stopifnot(!is.null(err4))
stopifnot(grepl("unrecognized", err4, ignore.case = TRUE))
stopifnot(grepl("batz.generate_plotframe.bat", err4, fixed = TRUE))
cat("PASS: unrecognized name (batz.generate_plotframe.bat - confirmed not a plot function) errors clearly:\n  ", err4, "\n\n")

cat("========================================\n")
cat("TEST 5: missing master file errors clearly\n")
cat("========================================\n")

empty.dir <- file.path(tempdir(), "gp_test5_empty")
if (!dir.exists(empty.dir)) dir.create(empty.dir, recursive = TRUE)

err5 <- tryCatch({
  batz.generate_plotopts(generate.files = "batz.plotcover_bullseye",
                          dir.load = empty.dir, dir.save = tempdir())
  NULL
}, error = function(e) conditionMessage(e))

stopifnot(!is.null(err5))
stopifnot(grepl("not found", err5, ignore.case = TRUE))
cat("PASS: missing master file errors clearly:\n  ", err5, "\n\n")

cat("========================================\n")
cat("TEST 6: dir.save is created if it doesn't already exist\n")
cat("========================================\n")

out.dir6 <- file.path(tempdir(), "gp_test6_new", "nested")
if (dir.exists(file.path(tempdir(), "gp_test6_new"))) unlink(file.path(tempdir(), "gp_test6_new"), recursive = TRUE)
stopifnot(!dir.exists(out.dir6))

saved6 <- batz.generate_plotopts(generate.files = "batz.plotactivity_observations",
                                  project.name = "newdir.test",
                                  dir.load = getwd(),
                                  dir.save = out.dir6)

stopifnot(dir.exists(out.dir6))
stopifnot(file.exists(file.path(out.dir6, saved6)))
cat("PASS: dir.save (including nested, non-existent path) is created automatically.\n\n")

cat("========================================\n")
cat("TEST 7: default project.name is the literal \"new project\" (per Josh's own spec text)\n")
cat("========================================\n")

out.dir7 <- file.path(tempdir(), "gp_test7")
if (dir.exists(out.dir7)) unlink(out.dir7, recursive = TRUE)

saved7 <- batz.generate_plotopts(generate.files = "batz.plotdetections_first.last",
                                  dir.load = getwd(),
                                  dir.save = out.dir7)

stopifnot(saved7 == "new project_plotopts_batz.plotdetections_first.last.csv")
cat("PASS: default project.name (\"new project\") is used verbatim as the file-name prefix:", saved7, "\n\n")

## Cleanup
unlink(out.dir1, recursive = TRUE)
unlink(out.dir3, recursive = TRUE)
unlink(file.path(tempdir(), "gp_test6_new"), recursive = TRUE)
unlink(out.dir7, recursive = TRUE)
unlink(empty.dir, recursive = TRUE)

cat("========================================\n")
cat("ALL TESTS PASSED\n")
cat("========================================\n")

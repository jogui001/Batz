# =============================================================================
# batz.generate_plotopts.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_plotopts() (Batz package).
# Writes <project.name>_plotopts_<function>.csv from the built-in settings
# (Batz_reference_db PLOTOPTS_* tabs, carried by the package).
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "plotopts_test"); dir.create(dir.out, showWarnings = FALSE)

## 1. Every plot function
f <- batz.generate_plotopts(project.name = "acme.2026", dir.save = dir.out)
print(f)                                                    # 11 files
x <- read.csv(file.path(dir.out, f[1]), check.names = FALSE)
stopifnot(identical(names(x), c("category", "parameter", "default.value", "override.value", "notes")))

## 2. One function; the file matches the settings the function uses without aes.default
f2 <- batz.generate_plotopts("batz.plotcover_bullseye", project.name = "acme.2026", dir.save = dir.out)
y <- read.csv(file.path(dir.out, f2), check.names = FALSE, colClasses = "character", na.strings = character(0))
stopifnot(identical(y$parameter, Batz:::batz.db("plotopts.batz.plotcover_bullseye")$parameter))

## 3. A name with no built-in settings stops with the list of names
try(batz.generate_plotopts("batz.plot_weather.daily", dir.save = dir.out))

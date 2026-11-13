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
print(f)                                                    # 12 files (batz.plot.line_weather.daily included)
x <- read.csv(file.path(dir.out, f[1]), check.names = FALSE)
## the override.value column is named after the project (2026-11-10)
stopifnot(identical(names(x), c("category", "parameter", "default.value", "acme.2026", "notes")))

## 2. One function; the file matches the settings the function uses without aes.plotopts
f2 <- batz.generate_plotopts("batz.plot.bullseye_clutter.mic", project.name = "acme.2026", dir.save = dir.out)
y <- read.csv(file.path(dir.out, f2), check.names = FALSE, colClasses = "character", na.strings = character(0))
stopifnot(identical(y$parameter, Batz:::batz.db("plotopts.batz.plot.bullseye_clutter.mic")$parameter))

## 3. A name with no built-in settings stops with the list of names
try(batz.generate_plotopts("batz.not_a_plot", dir.save = dir.out))

## 4. project.name = "": the column keeps the name override.value; file plotopts_<function>.csv
f4 <- batz.generate_plotopts("batz.plot.heatmap_aru.time", project.name = "", dir.save = dir.out)
stopifnot(f4 == "plotopts_batz.plot.heatmap_aru.time.csv",
          "override.value" %in% names(read.csv(file.path(dir.out, f4), check.names = FALSE)))

## 5. Use the project file: aes.plotopts + aes.style = the project name
# po <- read.csv(file.path(dir.out, "acme.2026_plotopts_batz.plot.heatmap_aru.time.csv"), check.names = FALSE)
# po$acme.2026[po$parameter == "dawn.color"] <- "orange"
# batz.plot.heatmap_aru.time(plotframe, fig.list, aes.plotopts = po, aes.style = "acme.2026")

## 6. batz.plot.line_weather.daily now has built-in settings (aes.plotopts / aes.style)
w <- read.csv(file.path(dir.out, "acme.2026_plotopts_batz.plot.line_weather.daily.csv"), check.names = FALSE)
print(w[, 1:3])
w$acme.2026[w$parameter == "temp.range"] <- "5;30"
print(Batz:::weather.daily.plotopts(w, "acme.2026", given = character(0), "batz.plot.line_weather.daily"))   # $temp.range 5 30

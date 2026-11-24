# =============================================================================
# batz.generate_plotopts.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_plotopts() (Batz package).
# Writes <project.name>_pltop.<function without batz.plot.>.csv from the built-in settings
# (Batz_reference_db pltop.* tabs, carried by the package).
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "plotopts_test"); dir.create(dir.out, showWarnings = FALSE)

## 1. Every plot function
f <- batz.generate_plotopts(project.name = "acme.2026", dir.save = dir.out)
print(f)                                                    # 13 CSVs (every batz.plot function), then acme.2026_pltop.pooled.xlsx
stopifnot(length(f) == 14, f[14] == "acme.2026_pltop.pooled.xlsx")
x <- read.csv(file.path(dir.out, f[1]), check.names = FALSE)
## the override.value column is named after the project (2026-11-10)
stopifnot(identical(names(x), c("category", "parameter", "default.value", "acme.2026", "notes")))

## 2. One function; the file matches the settings the function uses without aes.plotopts
f2 <- batz.generate_plotopts(functions = "batz.plot.bullseye_clutter.mic", project.name = "acme.2026", dir.save = dir.out)
y <- read.csv(file.path(dir.out, f2[1]), check.names = FALSE, colClasses = "character", na.strings = character(0))
stopifnot(identical(y$parameter, Batz:::batz.db("plotopts.batz.plot.bullseye_clutter.mic")$parameter))

## 3. A name with no built-in settings stops with the list of names
try(batz.generate_plotopts(functions = "batz.not_a_plot", dir.save = dir.out))

## 4. project.name = "": the column keeps the name override.value; file pltop.<function without batz.plot.>.csv
f4 <- batz.generate_plotopts(functions = "batz.plot.heatmap_aru.time", project.name = "", dir.save = dir.out)
stopifnot(identical(f4, c("pltop.heatmap_aru.time.csv", "pltop.pooled.xlsx")),
          "override.value" %in% names(read.csv(file.path(dir.out, f4[1]), check.names = FALSE)))

## 5. Use the project file: aes.plotopts + aes.style = the project name
# po <- read.csv(file.path(dir.out, "acme.2026_pltop.heatmap_aru.time.csv"), check.names = FALSE)
# po$acme.2026[po$parameter == "dawn.color"] <- "orange"
# batz.plot.heatmap_aru.time(plotframe, fig.list, aes.plotopts = po, aes.style = "acme.2026")

## 6. batz.plot.line_weather.daily now has built-in settings (aes.plotopts / aes.style)
w <- read.csv(file.path(dir.out, "acme.2026_pltop.line_weather.daily.csv"), check.names = FALSE)
print(w[, 1:3])
w$acme.2026[w$parameter == "temp.range"] <- "5;30"
print(Batz:::weather.daily.plotopts(w, "acme.2026", given = character(0), "batz.plot.line_weather.daily"))   # $temp.range 5 30


## 7. Pooled workbook: one pltop.<function> tab per function, same cells as the CSVs
f7 <- batz.generate_plotopts(project.name = "acme.2026", file.format = "xlsx", dir.save = dir.out)
stopifnot(identical(f7, "acme.2026_pltop.pooled.xlsx"))
tabs <- readxl::excel_sheets(file.path(dir.out, f7)); print(tabs)
stopifnot(length(tabs) == 13, all(grepl("^pltop\\.", tabs)), all(nchar(tabs) <= 31))
xl <- as.data.frame(readxl::read_excel(file.path(dir.out, f7), "pltop.heatmap_aru.time", col_types = "text"))
xl[is.na(xl)] <- ""
cs <- read.csv(file.path(dir.out, "acme.2026_pltop.heatmap_aru.time.csv"), check.names = FALSE, colClasses = "character", na.strings = character(0))
stopifnot(identical(xl$parameter, cs$parameter), identical(xl$default.value, cs$default.value))
## the plot functions read their own tab from the pooled workbook (see the heatmap_aru.time dev script, test 12)
print(head(Batz:::plotutil_read_plotopts(file.path(dir.out, f7), "batz.plot.bar_activity.site")[, 1:3]))
try(batz.generate_plotopts(file.format = "pdf", dir.save = dir.out))      # error: csv / xlsx only

## 8. functions: "batz." / "batz.plot." optional, case ignored; old name generate.files still read (2026-11-24)
f8 <- batz.generate_plotopts(functions = c("plot.bar_activity.site", "heatmap_aru.time", "Batz.Plot.Line_Weather.Average"),
                             project.name = "", file.format = "csv", dir.save = dir.out)
stopifnot(identical(f8, c("pltop.bar_activity.site.csv", "pltop.heatmap_aru.time.csv", "pltop.line_weather.average.csv")))
out <- capture.output(f9 <- batz.generate_plotopts(generate.files = "bullseye_clutter.mic", project.name = "", file.format = "csv", dir.save = dir.out))
stopifnot(identical(f9, "pltop.bullseye_clutter.mic.csv"), any(grepl("WARNING.*'generate.files' has been renamed functions - used as functions", out)))
e <- try(batz.generate_plotopts(functions = "vegetation", dir.save = dir.out), silent = TRUE)
stopifnot(grepl("matches several", e))                     # bar_vegetation.species, box_vegetation.dbh
e <- try(batz.generate_plotopts(functions = "heatmap_aru.time", plot = TRUE, dir.save = dir.out), silent = TRUE)
stopifnot(grepl("unknown argument\\(s\\): plot", e))
cat("\nALL TESTS PASSED\n")

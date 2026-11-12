# =============================================================================
# batz.generate_figlist.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_figlist() (Batz package).
# Writes the fig.list template workbook (tab "fig.list": every fig.list
# heading, one starter row per plot function; tab "descriptions": what each
# heading does and which functions need or use it).
# Run the whole script; the workbook is saved in dir.out.
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "figlist_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## 1. All plot functions that take a fig.list
f <- batz.generate_figlist(dir.save = dir.out)
print(basename(f$file))                                   # fig.list_template_<timestamp>.xlsx
print(names(f$fig.list))
print(f$fig.list$plot.type)                               # one starter row per plot function
View(f$descriptions)
ok(identical(openxlsx::getSheetNames(f$file), c("fig.list", "descriptions")), "two tabs: fig.list, descriptions")
ok(all(nzchar(f$descriptions$description)), "every heading has a description")
ok(identical(names(f$descriptions), c("header", "required.in", "optional.in", "description")), "descriptions headers")
## $plot first, every starter row FALSE (a model row), headings grouped as in Batz_reference_db FIGLIST_all (2026-11-09)
ok(identical(names(f$fig.list)[1:3], c("plot", "plot.type", "plot.name")) && all(f$fig.list$plot == "FALSE"), "$plot first, model rows FALSE")
print(names(f$fig.list))   # plot, plot.type, plot.name | data: plot.set ... pool | facets | columns / time | X axis | Y axis | Y right | legend / bins

## 2. Only some functions; project name in the file name
f2 <- batz.generate_figlist(functions = c("batz.plotsm4_heatmap", "batz.plotdetections_first.last"),
                            project.name = "Bristol NH", dir.save = dir.out)
print(basename(f2$file))                                  # Bristol-NH_fig.list_template_<timestamp>.xlsx
ok(nrow(f2$fig.list) == 2, "one row per chosen function")

## 3. The template works as a fig.list: fill a row, the other rows are skipped by each function
fl <- openxlsx::read.xlsx(f$file, "fig.list", check.names = FALSE, sep.names = ".")
fl[is.na(fl)] <- ""
i <- fl$plot.type == "sm4.heatmap"
fl$plot.name[i] <- "my.site"; fl$plot.set[i] <- "my.site"            # ... fill in the rest, then:
# batz.plotsm4_heatmap(sm4, fl, suntimes, plotopts)

## example entries in every heading a plot type uses, "NA" in the others; $facet.label_format and $facet.label (2026-11-09)
ok(!any(as.matrix(f$fig.list) == ""), "every used heading has an example entry")
ok(all(c("facet.label_format", "facet.label") %in% names(f$fig.list)), "$facet.label_format and $facet.label headings")
print(f$fig.list[, c("plot.type", "facet.label_format", "facet.label")])

## functions = "" : every plot type (2026-11-09)
f3 <- batz.generate_figlist(functions = "", dir.save = NULL)
ok(nrow(f3$fig.list) == nrow(f$fig.list) && identical(f3$fig.list$plot.type, f$fig.list$plot.type), "functions = \"\": all plot types")

## $plot.dimensions: a heading for every plot type, right after $plot.name, with an example size (2026-11-09)
ok(identical(names(f$fig.list)[match("plot.name", names(f$fig.list)) + 1], "plot.dimensions"), "$plot.dimensions after $plot.name")
ok(all(f$fig.list$plot.dimensions != "NA"), "every plot type has an example $plot.dimensions")
ok(all(f$fig.list$plot.zero == "FALSE"), "every plot type has $plot.zero = FALSE")
print(f$fig.list[, c("plot.type", "plot.dimensions")])

try(batz.generate_figlist(functions = "batz.not_a_function", dir.save = NULL))   # error names it
cat("Workbook in:", dir.out, "\n")

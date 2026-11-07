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

## 1b. Same headings, same order, for every plot type; "NA" = not used by that plot type
print(t(f$fig.list))
ok(all(f$fig.list[f$fig.list$plot.type == "sm4.heatmap", c("facet.set", "myso", "bin.colors")] == "NA"), "sm4.heatmap row: unused headings are NA")
ok(all(f$fig.list[f$fig.list$plot.type == "sm4.heatmap", c("plot.name", "plot.set", "date.start")] == ""), "sm4.heatmap row: its own headings are blank")

## 2. Only some functions; project name in the file name
f2 <- batz.generate_figlist(functions = c("batz.plotsm4_heatmap", "batz.plotdetections_first.last"),
                            project.name = "Bristol NH", dir.save = dir.out)
print(basename(f2$file))                                  # Bristol-NH_fig.list_template_<timestamp>.xlsx
ok(nrow(f2$fig.list) == 2, "one row per chosen function")

## 3. The template works as a fig.list: fill a row, the other rows are skipped by each function,
##    and "NA" cells are treated as blank (the setting then comes from plotopts)
fl <- openxlsx::read.xlsx(f$file, "fig.list", check.names = FALSE, sep.names = ".")
fl[is.na(fl)] <- ""
i <- fl$plot.type == "sm4.heatmap"
fl$plot.name[i] <- "my.site"; fl$plot.set[i] <- "my.site"            # ... fill in the rest, then:
# batz.plotsm4_heatmap(sm4, fl, suntimes, plotopts)

try(batz.generate_figlist(functions = "batz.not_a_function", dir.save = NULL))   # error names it
cat("Workbook in:", dir.out, "\n")

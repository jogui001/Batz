# =============================================================================
# batz.plot.box_vegetation.dbh.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot.box_vegetation.dbh() (Batz package).
# Test data: the merged Survey123 workbook (made by
# batz.merge_sitevisit.acoustics(), quad tab, $...dbh.overall columns) and a
# small made-up quad tab shaped like the example plot (Falmouth1/2).
# Set merged.file to your merged workbook, then run the whole script.
# =============================================================================

library(Batz)

merged.file <- "C:/path/to/mergedsurvey123.xlsx"          # <- edit
dir.out <- file.path(tempdir(), "vegetation_dbh_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## ---- example-like data: Falmouth1 trees only, Falmouth2 trees and snags ------
q <- data.frame(project.name = "Falmouth", aru.name = rep(c("Falmouth1", "Falmouth2"), each = 5), measurement.units = "in",
  vegetation.tree_tree1.dbh = c("13.1", "13.5", "13.6", "15.5", "17.5", "4.5", "7", "9", "12", "10;9.3"),
  vegetation.snag_snag1.dbh = c(NA, NA, NA, NA, NA, 3.4, 3.5, 4.3, 6, 9.6))

## 1. Defaults: snag grey / tree white, inches, legend right, page size
r1 <- batz.plot.box_vegetation.dbh(q, project.name = "Falmouth", dir.save = dir.out,
                               tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")
print(r1$summary)
print(r1$plot)
ok(isTRUE(all.equal(max(r1$dbh$dbh[r1$dbh$site == "Falmouth2" & r1$dbh$type == "Tree"]), 13.656)),
   "a two-trunk tree (10;9.3) uses sqrt(10^2 + 9.3^2) = 13.656")

## 1b. table = TRUE (default): the summary is saved as a CSV next to the PNG
print(read.csv(r1$file.table))
ok(identical(names(read.csv(r1$file.table)), c("site", "type", "n", "min", "median", "mean", "max")), "table CSV headers")
r1b <- batz.plot.box_vegetation.dbh(q, table = FALSE, dir.save = dir.out,
                                tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")
ok(is.na(r1b$file.table), "table = FALSE: no CSV")

## 2. Same as the example plot: powerpoint size (12 x 6 in)
r2 <- batz.plot.box_vegetation.dbh(q, project.name = "Falmouth", size = "powerpoint", dir.save = dir.out,
                               tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")

## 3. Centimetres, own colors, every tree / snag as a dot, legend at the bottom
r3 <- batz.plot.box_vegetation.dbh(q, units = "cm", type.colors = "Snag=tan;Tree=darkseagreen", points.show = TRUE,
                               legend.position = "bottom", dir.save = dir.out,
                               tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")
ok(isTRUE(all.equal(max(r3$dbh$dbh), round(17.5 * 2.54, 3))), "inches converted to cm")
ok(r3$plot$labels$y == "Diameter at Breast Height (cm)", "Y title follows units")

## 4. Settings order: call > plotopts > built-in default
po <- batz.plot.box_vegetation.dbh.defaults()
po$override.value[po$parameter == "legend.position"] <- "top"
po$override.value[po$parameter == "snag.include"] <- "FALSE"
r4 <- batz.plot.box_vegetation.dbh(q, plotopts = po, legend.position = "left", dir.save = NULL,
                               tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")
print(r4$settings[r4$settings$source != "default", ])
ok(r4$plot$theme$legend.position == "left" && !"Snag" %in% r4$dbh$type, "call beats plotopts; plotopts beats default")

## 5. Starting plotopts file (edit it, then pass it as plotopts = "<file>.csv")
write.csv(batz.plot.box_vegetation.dbh.defaults(), file.path(dir.out, "plotopts_vegetation.dbh.csv"), row.names = FALSE)

## 6. Real data: the merged Survey123 workbook (quad tab, .overall columns)
if (file.exists(merged.file)) {
  r6 <- batz.plot.box_vegetation.dbh(merged.file, dir.save = dir.out)
  print(r6$summary)
}
cat("PNGs in:", dir.out, "\n")

## Help page lists plot.zero (2026-11-13)
# ?batz.plot.box_vegetation.dbh -> Details settings list includes plot.zero, axis/legend title size and face
stopifnot(any(grepl("plot.zero", capture.output(tools::Rd2txt(utils:::.getHelpFile(help("batz.plot.box_vegetation.dbh", package = "Batz")))))))
d0 <- batz.plot.box_vegetation.dbh(q, dir.save = NULL); d1 <- batz.plot.box_vegetation.dbh(q, dir.save = NULL, plot.zero = TRUE)
cat("plot.zero dbh rows drawn: FALSE =", nrow(ggplot2::layer_data(d0$plot, 1)), " TRUE =", nrow(ggplot2::layer_data(d1$plot, 1)), "\n")

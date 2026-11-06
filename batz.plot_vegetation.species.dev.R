# =============================================================================
# batz.plot_vegetation.species.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot_vegetation.species() (Batz package).
# Test data: mergedsurvey123.xlsx (made by batz.merge_sitevisit.acoustics())
# and a small made-up quad tab that matches the example plot (Falmouth1/2).
# Set merged.file to your merged workbook, then run the whole script.
# =============================================================================

library(Batz)

merged.file <- "C:/path/to/mergedsurvey123.xlsx"          # <- edit
dir.out <- file.path(tempdir(), "vegetation_species_test"); dir.create(dir.out, showWarnings = FALSE)

## ---- example-like data (two sites) -------------------------------------------
q <- data.frame(project.name = "Falmouth", aru.name = rep(c("Falmouth1", "Falmouth2"), c(4, 8)),
  vegetation.tree_tree1.species = c("Birch", "Oak", "Spruce", NA, "Maple", "Maple", "Maple", "Oak", "Oak", "Maple", "Maple", "Maple"),
  vegetation.tree_tree2.species = c(NA, NA, NA, NA, "Maple", "Maple", "Maple", NA, NA, NA, NA, NA),
  vegetation.tree_tree3.species = NA,
  vegetation.sapling_species1 = c(NA, NA, NA, NA, "Hemlock", "Maple", "red spruce", "Red Spruce", "Red Spruce", "Red spruce", NA, NA),
  vegetation.sapling_species2 = NA, vegetation.sapling_species3 = NA,
  vegetation.snag_snag1.dbh = c(NA, NA, NA, NA, 10, 12, 8, NA, NA, NA, NA, NA),
  vegetation.snag_snag2.dbh = c(NA, NA, NA, NA, 5, 6, NA, NA, NA, NA, NA, NA),
  vegetation.snag_snag3.dbh = NA)

## 1. Defaults: stacked, legend right, page size (6 x 7 in), black / grey
r1 <- batz.plot_vegetation.species(q, project.name = "Falmouth", dir.save = dir.out)
print(r1$counts)                               # Red Spruce = 4 (case ignored), Snag = 5
print(r1$plot)

## 2. Same as the example plot: powerpoint size (12 x 6 in)
r2 <- batz.plot_vegetation.species(q, project.name = "Falmouth", size = "powerpoint", dir.save = dir.out)

## 3. Grouped bars, legend at the bottom, site=color pairs, half page
r3 <- batz.plot_vegetation.species(q, project.name = "Falmouth", bar = "grouped", legend.position = "bottom",
                                   site.colors = "Falmouth1=darkgreen;Falmouth2=orange", size = "half page",
                                   dir.save = dir.out)
print(r3$plot)
## grouped: every site has its place under every species; a short line at 0 where a site has none
pdat <- r3$plot$data
stopifnot(nrow(pdat) == 2 * length(unique(pdat$species)) * 2,                 # 2 panels x species x 2 sites
          any(pdat$count == 0))
r3b <- batz.plot_vegetation.species(q, project.name = "Falmouth", bar = "grouped", dir.save = NULL,
                                    plotopts = data.frame(parameter = "zero.show", default.value = "FALSE"))   # no zero lines

## 4. Settings priority: function input > plotopts > built-in default
po <- batz.plot_vegetation.species.defaults()
po$override.value[po$parameter == "bar"] <- "grouped"
po$override.value[po$parameter == "legend.position"] <- "top"
r4 <- batz.plot_vegetation.species(q, bar = "stacked", plotopts = po, dir.save = NULL)
print(r4$settings[r4$settings$parameter %in% c("bar", "legend.position", "snag.include"), ])
stopifnot(r4$settings$value[r4$settings$parameter == "bar"] == "stacked",          # input wins
          r4$settings$value[r4$settings$parameter == "legend.position"] == "top")  # plotopts wins over default

## 5. One site, no snags, a chosen order of sites
r5 <- batz.plot_vegetation.species(q, site.names = c("Falmouth2", "Falmouth1"), dir.save = NULL,
                                   plotopts = data.frame(parameter = "snag.include", default.value = "FALSE"))
stopifnot(!"Snag" %in% r5$counts$species)

## 5b. DBH = TRUE (default): mean DBH printed above each tree / snag bar
q$vegetation.tree_tree1.dbh <- c("8", "14", "20;5", NA, "10", "12", "9;4", "16", "18", "7", "11", "13")
q$vegetation.tree_tree2.dbh <- c(NA, NA, NA, NA, "6", "8", "5", NA, NA, NA, NA, NA)
q$measurement.units <- "in"
r5b <- batz.plot_vegetation.species(q, project.name = "Falmouth", dir.save = dir.out)                     # per species
print(r5b$dbh.mean)
stopifnot(r5b$dbh.mean$dbh.mean[r5b$dbh.mean$species == "Spruce"] == 20.6,                             # sqrt(20^2 + 5^2)
          r5b$dbh.mean$dbh.mean[r5b$dbh.mean$species == "Snag"] == 8.2)                                 # mean of 10,12,8,5,6
r5c <- batz.plot_vegetation.species(q, project.name = "Falmouth", bar = "grouped", dir.save = dir.out)  # per species and site
print(r5c$dbh.mean)
r5d <- batz.plot_vegetation.species(q, DBH = FALSE, dir.save = NULL); stopifnot(is.null(r5d$dbh.mean))

## 6. Starting plotopts file (edit it, then pass it as plotopts = "<file>.csv")
write.csv(batz.plot_vegetation.species.defaults(), file.path(dir.out, "plotopts_vegetation.species.csv"), row.names = FALSE)

## 7. Real data: the merged Survey123 workbook (quad tab)
if (file.exists(merged.file)) {
  r7 <- batz.plot_vegetation.species(merged.file, project.name = "Bristol NH", dir.save = dir.out)
  print(r7$counts)
}
cat("PNGs in:", dir.out, "\n")

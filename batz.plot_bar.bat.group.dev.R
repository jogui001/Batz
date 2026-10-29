## batz.plot_bar.bat.group.dev.R
## Dev/test script for batz.plot_bar.bat.group() - added 2026-10-21, per Josh.
##
## KEEP THIS FILE OUTSIDE THE PACKAGE'S R/ FOLDER - it has top-level code that
## reads test-data paths and would break devtools::load_all()/document().
##
## Model plot: Otomops harrisoni community bar plot, Kajiado (South) and NKRCP
## (North) panels, from South_North_combined_vetted_1.csv via
## batz.generate_plotframe.bat.group().

## ---- CONFIG - point these at your own copies ---------------------------------
dir.pkg       <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz/R"
file.data     <- "South_North_combined_vetted_1.csv"
file.fig.list <- "fig.list_bar.bat.group.csv"
file.plotopts <- "plotopts_bar.bat.group.csv"
dir.out       <- tempdir()

for (f in c("batz.util_standardize.headers.R", "batz.util_special.characters.R",
            "batz.generate_plotframe.bat.group.R", "batz.plot_bar.bat.group.R")) {
  source(file.path(dir.pkg, f))
}
vetted   <- read.csv(file.data, check.names = FALSE, stringsAsFactors = FALSE)
fig.list <- read.csv(file.fig.list, check.names = FALSE, stringsAsFactors = FALSE)
plotopts <- read.csv(file.plotopts, check.names = FALSE, stringsAsFactors = FALSE)
plfr <- suppressWarnings(batz.generate_plotframe.bat.group(vetted, groupby.date = "date-12",
                                                           write.output = FALSE))
plfr.copy <- plfr
pass <- function(name) cat("PASS -", name, "\n")
run <- function(fl = fig.list, po = plotopts, d = plfr, ...) {
  batz.plot_bar.bat.group(d, fl, po, dir.save = dir.out, ...)
}

## ---- TEST 1: model plot ---------------------------------------------------------
res <- run(project.name = "south.north")
b <- res$plots[[1]]$bars
stopifnot(length(res$ggplots) == 1, file.exists(file.path(dir.out, res$plots[[1]]$file)))
stopifnot(identical(b$community, c("AL", "ES", "MAI", "OLG", "OLK", "OLMN", "OY", "RW", "SH",
                                   "IL", "KA", "ME", "SE", "WG")))
stopifnot(identical(unique(b$group), c("South", "North")), identical(plfr, plfr.copy))
pass("model plot: 14 communities, South (Kajiado) then North (NKRCP); input unchanged")

## ---- TEST 2: values = bat.group table (year 'all') ---------------------------------
o <- plfr[plfr$species == "Otomops harrisoni" & plfr$year == "all", ]
i <- match(b$community, o$community)
stopifnot(all(b$sites_sampled == o$sites_sampled[i]), all(b$sites_detected == o$sites_detected[i]))
stopifnot(isTRUE(all.equal(b$passes.per.night,
                           ifelse(o$date.monitoringnight_sampled[i] > 0,
                                  o$observation_count[i] / o$date.monitoringnight_sampled[i], 0))))
stopifnot(abs(b$passes.per.night[b$community == "OLMN"] - 1924 / 7) < 1e-9,
          b$passes.per.night[b$community == "OLK"] == 0)
pass("sites sampled/detected and passes per night (OLMN 1924/7, OLK 0) match the bat.group table")

## ---- TEST 3: community colors = the October plot ---------------------------------
cols <- setNames(b$color, b$community)
stopifnot(cols[["AL"]] == "#A6CEE3", cols[["OLMN"]] == "#FDBF6F", cols[["KA"]] == "#D95F02",
          cols[["WG"]] == "#66A61E")
pass("community colors match the colors read from the October plot")

## ---- TEST 4: fig.list options -------------------------------------------------------
fl4 <- fig.list; fl4$plot.set <- "North"; fl4$community.exclude <- "SE"; fl4$plot.year <- "2025"
b4 <- run(fl4)$plots[[1]]$bars
stopifnot(identical(b4$community, c("IL", "KA", "ME", "WG")))
o25 <- plfr[plfr$species == "Otomops harrisoni" & plfr$year == "2025" & plfr$community == "KA", ]
stopifnot(b4$sites_sampled[b4$community == "KA"] == o25$sites_sampled)
fl4b <- fig.list; fl4b$plot.group <- ""; fl4b$plot.set <- "OLMN;KA"; fl4b$passes.per <- "detected"
b4b <- run(fl4b)$plots[[1]]$bars
stopifnot(nrow(b4b) == 2, abs(b4b$passes.per.night[b4b$community == "OLMN"] - 1924 / 6) < 1e-9)
pass("plot.set / community.exclude / plot.year; no plot.group = community list; passes.per = detected via fig.list")

## ---- TEST 5: snake_case input, missing headers, other plot types -------------------
p5 <- plfr; names(p5) <- standardize.headers(names(p5))
stopifnot(identical(run(d = p5)$plots[[1]]$bars, b))
e <- tryCatch(run(d = plfr[, setdiff(names(plfr), "sites_detected")],
                  fl = fig.list[, setdiff(names(fig.list), "plot.species")]), error = conditionMessage)
stopifnot(grepl("data is missing these headers: sites_detected", e),
          grepl("fig.list is missing these headers: plot.species", e), grepl("\n\n", e))
fl5 <- rbind(fig.list, fig.list); fl5$plot.type[2] <- "heat.site.species"; fl5$plot.name[2] <- "x"
stopifnot(length(run(fl5)$plots) == 1)
pass("snake_case data accepted; missing headers reported together; other plot types skipped")

## ---- TEST 6: overide.value and unknown community color --------------------------------
po6 <- plotopts
po6$overide.value[po6$parameter == "community.colors"] <- "AL=#000000"
b6 <- run(po = po6)$plots[[1]]$bars
stopifnot(b6$color[b6$community == "AL"] == "#000000", all(nzchar(b6$color)))
pass("$overide.value used; communities missing from community.colors get default colors")

cat("\nAll tests passed. Plots saved in:", dir.out, "\n")

## batz.plot_heat.site.species.dev.R
## Dev/test script for batz.plot_heat.site.species() - added 2026-10-16, per Josh.
##
## KEEP THIS FILE OUTSIDE THE PACKAGE'S R/ FOLDER - it has top-level code that
## reads test-data paths and would break devtools::load_all()/document().
##
## Model data: South_North_combined_vetted_1.csv (64,028 records, South + North
## projects, 14 communities). Model plot: Otomops harrisoni, all OLMN (Kajiado)
## and KA (NKRCP) sites, KABC_06/KABC_07 left out, OLMN3 and KA07 in bold.

## ---- CONFIG - point these at your own copies ---------------------------------
dir.pkg       <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz/R"
file.data     <- "South_North_combined_vetted_1.csv"
file.fig.list <- "fig.list_heat.site.species.csv"
file.plotopts <- "plotopts_heat.site.species.csv"
dir.out       <- tempdir()

for (f in c("batz.util_standardize.headers.R", "batz.util_special.characters.R",
            "batz.plot_heat.site.species.R")) {
  source(file.path(dir.pkg, f))
}

vetted   <- read.csv(file.data, check.names = FALSE, stringsAsFactors = FALSE)
fig.list <- read.csv(file.fig.list, check.names = FALSE, stringsAsFactors = FALSE)
plotopts <- read.csv(file.plotopts, check.names = FALSE, stringsAsFactors = FALSE)
vetted.copy <- vetted

pass <- function(name) cat("PASS -", name, "\n")
run <- function(fl = fig.list, po = plotopts, d = vetted, ...) {
  batz.plot_heat.site.species(d, fl, po, groupby.date = "date-12", dir.save = dir.out, ...)
}

## ---- TEST 1: model plot on real data ---------------------------------------
res <- run(project.name = "south.north")
p <- res$plots[[1]]
stopifnot(length(res$ggplots) == 1, file.exists(file.path(dir.out, p$file)))
stopifnot(nrow(p$sites) == 14, !any(c("KABC_06", "KABC_07") %in% p$sites$aru.name))
stopifnot(identical(p$sites$aru.name, c("OLMN3", "OLMN7", "OLMN9", "OLMN13", "OLMN17", "OLMN20",
                                        "OLMN29", "KA01", "KA02", "KA03", "KA04", "KA05", "KA06", "KA07")))
stopifnot(nrow(p$cells) == 14 * 33)                         # every site x bin cell
stopifnot(all(table(p$cells$value.class) == c(318, 69, 55, 15, 4, 1)))
top <- p$cells[which.max(p$cells$value), ]
stopifnot(top$aru.name == "KA07", top$bin.start == "21:40", top$passes == 167)
stopifnot(identical(vetted, vetted.copy))
pass("model plot: 14 sites in order, 33 x 20-min bins, class counts and max cell (KA07 21:40 = 167) as before; input unchanged")

## ---- TEST 2: cells match an independent count --------------------------------
v <- unique(vetted)
t <- as.POSIXct(v$time, format = "%H:%M:%S", tz = "UTC")
m <- (as.numeric(format(t, "%H")) * 60 + as.numeric(format(t, "%M")) - 19 * 60) %% 1440
for (i in sample(which(p$cells$passes > 0), 25)) {
  r <- p$cells[i, ]
  n <- sum(v$aru.name == r$aru.name & v$species == "Otomops harrisoni" &
           m >= r$bin * 20 & m < (r$bin + 1) * 20)
  stopifnot(n == r$passes)
}
pass("25 random non-zero cells match a direct count")

## ---- TEST 3: mean per night with multi-night sites (KABC kept) ---------------
fl3 <- fig.list; fl3$site.exclude <- ""
p3 <- run(fl3)$plots[[1]]
kb <- p3$sites[p3$sites$aru.name == "KABC_06", ]
stopifnot(kb$nights == 3, all.equal(p3$cells$value, p3$cells$passes / p3$cells$nights))
stopifnot(any(p3$cells$value > 0 & p3$cells$value < 1 & p3$cells$value.class == "1-10"))
fl3$value.type <- "total"                                    # fig.list overrides aes.default
p3t <- run(fl3)$plots[[1]]
stopifnot(identical(p3t$cells$value, as.numeric(p3t$cells$passes)))
pass("mean.night = passes / nights (KABC_06: 3 nights, fractions go to 1-10); value.type = total via fig.list")

## ---- TEST 4: header spellings accepted ---------------------------------------
v4 <- vetted
names(v4)[names(v4) == "aru.name"] <- "ARU Name"
names(v4)[names(v4) == "species"]  <- "Species"
r4 <- run(d = v4)
stopifnot(identical(r4$plots[[1]]$cells, p$cells))
pass("'ARU Name' / 'Species' / 'date-12' accepted, identical result")

## ---- TEST 5: missing headers - all inputs reported, blank line between -----
e <- tryCatch(run(d = vetted[, setdiff(names(vetted), "time")],
                  fl = fig.list[, setdiff(names(fig.list), "plot.species")]),
              error = conditionMessage)
stopifnot(grepl("data is missing these headers: time", e),
          grepl("fig.list is missing these headers: plot.species", e), grepl("\n\n", e))
po5 <- plotopts[plotopts$parameter != "bin.minutes", ]
e <- tryCatch(run(po = po5), error = conditionMessage)
stopifnot(grepl("missing these required \\$parameter rows: bin.minutes", e))
pass("missing data/fig.list headers and aes.default parameter rows stop with clear messages")

## ---- TEST 6: no plot.group -> plot.set lists sites, one block ----------------
fl6 <- fig.list
fl6$plot.group <- ""; fl6$plot.set <- "KA07;OLMN3;KA04"; fl6$plot.name <- "three.sites"
p6 <- run(fl6)$plots[[1]]
stopifnot(identical(p6$sites$aru.name, c("KA04", "KA07", "OLMN3")), all(p6$sites$group == ""))
pass("blank $plot.group: $plot.set = site list, single block")

## ---- TEST 7: bin width / window overrides -------------------------------------
fl7 <- fig.list; fl7$bin.minutes <- 30; fl7$time.start <- "18:00"; fl7$time.end <- "06:00"
p7 <- run(fl7)$plots[[1]]
stopifnot(p7$settings$n.bins == 24, max(p7$cells$bin) == 23, p7$cells$bin.start[1] == "18:00")
stopifnot(sum(p7$cells$passes) >= sum(p$cells$passes))
pass("bin.minutes = 30 and 18:00-06:00 from fig.list: 24 bins")

## ---- TEST 8: other plot types skipped; bad bin.colors falls back -------------
fl8 <- rbind(fig.list, fig.list); fl8$plot.type[2] <- "sm4.heatmap"; fl8$plot.name[2] <- "other"
po8 <- plotopts; po8$overide.value[po8$parameter == "bin.colors"] <- "#FFFFFF;#000000"
r8 <- run(fl8, po8)
stopifnot(length(r8$plots) == 1, length(r8$plots[[1]]$settings$bin.colors) == 6)
pass("non-heat.site.species rows skipped; wrong number of bin.colors -> white + viridis fallback")

## ---- TEST 9: $overide.value is used --------------------------------------
po9 <- plotopts; po9$overide.value[po9$parameter == "bin.breaks"] <- "5;20"
p9 <- run(po = po9)$plots[[1]]
stopifnot(identical(levels(p9$cells$value.class), c("0", "1-5", "6-20", ">20")))
pass("$overide.value bin.breaks = 5;20 gives classes 0 / 1-5 / 6-20 / >20 (fallback colors noted)")

cat("\nAll tests passed. Plots saved in:", dir.out, "\n")

# =============================================================================
# batz.plotdetections_first.last.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plotdetections_first.last() (Batz package).
# Uses a bat plotframe (e.g. plfr.heb.bat.csv) and a suntimes table made by
# batz.generate_suntimes.arulist(). One "bat.detection" fig.list row per site.
# Tests: a site with no suntimes rows (no Dawn/Dusk lines, plot still made),
# and a trailing comma in the call. Set the paths, then run the whole script.
# =============================================================================

library(Batz)

dir.in  <- "C:/path/to/test/files"                     # holds the two files below
dir.out <- file.path(tempdir(), "first.last_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

bat <- read.csv(file.path(dir.in, "plfr.heb.bat.csv"), check.names = FALSE)
bat <- bat[, names(bat) != ""]                          # drop the row-number column

## suntimes in the current format, from an arulist (aru.name must match the plot.set / $group)
arul <- data.frame(aru.name = c("Ayers", "Cemetery", "SugarHill"),
                   longitude = c(-71.7203, -71.7250, -71.7315), latitude = c(43.6004, 43.5990, 43.5943),
                   date.start = "2026-06-01", date.end = "2026-07-07", time.zone = "America/New_York",
                   sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "")
arul$sunregion <- arul$aru.name
arul$sunregion.longitude <- arul$longitude; arul$sunregion.latitude <- arul$latitude
dir.arul <- file.path(dir.out, "arulist"); dir.create(dir.arul, showWarnings = FALSE)
write.csv(arul, file.path(dir.arul, "test_arulist.csv"), row.names = FALSE)
st <- batz.generate_suntimes.arulist(dir.load = dir.arul, dir.save = dir.arul, project.name = "test")$aru.suntimes

## fig.list: every header "NA", then fill the bat.detection settings
fb <- batz.generate_figlist(functions = "batz.plotdetections_first.last", dir.save = dir.out)$fig.list
fb <- fb[rep(1, 4), ]
fb$plot <- TRUE; fb$plot.type <- "bat.detection"; fb$plot.set <- c("Ayers", "Cemetery", "Clerk", "Sugar Hill")
fb$plot.name <- paste("HEB", fb$plot.set); fb$facet.header <- "sppid"; fb$facet.plot <- "TRUE"
fb$facet.set <- "NE"; fb$myso <- "FALSE"; fb$all.detections <- "TRUE"; fb$`40khzmyo` <- "TRUE"
fb$facet.label <- "common_name"; fb$date.format <- "%b-%d"
fb$date.start <- "6/26/2026"; fb$date.end <- "7/2/2026"; fb$xaxe.interval <- "4"
fb$xaxe.title <- "Date"; fb$yaxe.title <- "Time"

## Clerk has no suntimes rows and "Sugar Hill" does not match "SugarHill": NOTE, plot made without sun lines
r <- batz.plotdetections_first.last(data = bat, fig.list = fb, suntimes = st, aes.default = NULL,
                                    project.name = "new.project", dir.save = dir.out,
                                    strip.special = TRUE, strip.special.plotopts = FALSE, )   # trailing comma
ok(length(r$ggplots) == 4, "4 plots, including sites with no suntimes rows")
ok(nrow(r$plots[[3]]$sdb) == 0, "Clerk: 0 suntimes rows, no error")
cat("PNGs in:", dir.out, "\n")

# =============================================================================
# batz.plotsm4_heatmap.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plotsm4_heatmap() (Batz package).
# Made-up SM4 system checks -> batz.generate_plotframe.sm4() -> heatmap,
# with and without suntimes (suntimes is optional: no Dawn / Dusk lines).
# Run the whole script; the PNGs are saved in dir.out.
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "sm4_heatmap_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## checks every 60 s, 20:02:59 - 05:44:59, 4 nights; the unit stops 22:00 - 00:00 on night 2
t <- as.POSIXct("2026-06-26 20:02:59", tz = "UTC") + c(outer(60 * 0:581, 86400 * 0:3, "+"))
sm4 <- data.frame(aru.name = "AYERS", date = format(t, "%Y-%m-%d"), time = format(t, "%H:%M:%S"),
                  mic0.type = "U2", power.v = 6.4, temperature.c = 21.5, files = 1, scrubbed = 0)
gap <- t >= as.POSIXct("2026-06-27 22:00:00", tz = "UTC") & t < as.POSIXct("2026-06-28 00:00:00", tz = "UTC")
sm4 <- sm4[!gap, ]
pf <- batz.generate_plotframe.sm4(sm4, dir.save = NULL)            # NOTE: most likely schedule ... observed 20:03-05:40
ok(sum(pf$mins.oper == 0) == 8, "2-hour gap -> 8 intervals of 0")

fl <- batz.generate_figlist(functions = "batz.plotsm4_heatmap", dir.save = NULL)$fig.list
fl$plot.set <- "AYERS"; fl$plot.name <- "Test AYERS"
fl$date.start <- "6/26/2026"; fl$date.end <- "6/29/2026"; fl$date.format <- "%b-%d"; fl$xaxe.interval <- "4"
fl$xaxe.title <- "Monitoring Night"; fl$yaxe.title <- "Hour of Monitoring"

## 1. no suntimes: Midnight line only
r1 <- batz.plotsm4_heatmap(data = pf, fig.list = fl, project.name = "nosun", dir.save = dir.out)
lines1 <- vapply(r1$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines1 == "GeomLine") == 1, "no suntimes: one reference line (Midnight)")

## 2. with suntimes: Dawn, Dusk and Midnight
st <- data.frame(aru.name = "AYERS", date = format(as.Date("2026-06-26") + 0:3), date.monitoringnight = format(as.Date("2026-06-26") + 0:3),
                 sunregion = "AYERS", time.zone = "America/New_York", sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "",
                 sunset = paste(as.Date("2026-06-26") + 0:3, "20:33:00"), sunset.unix = NA,
                 sunrise = paste(as.Date("2026-06-26") + 0:3, "05:08:00"), sunrise.unix = NA,
                 sunrise.monitoringnight = paste(as.Date("2026-06-27") + 0:3, "05:08:00"), sunrise.monitoringnight.unix = NA)
r2 <- batz.plotsm4_heatmap(data = pf, fig.list = fl, suntimes = st, project.name = "sun", dir.save = dir.out)
lines2 <- vapply(r2$ggplots[[1]]$layers, function(l) class(l$geom)[1], "")
ok(sum(lines2 == "GeomLine") == 3, "with suntimes: Dawn, Dusk and Midnight")

cat("\nALL TESTS PASSED - PNGs in", dir.out, "\n")

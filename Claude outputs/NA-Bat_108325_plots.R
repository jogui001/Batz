library(Batz)
x <- read.csv("plfr.csv")
al <- read.csv("arulist.csv")
lab <- setNames(al$aru.label, al$aru.name)
## per ARU, night and species (duplicate rows summed: each pair is one value and a 0)
y <- x[x$aru %in% c("108325-1", "108325-2NE", "108325-3NW", "MOBILE108325"), ]
a <- aggregate(obs ~ aru + date + sppid, y, sum)
sta <- a[a$aru != "MOBILE108325", ]
sta$group <- unname(lab[sta$aru])
d1 <- data.frame(spp.id = sta$sppid, date = sta$date, observations.count = sta$obs, group = sta$group)
## mean per stationary ARU (3 ARUs deployed every night 7/8-8/5; no row = 0)
nights <- format(seq(as.Date("2026-07-08"), as.Date("2026-08-05"), by = "day"))
sp <- unique(sta$sppid)
grid <- expand.grid(date = nights, sppid = sp, stringsAsFactors = FALSE)
tot <- aggregate(obs ~ date + sppid, sta, sum)
grid$obs <- tot$obs[match(paste(grid$date, grid$sppid), paste(tot$date, tot$sppid))]
grid$obs[is.na(grid$obs)] <- 0
grid$obs <- grid$obs / 3
mob <- a[a$aru == "MOBILE108325", ]
d2 <- rbind(data.frame(spp.id = grid$sppid, date = grid$date, observations.count = grid$obs, group = "108325"),
            data.frame(spp.id = mob$sppid, date = mob$date, observations.count = mob$obs, group = "108325-MOB"))
write.csv(d1, "108325_plotdata_per.aru.csv", row.names = FALSE)
write.csv(d2, "108325_plotdata_mean.per.aru.csv", row.names = FALSE)

fl <- batz.generate_figlist(functions = "batz.plot.bar_activity.site", dir.save = NULL)$fig.list
fl$plot <- TRUE; fl$plot.group <- "group"; fl$pool <- FALSE; fl$legend <- TRUE
fl$date.start <- "7/8/2026"; fl$date.end <- "8/5/2026"
fl$date.interval <- "7/8/2026;7/15/2026;7/22/2026;7/29/2026;8/5/2026"
fl$date.format <- "%b %d"
fl$facet.set <- "AllDet;epfu;labo;laci;lano;myle;mylu;myse;pesu"
fl$facet.label <- "AllDet=All detections"
fl$all.detections <- TRUE; fl[["40khzmyo"]] <- TRUE; fl$myso <- FALSE
fl$yaxe.trans <- "log"; fl$y.scale <- "rounded"; fl$ymax <- ""; fl$y.custom <- ""
fl$plot.dimensions <- "9 x 6.5"
f1 <- fl; f1$plot.name <- "108325 per ARU"; f1$plot.set <- "108325-SE1;108325-NE1;108325-NW"
f2 <- fl; f2$plot.name <- "108325 mean per ARU"; f2$plot.set <- "108325;108325-MOB"
r1 <- batz.plot.bar_activity.site(d1, f1, project.name = "NA-Bat", dir.save = "out", xaxe.title = "Monitoring Night", yaxe.title = "Number of observations")
r2 <- batz.plot.bar_activity.site(d2, f2, project.name = "NA-Bat", dir.save = "out", xaxe.title = "Monitoring Night", yaxe.title = "Mean number of observations per ARU")
w <- read.csv("../s/weather.csv"); w <- w[w$site == "108325", ]
st <- read.csv(list.files("../s", pattern = "NA-Bat_suntimes", full.names = TRUE)[1]); st <- st[st$sunregion == "108325", ]
r3 <- batz.plot.line_weather.average(w, st, panel.layout = "separate", date.start = "2026-07-08", date.end = "2026-08-05",
                                     project.name = "NA-Bat", dir.save = "out", plot.width = 9, plot.height = 6.5)

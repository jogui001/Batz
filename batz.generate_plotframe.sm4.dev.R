# =============================================================================
# batz.generate_plotframe.sm4.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.generate_plotframe.sm4() (Batz package).
# Summarizes SM system checks (one per minute) into intervals for
# batz.plot.heatmap_aru.time(). Tests use made-up checks; the last section runs the
# whole pipeline on real logs (edit the paths).
# =============================================================================

library(Batz)

dir.out <- file.path(tempdir(), "plotframe.sm4_test"); dir.create(dir.out, showWarnings = FALSE)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

## made-up checks: one ARU, every 60 s from 20:02:59 to 05:44:59, with
##  - 3 checks delayed 15 s (recording being written) in the 22:00 interval
##  - the mic disconnected (mic0.type blank) for 5 checks at 01:00
##  - a "U 2" with a space (counts as U2 after spaces are removed)
mk <- function(start, n) {
  t <- as.POSIXct(start, tz = "UTC") + 60 * (0:(n - 1))
  data.frame(aru.name = "AYERS", date = format(t, "%Y-%m-%d"), time = format(t, "%H:%M:%S"),
             mic0.type = "U2", power.v = 6.4, temperature.c = 21.5, files = 1, scrubbed = 0)
}
d <- mk("2026-06-26 20:02:59", 582)
i <- which(d$time %in% c("22:03:59", "22:04:59", "22:05:59"))
d$time[i] <- format(as.POSIXct(paste(d$date[i], d$time[i]), tz = "UTC") + 15, "%H:%M:%S")   # late checks
d$mic0.type[d$time %in% sprintf("01:%02d:59", 0:4)] <- ""                                    # no mic
d$mic0.type[d$time == "03:00:59"] <- "U 2"
d$power.v[nrow(d)] <- 6.1

## ---- 1. defaults: 15-minute intervals ---------------------------------------
pf <- batz.generate_plotframe.sm4(d, dir.save = dir.out, project.name = "Bristol NH")
print(head(pf))
ok(identical(names(pf), c("aru.name", "date", "time", "date.monitoringnight", "mins.oper", "power.v",
                          "temperature.c", "files", "scrubbed", "mins2.noon.mon", "mins2.midnight", "aru.schedule")), "headers")
ok(pf$mins.oper[pf$time == "20:00:00"] == 13, "first interval: checks from 20:02:59 -> 13 minutes")
ok(pf$mins.oper[pf$time == "22:00:00"] == 15, "late checks (within buffer) still 15 minutes")
ok(pf$mins.oper[pf$time == "01:00:00"] == 10, "5 checks with no mic -> 10 minutes")
ok(pf$mins.oper[pf$time == "03:00:00"] == 15, "\"U 2\" counts as U2")
ok(all(pf$date.monitoringnight == "2026-06-26"), "checks after midnight belong to the night that started at noon")
ok(pf$mins2.noon.mon[pf$time == "01:00:00"] == 780 && pf$mins2.midnight[pf$time == "01:00:00"] == 60,
   "mins2.noon.mon / mins2.midnight")
ok(pf$files[pf$time == "21:00:00"] == 15 && pf$power.v[nrow(pf)] == 6.1, "files summed; last power.v")
ok(length(list.files(dir.out, "^Bristol-NH_plotframe\\.sm4_\\d{14}\\.csv$")) >= 1, "file saved")

## ---- 2. buffer = 0: the 3 late checks no longer count as one run -------------
pf0 <- batz.generate_plotframe.sm4(d, dir.save = NULL, buffer = 0)
print(pf0[pf0$time == "22:00:00", c("time", "mins.oper")])

## ---- 3. other intervals --------------------------------------------------------
pf30 <- batz.generate_plotframe.sm4(d, dir.save = NULL, interval = 30)
ok(all(pf30$mins2.noon.mon %% 30 == 0) && max(pf30$mins.oper) == 30, "30-minute intervals")
pf25 <- batz.generate_plotframe.sm4(d, dir.save = NULL, interval = 25)
ok(all(pf25$mins2.noon.mon %% 25 == 0), "25-minute intervals from noon")

## ---- 4. a break between nightly runs is not a failure (no rows for the daytime) ----
d2 <- rbind(mk("2026-06-26 20:00:59", 60), mk("2026-06-27 20:00:59", 60))
pf2 <- batz.generate_plotframe.sm4(d2, dir.save = NULL)       # NOTE: most likely schedule ... observed 20:01-21:00
ok(nrow(pf2) == 8 && all(pf2$mins.oper == 15), "two nightly runs, 4 full intervals each, nothing in between")

## ---- 5. errors -------------------------------------------------------------------
try(batz.generate_plotframe.sm4(d, interval = 12))          # 5 to 60, divisible by 5
try(batz.generate_plotframe.sm4(d[, -4]))                    # missing $mic0.type

## ---- 6. scheduled recording: 0 for scheduled intervals with no checks (2026-11-07) ----
## deployment history + schedules as made by batz.merge_sitevisit.acoustics() (dates in UTC)
dh <- data.frame(aru.name = "AYERS", site = "AYERS", visit.reason = c("deployment", "retrieval"),
                 date = as.POSIXct(c("2026-06-26 17:00:00", "2026-06-27 15:00:00"), tz = "UTC"),
                 aru.schedule = "USFWS.compliance", aru.schedule_new = "")
sc <- data.frame(survey123.name = "survey123.habitat", header.name = "survey.type", header.value = "Summer - Acoustic",
                 aru.schedule = "USFWS.compliance", aru.schedule_divisions = "1", frequencey = "ultrasonic",
                 time.on = "sunset-30", time.off = "sunrise+30", recording.type = "triggered", recording.interval = "continuous")
st <- data.frame(aru.name = "Ayers", date = c("6/26/2026", "6/27/2026"), time.zone = "America/New_York",
                 sunset = c("6/26/2026 20:32", "6/27/2026 20:32"), sunrise = c("6/26/2026 5:12", "6/27/2026 5:13"))
meta <- list(deployment.history = dh, aru.scheduals = sc)
## scheduled 20:02 - 05:43 (sunset-30 to sunrise+30); the checks run 20:02:59 - 05:44:59 with a 01:00-01:05 mic gap
pfs <- batz.generate_plotframe.sm4(d, dir.save = NULL, site.metadata = meta, suntimes = st)
ok(nrow(pfs) == nrow(pf), "every scheduled interval has checks: no 0 rows added")
d6 <- d[!(d$time >= "23:00:00" | d$time < "01:00:00"), ]                               # 2 hours with no checks
pf6 <- batz.generate_plotframe.sm4(d6, dir.save = NULL, site.metadata = meta, suntimes = st)
z <- pf6[pf6$mins.oper == 0, ]
print(z[, c("aru.name", "date", "time", "mins.oper", "files")])
ok(nrow(z) == 8 && all(z$files == 0) && all(is.na(z$power.v)), "8 missing scheduled intervals -> mins.oper 0")
## not recovered yet: the last service visit is the recovery date (NOTE)
meta2 <- meta; meta2$deployment.history$visit.reason[2] <- "sitevisit_maintenance"
meta2$deployment.history$date[2] <- as.POSIXct("2026-06-27 06:00:00", tz = "UTC")      # 02:00 local
pf7 <- batz.generate_plotframe.sm4(d6, dir.save = NULL, site.metadata = meta2, suntimes = st)
ok(sum(pf7$mins.oper == 0) == 8, "last service visit (02:00) used as the recovery date")
## XXonXXoff and HH:MM: 21:00-23:00, 10 minutes on / 10 off
meta3 <- meta; meta3$aru.scheduals$time.on <- "21:00"; meta3$aru.scheduals$time.off <- "23:00"
meta3$aru.scheduals$recording.interval <- "10on10off"
d8 <- d[!(d$time >= "21:00:00" & d$time < "23:00:00"), ]
pf8 <- batz.generate_plotframe.sm4(d8, dir.save = NULL, site.metadata = meta3, suntimes = st, schedule.buffer = 0)
print(pf8[pf8$mins.oper == 0, c("time", "mins.oper")])                                 # 21:00 ... 22:45: blocks 21:00-21:10, 21:20-21:30, ...
ok(sum(pf8$mins.oper == 0) == 8, "10on10off from 21:00 to 23:00 touches all 8 intervals")

## ---- 7. no site.metadata: aru.schedule and aru.scheduals_new (2026-11-07) ------
d7 <- d[!(d$time >= "23:00:00" | d$time < "01:00:00"), ]                               # 2 hours with no checks
## not given: most likely schedule from the checks (NOTE names it and the matches)
p7a <- batz.generate_plotframe.sm4(d7, dir.save = NULL)
ok(sum(p7a$mins.oper == 0) == 8, "aru.schedule not given: inferred schedule -> 8 intervals of 0")
## given, sun-based: needs suntimes (st from section 6)
p7b <- batz.generate_plotframe.sm4(d7, dir.save = NULL, aru.schedule = "USFWS.compliance", suntimes = st)
ok(sum(p7b$mins.oper == 0) == 8, "aru.schedule = USFWS.compliance with suntimes")
## "": every interval between the first and last check with no checks is 0
p7c <- batz.generate_plotframe.sm4(d7, dir.save = NULL, aru.schedule = "")
ok(sum(p7c$mins.oper == 0) == 8, "aru.schedule = \"\": the gap inside the run is 0")
## a schedule not in the database, from a data frame (or a CSV path)
new.sch <- data.frame(survey123.name = "", header.name = "", header.value = "", aru.schedule = "test.2200to0000",
                      aru.schedule_divisions = "1", frequencey = "ultrasonic", time.on = "22:00", time.off = "00:00",
                      recording.type = "triggered", recording.interval = "continuous")
write.csv(new.sch, file.path(dir.out, "new_schedule.csv"), row.names = FALSE)
p7d <- batz.generate_plotframe.sm4(d7, dir.save = NULL, aru.schedule = "test.2200to0000",
                                   aru.scheduals_new = file.path(dir.out, "new_schedule.csv"), schedule.buffer = 0)
ok(sum(p7d$mins.oper == 0) == 4, "aru.scheduals_new schedule 22:00-00:00: only 23:00-00:00 is 0")
try(batz.generate_plotframe.sm4(d7, dir.save = NULL, aru.schedule = "no.such.schedule"))   # lists the allowed names
## ARU names: case, spaces, "." and "_" ignored (deployment history "Ayers" = log "AYERS" = "ay_ers")
meta9 <- meta; meta9$deployment.history$aru.name <- "ay_ers"; meta9$deployment.history$site <- "ay_ers"
d9 <- d6; d9$aru.name <- "Ayers."
p9 <- batz.generate_plotframe.sm4(d9, dir.save = NULL, site.metadata = meta9, suntimes = st)
ok(sum(p9$mins.oper == 0) == 8 && all(p9$aru.name == "Ayers."), "ARU names matched ignoring case, \".\" and \"_\"")

## ---- 8. $aru.schedule in the output (2026-11-09) -----------------------------
ok(all(pfs$aru.schedule == "USFWS.compliance"), "deployment history schedule -> $aru.schedule USFWS.compliance")
ok(all(grepl("^observed", p7a$aru.schedule) | p7a$aru.schedule %in% unique(Batz:::batz.db("aru.scheduals")$aru.schedule)),
   "inferred schedule named in $aru.schedule")
print(unique(p7a$aru.schedule))
ok(all(p7c$aru.schedule == ""), "aru.schedule = \"\" -> blank $aru.schedule")
## a visit at 02:00 changes the schedule to "continuous": rows from then on take the new one
meta10 <- meta
meta10$deployment.history <- rbind(dh[1, ], dh[1, ], dh[2, ])
meta10$deployment.history$visit.reason[2] <- "sitevisit_maintenance"
meta10$deployment.history$date[2] <- as.POSIXct("2026-06-27 06:00:00", tz = "UTC")          # 02:00 local
meta10$deployment.history$aru.schedule_new[2] <- "continuous"
p10 <- batz.generate_plotframe.sm4(d, dir.save = NULL, site.metadata = meta10, suntimes = st)
print(table(p10$time >= "02:00:00" & p10$time < "12:00:00", p10$aru.schedule))
ok(all(p10$aru.schedule[p10$time >= "02:00:00" & p10$time < "12:00:00"] == "continuous") &&
   all(p10$aru.schedule[p10$time >= "12:00:00" | p10$time < "02:00:00"] == "USFWS.compliance"),
   "schedule change at a visit: $aru.schedule changes from that visit on")

cat("\nALL TESTS PASSED - output in", dir.out, "\n")

## ---- real data: logs -> plot frame -> heatmap (edit the paths) ----------------
# logs <- batz.merge_sm.logfiles(dir.logs, dir.sub = TRUE)           # or read.csv("sm4.csv")
# meta <- batz.merge_sitevisit.acoustics(dir.load = wd.Raw, dir.save = wd.out, project.name = "Bristol NH")
# pf <- batz.generate_plotframe.sm4(logs$SM4, dir.save = wd.out, project.name = "Bristol NH",
#                                   site.metadata = meta, suntimes = "Bristol NH_suntimes_20260601to20260731_20261106203223.csv")
# suntimes <- read.csv("Bristol NH_suntimes_20260601to20260731_20261106203223.csv", check.names = FALSE)
# fig.list <- batz.generate_figlist(functions = "batz.plot.heatmap_aru.time", dir.save = NULL)$fig.list
# fig.list <- fig.list[rep(1, 4), ]
# fig.list$plot <- TRUE                                                 # model rows come with $plot = FALSE
# fig.list$plot.set <- c("Ayers", "Cemetery", "Clerk", "SugarHill")      # one ARU per row
# fig.list$plot.name <- paste("HEB", fig.list$plot.set)
# fig.list$date.start <- "6/26/2026"; fig.list$date.end <- "7/2/2026"
# fig.list$date.format <- "%b-%d"; fig.list$xaxe.interval <- "4"
# fig.list$xaxe.title <- "Monitoring Night"; fig.list$yaxe.title <- "Hour of Monitoring"
# batz.plot.heatmap_aru.time(data = pf, fig.list = fig.list, suntimes = suntimes, project.name = "Bristol NH", dir.save = wd.charts)

## NA-Bat: for every sun region - bats per ARU, bats mean per ARU (+ mobile) and night weather.
## Early-July and late-July deployments each share one date axis; every plot of a kind shares one Y axis.
library(Batz)
x  <- read.csv("plfr2.csv")                               # plfr.NA.bat.csv (batz.generate_plotframe.bat)
al <- read.csv("arulist.csv")                             # NA-Bat_20261118173904.arulist.csv
w  <- read.csv("../s/weather.csv")                        # weather.csv (batz.generate_weatherstation.data)
st <- read.csv(list.files("../s", pattern = "NA-Bat_suntimes", full.names = TRUE)[1])
dir.out <- "all"; unlink(dir.out, recursive = TRUE)

## ---- sunregion_locations.xlsx: $Mobile = mobile nights, $break.start/$break.end = outage (2 detectors) ----
sl <- as.data.frame(readxl::read_excel("sl.xlsx", col_types = "text"))
xl.date <- function(v) {                                   # Excel serial, Y-m-d, m/d/Y; "7/162026" read as 7/16/2026
  v <- trimws(v); out <- as.Date(rep(NA_character_, length(v)))
  num <- suppressWarnings(as.numeric(v)); out[!is.na(num)] <- as.Date(num[!is.na(num)], origin = "1899-12-30")
  i <- is.na(out); out[i] <- as.Date(v[i], format = "%Y-%m-%d")
  i <- is.na(out); out[i] <- as.Date(v[i], format = "%m/%d/%Y")
  i <- is.na(out) & grepl("^[0-9]{1,2}/[0-9]{1,2}[0-9]{4}$", v)
  if (any(i)) { fx <- sub("^([0-9]{1,2}/[0-9]{1,2})([0-9]{4})$", "\\1/\\2", v[i])
                cat("NOTE: mobile date", v[i], "read as", fx, "\n"); out[i] <- as.Date(fx, format = "%m/%d/%Y") }
  out
}
mob.nights <- lapply(setNames(strsplit(sl$Mobile, ";"), sl$site), xl.date)
## correction (per Josh, 2026-11-19): 108325's mobile nights are Jul 15 and Aug 5 (Jul 17 is 1079's)
mob.nights[["108325"]] <- as.Date(c("2026-07-15", "2026-08-05"))
brk <- setNames(lapply(seq_len(nrow(sl)), function(i) c(xl.date(sl$break.start[i]), xl.date(sl$break.end[i]))), sl$site)

## ---- deployments: early July and late July groups share one date window -----------------
md <- function(d) as.Date(d, format = "%m/%d/%Y")
al$d0 <- md(al$date.start); al$d1 <- md(al$date.end) - 1   # last night = night before pick-up
regions <- unique(as.character(al$sunregion))
start <- tapply(al$d0, al$sunregion, min); end <- tapply(al$d1, al$sunregion, max)
group <- ifelse(as.Date(start[regions], origin = "1970-01-01") <= as.Date("2026-07-15"), "early", "late"); names(group) <- regions
win <- lapply(split(regions, group[regions]), function(rr)
  c(min(as.Date(start[rr], origin = "1970-01-01")), max(as.Date(end[rr], origin = "1970-01-01"))))
for (g in names(win)) cat(sprintf("%s July: %s - %s, sun regions %s\n", g, win[[g]][1], win[[g]][2], paste(regions[group == g], collapse = ", ")))

## ---- bat counts per ARU, night and species -------------------------------------------------
x$aru <- x$aru.name; x$sppid <- x$spp.id; x$obs <- x$observations.count
## LUSO and MYSO recoded to MYLU (per Josh, 2026-11-19); counts on the same ARU and night are added
lu <- toupper(x$sppid) %in% c("LUSO", "MYSO")
cat("NOTE:", sum(lu), "LUSO / MYSO row(s) recoded to mylu\n"); x$sppid[lu] <- "mylu"
x$date <- format(as.Date(x$date, format = "%m/%d/%Y"))
fix <- grepl("^5157-", x$aru)                                # "5157-NW1"/"5157-SE1": sun region 5175 in the arulist
if (any(fix)) { cat("NOTE: typo", paste(unique(x$aru[fix]), collapse = ", "), "read as", paste(unique(sub("^5157-", "5175-", x$aru[fix])), collapse = ", "), "\n")
                x$aru[fix] <- sub("^5157-", "5175-", x$aru[fix]) }
a <- aggregate(obs ~ aru + date + sppid, x, sum)
is.mob <- grepl("MOB", a$aru, ignore.case = TRUE)
a <- a[a$obs > 0 | !is.mob, ]
## mobile sun region: the number in its name ("105059-MOB", "MOBILE105059"); a mobile named without numbers
## (e.g. "MOBILE"): the sun region whose $Mobile lists that night
is.mob <- grepl("MOB", a$aru, ignore.case = TRUE)
a$mob.region <- ifelse(is.mob & grepl("[0-9]", a$aru), gsub("[^0-9]", "", a$aru), NA)
nonum <- is.mob & !grepl("[0-9]", a$aru)
for (i in which(nonum)) {
  hit <- names(mob.nights)[vapply(mob.nights, function(d) as.Date(a$date[i]) %in% d, logical(1))]
  if (length(hit) == 1) a$mob.region[i] <- hit
}
for (u in unique(a$aru[nonum])) cat("NOTE:", u, "on", paste(unique(a$date[a$aru == u]), collapse = ", "), "-> sun region",
                                    paste(unique(a$mob.region[a$aru == u]), collapse = ", "), "\n")

bat <- list()
for (r in regions) {
  ar <- al[al$sunregion == r, ]
  sta <- a[!is.mob & a$aru %in% ar$aru.name, ]
  if (!nrow(sta)) { cat("NOTE: no bat data for sun region", r, "\n"); next }
  sta$group <- ar$aru.label[match(sta$aru, ar$aru.name)]     # arulist plot labels (e.g. 108325-1 -> 108325-SE1)
  sta <- aggregate(obs ~ group + date + sppid, sta, sum)      # one detector moved (1079-NE2 / -NE2-2): one label
  ww <- win[[group[[r]]]]; nights <- seq(ww[1], ww[2], by = "day")
  d1 <- data.frame(spp.id = sta$sppid, date = sta$date, observations.count = sta$obs, group = sta$group)
  ## mean per stationary detector: night total / 3, or / 2 on outage nights
  n.det <- rep(3, length(nights)); b <- brk[[r]]
  if (!is.null(b) && !anyNA(b)) n.det[nights >= b[1] & nights <= b[2]] <- 2
  grid <- expand.grid(date = format(nights), sppid = unique(sta$sppid), stringsAsFactors = FALSE)
  tot <- aggregate(obs ~ date + sppid, sta, sum)
  grid$obs <- tot$obs[match(paste(grid$date, grid$sppid), paste(tot$date, tot$sppid))]
  grid$obs[is.na(grid$obs)] <- 0
  grid$obs <- grid$obs / n.det[match(grid$date, format(nights))]
  mob <- a[is.mob & a$mob.region %in% r, ]
  off <- setdiff(as.Date(unique(mob$date)), mob.nights[[r]])
  if (length(off)) cat("NOTE: sun region", r, "- mobile night(s) in the data not in $Mobile:", format(as.Date(off, origin = "1970-01-01")), "\n")
  miss <- setdiff(mob.nights[[r]], as.Date(unique(mob$date)))
  if (length(miss)) cat("NOTE: sun region", r, "- $Mobile night(s) with no mobile detections in the data:", format(as.Date(miss, origin = "1970-01-01")), "\n")
  d2 <- rbind(data.frame(spp.id = grid$sppid, date = grid$date, observations.count = grid$obs, group = r),
              if (nrow(mob)) data.frame(spp.id = mob$sppid, date = mob$date, observations.count = mob$obs, group = paste0(r, "-MOB")))
  bat[[r]] <- list(d1 = d1, d2 = d2, ww = ww, labels = sort(unique(sta$group)), mob = nrow(mob) > 0,
                   brk = if (!is.null(b) && !anyNA(b)) b else NULL)
}

## ---- bat plots: one Y scale for all ---------------------------------------------------------
cat("Largest nightly count in any bat plot:", max(unlist(lapply(bat, function(b) c(b$d1$observations.count, b$d2$observations.count)))), "\n")
fl <- batz.generate_figlist(functions = "batz.plot.bar_activity.site", dir.save = NULL)$fig.list
fl$plot <- TRUE; fl$plot.group <- "group"; fl$pool <- FALSE; fl$legend <- TRUE
fl$date.format <- "%b %d"; fl$facet.set <- "AllDet;epfu;labo;laci;lano;myle;mylu;myse;pesu"
fl$facet.label <- "AllDet=All detections"; fl$all.detections <- TRUE; fl[["40khzmyo"]] <- TRUE; fl$myso <- FALSE
top <- max(unlist(lapply(bat, function(b) c(b$d1$observations.count, b$d2$observations.count))))
fl$yaxe.trans <- "log"; fl$y.scale <- "custom"; fl$y.custom <- if (top <= 300) "0;5;20;70;300" else "0;5;20;70;300;1000"
fl$ymax <- if (top <= 300) 300 else 1000
fl$plot.dimensions <- "9 x 6.5"
for (r in names(bat)) {
  b <- bat[[r]]
  fl$date.start <- format(b$ww[1], "%m/%d/%Y"); fl$date.end <- format(b$ww[2], "%m/%d/%Y")
  fl$date.interval <- paste(format(seq(b$ww[1], b$ww[2], by = "7 days"), "%m/%d/%Y"), collapse = ";")
  f1 <- fl; f1$plot.name <- paste(r, "per ARU"); f1$plot.set <- paste(b$labels, collapse = ";")
  ## per-ARU graph: outage nights (2 detectors) shaded grey
  f1$break.start <- if (!is.null(b$brk)) format(b$brk[1], "%m/%d/%Y") else ""
  f1$break.end <- if (!is.null(b$brk)) format(b$brk[2], "%m/%d/%Y") else ""
  f2 <- fl; f2$plot.name <- paste(r, "mean per ARU"); f2$plot.set <- paste(c(r, if (b$mob) paste0(r, "-MOB")), collapse = ";")
  f2$break.start <- ""; f2$break.end <- ""                    # mean graph: no shading (outage nights still / 2)
  batz.plot.bar_activity.site(b$d1, f1, project.name = "NA-Bat", dir.save = dir.out, legend.40khzmyo = "before",
                              break.fill = "grey85", xaxe.title = "Monitoring Night", yaxe.title = "Number of observations")
  ## mean graph: sun region bars red, mobile blue
  batz.plot.bar_activity.site(b$d2, f2, project.name = "NA-Bat", dir.save = dir.out, legend.40khzmyo = "before",
                              legend.groupval.colors = "red;#1f78b4",
                              xaxe.title = "Monitoring Night", yaxe.title = "Mean number of observations per ARU")
}

## ---- second set: per-ARU graphs for the first 14 nights of deployment (same Y axis) -------------
## nights 1-14 from the group's first night: early July Jul 7-20, late July Jul 17-30
dir.14 <- file.path(dir.out, "first14"); dir.create(dir.14, showWarnings = FALSE, recursive = TRUE)
for (r in names(bat)) {
  b <- bat[[r]]; d0 <- b$ww[1]
  ## regions with an outage: the region's own first night, then 14 nights with all 3 detectors working
  ## (outage nights skipped, so 1079 / 5175 / 55 run into August); others: 14 nights from the group's first night
  if (!is.null(b$brk)) {
    d0 <- min(as.Date(start[r], origin = "1970-01-01"))
    nn <- seq(d0, b$ww[2], by = "day"); ok <- !(nn >= b$brk[1] & nn <= b$brk[2])
    d13 <- nn[which(ok)[14]]
  } else d13 <- d0 + 13
  cat(sprintf("first 14 nights: %s %s - %s\n", r, d0, d13))
  f3 <- fl; f3$plot.name <- paste(r, "per ARU - first 14 nights", if (!is.null(b$brk)) "(outage nights not counted)" else ""); f3$plot.set <- paste(b$labels, collapse = ";")
  f3$date.start <- format(d0, "%m/%d/%Y"); f3$date.end <- format(d13, "%m/%d/%Y")
  f3$date.interval <- paste(format(seq(d0, d13, by = if (d13 - d0 > 16) "7 days" else "3 days"), "%m/%d/%Y"), collapse = ";")
  in14 <- !is.null(b$brk) && b$brk[1] <= d13
  f3$break.start <- if (in14) format(b$brk[1], "%m/%d/%Y") else ""
  f3$break.end <- if (in14) format(min(b$brk[2], d13), "%m/%d/%Y") else ""
  batz.plot.bar_activity.site(b$d1, f3, project.name = "NA-Bat", dir.save = dir.14, legend.40khzmyo = "before",
                              break.fill = "grey85", xaxe.title = "Monitoring Night", yaxe.title = "Number of observations")
}

## ---- weather: every sun region, one Y scale per panel, group date windows ---------------------
wx <- function(r) { ws <- w[vapply(strsplit(as.character(w$site), ";"), function(s) r %in% s, logical(1)), ]; ws$site <- r; ws }
wr <- lapply(setNames(regions, regions), function(r) {
  ww <- win[[group[[r]]]]
  batz.plot.line_weather.average(wx(r), st[as.character(st$sunregion) == r, ], panel.layout = "separate",
                                 date.start = ww[1], date.end = ww[2], dir.save = NULL)$data
})
all.w <- do.call(rbind, wr)
t.rng <- c(min(0, floor(min(all.w$temperature.min, na.rm = TRUE) / 5) * 5), ceiling(max(all.w$temperature.max, na.rm = TRUE) / 5) * 5)
w.rng <- c(0, ceiling(max(all.w$wind.max, na.rm = TRUE) / 2) * 2)
p.rng <- c(0, max(pretty(c(0, max(all.w$precipitation.total, na.rm = TRUE)))))
cat("Weather axes: temperature", t.rng, "C; wind", w.rng, "m/s; precipitation", p.rng, "mm\n")
for (r in regions) {
  ww <- win[[group[[r]]]]
  batz.plot.line_weather.average(wx(r), st[as.character(st$sunregion) == r, ], panel.layout = "separate",
                                 date.start = ww[1], date.end = ww[2], temp.range = t.rng, wind.range = w.rng, precip.range = p.rng,
                                 project.name = "NA-Bat", dir.save = dir.out, plot.width = 9, plot.height = 6.5)
}

## ---- second set: mean per ARU + temperature 20:00-24:00 (secondary Y) + rain strip ----------------
## Temperature: each night's max and min between 20:00 and midnight (station local time), one scale for all.
## Rain strip (top): a window counts when its rain total is > 1 mm; windows in night order are
##   12:00-18:00 (light blue), 18:00-24:00 (black), 00:00-08:00 next morning (light blue);
##   rain in more than one window: the second of them in night order; no rain: white. No grey outage shading.
dir.w <- file.path(dir.out, "mean_weather"); dir.create(dir.w, showWarnings = FALSE, recursive = TRUE)
night.wx <- function(r, nights) {
  ws <- wx(r); t <- as.POSIXct(ws$date.time, tz = "America/New_York")
  h <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60; dd <- as.Date(format(t, "%Y-%m-%d"))
  p <- suppressWarnings(as.numeric(ws$precipitation.mm_interval)); tc <- suppressWarnings(as.numeric(ws$temperature.c))
  do.call(rbind, lapply(nights, function(n) {
    ev <- dd == n & h >= 20
    r1 <- sum(p[dd == n & h >= 12 & h < 18], na.rm = TRUE); r2 <- sum(p[dd == n & h >= 18], na.rm = TRUE)
    r3 <- sum(p[dd == n + 1 & h < 8], na.rm = TRUE)
    wet <- c("12:00-18:00", "18:00-24:00", "00:00-08:00")[c(r1, r2, r3) > 1]
    data.frame(date = n, t.max = if (any(!is.na(tc[ev]))) max(tc[ev], na.rm = TRUE) else NA,
               t.min = if (any(!is.na(tc[ev]))) min(tc[ev], na.rm = TRUE) else NA,
               rain.12.18 = r1, rain.18.24 = r2, rain.00.08 = r3,
               rain = if (length(wet) == 0) "none" else if (length(wet) == 1) wet else wet[2])
  }))
}
nw <- lapply(setNames(names(bat), names(bat)), function(r) { ww <- bat[[r]]$ww; night.wx(r, seq(ww[1], ww[2], by = "day")) })
t.all <- unlist(lapply(nw, function(d) c(d$t.max, d$t.min)))
t.sc <- c(floor(min(t.all, na.rm = TRUE) / 5) * 5, ceiling(max(t.all, na.rm = TRUE) / 5) * 5)
cat("Temperature 20:00-24:00 axis (all plots):", t.sc, "C\n")
rain.col <- c(none = "white", "12:00-18:00" = "lightblue", "18:00-24:00" = "black", "00:00-08:00" = "lightblue")
for (r in names(bat)) {
  b <- bat[[r]]; d <- nw[[r]]
  f4 <- fl; f4$plot.name <- paste(r, "mean per ARU - temperature 20:00-24:00 and rain")
  f4$plot.set <- paste(c(r, if (b$mob) paste0(r, "-MOB")), collapse = ";")
  f4$date.start <- format(b$ww[1], "%m/%d/%Y"); f4$date.end <- format(b$ww[2], "%m/%d/%Y")
  f4$date.interval <- paste(format(seq(b$ww[1], b$ww[2], by = "7 days"), "%m/%d/%Y"), collapse = ";")
  f4$break.start <- ""; f4$break.end <- ""
  grDevices::pdf(NULL)                                          # save.auto = FALSE draws the plot: send it nowhere
  res <- batz.plot.bar_activity.site(b$d2, f4, project.name = "NA-Bat", save.auto = FALSE, legend.40khzmyo = "before",
                                     legend.groupval.colors = "red;#1f78b4",
                                     xaxe.title = "Monitoring Night", yaxe.title = "Mean number of observations per ARU")
  grDevices::dev.off()
  p <- res$plots[[1]]; g <- res$ggplots[[1]]
  top <- p$y.upper.plot                                          # top of the (log) bat axis
  y.t <- function(v) (v - t.sc[1]) / diff(t.sc) * top             # temperature on the bat axis
  tl <- rbind(data.frame(date = d$date, y = y.t(d$t.max), what = "Max temp"),
              data.frame(date = d$date, y = y.t(d$t.min), what = "Min temp"))
  rs <- data.frame(xmin = d$date - 0.5, xmax = d$date + 0.5, ymin = top * 1.04, ymax = top * 1.12,
                   fill = unname(rain.col[d$rain]))
  for (fc in unique(rs$fill))                                     # one layer per strip colour (fill scale = detectors)
    g <- g + ggplot2::geom_rect(data = rs[rs$fill == fc, , drop = FALSE], ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                fill = fc, colour = "grey60", linewidth = 0.1, inherit.aes = FALSE)
  g <- g +
    ggplot2::geom_line(data = tl, ggplot2::aes(x = date, y = y, linetype = what), colour = "grey20", linewidth = 0.5,
                       inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_point(data = tl, ggplot2::aes(x = date, y = y), colour = "grey20", size = 0.6, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("Max temp" = "solid", "Min temp" = "dashed"),
                                   guide = ggplot2::guide_legend(order = 3)) +
    suppressMessages(ggplot2::scale_y_continuous(limits = c(0, top * 1.12), breaks = p$break.pos, labels = p$break.labels,
                       name = "\nMean number of observations per ARU",
                       sec.axis = ggplot2::sec_axis(~ t.sc[1] + . / top * diff(t.sc), name = "Temperature 20:00-24:00 (\u00b0C)",
                                                    breaks = pretty(t.sc)[pretty(t.sc) >= t.sc[1] & pretty(t.sc) <= t.sc[2]]))) +
    ggplot2::labs(caption = "Top strip, rain > 1 mm: white = none; light blue = 12:00-18:00 or 00:00-08:00; black = 18:00-24:00 (rain in several windows: the second)") +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, hjust = 0))
  f <- file.path(dir.w, sprintf("NA-Bat_%s_5_bats.mean.per.ARU_temp.rain.png", r))
  suppressMessages(ggplot2::ggsave(f, g, width = 9, height = 7, dpi = 300)); cat("Saved:", f, "\n")
  utils::write.csv(d, file.path(dir.w, sprintf("NA-Bat_%s_night.temp.rain.csv", r)), row.names = FALSE)
}

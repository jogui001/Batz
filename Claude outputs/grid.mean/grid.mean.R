## Grid-cell mean per stationary ARU per night, whole deployment, with the mobile route
## Built 2026-11-02, per Josh (style of the "Auto-Processed Calls Per ARU" plot).
## Red   = mean number of calls per stationary ARU that night (sum / ARUs with any
##         record, incl. NOISE, that night - so inactive detectors aren't counted).
## Blue  = the mobile route on its night. Black (All detections) = 40kHzMyo, mean per ARU.
## Species = Batz manid.kp (manual ID, else Kaleidoscope auto ID); All detections =
##           every record except NOISE. Y axis = log1p scale, 5 evenly spaced breaks.
library(ggplot2)
dir.in  <- if (exists("dir.in"))  dir.in  else "."
dir.out <- if (exists("dir.out")) dir.out else "."
if (!exists("merged")) {
  r <- batz.merge_vetted.acoustics(dir.load = dir.in, load.pattern = "FY26*FinalVetted.csv", dir.sub = FALSE)
  merged <- r$vetted.merged
}
v <- data.frame(aru = trimws(merged$aru.name),
                night = as.Date(as.character(merged$date.monitoringnight), tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
                spp = trimws(as.character(merged$manid.kp)), stringsAsFactors = FALSE)
v$aru <- sub("^5157-", "5175-", v$aru)                    # 5157-NW1 / 5157-SE1 are 5175 detectors
v$aru[v$aru == "55-NW1 new"] <- "55-NW1"
v$aru[v$aru == "107802-SW3"] <- "107802-SW1"
v$aru[v$aru == "1079-NE2-2"] <- "1079-NE2"
v$mobile <- grepl("MOB", v$aru, ignore.case = TRUE)
v$cell <- sub("^MOBILE", "", sub("-.*$", "", v$aru), ignore.case = TRUE)

species <- c("All detections" = "ALL", "Big brown bat" = "epfu", "Eastern red bat" = "labo",
             "Eastern small-footed myotis" = "myle", "Hoary bat" = "laci", "Little brown bat" = "mylu",
             "Northern long-eared bat" = "myse", "Silver-haired bat" = "lano", "Tri-colored bat" = "pesu")

plot.cell <- function(cell) {
  d <- v[v$cell == cell, ]
  mob.lab <- paste0(cell, "-MOB")
  d$series <- ifelse(d$mobile, mob.lab, cell)
  nights <- unique(d[, c("series", "night")])
  n.aru <- aggregate(aru ~ series + night, d, function(a) length(unique(a)))
  bat <- d[toupper(d$spp) != "NOISE", ]
  pd <- do.call(rbind, lapply(seq_len(nrow(nights)), function(i) {
    b <- bat[bat$series == nights$series[i] & bat$night == nights$night[i], ]
    k <- n.aru$aru[n.aru$series == nights$series[i] & n.aru$night == nights$night[i]]
    data.frame(series = nights$series[i], night = nights$night[i], arus = k, panel = names(species),
               n = vapply(species, function(s) if (s == "ALL") nrow(b) else sum(tolower(b$spp) == s), numeric(1)) / k,
               myo40 = c(sum(tolower(b$spp) %in% c("40kmyo", "40khzmyo")) / k, rep(0, length(species) - 1)))
  }))
  pd$panel <- factor(pd$panel, names(species))
  pd$series <- factor(pd$series, c(cell, mob.lab))
  top <- max(1200, ceiling(max(pd$n)))
  brk <- round(expm1(seq(0, log1p(top), length.out = 5)))
  d0 <- min(pd$night); d1 <- max(pd$night)
  xb <- unique(as.Date(round(seq(as.numeric(d0), as.numeric(d1), length.out = 4)), origin = "1970-01-01"))
  myo <- pd[pd$panel == "All detections" & pd$myo40 > 0, ]
  myo$series <- rep("40kHzMyo", nrow(myo))
  g <- ggplot(pd, aes(x = night)) +
    geom_col(aes(y = log1p(n), fill = series, group = series),
             position = position_dodge(width = 0.8, preserve = "single"), width = 0.6) +
    geom_col(data = myo, aes(y = log1p(myo40), fill = series), width = 0.9) +
    facet_wrap(~ panel, ncol = 3, drop = FALSE) +
    scale_fill_manual(values = setNames(c("red", "#1F78B4", "black"), c(cell, mob.lab, "40kHzMyo")),
                      breaks = c(cell, mob.lab, "40kHzMyo"), name = "Detector") +
    scale_y_continuous(breaks = log1p(brk), labels = format(brk, big.mark = ",", trim = TRUE),
                       limits = c(0, log1p(top))) +
    scale_x_date(breaks = xb, date_labels = "%b-%d\n%Y", limits = c(d0 - 1, d1 + 1)) +
    labs(x = "Monitoring Night", y = "Number of Auto-Processed Calls Per ARU") +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "bottom", strip.background = element_blank(),
          axis.text.x = element_text(size = 7), axis.title.x = element_text(margin = margin(t = 12)))
  f <- file.path(dir.out, sprintf("grid.mean_%s_%s_%s.png", cell, format(d0, "%m%d"), format(d1, "%m%d")))
  ggsave(f, g, width = 11.7, height = 8.7, dpi = 200)    # same size as the first.two.weeks plots
  write.csv(pd[order(pd$panel, pd$night, pd$series), ], sub("\\.png$", "_data.csv", f), row.names = FALSE)
  cat(sprintf("Saved: %s (%s - %s; ARUs per night %s-%s; mobile %s)\n", basename(f), d0, d1,
              min(pd$arus[pd$series == cell]), max(pd$arus[pd$series == cell]),
              paste(format(unique(pd$night[pd$series == mob.lab]), "%d %b"), collapse = ", ")))
  invisible(pd)
}
for (cell in c("105059", "107802", "108325", "1079", "5175", "55")) plot.cell(cell)

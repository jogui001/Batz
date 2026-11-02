## First two weeks at each grid cell - nightly observations per detector, per species
## Built 2026-11-02, per Josh. One PNG per grid cell (Seven Islands FY26).
## Window   = the cell's first monitoring night + 13 days (14 nights).
## Bars     = each stationary detector's count that night (3 recycled colors) and
##            the mobile route (grey) on its night; black overlay = 40kMyo.
## Species  = Batz manid.kp (manual ID, else Kaleidoscope auto ID); All
##            detections = every record except NOISE.
## Shading  = detector inactive periods (per Josh), when inside the window.
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
v$aru[v$aru == "1079-NE2-2"] <- "1079-NE2"                 # NE2 redeployed 30 Jul (after its inactive period)
v$mobile <- grepl("MOB", v$aru, ignore.case = TRUE)
v$cell <- sub("^MOBILE", "", sub("-.*$", "", v$aru), ignore.case = TRUE)
v$aru[v$mobile] <- "Mobile"

inactive <- data.frame(aru = c("1079-NE2", "5175-SE1", "55-NW3"),
                       start = as.Date(c("2026-07-19", "2026-07-25", "2026-07-21")),
                       end   = as.Date(c("2026-07-30", "2026-07-31", "2026-07-31")))
inactive$cell <- sub("-.*$", "", inactive$aru)

species <- c("All detections" = "ALL", "Big brown bat" = "epfu", "Eastern red bat" = "labo",
             "Hoary bat" = "laci", "Silver-haired bat" = "lano", "Eastern small-footed myotis" = "myle",
             "Little brown bat" = "mylu", "Northern long-eared bat" = "myse", "Tricolored bat" = "pesu")
cols3 <- c("steelblue", "forestgreen", "saddlebrown")

plot.cell <- function(cell) {
  d <- v[v$cell == cell, ]
  stat <- d[!d$mobile, ]
  d0 <- min(stat$night); d1 <- d0 + 13
  d <- d[d$night >= d0 & d$night <= d1, ]
  ## one row per detector-night with any record (incl. NOISE) = active that night
  dn <- unique(d[, c("aru", "night")])
  bat <- d[toupper(d$spp) != "NOISE", ]
  pd <- do.call(rbind, lapply(seq_len(nrow(dn)), function(i) {
    b <- bat[bat$aru == dn$aru[i] & bat$night == dn$night[i], ]
    data.frame(aru = dn$aru[i], night = dn$night[i], panel = names(species),
               n = vapply(species, function(s) if (s == "ALL") nrow(b) else sum(tolower(b$spp) == s), numeric(1)),
               myo40 = c(sum(tolower(b$spp) %in% c("40kmyo", "40khzmyo")), rep(0, length(species) - 1)))
  }))
  dets <- sort(unique(pd$aru[pd$aru != "Mobile"]))
  lev <- c(dets, if (any(pd$aru == "Mobile")) "Mobile")
  pd$aru <- factor(pd$aru, lev)
  pd$panel <- factor(pd$panel, names(species))
  fill <- c(setNames(rep(cols3, length.out = length(dets)), dets), Mobile = "grey60")
  brk <- c(0, 5, 20, 70, 300); mx <- max(pd$n)
  if (mx > 300) brk <- c(brk, 1000); if (mx > 1000) brk <- c(brk, 3000)
  ina <- inactive[inactive$cell == cell & inactive$start <= d1 & inactive$end >= d0, ]
  ina$xmin <- pmax(ina$start, d0) - 0.5; ina$xmax <- pmin(ina$end, d1) + 0.5
  pos <- position_dodge(width = 0.9, preserve = "single")
  g <- ggplot(pd, aes(x = night, group = aru))
  if (nrow(ina) > 0) g <- g + geom_rect(data = ina, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                                        inherit.aes = FALSE, fill = "grey93")
  g <- g +
    geom_col(aes(y = log1p(n), fill = aru), position = pos, width = 0.9) +
    geom_col(data = pd[pd$panel == "All detections", ], aes(y = log1p(myo40)), fill = "black", position = pos, width = 0.9) +
    facet_wrap(~ panel, ncol = 3, drop = FALSE) +
    scale_fill_manual(values = fill, name = NULL) +
    scale_y_continuous(breaks = log1p(brk), labels = brk, limits = c(0, log1p(max(brk)))) +
    scale_x_date(limits = c(d0 - 0.6, d1 + 0.6), breaks = seq(d0, d1 + 1, by = 7), date_labels = "%b %d", expand = c(0, 0)) +
    labs(x = "Monitoring Night", y = "Number of observations",
         caption = if (nrow(ina) > 0) paste0("Shaded: ", paste(sprintf("%s inactive %s - %s", ina$aru,
                       format(ina$start, "%d %b"), format(ina$end, "%d %b")), collapse = "; ")) else NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.text = element_text(size = 11),
          strip.background = element_blank(), strip.text = element_text(size = 11))
  f <- file.path(dir.out, sprintf("first.two.weeks_%s_%s_%s.png", cell, format(d0, "%m%d"), format(d1, "%m%d")))
  ggsave(f, g, width = 11.7, height = 8.7, dpi = 200)
  write.csv(pd[order(pd$panel, pd$night, pd$aru), ], sub("\\.png$", "_data.csv", f), row.names = FALSE)
  cat(sprintf("Saved: %s  (%s - %s, detectors: %s%s)\n", basename(f), d0, d1, paste(lev, collapse = ", "),
              if (nrow(ina) > 0) paste0("; shaded: ", paste(ina$aru, collapse = ", ")) else ""))
  invisible(pd)
}
for (cell in c("105059", "107802", "108325", "1079", "5175", "55")) plot.cell(cell)

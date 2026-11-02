## Mobile route vs stationary (mean per detector) - per species, per mobile-route night
## Built 2026-11-02, per Josh. Reads the FY26 Seven Islands FinalVetted csvs.
## Mobile night  = MonitoringNight of each mobile route (aru.name contains "MOB").
## Stationary    = the same grid cell's stationary detectors, same MonitoringNight:
##                 count per species / number of that cell's detectors with any
##                 record (incl. NOISE) that night.
## All detections = every record except NOISE; black overlay = 40kMyo.
## Aru-name fixes (per Josh): "55-NW1 new" -> "55-NW1", "107802-SW3" -> "107802-SW1".
library(ggplot2)
## needs Batz loaded (batz.merge_vetted.acoustics); or set `merged` to its $vetted.merged first
dir.in  <- if (exists("dir.in"))  dir.in  else "."
dir.out <- if (exists("dir.out")) dir.out else "."

## species per record = Batz manid.kp: manual ID, else the Kaleidoscope auto ID
## (most stationary records only have an auto ID - only vouchers were vetted)
if (!exists("merged")) {
  r <- batz.merge_vetted.acoustics(dir.load = dir.in, load.pattern = "FY26*FinalVetted.csv", dir.sub = FALSE)
  merged <- r$vetted.merged
}
v <- data.frame(aru = trimws(merged$aru.name), night = as.Date(as.character(merged$date.monitoringnight), tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
                spp = trimws(as.character(merged$manid.kp)), stringsAsFactors = FALSE)
v$aru[v$aru == "55-NW1 new"] <- "55-NW1"
v$aru[v$aru == "107802-SW3"] <- "107802-SW1"
v$type <- ifelse(grepl("MOB", v$aru, ignore.case = TRUE), "Mobile", "Stationary")
v$cell <- sub("^MOBILE", "", sub("-.*$", "", v$aru), ignore.case = TRUE)
v$cell[v$cell == "5157"] <- "5175"                       # 5157-NW1 / 5157-SE1 are 5175 detectors

species <- c("All detections" = "ALL", "Big brown bat" = "Epfu", "Eastern red bat" = "Labo",
             "Hoary bat" = "Laci", "Silver-haired bat" = "Lano", "Eastern small-footed myotis" = "Myle",
             "Little brown bat" = "Mylu", "Northern long-eared bat" = "Myse", "Tricolored bat" = "Pesu")

count.night <- function(cell, night, type) {
  d <- v[v$cell == cell & v$night == night & v$type == type, ]
  n.det <- if (type == "Mobile") 1 else length(unique(d$aru))
  bat <- d[toupper(d$spp) != "NOISE", ]
  out <- data.frame(panel = names(species), n = vapply(species, function(s)
    if (s == "ALL") nrow(bat) else sum(tolower(bat$spp) == tolower(s)), numeric(1)))
  out$n <- if (n.det > 0) out$n / n.det else NA
  out$myo40 <- c(sum(tolower(bat$spp) %in% c("40kmyo", "40khzmyo")) / max(n.det, 1), rep(0, length(species) - 1))
  out$cell <- cell; out$night <- night; out$type <- type; out$detectors <- n.det
  out
}

make.graph <- function(cells, colors, name) {
  mob <- unique(v[v$type == "Mobile" & v$cell %in% cells, c("cell", "night")])
  mob <- mob[order(mob$night), ]
  pd <- do.call(rbind, c(lapply(seq_len(nrow(mob)), function(i) count.night(mob$cell[i], mob$night[i], "Stationary")),
                         lapply(seq_len(nrow(mob)), function(i) count.night(mob$cell[i], mob$night[i], "Mobile"))))
  pd$bar <- ifelse(pd$type == "Mobile", "Mobile", paste0("Stationary (mean) - ", pd$cell))
  lev <- c(paste0("Stationary (mean) - ", cells), "Mobile")
  pd$bar <- factor(pd$bar, lev)
  pd$slot <- factor(ifelse(pd$type == "Mobile", "b", "a"))
  pd$panel <- factor(pd$panel, names(species))
  pd$xlab <- factor(format(pd$night, "%b %d"), unique(format(sort(pd$night), "%b %d")))
  brk <- c(0, 5, 20, 70, 300); if (max(pd$n, na.rm = TRUE) > 300) brk <- c(brk, 1000)
  if (max(pd$n, na.rm = TRUE) > 1000) brk <- c(brk, 3000)
  pos <- position_dodge(width = 0.9)
  g <- ggplot(pd, aes(x = xlab, group = slot)) +
    geom_col(aes(y = log1p(n), fill = bar), position = pos, width = 0.9) +
    geom_col(data = pd[pd$panel == "All detections", ], aes(y = log1p(myo40)), fill = "black", position = pos, width = 0.9) +
    facet_wrap(~ panel, ncol = 3, drop = FALSE) +
    scale_fill_manual(values = c(setNames(colors, lev[seq_along(cells)]), Mobile = "grey60"), drop = FALSE, name = NULL) +
    scale_y_continuous(breaks = log1p(brk), labels = brk, limits = c(0, log1p(max(brk)))) +
    scale_x_discrete(drop = FALSE) +
    labs(x = "Mobile-route night", y = "Number of observations") +
    theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.text = element_text(size = 10), strip.background = element_blank(),
          strip.text = element_text(size = 11))
  if (length(cells) > 3) g <- g + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  f <- file.path(dir.out, paste0(name, ".png"))
  ggsave(f, g, width = 13, height = 8.4, dpi = 200)
  tab <- pd[, c("cell", "night", "type", "detectors", "panel", "n", "myo40")]
  write.csv(tab[order(tab$cell, tab$night, tab$type), ], file.path(dir.out, paste0(name, "_data.csv")), row.names = FALSE)
  cat("Saved:", f, "\n")
  invisible(pd)
}

g1 <- make.graph(c("105059", "107802", "108325"), c("forestgreen", "steelblue", "saddlebrown"), "mobile.vs.stationary_graph1")
g2 <- make.graph(c("1079", "5175", "55"), c("#D95F02", "#6A3D9A", "#B8860B"), "mobile.vs.stationary_graph2")
## Graph 3 (2026-11-02, per Josh): all sites
g3 <- make.graph(c("105059", "107802", "108325", "1079", "5175", "55"),
                 c("forestgreen", "steelblue", "saddlebrown", "#D95F02", "#6A3D9A", "#B8860B"),
                 "mobile.vs.stationary_graph3")

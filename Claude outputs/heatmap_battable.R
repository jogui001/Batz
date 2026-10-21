## Heat map of bat observations by species (rows) and community (columns)
## from battable_KRCP_NKRCP.csv. Blank cells (missing or 0) count as 0.
## NoID left out; families and species alphabetical; each family boxed;
## the families are split into two blocks placed side by side.
library(ggplot2)

battable <- read.csv("battable_KRCP_NKRCP.csv", check.names = FALSE, stringsAsFactors = FALSE)
battable <- battable[battable$species != "NoID", ]
battable <- battable[order(battable$family, battable$species), ]      # alphabetical
krcp  <- c("ES", "MAI", "OLG", "OLK", "OLMN", "OY", "SH", "AL", "RW")
nkrcp <- c("IW", "KA", "ME", "SE", "WG")
comm.cols <- c(krcp, nkrcp)

## ---- split families into two blocks with about the same number of rows
fams <- unique(battable$family)
n.rows <- cumsum(table(factor(battable$family, levels = fams)))
split.after <- which.min(abs(n.rows - nrow(battable) / 2))
battable$block <- ifelse(match(battable$family, fams) <= split.after, 1, 2)

## ---- layout (x in tile units): family name | species name | communities
w.fam  <- 4.6      # width of the family-name column
w.spp  <- 7.4      # width of the species-name column
x.comm <- setNames(c(seq_along(krcp), length(krcp) + 0.5 + seq_along(nkrcp)), comm.cols)
w.block <- w.fam + w.spp + max(x.comm) + 0.5
block.gap <- 0.8
fam.gap <- 0.45

lay <- do.call(rbind, lapply(1:2, function(b) {
  d <- battable[battable$block == b, ]
  y <- numeric(nrow(d)); cur <- 0
  for (i in seq_len(nrow(d))) {
    if (i > 1 && d$family[i] != d$family[i - 1]) cur <- cur - fam.gap
    y[i] <- cur; cur <- cur - 1
  }
  d$y <- y
  d$x0 <- (b - 1) * (w.block + block.gap)          # left edge of this block
  d
}))

## ---- tiles
hm <- data.frame(species = rep(lay$species, length(comm.cols)),
                 y = rep(lay$y, length(comm.cols)),
                 x = rep(lay$x0 + w.fam + w.spp, length(comm.cols)) + rep(x.comm[comm.cols], each = nrow(lay)),
                 obs = as.numeric(unlist(lay[comm.cols], use.names = FALSE)))
hm$obs[is.na(hm$obs)] <- 0
class.labels <- c("0", "1-10", "11-50", "51-100", "101-500", "501-1000", "1001+")
class.colors <- c("0" = "white", "1-10" = "#a6cee3", "11-50" = "#1f78b4", "51-100" = "#fb9ac4",
                  "101-500" = "#e31a1c", "501-1000" = "#8b0000", "1001+" = "black")
hm$obs.class <- factor(class.labels[findInterval(hm$obs, c(0, 10, 50, 100, 500, 1000), left.open = TRUE) + 1],
                       levels = class.labels)

## ---- family boxes
fam <- do.call(rbind, lapply(split(lay, factor(lay$family, levels = fams)), function(d)
  data.frame(family = d$family[1], x0 = d$x0[1], ymin = min(d$y) - 0.5, ymax = max(d$y) + 0.5)))
fam$shade <- rep(c("grey95", "white"), length.out = nrow(fam))
## three boxes per family: names | KRCP | NKRCP, with a gap between each
fam$xn  <- fam$x0 + w.fam + w.spp                 # right edge of the names box
fam$xk1 <- fam$xn + 0.5; fam$xk2 <- fam$xn + length(krcp) + 0.5          # KRCP box
fam$xm1 <- fam$xk2 + 0.5; fam$xm2 <- fam$xn + max(x.comm) + 0.5          # NKRCP box
y.top <- 0.5

## ---- headings for both blocks
heads <- do.call(rbind, lapply(unique(lay$x0), function(x0)
  data.frame(x = x0 + w.fam + w.spp + x.comm, label = names(x.comm))))
proj <- do.call(rbind, lapply(unique(lay$x0), function(x0)
  data.frame(x = x0 + w.fam + w.spp + c(mean(x.comm[krcp]), mean(x.comm[nkrcp])), label = c("KRCP", "NKRCP"))))

p <- ggplot() +
  geom_rect(data = fam, aes(xmin = x0, xmax = xn, ymin = ymin, ymax = ymax),
            fill = fam$shade, colour = NA) +
  geom_tile(data = hm, aes(x = x, y = y, fill = obs.class), width = 1, height = 1,
            colour = "grey75", linewidth = 0.25, show.legend = TRUE) +
  geom_rect(data = fam, aes(xmin = x0,  xmax = xn,  ymin = ymin, ymax = ymax), fill = NA, colour = "black", linewidth = 0.6) +
  geom_rect(data = fam, aes(xmin = xk1, xmax = xk2, ymin = ymin, ymax = ymax), fill = NA, colour = "black", linewidth = 0.6) +
  geom_rect(data = fam, aes(xmin = xm1, xmax = xm2, ymin = ymin, ymax = ymax), fill = NA, colour = "black", linewidth = 0.6) +
  geom_text(data = fam, aes(x = x0 + 0.2, y = (ymin + ymax) / 2, label = family),
            hjust = 0, fontface = "bold", size = 3) +
  geom_text(data = lay, aes(x = x0 + w.fam, y = y, label = species),
            hjust = 0, fontface = "italic", size = 2.9) +
  geom_text(data = heads, aes(x = x, y = y.top + 0.25, label = label), size = 2.8, angle = 90, hjust = 0) +
  geom_text(data = proj, aes(x = x, y = y.top + 2.6, label = label), fontface = "bold", size = 3.6) +
  scale_fill_manual(values = class.colors, limits = class.labels, drop = FALSE,
                    name = "Number of Observations") +
  coord_cartesian(xlim = c(-0.1, 2 * w.block + block.gap + 0.1),
                  ylim = c(min(fam$ymin) - 0.1, y.top + 3.1), expand = FALSE, clip = "off") +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom",
        legend.key      = element_rect(colour = "grey60"),
        plot.margin     = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "white", colour = NA)) +
  guides(fill = guide_legend(nrow = 1, title.position = "left", title.vjust = 0.5))

ggsave("heatmap_battable.png", plot = p, dpi = 300, width = 13.8, height = 6.5, units = "in")

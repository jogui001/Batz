## Number of bat species per community, stacked by family
## (matches the earlier ES/MAI/OLG/... species-richness plot)
library(ggplot2)

plotcall2 <- read.csv("plotcall2.csv")

## ---- family colours (legend order = alphabetical, as in the original)
Emballonuridae.color   <- "#ff7f00"
Hipposideridae.color   <- "black"
Megadermatidae.color   <- "#33a02c"
Miniopteridae.color    <- "#fb9a99"
Molossidae.color       <- "#e31a1c"
Nycteridae.color       <- "#6a3d9a"
Rhinolophidae.color    <- "#1f78b4"
Vespertilionidae.color <- "#fdbf6f"
family.colors <- c(Emballonuridae = Emballonuridae.color, Hipposideridae = Hipposideridae.color,
                   Megadermatidae = Megadermatidae.color, Miniopteridae = Miniopteridae.color,
                   Molossidae = Molossidae.color, Nycteridae = Nycteridae.color,
                   Rhinolophidae = Rhinolophidae.color, Vespertilionidae = Vespertilionidae.color)

## ---- $family has some genus/species names in it - map them to their family.
## Rhinonycteridae (Triaenops afer) has no colour in the original key; it was
## formerly placed in Hipposideridae, so it is plotted there. To show it as its
## own family instead, delete that line and add a colour for it above.
family.fix <- c("Miniopterus sp"          = "Miniopteridae",
                "Mops condylurus pumilus" = "Molossidae",
                "Nycteris sp"             = "Nycteridae",
                "Tadarida sp"             = "Molossidae",
                "Taphozous sp"            = "Emballonuridae",
                "Rhinonycteridae"         = "Hipposideridae")
plotcall2$family2 <- ifelse(plotcall2$family %in% names(family.fix),
                            family.fix[plotcall2$family], plotcall2$family)

## ---- species detected (obs >= 1): one row per community x species
spp.detected <- plotcall2[as.numeric(plotcall2$obs) >= 1, ]
spp.detected$family2 <- factor(spp.detected$family2, levels = names(family.colors))
spp.detected$n <- 1

## ---- black numbers at the bottom of each bar
bar.labels <- data.frame(community = c("IW", "KA", "ME", "SE", "WG"),
                         label     = c(7, 8, 6, 6, 9))

p <- ggplot(spp.detected, aes(x = community, y = n, fill = family2)) +
  geom_col(width = 0.9) +
  geom_text(data = bar.labels, aes(x = community, y = 1.9, label = label),
            inherit.aes = FALSE, size = 4.5, colour = "black") +
  scale_fill_manual(values = family.colors, drop = FALSE, name = "Bat Family:") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Community", y = "Number of Species") +
  theme_gray(base_size = 14) +
  theme(axis.title = element_text(size = 17),
        axis.text  = element_text(size = 12),
        legend.position = "bottom",
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.55, "cm"),
        plot.margin = margin(5.5, 12, 5.5, 5.5)) +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE, title.position = "top", title.hjust = 0.5))

ggsave("species_by_community.png", plot = p, dpi = 300, height = 4.05, width = 5.1, units = "in")

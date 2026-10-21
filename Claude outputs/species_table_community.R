## Species table, community level (NKRCP)
## One row per Family x Species x Community. "Detected" = 1 if the species
## had obs > 0 in that community, 0 if not.
plotcall2 <- read.csv("plotcall2.csv")

family.fix <- c("Miniopterus sp" = "Miniopteridae", "Mops condylurus pumilus" = "Molossidae",
                "Nycteris sp" = "Nycteridae", "Tadarida sp" = "Molossidae",
                "Taphozous sp" = "Emballonuridae", "Rhinonycteridae" = "Hipposideridae")
plotcall2$family2 <- ifelse(plotcall2$family %in% names(family.fix),
                            family.fix[plotcall2$family], plotcall2$family)

spp.table <- data.frame(
  Family    = plotcall2$family2,
  Species   = plotcall2$spp,
  Project   = "NKRCP",
  Community = plotcall2$community,
  Sites     = as.integer(plotcall2$sites),               # sites in the community
  Detected  = as.integer(as.numeric(plotcall2$obs) > 0)  # 1 = obs > 0 in that community
)
spp.table <- spp.table[order(spp.table$Family, spp.table$Species, spp.table$Community), ]
rownames(spp.table) <- NULL
write.csv(spp.table, "species_table_community.csv", row.names = FALSE)

## check: species detected per community (should match the bar heights)
print(tapply(spp.table$Detected, spp.table$Community, sum))

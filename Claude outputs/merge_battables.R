## Merge KRCP 2023 + 2024 bat tables (communities as columns), summing
## communities found in both, then add NKRCP communities (IW, KA, ME, SE, WG)
## from plotcall2.csv.
t23 <- read.csv("krcp2023.csv", check.names = FALSE, stringsAsFactors = FALSE)
t24 <- read.csv("krcp2024.csv", check.names = FALSE, stringsAsFactors = FALSE)
nk  <- read.csv("plotcall2.csv", stringsAsFactors = FALSE)

## ---- one spelling per species (so rows line up), and fix family entries
species.fix <- c("Nycticeinops schlieffenii" = "Nycticeinops schlieffeni",
                 "Rhinilophus fumigatus"     = "Rhinolophus fumigatus",
                 "Afronycteris nana"         = "Afronycteris nanus",
                 ## NKRCP spellings -> KRCP spellings
                 "Miniopterus sp"            = "Miniopterus spp.",
                 "Mops condylurus pumilus"   = "Mops condylurus/pumilus",
                 "Nycteris sp"               = "Nycteris spp.",
                 "Tadarida sp"               = "Tadarida spp.",
                 "Taphozous sp"              = "Taphozous spp.")
family.of <- c("Nycticeinops schlieffeni" = "Vespertilionidae",
               "Rhinolophus fumigatus"    = "Rhinolophidae",
               "Afronycteris nanus"       = "Vespertilionidae",
               "Miniopterus spp."         = "Miniopteridae",
               "Mops condylurus/pumilus"  = "Molossidae",
               "Nycteris spp."            = "Nycteridae",
               "Tadarida spp."            = "Molossidae",
               "Taphozous spp."           = "Emballonuridae")
tidy.names <- function(df) {
  df$species <- ifelse(df$species %in% names(species.fix), species.fix[df$species], df$species)
  df$family  <- ifelse(df$species %in% names(family.of), family.of[df$species], df$family)
  df
}

## ---- KRCP tables -> long (family, species, community, obs)
to.long <- function(df) {
  comm <- setdiff(names(df), c("", "year", "family", "species"))
  data.frame(family = rep(df$family, length(comm)), species = rep(df$species, length(comm)),
             community = rep(comm, each = nrow(df)),
             obs = as.numeric(unlist(df[comm], use.names = FALSE)), stringsAsFactors = FALSE)
}
long <- rbind(to.long(tidy.names(t23)), to.long(tidy.names(t24)))

## ---- NKRCP (already long) -> same shape
names(nk)[names(nk) == "spp"] <- "species"
nk <- tidy.names(nk)
long <- rbind(long, data.frame(family = nk$family, species = nk$species, community = nk$community,
                               obs = as.numeric(nk$obs), stringsAsFactors = FALSE))

## ---- sum duplicates (same species + community, e.g. MAI/OLG in 2023 and 2024) and go wide
agg <- aggregate(obs ~ family + species + community, data = long, FUN = sum)
comm.order <- c("ES", "MAI", "OLG", "OLK", "OLMN", "OY", "SH", "AL", "RW",   # KRCP
                "IW", "KA", "ME", "SE", "WG")                               # NKRCP
battable <- reshape(agg, idvar = c("family", "species"), timevar = "community", direction = "wide")
names(battable) <- sub("^obs\\.", "", names(battable))
battable[setdiff(comm.order, names(battable))] <- NA
battable <- battable[, c("family", "species", comm.order)]
## blank (NA) for a species missing from a data set AND for a count of 0
battable[comm.order][!is.na(battable[comm.order]) & battable[comm.order] == 0] <- NA

## ---- row order: families/species in the 2023 table's order, new ones at the end of their family
ord.sp <- unique(c(tidy.names(t23)$species, tidy.names(t24)$species, battable$species))
fam.order <- unique(battable$family[match(ord.sp, battable$species)])
battable <- battable[order(match(battable$family, fam.order), match(battable$species, ord.sp)), ]
rownames(battable) <- NULL

write.csv(battable, "battable_KRCP_NKRCP.csv", row.names = FALSE, na = "")

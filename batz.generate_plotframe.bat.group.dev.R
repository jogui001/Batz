## batz.generate_plotframe.bat.group.dev.R
## Dev/test script for batz.generate_plotframe.bat.group() - added 2026-10-14
## as batz.generate_plotframe.conservancyrate(), renamed 2026-10-21, per Josh.
##
## KEEP THIS FILE OUTSIDE THE PACKAGE'S R/ FOLDER - it has top-level code that
## reads test-data paths and would break devtools::load_all()/document().
##
## Model data: Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv (South project,
## 42,487 records, 2023 + 2024, 79 ARUs, 9 communities), after the 2026-10-14
## clean-up (lowercase headers, SITE -> aru.name, MANUAL ID -> species, new
## $community and $project columns, Basecamp ARUs recoded, latitudes set to south).

## ---- CONFIG - point these at your own copies ---------------------------------
dir.pkg   <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz/R"
file.data <- "Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv"
file.south <- "South.csv"   # optional - per-ARU-night South summary used as a cross-check (TEST 10)
dir.out   <- tempdir()

## ---- load the package helpers + the function under test -------------------
for (f in c("batz.util_standardize.headers.R", "batz.util_special.characters.R",
            "batz.generate_plotframe.bat.group.R")) {
  source(file.path(dir.pkg, f))
}

vetted <- read.csv(file.data, check.names = FALSE, stringsAsFactors = FALSE)
vetted.copy <- vetted

## South project species list (28 species/classes, as listed in South.csv)
spp <- c("insect/noise", "Nycticeinops schlieffeni", "Scotophilus andrewreborii",
         "Mops condylurus/pumilus", "Otomops harrisoni", "Miniopterus africanus",
         "Tadarida spp.", "Laephotis kirinyaga", "Pipistrellus hesperidus",
         "Neoromicia somalica", "Scotoecus hirundo", "Miniopterus spp.",
         "Tadarida aegyptiaca", "Eptesicus hottentotus", "Nycteris spp.",
         "Lavia frons", "Cardioderma cor", "NoID", "Taphozous mauritianus/perforatus",
         "Rhinolophus fumigatus", "Afronycteris nanus", "Rhinolophus webalai",
         "Coleura afra", "Rhinolophus eloquens", "Myotis spp", "Rhinolophus simulator",
         "Hipposideros caffer", "Rhinolophus hildebrandtii")

pass <- function(name) cat("PASS -", name, "\n")

## ---- TEST 1: full run on real data -------------------------------------------
w <- NULL
res <- withCallingHandlers(
  batz.generate_plotframe.bat.group(vetted, species.list = spp,
                                    groupby.date = "date-12", dir.save = dir.out,
                                    project.name = "south"),
  warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
stopifnot(identical(names(res), c("year", "community", "species", "family", "observation_count",
                                  "sites_sampled", "sites_detected",
                                  "date.monitoringnight_sampled",
                                  "date.monitoringnight_detected", "project")))
grps <- unique(res[, c("project", "year", "community")])
stopifnot(nrow(res) == nrow(grps) * length(spp))           # full grid
stopifnot(all(table(paste(res$year, res$community)) == length(spp)))
pass("output columns + one row per year/community/species")

## ---- TEST 2: values match a direct, independent count ------------------------
v <- unique(vetted); v$night <- v[["date-12"]]
for (i in sample(which(res$year != "all"), 40)) {
  r <- res[i, ]
  sub <- v[v$year == as.integer(r$year) & v$community == r$community & v$project == r$project, ]
  hit <- sub[sub$species == r$species, ]
  stopifnot(r$observation_count == nrow(hit),
            r$sites_sampled == length(unique(sub$aru.name)),
            r$sites_detected == length(unique(hit$aru.name)),
            r$date.monitoringnight_sampled == nrow(unique(sub[, c("aru.name", "night")])),
            r$date.monitoringnight_detected == nrow(unique(hit[, c("aru.name", "night")])))
}
pass("40 random rows match a direct subset count")

## ---- TEST 3: year = "all" pools every year -----------------------------------
for (i in sample(which(res$year == "all"), 20)) {
  r <- res[i, ]
  sub <- v[v$community == r$community & v$project == r$project, ]
  hit <- sub[sub$species == r$species, ]
  stopifnot(r$observation_count == nrow(hit),
            r$sites_sampled == length(unique(sub$aru.name)),
            r$date.monitoringnight_detected == nrow(unique(hit[, c("aru.name", "night")])))
}
yrs <- res[res$year != "all", ]
all <- res[res$year == "all", ]
stopifnot(sum(all$observation_count) == sum(yrs$observation_count))
pass("year = 'all' rows match a direct count; total observations equal the per-year sum")

## ---- TEST 4: undetected species are zero-filled ------------------------------
z <- res[res$observation_count == 0, ]
stopifnot(nrow(z) > 0, all(z$sites_detected == 0), all(z$date.monitoringnight_detected == 0),
          all(z$sites_sampled > 0))
pass(sprintf("%d zero-filled rows, sampling effort still filled in", nrow(z)))

## ---- TEST 5: spelling mismatches between data and species.list are warned ---
stopifnot(any(grepl("Afronycteris nana", w)))   # data uses 'nana', the list above uses 'nanus'
pass("off-list species warning names 'Afronycteris nana'")

## ---- TEST 6: header spellings (snake_case / spaces / caps) are accepted ------
v2 <- vetted
names(v2)[names(v2) == "aru.name"] <- "ARU Name"
names(v2)[names(v2) == "date-12"]  <- "date_monitoringnight"
res2 <- suppressWarnings(batz.generate_plotframe.bat.group(v2, species.list = spp,
                                                           write.output = FALSE))
stopifnot(identical(res2, res))
pass("'ARU Name' / 'date_monitoringnight' accepted, identical result")

## ---- TEST 7: missing header error -------------------------------------------
e <- tryCatch(batz.generate_plotframe.bat.group(vetted[, setdiff(names(vetted), "community")],
                                               groupby.date = "date-12", write.output = FALSE),
              error = function(x) conditionMessage(x))
stopifnot(grepl("missing these headers: community", e))
pass("missing $community stops with a clear message")

## ---- TEST 8: species.list = NULL uses every species in data ------------------
res3 <- batz.generate_plotframe.bat.group(vetted, groupby.date = "date-12", write.output = FALSE)
stopifnot(setequal(unique(res3$species), unique(vetted$species)))
pass("species.list = NULL -> every species in the data")

## ---- TEST 9: snake_case output, file written, caller data untouched ---------
res4 <- suppressWarnings(batz.generate_plotframe.bat.group(vetted, species.list = spp,
                         groupby.date = "date-12", write.output = FALSE, snake_case = TRUE))
stopifnot("date_monitoringnight_sampled" %in% names(res4))
saved <- list.files(dir.out, pattern = "^south_bat\\.group_20230425to20240520_[0-9]{14}\\.csv$")
stopifnot(length(saved) >= 1, identical(vetted, vetted.copy))
pass("snake_case output; file saved as south_bat.group_<dates>_<timestamp>.csv; input unchanged")

## ---- TEST 9b: duplicates.remove ------------------------------------------
## The model data has MAI10's 2024 records entered twice (290 exact duplicates)
dupn <- sum(duplicated(vetted))
r.keep <- suppressWarnings(batz.generate_plotframe.bat.group(vetted, species.list = spp,
          groupby.date = "date-12", write.output = FALSE, duplicates.remove = FALSE))
stopifnot(sum(r.keep$observation_count[r.keep$year != "all"]) -
          sum(res$observation_count[res$year != "all"]) ==
          sum(duplicated(vetted) & vetted$species %in% spp))
pass(sprintf("duplicates.remove drops the %d duplicate rows (FALSE keeps them)", dupn))

## ---- TEST 9c: $family ------------------------------------------------------
fam <- unique(res[, c("species", "family")])
stopifnot(fam$family[fam$species == "Mops condylurus/pumilus"] == "Molossidae",
          fam$family[fam$species == "insect/noise"] == "",
          fam$family[fam$species == "Rhinolophus webalai"] == "Rhinolophidae")
## misspelt genus (the real data was corrected 2026-10-14, so use a modified copy)
vbad <- vetted; vbad$species[vbad$species == "Rhinolophus fumigatus"] <- "Rhinilophus fumigatus"
w2 <- tryCatch(batz.generate_plotframe.bat.group(vbad, groupby.date = "date-12",
               write.output = FALSE), warning = conditionMessage)
stopifnot(grepl("No \\$family found for: Rhinilophus fumigatus", w2))
res5 <- suppressWarnings(batz.generate_plotframe.bat.group(vbad, species.list = c(spp, "Rhinilophus fumigatus"),
         groupby.date = "date-12", write.output = FALSE,
         family.ref = data.frame(Genus = "Rhinilophus", Family = "Rhinolophidae")))
stopifnot(all(res5$family[res5$species == "Rhinilophus fumigatus"] == "Rhinolophidae"),
          all(res5$family[res5$species == "Coleura afra"] == "Emballonuridae"))
e <- tryCatch(batz.generate_plotframe.bat.group(vetted, groupby.date = "date-12",
              write.output = FALSE, family.ref = data.frame(x = 1)), error = conditionMessage)
stopifnot(grepl("needs a `family` column", e))
pass("$family from built-in genus table; non-bat classes blank; misspelt genus warned; family.ref fills/overrides")

## ---- TEST 10 (optional): cross-check against South.csv ----------------------
## South.csv is a per-ARU-per-night summary of the same 2024 records - summing
## its $obs by community/species should match observation_count for 2024.
if (file.exists(file.south)) {
  s <- read.csv(file.south, check.names = FALSE, stringsAsFactors = FALSE)
  s.sum <- aggregate(obs ~ community + species, data = s, FUN = sum)
  r24 <- res[res$year == "2024", ]
  j <- match(paste(s.sum$community, s.sum$species), paste(r24$community, r24$species))
  cmp <- data.frame(s.sum, function.obs = r24$observation_count[j])
  cmp <- cmp[!is.na(cmp$function.obs), ]
  diff <- cmp[cmp$obs != cmp$function.obs, ]
  cat(sprintf("TEST 10 - %d community/species pairs compared with South.csv, %d differ\n",
              nrow(cmp), nrow(diff)))
  if (nrow(diff) > 0) print(head(diff, 20))
}

cat("\nAll tests passed.\n")

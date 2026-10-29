# =============================================================================
# batz.generate_aru.appendix.dev.R  -  dev/test script (2026-10-27, per Josh; TEST 1f added 2026-10-28)
# Keep this file OUTSIDE the package's R/ folder.
#
# Builds the stage-1 ARU site appendices from the Survey123 deployment /
# quadrant / servicing export and checks the input-handling rules.
# Needs: officer, xml2, readxl (install.packages("officer") - readxl comes
# with the tidyverse); magick for HEIC photos; ggplot2 etc. for Figures 4/5. PDF needs the doconv package (Word) or LibreOffice.
# =============================================================================

## ---- paths: edit these ------------------------------------------------------
dir.repo  <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
file.data <- "FY26_SevenIslands_NABat_Aru_DeploymentService.xlsx"  # Survey123 export (renamed 2026-10-28 from ...DeploymentSurice.xlsx)
file.inventory <- "survey123 equipment database.xlsx"              # equipment list
dir.photos <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/3 All test data/NABAT Test Data/ARU Photos"
file.sm4  <- "mergedsm4.csv"                                       # merged SM4 log
dir.out   <- file.path(tempdir(), "aru.appendix.test")

## ---- load the functions from the repo (no install needed) ------------------
## (all of R/ - Figures 4/5 use batz.plotcover_bullseye(), batz.plotsm4_heatmap()
##  and batz.generate_suntimes.arulist())
for (f in list.files(file.path(dir.repo, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
template <- file.path(dir.repo, "inst", "templates", "batz.aru.appendix.terrestrial.template.docx")

## ---- TEST 1: full run, Word + PDF, collated + separate ----------------------
r <- batz.generate_aru.appendix(
  data = file.data,
  client = "Wildlife Management Institute", client.contact = "Meghan Gilbart",
  project.name = "NA Bat", project.label = "Maine NABAT",
  project.code = "MAMM-NG26-007", project.manager = "Joshua Guilbert",
  word = TRUE, pdf = TRUE, collate = TRUE, separate = TRUE,
  dir.save = dir.out, template = template)
stopifnot(nrow(r$sites) == 19)                       # 19 deployed sites
se1 <- r$sites[r$sites$aru.name == "5175-SE1", ]
stopifnot(se1$state == "Maine", se1$site.type == "Inland",
          se1$aru.make.model.serial == "Wildlife Acoustics, SM4BatFS, S4U28628",
          se1$recovery.notes == "5.1V, 11 GB Card A")
print(t(se1))

## ---- TEST 1b: show.time = TRUE adds the time to every date --------------
r1b <- batz.generate_aru.appendix(
  data = file.data, project.name = "NA Bat", show.time = TRUE,
  collate = TRUE, separate = FALSE, dir.save = file.path(dir.out, "with.time"), template = template)
stopifnot(r1b$sites$date.deployment[r1b$sites$aru.name == "5175-SE1"] == "7/19/2026 18:51",
          r$sites$date.deployment[r$sites$aru.name == "5175-SE1"] == "7/19/2026")

## ---- TEST 1c: replaced equipment + equipment.inventory --------------------
## (test data has no replacements, so one is added to 5175-SE1's 7/31 visit;
##  the inventory is made up from the deployment serials)
sh <- readxl::excel_sheets(file.data)
d <- lapply(sh, function(s) as.data.frame(readxl::read_excel(file.data, s, .name_repair = "minimal"), check.names = FALSE))
names(d) <- sh
i <- which(d$survice[["Site Name"]] == "5175-SE1" & d$survice[["Reason for site visit"]] == "sitevisit_maintenance")
d$survice[["New ARU serial number"]][i] <- "S4U99999"
dep <- d$deploy
inv <- data.frame(`Serial Number` = c(dep[["Serial Number of Detector"]], dep[["Serial Number of Microphone"]], "S4U99999"),
                  Make = "Wildlife Acoustics",
                  Model = c(rep("SM4BAT-FS", nrow(dep)), rep("SMM-U2", nrow(dep)), "SM4BAT-FS"),
                  `Equipment Type` = c(rep("ARU", nrow(dep)), rep("Microphone", nrow(dep)), "ARU"), check.names = FALSE)
r1c <- batz.generate_aru.appendix(d, project.name = "NA Bat", collate = TRUE, separate = FALSE,
                                  dir.save = file.path(dir.out, "inventory"), template = template,
                                  equipment.inventory = inv)
print(r1c$serial.check)
stopifnot(any(grepl("S4U28628", r1c$serial.check$serial)),                 # retrieval form still has the old ARU
          r1c$sites$microphone.make.model.serial[r1c$sites$aru.name == "5175-SE1"] ==
            "Wildlife Acoustics, SMM-U2, MU236535")                         # make/model filled from inventory

## ---- TEST 1d: the real equipment database ---------------------------------
r1d <- batz.generate_aru.appendix(file.data, project.name = "NA Bat", collate = TRUE, separate = FALSE,
                                  dir.save = file.path(dir.out, "inventory.real"), template = template,
                                  equipment.inventory = file.inventory)     # path or data frame
print(r1d$serial.check)    # 55-SW1: deployment mic MU236246 is not in the database (MU236346 is)
stopifnot(any(r1d$serial.check$serial == "MU236246" & grepl("not in", r1d$serial.check$issue)),
          !any(r1d$serial.check$form == "equipment.inventory"))   # no serial under two equipment types
se1d <- r1d$sites[r1d$sites$aru.name == "5175-SE1", ]
stopifnot(se1d$power.make.model.serial == "Internal power, 4 x D Batteries",   # no power info -> default
          se1d$recovery.microphone.height == se1d$microphone.height)          # recovery repeats deployment

## ---- TEST 1e: equipment not "Working" in the database is reported ----------
d2 <- d; d2$deploy[["Serial Number of Detector"]][1] <- "S4U23362"            # Retired in the database
r1e <- batz.generate_aru.appendix(d2, project.name = "NA Bat", collate = TRUE, separate = FALSE,
                                  dir.save = file.path(dir.out, "inventory.status"), template = template,
                                  equipment.inventory = file.inventory)
stopifnot(any(grepl("status: Retired", r1e$serial.check$issue)))

## ---- TEST 1f: new model (2026-10-28) - habitat table, photos, Fig 4 and 5 -
## HEIC photos need install.packages("magick")
plotopts.bull <- file.path(dir.repo, "inst", "plotopts", "plotopts_bullseye.csv")
r1f <- batz.generate_aru.appendix(file.data, client = "Wildlife Management Institute",
                                  client.contact = "Meghan Gilbart", project.name = "NA Bat",
                                  project.label = "Maine NABAT", project.code = "MAMM-NG26-007",
                                  project.manager = "Joshua Guilbert", pdf = TRUE,
                                  collate = TRUE, separate = FALSE,
                                  dir.save = file.path(dir.out, "new.model"), template = template,
                                  equipment.inventory = file.inventory,
                                  photo.dir = dir.photos, sm4.data = file.sm4,
                                  plotopts.bullseye = plotopts.bull,
                                  plotopts.sm4heatmap = file.path(dir.repo, "inst", "plotopts", "plotopts_sm4heatmap.csv"))
## species: each once, most often recorded first
se1f <- r1f$sites[r1f$sites$aru.name == "5175-SE1", ]
hab.se1 <- r1f$habitat[r1f$habitat$aru.name == "5175-SE1", ]
print(t(hab.se1))
sp <- strsplit(hab.se1$tree.species.all[1], ", ")[[1]]
stopifnot(!any(duplicated(tolower(sp))))
## 2026-10-28: dominant, then all species, then quadrant species by
## frequency; "Balsalm Fir" (form, once) is shown as "Balsam Fir" (quadrants, twice)
stopifnot(r1f$habitat$tree.species.all[r1f$habitat$aru.name == "105059-SE3"][1] ==
            "Balsam Fir, White Pine, Spruce, Birch",
          r1f$sites$site.water.distance[r1f$sites$aru.name == "105059-SE3"] == "1 m",
          r1f$sites$microphone.height[r1f$sites$aru.name == "105059-SE3"] == "4 m",
          r1f$sites$date.recovery_planned[r1f$sites$aru.name == "105059-NW3"] == "8/5/2026",   # 7/8 + 28 days
          r1f$sites$survey.schedule[r1f$sites$aru.name == "105059-SE3"] ==
            "Continuous -30min Dusk to +30min Dawn")
## photos: 5175-SE1 has all six views; 107802-NW1 matched by its folder name;
## 55-NW1 uses the Original (not "New") photos
stopifnot(se1f$photo.north == "", r1f$sites$photo.west[r1f$sites$aru.name == "107802-NW1"] == "")
## Fig 5: mergedsm4 names 5157-NW1/5157-SE1 (5175?), so 5175-SE1 has no log
stopifnot(se1f$figure.sm4 == "No SM4 log data for this site",
          r1f$sites$figure.sm4[r1f$sites$aru.name == "5175-SW3"] == "")

## ---- TEST 1g: spelling corrections (2026-10-28) - a deployment form that
## already uses the OLD Batz header spellings still fills the report
d3 <- d
names(d3$deploy)[names(d3$deploy) == "Understory Clutter"] <- "vegetation.understroy_clutter"
names(d3$deploy)[names(d3$deploy) == "Horizontal Microphone Orientation"] <- "microphone.orientation_horizonal"
r1g <- batz.generate_aru.appendix(d3, project.name = "NA Bat", collate = TRUE, separate = FALSE,
                                  dir.save = file.path(dir.out, "old.spelling"), template = template)
stopifnot("vegetation.understory_clutter" %in% names(r1g$aru.habitat),
          "microphone.orientation_horizontal" %in% names(r1g$aru.habitat),
          r1g$habitat$understory.clutter[r1g$habitat$aru.name == "5175-SE1"][1] ==
            "Minimal clutter (open understory facilitates flight)",
          r1g$sites$microphone.orientation_horizontal[r1g$sites$aru.name == "5175-SE1"] == "56")

## ---- TEST 2: a missing tab - asks Y/N (or set continue.missing) ------------
dep <- as.data.frame(readxl::read_excel(file.data, "deploy", .name_repair = "minimal"), check.names = FALSE)
r2 <- batz.generate_aru.appendix(list(deploy = dep), project.name = "NA Bat", template = template,
                                 dir.save = dir.out, continue.missing = TRUE, separate = FALSE)

## ---- TEST 3: no required tab - stops ----------------------------------------
try(batz.generate_aru.appendix(list(photos = data.frame(a = 1)), template = template))

## ---- TEST 4: word and pdf both FALSE - stops -------------------------------
try(batz.generate_aru.appendix(file.data, word = FALSE, pdf = FALSE, template = template))

list.files(dir.out)

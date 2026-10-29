# =============================================================================
# batz.merge_sitevisit.acoustics.dev.R  -  dev/test script (2026-10-29, per Josh)
# Keep this file OUTSIDE the package's R/ folder.
#
# Merges Survey123 ARU site-visit exports (habitat assessment / deployment,
# quadrant, maintenance & retrieval) into one workbook with Batz headers.
# Needs: readxl, openxlsx (or writexl).
# =============================================================================

## ---- paths: edit these ------------------------------------------------------
dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
dir.data <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/3 All test data/NABAT Test Data"
dir.out  <- file.path(tempdir(), "merge_sitevisit.test")

## ---- load the functions from the repo (no install needed) ------------------
for (f in c("batz.util_survey123.reference.R", "batz.util_survey123.aruforms.R", "batz.merge_sitevisit.acoustics.R")) {
  source(file.path(dir.repo, "R", f))
}

## ---- TEST 1: the two FY26 exports (default load.pattern finds them) ---------
m <- batz.merge_sitevisit.acoustics(dir.load = dir.data, dir.sub = TRUE, dir.save = dir.out)
print(m$log)
stopifnot(nrow(m$habitat) == 19, nrow(m$quad) == 80, nrow(m$service) == 24,
          names(m$habitat)[1] == "form.version",
          all(m$habitat$form.version == "inland_2026"),
          all(m$quad$form.version == "forest_2026"),
          all(m$service$form.version == "service_2026"),
          is.na(m$log$tab[m$log$sheet == "repeat_photo_1"]),       # photo repeat sheet skipped
          "measurement.units" %in% names(m$quad),                   # 2026-10-29 spelling
          "microphone.calibration_value" %in% names(m$service))
str(Survey123.aruforms_headers.choices, 1)                          # the database: quad, habitat, service, Choices

## ---- TEST 1b: a sheet with Batz headers in the earlier spellings ------------
q.old <- m$quad; names(q.old)[names(q.old) == "measurement.units"] <- "measurment.units"
openxlsx::write.xlsx(list(quad = q.old), file.path(dir.out, "old.spelling_deployments.xlsx"), overwrite = TRUE)
m1b <- batz.merge_sitevisit.acoustics(dir.out, load.pattern = "old.spelling*", dir.save = file.path(dir.out, "1b"))
stopifnot(all(m1b$quad$measurement.units == m$quad$measurement.units, na.rm = TRUE))
openxlsx::getSheetNames(m$file)                                     # quad, habitat, service

## ---- TEST 2: form.list adds a form version and a new tab -------------------
h <- Survey123.aruforms_headers.choices$habitat
newver <- data.frame(`aru.habitat_Batz.headers` = h$`aru.habitat_Batz.headers`, Batz.headers = h$Batz.headers,
                     coastal_2027 = paste0(h$inland_2026, " (2027)"), check.names = FALSE)
newtab <- data.frame(`aru.mobile_Batz.headers` = c("aru.mobile$aru.name", "aru.mobile$date", "aru.mobile$route"),
                     Batz.headers = c("aru.name", "date", "route.name"), mobile_2027 = c("Site", "Date", "Route"),
                     check.names = FALSE)
d3 <- file.path(dir.out, "data3"); dir.create(d3, recursive = TRUE, showWarnings = FALSE)
openxlsx::write.xlsx(list(mob = data.frame(Site = c("A1", "A2"), Date = "2027-07-01", Route = "R1")),
                     file.path(d3, "mobile_deployments.xlsx"), overwrite = TRUE)
m2 <- batz.merge_sitevisit.acoustics(d3, form.list = list(habitat = newver, newtab), dir.save = d3)
stopifnot("mobile" %in% names(m2), nrow(m2$mobile) == 2, m2$mobile$route.name[1] == "R1",
          "coastal_2027" %in% names(m2$headers.db$habitat),            # version added as a column
          all(c("inland_2026", "coastal_2027") %in% names(m2$headers.db$habitat)),
          "mobile" %in% names(m2$headers.db))

## ---- TEST 3: errors ---------------------------------------------------------
try(batz.merge_sitevisit.acoustics(dir.data, load.pattern = "*nothing*.xlsx"))          # no files
try(batz.merge_sitevisit.acoustics(d3, form.list = list(bad = data.frame(a = 1, b = 2)))) # bad tab

# =============================================================================
# batz.merge_sitevisit.acoustics.dev.R  -  dev/test script (2026-10-29, per Josh)
# Keep this file OUTSIDE the package's R/ folder.
#
# Merges Survey123 ARU site-visit exports (habitat assessment / deployment,
# quadrant, maintenance & retrieval) into one workbook with Batz headers.
# Needs: readxl, openxlsx.
# =============================================================================

## ---- paths: edit these ------------------------------------------------------
dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
dir.data <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/3 All test data/NABAT Test Data"
dir.out  <- file.path(tempdir(), "merge_sitevisit.test")

## ---- load the functions from the repo (no install needed) ------------------
for (f in c("batz.util_survey123.reference.R", "batz.util_survey123.aruforms.R", "batz.merge_sitevisit.acoustics.R",
            "batz.datawrangler_consolidate.globalID.R")) {
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
newver <- data.frame(`aru.habitat_batz.headers` = h$`aru.habitat_batz.headers`, batz.headers = h$batz.headers,
                     coastal_2027 = paste0(h$inland_2026, " (2027)"), check.names = FALSE)
newtab <- data.frame(`aru.mobile_batz.headers` = c("aru.mobile$aru.name", "aru.mobile$date", "aru.mobile$route"),
                     batz.headers = c("aru.name", "date", "route.name"), mobile_2027 = c("Site", "Date", "Route"),
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

## ---- TEST 4: workbooks that can't be opened (2026-10-30) --------------------
## 4a: open the FY26 SiteMaintenance & Retrieval workbook in Excel, then run:
try(batz.merge_sitevisit.acoustics(dir.data, dir.save = dir.out))   # "<file> is open in another program ..."
## 4b-4d: simulated bad files
d4 <- file.path(dir.out, "data4"); dir.create(d4, showWarnings = FALSE)
bad <- file.path(d4, "x_deployments.xlsx")
file.create(bad)                                  ; try(batz.merge_sitevisit.acoustics(d4, dir.save = d4)) # 0 bytes
writeLines("hello", bad)                          ; try(batz.merge_sitevisit.acoustics(d4, dir.save = d4)) # not an .xlsx
writeBin(as.raw(c(0xd0, 0xcf, 0x11, 0xe0)), bad) ; try(batz.merge_sitevisit.acoustics(d4, dir.save = d4)) # password / old .xls

## ---- TEST 5: consolidated GlobalID (2026-10-30) -----------------------------
stopifnot(names(m$habitat)[match("globalid", names(m$habitat)) + 1] == "globalid_consolidate",
          "globalid_consolidate" %in% names(m$service),
          "globalid_consolidate" %in% names(m$quad))                        # quad gets project.name from its habitat record (2026-10-30)
## one ID per site across habitat + service
s.all <- rbind(m$habitat[, c("project.name", "aru.name", "globalid_consolidate")],
               m$service[, c("project.name", "aru.name", "globalid_consolidate")])
stopifnot(all(tapply(s.all$globalid_consolidate, paste(s.all$project.name, s.all$aru.name),
                     function(v) length(unique(v)) == 1)))
m5 <- batz.merge_sitevisit.acoustics(dir.data, dir.save = dir.out, header.combo = "aru.name")  # sites by aru.name only

## ---- TEST 6: project.name from the other tabs of the same file (2026-10-30) -
q <- m$quad; h <- m$habitat
stopifnot("project.name" %in% names(q),                                        # quad now has project.name
          all(q$project.name == h$project.name[match(q$parentglobalid, h$globalid)], na.rm = TRUE))
table(q$project.name, useNA = "ifany")
## quad sheet with no habitat sheet in its file -> "unknown.check" + message
f.ha <- list.files(dir.data, "HabitatAssessment.*xlsx$", full.names = TRUE, recursive = TRUE)[1]
d6 <- file.path(dir.out, "data6"); dir.create(d6, showWarnings = FALSE)
openxlsx::write.xlsx(list(q = readxl::read_excel(f.ha, 2)[1:6, ]), file.path(d6, "quadonly_deployments.xlsx"), overwrite = TRUE)
m6 <- batz.merge_sitevisit.acoustics(d6, dir.save = d6)   # prints: 6 have no associated project name, the value  "unknown.check"
stopifnot(all(m6$quad$project.name == "unknown.check"))

## ---- TEST 7: DBH - extra trunks merged with ";" + .overall --------------------
q <- m$quad
dbh <- grep("dbh", names(q), value = TRUE); print(dbh)
stopifnot(!any(grepl("\\.dbh(\\d+|_other|_average)$", names(q))),              # dbh2 / dbh_other / dbh_average columns removed
          names(q)[match("vegetation.tree_tree1.dbh", names(q)) + 1] == "vegetation.tree_tree1.dbh.overall",
          names(q)[match("vegetation.snag_snag3.dbh", names(q)) + 1] == "vegetation.snag_snag3.dbh.overall")
print(head(q[!is.na(q$vegetation.tree_tree1.dbh), c("vegetation.tree_tree1.dbh", "vegetation.tree_tree1.dbh.overall")], 10))
## the helper on its own: 6, 8, 10 -> sqrt(6^2 + 8^2 + 10^2) = 14.142
x <- data.frame(vegetation.tree_tree1.dbh = c("6", "12", NA), vegetation.tree_tree1.dbh2 = c("8", NA, "5"),
                vegetation.tree_tree1.dbh_other = c("10", NA, NA))
y <- sitevisit.dbh.merge(x); print(y)
stopifnot(identical(y$vegetation.tree_tree1.dbh, c("6;8;10", "12", "5")),
          isTRUE(all.equal(y$vegetation.tree_tree1.dbh.overall, c(14.142, 12, 5))),
          identical(names(y), c("vegetation.tree_tree1.dbh", "vegetation.tree_tree1.dbh.overall")))

## ---- TEST 8: date.deployment filled from date.habitat.assessment -------------
## only when date.deployment is blank and no other record of that project/ARU is earlier
h <- data.frame(project.name = c("P", "P", "P", "Q"), aru.name = c("A1", "A2", "A3", "A1"),
                date.habitat.assessment = as.POSIXct(c("2026-05-10", "2026-05-10", "2026-05-10", "2026-05-12"), tz = "UTC"),
                date.deployment = as.POSIXct(c(NA, NA, "2026-05-11", NA), tz = "UTC"))
sv <- data.frame(project.name = c("P", "P"), aru.name = c("A2", "A1"),
                 date = as.POSIXct(c("2026-05-01", "2026-06-01"), tz = "UTC"))   # A2 has an earlier service visit
r8 <- sitevisit.deploy.date(list(habitat = h, service = sv))                     # NOTE: 2 record(s) ...
print(r8$habitat)
stopifnot(format(r8$habitat$date.deployment[1]) == "2026-05-10",   # P/A1: copied (service visit is later)
          is.na(r8$habitat$date.deployment[2]),                     # P/A2: earlier service record -> not copied
          format(r8$habitat$date.deployment[3]) == "2026-05-11",   # already had a deployment date
          format(r8$habitat$date.deployment[4]) == "2026-05-12")   # Q/A1: its own site -> copied
## on the real exports: habitat records where the two dates are now the same
print(m$habitat[which(format(m$habitat$date.habitat.assessment) == format(m$habitat$date.deployment)),
                c("project.name", "aru.name", "date.habitat.assessment", "date.deployment")])

## ---- TEST 9: habitat $date = earlier of the two dates, as "YYYY-MM-DD" -------
h9 <- data.frame(date.habitat.assessment = as.POSIXct(c("2026-05-10 14:32", "2026-05-12 09:00", NA, NA), tz = "UTC"),
                 date.deployment         = as.POSIXct(c("2026-05-11 08:00", "2026-05-11 17:45", "2026-05-20 10:00", NA), tz = "UTC"),
                 date = c("20260101", "x", NA, "20260505"))
r9 <- sitevisit.habitat.date(h9); print(r9)
stopifnot(identical(r9$date, c("2026-05-10", "2026-05-11", "2026-05-20", NA)))   # earlier date, time removed; old values replaced
## on the real exports
print(head(m$habitat[, c("aru.name", "date.habitat.assessment", "date.deployment", "date")], 10))
stopifnot(all(is.na(m$habitat$date) | grepl("^\\d{4}-\\d{2}-\\d{2}$", m$habitat$date)))

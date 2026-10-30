# =============================================================================
# batz.datawrangler_consolidate.globalID.dev.R  -  dev/test script (2026-10-30, per Josh)
# Keep this file OUTSIDE the package's R/ folder.
#
# Gives every record of a site (project.name + aru.name) the GlobalID of that
# site's earliest record, as $globalID_consolidate.
# Needs: readxl (only when reading .xlsx files).
# =============================================================================

## ---- paths: edit these ------------------------------------------------------
dir.repo <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"
dir.data <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/3 All test data/NABAT Test Data"
dir.out  <- file.path(tempdir(), "consolidate.globalID.test"); dir.create(dir.out, showWarnings = FALSE)

## ---- load the functions from the repo (no install needed) ------------------
for (f in c("batz.util_survey123.reference.R", "batz.util_survey123.aruforms.R",
            "batz.merge_sitevisit.acoustics.R", "batz.datawrangler_consolidate.globalID.R")) {
  source(file.path(dir.repo, "R", f))
}

## ---- TEST 1: small data frame ----------------------------------------------
x <- data.frame(project.name = c("P", "p ", "P", "P", NA),
                aru.name     = c("A", "A", "A", "B", "B"),
                GlobalID     = c("g1", "g2", "g3", "g4", "g5"),
                date         = c("2026-07-10", "07/02/2026", NA, NA, "2026-01-01"))
y <- batz.datawrangler_consolidate.globalID(x)
print(y)
stopifnot(identical(y$globalID_consolidate, c("g2", "g2", "g2", "g4", "g5")),  # earliest = g2; case/space ignored
          names(y)[4] == "globalID_consolidate")                               # placed after GlobalID

## ---- TEST 2: the merged FY26 workbook (habitat + service together) ---------
m <- batz.merge_sitevisit.acoustics(dir.load = dir.data, dir.save = dir.out)   # already adds the column
z <- batz.datawrangler_consolidate.globalID(m$file)                             # same thing, from the file
a <- rbind(data.frame(tab = "habitat", site = paste(z$habitat$project.name, z$habitat$aru.name),
                      date = as.Date(z$habitat$date.habitat.assessment), id = z$habitat$GlobalID,
                      cons = z$habitat$globalID_consolidate),
           data.frame(tab = "service", site = paste(z$service$project.name, z$service$aru.name),
                      date = as.Date(z$service$date), id = z$service$GlobalID,
                      cons = z$service$globalID_consolidate))
a <- a[order(a$site, a$date), ]
stopifnot(all(tapply(seq_len(nrow(a)), a$site, function(i) all(a$cons[i] == a$id[i][1]))))
head(a, 10)
identical(m$service$globalID_consolidate, z$service$globalID_consolidate)

## ---- TEST 3: csv input, other headers ---------------------------------------
write.csv(x, file.path(dir.out, "x.csv"), row.names = FALSE)
batz.datawrangler_consolidate.globalID(file.path(dir.out, "x.csv"), header.combo = "aru.name")

## ---- TEST 4: nothing to consolidate (returns data unchanged with a NOTE) ---
batz.datawrangler_consolidate.globalID(x, header.uniqueid = "nope")

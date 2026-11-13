## batz.datawrangler_format.vetted.dev.R
## Dev/test script for batz.datawrangler_format.vetted() - added 2026-10-21, per Josh.
##
## KEEP THIS FILE OUTSIDE THE PACKAGE'S R/ FOLDER - it has top-level code that
## reads test-data paths and would break devtools::load_all()/document().
##
## Test idea: take the already-cleaned South_North_combined_vetted_1.csv, turn it
## back into a "raw" file (upper-case headers, SITE / MANUAL ID, no $community /
## $project, positive South latitudes, Basecamp ARUs under their field names),
## run the function, and check it gives back the cleaned file.

## ---- CONFIG - point these at your own copies ---------------------------------
dir.pkg   <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz/R"
file.data <- "South_North_combined_vetted_1.csv"
dir.out   <- tempdir()

for (f in c("batz.util_standardize.headers.R", "batz.util_special.characters.R",
            "batz.datawrangler_format.vetted.R")) {
  source(file.path(dir.pkg, f))
}

clean <- read.csv(file.data, check.names = FALSE, stringsAsFactors = FALSE)
pass <- function(name) cat("PASS -", name, "\n")

## ---- build a "raw" copy of each project ---------------------------------------
basecamp <- data.frame(old = c("Basecamp IL 8", "Basecamp IL 10", "Basecamp KA 6", "Basecamp KA 7",
                               "Basecamp WG 7", "Basecamp WG 9", "Basecamp ME 9"),
                       new = c("ILBC_08", "ILBC_10", "KABC_06", "KABC_07",
                               "WGBC_07", "WGBC_09", "MEBC_09"))
make.raw <- function(x) {
  x$aru.name <- ifelse(x$aru.name %in% basecamp$new, basecamp$old[match(x$aru.name, basecamp$new)], x$aru.name)
  x$latitude <- abs(x$latitude)
  x <- x[, setdiff(names(x), c("community", "project"))]
  names(x)[names(x) == "aru.name"] <- "SITE"
  names(x)[names(x) == "species"] <- "MANUAL ID"
  names(x) <- toupper(names(x))
  x
}
south.clean <- clean[clean$project == "South", ]; rownames(south.clean) <- NULL
north.clean <- clean[clean$project == "North", ]; rownames(north.clean) <- NULL
south.raw <- make.raw(south.clean)
north.raw <- make.raw(north.clean)
stopifnot(all(south.raw$LATITUDE >= 0, na.rm = TRUE), "SITE" %in% names(south.raw))

## ---- TEST 1: South - headers, community, project, latitudes -------------------
s <- batz.datawrangler_format.vetted(south.raw, project = "South", latitude.sign = "south",
                                     dir.save = dir.out)
stopifnot(identical(names(s), names(south.clean)))
stopifnot(identical(s$aru.name, south.clean$aru.name), identical(s$species, south.clean$species))
stopifnot(identical(s$community, south.clean$community), all(s$project == "South"))
stopifnot(isTRUE(all.equal(s$latitude, south.clean$latitude)))
stopifnot(length(list.files(dir.out, "^South_vetted\\.format_[0-9]{8}to[0-9]{8}_[0-9]{14}\\.csv$")) >= 1)
pass("South: headers lower-cased and renamed, $community/$project match the hand-cleaned file, latitudes south, CSV saved")

## ---- TEST 2: North - Basecamp recode, BC suffix dropped, latitudes kept -------
n <- batz.datawrangler_format.vetted(north.raw, aru.recode = basecamp, project = "North",
                                     latitude.sign = "north", write.output = FALSE)
stopifnot(identical(n$aru.name, north.clean$aru.name), identical(n$community, north.clean$community))
stopifnot(isTRUE(all.equal(n$latitude, north.clean$latitude)))
stopifnot(all(n$community[grepl("BC_", n$aru.name)] %in% c("IL", "KA", "ME", "WG")))
pass("North: Basecamp ARUs recoded, ILBC/KABC/MEBC/WGBC -> IL/KA/ME/WG, identical to hand-cleaned file")

## ---- TEST 3: combined result equals the cleaned combined file ------------------
comb <- rbind(s, n)
attr(comb, "format.log") <- NULL
stopifnot(isTRUE(all.equal(comb, rbind(south.clean, north.clean), check.attributes = FALSE)))
pass("South + North output equals South_North_combined_vetted_1.csv row for row")

## ---- TEST 4: log, existing $community kept unless overwrite ---------------------
lg <- attr(s, "format.log")
stopifnot(all(c("step", "rows.changed", "note") %in% names(lg)), "community" %in% lg$step)
x <- south.clean; x$community[1:5] <- ""; x$community[6] <- "ZZ"
k <- batz.datawrangler_format.vetted(x, write.output = FALSE)
stopifnot(identical(k$community[1:5], south.clean$community[1:5]), k$community[6] == "ZZ")
k2 <- batz.datawrangler_format.vetted(x, write.output = FALSE, community.overwrite = TRUE)
stopifnot(identical(k2$community, south.clean$community))
pass("format.log attached; blanks filled, other $community values kept unless community.overwrite = TRUE")

## ---- TEST 5: options off / edge cases ------------------------------------------
y <- batz.datawrangler_format.vetted(south.raw, headers.lower = FALSE, headers.rename = NULL,
                                     community.add = FALSE, write.output = FALSE)
stopifnot(identical(names(y), names(south.raw)))
z <- batz.datawrangler_format.vetted(south.raw, headers.rename = c(site = "aru.name", "in file" = "aru.name"),
                                     write.output = FALSE)                    # 2nd rename skipped
stopifnot(sum(names(z) == "aru.name") == 1)
sn <- batz.datawrangler_format.vetted(south.raw, write.output = FALSE, snake_case = TRUE)
stopifnot(all(c("aru_name", "date_12", "in_file") %in% names(sn)))
e <- tryCatch(batz.datawrangler_format.vetted(south.raw, headers.rename = c("aru.name")), error = conditionMessage)
stopifnot(grepl("named vector", e))
d <- batz.datawrangler_format.vetted(south.raw, duplicates.remove = TRUE, write.output = FALSE)
stopifnot(nrow(d) == nrow(unique(south.raw)))
pass("steps can be switched off; clashing rename skipped; snake_case; bad headers.rename stops; duplicates.remove")

cat("\nAll tests passed.\n")

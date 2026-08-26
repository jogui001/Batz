# =============================================================================
# batz.arumeta_generate.eventlog.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.arumeta_generate.eventlog() - tested against real test
# data before being wrapped into the final function
# (batz.arumeta_generate.eventlog.R).
#
# Purpose (per spec): load site-survey forms for ARU deployments from a
# target folder (optional subfolders), classify each file/row as a
# "deployment" or "service" (service/recovery) event, and build one combined
# event log per ARU/site - date of every deployment, service visit, and
# recovery, plus any equipment changes made along the way.
#
# NAMING: kept the family/action name exactly as given
# (arumeta_generate.eventlog fits the "_" separates family from action, "."
# separates words within the action" convention already in use).
#
# =============================================================================
# CORRECTIONS (2026-08-23, per Josh, after reviewing the first version):
# =============================================================================
#  A. CONFIRMED: HabitatAssessments_20m.csv (not quad.csv) is the deployment
#     file - Josh's corrected spec now says so directly ("one set of data
#     with $Date of Deployment as a header, normally found in files called
#     *HabitatAssessments_20m.csv"), matching what point 1 below already
#     concluded from the real headers. No code change needed.
#  B. EVENT-TYPE DERIVATION CHANGED: Josh confirmed status.check/download/
#     aru_swap/mic_swap/recovery are ALL drawn from "Select all actions
#     performed" alone - "recovery" is NOT derived from "Reason for site
#     visit"/"retrieval" as the first version guessed (see old point 8
#     below, kept for context on what changed and why). Removed the
#     "Reason for site visit" check entirely; every service row's event(s)
#     now come purely from splitting "Select all actions performed" on
#     commas, with a "service" fallback when that field is blank. NOTE: in
#     the current 15-row real test sample, "Select all actions performed"
#     only ever contains "aru_swap"/"mic_swap" tokens - no row's action list
#     contains anything resembling "recovery"/"status_check"/"download", so
#     this test run now shows ZERO recovery events (down from 6 under the
#     old "Reason for site visit" logic) - that's expected given this
#     correction, not a bug; those three event types just aren't exercised
#     by this particular sample.
#  C. GROUPING KEY CHANGED FROM ARU SERIAL TO SITE: Josh confirmed "Site and
#     ARU are the same thing... for now use the site" (headers aren't
#     standardized across forms yet, so ARU serial numbers can't be trusted
#     to line up cleanly). Same-timestamp event ordering now sorts by
#     Site.unified instead of ARU.serial (both columns are still kept in the
#     output - ARU.serial just isn't the grouping/ordering key anymore).
# =============================================================================
# FLAGGED SPEC ISSUES (original list; item 1 confirmed by correction A above,
# item 8 superseded by correction B above - kept for context):
# =============================================================================
#
#  1. WHICH FILE IS "deployment"? Josh's "Required Inputs" section literally
#     names "*HabitatAssessments_quad.csv" as the deployment file. But the
#     "deployment" header list given a few lines later ($Client, $Project,
#     $Project Code, $Date of Deployment, $Detector Model, $Detector Make,
#     $Microphone Model, $Microphone Make, $Site, $Survey Type, $X, $Y,
#     $Serial Number of Detector, $Serial Number of Microphone, $Personnel,
#     $Date of Habitat Assessment) has ZERO overlap with the real
#     HabitatAssessments_quad.csv headers (quad.csv is a tree/canopy/DBH
#     survey form - no Client/Project/Detector/Serial-Number columns at all).
#     That same header list instead matches HabitatAssessments_20m.csv almost
#     exactly (14 of 16 headers present verbatim, case-sensitive; only
#     "Microphone Model"/"Microphone Make" are absent from the real file) -
#     and this also matches the separately-given "Test data pattern" line
#     (c("*HabitatAssessments_20m.csv", "*Acoustic_SiteVisitARU.csv")), which
#     already said 20m, not quad. Concluded "Required Inputs" has a
#     copy/paste or typo error naming the wrong file, and used
#     HabitatAssessments_20m.csv as the deployment source instead of
#     HabitatAssessments_quad.csv (quad.csv is NOT read by this function at
#     all). PLEASE CONFIRM - if quad.csv really was intended, the
#     "deployment" header list would need to be rewritten to match its real
#     columns (quadrant/DBH/canopy fields), since none of the given headers
#     exist there.
#  2. `load.pattern` default given in "Optional inputs" was
#     c("*Acoustic_SiteVisitARU.csv, "*Acoustic_SiteVisitARU.csv") - the same
#     file listed twice, with a stray/malformed quote too - obviously a
#     copy-paste artifact. Used c("*HabitatAssessments_20m.csv",
#     "*Acoustic_SiteVisitARU.csv") instead, matching the "Test data pattern"
#     line and point 1 above.
#  3. NAMING LEAKAGE FROM A DIFFERENT FUNCTION FAMILY: the Steps section
#     names its working/output objects "sm4eventlog", "log.file1"/
#     "log.file2", and the final "sm4merge.logfile"/"sm4merge.logfile1" -
#     "sm4" is the SM4-datalogger family prefix used elsewhere in the batz
#     package (e.g. batz.sm4logfile_merge&format), not this ARU/arumeta
#     family - looks like spec-template leftover, the same "leftover
#     boilerplate" issue flagged repeatedly for batz.arumeta_merge.format
#     (see preferences.md). Renamed everything to the arumeta family instead:
#       sm4eventlog            -> aru.eventlog          (main returned table)
#       log.file2 (per-file)   -> aru.eventlog.filelog  (per-file diagnostic)
#       log.file1 (per-site)   -> aru.eventlog.sitelog  (per-site/ARU summary)
#     ("log.file1"/"log.file2" in the Steps section are just scratch names
#     for the two log tables being built - log.file1 is explicitly described
#     later as the per-site/ARU summary with $Client/$Project/.../$deployment/
#     $service/$recovery/$duplicates.removed columns, so by elimination
#     log.file2 must be the per-file $filename/$file.type/$status/$notes
#     table described in the "log.file" headers section. Please confirm this
#     mapping is right.)
#  4. `log.file` COLUMN LIST CONTRADICTS ITSELF: the Optional-inputs bullet
#     says log.file "records $filename, $status, $action, $reason", but the
#     later, more detailed ""log.file" has the following headers" section
#     gives a different set: $filename, $file.type, $status, $notes (no
#     $action, no $reason). Went with the more detailed/explicit later
#     definition (same resolution approach used previously in this project
#     when two parts of a spec disagreed) - aru.eventlog.filelog has columns
#     $filename, $file.type, $status, $notes.
#  5. DUPLICATE-HEADER-WITHIN-A-FILE HANDLING: spec says "if there are
#     duplicated headers merge them into a single header with the same name
#     by deleting any duplicated data (keep one duplication remove the
#     others)" - read literally, that's "first copy wins, discard the rest,
#     no comparison". But we already know from batz.arumeta_merge.format
#     that this EXACT real data has duplicate "Personnel" columns that
#     sometimes genuinely DISAGREE row-by-row (one real row: "BRF" vs "ECG").
#     Blindly keeping "the first" would silently discard a real, different
#     value. Reused the same conflict-aware merge already tested in
#     batz.arumeta_merge.format instead: identical values (treating NA/""
#     as equivalent blanks) -> keep one; a blank paired with a non-blank ->
#     keep the non-blank; two different non-blank values -> concatenate
#     "value1; value2". Flagging in case literal first-wins-discard was
#     actually intended.
#  6. STATUS PRIORITY when a file has BOTH duplicate headers AND missing
#     headers (not addressed explicitly - the spec describes the four
#     statuses as if mutually exclusive): used priority failed (missing >
#     max.missing) > duplicate (any duplicate columns resolved) > missing
#     (0 < missing <= max.missing) > success, with $notes listing both
#     duplicate-column names and missing-column names when both apply.
#  7. HEADER MATCHING is exact-string, case-insensitive (not fuzzy/typo-
#     tolerant). This matters for real HabitatAssessments_20m.csv, whose
#     header row has genuine TYPOS ("Clien", "Prject Code" for what the
#     deployment master list calls "Client"/"Project Code") - those don't
#     match even case-insensitively and count as "missing" (padded NA), same
#     as the two truly-absent Microphone Model/Microphone Make columns. That
#     puts the 20m file at 4 missing headers (Client, Project Code,
#     Microphone Model, Microphone Make) - under max.missing = 5, so it
#     merges successfully with status "missing". Not auto-correcting typos -
#     flagging instead, since guessing at typo-fixes felt riskier than just
#     reporting the gap via $notes.
#  8. EVENT-TYPE DERIVATION (the most open-ended part of the spec): the
#     "Steps" section says "For each ARU, if more than one action occurred
#     at the same date/time stamp then record them in the following order:
#     deployment, status.check, download, aru_swap, mic_swap" - implying
#     each event needs a categorical type, but never spells out how to
#     derive status.check/download/aru_swap/mic_swap from the service file's
#     actual columns. Inspected the real Acoustic_SiteVisitARU.csv:
#       - "Select all actions performed" holds a comma-separated list of
#         literal tokens - real values seen: "", "aru_swap", "mic_swap ",
#         "aru_swap,  mic_swap " - i.e. "aru_swap"/"mic_swap" are literal
#         option values in the real form, confirming those two event types
#         map directly to that column (split on comma, trim whitespace).
#         "status_check"/"download" presumably exist as further option
#         values in the full form that this 15-row sample just didn't
#         happen to exercise - used the same literal-token convention for
#         them (nothing else in the real data suggested a better mapping).
#       - "Reason for site visit" holds "retrieval" or
#         "sitevisit_maintenance" in this sample. Read "retrieval" as the
#         event type "recovery" (matches the function's own stated purpose
#         - "date of each ARU deployment, service and recovery" - and the
#         $recovery column expected in the site summary log) -
#         "sitevisit_maintenance" doesn't map to any of the five listed
#         event types on its own.
#       - A visit row can generate MORE THAN ONE event (recovery AND an
#         action token, if both apply) - a plain visit with reason =
#         "sitevisit_maintenance" and no actions performed gets a fallback
#         event type of "service" (not in the given priority list) so it
#         isn't silently dropped from the log.
#     "recovery" and "service" aren't in the given 5-item ordering list -
#     sorted them after the five named types (in that order) at same-
#     timestamp ties, since the spec is silent on where they'd fall.
#     PLEASE REVIEW THIS SECTION ESPECIALLY CLOSELY.
#  9. UNIFIED ARU/SITE/DATE-TIME COLUMNS: deployment and service rows don't
#     share column names for the same real-world concept (deployment has
#     "Serial Number of Detector"/"Site"/"Date of Deployment"; service has
#     "ARU Serial Number"/"Site Name"/"Date"+"Time"). Added derived columns
#     $ARU.serial, $Site.unified, $date.time (dot-style per Josh's own
#     stated column-naming shorthand) so the two sources can be combined,
#     grouped, and ordered together - the original source columns are kept
#     as-is alongside these derived ones (nothing is dropped).
# 10. SITE SUMMARY ($Client/$Project/$Project Code per site): only
#     deployment rows carry real Client/Project Code values in this data
#     (service rows have neither column at all, i.e. both counted as
#     "missing" per point 7's cousin above for the service master list too -
#     service is missing Client + Project Code, 2 of 15 headers, well under
#     max.missing). The per-site summary takes the first non-blank
#     Client/Project/Project Code found across ALL of a site's rows
#     (deployment or service), so a site with a matching deployment record
#     still gets Client/Project Code filled in on its summary row even
#     though the service-derived rows themselves don't carry it directly.
# 11. Output column names Josh wrote for the two log tables use spaces/
#     capitals ($Client, $Project Code, $Reason for site visit, etc.) which
#     conflicts with the dot-separated "$collum.name" shorthand convention
#     Josh himself specified at the top of this spec message. Applied that
#     convention to the log/summary tables' OWN columns (aru.eventlog.
#     filelog: filename/file.type/status/notes; aru.eventlog.sitelog:
#     client/project/project.code/site/deployment/service/recovery/
#     duplicates.removed - all lower, dot-separated) - the raw survey-form
#     headers pulled INTO aru.eventlog itself are left exactly as they are
#     in the source files (same precedent as batz.arumeta_merge.format,
#     which keeps real headers like "Site Name" untouched but uses its own
#     dot-style names for $inputfile/$event/$action/$count).
# 12. No `write.output`/`dir.save` option was given for this function
#     (unlike batz.templogger_merge.format/batz.suntimes_generate) - so
#     nothing is written to disk; the function only returns/auto-assigns
#     data frames. The "Final output: dataframe = "SM4eventlog.csv" ..."
#     line reads like it's just naming the returned object (renamed per
#     point 3 above), not asking for an actual CSV write step - flagging in
#     case a real write.output/dir.save pair should be added to match the
#     other two functions that write files.
#
# Test data: real files copied into this sandbox (verified against Josh's
# "4 Current  test data" folder as of 2026-08-19, per preferences.md):
#   /home/claude/arumeta_work2/HabitatAssessments_20m.csv   (deployment)
#   /home/claude/arumeta_work2/Acoustic_SiteVisitARU.csv    (service)
#   (HabitatAssessments_quad.csv also present in that folder but NOT read by
#   this function at all - see point 1 above.)
# =============================================================================

pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

# -----------------------------------------------------------------------------
# master header lists (canonical column order + names for each event source)
# -----------------------------------------------------------------------------
deployment.headers <- c("Client", "Project", "Project Code", "Date of Deployment",
                         "Detector Model", "Detector Make", "Microphone Model", "Microphone Make",
                         "Site", "Survey Type", "X", "Y", "Serial Number of Detector",
                         "Serial Number of Microphone", "Personnel", "Date of Habitat Assessment")

service.headers <- c("Client", "Project", "Project Code", "Date", "Site Name",
                      "Reason for site visit", "Personnel", "Notes", "ARU Serial Number",
                      "Mic Serial Number", "Power Kit/Solar Serial Number", "HOBO Sensor Serial Number",
                      "Select all actions performed", "New mic serial number", "New ARU serial number")

# canonical same-timestamp ordering (per spec); anything else sorts after these
event.priority <- c(deployment = 1, status_check = 2, download = 3, aru_swap = 4, mic_swap = 5,
                     recovery = 6, service = 7)

# -----------------------------------------------------------------------------
# helper: within-file duplicate-header merge (conflict-aware - see point 5)
# -----------------------------------------------------------------------------
resolve.duplicate.columns <- function(df) {
  nm <- names(df)
  dup.names <- unique(nm[duplicated(nm)])
  if (length(dup.names) == 0) return(list(df = df, dup.cols = character(0)))

  # BUGFIX (2026-08-23): build one logical `keep` mask over ALL columns, using
  # the ORIGINAL, frozen `nm` for every idx lookup, and drop extra columns in
  # a SINGLE `df[keep]` at the end. Two R gotchas otherwise bite when a file
  # has more than one duplicate-name group (real HabitatAssessments_20m.csv
  # has four: "Type of Sampling", "Specify Other Sampling Type", "Detector
  # Model", "Specify Other Detector Model"): (1) `df[-idx[-1]]` run once per
  # group, before all groups are resolved, silently renames a STILL-
  # duplicated column elsewhere via make.unique (e.g. to "...Type.1"), so the
  # next group's `which(names(df) == dn)` finds only 1 match instead of 2;
  # (2) once a group only has a single occurrence left to drop, `idx[-1]` is
  # `integer(0)`, and `df[-integer(0)]` does NOT mean "drop nothing" for a
  # data.frame - it drops EVERY column (same reason `x[integer(0)]` selects
  # nothing - R can't distinguish a negated empty index from a positive
  # empty one). Reused the exact `keep`-mask pattern already tested in
  # batz.arumeta_merge.format's resolve.dup.columns(), which sidesteps both.
  keep <- rep(TRUE, ncol(df))
  for (dn in dup.names) {
    idx <- which(nm == dn)
    cols <- lapply(idx, function(i) {
      v <- as.character(df[[i]])
      ifelse(is.na(v) | v == "", NA_character_, v)
    })
    merged <- cols[[1]]
    for (i in 2:length(cols)) {
      this.col <- cols[[i]]
      conflict <- !is.na(merged) & !is.na(this.col) & merged != this.col
      fill     <- is.na(merged) & !is.na(this.col)
      merged[fill]     <- this.col[fill]
      merged[conflict] <- paste(merged[conflict], this.col[conflict], sep = "; ")
    }
    df[[idx[1]]] <- merged
    keep[idx[-1]] <- FALSE
  }
  list(df = df[keep], dup.cols = dup.names)
}

# -----------------------------------------------------------------------------
# helper: exact, case-insensitive match of tmp's column names against a
# master header list. Returns a named character vector: master name ->
# matching tmp column name (or NA if not found).
# -----------------------------------------------------------------------------
match.headers <- function(tmp.names, master) {
  vapply(master, function(m) {
    idx <- which(tolower(trimws(tmp.names)) == tolower(trimws(m)))
    if (length(idx) >= 1) tmp.names[idx[1]] else NA_character_
  }, character(1))
}

# -----------------------------------------------------------------------------
# helper: full outer rbind - union of columns, NA-padded on whichever side
# lacks a given column (used for the final deployment+service combine).
# -----------------------------------------------------------------------------
full.outer.rbind <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  all.cols <- union(names(a), names(b))
  for (cn in setdiff(all.cols, names(a))) a[[cn]] <- NA
  for (cn in setdiff(all.cols, names(b))) b[[cn]] <- NA
  rbind(a[all.cols], b[all.cols])
}

# -----------------------------------------------------------------------------
# helper: process a single file -> classify, trim/pad against its master
# header list, compute file-level status + notes.
# -----------------------------------------------------------------------------
process.one.file <- function(f, max.missing) {
  raw <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  names(raw) <- sub("^﻿", "", names(raw))  # strip UTF-8 BOM (seen on real ObjectID header)

  dc <- resolve.duplicate.columns(raw)
  tmp <- dc$df
  dup.cols <- dc$dup.cols

  is.deployment <- any(tolower(trimws(names(tmp))) == "date of deployment")
  master <- if (is.deployment) deployment.headers else service.headers
  file.type <- if (is.deployment) "deployment" else "service"

  matched <- match.headers(names(tmp), master)
  present <- !is.na(matched)
  missing.headers <- master[!present]
  n.missing <- length(missing.headers)

  if (n.missing > max.missing) {
    return(list(status = "failed", file.type = file.type,
                notes = paste0("missing headers (", n.missing, " > max.missing): ",
                                paste(missing.headers, collapse = ", ")),
                data = NULL))
  }

  out <- as.data.frame(tmp[matched[present]], stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- master[present]
  for (mh in missing.headers) out[[mh]] <- NA
  out <- out[master]

  status <- if (length(dup.cols) > 0) "duplicate" else if (n.missing > 0) "missing" else "success"
  notes.parts <- character(0)
  if (length(dup.cols) > 0) {
    notes.parts <- c(notes.parts, paste0("duplicate columns merged: ", paste(dup.cols, collapse = ", ")))
  }
  if (n.missing > 0) {
    notes.parts <- c(notes.parts, paste0("missing columns (padded NA): ", paste(missing.headers, collapse = ", ")))
  }
  notes <- if (length(notes.parts) > 0) paste(notes.parts, collapse = " | ") else NA_character_

  list(status = status, file.type = file.type, notes = notes, data = out)
}

# -----------------------------------------------------------------------------
# helper: explode each service row into one row per derived event.type
# (see point 8) - a row can produce >1 event row (e.g. recovery + aru_swap).
# -----------------------------------------------------------------------------
explode.service.events <- function(df) {
  # CORRECTION (2026-08-23, per Josh): all five event types - including
  # recovery - are drawn from "Select all actions performed" alone, not from
  # "Reason for site visit". Split that column on commas; each token becomes
  # its own event row (a visit can produce more than one). A blank/no-action
  # visit still gets a fallback "service" event so it isn't dropped.
  if (is.null(df) || nrow(df) == 0) return(NULL)
  out.rows <- vector("list", 0)
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    actions <- trimws(unlist(strsplit(as.character(row[["Select all actions performed"]]), ",")))
    actions <- actions[!is.na(actions) & actions != ""]

    events <- if (length(actions) > 0) actions else "service"

    for (ev in events) {
      r2 <- row
      r2$event.type <- ev
      out.rows[[length(out.rows) + 1]] <- r2
    }
  }
  do.call(rbind, out.rows)
}

# -----------------------------------------------------------------------------
# batz.arumeta_generate.eventlog(dir.load, dir.sub, load.pattern,
#                                 duplicates.remove, log.file, max.missing)
# -----------------------------------------------------------------------------
batz.arumeta_generate.eventlog <- function(dir.load = getwd(),
                                            dir.sub           = FALSE,
                                            load.pattern      = c("*HabitatAssessments_20m.csv", "*Acoustic_SiteVisitARU.csv"),
                                            duplicates.remove = TRUE,
                                            log.file          = FALSE,
                                            max.missing       = 5) {

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  deployment <- NULL
  service    <- NULL
  file.log.rows <- list()

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f, max.missing = max.missing)
      file.log.rows[[length(file.log.rows) + 1]] <- data.frame(
        filename = basename(f), file.type = r$file.type, status = r$status,
        notes = r$notes, stringsAsFactors = FALSE)

      cat("  ", basename(f), " -> ", r$file.type, " (", r$status, ")\n", sep = "")

      if (!is.null(r$data)) {
        if (r$file.type == "deployment") {
          deployment <- if (is.null(deployment)) r$data else rbind(deployment, r$data)
        } else {
          service <- if (is.null(service)) r$data else rbind(service, r$data)
        }
      }
    }
  }

  file.log <- if (length(file.log.rows) > 0) {
    do.call(rbind, file.log.rows)
  } else {
    data.frame(filename = character(0), file.type = character(0), status = character(0),
               notes = character(0), stringsAsFactors = FALSE)
  }

  # ---- derive unified ARU / site / date-time columns + event.type ----
  if (!is.null(deployment) && nrow(deployment) > 0) {
    deployment$event.type   <- "deployment"
    deployment$ARU.serial   <- deployment[["Serial Number of Detector"]]
    deployment$Site.unified <- deployment[["Site"]]
    deployment$date.time    <- as.character(deployment[["Date of Deployment"]])
  }

  service.events <- explode.service.events(service)
  if (!is.null(service.events) && nrow(service.events) > 0) {
    service.events$ARU.serial   <- service.events[["ARU Serial Number"]]
    service.events$Site.unified <- service.events[["Site Name"]]
    service.events$date.time    <- paste(service.events[["Date"]], service.events[["Time"]])
  }

  aru.eventlog <- full.outer.rbind(deployment, service.events)

  if (is.null(aru.eventlog)) {
    aru.eventlog <- data.frame()
    cat("\nNo deployment or service records were loaded - aru.eventlog is empty.\n")
  } else {
    # order by ARU, timestamp, then the canonical same-timestamp priority
    rank <- ifelse(aru.eventlog$event.type %in% names(event.priority),
                    event.priority[aru.eventlog$event.type], 99)
    # CORRECTION (2026-08-23, per Josh): "Site and ARU are the same thing...
    # for now use the site" - group/order by Site.unified, not ARU.serial
    # (headers aren't standardized across forms yet, so ARU serial numbers
    # can't be trusted to line up 1:1 with a site the way Site names can).
    aru.eventlog <- aru.eventlog[order(aru.eventlog$Site.unified, aru.eventlog$date.time, rank), ]

    # duplicates.remove - drop exact duplicate rows in the final combined table
    dup.mask <- duplicated(aru.eventlog)
    n.dup <- sum(dup.mask)
    dup.sites <- if (n.dup > 0) aru.eventlog$Site.unified[dup.mask] else character(0)
    if (duplicates.remove && n.dup > 0) {
      cat("\n", n.dup, " exact duplicate row(s) removed from aru.eventlog.\n", sep = "")
      aru.eventlog <- aru.eventlog[!dup.mask, ]
    }
    rownames(aru.eventlog) <- NULL
  }

  # ---- required status messages ----
  dep.log <- file.log[file.log$file.type == "deployment", ]
  svc.log <- file.log[file.log$file.type == "service", ]

  report.category <- function(cat.log, label) {
    failed  <- cat.log$filename[cat.log$status == "failed"]
    missing <- cat.log$filename[cat.log$status == "missing"]
    if (length(failed) > 0) {
      cat("These ", label, " files failed to merge as they lacked enough matching headers: ",
          paste(failed, collapse = ", "), "\n", sep = "")
    } else {
      cat("all ", label, " files successfully merged\n", sep = "")
    }
    if (length(missing) > 0) {
      cat("these ", label, " files were missing headers please check outputs: ",
          paste(missing, collapse = ", "), "\n", sep = "")
    }
  }
  report.category(dep.log, "deployment")
  report.category(svc.log, "service")

  if (nrow(aru.eventlog) > 0) {
    all.sites <- unique(aru.eventlog$Site.unified[!is.na(aru.eventlog$Site.unified)])
    sites.with.deployment <- unique(aru.eventlog$Site.unified[aru.eventlog$event.type == "deployment"])
    sites.with.recovery   <- unique(aru.eventlog$Site.unified[aru.eventlog$event.type == "recovery"])

    missing.dep.sites <- setdiff(all.sites, sites.with.deployment)
    missing.rec.sites <- setdiff(all.sites, sites.with.recovery)

    if (length(missing.dep.sites) > 0) {
      cat("these sites are missing deployment entries: ", paste(missing.dep.sites, collapse = ", "), "\n", sep = "")
    }
    if (length(missing.rec.sites) > 0) {
      cat("these sites are missing recovery entries: ", paste(missing.rec.sites, collapse = ", "), "\n", sep = "")
    }
  }

  # ---- per-site/ARU summary log (aru.eventlog.sitelog) ----
  first.nonblank <- function(x) {
    v <- x[!is.na(x) & x != ""]
    if (length(v) > 0) v[1] else NA
  }

  if (nrow(aru.eventlog) > 0) {
    sites <- unique(aru.eventlog$Site.unified[!is.na(aru.eventlog$Site.unified)])
    site.rows <- lapply(sites, function(s) {
      sub <- aru.eventlog[aru.eventlog$Site.unified == s, ]
      data.frame(
        client       = first.nonblank(sub$Client),
        project      = first.nonblank(sub$Project),
        project.code = first.nonblank(sub[["Project Code"]]),
        site         = s,
        deployment   = sum(sub$event.type == "deployment"),
        service      = sum(sub$event.type %in% c("status_check", "download", "aru_swap", "mic_swap", "service")),
        recovery     = sum(sub$event.type == "recovery"),
        duplicates.removed = sum(dup.sites == s),
        stringsAsFactors = FALSE
      )
    })
    aru.eventlog.sitelog <- do.call(rbind, site.rows)
  } else {
    aru.eventlog.sitelog <- data.frame(client = character(0), project = character(0),
                                        project.code = character(0), site = character(0),
                                        deployment = integer(0), service = integer(0),
                                        recovery = integer(0), duplicates.removed = integer(0),
                                        stringsAsFactors = FALSE)
  }

  result <- list(aru.eventlog = aru.eventlog)
  if (log.file) {
    result$aru.eventlog.filelog <- file.log
    result$aru.eventlog.sitelog <- aru.eventlog.sitelog
  }

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("=== default call against real test data (log.file = TRUE) ===\n")
res <- batz.arumeta_generate.eventlog("/home/claude/arumeta_work2", log.file = TRUE)

cat("\n=== aru.eventlog dim ===\n"); print(dim(aru.eventlog))
cat("\n=== aru.eventlog (key columns) ===\n")
print(aru.eventlog[, c("ARU.serial", "Site.unified", "date.time", "event.type")])

cat("\n=== aru.eventlog.filelog ===\n"); print(aru.eventlog.filelog)
cat("\n=== aru.eventlog.sitelog ===\n"); print(aru.eventlog.sitelog)

cat("\n\n=== max.missing = 1 (should fail the 20m/deployment file - 4 missing > 1) ===\n")
res2 <- batz.arumeta_generate.eventlog("/home/claude/arumeta_work2", log.file = TRUE, max.missing = 1)
cat("\naru.eventlog2 dim:", paste(dim(aru.eventlog), collapse = " x "), "\n")
print(aru.eventlog.filelog)

cat("\n\n=== duplicates.remove = FALSE (row count should not shrink) ===\n")
res3 <- batz.arumeta_generate.eventlog("/home/claude/arumeta_work2", duplicates.remove = FALSE)
cat("nrow with duplicates.remove = FALSE:", nrow(aru.eventlog), "\n")

cat("\n\n=== empty directory (no matching files) ===\n")
empty.dir <- tempfile(); dir.create(empty.dir)
res4 <- batz.arumeta_generate.eventlog(empty.dir, log.file = TRUE)
cat("aru.eventlog rows:", nrow(aru.eventlog), " filelog rows:", nrow(aru.eventlog.filelog), "\n")

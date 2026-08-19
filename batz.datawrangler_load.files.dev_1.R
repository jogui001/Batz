# =============================================================================
# batz.datawrangler_load.files.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.datawrangler_load.files() - tested against real test
# data before being wrapped into the final function
# (batz.datawrangler_load.files.R).
#
# Purpose (per spec): load every .csv and .xlsx file in a directory (and its
# subdirectories, if dir.sub = TRUE) into R. A .csv, or an .xlsx with only one
# sheet, becomes a data frame. An .xlsx with multiple sheets becomes a list
# (one data frame per sheet). Each resulting object is named the same as its
# input file (extension stripped).
#
# NAME: renamed from Josh's "batz.datawrangler_loadfiles" to
# batz.datawrangler_load.files() to match the project's established
# convention - "_" only separates the family ("datawrangler") from the
# action, and "." separates multi-word pieces within the action itself
# (same reasoning as batz.arumeta_merge.format's "merge.format").
#
# Test data:
#   Real files (staged from the "hildas" device folder, "1Raw Testdata"),
#   copied into a local sandbox test tree (NOT modifying Josh's real files):
#     /home/claude/loadfiles_work/top/
#       Acoustic_SiteVisitARU.csv
#       HabitatAssessments_quad.csv
#       HabitatAssessments_20m.csv
#       2HabitatAssessments_quad.csv   (different base name - no conflict)
#       test_07072026_07082026_ARUdeployments.xlsx  (REAL 3-sheet workbook -
#         exercises the "xlsx with multiple sheets -> list" branch)
#     /home/claude/loadfiles_work/top/subdir/
#       HabitatAssessments_quad.csv    (byte-identical duplicate of the
#         top-level file of the same name - exercises "dir.sub=TRUE finds an
#         identical file name -> merge, same columns" branch)
#       Acoustic_SiteVisitARU.csv      (SYNTHETIC - a copy of the real file
#         with its last column deliberately dropped, built locally in this
#         sandbox only, NOT one of Josh's real files - exercises "identical
#         file name but columns DON'T match -> do not merge" branch)
#       recode.csv                     (only exists in the subdirectory - no
#         top-level counterpart - exercises plain recursive load with no
#         duplicate-name logic involved at all)
#   HabitatAssessments_20m.csv also had one row duplicated within itself
#   (SYNTHETIC edit, sandbox-only) to exercise remove.duplicates on a single,
#   non-merged file.
#
#   Three additional small SYNTHETIC trees (sandbox-only, not from Josh's
#   real data - built purely to isolate the newest features from each other
#   and from the merge/dedup logic already exercised above):
#     /home/claude/loadfiles_work/v2a/  (fileA.csv top-level + fileA.csv in
#       subdir/, same columns/different rows) - isolates skip.duplicates.
#     /home/claude/loadfiles_work/v2b/  (fileA.csv + fileB_altname.csv, both
#       with column set x,y but DIFFERENT names) - isolates header.match.
#     /home/claude/loadfiles_work/v2c/  (fileA.csv + fileB.csv top-level,
#       subdir/fileA.csv [same name/cols as top fileA] + subdir/fileC.csv
#       [brand new name/cols]) - isolates max.objects, specifically that a
#       merge (subdir/fileA.csv into fileA) is never blocked by the cap even
#       after it's reached, while a file that would need a brand-new object
#       (subdir/fileC.csv) IS blocked once the cap is hit.
#
# ASSUMPTIONS / OPEN QUESTIONS (flagging per project convention):
#   1. Object names are taken literally from the input file name with its
#      extension stripped (tools::file_path_sans_ext(basename(f))) - exactly
#      as the spec says ("name will be the same as the name of the input
#      file"). This is NOT run through the project's usual "." naming
#      convention - if a file is named with underscores, spaces, or leading
#      digits (e.g. "2HabitatAssessments_quad"), the created object keeps
#      that exact name. A name like that isn't a syntactically "plain" R
#      name, so accessing it back out requires backticks or get(), e.g.
#      `` `2HabitatAssessments_quad` `` or get("2HabitatAssessments_quad") -
#      assign() itself doesn't care, it accepts any string as a name.
#   2. Following the auto-assign convention already established in
#      batz.arumeta_merge.format() (per Josh: "can the function be structured
#      to auto generate separate data frames?"), this function builds each
#      loaded file into its own standalone object directly in the CALLING
#      environment via assign(name, value, envir = parent.frame()) - not
#      nested inside a list you have to pull apart. It also returns
#      (invisibly) a named list of everything it loaded, for anyone who'd
#      rather do `result <- batz.datawrangler_load.files(...)` and work with
#      `result$my.file`-style access instead.
#   3. dir.sub isn't in this round's "Optional inputs" list, but the Steps
#      section still explicitly says "for the current working directory (and
#      all sub directors if dir.sub = TRUE)" - so it's clearly still needed
#      for the function to work at all. Kept it (default FALSE, as before)
#      rather than treating its absence from the list as a request to remove
#      it - flagging that I noticed the list doesn't mention it.
#   4. "Same number of columns with the same names" (and, likewise,
#      header.match's "matches the header set... 100%") is checked as an
#      unordered SET match (same column names present, regardless of order),
#      not that columns have to be in the same left-to-right order. Columns
#      are then reordered to line up by name before merging, per "when
#      merging any two files, reorder the new file columns to match the
#      original dataframe."
#   5. When two files ARE merged (by name OR by header.match), all columns
#      are coerced to character first (avoids rbind errors when the same
#      column comes back as, say, POSIXct from one file and character from
#      another - same reasoning as batz.arumeta_merge.format), and
#      check.names = FALSE is used throughout so real column names with
#      spaces/punctuation are never silently mangled (see the CRITICAL
#      check.names note in the project's preferences.md).
#   6. Only a file whose name ends in "ARUdeployments.xlsx" was treated
#      specially (opened sheet-by-sheet) in batz.arumeta_merge.format; here,
#      EVERY .xlsx file is opened and checked for its sheet count - a
#      single-sheet workbook becomes a data frame, a multi-sheet workbook
#      becomes a list of one data frame per sheet (named by sheet name).
#   7. If dir.load contains zero matching .csv/.xlsx files, the function
#      prints a message saying so, creates nothing, and returns an empty
#      (invisible) list - not an error.
#   8. A file that fails to read (e.g. corrupt/unreadable) is skipped with a
#      console warning rather than stopping the whole function.
#   9. remove.duplicates = TRUE removes exact duplicate rows (base
#      duplicated()) from WITHIN each individual data frame - including
#      separately within each sheet of a multi-sheet xlsx list - never across
#      different top-level objects. Runs as a final pass AFTER all files are
#      loaded/merged, so it also catches duplicates that only appear once two
#      files have been merged together.
#
# NEW/CHANGED THIS ROUND (2026-08-19, later same day, per Josh):
#  - max.files renamed to max.objects (Josh's own wording change).
#  - Added header.match = FALSE (default) and skip.duplicates = FALSE
#    (default).
#  - log.file gained two new columns, $reason and $objectname.
#  - max.objects now explicitly does NOT count merges - only files that
#    create a brand-new top-level object count toward the cap, and merging
#    keeps working even after the cap is reached (this was specifically
#    requested: "do not count merged files towards the max.object limit...
#    once the limit is reached keep loading and merging files").
#
#  10. SPEC CONTRADICTION - flagging rather than guessing: the "Optional
#      inputs" list says `skip.duplicates = FALSE (default)`, but the Steps
#      section separately labels BOTH branches "(default)" - "if
#      skip.duplicates = TRUE (default) then do not load that file in" AND
#      "if skip.duplicates = FALSE (default) then merge...". Only one can
#      actually be the default. Went with the explicit parameter-list
#      statement (FALSE is the default - matching the existing merge
#      behavior nothing changes unless you turn skip.duplicates ON) - please
#      confirm this is what you meant.
#  11. Full decision order implemented for a file whose base name ALREADY
#      matches something already loaded (only possible when dir.sub = TRUE
#      and the duplicate lives in a different folder):
#        - skip.duplicates = TRUE: skip it outright, no column check at all.
#        - skip.duplicates = FALSE: merge if columns match (by name); if they
#          DON'T match, it falls through to the SAME header.match check
#          described in 12 below before finally becoming its own object
#          (under a folder-suffixed alt name, same as before).
#  12. header.match: read literally as "each time a file is loaded" - so it's
#      checked for ANY file that isn't already resolved by the name-based
#      logic above (i.e., a brand-new file name, OR a same-name-but-
#      different-columns file that fell through step 11). It's NOT checked
#      when a file's name already matches an existing object AND their
#      columns already match (that's just resolved as a normal same-name
#      merge, no need to also hunt for a header match elsewhere) or when
#      skip.duplicates already skipped it. When header.match = TRUE and it's
#      time to check: every ALREADY-LOADED plain data-frame object (list-type
#      multi-sheet-xlsx entries are never compared - a sheet-list has no
#      single column set) is scanned in the order it was created, and the
#      FIRST one whose column-name set matches the new file's 100% becomes
#      the merge target - the file is merged into THAT object (keeping that
#      object's existing name), not given a new name of its own. A
#      multi-sheet xlsx is never itself checked against header.match either
#      way (as source or as target) - it can still be skipped/merged-by-NAME
#      via skip.duplicates logic, just not via header.match.
#  13. max.objects: capping ONLY applies at the exact moment a file is about
#      to become a brand-new standalone object (i.e., neither a name-based
#      nor a header.match merge applied). A merge NEVER counts against the
#      cap and is NEVER blocked by it, even after the cap has already been
#      reached - per Josh's explicit instruction. Once the cap is hit, any
#      file that would have needed a new object is skipped instead
#      (reason = "object limit met"), but files that can still merge into
#      something already loaded keep working normally for the rest of the
#      scan.
#  14. log.file's $reason column uses Josh's exact given strings wherever
#      they map cleanly:
#        loaded -> "file was first of its kind" (brand-new name, no
#                  header.match hit) | "file with same name already exists
#                  with different headers" (same-name dup, columns differ,
#                  and no header.match hit elsewhere either)
#        merged -> "file had the same name" (matched by name) | "file has
#                  same headers" (matched by header.match, different name)
#        skipped -> "object limit met" (blocked by max.objects) | "duplicate
#                  file" (blocked by skip.duplicates = TRUE)
#      One gap remained beyond Josh's given four reason strings: a genuine
#      file-READ failure (corrupt/unreadable file) doesn't fit any of them.
#      Added an extra reason, "could not read file", purely for this case,
#      since leaving it unlabeled or mislabeling it as one of the other four
#      seemed worse - flagging this as an addition beyond what was specified.
#
# CORRECTION (2026-08-19, per Josh): the skip.duplicates = TRUE skip reason
# was originally mapped to "headers mismatched" (reusing the closest given
# option) - Josh corrected this: the file is skipped because it has a
# DUPLICATE NAME, not because headers were compared/mismatched (skip.
# duplicates = TRUE never even checks columns). Changed the reason string to
# "duplicate file" instead, which isn't one of Josh's originally-given four
# reason strings either, but accurately describes why the skip happens.
# =============================================================================

suppressMessages(library(readxl))

# -----------------------------------------------------------------------------
# core function
# -----------------------------------------------------------------------------
batz.datawrangler_load.files <- function(dir.load = getwd(),
                                          dir.sub           = FALSE,
                                          remove.duplicates = TRUE,
                                          log.file          = FALSE,
                                          header.match      = FALSE,
                                          skip.duplicates   = FALSE,
                                          max.objects       = 25) {

  read.one <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "csv") {
      return(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE))
    }
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(f)
      if (length(sheets) == 1) {
        return(suppressMessages(as.data.frame(
          readxl::read_excel(f, .name_repair = "minimal"),
          stringsAsFactors = FALSE, check.names = FALSE)))
      }
      sheet.list <- lapply(sheets, function(s) {
        suppressMessages(as.data.frame(
          readxl::read_excel(f, sheet = s, .name_repair = "minimal"),
          stringsAsFactors = FALSE, check.names = FALSE))
      })
      names(sheet.list) <- sheets
      return(sheet.list)
    }
    NULL
  }

  same.columns <- function(a, b) {
    is.data.frame(a) && is.data.frame(b) &&
      ncol(a) == ncol(b) && setequal(names(a), names(b))
  }

  merge.two <- function(a, b) {
    a <- as.data.frame(lapply(a, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    b <- as.data.frame(lapply(b, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    b <- b[names(a)]
    rbind(a, b)
  }

  dedup.frame <- function(df, label) {
    if (!remove.duplicates) return(df)
    dup.mask <- duplicated(df)
    n.dup <- sum(dup.mask)
    if (n.dup > 0) {
      cat("  ", label, ": ", n.dup, " duplicate row(s) removed\n", sep = "")
    }
    df[!dup.mask, , drop = FALSE]
  }

  log.rows <- list()
  add.log <- function(filename, status, action, reason, objectname) {
    if (!log.file) return(invisible(NULL))
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filename = filename, status = status, action = action,
      reason = reason, objectname = objectname, stringsAsFactors = FALSE)
  }

  find.header.match <- function(obj, loaded) {
    if (!is.data.frame(obj)) return(NULL)
    for (nm in names(loaded)) {
      if (same.columns(loaded[[nm]], obj)) return(nm)
    }
    NULL
  }

  all.files <- list.files(dir.load, pattern = "\\.(csv|xlsx)$",
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  loaded <- list()
  n.capped <- 0

  if (length(all.files) == 0) {
    cat("No .csv/.xlsx files found.\n")
  } else {
    for (f in all.files) {
      base.name <- tools::file_path_sans_ext(basename(f))

      obj <- tryCatch(read.one(f), error = function(e) {
        cat("  [skipped] could not read '", f, "': ", conditionMessage(e), "\n", sep = "")
        NULL
      })
      if (is.null(obj)) {
        add.log(basename(f), "failed", "skipped", "could not read file", NA_character_)
        next
      }

      name.exists <- base.name %in% names(loaded)

      # ---- name-based duplicate handling (skip.duplicates governs this) ----
      if (name.exists) {
        if (skip.duplicates) {
          add.log(basename(f), "failed", "skipped", "duplicate file", NA_character_)
          cat("  skipped '", f, "' - an object named '", base.name,
              "' already exists (skip.duplicates = TRUE)\n", sep = "")
          next
        }
        existing <- loaded[[base.name]]
        if (same.columns(existing, obj)) {
          loaded[[base.name]] <- merge.two(existing, obj)
          add.log(basename(f), "success", "merged", "file had the same name", base.name)
          cat("  loaded '", f, "' -> merged into existing '", base.name,
              "' (same name)\n", sep = "")
          next
        }
        # same name, different columns - falls through to header.match below;
        # if that doesn't find a home either, it becomes its own alt-named object
        needs.alt.name <- TRUE
      } else {
        needs.alt.name <- FALSE
      }

      # ---- header.match fallback (any file not already resolved above) ----
      match.name <- if (header.match) find.header.match(obj, loaded) else NULL

      if (!is.null(match.name)) {
        loaded[[match.name]] <- merge.two(loaded[[match.name]], obj)
        add.log(basename(f), "success", "merged", "file has same headers", match.name)
        cat("  loaded '", f, "' -> merged into existing '", match.name,
            "' (matching headers)\n", sep = "")
        next
      }

      # ---- otherwise this file needs to become its own new object ----
      if (length(loaded) >= max.objects) {
        add.log(basename(f), "failed", "skipped", "object limit met", NA_character_)
        n.capped <- n.capped + 1
        cat("  skipped '", f, "' - max.objects =", max.objects, "reached\n")
        next
      }

      final.name <- if (needs.alt.name) paste0(base.name, " [", basename(dirname(f)), "]") else base.name
      reason <- if (needs.alt.name) "file with same name already exists with different headers" else "file was first of its kind"
      loaded[[final.name]] <- obj
      add.log(basename(f), "success", "loaded", reason, final.name)
      cat("  loaded '", f, "' -> ", final.name, "\n", sep = "")
    }
  }

  if (n.capped > 0) {
    cat("\nmax.objects = ", max.objects, " reached - ", n.capped,
        " additional file(s) were NOT loaded as new objects (merges into existing objects were still allowed; increase max.objects to load more new ones).\n", sep = "")
  }

  cat("\n", length(loaded), " object(s) created: ", paste(names(loaded), collapse = ", "), "\n", sep = "")

  # ---- remove.duplicates: within each individual data frame, never across ----
  for (nm in names(loaded)) {
    obj <- loaded[[nm]]
    if (is.data.frame(obj)) {
      loaded[[nm]] <- dedup.frame(obj, nm)
    } else if (is.list(obj)) {
      obj2 <- lapply(seq_along(obj), function(i)
        dedup.frame(obj[[i]], paste0(nm, " [sheet: ", names(obj)[i], "]")))
      names(obj2) <- names(obj)
      loaded[[nm]] <- obj2
    }
  }

  # ---- log.file ----
  if (log.file) {
    loaded[["log.file"]] <- if (length(log.rows) > 0) {
      do.call(rbind, log.rows)
    } else {
      data.frame(filename = character(0), status = character(0), action = character(0),
                 reason = character(0), objectname = character(0), stringsAsFactors = FALSE)
    }
  }

  caller.env <- parent.frame()
  for (nm in names(loaded)) {
    assign(nm, loaded[[nm]], envir = caller.env)
  }

  invisible(loaded)
}

# -----------------------------------------------------------------------------
# tests - carried forward from the previous round (name-dup merge/no-merge,
# multi-sheet xlsx, dedup, empty dir, both calling styles)
# -----------------------------------------------------------------------------
cat("=== dir.sub = FALSE (top-level only; subdir files should NOT appear) ===\n")
rm(list = ls()[ls() %in% c("Acoustic_SiteVisitARU", "HabitatAssessments_quad",
                            "HabitatAssessments_20m", "2HabitatAssessments_quad",
                            "test_07072026_07082026_ARUdeployments", "recode", "log.file")])
res.top <- batz.datawrangler_load.files("/home/claude/loadfiles_work/top", dir.sub = FALSE)
cat("\nobjects returned:", paste(names(res.top), collapse = ", "), "\n")

cat("\n\n=== dir.sub = TRUE (default skip.duplicates/header.match - unchanged\n",
    "behavior from the previous round) ===\n", sep = "")
res.all <- batz.datawrangler_load.files("/home/claude/loadfiles_work/top", dir.sub = TRUE)
cat("\nobjects returned:", paste(names(res.all), collapse = ", "), "\n")
cat("HabitatAssessments_quad nrow (merged + deduped):", nrow(res.all$HabitatAssessments_quad), "\n")
cat("Acoustic_SiteVisitARU [subdir] exists (mismatched dup kept separate):",
    "Acoustic_SiteVisitARU [subdir]" %in% names(res.all), "\n")

cat("\n\n=== empty directory ===\n")
empty.dir <- tempfile(); dir.create(empty.dir)
res.empty <- batz.datawrangler_load.files(empty.dir, dir.sub = TRUE)
cat("length(res.empty):", length(res.empty), "\n")

cat("\n\n=== log.file = TRUE now has $reason and $objectname too ===\n")
res.log <- batz.datawrangler_load.files("/home/claude/loadfiles_work/top", dir.sub = TRUE, log.file = TRUE)
print(res.log$log.file)

# -----------------------------------------------------------------------------
# NEW: skip.duplicates tests (v2a - same name, same columns, different rows)
# -----------------------------------------------------------------------------
cat("\n\n=== skip.duplicates = FALSE (default) - v2a/fileA.csv +\n",
    "v2a/subdir/fileA.csv should MERGE (same columns) ===\n", sep = "")
res.v2a.merge <- batz.datawrangler_load.files("/home/claude/loadfiles_work/v2a", dir.sub = TRUE, log.file = TRUE)
cat("fileA nrow (should be 4 - both files' rows):", nrow(res.v2a.merge$fileA), "\n")
print(res.v2a.merge$log.file)

cat("\n\n=== skip.duplicates = TRUE - the subdir copy should be SKIPPED,\n",
    "not merged (fileA should keep only the top-level file's 2 rows) ===\n", sep = "")
res.v2a.skip <- batz.datawrangler_load.files("/home/claude/loadfiles_work/v2a", dir.sub = TRUE,
                                              skip.duplicates = TRUE, log.file = TRUE)
cat("fileA nrow (should be 2 - subdir copy skipped):", nrow(res.v2a.skip$fileA), "\n")
print(res.v2a.skip$log.file)

# -----------------------------------------------------------------------------
# NEW: header.match tests (v2b - different names, identical column SET)
# -----------------------------------------------------------------------------
cat("\n\n=== header.match = FALSE (default) - fileA.csv and fileB_altname.csv\n",
    "(same columns, different names) should stay as TWO separate objects ===\n", sep = "")
res.v2b.off <- batz.datawrangler_load.files("/home/claude/loadfiles_work/v2b", dir.sub = FALSE, log.file = TRUE)
cat("objects (excl. log.file):", paste(setdiff(names(res.v2b.off), "log.file"), collapse = ", "), "\n")

cat("\n\n=== header.match = TRUE - fileB_altname.csv should MERGE into fileA\n",
    "(same headers, different name) instead of becoming its own object ===\n", sep = "")
res.v2b.on <- batz.datawrangler_load.files("/home/claude/loadfiles_work/v2b", dir.sub = FALSE,
                                            header.match = TRUE, log.file = TRUE)
cat("objects (excl. log.file):", paste(setdiff(names(res.v2b.on), "log.file"), collapse = ", "), "\n")
cat("fileA nrow (should be 3 - both files merged):", nrow(res.v2b.on$fileA), "\n")
print(res.v2b.on$log.file)

# -----------------------------------------------------------------------------
# NEW: max.objects tests (v2c - merges must NOT count toward/be blocked by cap)
# -----------------------------------------------------------------------------
cat("\n\n=== max.objects = 2, v2c: fileA.csv + fileB.csv (2 new objects, hits\n",
    "the cap) + subdir/fileA.csv (SAME NAME as fileA - should still MERGE,\n",
    "not get blocked by the cap) + subdir/fileC.csv (brand-new name/columns -\n",
    "SHOULD be skipped, object limit met) ===\n", sep = "")
res.v2c <- batz.datawrangler_load.files("/home/claude/loadfiles_work/v2c", dir.sub = TRUE,
                                         max.objects = 2, log.file = TRUE)
cat("objects (excl. log.file):", paste(setdiff(names(res.v2c), "log.file"), collapse = ", "), "\n")
cat("fileA nrow (should be 3 - merge succeeded even though cap was already met):",
    nrow(res.v2c$fileA), "\n")
cat("fileC exists (should be FALSE - blocked by the cap):", "fileC" %in% names(res.v2c), "\n")
print(res.v2c$log.file)

# =============================================================================
# batz.arumeta_merge.format.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.arumeta_merge.format() - tested against real test data
# before being wrapped into the final function (batz.arumeta_merge.format.R).
#
# Purpose (per spec): search a directory (and its subdirectories, if
# dir.sub = TRUE) for .csv / .xlsx files, sort them into three categories by
# filename (or, for a workbook ending in "ARUdeployments.xlsx", by sheet
# name), merge each category's files into one data frame, remove duplicate
# rows, and report how many were removed.
#
# NAME: renamed from Josh's "batz.arumeta.merge&format" to
# batz.arumeta_merge.format() to match the project's established convention
# (family "arumeta", "_" before the action, "." separating multi-word
# actions) - same pattern as the existing batz.templogger_merge.format().
#
# Test data (staged from the "hildas" device folder, "1Raw Testdata"):
#   Acoustic_SiteVisitARU.csv                    -> "sitevis"  category
#   HabitatAssessments_quad.csv                  -> "quad"     category
#   2HabitatAssessments_quad.csv (byte-identical -> "quad"     category
#     duplicate of the above - deliberate test of cross-file dedup)
#   HabitatAssessments_20m.csv                   -> "20m"      category
#   test_07072026_07082026_ARUdeployments.xlsx, sheets:
#     "HabitatAssessments_20m"  -> "20m"     category
#     "HabitatAssessments_quad" -> "quad"    category
#     "sitevist" (typo'd sheet name, NOT "SiteVisitARU") -> "sitevis" category
#   Plus, for the dir.sub=TRUE recursion test, a synthetic nested copy of
#   HabitatAssessments_20m.csv was placed one level down (not in Josh's real
#   folder - built locally in this sandbox for testing only).
#
# NOTE (2026-08-18): Josh confirmed count.missing/list.missing/count.duplicates/
# list.duplicates were leftover boilerplate from his spec template, not
# actually wanted for this function - removed from the signature entirely.
#
# NOTE (2026-08-18, later): Josh also confirmed first.match was never supposed
# to be part of this function's spec either - removed from the signature too.
# batz.arumeta_merge.format() now takes dir.load, load.pattern, dir.sub, and
# log.file (see the 2026-08-20 note above for the load.pattern/dir.sub change).
#
# NOTE (2026-08-19, per Josh): dir.path renamed to dir.load, to match the
# parameter name used by batz.datawrangler_load.files for the same concept
# (directory to search/load from) - names should be constant across functions.
#
# NOTE (2026-08-20, per Josh): cross-function optional-input naming pass -
# "sub.dir" renamed to "dir.sub" (default flipped TRUE -> FALSE, matching the
# new project-wide default), and a new "load.pattern" input added
# (default c("*.csv", "*.xlsx")) replacing the previously-hardwired
# "\\.(csv|xlsx)$" pattern. batz.arumeta_merge.format() now takes dir.load,
# load.pattern, dir.sub, and log.file.
#
# BUG FOUND AND FIXED (2026-08-18): the original version of this script called
# as.data.frame(lapply(d, as.character)) with the default check.names = TRUE
# while combining sources within a category. That silently renamed every
# column with a space or special character (e.g. "Specify other project." ->
# "Specify.other.project.") - but the code was still selecting the FINAL
# column set by the ORIGINAL (unmangled) names, so every one of those
# mangled-and-therefore-unmatched columns got silently replaced with an
# all-NA column instead of the real data. On the real site-visit data this
# wiped out "Site Name", "Reason for site visit", "ARU Serial Number", "Mic
# Serial Number", and about 20 other columns down to 100% NA in the merged
# aru.visit data frame - a serious silent data-loss bug, not just a cosmetic
# one. Root cause + fix explained in assumption 5 below.
#
# NEW FEATURE (2026-08-18, per Josh): if a single input file/sheet has the
# same column name more than once, check whether every duplicate holds
# identical content; if so, drop the extra copy(ies); if the content
# differs anywhere, merge them into one column instead. See assumption 10.
#
# NEW FEATURE (2026-08-18, per Josh): optional log.file = FALSE. When TRUE,
# an additional data frame called arumeta.mergelog is added to the returned
# list, with one row per deletion/merge action taken (columns: $inputfile,
# $event, $action, $count). See assumption 12.
#
# NEW FEATURE (2026-08-18, per Josh): after assigning the function's result
# to a variable, aru.visit/aru.quad/aru.20m/arumeta.mergelog only existed
# nested inside that list (result$aru.visit etc.) - Josh expected them to
# show up as their own separate data frames in his environment (matching how
# his original spec phrased log.file as "generate a dataframe called
# arumeta.mergelog", i.e. a standalone object, not a list element). Now, in
# addition to being returned, each non-NULL output is also assign()-ed
# directly into the caller's environment under its own name, so simply
# calling batz.arumeta_merge.format(...) - with no assignment needed -
# creates aru.visit/aru.quad/aru.20m (and arumeta.mergelog if log.file=TRUE)
# right in the workspace. The list is still returned too (invisibly), for
# anyone who prefers result$aru.visit-style access. See assumption 13.
#
# ASSUMPTIONS / OPEN QUESTIONS (flagging per project convention - see the
# message accompanying this script for the specific ones I'm asking Josh to
# confirm before finalizing):
#   1. Category matching is done with a short, case-insensitive SUBSTRING,
#      not the exact patterns Josh listed ("SiteVisitARU", "Assessments_quad",
#      "Assessments_20m"). This was necessary because the real xlsx workbook's
#      site-visit sheet is named "sitevist" (typo, missing the second "i") -
#      it does NOT contain "SiteVisitARU" as a substring, so matching on that
#      exact string would silently miss it. Substrings used instead: "sitevis"
#      (matches both "SiteVisitARU" and "sitevist"), "quad", "20m". These are
#      checked against the file name for .csv files, and against the SHEET
#      name (not the file name) for sheets inside an "...ARUdeployments.xlsx"
#      workbook.
#   2. `dir.load` was added as an explicit argument (default `getwd()`) rather
#      than hard-wiring the function to the R working directory - keeps it
#      testable/reusable while still matching "search the current directory"
#      as the default behavior.
#   3. For an xlsx file, ONLY files whose name ends in "ARUdeployments.xlsx"
#      (case-insensitive) are opened sheet-by-sheet and classified by sheet
#      name. Any other .xlsx file is classified as a single unit by its own
#      file name, the same way a .csv is - not explicitly stated in the spec,
#      but the spec's wording ties the "check sheets" behavior specifically to
#      that filename ending.
#   4. All columns are coerced to character before combining multiple sources
#      within the same category. This was necessary because the same logical
#      column (e.g. a date/time field) comes back as POSIXct from readxl but
#      as plain text from read.csv, and R's rbind/merge cannot combine
#      differently-typed columns of the same name directly. Matches the
#      character-based approach already used in batz.datawrangler_rename.
#   5. Combining data frames within a category uses a fill-by-NA row bind (a
#      small hand-written helper, not a package dependency): if one source has
#      a column the others don't, the others get that column filled with NA
#      rather than erroring. The union/select step now explicitly passes
#      check.names = FALSE (this is the bug fix above) so raw column names
#      (spaces, punctuation, etc.) are preserved exactly and correctly matched
#      up across sources - no more silent mangling/NA-wipe.
#   6. "Remove duplicate rows" = exact full-row duplicates (base `duplicated()`
#      across every real data column of the merged category data frame, i.e.
#      excluding the internal `.source.file` tracking column - see 12), which
#      is what catches the deliberate byte-identical HabitatAssessments_quad.csv
#      / 2HabitatAssessments_quad.csv pair in the test data.
#   7. The duplicate-row removal and its print ("name of data frame + number
#      of duplicate rows removed") happen UNCONDITIONALLY, per the Steps
#      section - there's no longer a count.duplicates/list.duplicates flag to
#      consider gating it behind (removed per Josh, see NOTE above).
#   8. (removed) first.match was dropped from the function entirely per Josh -
#      it was never supposed to be part of this spec (same as the four
#      count./list. flags above).
#   9. No "format" step is implemented yet - the Steps section only covers
#      search/load/merge and duplicate removal/reporting, nothing about
#      standardizing columns, dates, etc. (which is what "format" meant in
#      batz.templogger_merge.format). Left out until Josh specifies it.
#  10. Within-file duplicate columns (per Josh, 2026-08-18): both CSV reads
#      (check.names = FALSE) and xlsx reads (.name_repair = "minimal") now
#      preserve a file/sheet's raw column names AS-IS, including literal
#      duplicates (e.g. the real site-visit form has "Personnel" twice, both
#      in the .csv and in the xlsx "sitevist" sheet). For every duplicate
#      name found in a single source: values are compared row-by-row treating
#      NA and "" (blank) as equivalent; if every row matches, the extra
#      copy(ies) are dropped and the name prints once with the number of
#      copies removed; if any row differs, the columns are merged into ONE
#      column by taking whichever copy is non-blank for that row, and where
#      BOTH copies are non-blank and different, concatenating them with "; "
#      (flagging a genuine conflict rather than picking one arbitrarily). On
#      the real "Personnel" columns in both the .csv and the "sitevist" sheet,
#      most rows match ("BRF"/"BRF") but one row has "BRF" vs "ECG" - merged
#      to "BRF; ECG" for that row.
#  11. Column names now also have a UTF-8 BOM character stripped if present
#      (the real Acoustic_SiteVisitARU.csv's first column comes in as
#      "﻿ObjectID" due to a byte-order-mark at the start of the file).
#      Without this, "ObjectID" from the csv and "ObjectID" from the xlsx
#      sheet would never line up as the same column across sources. Not
#      explicitly requested, but an obviously-correct fix once noticed.
#  12. log.file / arumeta.mergelog: interpreted "$inputfile = name of file the
#      event happened to" literally, which required tracking row-level
#      provenance. Every source's rows get a hidden `.source.file` column when
#      first read (stripped back out before any data frame is returned); when
#      duplicate ROWS are removed at the category-merge step, the removed rows
#      are tallied by which source file they came from, so
#      e.g. "HabitatAssessments_quad.csv" and "2HabitatAssessments_quad.csv"
#      get logged separately rather than lumping the event under the category
#      name. For duplicate COLUMN events, $inputfile is the source label
#      already used in the console messages (filename, or
#      "filename [sheet: sheetname]" for an xlsx sheet). $event is either
#      "duplicated row" or "duplicated column" (Josh's literal wording).
#      $action is "deletion" (identical copies, extras just dropped) or
#      "merging" (content differed, combined into one). $count is the number
#      of extra rows/columns the action was done to (i.e. NOT counting the one
#      survivor) - e.g. 3 copies of a column with differing content logs
#      count = 2 (merging), matching "how many were collapsed into the
#      survivor". Only actions that actually did something (count > 0) get a
#      log row - a category/file with nothing duplicated adds nothing to the
#      log, rather than a row saying "0". `arumeta.mergelog` is only added to
#      the returned list when log.file = TRUE (so existing callers that use
#      log.file = FALSE, the default, see no change to the return shape).
#  13. Auto-assign into caller's environment (per Josh, 2026-08-18): after
#      building `result`, the function now loops over its (non-NULL) elements
#      and calls assign(name, value, envir = parent.frame()) for each one, so
#      aru.visit/aru.quad/aru.20m (and arumeta.mergelog, if log.file = TRUE)
#      land as their own standalone data frames in whatever environment the
#      function was called from - no assignment required. `parent.frame()` is
#      evaluated at call time, so this correctly targets .GlobalEnv when
#      called at the console/top level, or a calling function's local
#      environment if called from inside one. The list is still built and
#      returned - just invisibly now (`invisible(result)` instead of a bare
#      `result`) so it doesn't also print to the console on a bare call - so
#      `result <- batz.arumeta_merge.format(...)` + `result$aru.visit` still
#      works exactly as before for anyone who prefers that style.
# =============================================================================

suppressMessages(library(readxl))

# -----------------------------------------------------------------------------
# core function
# -----------------------------------------------------------------------------
batz.arumeta_merge.format <- function(dir.load = getwd(),
                                       load.pattern     = c("*.csv", "*.xlsx"),
                                       dir.sub          = FALSE,
                                       log.file         = FALSE) {

  ## convert a plain wildcard/glob suffix pattern (or vector of them) into
  ## one combined regex suitable for list.files()'s pattern= argument
  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  log.rows <- list()
  add.log <- function(inputfile, event, action, count) {
    if (!log.file || count <= 0) return(invisible(NULL))
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      inputfile = inputfile, event = event, action = action, count = as.integer(count),
      stringsAsFactors = FALSE)
  }

  resolve.dup.columns <- function(df, source.label = "") {
    nm <- names(df)
    dup.names <- unique(nm[duplicated(nm)])
    if (length(dup.names) == 0) return(df)

    keep <- rep(TRUE, ncol(df))
    for (dn in dup.names) {
      idx <- which(nm == dn)
      cols <- lapply(idx, function(i) {
        v <- as.character(df[[i]])
        ifelse(is.na(v) | v == "", NA_character_, v)
      })

      same <- all(sapply(cols[-1], function(v) identical(v, cols[[1]])))

      if (same) {
        cat("  [", source.label, "] duplicate column '", dn, "' (", length(idx),
            " copies) - identical content, dropped ", length(idx) - 1,
            " extra copy(ies)\n", sep = "")
        keep[idx[-1]] <- FALSE
        add.log(source.label, "duplicated column", "deletion", length(idx) - 1)
      } else {
        merged <- cols[[1]]
        for (i in 2:length(cols)) {
          this.col <- cols[[i]]
          conflict <- !is.na(merged) & !is.na(this.col) & merged != this.col
          fill     <- is.na(merged) & !is.na(this.col)
          merged[fill]     <- this.col[fill]
          merged[conflict] <- paste(merged[conflict], this.col[conflict], sep = "; ")
        }
        cat("  [", source.label, "] duplicate column '", dn, "' (", length(idx),
            " copies) - content differs, merged into one column\n", sep = "")
        df[[idx[1]]] <- merged
        keep[idx[-1]] <- FALSE
        add.log(source.label, "duplicated column", "merging", length(idx) - 1)
      }
    }
    df[keep]
  }

  bind.fill <- function(df.list) {
    df.list <- df.list[!sapply(df.list, is.null)]
    if (length(df.list) == 0) return(NULL)
    if (length(df.list) == 1) return(as.data.frame(df.list[[1]], stringsAsFactors = FALSE, check.names = FALSE))

    all.cols <- unique(unlist(lapply(df.list, names)))
    df.list <- lapply(df.list, function(d) {
      d <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE, check.names = FALSE)
      missing.cols <- setdiff(all.cols, names(d))
      for (mc in missing.cols) d[[mc]] <- NA_character_
      d[all.cols]
    })
    do.call(rbind, df.list)
  }

  strip.bom <- function(df) {
    names(df) <- gsub("﻿", "", names(df), fixed = TRUE)
    df
  }

  category.patterns <- c(aru.visit = "sitevis", aru.quad = "quad", aru.20m = "20m")

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  buckets <- setNames(vector("list", length(category.patterns)), names(category.patterns))

  add.to.bucket <- function(cat.name, df, source.label) {
    df <- strip.bom(df)
    df <- resolve.dup.columns(df, source.label)
    df[[".source.file"]] <- source.label
    buckets[[cat.name]][[length(buckets[[cat.name]]) + 1]] <<- df
    cat("  matched", source.label, "->", cat.name, "\n")
  }

  classify <- function(name.lower) {
    hit <- names(category.patterns)[sapply(category.patterns, function(p) grepl(p, name.lower, ignore.case = TRUE))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  for (f in all.files) {
    fname <- basename(f)

    if (grepl("\\.csv$", fname, ignore.case = TRUE)) {
      cat.name <- classify(fname)
      if (!is.na(cat.name)) {
        df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
        add.to.bucket(cat.name, df, fname)
      }
    } else if (grepl("\\.xlsx$", fname, ignore.case = TRUE)) {
      if (grepl("ARUdeployments\\.xlsx$", fname, ignore.case = TRUE)) {
        sheets <- excel_sheets(f)
        for (s in sheets) {
          cat.name <- classify(s)
          if (!is.na(cat.name)) {
            df <- suppressMessages(as.data.frame(
              read_excel(f, sheet = s, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
            add.to.bucket(cat.name, df, paste0(fname, " [sheet: ", s, "]"))
          }
        }
      } else {
        cat.name <- classify(fname)
        if (!is.na(cat.name)) {
          df <- suppressMessages(as.data.frame(
            read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
          add.to.bucket(cat.name, df, fname)
        }
      }
    }
  }

  result <- list()
  for (cat.name in names(category.patterns)) {
    merged <- bind.fill(buckets[[cat.name]])
    if (is.null(merged)) {
      cat("\n", cat.name, ": no matching files found\n", sep = "")
      result[[cat.name]] <- NULL
      next
    }
    data.cols <- setdiff(names(merged), ".source.file")
    dup.mask <- duplicated(merged[data.cols])
    n.dup <- sum(dup.mask)

    if (n.dup > 0) {
      removed.by.file <- table(merged$.source.file[dup.mask])
      for (fn in names(removed.by.file)) {
        add.log(fn, "duplicated row", "deletion", as.integer(removed.by.file[[fn]]))
      }
    }

    cat("\n", cat.name, ": ", n.dup, " duplicate row(s) removed\n", sep = "")
    result[[cat.name]] <- merged[!dup.mask, data.cols, drop = FALSE]
  }

  if (log.file) {
    result$arumeta.mergelog <- if (length(log.rows) > 0) {
      do.call(rbind, log.rows)
    } else {
      data.frame(inputfile = character(0), event = character(0),
                 action = character(0), count = integer(0), stringsAsFactors = FALSE)
    }
  }

  caller.env <- parent.frame()
  for (nm in names(result)) {
    if (!is.null(result[[nm]])) assign(nm, result[[nm]], envir = caller.env)
  }

  invisible(result)
}

# -----------------------------------------------------------------------------
# tests
# -----------------------------------------------------------------------------
cat("=== dir.sub = FALSE (top-level only; nested copy should NOT appear) ===\n")
res.top <- batz.arumeta_merge.format("/home/claude/arumeta_work/top", dir.sub = FALSE)
cat("\naru.visit rows:", nrow(res.top$aru.visit),
    "| aru.quad rows:", nrow(res.top$aru.quad),
    "| aru.20m rows:", nrow(res.top$aru.20m), "\n")

cat("\n\n=== dir.sub = TRUE (should also pick up the nested 20m copy) ===\n")
res.all <- batz.arumeta_merge.format("/home/claude/arumeta_work/top", dir.sub = TRUE)
cat("\naru.visit rows:", nrow(res.all$aru.visit),
    "| aru.quad rows:", nrow(res.all$aru.quad),
    "| aru.20m rows:", nrow(res.all$aru.20m), "\n")

cat("\n\n=== aru.visit real data check - these should NOT be all-NA anymore ===\n")
print(sapply(res.all$aru.visit[c("Site Name", "Reason for site visit", "ARU Serial Number", "Personnel")],
             function(x) sum(!is.na(x) & x != "")))

cat("\n\n=== log.file = FALSE (default) - result should have NO arumeta.mergelog element ===\n")
res.nolog <- batz.arumeta_merge.format("/home/claude/arumeta_work/top", dir.sub = TRUE)
cat("has arumeta.mergelog:", !is.null(res.nolog$arumeta.mergelog), "\n")

cat("\n\n=== log.file = TRUE - arumeta.mergelog contents ===\n")
res.log <- batz.arumeta_merge.format("/home/claude/arumeta_work/top", dir.sub = TRUE, log.file = TRUE)
print(res.log$arumeta.mergelog)

cat("\n\n=== auto-assign - bare call (no assignment) should still create\n",
    "aru.visit/aru.quad/aru.20m/arumeta.mergelog as standalone objects here ===\n", sep = "")
rm(list = intersect(c("aru.visit", "aru.quad", "aru.20m", "arumeta.mergelog"), ls()))
cat("before call - exist? aru.visit:", exists("aru.visit"),
    "| aru.quad:", exists("aru.quad"),
    "| aru.20m:", exists("aru.20m"),
    "| arumeta.mergelog:", exists("arumeta.mergelog"), "\n")
batz.arumeta_merge.format("/home/claude/arumeta_work/top", dir.sub = TRUE, log.file = TRUE)
cat("after bare call - exist? aru.visit:", exists("aru.visit"),
    "| aru.quad:", exists("aru.quad"),
    "| aru.20m:", exists("aru.20m"),
    "| arumeta.mergelog:", exists("arumeta.mergelog"), "\n")
cat("nrow(aru.visit):", nrow(aru.visit), "| nrow(aru.quad):", nrow(aru.quad),
    "| nrow(aru.20m):", nrow(aru.20m), "| nrow(arumeta.mergelog):", nrow(arumeta.mergelog), "\n")

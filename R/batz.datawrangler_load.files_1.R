#' Load every .csv/.xlsx file in a directory into its own R object
#'
#' Scans a directory (and, optionally, its subdirectories) for \code{.csv}
#' and \code{.xlsx} files and loads each one into R as its own standalone
#' object, named the same as the file (extension stripped). A \code{.csv},
#' or an \code{.xlsx} workbook with only one sheet, becomes a data frame. An
#' \code{.xlsx} workbook with more than one sheet becomes a list (one data
#' frame per sheet, named by sheet name).
#'
#' Two independent mechanisms can combine files into one data frame instead
#' of creating separate objects: a duplicate FILE NAME (see
#' \code{skip.duplicates}) and, optionally, a duplicate HEADER SET across
#' differently-named files (see \code{header.match}).
#'
#' @param dir.load Character. Directory to search. Defaults to the current
#'   working directory (\code{getwd()}).
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also
#'   searches every subdirectory of \code{dir.load}.
#' @param remove.duplicates Logical, default \code{TRUE}. If \code{TRUE},
#'   exact duplicate rows are removed from WITHIN each individual data frame
#'   (including separately within each sheet of a multi-sheet \code{.xlsx}
#'   list) - never across two different top-level objects. Runs as a final
#'   pass after all files are loaded/merged, so it also catches duplicates
#'   that only appear once two files have been merged together.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, an
#'   additional object named \code{log.file} is created (see \code{Value}
#'   below), with one row per file the scan encountered: \code{$filename},
#'   \code{$status} (\code{"success"}/\code{"failed"}), \code{$action}
#'   (\code{"loaded"}/\code{"merged"}/\code{"skipped"}), \code{$reason} (see
#'   \code{Details}), \code{$objectname} (the object the file ended up in,
#'   or \code{NA} if it wasn't loaded at all).
#' @param header.match Logical, default \code{FALSE}. If \code{TRUE}, every
#'   file - not just ones sharing a file name with something already loaded
#'   - has its column-name set compared against every already-loaded plain
#'   data frame (in the order those were created); if one matches 100%
#'   (same columns, order doesn't matter), the new file is merged into THAT
#'   object instead of becoming its own. Only ever applies to plain
#'   single-sheet data frames - a multi-sheet \code{.xlsx}-derived list is
#'   never compared, as source or as target.
#' @param skip.duplicates Logical, default \code{FALSE}. Governs what
#'   happens when a file's name already matches an object already loaded
#'   (only possible when \code{dir.sub = TRUE} and the duplicate lives in a
#'   different folder). \code{TRUE}: the duplicate is skipped outright -
#'   nothing is checked or merged. \code{FALSE} (default): merge it into the
#'   existing object if the columns match; if they don't, it falls through to
#'   the \code{header.match} check (if enabled) before finally becoming its
#'   own separate object.
#' @param max.objects Integer, default \code{25}. The maximum number of
#'   top-level objects (not merges, and not raw files/sheets) the function
#'   will create in one call. Once that many exist, no further BRAND-NEW
#'   objects are created - but merges (by name or by \code{header.match})
#'   into already-existing objects keep happening regardless, since they
#'   don't add to the count.
#'
#' @return Invisibly, a named list of everything that was loaded (data
#'   frames and/or lists-of-sheets, one element per input file/merged group,
#'   plus \code{log.file} if requested). In addition to being returned,
#'   every element of that list is also assigned as its own object - under
#'   its own name - directly into the environment the function was called
#'   from, so calling \code{batz.datawrangler_load.files(...)} on its own
#'   (with no assignment) loads everything right into your workspace,
#'   including \code{log.file} itself if \code{log.file = TRUE}. You can
#'   still do \code{result <- batz.datawrangler_load.files(...)} and access
#'   things as \code{result$my.file} if you'd rather work with the list. If
#'   a file's name isn't a syntactically "plain" R name (it has spaces,
#'   starts with a digit, etc.), the object is still created exactly as
#'   named - just access it with backticks or \code{get()}.
#'
#' @details
#' For a file whose base name already matches something already loaded, the
#' order of decisions is: (1) \code{skip.duplicates = TRUE} skips it, full
#' stop; (2) otherwise, if its columns match the existing same-named object,
#' it's merged into that object (columns are lined up by name first - "when
#' merging any two files, the new file's columns are reordered to match the
#' original data frame's"); (3) if the columns DON'T match, it falls through
#' to \code{header.match} - checked exactly the same way as for a brand-new
#' file name - before finally becoming its own object under a folder-
#' suffixed alt name (e.g. \code{"myfile [subfolder]"}) if nothing matches.
#'
#' When merging (by either mechanism), all columns are coerced to character
#' first and combined with raw column names preserved exactly
#' (\code{check.names = FALSE}) - the same approach used in
#' \code{batz.arumeta_merge.format}, avoiding type mismatches between sources
#' and avoiding silently mangled column names.
#'
#' \code{$reason} values in \code{log.file}: for \code{action = "loaded"},
#' either \code{"file was first of its kind"} (brand-new name, no
#' \code{header.match} hit) or \code{"file with same name already exists
#' with different headers"} (a same-name duplicate that also didn't match
#' anything via \code{header.match}). For \code{action = "merged"}, either
#' \code{"file had the same name"} or \code{"file has same headers"}. For
#' \code{action = "skipped"}: \code{"object limit met"} (blocked by
#' \code{max.objects}), \code{"duplicate file"} (skipped because
#' \code{skip.duplicates = TRUE} found a same-named file already loaded), or
#' \code{"could not read file"} (the file itself failed to read/parse - not
#' one of the originally-specified reasons, added because there wasn't
#' another option that fit a genuinely corrupt/unreadable file).
#'
#' @examples
#' \dontrun{
#' batz.datawrangler_load.files()
#' # everything in the current directory is now loaded as separate objects
#'
#' result <- batz.datawrangler_load.files("path/to/raw/data", dir.sub = TRUE,
#'                                         header.match = TRUE, log.file = TRUE,
#'                                         max.objects = 50)
#' result$my.file
#' result$log.file
#' }
#'
#' @export
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

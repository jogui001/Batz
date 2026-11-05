#' Load every .csv/.xlsx file in a directory into its own R object
#'
#' Scans a directory (optionally with its subdirectories) for \code{.csv} and
#' \code{.xlsx} files and loads each one as its own object, named after the
#' file without its extension. A \code{.csv}, or an \code{.xlsx} with one
#' sheet, becomes a data frame; an \code{.xlsx} with several sheets becomes a
#' list of data frames named by sheet. Files can also be combined into one
#' data frame by a duplicate file name (\code{duplicates.skip}) or by an
#' identical set of column names (\code{header.match}).
#'
#' @param dir.load Character. Directory to search. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also searches
#'   every subdirectory of \code{dir.load}.
#' @param duplicates.remove Logical, default \code{TRUE}. If \code{TRUE},
#'   exact duplicate rows are removed within each data frame (and within each
#'   sheet of a multi-sheet list), never across objects. Runs after all files
#'   are loaded and merged, so it also catches duplicates created by merging.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, a
#'   \code{log.file} data frame with one row per file found is also created
#'   (see Value).
#' @param header.match Logical, default \code{FALSE}. If \code{TRUE}, a file
#'   not already merged by name is compared with every loaded data frame, in
#'   the order they were created; if one has exactly the same column names
#'   (order does not matter), the file is merged into it. Multi-sheet lists
#'   are never compared, as source or as target.
#' @param duplicates.skip Logical, default \code{FALSE}. Controls a file whose
#'   base name matches an object already loaded (a file in another folder
#'   with \code{dir.sub = TRUE}, or a \code{.csv} and \code{.xlsx} with the
#'   same name). \code{TRUE}: the file is skipped. \code{FALSE}: it is merged
#'   into the existing object if the columns match; otherwise it goes to the
#'   \code{header.match} check, and then becomes its own object.
#' @param objects.max Integer, default \code{25}. Maximum number of new
#'   objects created in one call. When reached, further files that would
#'   become new objects are skipped; merges into existing objects still
#'   happen.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE}, each
#'   file or sheet is cleaned right after reading: accented letters and common
#'   symbols are simplified (accented letters to plain letters, the multiplication sign to \code{X},
#'   curly quotes to straight quotes) and other non-ASCII characters (e.g.
#'   degree, micro and trademark signs) are removed, in column names and text
#'   columns. Files are always read with a UTF-8 / Latin-1 fallback, whatever
#'   this is set to.
#'
#' @return Invisibly, a named list with one element per object created (data
#'   frames, or lists of data frames for multi-sheet workbooks), plus
#'   \code{log.file} when requested. Every element is also assigned into the
#'   calling environment under its own name, so a bare call loads everything
#'   into the workspace. Names that are not plain R names (spaces, a leading
#'   digit, a \code{[folder]} suffix) need backticks or \code{get()}.
#'
#'   Merged objects have all columns as character; unmerged objects keep the
#'   column types from reading. No files are written.
#'
#'   \code{log.file} columns:
#'   \describe{
#'     \item{\code{$filename}}{File name (no path).}
#'     \item{\code{$status}}{\code{"success"} or \code{"failed"}.}
#'     \item{\code{$action}}{\code{"loaded"}, \code{"merged"} or \code{"skipped"}.}
#'     \item{\code{$reason}}{Why (see Details).}
#'     \item{\code{$objectname}}{Object the file ended up in, or \code{NA}.}
#'     \item{\code{$strip.special}}{\code{"FALSE"} (not selected),
#'       \code{"TRUE NONE"} (nothing changed), or \code{"TRUE ; <header>; ..."}
#'       (standardized headers of the text columns where characters changed,
#'       across all sheets of the file).}
#'     \item{\code{$accented.letters.header}, \code{$removed.symbols.header}}{
#'       Number of unique column names with a character simplified / removed.}
#'     \item{\code{$accented.letters.data}, \code{$removed.symbols.data}}{
#'       Number of unique data values with a character simplified / removed
#'       (the same value in 500 rows counts once). The four count columns are
#'       \code{NA} when \code{strip.special = FALSE}.}
#'   }
#'
#' @details
#' \strong{Reading.} A \code{.csv} is read with \code{special.read.csv()}
#' (UTF-8, falling back to Latin-1); an \code{.xlsx} is read with
#' \code{readxl}. Each data frame (or each sheet) is then passed through
#' \code{special.strip()} (when \code{strip.special = TRUE}); a text column
#' that changed is re-typed, so \code{"42.5"} followed by a degree sign becomes the number 42.5.
#' Column names are then standardized with \code{standardize.headers()}:
#' trimmed, each run of non-alphanumeric characters replaced by one
#' underscore, leading/trailing underscores dropped, lower case. File names,
#' and so object names, are not changed. All column matching below compares
#' the standardized names. A file that cannot be read is logged as skipped.
#'
#' \strong{Where each file goes}, in this order:
#' \enumerate{
#'   \item Same base name as a loaded object and \code{duplicates.skip = TRUE}:
#'     skipped (\code{"duplicate file"}).
#'   \item Same base name and the same set of column names: merged into that
#'     object (\code{"file had the same name"}).
#'   \item \code{header.match = TRUE} and a loaded data frame has the same set
#'     of column names: merged into the first one found
#'     (\code{"file has same headers"}).
#'   \item \code{objects.max} objects already exist: skipped
#'     (\code{"object limit met"}).
#'   \item Otherwise a new object. A file with a duplicate name but different
#'     columns is named \code{"<name> [<folder>]"}
#'     (\code{"file with same name already exists with different headers"});
#'     any other file uses its own name (\code{"file was first of its kind"}).
#' }
#' A file that could not be read gets \code{$reason}
#' \code{"could not read file"}.
#'
#' \strong{Merging.} All columns of both data frames are converted to
#' character, the new file's columns are reordered to match the existing
#' object, and the rows are bound with column names kept exactly as they are.
#'
#' \strong{Console.} One line per file, a count of files skipped by
#' \code{objects.max}, the objects created, and the duplicate rows removed
#' per object.
#'
#' @examples
#' \dontrun{
#' batz.datawrangler_load.files()
#' # everything in the current directory is now loaded as separate objects
#'
#' result <- batz.datawrangler_load.files("path/to/raw/data", dir.sub = TRUE,
#'                                         header.match = TRUE, log.file = TRUE,
#'                                         objects.max = 50)
#' result$my.file
#' result$log.file
#' }
#'
#' @seealso \code{\link{batz.datawrangler_rename}}
#'
#' @export
batz.datawrangler_load.files <- function(dir.load = getwd(),
                                          dir.sub           = FALSE,
                                          duplicates.remove = TRUE,
                                          log.file          = FALSE,
                                          header.match      = FALSE,
                                          duplicates.skip   = FALSE,
                                          objects.max       = 25,
                                          strip.special     = TRUE) {

  ## header standardization (per Josh, 2026-09-14 project preference) - see
  ## @details "Header standardization" above. Applied once, right after a
  ## data frame is read, before any merge-matching logic sees its names.
  standardize.df.headers <- function(df) {
    names(df) <- standardize.headers(names(df))
    df
  }

  ## special characters (per Josh, 2026-09-30): strip each file/sheet right
  ## after it's read; the (standardized) headers stripped in the file most
  ## recently read are kept in strip.headers for the log.
  strip.headers <- character(0)
  strip.counts <- NULL   # 2026-10-02: simplified/removed counts for the log
  strip.one <- function(df) {
    s <- special.strip(df, strip.special)
    strip.headers <<- c(strip.headers, standardize.headers(s$headers))
    strip.counts <<- special.counts.add(strip.counts, s$counts)
    s$df
  }

  read.one <- function(f) {
    strip.headers <<- character(0)
    strip.counts <<- NULL
    ext <- tolower(tools::file_ext(f))
    if (ext == "csv") {
      return(standardize.df.headers(strip.one(
        special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE))))
    }
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(f)
      if (length(sheets) == 1) {
        return(standardize.df.headers(strip.one(suppressMessages(as.data.frame(
          readxl::read_excel(f, .name_repair = "minimal"),
          stringsAsFactors = FALSE, check.names = FALSE)))))
      }
      sheet.list <- lapply(sheets, function(s) {
        standardize.df.headers(strip.one(suppressMessages(as.data.frame(
          readxl::read_excel(f, sheet = s, .name_repair = "minimal"),
          stringsAsFactors = FALSE, check.names = FALSE))))
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
    if (!duplicates.remove) return(df)
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
      reason = reason, objectname = objectname,
      special.log.cols(strip.special, strip.headers, strip.counts),
      stringsAsFactors = FALSE)
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
        strip.headers <- character(0)
        strip.counts <- NULL
        add.log(basename(f), "failed", "skipped", "could not read file", NA_character_)
        next
      }

      name.exists <- base.name %in% names(loaded)

      # ---- name-based duplicate handling (duplicates.skip governs this) ----
      if (name.exists) {
        if (duplicates.skip) {
          add.log(basename(f), "failed", "skipped", "duplicate file", NA_character_)
          cat("  skipped '", f, "' - an object named '", base.name,
              "' already exists (duplicates.skip = TRUE)\n", sep = "")
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
      if (length(loaded) >= objects.max) {
        add.log(basename(f), "failed", "skipped", "object limit met", NA_character_)
        n.capped <- n.capped + 1
        cat("  skipped '", f, "' - objects.max =", objects.max, "reached\n")
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
    cat("\nobjects.max = ", objects.max, " reached - ", n.capped,
        " additional file(s) were NOT loaded as new objects (merges into existing objects were still allowed; increase objects.max to load more new ones).\n", sep = "")
  }

  cat("\n", length(loaded), " object(s) created: ", paste(names(loaded), collapse = ", "), "\n", sep = "")

  # ---- duplicates.remove: within each individual data frame, never across ----
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
                 reason = character(0), objectname = character(0),
                 special.log.cols.empty(), stringsAsFactors = FALSE)
    }
  }

  caller.env <- parent.frame()
  for (nm in names(loaded)) {
    assign(nm, loaded[[nm]], envir = caller.env)
  }

  invisible(loaded)
}

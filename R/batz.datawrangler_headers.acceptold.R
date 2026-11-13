#' Rename old (deprecated) headers when data are loaded
#'
#' Renames each column of \code{data} whose name is a known old header
#' spelling (from \code{\link{batz.generate_headers.acceptold}}) to its
#' current name, so files built with older \code{batz} header names still
#' work. A column that cannot be renamed safely is left unchanged, with a
#' warning and (optionally) a row in a conflict log.
#'
#' @param data A data frame whose column names are checked.
#' @param function.name Character, required. Name of the calling \code{batz}
#'   function (e.g. \code{"batz.merge_vetted.acoustics"}); written to the
#'   conflict log.
#' @param filename Character, default \code{deparse(substitute(data))} (the
#'   name of the object passed as \code{data}). File or object name for the
#'   log's \code{$filename} column.
#' @param path Character, default \code{NA_character_}. Path of the file
#'   \code{data} came from, for the log's \code{$path} column.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, a
#'   conflict log is attached to the result as
#'   \code{attr(result, "header.conflict.log")}.
#' @param headers.table Data frame or \code{NULL} (default). A reference
#'   table already read with \code{\link{batz.generate_headers.acceptold}}
#'   (useful when processing many files in a loop). If \code{NULL}, it is
#'   read with \code{batz.generate_headers.acceptold(dir.load, file.name)}.
#' @param dir.load Character, default \code{NULL}: the old-headers table
#'   carried by the package (Batz_reference_db, tab \code{DB_headers.acceptold})
#'   is used. A folder reads the reference CSV from there instead. Used only
#'   when \code{headers.table = NULL}.
#' @param file.name Character, default \code{"batz_headers_acceptold.csv"}.
#'   Reference CSV name; used only when \code{headers.table = NULL}.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   \code{data} and a supplied \code{headers.table} are cleaned at the
#'   start: accented letters and common symbols are simplified
#'   (\code{é} to \code{e}, \code{×} to \code{X}) and other non-ASCII
#'   characters (e.g. \code{°}, \code{µ}, \code{™}) are removed, in column
#'   names and text values.
#'
#' @return \code{data} (a data frame) with each safely matched old header
#'   renamed. Columns not in the reference table are unchanged. When
#'   \code{strip.special = TRUE}, text columns that changed are re-typed
#'   (\code{"42.5°"} becomes the number \code{42.5}). When
#'   \code{log.file = TRUE}, \code{attr(result, "header.conflict.log")} is a
#'   data frame (zero rows if there were no conflicts) with columns
#'   \code{$filename}, \code{$path}, \code{$function}, \code{$header.old},
#'   \code{$header.new}, \code{$strip.special} (\code{"FALSE"},
#'   \code{"TRUE NONE"}, or \code{"TRUE ; <header>; ..."} naming the data
#'   columns that changed), \code{$accented.letters.header},
#'   \code{$removed.symbols.header} (number of unique column names with a
#'   character simplified / removed), \code{$accented.letters.data} and
#'   \code{$removed.symbols.data} (number of unique values with a character
#'   simplified / removed; \code{NA} when \code{strip.special = FALSE}).
#'
#' @details
#' \strong{Matching.} Column names of \code{data} and \code{$header.old} are
#' compared after standardizing (lower case, every run of non-alphanumeric
#' characters turned into \code{_}), so \code{"Mic_Serial_Number"},
#' \code{"mic.serial.number"} and \code{"mic_serial_number"} all match.
#' Columns that do not match any \code{$header.old} are left alone and not
#' logged.
#'
#' \strong{Outcomes for a matched column.}
#' \itemize{
#'   \item One non-blank \code{$header.new}: the column is renamed, with no
#'     warning and no log row.
#'   \item Only a blank \code{$header.new} (no confirmed replacement): left
#'     unchanged, warning, log row with \code{header.new = "NOMATCH"}.
#'   \item More than one different \code{$header.new} for the same
#'     standardized old header: left unchanged, warning, log row with
#'     \code{$header.new} = the candidates separated by \code{"; "}.
#' }
#'
#' \strong{Name collisions.} After renaming, if two columns would end up
#' with the same name (two old spellings of one field in the same file, or
#' an old header whose new name already exists in \code{data}), all columns
#' involved keep their original names, a warning is given and each gets a
#' log row with \code{$header.new} = the shared name.
#'
#' The conflict log is an attribute so the return value is always a plain
#' data frame: \code{data <- batz.datawrangler_headers.acceptold(data, ...)}
#' works whatever \code{log.file} is. The default \code{filename} is taken
#' before \code{data} is cleaned.
#'
#' @seealso \code{\link{batz.generate_headers.acceptold}}
#'
#' @examples
#' \dontrun{
#' # clean renames (nothing is logged)
#' d <- data.frame(ARU.serial = "SM4-01", Alldect = 12)
#' d2 <- batz.datawrangler_headers.acceptold(d, function.name = "batz.example")
#' names(d2)  # "aru.serial" "all.detections"
#'
#' # old header with no confirmed replacement
#' d <- data.frame(project_code = "SITE1")
#' d2 <- batz.datawrangler_headers.acceptold(d, function.name = "batz.example",
#'                                            log.file = TRUE)
#' attr(d2, "header.conflict.log")  # one row, header.new = "NOMATCH"
#' }
#'
#' @export
batz.datawrangler_headers.acceptold <- function(data,
                                                 function.name,
                                                 filename = deparse(substitute(data)),
                                                 path = NA_character_,
                                                 log.file = FALSE,
                                                 headers.table = NULL,
                                                 dir.load = NULL,
                                                 file.name = "batz_headers_acceptold.csv",
                                                 strip.special = TRUE) {

  if (missing(function.name) || is.null(function.name) || !nzchar(function.name)) {
    stop("batz.datawrangler_headers.acceptold(): 'function.name' is required - ",
         "pass the name of the batz function calling this (e.g. ",
         "\"batz.merge_vetted.acoustics\"), so any logged conflict can be traced ",
         "back to where it occurred.")
  }

  ## special characters (per Josh, 2026-09-30): strip every data-frame
  ## input up front. filename is forced first so its default
  ## deparse(substitute(data)) still names the caller's object.
  force(filename)
  s <- special.strip(data, strip.special)
  data <- s$df
  strip.cols <- special.log.cols(strip.special, s$headers, s$counts)
  headers.table <- special.strip(headers.table, strip.special)$df

  if (is.null(headers.table)) {
    headers.table <- batz.generate_headers.acceptold(dir.load = dir.load, file.name = file.name)
  }

  nm.orig <- names(data)
  nm.std  <- standardize.headers(nm.orig)
  old.std <- standardize.headers(headers.table$header.old)

  ## Group the reference table by its own standardized $header.old, so a
  ## header spelled two different ways that the workbook nonetheless maps
  ## to two DIFFERENT $header.new targets is caught as a real conflict,
  ## not silently resolved by whichever row happens to appear first.
  by.key <- split(headers.table$header.new, old.std)

  nm.new   <- nm.orig
  log.rows <- list()

  for (i in seq_along(nm.orig)) {
    key <- nm.std[i]
    if (!(key %in% names(by.key))) next   # not a recognized old header at all - leave alone

    candidates <- unique(by.key[[key]])
    nonblank   <- unique(candidates[nzchar(candidates)])

    if (length(nonblank) == 0) {
      ## recognized old header, no confirmed replacement yet (blank in the
      ## reference table) - never rename to a blank target.
      warning("batz.datawrangler_headers.acceptold(): column \"", nm.orig[i],
              "\" is a recognized deprecated header with no confirmed ",
              "replacement yet (flagged for review) - left unchanged.",
              call. = FALSE)
      log.rows[[length(log.rows) + 1]] <- data.frame(
        filename = filename, path = path, `function` = function.name,
        header.old = nm.orig[i], header.new = "NOMATCH",
        strip.cols,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    } else if (length(nonblank) > 1) {
      ## genuine ambiguity: this old header standardizes to a key the
      ## reference table maps to more than one distinct new name.
      warning("batz.datawrangler_headers.acceptold(): column \"", nm.orig[i],
              "\" matches more than one possible replacement header (",
              paste(nonblank, collapse = "; "), ") - left unchanged, please resolve.",
              call. = FALSE)
      log.rows[[length(log.rows) + 1]] <- data.frame(
        filename = filename, path = path, `function` = function.name,
        header.old = nm.orig[i], header.new = paste(nonblank, collapse = "; "),
        strip.cols,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    } else {
      nm.new[i] <- nonblank
    }
  }

  ## A safe, unambiguous rename can still collide with another column's
  ## final name - discovered a step later, so it gets the same treatment:
  ## flagged, logged, and reverted rather than silently overwriting or
  ## duplicating a column.
  dupe.targets <- unique(nm.new[duplicated(nm.new)])
  for (dt in dupe.targets) {
    hit <- which(nm.new == dt)
    warning("batz.datawrangler_headers.acceptold(): renaming ",
            paste(sprintf('"%s"', nm.orig[hit]), collapse = " and "),
            " would both produce the column name \"", dt,
            "\" - left unchanged, please resolve.", call. = FALSE)
    for (h in hit) {
      log.rows[[length(log.rows) + 1]] <- data.frame(
        filename = filename, path = path, `function` = function.name,
        header.old = nm.orig[h], header.new = dt,
        strip.cols, stringsAsFactors = FALSE,
        check.names = FALSE
      )
      nm.new[h] <- nm.orig[h]   # revert to original spelling
    }
  }

  names(data) <- nm.new

  if (isTRUE(log.file)) {
    header.conflict.log <- if (length(log.rows) > 0) {
      do.call(rbind, log.rows)
    } else {
      data.frame(filename = character(0), path = character(0),
                 `function` = character(0), header.old = character(0),
                 header.new = character(0), special.log.cols.empty(),
                 stringsAsFactors = FALSE,
                 check.names = FALSE)
    }
    attr(data, "header.conflict.log") <- header.conflict.log
  }

  data
}

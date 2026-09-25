#' Rename deprecated/legacy headers on load, using the package's own
#' header-rename reference table
#'
#' Consults \code{\link{batz.generate_headers.acceptold}}'s reference table
#' and renames any column of \code{data} that matches a recognized old
#' header spelling to its current standard name, so a file or data frame
#' built against an older \code{batz} naming convention keeps working with
#' today's functions. Matching is tolerant of separator/case differences
#' (via \code{standardize.headers()}), the same way
#' \code{\link{canonicalize.headers}} already matches headers elsewhere in
#' this package. A column that can't be safely resolved - because the
#' reference table has no confirmed replacement for it yet, or because it
#' genuinely matches more than one possible replacement - is left
#' unchanged and flagged with a \code{warning()} (and, optionally, a
#' logged row) rather than guessed at.
#'
#' @param data A data frame (or an object \code{names()} works on) whose
#'   column names should be checked/renamed.
#' @param function.name Character, required. Name of the \code{batz}
#'   function calling this (e.g. \code{"batz.merge_vetted.acoustics"}), so
#'   any logged conflict can be traced back to where it occurred.
#' @param filename Character, default the name of whatever was passed in
#'   as \code{data} (via \code{deparse(substitute(data))}). File name (if
#'   \code{data} was just read from disk) or data frame/object name, for
#'   \code{$filename} in the conflict log.
#' @param path Character, default \code{NA}. Path of the file \code{data}
#'   was loaded from, if any, for \code{$path} in the conflict log.
#' @param log.file Logical, default \code{FALSE} (per Josh's spec). If
#'   \code{TRUE}, a \code{header.conflict.log} data frame recording every
#'   column this call could NOT safely resolve is attached to the return
#'   value - see Details for exactly how.
#' @param headers.table Optional, default \code{NULL}. A reference table
#'   already loaded via \code{\link{batz.generate_headers.acceptold}}, to
#'   avoid re-reading the CSV from disk on every call (e.g. a caller that
#'   processes many files in a loop can load it once and pass it through).
#'   When \code{NULL} (the default), it's loaded fresh via
#'   \code{batz.generate_headers.acceptold(dir.load, file.name)}.
#' @param dir.load,file.name Passed through to
#'   \code{\link{batz.generate_headers.acceptold}} when \code{headers.table}
#'   is \code{NULL}. See that function's own documentation.
#'
#' @return \code{data}, with every column that matched exactly one
#'   confirmed replacement header renamed to it. Columns that didn't match
#'   anything in the reference table are left completely untouched. When
#'   \code{log.file = TRUE}, a \code{header.conflict.log} data frame (see
#'   Details) is attached to the returned object as an attribute, i.e.
#'   \code{attr(result, "header.conflict.log")} - the return value is
#'   always just the data frame itself either way, so
#'   \code{data <- batz.datawrangler_headers.acceptold(data, ...)} works
#'   the same regardless of \code{log.file}.
#'
#' @details
#' \strong{Built 2026-09-25, per Josh's request, alongside
#' \code{\link{batz.generate_headers.acceptold}}.} Several judgment calls
#' were made building this; all flagged below - please confirm.
#'
#' \strong{Judgment call, flagged: the conflict log is attached as an
#' attribute, not returned as a second list element.} Josh's own spec asks
#' for a function that "makes the changes required for the script to run"
#' when loading a file/data frame - i.e. it's meant to sit inline in a
#' pipeline (\code{data <- batz.datawrangler_headers.acceptold(data, ...)})
#' - so this keeps the return value a plain data frame in both
#' \code{log.file} states, rather than changing shape to
#' \code{list(data = ..., header.conflict.log = ...)} whenever
#' \code{log.file = TRUE}, which would break that inline use every time
#' logging is turned on. The log is still fully accessible via
#' \code{attr(result, "header.conflict.log")}. Please confirm this is the
#' right shape, versus preferring the list form despite the pipeline
#' inconvenience.
#'
#' \strong{Judgment call, flagged: only conflicts are logged, not clean
#' renames.} Josh's spec describes \code{header.conflict.log} as
#' generated "if there is a conflict" and names its purpose as recording
#' "the conflict" - so a column that matches exactly one confirmed
#' replacement is renamed silently (no log row at all); only a column
#' that could NOT be safely renamed - no confirmed replacement yet
#' (logged as \code{header.new = "NOMATCH"}), or a genuine match to more
#' than one possible replacement (logged as \code{header.new} = the
#' candidates, semicolon-separated) - produces a log row. A column not
#' recognized in the reference table at all isn't a conflict either
#' (it's just not a deprecated header \code{batz} knows about) and also
#' produces no log row.
#'
#' \strong{Judgment call, flagged: a same-target collision between two
#' DIFFERENT old headers is treated as a conflict too, beyond Josh's
#' original spec.} If two of \code{data}'s columns would each resolve to
#' the same new name (e.g. two legacy spellings of what the reference
#' table calls the same field, both present at once in one file - or an
#' old header whose confirmed replacement happens to collide with another
#' column already spelled that way), renaming both would silently merge
#' or overwrite a column. This is caught after the initial rename pass:
#' both offending columns are reverted to their original spelling,
#' warned, and logged with \code{header.new} = the colliding target name.
#' This case isn't in Josh's original spec (which only describes one old
#' header matching more than one new name) but seemed like the same kind
#' of problem under a different shape - please confirm this extra check
#' is wanted, or whether a same-target collision should instead just be
#' allowed to rename both (accepting the resulting duplicate column
#' names) or silently rename only the first.
#'
#' \strong{Matching is tolerant, resolution is not.} Both \code{data}'s
#' own column names and the reference table's \code{$header.old} column
#' are run through \code{standardize.headers()} before comparing, so
#' \code{"Mic_Serial_Number"}, \code{"mic.serial.number"}, and
#' \code{"mic_serial_number"} are all recognized as the same old header
#' regardless of which separator/case style actually shows up in a given
#' file. Ambiguity is then judged on that same standardized key: if the
#' reference table maps that key to more than one distinct non-blank
#' \code{$header.new}, it's a real conflict (see
#' \code{\link{batz.generate_headers.acceptold}}'s own \code{@details} for
#' the 8 rows currently flagged this way) and nothing is guessed.
#'
#' @seealso \code{\link{batz.generate_headers.acceptold}}, whose reference
#'   table this function consults.
#'
#' @examples
#' \dontrun{
#' # clean, unambiguous rename (no conflict, so log.file=TRUE logs nothing)
#' d <- data.frame(ARU.serial = "SM4-01", Alldect = 12)
#' d2 <- batz.datawrangler_headers.acceptold(d, function.name = "batz.example")
#' names(d2)  # "aru.serial" "all.dectections"
#'
#' # a header flagged pending review in the reference table
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
                                                 dir.load = getwd(),
                                                 file.name = "batz_headers_acceptold.csv") {

  if (missing(function.name) || is.null(function.name) || !nzchar(function.name)) {
    stop("batz.datawrangler_headers.acceptold(): 'function.name' is required - ",
         "pass the name of the batz function calling this (e.g. ",
         "\"batz.merge_vetted.acoustics\"), so any logged conflict can be traced ",
         "back to where it occurred.")
  }

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
        header.old = nm.orig[h], header.new = dt, stringsAsFactors = FALSE,
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
                 header.new = character(0), stringsAsFactors = FALSE,
                 check.names = FALSE)
    }
    attr(data, "header.conflict.log") <- header.conflict.log
  }

  data
}

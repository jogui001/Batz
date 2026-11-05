#' Read the reference table of old (deprecated) headers
#'
#' Reads the package's header-rename reference table: each old column-name
#' spelling (\code{$header.old}), its current name (\code{$header.new}), and
#' which \code{batz} functions accept it as input (\code{$functions.in}) or
#' produced it as output (\code{$functions.out}).
#' \code{\link{batz.datawrangler_headers.acceptold}} uses this table to
#' rename old headers when data are loaded.
#'
#' @param dir.load Character, default \code{getwd()}. Folder that holds
#'   \code{file.name}.
#' @param file.name Character, default \code{"batz_headers_acceptold.csv"}.
#'   Name of the reference CSV.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols in the table are simplified to a
#'   plain equivalent and other non-ASCII characters are removed (see
#'   Details).
#'
#' @return A data frame with four character columns, in this order:
#'   \code{$header.old} (old header, original spelling), \code{$header.new}
#'   (current name; blank when no replacement has been confirmed),
#'   \code{$functions.in} and \code{$functions.out} (semicolon-separated
#'   function names; blank if none). Other columns in the CSV are dropped.
#'
#' @details
#' \itemize{
#'   \item The CSV is read as UTF-8; if that fails it is re-read as Latin-1,
#'     so a stray Excel-saved byte (e.g. a degree sign) does not stop it
#'     loading. All columns are read as character.
#'   \item With \code{strip.special = TRUE} the table is cleaned straight
#'     after reading (headers and values): accented letters and common
#'     symbols are simplified (\code{café} to \code{cafe}, \code{×} to
#'     \code{X}, curly quotes to straight quotes);
#'     characters with no plain equivalent (e.g. \code{°}, \code{µ},
#'     \code{™}) are removed. No log is written.
#'   \item The function stops with an error if the file is missing or if any
#'     of the four required columns is missing.
#'   \item \code{NA} values become \code{""} and surrounding white space is
#'     trimmed.
#'   \item A blank \code{$header.new} means "known old header, no confirmed
#'     replacement". \code{\link{batz.datawrangler_headers.acceptold}} never
#'     renames to a blank name; it leaves the column as is and warns.
#' }
#'
#' @seealso \code{\link{batz.datawrangler_headers.acceptold}}
#'
#' @examples
#' \dontrun{
#' headers.table <- batz.generate_headers.acceptold()
#' head(headers.table)
#'
#' # old headers with no confirmed replacement
#' subset(headers.table, header.new == "")
#' }
#'
#' @export
batz.generate_headers.acceptold <- function(dir.load = getwd(),
                                             file.name = "batz_headers_acceptold.csv",
                                             strip.special = TRUE) {

  path <- file.path(dir.load, file.name)
  if (!file.exists(path)) {
    stop("batz.generate_headers.acceptold(): reference file not found at: ", path,
         " - expected the package's own batz_headers_acceptold.csv (ships alongside the R/ source, see dir.load/file.name).")
  }

  tbl <- special.read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                          colClasses = "character")
  s <- special.strip(tbl, strip.special); tbl <- s$df

  required <- c("header.old", "header.new", "functions.in", "functions.out")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("batz.generate_headers.acceptold(): reference file at ", path,
         " is missing these column(s): ", paste(missing, collapse = ", "))
  }

  tbl <- tbl[, required, drop = FALSE]

  ## defensive: NA/whitespace-only in any column collapses to a clean "" -
  ## a blank $header.new means "flagged, pending review" (see @details);
  ## blank $functions.in/$functions.out just mean "not accepted/produced
  ## as that role anywhere".
  for (col in required) {
    tbl[[col]] <- ifelse(is.na(tbl[[col]]), "", trimws(tbl[[col]]))
  }

  tbl
}

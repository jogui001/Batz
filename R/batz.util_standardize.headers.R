#' Standardize a vector of header/column names (internal helper)
#'
#' Trims leading/trailing whitespace, collapses every run of one or more
#' non-alphanumeric characters (spaces, punctuation, symbols) into a single
#' underscore, then lowercases the result - i.e. converts to snake_case.
#' Used by every \code{batz} function that loads a file or accepts an
#' externally-supplied data frame, to standardize incoming headers before
#' any matching/selecting/renaming happens (per project preference, added
#' 2026-09-14 - see \code{preferences.md}, "Header standardization").
#'
#' @param x Character vector (or coercible) of header/column names to
#'   standardize.
#'
#' @return A character vector the same length as \code{x}, in snake_case.
#'
#' @details
#' Not exported - this is a package-internal utility shared across
#' \code{batz} functions. All functions in a package's \code{R/} folder are
#' loaded together, so any function in this package can call
#' \code{standardize.headers()} directly without qualification, the same
#' way several \code{batz} functions already call each other (e.g.
#' \code{batz.merge_vetted.acoustics2()} calls
#' \code{\link{batz.datawrangler_rename}} and
#' \code{\link{batz.batusa_recode.names}}).
#'
#' \strong{Substitutes} (never deletes) each non-alphanumeric run with a
#' single underscore, so \code{"Project Code"} becomes
#' \code{"project_code"} (the word boundary is preserved as an underscore),
#' not \code{"projectcode"} - deleting instead of substituting would glue
#' adjacent words together and defeat the point of "snake_case".
#'
#' @examples
#' \dontrun{
#' standardize.headers(c("Project Code", "  Date of Deployment ", "X", "MIC0 TYPE"))
#' # -> "project_code"  "date_of_deployment"  "x"  "mic0_type"
#' }
#'
#' @keywords internal
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

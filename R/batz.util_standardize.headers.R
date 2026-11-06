#' Standardize a vector of header names (internal helper)
#'
#' Converts header names to snake_case so incoming headers can be matched
#' regardless of spacing, punctuation or case. Used by \code{batz} functions
#' that load files or accept outside data frames, before any matching,
#' selecting or renaming.
#'
#' @param x Character vector (or something coercible to one) of header names.
#'
#' @details
#' Steps, in order:
#' \itemize{
#'   \item trim leading and trailing whitespace;
#'   \item replace every run of non-alphanumeric characters (spaces,
#'     punctuation, symbols, including \code{"."}) with one underscore;
#'   \item drop a leading or trailing underscore;
#'   \item lower-case the result.
#' }
#' Word boundaries are kept as underscores, so \code{"Project Code"} becomes
#' \code{"project_code"}, not \code{"projectcode"}. Not exported.
#'
#' @return A character vector the same length as \code{x}, in snake_case.
#'
#' @examples
#' \dontrun{
#' standardize.headers(c("Project Code", "  Date of Deployment ", "X", "MIC0 TYPE"))
#' # -> "project_code"  "date_of_deployment"  "x"  "mic0_type"
#' }
#'
#' @seealso \code{\link{canonicalize.headers}}, \code{\link{snake.headers}}
#' @keywords internal
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

#' Canonicalize a data frame's headers against a required-header list
#' (internal helper)
#'
#' Matches the columns of \code{df} to a list of required header names while
#' treating \code{"."}, \code{"_"}, spaces and case as equivalent, renames each
#' matched column to its \code{required} spelling, and reports what is missing.
#'
#' @param df A data frame.
#' @param required Character vector of header names, in the calling
#'   function's own spelling (e.g. dot-separated), that \code{df} should have.
#'
#' @details
#' Both \code{names(df)} and \code{required} are passed through
#' \code{\link{standardize.headers}} only to match them; the standardized form
#' is never used as a column name. A column that matches is renamed to the
#' \code{required} spelling (e.g. \code{"date_mon"} becomes \code{"date.mon"}),
#' so the calling function can use its usual names. Columns not named in
#' \code{required} are left unchanged. Only the local copy of \code{df} is
#' changed.
#'
#' \strong{Corrected spellings.} Headers whose spelling was corrected
#' (\code{schedual1} to \code{schedule1}, \code{schedual2} to \code{schedule2})
#' are matched under either spelling: a column with the old spelling counts as
#' the corrected header when the corrected one is not already present.
#'
#' Used by the plot, plot-frame and summary functions, e.g.
#' \code{\link{batz.generate_plotframe.bat}},
#' \code{\link{batz.plotactivity_observations}},
#' \code{\link{batz.plotdetections_first.last}} and
#' \code{\link{batz.generate_suntimes.arulist}}.
#'
#' @return A list with \code{$df} (\code{df} with every matched column renamed
#'   to its \code{required} spelling) and \code{$missing} (the entries of
#'   \code{required}, in their own spelling, with no match in \code{names(df)}).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(date_mon = "1", spp_id = "epfu", obs = 1)
#' canonicalize.headers(df, c("date.mon", "spp.id", "obs", "vetting.type"))
#' # $df has columns date.mon, spp.id, obs
#' # $missing is "vetting.type"
#' }
#'
#' @keywords internal
canonicalize.headers <- function(df, required) {
  std.have <- standardize.headers(names(df))
  ## 2026-10-28: an old (misspelled) header stands in for its corrected one
  fix <- headers.corrected()
  for (k in seq_len(nrow(fix))) {
    std.new <- standardize.headers(fix$header.new[k])
    j <- which(std.have == standardize.headers(fix$header.old[k]))
    if (length(j) > 0 && !(std.new %in% std.have)) std.have[j[1]] <- std.new
  }
  std.want <- standardize.headers(required)
  idx <- match(std.want, std.have)
  found <- !is.na(idx)
  names(df)[idx[found]] <- required[found]
  list(df = df, missing = required[!found])
}

#' Headers whose spelling was corrected (internal)
#'
#' Returns a data frame with \code{$header.old} (old spelling, still accepted
#' by \code{canonicalize.headers()}) and \code{$header.new} (corrected
#' spelling): \code{schedual1}/\code{schedule1} and
#' \code{schedual2}/\code{schedule2}.
#' @keywords internal
#' @noRd
headers.corrected <- function() batz.db("headers.corrected")   # table: Batz_reference_db, tab DB_headers.corrected

#' Convert Batz (dot-style) header names to snake_case (internal helper)
#'
#' Replaces every \code{"."} in a header name with \code{"_"}. Nothing else
#' changes: case, existing underscores and other characters are kept. This is
#' how \code{$name.snake} in the Batz reference tables (tab
#' \code{Header.names}) is derived from \code{$name.standard}, and it is used
#' for the \code{snake_case = TRUE} output option of \code{batz} functions.
#' To match raw incoming headers use \code{\link{standardize.headers}}
#' instead (it also lower-cases and replaces spaces and punctuation).
#'
#' @param x Character vector of header names.
#' @return \code{x} with every \code{"."} replaced by \code{"_"}.
#' @examples
#' \dontrun{
#' snake.headers(c("aru.name", "date.monitoringnight", "GlobalID", "microphone.orientation_vertical"))
#' # -> "aru_name" "date_monitoringnight" "GlobalID" "microphone_orientation_vertical"
#' }
#' @keywords internal
snake.headers <- function(x) {
  gsub(".", "_", as.character(x), fixed = TRUE)
}

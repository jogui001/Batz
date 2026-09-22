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

#' Canonicalize a data frame's headers against a required-header list
#' (internal helper)
#'
#' \strong{Added 2026-09-21, per Josh, to fix a real header-matching bug:}
#' several \code{batz} functions (\code{\link{batz.generate_plotframe.bat}},
#' \code{\link{batz.plotdetections_first.last}},
#' \code{\link{batz.plotactivity_observations}}) deliberately do NOT run
#' \code{standardize.headers()} on their \code{data}/\code{fig.list}/
#' \code{suntimes}/\code{aes.default} inputs, because those inputs are
#' expected to already be an upstream \code{batz} function's own
#' already-established (dot-separated) output schema, not raw file
#' headers - see each function's own \code{@details}, "Header
#' standardization". In practice, though, whatever a caller actually hands
#' one of these functions may have passed back through a loading/reload
#' step somewhere in between (a save-to-CSV-and-reload, for instance) that
#' DOES run \code{standardize.headers()} on its own raw headers - at which
#' point a column the function expects to find as \code{"date.mon"} might
#' actually arrive as \code{"date_mon"}, and a literal \code{setdiff()}
#' against the function's own dot-separated required-header list reports
#' it (and everything else that differs only in separator style) as
#' entirely missing, even though the data is really all there.
#'
#' \code{canonicalize.headers()} fixes this by standardizing BOTH sides
#' (the incoming data frame's own headers, and the \code{required} list
#' this function is looking for) to the same snake_case key purely to
#' MATCH columns up - never to decide what anything is actually called
#' afterward. Every column of \code{df} that matches one of \code{required}
#' (after standardizing both) is renamed, in the copy returned here only,
#' to that item's own \code{required} spelling (e.g. always
#' \code{"date.mon"}, never \code{"date_mon"}, regardless of which style
#' the column arrived in) - so the rest of the calling function keeps
#' working exactly as before, referencing its usual dot-separated column
#' names, and the function's return value/on-disk output keeps its usual
#' naming convention completely unaffected by this. This never touches the
#' caller's own object: \code{df} is this call's own local copy (R's
#' ordinary copy-on-modify semantics for a function argument), and nothing
#' in this helper writes anything back to the caller's environment. A
#' column of \code{df} that isn't named in \code{required} is left exactly
#' as it was, untouched.
#'
#' @param df A data frame to canonicalize.
#' @param required Character vector of header names, in the calling
#'   function's own canonical style (e.g. dot-separated), that \code{df}
#'   is expected to have.
#'
#' @return A list with \code{df} (a copy of \code{df} with every matched
#'   column renamed to its own \code{required} spelling) and \code{missing}
#'   (the subset of \code{required}, in \code{required}'s own original
#'   spelling, with no match at all in \code{names(df)} - ready to print in
#'   an error message).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(date_mon = "1", spp_id = "epfu", obs = 1)
#' canonicalize.headers(df, c("date.mon", "spp.id", "obs", "vetting.type"))
#' # $df has columns date.mon/spp.id/obs (renamed from date_mon/spp_id/obs)
#' # $missing is "vetting.type" (genuinely absent either way)
#' }
#'
#' @keywords internal
canonicalize.headers <- function(df, required) {
  std.have <- standardize.headers(names(df))
  std.want <- standardize.headers(required)
  idx <- match(std.want, std.have)
  found <- !is.na(idx)
  names(df)[idx[found]] <- required[found]
  list(df = df, missing = required[!found])
}

#' survey123.forms() - Survey123 ARU form headers and choices (internal)
#'
#' Returns a list of data frames, read from the package's copy of
#' Batz_reference_db (tabs DB_s123.forms.quad, DB_s123.forms.habitat,
#' DB_s123.forms.service, DB_s123.forms.Choices), used by
#' \code{\link{batz.merge_sitevisit.acoustics}} to translate Survey123 ARU form
#' exports to Batz headers. One data frame per tab of
#' \code{Survey123_aruforms all headers.xlsx}:
#' \describe{
#'   \item{\code{$quad}, \code{$habitat}, \code{$service}}{\code{aru.<tab>_batz.headers}
#'     (the tab plus the header, e.g. \code{aru.quad$aru.name}; some tabs share a
#'     header that functions such as \code{batz.generate_aru.appendix} use at
#'     different times), \code{batz.headers} (the header used in the Batz
#'     package), then one column per form version (\code{forest_2026},
#'     \code{inland_2026}, \code{service_2026}) holding that version's
#'     Survey123 export header.}
#'   \item{\code{$Choices}}{\code{list_name}, \code{name} (the code
#'     Survey123 stores) and \code{label} (the display text).}
#' }
#' Batz headers are lower case with "." for spaces (e.g. \code{globalid},
#' \code{vegetation.tree_tree1.dbh}, \code{vegetation.sapling_species},
#' \code{equipment.replace}, \code{action.performed_other},
#' \code{measurement.units}, \code{microphone.calibration_value}). Survey123
#' export headers are kept exactly as the forms write them, including their
#' spelling ("Unit of measurment", "Mic callibration value"). Names are
#' trimmed; blank rows and repeated \code{list_name}/\code{name} pairs are
#' not included.
#'
#' @keywords internal
#' @noRd
survey123.forms <- function() {
  list(quad = batz.db("s123.forms.quad"), habitat = batz.db("s123.forms.habitat"),
       service = batz.db("s123.forms.service"), Choices = batz.db("s123.forms.Choices"))
}

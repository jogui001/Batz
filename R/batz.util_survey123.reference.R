#' Survey123 reference tables (internal)
#'
#' Header and choice tables for the Survey123 ARU forms, used by
#' \code{\link{batz.generate_aru.appendix}} (and \code{survey123.ref.renamed()}
#' also by \code{\link{batz.merge_sitevisit.acoustics}}). The master copy is
#' the \code{survey123.headers} / \code{survey123.choices} tabs of the Batz
#' reference tables workbook.
#' \describe{
#'   \item{\code{survey123.ref.headers()}}{tabs \code{quad}, \code{deploy},
#'     \code{service}: \code{$tab}, \code{$batz.header} (the Batz column
#'     name, lower case with "." for spaces, e.g. \code{globalid}) and
#'     \code{$survey123.header} (the Survey123 export header, as the form
#'     writes it).}
#'   \item{\code{survey123.ref.choices()}}{tab \code{Choices}:
#'     \code{$list.name}, \code{$name} (the stored code) and \code{$label}
#'     (what the form shows); no duplicate \code{list.name}/\code{name} rows.}
#'   \item{\code{survey123.ref.renamed()}}{Batz headers whose spelling was
#'     corrected: \code{$header.old} (misspelled header, still accepted on
#'     load, e.g. \code{measurment.units}) and \code{$header.new} (the
#'     header it is read as, e.g. \code{measurement.units}).}
#' }
#'
#' @keywords internal
#' @noRd
survey123.ref.headers <- function() batz.db("survey123.ref.headers")   # table: Batz_reference_db, tab DB_survey123.ref.headers

#' @keywords internal
#' @noRd
survey123.ref.choices <- function() batz.db("survey123.ref.choices")   # table: Batz_reference_db, tab DB_survey123.ref.choices

## 2026-10-28, per Josh: corrected Batz header spellings (old -> new); the
## old spellings are still accepted when a form is loaded
survey123.ref.renamed <- function() batz.db("survey123.ref.renamed")   # table: Batz_reference_db, tab DB_survey123.ref.renamed


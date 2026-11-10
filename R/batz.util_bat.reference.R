#' Bat reference tables (internal)
#'
#' Shared bat reference tables used by the functions that work with bat names
#' (\code{\link{batz.batusa_recode.names}}, \code{\link{batz.batusa_list.species}}).
#' The master copy is the Batz reference-tables workbook; update these tables
#' from it.
#'
#' @details
#' \describe{
#'   \item{\code{bat.ref.names()}}{data frame, tab \code{bat.names}: 54 species
#'     plus 8 category labels (\code{All detections}, \code{HiF}, \code{Social}, ...).
#'     Columns include \code{$region}, \code{$scientific_name}, \code{$common_name},
#'     \code{$label_name}, \code{$label2_name},
#'     \code{$code4}, \code{$code6}, \code{$family} (blank for category labels),
#'     listing-status and state columns, \code{$hibernation.strategy},
#'     \code{$phonic.group} and \code{$notes}.}
#'   \item{\code{bat.ref.groups()}}{data frame, tab \code{BAT Groupings}: species
#'     pairs and genus-level groups (e.g. \code{LUSO} = little brown bat / Indiana
#'     bat, \code{MYSP} = unknown Myotis).}
#'   \item{\code{bat.ref.freqclasses()}}{data frame, tab \code{NABATFrequency Classes}:
#'     frequency classes and other call labels (\code{HiF}, \code{40kMyo},
#'     \code{NoID}, ...). \code{$label} holds the spellings found in vetting files.}
#'   \item{\code{aru.ref.types()}}{list with \code{$header} (input-file headers that
#'     may name the recorder model) and \code{$recorder.type} (recognized recorder
#'     models).}
#'   \item{\code{bat.ref.split.regions(x)}}{splits region cells on \code{;} or
#'     \code{,}, drops quotes, trims and lower-cases; returns a character vector.}
#'   \item{\code{bat.ref.filter.regions(df, regions = c("ALL", "NABAT"))}}{keeps the
#'     rows of \code{df} whose \code{$region} matches any of \code{regions}. Rows
#'     marked \code{All} are always kept. A cell may list several regions; the row
#'     is kept if any of them matches. Case and quotes are ignored.}
#' }
#' After editing a table, check that every column has \code{nrow()} values.
#' @keywords internal
#' @noRd
bat.ref.names <- function() batz.db("bat.ref.names")                 # table: Batz_reference_db, tab DB_bat.ref.names

#' @keywords internal
#' @noRd
bat.ref.groups <- function() batz.db("bat.ref.groups")                 # table: Batz_reference_db, tab DB_bat.ref.groups

#' @keywords internal
#' @noRd
bat.ref.freqclasses <- function() batz.db("bat.ref.freqclasses")                 # table: Batz_reference_db, tab DB_bat.ref.freqclasses

#' @keywords internal
#' @noRd
aru.ref.types <- function() {
  d <- batz.db("aru.ref.types")                     # table: Batz_reference_db, tab DB_aru.ref.types
  split(d$value, factor(d$list, levels = unique(d$list)))
}

#' @keywords internal
#' @noRd
bat.ref.split.regions <- function(x) {
  x <- unlist(strsplit(as.character(x), "[;,]"))
  x <- tolower(trimws(gsub("[\"'\u201c\u201d]", "", x)))
  x[!is.na(x) & nzchar(x)]
}

#' @keywords internal
#' @noRd
bat.ref.filter.regions <- function(df, regions = c("ALL", "NABAT")) {
  want <- unique(c(bat.ref.split.regions(regions), "all"))
  keep <- vapply(df$region, function(r) any(bat.ref.split.regions(r) %in% want), logical(1))
  df[keep, , drop = FALSE]
}


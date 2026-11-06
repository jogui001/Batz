#' Copy the reference tables from Batz_reference_db into the package
#'
#' \verb{Batz_reference_db_<YYYYMMDD>_v<N>.xlsx} is the one master copy of
#' every table the Batz functions use: plot settings (plotopts), bat names
#' and groups, Survey123 form headers and choices, old-header lists, etc.
#' This function copies those tables into the package source as
#' \verb{R/sysdata.rda}, so the installed package carries them and works on
#' any computer. Run it after editing a table in the workbook, then commit,
#' push and reinstall.
#'
#' @param refdb Character. The workbook, or the folder that holds it
#'   (default: the "8reference tables" folder used for Batz). For a folder,
#'   the newest \verb{Batz_reference_db_*.xlsx} is used (latest date, then
#'   highest version number); Excel lock files (\verb{~$...}) are skipped.
#' @param dir.repo Character, default \code{getwd()}. The Batz package
#'   source folder (the one holding \verb{DESCRIPTION}); \verb{R/sysdata.rda}
#'   is written there.
#'
#' @return Invisibly, a list: \code{$file} (the workbook read),
#'   \code{$tables} (data frame: \code{$name}, \code{$tab}, \code{$rows},
#'   \code{$columns}) and \code{$sysdata} (the file written).
#'
#' @details
#' The workbook's \code{database.names} tab lists the tables to copy: one
#' row per table with \code{$name.standard} (the name the functions use),
#' \code{$tab} (the workbook tab holding it), \code{$data.type},
#' \code{$functions.in} and \code{$description}. Each tab is read as text
#' (every column character, blanks as \code{""}), with its first row as the
#' headers. A listed tab that is missing stops the function. To add a table,
#' add its tab and a \code{database.names} row.
#'
#' Inside the package the tables are read with \code{batz.db(<name>)}
#' (internal).
#'
#' @seealso \code{\link{batz.generate_plotopts}}
#'
#' @examples
#' \dontrun{
#' batz.util_refdb.sync(
#'   refdb = "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/8reference tables",
#'   dir.repo = "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz")
#' }
#' @export
batz.util_refdb.sync <- function(refdb = "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/8reference tables",
                                 dir.repo = getwd()) {
  FN <- "batz.util_refdb.sync"
  file <- batz.util_refdb.newest(refdb, FN)
  if (!file.exists(file.path(dir.repo, "DESCRIPTION")))
    stop(FN, ": dir.repo has no DESCRIPTION file - give the Batz package source folder: ", dir.repo, call. = FALSE)
  sheets <- readxl::excel_sheets(file)
  if (!"database.names" %in% sheets) stop(FN, ": ", basename(file), " has no 'database.names' tab.", call. = FALSE)
  reg <- as.data.frame(readxl::read_excel(file, "database.names", col_types = "text"))
  reg <- reg[!is.na(reg$name.standard) & nzchar(reg$name.standard), , drop = FALSE]
  miss <- reg$tab[!reg$tab %in% sheets]
  if (length(miss)) stop(FN, ": tab(s) listed in database.names are not in ", basename(file), ": ",
                         paste(miss, collapse = ", "), call. = FALSE)
  db <- list()
  for (i in seq_len(nrow(reg))) {
    d <- as.data.frame(readxl::read_excel(file, reg$tab[i], col_types = "text", .name_repair = "minimal"),
                       stringsAsFactors = FALSE)
    for (cl in names(d)) { v <- d[[cl]]; v[is.na(v)] <- ""; d[[cl]] <- v }
    d <- d[rowSums(d != "") > 0, , drop = FALSE]                 # drop empty rows
    rownames(d) <- NULL
    db[[reg$name.standard[i]]] <- d
  }
  attr(db, "source") <- basename(file)
  .batz.db <- db
  out <- file.path(dir.repo, "R", "sysdata.rda")
  save(.batz.db, file = out, compress = "xz")
  tabs <- data.frame(name = reg$name.standard, tab = reg$tab,
                     rows = vapply(db, nrow, integer(1)), columns = vapply(db, ncol, integer(1)),
                     row.names = NULL, stringsAsFactors = FALSE)
  print(tabs, row.names = FALSE)
  cat(sprintf("Saved: %s (%d tables from %s)\nNext: commit and push, then reinstall Batz.\n", out, nrow(tabs), basename(file)))
  invisible(list(file = file, tables = tabs, sysdata = out))
}


#' Newest Batz_reference_db workbook in a folder (internal)
#' @keywords internal
#' @noRd
batz.util_refdb.newest <- function(refdb, fn = "batz") {
  if (length(refdb) != 1 || is.na(refdb) || !nzchar(refdb)) stop(fn, ": refdb is blank.", call. = FALSE)
  if (dir.exists(refdb)) {
    f <- list.files(refdb, pattern = "^Batz_reference_db_.*\\.xlsx$", full.names = TRUE)
    if (!length(f)) stop(fn, ": no Batz_reference_db_*.xlsx in ", refdb, call. = FALSE)
    b <- basename(f)
    dt <- suppressWarnings(as.numeric(sub("^Batz_reference_db_(\\d{8}).*$", "\\1", b)))
    vn <- suppressWarnings(as.numeric(sub("^.*_v(\\d+)\\.xlsx$", "\\1", b)))
    vn[is.na(vn)] <- 0; dt[is.na(dt)] <- 0
    return(f[order(-dt, -vn)][1])
  }
  if (!file.exists(refdb)) stop(fn, ": refdb not found: ", refdb, call. = FALSE)
  refdb
}


#' A reference table carried by the package (internal)
#'
#' Returns one table copied from Batz_reference_db by
#' \code{\link{batz.util_refdb.sync}} (\verb{R/sysdata.rda}).
#' @param name The table's \code{$name.standard} in the workbook's
#'   \code{database.names} tab.
#' @keywords internal
#' @noRd
batz.db <- function(name) {
  db <- get0(".batz.db", envir = asNamespace("Batz"), inherits = FALSE)
  if (is.null(db)) stop("Batz has no reference tables (R/sysdata.rda) - run batz.util_refdb.sync() and reinstall.", call. = FALSE)
  d <- db[[name]]
  if (is.null(d)) stop("Batz reference table '", name, "' is not in R/sysdata.rda - add it to Batz_reference_db ",
                       "(a tab plus a database.names row), run batz.util_refdb.sync() and reinstall.", call. = FALSE)
  d
}

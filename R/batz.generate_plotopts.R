#' Generate project settings ("plotopts") files for Batz plot functions
#'
#' Writes the built-in plot settings of each requested plot function to
#' project files: one CSV per function, named after the function's settings
#' tab in Batz_reference_db,
#' \verb{<project.name>_pltop.<function name without batz.plot.>.csv}
#' (e.g. \verb{acme_pltop.heatmap_aru.time.csv}), and/or one pooled Excel
#' workbook, \verb{<project.name>_pltop.pooled.xlsx}, with one
#' \verb{pltop.<function name without batz.plot.>} tab per function.
#' The \code{$override.value} column is named after the project
#' (\code{project.name}). Edit that column of the copy to customise a
#' project's plots, then pass the file as the function's
#' \code{aes.plotopts} with \code{aes.style = <project.name>} (the column
#' read before \code{$default.value}). Every plot function takes
#' \code{aes.plotopts} as a data frame, the path to its CSV, or the path to
#' the pooled workbook (it reads its own \verb{pltop.*} tab).
#'
#' @param functions Character, default \code{""} (or \code{NULL}): every
#'   Batz plot function (\code{batz.plot.*}). Plot functions to write
#'   settings for. \code{"batz."} or \code{"batz.plot."} may be left out
#'   (e.g. \code{"plot.bar_activity.site"} or \code{"bar_activity.site"});
#'   case and spaces are ignored, and a name that is part of exactly one plot
#'   function name is read as that function (NOTE).
#' @param project.name Character, default \code{"new project"}. Leading part
#'   of each output file name, and the name of the column that holds the
#'   project's own settings (written as \code{$override.value} in the
#'   built-in table). \code{""}: the column keeps the name
#'   \code{override.value} and the files are named
#'   \verb{pltop.<function name without batz.plot.>.csv} and
#'   \verb{pltop.pooled.xlsx}.
#' @param file.format Character, default \code{c("csv", "xlsx")}: which
#'   files to write. \code{"csv"}: one CSV per function. \code{"xlsx"}: one
#'   pooled workbook with a tab per function. Both: both.
#' @param dir.save Directory the settings files are written to (created if
#'   missing). Default \code{getwd()}.
#' @param save.auto Default \code{TRUE}: the files are saved in \code{dir.save}.
#'   \code{FALSE}: no files are saved. \code{"BOTH"}: saved and printed.
#'   \code{"print"}: nothing is saved; each
#'   output (plot or table) is printed instead. \code{"generate <number>"}
#'   (e.g. \code{"generate 5"}): nothing is saved; up to \code{<number>}
#'   outputs are created as objects in the R global environment, each named
#'   after the file it would have been saved as, without the extension and the
#'   time stamp (e.g. \code{acme_vegetation.dbh}); \code{"generate"} alone
#'   creates every output.
#' @param strip.special.plotopts Logical, default \code{FALSE}. \code{FALSE}
#'   keeps every character (e.g. an axis label with a degree sign).
#'   \code{TRUE} simplifies accented letters (e.g. e-acute to \code{"e"}) and
#'   removes other non-ASCII characters in the copy.
#'
#' @details
#' \strong{Source.} The settings come from Batz_reference_db, the one master
#' copy of the Batz tables: its \code{pltop.*} tabs, listed in the
#' \code{database.names} tab as \code{plotopts.<function name>}. The package
#' carries a copy of them (\code{\link{batz.util_refdb.sync}}), so the file
#' written matches the installed version. They are the same settings each
#' plot function uses when no \code{aes.plotopts} is given.
#'
#' \strong{Copy.} Every column is written as text, as stored; no settings
#' are added, removed or computed. Only the \code{$override.value} header is
#' renamed to \code{project.name} (kept when \code{project.name = ""}). With \code{strip.special.plotopts = TRUE},
#' special characters are simplified or removed first. A name in
#' \code{functions} that matches no plot function, matches several, or
#' has no built-in settings stops the function with an error that lists
#' the recognised names.
#'
#' \strong{Pooled workbook.} One tab per function, in the order of
#' \code{functions}, named as the Batz_reference_db tab (at most 31
#' characters). Bold header row, frozen; column widths fitted. The cells are
#' the same text as in the CSVs.
#'
#' @return Invisibly, a character vector of the file names written (names
#'   only, not full paths): the CSVs in the order of \code{functions},
#'   then the pooled workbook. Each saved file name is printed.
#'
#' @examples
#' \dontrun{
#' # Settings files for every plot function, into a project sub-folder
#' batz.generate_plotopts(project.name = "acme.wetlands.2026", dir.save = "acme_project")
#' # then, e.g.:
#' # batz.plot.heatmap_aru.time(data, fig.list,
#' #   aes.plotopts = read.csv("acme_project/acme.wetlands.2026_pltop.heatmap_aru.time.csv",
#' #                           check.names = FALSE),
#' #   aes.style = "acme.wetlands.2026")
#'
#' # Pooled workbook only, then any plot function reads its own tab
#' batz.generate_plotopts(project.name = "acme.wetlands.2026", file.format = "xlsx",
#'                        dir.save = "acme_project")
#' # batz.plot.heatmap_aru.time(data, fig.list,
#' #   aes.plotopts = "acme_project/acme.wetlands.2026_pltop.pooled.xlsx",
#' #   aes.style = "acme.wetlands.2026")
#'
#' # One function only
#' batz.generate_plotopts(functions = "bullseye_clutter.mic",
#'                        project.name = "acme.wetlands.2026")
#' }
#'
#' @seealso \code{\link{batz.util_refdb.sync}}, \code{\link{batz.generate_figlist}}
#'
#' @export
batz.generate_plotopts <- function(functions = "",
                                   project.name = "new project",
                                   file.format = c("csv", "xlsx"),
                                   dir.save = getwd(), save.auto = TRUE,
                                   strip.special.plotopts = FALSE, ...) {
  sv <- util_saver(save.auto, "batz.generate_plotopts")   # TRUE save / FALSE / "print" / "generate <n>"
  FN <- "batz.generate_plotopts"
  dots <- list(...)
  if ("generate.files" %in% names(dots)) {                 # old name of functions
    cat("WARNING: ", FN, " - argument 'generate.files' has been renamed functions",
        if (missing(functions)) " - used as functions" else " - ignored, functions used", ".\n", sep = "")
    if (missing(functions)) functions <- dots$generate.files
    dots$generate.files <- NULL
  }
  if (length(dots)) stop(FN, ": unknown argument(s): ", paste(names(dots), collapse = ", "), call. = FALSE)
  db <- get0(".batz.db", envir = asNamespace("Batz"), inherits = FALSE)
  known <- sub("^plotopts\\.", "", grep("^plotopts\\.", names(db), value = TRUE))
  functions <- plotutil_match_functions(functions, plotutil_all_plots(), FN)
  unknown <- setdiff(functions, known)
  if (length(unknown) > 0)
    stop(FN, ": no built-in settings for: ", paste(unknown, collapse = ", "),
         ". Plot functions with built-in settings: ", paste(known, collapse = ", "), ".", call. = FALSE)
  ff <- unique(tolower(trimws(as.character(file.format))))
  bad.ff <- setdiff(ff, c("csv", "xlsx"))
  if (!length(ff) || length(bad.ff))
    stop(FN, ": file.format must be \"csv\", \"xlsx\" or both - not: ", paste(bad.ff, collapse = ", "), call. = FALSE)
  if (sv$save && !dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
  pj <- if (is.null(project.name) || !length(project.name) || is.na(project.name[1])) "" else trimws(as.character(project.name[1]))
  pj.col <- if (nzchar(pj)) pj else NULL

  saved <- character(0)
  pooled <- list()
  for (fn.name in functions) {
    opts <- batz.db(paste0("plotopts.", fn.name))
    s <- special.strip(opts, strip.special.plotopts); opts <- s$df
    for (h in names(opts)) opts[[h]] <- as.character(opts[[h]])
    ## $override.value is named after the project (2026-11-10); blank project.name keeps override.value
    if (!is.null(pj.col) && "override.value" %in% names(opts)) names(opts)[names(opts) == "override.value"] <- pj.col
    tab.name <- paste0("pltop.", sub("^batz\\.plot\\.", "", fn.name))   # same name as the Batz_reference_db tab
    pooled[[tab.name]] <- opts
    if ("csv" %in% ff) {
      out.name <- if (nzchar(pj)) sprintf("%s_%s.csv", pj, tab.name) else sprintf("%s.csv", tab.name)
      if (!is.na(sv$out(opts, file.path(dir.save, out.name), function(.f) utils::write.csv(opts, .f, row.names = FALSE))))
        saved <- c(saved, out.name)
    }
  }
  if ("xlsx" %in% ff) {
    out.name <- if (nzchar(pj)) sprintf("%s_pltop.pooled.xlsx", pj) else "pltop.pooled.xlsx"
    wb <- openxlsx::createWorkbook()
    bold <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "bottom")
    for (tab.name in names(pooled)) {
      d <- pooled[[tab.name]]
      openxlsx::addWorksheet(wb, tab.name)
      openxlsx::writeData(wb, tab.name, d, headerStyle = bold)
      w <- vapply(names(d), function(h) min(60, max(nchar(c(h, d[[h]]), type = "width"), 8) + 2), numeric(1))
      openxlsx::setColWidths(wb, tab.name, seq_along(d), widths = w)
      openxlsx::freezePane(wb, tab.name, firstRow = TRUE)
    }
    if (!is.na(sv$out(pooled, file.path(dir.save, out.name), function(.f) openxlsx::saveWorkbook(wb, .f, overwrite = TRUE)))) {
      cat(sprintf("  %d tab(s): %s\n", length(pooled), paste(names(pooled), collapse = ", ")))
      saved <- c(saved, out.name)
    }
  }
  invisible(saved)
}

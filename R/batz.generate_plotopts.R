#' Generate a project settings ("plotopts") CSV file for Batz plot functions
#'
#' Writes the built-in plot settings of each requested plot function to a
#' new project file named \verb{<project.name>_plotopts_<function.name>.csv}.
#' The \code{$override.value} column is named after the project
#' (\code{project.name}). Edit that column of the copy to customise a
#' project's plots, then pass the file as the function's
#' \code{aes.plotopts} with \code{aes.style = <project.name>} (the column
#' read before \code{$default.value}).
#'
#' @param generate.files Character vector of plot function names. Default:
#'   every plot function with built-in settings:
#'   \code{"batz.plot_bar.bat.group"}, \code{"batz.plot_heat.site.species"},
#'   \code{"batz.plotactivity_observations"},
#'   \code{"batz.plotdetections_first.last"},
#'   \code{"batz.plotcover_bullseye"}, \code{"batz.plotactivity_daily.count"},
#'   \code{"batz.plotactivity_heatmap"}, \code{"batz.plotactivity_heatmap2"},
#'   \code{"batz.plotsm4_heatmap"}, \code{"batz.plot_vegetation.species"},
#'   \code{"batz.plot_vegetation.dbh"}, \code{"batz.plot_weather.daily"}.
#' @param project.name Character, default \code{"new project"}. Leading part
#'   of each output file name, and the name of the column that holds the
#'   project's own settings (written as \code{$override.value} in the
#'   built-in table). \code{""}: the column keeps the name
#'   \code{override.value} and the files are named
#'   \verb{plotopts_<function.name>.csv}.
#' @param dir.save Directory the settings files are written to (created if
#'   missing). Default \code{getwd()}.
#' @param strip.special.plotopts Logical, default \code{FALSE}. \code{FALSE}
#'   keeps every character (e.g. an axis label with a degree sign).
#'   \code{TRUE} simplifies accented letters (e.g. e-acute to \code{"e"}) and
#'   removes other non-ASCII characters in the copy.
#'
#' @details
#' \strong{Source.} The settings come from Batz_reference_db, the one master
#' copy of the Batz tables: its \code{PLOTOPTS_*} tabs, listed in the
#' \code{database.names} tab as \code{plotopts.<function name>}. The package
#' carries a copy of them (\code{\link{batz.util_refdb.sync}}), so the file
#' written matches the installed version. They are the same settings each
#' plot function uses when no \code{aes.plotopts} is given.
#'
#' \strong{Copy.} Every column is written as text, as stored; no settings
#' are added, removed or computed. Only the \code{$override.value} header is
#' renamed to \code{project.name} (kept when \code{project.name = ""}). With \code{strip.special.plotopts = TRUE},
#' special characters are simplified or removed first. A name in
#' \code{generate.files} with no built-in settings stops the function with
#' an error that lists the recognised names.
#'
#' @return Invisibly, a character vector of the file names written (names
#'   only, not full paths), in the order of \code{generate.files}. Each saved
#'   file name is printed.
#'
#' @examples
#' \dontrun{
#' # Settings files for every plot function, into a project sub-folder
#' batz.generate_plotopts(project.name = "acme.wetlands.2026", dir.save = "acme_project")
#' # then, e.g.:
#' # batz.plotsm4_heatmap(data, fig.list,
#' #   aes.plotopts = read.csv("acme_project/acme.wetlands.2026_plotopts_batz.plotsm4_heatmap.csv",
#' #                           check.names = FALSE),
#' #   aes.style = "acme.wetlands.2026")
#'
#' # One function only
#' batz.generate_plotopts(generate.files = "batz.plotcover_bullseye",
#'                        project.name = "acme.wetlands.2026")
#' }
#'
#' @seealso \code{\link{batz.util_refdb.sync}}, \code{\link{batz.generate_figlist}}
#'
#' @export
batz.generate_plotopts <- function(generate.files = c("batz.plot_bar.bat.group", "batz.plot_heat.site.species",
                                                      "batz.plotactivity_observations", "batz.plotdetections_first.last",
                                                      "batz.plotcover_bullseye", "batz.plotactivity_daily.count",
                                                      "batz.plotactivity_heatmap", "batz.plotactivity_heatmap2",
                                                      "batz.plotsm4_heatmap", "batz.plot_vegetation.species",
                                                      "batz.plot_vegetation.dbh", "batz.plot_weather.daily"),
                                   project.name = "new project",
                                   dir.save = getwd(),
                                   strip.special.plotopts = FALSE) {
  FN <- "batz.generate_plotopts"
  if (length(generate.files) == 0 || !is.character(generate.files))
    stop(FN, ": generate.files must be a non-empty character vector of plot function names.", call. = FALSE)
  db <- get0(".batz.db", envir = asNamespace("Batz"), inherits = FALSE)
  known <- sub("^plotopts\\.", "", grep("^plotopts\\.", names(db), value = TRUE))
  unknown <- setdiff(generate.files, known)
  if (length(unknown) > 0)
    stop(FN, ": no built-in settings for: ", paste(unknown, collapse = ", "),
         ". Plot functions with built-in settings: ", paste(known, collapse = ", "), ".", call. = FALSE)
  if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
  pj <- if (is.null(project.name) || !length(project.name) || is.na(project.name[1])) "" else trimws(as.character(project.name[1]))
  pj.col <- if (nzchar(pj)) pj else NULL

  saved <- character(0)
  for (fn.name in generate.files) {
    opts <- batz.db(paste0("plotopts.", fn.name))
    s <- special.strip(opts, strip.special.plotopts); opts <- s$df
    for (h in names(opts)) opts[[h]] <- as.character(opts[[h]])
    ## $override.value is named after the project (2026-11-10); blank project.name keeps override.value
    if (!is.null(pj.col) && "override.value" %in% names(opts)) names(opts)[names(opts) == "override.value"] <- pj.col
    out.name <- if (nzchar(pj)) sprintf("%s_plotopts_%s.csv", pj, fn.name) else sprintf("plotopts_%s.csv", fn.name)
    utils::write.csv(opts, file.path(dir.save, out.name), row.names = FALSE)
    cat("Saved:", out.name, "\n")
    saved <- c(saved, out.name)
  }
  invisible(saved)
}

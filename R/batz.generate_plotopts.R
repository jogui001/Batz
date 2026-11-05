#' Generate a project settings ("plotopts") CSV file for Batz plot functions
#'
#' Copies the master plot settings CSV of each requested plot function into a
#' new project-specific file named
#' \verb{<project.name>_plotopts_<function.name>.csv}. Edit the
#' \code{$override.value} column of the copy to customise a project's plots
#' without touching the master file.
#'
#' @param generate.files Character vector of plot function names to create a
#'   settings file for. Default
#'   \code{c("batz.plotcover_bullseye", "batz.plotdetections_first.last",
#'   "batz.plotactivity_observations")}. Every element must be one of these
#'   three names.
#' @param project.name Character, default \code{"new project"}. Leading part of
#'   each output file name.
#' @param dir.load Directory that holds the master settings CSV files. Default
#'   \code{getwd()}.
#' @param dir.save Directory the new settings files are written to (created if
#'   missing). Default \code{getwd()}.
#' @param strip.special.plotopts Logical, default \code{FALSE}. \code{FALSE}
#'   keeps every character (e.g. an axis label \code{"Temperature (degree sign C)"}).
#'   \code{TRUE} simplifies accented letters (e.g. e-acute to \code{"e"}) and
#'   removes other non-ASCII characters in the copy.
#'
#' @details
#' \strong{Master files.} Each recognised plot function maps to one master file
#' in \code{dir.load}:
#' \itemize{
#'   \item \code{batz.plotcover_bullseye}: \verb{plotopts_bullseye.csv}
#'   \item \code{batz.plotdetections_first.last}: \verb{plotopts_first.last.csv}
#'   \item \code{batz.plotactivity_observations}: \verb{plotopts_callobs.csv}
#' }
#' Any other name in \code{generate.files} stops the function with an error
#' that lists the recognised names. A missing master file also stops the
#' function.
#'
#' \strong{Copy.} The master file is read with \code{special.read.csv()} (UTF-8,
#' with a Latin-1 fallback, so a stray degree-sign byte does not stop it loading).
#' All columns are read as character, so values are written back as read
#' (only the CSV quoting may change). No settings are added, removed or
#' computed. With \code{strip.special.plotopts = TRUE}, special characters are
#' simplified or removed first. No log is written.
#'
#' @return Invisibly, a character vector of the file names written (names only,
#'   not full paths), in the order of \code{generate.files}. Writes one CSV per
#'   element of \code{generate.files} to \code{dir.save} and prints each saved
#'   file name to the console.
#'
#' @examples
#' \dontrun{
#' # Settings files for all three plot functions, into a project sub-folder
#' batz.generate_plotopts(project.name = "acme.wetlands.2026",
#'                        dir.save = "acme_project")
#'
#' # One function only
#' batz.generate_plotopts(generate.files = "batz.plotcover_bullseye",
#'                        project.name = "acme.wetlands.2026")
#' }
#'
#' @seealso \code{\link{batz.plotcover_bullseye}},
#'   \code{\link{batz.plotdetections_first.last}},
#'   \code{\link{batz.plotactivity_observations}}
#'
#' @export
batz.generate_plotopts <- function(generate.files = c("batz.plotcover_bullseye",
                                                        "batz.plotdetections_first.last",
                                                        "batz.plotactivity_observations"),
                                    project.name = "new project",
                                    dir.load = getwd(),
                                    dir.save = getwd(),
                                    strip.special.plotopts = FALSE) {

  ## Round nineteen, per Josh (2026-09-16): fixed mapping of recognized
  ## plot function name -> its master settings CSV file name. See
  ## "Recognized plot functions and their master files" in @details for
  ## why these three (and only these three) are here.
  MASTER.PLOTOPTS.FILES <- c(
    batz.plotcover_bullseye        = "plotopts_bullseye.csv",
    batz.plotdetections_first.last = "plotopts_first.last.csv",
    batz.plotactivity_observations = "plotopts_callobs.csv"
  )

  if (length(generate.files) == 0 || !is.character(generate.files)) {
    stop("batz.generate_plotopts(): generate.files must be a non-empty character vector of plot function names.")
  }

  unknown <- setdiff(generate.files, names(MASTER.PLOTOPTS.FILES))
  if (length(unknown) > 0) {
    stop("batz.generate_plotopts(): unrecognized plot function name(s) in generate.files: ",
         paste(unknown, collapse = ", "),
         ". Recognized plot functions are: ",
         paste(names(MASTER.PLOTOPTS.FILES), collapse = ", "), ".")
  }

  if (!dir.exists(dir.save)) {
    dir.create(dir.save, recursive = TRUE)
  }

  saved.files <- character(0)

  for (fn.name in generate.files) {
    master.file <- MASTER.PLOTOPTS.FILES[[fn.name]]
    master.path <- file.path(dir.load, master.file)

    if (!file.exists(master.path)) {
      stop("batz.generate_plotopts(): master plotopts file not found for '", fn.name,
           "' - expected it at: ", master.path)
    }

    ## Read the master file as-is (check.names = FALSE per this project's
    ## standing preference against silent column-name mangling - see
    ## preferences.md, "CRITICAL - check.names") and write it back out
    ## unchanged under the new project-specific name.
    opts <- special.read.csv(master.path, stringsAsFactors = FALSE, check.names = FALSE,
                             colClasses = "character")
    ## special characters (per Josh, 2026-09-30): strip, then keep every
    ## column character (as read) so the copy isn't re-typed.
    s <- special.strip(opts, strip.special.plotopts); opts <- s$df
    for (h in s$headers) opts[[h]] <- as.character(opts[[h]])

    out.name <- sprintf("%s_plotopts_%s.csv", project.name, fn.name)
    out.path <- file.path(dir.save, out.name)

    utils::write.csv(opts, out.path, row.names = FALSE)

    saved.files <- c(saved.files, out.name)
    cat("Saved:", out.name, "\n")
  }

  invisible(saved.files)
}

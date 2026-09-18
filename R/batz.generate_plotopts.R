#' Generate a settings ("plotopts") CSV file for one or more Batz plot functions
#'
#' Round nineteen, per Josh (2026-09-16). Every \code{batz} plot function
#' (\code{batz.plotcover_bullseye}, \code{batz.plotdetections_first.last},
#' \code{batz.plotactivity_observations}) reads its own drawing settings
#' from a master settings CSV (columns \code{category}, \code{parameter},
#' \code{default.value}, \code{overide.value}, \code{notes} - see each
#' function's own documentation, "Settings resolution (round nineteen)").
#' \code{batz.generate_plotopts()} is a small helper that copies that
#' master settings file, unchanged, into a new project-specific file named
#' after \code{project.name} - so starting a new project no longer means
#' hand-copying/renaming one of the master \verb{plotopts_*.csv} files
#' yourself.
#'
#' \strong{This does not invent or compute any settings.} It reads whatever
#' master settings CSV is currently on disk for each requested plot
#' function (via \code{dir.load} and the fixed \code{MASTER.PLOTOPTS.FILES}
#' mapping below) and writes it back out byte-for-byte-equivalent (same
#' columns, same rows, same values, re-quoted as needed by
#' \code{utils::write.csv()}) under the new file name. Editing the
#' \code{$overide.value} column of the file this function writes is the
#' normal way to customize a project's plot appearance without touching
#' the shared master file - see \code{$aes.style} on the plot functions
#' themselves.
#'
#' \strong{Recognized plot functions and their master files (judgment call,
#' flagged):} Josh's spec described \code{generate.files} only as "a
#' vector of all current plot function names in Batz package", without
#' listing the actual master file name to copy for each one. The three
#' functions treated as "plot functions" here are the three whose
#' settings files were restructured earlier in this same round nineteen
#' pass - \code{batz.generate_plotframe.bat} was confirmed NOT a plotting
#' function and is excluded, exactly as it was excluded from the rest of
#' round nineteen's changes. The mapping used:
#' \itemize{
#'   \item \code{batz.plotcover_bullseye} -> \verb{plotopts_bullseye.csv}
#'   \item \code{batz.plotdetections_first.last} -> \verb{plotopts_first.last.csv}
#'   \item \code{batz.plotactivity_observations} -> \verb{plotopts_callobs.csv}
#' }
#' A name in \code{generate.files} that isn't one of these three stops the
#' function immediately with a clear error listing the recognized names,
#' rather than silently skipping it.
#'
#' \strong{Output file name - "plotopts", not "plotobs" (judgment call,
#' flagged).} Josh's spec text says \verb{"<project.name>_plotobs_<function.name>.csv"}
#' when describing the file name, but calls the same file a "plotops
#' file" (also not "plotopts") one sentence earlier - read as a dictation/
#' typing slip either way, not a real request for a third, differently-spelled
#' term. The output name here uses \code{"plotopts"}, matching the
#' established spelling used everywhere else this round
#' (\verb{plotopts_bullseye.csv}, \verb{plotopts_first.last.csv},
#' \verb{plotopts_callobs.csv}, and the \code{$aes.style}/\code{$overide.value}
#' mechanism's own documentation) - please confirm this reading is right.
#'
#' \strong{\code{dir.load} - added, not explicitly requested (judgment
#' call, flagged).} Josh's spec listed \code{dir.save} but not a matching
#' "where to read the master files from" argument. Since this function has
#' to read each master settings CSV from somewhere, \code{dir.load}
#' (default \code{getwd()}, mirroring \code{dir.save}'s own default) was
#' added so the read location is explicit and overridable rather than
#' hardcoded - consistent with this project's existing
#' \code{dir.load}/\code{dir.save} naming pair used elsewhere in the
#' package (see the Naming conventions section of this project's
#' preferences).
#'
#' @param generate.files Character vector, default all three recognized
#'   plot function names (\code{c("batz.plotcover_bullseye",
#'   "batz.plotdetections_first.last", "batz.plotactivity_observations")}):
#'   which plot function(s) to generate a new settings file for. Every
#'   element must be one of these three recognized names - see Details.
#' @param project.name Character, default \code{"new project"} (per Josh's
#'   own literal spec text for this function - note this is a plain space,
#'   not the dot used in \code{project.name}'s default on the plot
#'   functions themselves, e.g. \code{"new.project"}; kept exactly as
#'   Josh specified it here since this value only ever becomes a file-name
#'   prefix, the same role \code{project.name} plays on the plot
#'   functions). Used as the leading part of every generated file's name:
#'   \verb{"<project.name>_plotopts_<function.name>.csv"}.
#' @param dir.load Directory to read each recognized plot function's master
#'   settings CSV from (see the \code{MASTER.PLOTOPTS.FILES} mapping in
#'   Details). Default \code{getwd()}. Added this round - see Details.
#' @param dir.save Directory to write the generated settings file(s) into.
#'   Default \code{getwd()}.
#'
#' @return Invisibly, a character vector of the file names written (not
#'   full paths - just the \verb{"<project.name>_plotopts_<function.name>.csv"}
#'   name for each element of \code{generate.files}, in the same order).
#'   As a side effect, each saved file's name is also printed to the
#'   console (per Josh: "Print the save names for all").
#'
#' @examples
#' \dontrun{
#' # Generate settings files for all three plot functions, using the
#' # master copies in the current working directory, into a new
#' # project sub-folder:
#' batz.generate_plotopts(project.name = "acme.wetlands.2026",
#'                         dir.save = "acme_project")
#'
#' # Just one function:
#' batz.generate_plotopts(generate.files = "batz.plotcover_bullseye",
#'                         project.name = "acme.wetlands.2026")
#' }
#'
#' @export
batz.generate_plotopts <- function(generate.files = c("batz.plotcover_bullseye",
                                                        "batz.plotdetections_first.last",
                                                        "batz.plotactivity_observations"),
                                    project.name = "new project",
                                    dir.load = getwd(),
                                    dir.save = getwd()) {

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
    opts <- utils::read.csv(master.path, stringsAsFactors = FALSE, check.names = FALSE,
                             colClasses = "character")

    out.name <- sprintf("%s_plotopts_%s.csv", project.name, fn.name)
    out.path <- file.path(dir.save, out.name)

    utils::write.csv(opts, out.path, row.names = FALSE)

    saved.files <- c(saved.files, out.name)
    cat("Saved:", out.name, "\n")
  }

  invisible(saved.files)
}

#' Merge and standardize SM5 ARU activity-log summary files
#'
#' Searches a directory (and, optionally, its subdirectories) for SM5
#' activity-log summary files, works out from each file's headers which SM5
#' firmware wrote it, and merges each firmware version into its own data
#' frame: \code{SM5_1.5} (firmware 1.5 or earlier) and \code{SM5_1.6}
#' (firmware 1.6 or later). The ARU name is taken from the file name,
#' \code{$date} is converted to \code{YYYY-MM-DD}, and signed decimal-degree
#' coordinates (\code{$X}/\code{$Y}) are added.
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard (glob) patterns, converted
#'   to a regular expression with \code{utils::glob2rx()} and matched
#'   case-insensitively. Default
#'   \code{c("*_A_Summary*.txt", "*_B_Summary*.txt")}; matches SM5 names
#'   such as \code{"NWS01_A_Summary_1.txt"}.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from each merged data frame.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and assign) \code{log.file_sm5}: one row per file examined, with
#'   the same columns as \code{log.file_sm4} (see
#'   \code{\link{batz.merge_sm4.logfiles}}).
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols are simplified (\code{é} -> \code{e},
#'   \code{×} -> \code{X}, curly quotes -> straight quotes) and other
#'   non-ASCII characters (e.g. \code{°}, \code{µ}, \code{™}) are removed,
#'   in column names and data, right after each file is read. Files are
#'   always read as UTF-8 with a Latin-1 fallback, whatever this is set to.
#'   The log columns \code{$strip.special}, \code{$accented.letters.header},
#'   \code{$removed.symbols.header}, \code{$accented.letters.data} and
#'   \code{$removed.symbols.data} report what was changed.
#'
#' @return Invisibly, a named list: \code{SM5_1.5} and \code{SM5_1.6}
#'   (always; an empty \code{data.frame()} if no file of that version
#'   loaded), plus \code{log.file_sm5} when \code{log.file = TRUE}. Each
#'   element is also assigned into the calling environment, so a bare call
#'   creates them in your workspace.
#'
#'   \code{SM5_1.5} columns: \code{$aru.name}, \code{$date}, \code{$time},
#'   \code{$lat}, \code{$ns}, \code{$longitude}, \code{$ew}, \code{$power.v},
#'   \code{$temperature.c}, \code{$acfiles}, \code{$fs1files},
#'   \code{$fs2files}, \code{$zc1files}, \code{$zc2files}, \code{$scrub1},
#'   \code{$scrub2}, \code{$X}, \code{$Y}.
#'
#'   \code{SM5_1.6} columns: \code{$aru.name}, \code{$date}, \code{$time},
#'   \code{$lat}, \code{$ns}, \code{$longitude}, \code{$ew}, \code{$power.v},
#'   \code{$temperature.c}, \code{$acfiles}, \code{$acl}, \code{$acr},
#'   \code{$X}, \code{$Y}.
#'
#' @details
#' Uses the same internal engine (\code{sm.logfile.merge()},
#' \code{batz.util_sm.logfile.R}) as \code{\link{batz.merge_sm4.logfiles}}
#' and \code{\link{batz.merge_sm.logfiles}}, so file search, ARU name, date
#' and coordinate conversion, duplicate removal and log reasons work the same
#' way.
#'
#' \strong{Firmware detection.} Raw headers are standardized with
#' \code{standardize.headers()} first (\code{"#ACFILES"} -> \code{acfiles},
#' \code{"POWER(V)"} -> \code{power_v}). A file is SM5.1.5 if it has the
#' firmware-1.5 headers (\code{DATE, TIME, LAT, NS, LON, EW, POWER(V),
#' TEMP(C), #ACFILES, #FS1FILES, #FS2FILES, #ZC1FILES, #ZC2FILES, #SCRUB1,
#' #SCRUB2}) and SM5.1.6 if it has the firmware-1.6 headers (\code{DATE,
#' TIME, LAT, NS, LON, EW, POWER(V), TEMP(C), #ACFILES, ACL, ACR}). A file
#' that is only partly right is assigned the version it most resembles and
#' fails with \code{"These headers are missing: ..."}. A file matching no
#' layout is logged as \code{"unknown"}.
#'
#' \strong{Output names.} \code{lon} becomes \code{$longitude},
#' \code{power_v} \code{$power.v} and \code{temp_c} \code{$temperature.c}.
#'
#' \strong{SM4 files are not merged here.} An SM4 file matched by
#' \code{load.pattern} is logged as \code{"Failure"} with
#' \code{$version = "SM4"} and a reason pointing to
#' \code{batz.merge_sm4.logfiles()} or \code{batz.merge_sm.logfiles()}.
#'
#' \strong{Not filtered.} Before the unit gets a GPS fix,
#' \code{$lat}/\code{$longitude} can be recorded as \code{0}, giving
#' \code{$X}/\code{$Y} of 0. Rows from before a deployment are kept; the log
#' shows them as a \code{$date.range} much larger than \code{$date.unique}.
#'
#' \strong{Note.} The SM5.1.6 layout has only been tested with synthetic
#' files.
#'
#' @seealso \code{\link{batz.merge_sm4.logfiles}},
#'   \code{\link{batz.merge_sm.logfiles}}
#'
#' @examples
#' \dontrun{
#' batz.merge_sm5.logfiles()
#' # SM5_1.5 and SM5_1.6 are now in your workspace
#'
#' batz.merge_sm5.logfiles(dir.sub = TRUE, log.file = TRUE)
#' # ...plus log.file_sm5, with a $version column
#' }
#'
#' @export
batz.merge_sm5.logfiles <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE,
                                   strip.special     = TRUE) {

  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = c("SM5.1.5", "SM5.1.6"),
                          caller.name = "batz.merge_sm5.logfiles()",
                          strip.special = strip.special)

  result <- list(SM5_1.5 = out$data[["SM5.1.5"]],
                 SM5_1.6 = out$data[["SM5.1.6"]])
  if (log.file) result$log.file_sm5 <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

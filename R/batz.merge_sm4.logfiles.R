#' Merge and standardize SM4 ARU activity-log summary files
#'
#' Searches a directory (and, optionally, its subdirectories) for SM4
#' Autonomous Recording Unit (ARU) activity-log summary files, checks each
#' file's headers, and merges the SM4 files into one data frame,
#' \code{sm4logs.merged}. The ARU name is taken from the file name,
#' \code{$date} is converted to \code{YYYY-MM-DD}, and \code{$latitude} /
#' \code{$longitude} are signed decimal degrees. Optionally returns a log with
#' one row per file examined.
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard (glob) patterns, converted
#'   to a regular expression with \code{utils::glob2rx()} and matched
#'   case-insensitively. Default
#'   \code{c("*_A_Summary*.txt", "*_B_Summary*.txt")}. The \code{*} after
#'   \code{_Summary} also picks up renamed copies such as
#'   \code{"WTG-GOM102_A_Summary - Copy.txt"}.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from the merged data frame (not per file). The number
#'   removed is printed.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and assign) \code{log.file_sm4}, with one row per file examined
#'   (see Value).
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols are simplified (\code{é} -> \code{e},
#'   \code{×} -> \code{X}, curly quotes -> straight quotes) and other
#'   non-ASCII characters (e.g. \code{°}, \code{µ}, \code{™}) are removed,
#'   in column names and data, right after each file is read. Files are
#'   always read as UTF-8 with a Latin-1 fallback, whatever this is set to.
#'
#' @return Invisibly, a named list: \code{sm4logs.merged} (always), plus
#'   \code{log.file_sm4} when \code{log.file = TRUE}. Each element is also
#'   assigned into the calling environment, so a bare call creates them in
#'   your workspace.
#'
#'   \code{sm4logs.merged} columns: \code{$aru.name}, \code{$date},
#'   \code{$time}, \code{$latitude}, \code{$ns}, \code{$longitude}, \code{$ew},
#'   \code{$power.v}, \code{$temperature.c}, \code{$files}, \code{$scrubbed},
#'   \code{$mic0.type}. It is an empty
#'   \code{data.frame()} (with a printed message) if no file loaded.
#'
#'   \code{log.file_sm4} columns:
#'   \itemize{
#'     \item \code{$aru.name}, \code{$filename}, \code{$filepath}: always set.
#'     \item \code{$version}: \code{"SM4"}, \code{"SM5.1.5"},
#'       \code{"SM5.1.6"} or \code{"unknown"}.
#'     \item \code{$date.start}, \code{$date.end}: earliest and latest
#'       \code{$date} in the file (\code{YYYY-MM-DD}).
#'     \item \code{$date.unique}: number of distinct \code{$date} values.
#'     \item \code{$date.range}: calendar days from \code{$date.start} to
#'       \code{$date.end}, inclusive. Larger than \code{$date.unique} when
#'       days are missing.
#'     \item \code{$records}: rows in the file (before duplicate removal).
#'     \item \code{$load.status}: \code{"Success"} or \code{"Failure"}.
#'     \item \code{$reason}: see Details.
#'     \item \code{$strip.special}: \code{"FALSE"} (not selected),
#'       \code{"TRUE NONE"} (nothing found) or
#'       \code{"TRUE ; <header>; ..."} (headers where characters were
#'       changed).
#'     \item \code{$accented.letters.header}, \code{$removed.symbols.header}:
#'       number of unique column names with a character simplified /
#'       removed.
#'     \item \code{$accented.letters.data}, \code{$removed.symbols.data}:
#'       number of unique data values with a character simplified / removed
#'       (\code{café} in 500 rows counts once). \code{NA} when
#'       \code{strip.special = FALSE} or the file could not be read.
#'   }
#'   The date and record columns are \code{NA} on \code{"Failure"} rows.
#'
#' @details
#' The reading, version detection, conversion and logging are done by the
#' internal engine \code{sm.logfile.merge()} (\code{batz.util_sm.logfile.R}),
#' shared with \code{\link{batz.merge_sm5.logfiles}} and
#' \code{\link{batz.merge_sm.logfiles}}. This function keeps only SM4 files.
#'
#' \strong{Headers.} Raw headers are standardized with
#' \code{standardize.headers()} (trimmed, lower case, runs of punctuation or
#' spaces become \code{"_"}). An SM4 file has \code{DATE, TIME, LAT, NS, LON,
#' EW, POWER(V), TEMP(C), #FILES, #SCRUBBED, MIC0 TYPE}. In the output,
#' \code{lat} becomes \code{$latitude}, \code{lon} \code{$longitude},
#' \code{power_v} \code{$power.v},
#' \code{temp_c} \code{$temperature.c} and \code{mic0_type}
#' \code{$mic0.type}. Extra columns are dropped and do not cause a failure.
#'
#' \strong{Version detection.} Each file's headers are compared with the
#' SM4, SM5 firmware 1.5 and SM5 firmware 1.6 layouts. An SM5 file is not
#' merged: it is logged as \code{"Failure"} with a reason pointing to
#' \code{batz.merge_sm5.logfiles()} or \code{batz.merge_sm.logfiles()}. A file
#' matching no layout is \code{"unknown"}, reason \code{"could not identify
#' SM version from headers"}.
#'
#' \strong{Log reasons.}
#' \itemize{
#'   \item \code{"All headers present and observation in file"} (success).
#'   \item \code{"no data"}: headers fine, zero rows.
#'   \item \code{"These headers are missing: <list>"}.
#'   \item \code{"no data and These headers are missing: <list>"}.
#'   \item \code{"could not read file"}.
#' }
#'
#' \strong{ARU name.} Everything before the first \code{"_"} in the file
#' name (\code{"AYERS_A_Summary.txt"} -> \code{"AYERS"}).
#'
#' \strong{Dates.} Only \code{"YYYY-Mon-DD"} values (e.g.
#' \code{"2026-Jun-26"}) are converted, with a fixed English month lookup
#' (not locale dependent). Other values are left unchanged; they count in
#' \code{$date.unique} but not in \code{$date.start}, \code{$date.end} or
#' \code{$date.range}.
#'
#' \strong{Coordinates.} The log's \code{LAT}/\code{LON} are decimal degrees
#' without a sign. \code{$latitude} is made negative when \code{$ns} is
#' \code{"S"}, and \code{$longitude} when \code{$ew} is \code{"W"};
#' \code{$ns} and \code{$ew} are kept.
#'
#' \strong{Duplicates.} A \code{" - Copy"} file identical to its original
#' adds identical rows; \code{duplicates.remove = TRUE} drops them from
#' \code{sm4logs.merged}, but both files keep their own log row.
#'
#' @seealso \code{\link{batz.merge_sm5.logfiles}},
#'   \code{\link{batz.merge_sm.logfiles}}
#'
#' @examples
#' \dontrun{
#' batz.merge_sm4.logfiles()
#' # sm4logs.merged is now in your workspace
#'
#' batz.merge_sm4.logfiles(dir.sub = TRUE, log.file = TRUE)
#' # sm4logs.merged and log.file_sm4 both created
#' table(log.file_sm4$version, log.file_sm4$load.status)
#' }
#'
#' @export
batz.merge_sm4.logfiles <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE,
                                   strip.special     = TRUE) {

  ## 2026-09-29: shared engine (batz.util_sm.logfile.R) - detects each
  ## file's SM version from its headers and only merges SM4 files here.
  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = "SM4", caller.name = "batz.merge_sm4.logfiles()",
                          strip.special = strip.special)

  sm4logs.merged <- out$data[["SM4"]]
  if (nrow(sm4logs.merged) == 0) {
    cat("\nNo files were successfully loaded - sm4logs.merged is empty.\n")
  }

  result <- list(sm4logs.merged = sm4logs.merged)
  if (log.file) result$log.file_sm4 <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

#' Merge activity-log summary files from any SM ARU (SM4 and SM5)
#'
#' Loads every summary file matching \code{load.pattern}, works out from each
#' file's headers whether an SM4 or an SM5 (firmware 1.5 or earlier, or 1.6
#' or later) wrote it, and merges each version into its own data frame:
#' \code{SM4}, \code{SM5_1.5}, \code{SM5_1.6}. Inputs, conversions and log
#' columns are the same as in \code{\link{batz.merge_sm4.logfiles}} and
#' \code{\link{batz.merge_sm5.logfiles}}.
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard (glob) patterns, converted
#'   to a regular expression with \code{utils::glob2rx()} and matched
#'   case-insensitively. Default
#'   \code{c("*_A_Summary*.txt", "*_B_Summary*.txt")}.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from each merged data frame.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and assign) \code{log.file_sm}: one row per file examined, with
#'   the same columns as \code{log.file_sm4} (see
#'   \code{\link{batz.merge_sm4.logfiles}}). \code{$version} is
#'   \code{"SM4"}, \code{"SM5.1.5"}, \code{"SM5.1.6"} or \code{"unknown"}.
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
#' @return Invisibly, a named list: \code{SM4}, \code{SM5_1.5},
#'   \code{SM5_1.6} (always; an empty \code{data.frame()} for a version with
#'   no files), plus \code{log.file_sm} when \code{log.file = TRUE}. Each
#'   element is also assigned into the calling environment.
#'
#'   \code{SM4} has the same columns as \code{sm4logs.merged}:
#'   \code{$aru.name}, \code{$date}, \code{$time}, \code{$lat}, \code{$ns},
#'   \code{$longitude}, \code{$ew}, \code{$power.v}, \code{$temperature.c},
#'   \code{$files}, \code{$scrubbed}, \code{$mic0.type}, \code{$X},
#'   \code{$Y}. \code{SM5_1.5}/\code{SM5_1.6} are as described in
#'   \code{\link{batz.merge_sm5.logfiles}}.
#'
#' @details
#' Uses the same internal engine (\code{sm.logfile.merge()},
#' \code{batz.util_sm.logfile.R}) as the SM4 and SM5 functions, but keeps all
#' three versions. Version detection is described in
#' \code{\link{batz.merge_sm5.logfiles}}. A file whose headers match none of
#' the three layouts is logged as \code{$version = "unknown"},
#' \code{"Failure"}, reason \code{"could not identify SM version from
#' headers"}; it is not merged.
#'
#' Each SM merge function has its own log name (\code{log.file_sm4},
#' \code{log.file_sm5}, \code{log.file_sm}), so none overwrites another.
#'
#' @seealso \code{\link{batz.merge_sm4.logfiles}},
#'   \code{\link{batz.merge_sm5.logfiles}}
#'
#' @examples
#' \dontrun{
#' batz.merge_sm.logfiles(dir.sub = TRUE, log.file = TRUE)
#' # SM4, SM5_1.5, SM5_1.6 and log.file_sm are now in your workspace
#' table(log.file_sm$version, log.file_sm$load.status)
#' }
#'
#' @export
batz.merge_sm.logfiles <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE,
                                   strip.special     = TRUE) {

  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = c("SM4", "SM5.1.5", "SM5.1.6"),
                          caller.name = "batz.merge_sm.logfiles()",
                          strip.special = strip.special)

  result <- list(SM4     = out$data[["SM4"]],
                 SM5_1.5 = out$data[["SM5.1.5"]],
                 SM5_1.6 = out$data[["SM5.1.6"]])
  if (log.file) result$log.file_sm <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

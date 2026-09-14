#' Merge and standardize SM4 ARU activity-log summary files
#'
#' Searches a directory (and, optionally, its subdirectories) for SM4
#' Autonomous Recording Unit activity-log summary files
#' (\code{"*_A_Summary.txt"}/\code{"*_B_Summary.txt"}), validates each
#' file's headers, and merges them into one standardized master data frame:
#' the ARU name is extracted from the file name, \code{$date} is normalized
#' to \code{YYYY-MM-DD}, and \code{$lat}/\code{$ns} and \code{$lon}/\code{$ew}
#' are converted to signed decimal degrees (\code{$Y}/\code{$X}).
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard/glob suffix patterns
#'   (converted internally to a regex via \code{utils::glob2rx()}), default
#'   \code{c("*_A_Summary.txt", "*_B_Summary.txt")}.
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from the final merged data frame. (Not listed in the
#'   original spec's "Optional inputs", but used in its Steps section -
#'   added as a real parameter; see the dev script's header comment.)
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and auto-assign) \code{sm4logs.merged_log.file}: one row per
#'   skipped input file, with \code{$filepath} and \code{$reason}
#'   (\code{"mismatched headers (missing: ...)"}, \code{"no records"}, or
#'   \code{"could not read file"}).
#'
#' @return Invisibly, a named list: \code{sm4logs.merged} (always), plus
#'   \code{sm4logs.merged_log.file} when \code{log.file = TRUE}. Every
#'   element is also auto-assigned into the calling environment (same
#'   pattern already used in \code{batz.merge_aru.meta},
#'   \code{batz.datawrangler_load.files}, and
#'   \code{batz.arumeta_generate.eventlog}), so a bare call with no
#'   assignment creates \code{sm4logs.merged} (and the log table, if
#'   requested) directly in your workspace.
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - real, documented output-schema change.} The 11 expected SM4 summary
#' columns are a literal, uninvented copy of the ARU device's own real
#' export column text (not a \code{batz}-invented shorthand), so - like the
#' raw headers read off every file - they are now run through the shared
#' package helper \code{standardize.headers()} (trim whitespace, collapse
#' every run of non-alphanumeric characters to a single underscore,
#' lowercase). The expected-header list itself was rewritten to the
#' standardized spellings so both sides of the header-validation check line
#' up: \code{DATE} -> \code{date}, \code{TIME} -> \code{time}, \code{LAT} ->
#' \code{lat}, \code{NS} -> \code{ns}, \code{LON} -> \code{lon}, \code{EW} ->
#' \code{ew}, \code{POWER(V)} -> \code{power_v}, \code{TEMP(C)} ->
#' \code{temp_c}, \code{#FILES} -> \code{files}, \code{#SCRUBBED} ->
#' \code{scrubbed}, \code{MIC0 TYPE} -> \code{mic0_type}. Since these are the
#' exact columns kept (and renamed to nothing else) in \code{sm4logs.merged},
#' this is a real, visible change in that returned data frame's own column
#' names - anyone with existing code reading \code{sm4logs.merged$DATE},
#' \code{$NS}, \code{$EW}, etc. by the old upper-case/punctuated names will
#' need to switch to the new standardized ones. \strong{This does NOT
#' affect \code{$aru.name}, \code{$X}, or \code{$Y}} - none of the three is
#' a header loaded from any file: \code{aru.name} is parsed from the file's
#' own NAME, and \code{X}/\code{Y} are this function's own derived/computed
#' columns, so all three keep their existing names per this project's
#' ordinary output convention.
#'
#' \strong{Header validation:} a file must have all 11 expected columns
#' (now matched by their standardized spellings, case-insensitively and
#' whitespace/punctuation-insensitively by construction) to be merged in. A
#' file missing one or more of them is skipped with reason \code{"mismatched
#' headers"}; a file with a header row but zero data rows is skipped with
#' reason \code{"no records"}. Extra, unexpected columns don't cause a skip -
#' only a missing expected column does.
#'
#' \strong{ARU name:} taken from the file name, everything before the first
#' \code{"_"} (e.g. \code{"AYERS_A_Summary.txt"} -> \code{"AYERS"}).
#'
#' \strong{Date conversion:} only the observed real-data format,
#' \code{"YYYY-Mon-DD"} with a 3-letter month abbreviation (e.g.
#' \code{"2026-Jun-26"}), is converted to \code{"YYYY-MM-DD"}; a value in
#' any other format is left unchanged. Uses a fixed, locale-independent
#' month-name lookup rather than \code{strptime}'s locale-dependent
#' \code{\%b}.
#'
#' \strong{Coordinate conversion:} \code{$lat}/\code{$lon} in the real data
#' are already plain decimal degrees, so \code{$Y}/\code{$X} are produced by
#' applying the correct sign from the hemisphere letter only (\code{"s"} ->
#' negative \code{$Y}, \code{"w"} -> negative \code{$X}) - not a
#' degrees-minutes-seconds parse. The original \code{$lat}/\code{$ns}/
#' \code{$lon}/\code{$ew} columns are kept alongside the new \code{$Y}/
#' \code{$X} columns, not replaced.
#'
#' @examples
#' \dontrun{
#' batz.merge_sm4.logfile()
#' # sm4logs.merged is now in your workspace
#'
#' batz.merge_sm4.logfile(dir.sub = TRUE, log.file = TRUE)
#' # sm4logs.merged and sm4logs.merged_log.file both created
#' }
#'
#' @export
batz.merge_sm4.logfile <- function(dir.load = getwd(),
                                          dir.sub           = FALSE,
                                          load.pattern      = c("*_A_Summary.txt", "*_B_Summary.txt"),
                                          duplicates.remove = TRUE,
                                          log.file          = FALSE) {

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  ## Header standardization (per Josh, 2026-09-14 project preference): the
  ## expected-header list is a literal, uninvented copy of the SM4 device's
  ## own real export column text, so it's rewritten here to the standardized
  ## spellings that raw file headers will also be run through below - see
  ## @details "Header standardization" above.
  expected.headers <- standardize.headers(c("DATE", "TIME", "LAT", "NS", "LON", "EW",
                                             "POWER(V)", "TEMP(C)", "#FILES", "#SCRUBBED", "MIC0 TYPE"))

  month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                     jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

  convert.date <- function(x) {
    m <- regmatches(x, regexpr("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x))
    out <- x
    has.match <- nzchar(m)
    if (any(has.match)) {
      parts <- regmatches(x[has.match], regexec("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x[has.match]))
      converted <- vapply(parts, function(p) {
        yr <- p[2]; mon <- tolower(p[3]); day <- p[4]
        mm <- month.lookup[mon]
        if (is.na(mm)) return(NA_character_)
        paste(yr, mm, day, sep = "-")
      }, character(1))
      out[has.match] <- ifelse(is.na(converted), x[has.match], converted)
    }
    out
  }

  process.one.file <- function(f) {
    raw <- tryCatch(
      read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
      error = function(e) NULL
    )
    if (is.null(raw)) return(list(data = NULL, reason = "could not read file"))

    ## header standardization (per Josh, 2026-09-14 project preference) -
    ## replaces the previous bare trimws(names(raw)); see @details above.
    names(raw) <- standardize.headers(names(raw))
    present <- expected.headers %in% names(raw)
    if (!all(present)) {
      return(list(data = NULL, reason = paste0("mismatched headers (missing: ",
                                                paste(expected.headers[!present], collapse = ", "), ")")))
    }

    if (nrow(raw) == 0) {
      return(list(data = NULL, reason = "no records"))
    }

    tmp <- raw[expected.headers]
    for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

    base.name <- basename(f)
    tmp$aru.name <- sub("_.*$", "", base.name)

    tmp$date <- convert.date(tmp$date)

    ns <- tolower(trimws(tmp$ns))
    ew <- tolower(trimws(tmp$ew))
    tmp$Y <- ifelse(ns == "s", -as.numeric(tmp$lat), as.numeric(tmp$lat))
    tmp$X <- ifelse(ew == "w", -as.numeric(tmp$lon), as.numeric(tmp$lon))

    tmp <- tmp[c("aru.name", expected.headers, "X", "Y")]

    list(data = tmp, reason = NA_character_)
  }

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  sm4logs.merged <- NULL
  log.rows <- list()

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f)
      if (is.null(r$data)) {
        cat("  [skipped] ", f, " - ", r$reason, "\n", sep = "")
        log.rows[[length(log.rows) + 1]] <- data.frame(filepath = f, reason = r$reason, stringsAsFactors = FALSE)
      } else {
        cat("  loaded ", f, " (", nrow(r$data), " rows)\n", sep = "")
        sm4logs.merged <- if (is.null(sm4logs.merged)) r$data else rbind(sm4logs.merged, r$data)
      }
    }
  }

  sm4logs.merged_log.file <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    data.frame(filepath = character(0), reason = character(0), stringsAsFactors = FALSE)
  }

  if (is.null(sm4logs.merged)) {
    sm4logs.merged <- data.frame()
    cat("\nNo files were successfully loaded - sm4logs.merged is empty.\n")
  } else if (duplicates.remove) {
    dup.mask <- duplicated(sm4logs.merged)
    n.dup <- sum(dup.mask)
    if (n.dup > 0) {
      cat("\n", n.dup, " duplicate row(s) removed from sm4logs.merged.\n", sep = "")
      sm4logs.merged <- sm4logs.merged[!dup.mask, ]
    }
    rownames(sm4logs.merged) <- NULL
  }

  result <- list(sm4logs.merged = sm4logs.merged)
  if (log.file) result$sm4logs.merged_log.file <- sm4logs.merged_log.file

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

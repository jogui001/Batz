#' Internal engine shared by the SM log-file merge functions
#'
#' Used by \code{batz.merge_sm4.logfiles()}, \code{batz.merge_sm5.logfiles()}
#' and \code{batz.merge_sm.logfiles()}, so all three find, read, identify,
#' convert and log Song Meter summary files the same way.
#' \code{sm.logfile.schemas()} returns the three known header sets,
#' \code{sm.logfile.detect.version()} picks the version of one file and
#' \code{sm.logfile.merge()} does the merge.
#'
#' @details
#' \strong{Version detection.} After \code{standardize.headers()}, each
#' file's headers are compared with the three header sets:
#' \itemize{
#'   \item \code{"SM4"}: \code{date, time, lat, ns, lon, ew, power_v,
#'     temp_c, files, scrubbed, mic0_type}
#'   \item \code{"SM5.1.5"} (SM5, firmware 1.5 or earlier): \code{date,
#'     time, lat, ns, lon, ew, power_v, temp_c, acfiles, fs1files,
#'     fs2files, zc1files, zc2files, scrub1, scrub2}
#'   \item \code{"SM5.1.6"} (SM5, firmware 1.6 or later): \code{date, time,
#'     lat, ns, lon, ew, power_v, temp_c, acfiles, acl, acr}
#' }
#' A file with every header of a set is that version (if it has every
#' header of more than one set, the larger set wins). A file matching no
#' set completely is assigned the version whose \emph{distinguishing}
#' headers it has the most of (SM4: \code{files}/\code{scrubbed}/
#' \code{mic0_type}; SM5.1.5: \code{fs1files}...\code{scrub2}; SM5.1.6:
#' \code{acl}/\code{acr}; \code{acfiles} only counts when another
#' distinguishing header of that set is present) and fails with
#' "These headers are missing: ..." for that version. No distinguishing
#' header at all, or a tie, gives \code{"unknown"}.
#'
#' \strong{Loading.} Files are found with \code{list.files()} (glob
#' \code{load.pattern}, case-insensitive, recursive when \code{dir.sub =
#' TRUE}). The ARU name is the file name up to the first \code{_}. Each file
#' is read with \code{special.read.csv()} (UTF-8, falling back to Latin-1)
#' and, when \code{strip.special = TRUE}, cleaned with \code{special.strip()}
#' (accented letters simplified, other non-ASCII symbols removed) before its
#' headers are checked. A file fails (logged, not loaded) when it can't be
#' read, its version is unknown, its version is not in \code{versions.keep},
#' headers are missing, or it has no rows.
#'
#' \strong{Output columns.} \code{$aru.name}, then the version's headers
#' with \code{lat} -> \code{$latitude}, \code{lon} -> \code{$longitude},
#' \code{power_v} -> \code{$power.v},
#' \code{temp_c} -> \code{$temperature.c}, \code{mic0_type} ->
#' \code{$mic0.type}. \code{$latitude} is negative when \code{ns = "S"} and
#' \code{$longitude} negative when \code{ew = "W"} (signed decimal degrees;
#' \code{$ns}/\code{$ew} are kept). Dates such
#' as \code{2026-Jun-05} are converted to \code{2026-06-05}. With
#' \code{duplicates.remove = TRUE}, exact duplicate rows are dropped per
#' version.
#'
#' \strong{Return.} \code{sm.logfile.merge()} returns \code{list(data, log)}:
#' \code{data} is a named list with one data frame per version in
#' \code{versions.keep} (empty data frame when nothing loaded); \code{log}
#' has one row per file: \code{aru.name, filename, version, date.start,
#' date.end, date.unique, date.range, records, load.status, reason,
#' filepath}, then \code{$strip.special} (\code{"FALSE"}, \code{"TRUE NONE"}
#' or \code{"TRUE ; <header>; ..."}) and the four count columns
#' \code{$accented.letters.header}, \code{$removed.symbols.header},
#' \code{$accented.letters.data}, \code{$removed.symbols.data} (\code{NA}
#' when \code{strip.special = FALSE} or the file couldn't be read).
#'
#' @keywords internal
#' @noRd
sm.logfile.schemas <- function() {
  common <- c("date", "time", "lat", "ns", "lon", "ew", "power_v", "temp_c")
  list(
    "SM4"     = c(common, "files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c(common, "acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c(common, "acfiles", "acl", "acr")
  )
}

#' Identify the SM version of one log file from its standardized headers
#' ("SM4", "SM5.1.5", "SM5.1.6" or "unknown").
#' @keywords internal
#' @noRd
sm.logfile.detect.version <- function(hdrs) {
  schemas <- sm.logfile.schemas()
  full <- vapply(schemas, function(s) all(s %in% hdrs), logical(1))
  if (any(full)) {
    cand <- names(schemas)[full]
    return(cand[which.max(lengths(schemas[cand]))])
  }
  distinct <- list(
    "SM4"     = c("files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c("acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c("acfiles", "acl", "acr")
  )
  ## acfiles alone can't tell SM5.1.5 from SM5.1.6 - score it for neither
  ## on its own; it only tips the balance toward "SM5" when paired with
  ## that version's other distinguishing headers.
  score <- vapply(distinct, function(d) {
    hits <- d %in% hdrs
    if (sum(hits[d != "acfiles"]) == 0) 0 else sum(hits)
  }, numeric(1))
  if (max(score) == 0 || sum(score == max(score)) > 1) return("unknown")
  names(score)[which.max(score)]
}

#' Find, read, check and merge SM log files; returns \code{list(data, log)}.
#' @keywords internal
#' @noRd
sm.logfile.merge <- function(dir.load, dir.sub, load.pattern, duplicates.remove,
                             versions.keep, caller.name, strip.special = TRUE) {

  ## special characters (per Josh, 2026-09-30): headers where non-ASCII
  ## characters were removed in the file currently being processed - read by
  ## make.log.row() for the log's $strip.special column.
  cur.strip <- character(0)
  cur.counts <- NULL

  schemas <- sm.logfile.schemas()

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                     jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

  convert.date <- function(x) {
    m <- regmatches(x, regexpr("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x))
    out <- x
    has.match <- nzchar(m)
    if (any(has.match)) {
      parts <- regmatches(x[has.match], regexec("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x[has.match]))
      converted <- vapply(parts, function(p) {
        mm <- month.lookup[tolower(p[3])]
        if (is.na(mm)) return(NA_character_)
        paste(p[2], mm, p[4], sep = "-")
      }, character(1))
      out[has.match] <- ifelse(is.na(converted), x[has.match], converted)
    }
    out
  }

  make.log.row <- function(aru.name, filename, filepath, version, load.status, reason,
                            date.start = NA_character_, date.end = NA_character_,
                            date.unique = NA_integer_, date.range = NA_integer_,
                            records = NA_integer_) {
    data.frame(aru.name = aru.name, filename = filename, version = version,
               date.start = date.start, date.end = date.end,
               date.unique = date.unique, date.range = date.range,
               records = records, load.status = load.status, reason = reason,
               filepath = filepath,
               special.log.cols(strip.special, cur.strip, cur.counts)[rep(1L, length(aru.name)), , drop = FALSE],
               stringsAsFactors = FALSE, row.names = NULL)
  }

  other.fn <- function(v) {
    if (v == "SM4") "batz.merge_sm4.logfiles() or batz.merge_sm.logfiles()"
    else "batz.merge_sm5.logfiles() or batz.merge_sm.logfiles()"
  }

  process.one.file <- function(f) {
    base.name <- basename(f)
    file.aru.name <- sub("_.*$", "", base.name)

    cur.strip <<- character(0)
    cur.counts <<- NULL
    raw <- tryCatch(
      special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
      error = function(e) NULL
    )
    if (!is.null(raw)) {
      s.strip <- special.strip(raw, strip.special)
      raw <- s.strip$df
      cur.strip <<- standardize.headers(s.strip$headers)
      cur.counts <<- s.strip$counts
    }
    if (is.null(raw)) {
      return(list(data = NULL, version = "unknown",
                  log = make.log.row(file.aru.name, base.name, f, "unknown",
                                     "Failure", "could not read file")))
    }

    names(raw) <- standardize.headers(names(raw))
    version <- sm.logfile.detect.version(names(raw))

    if (version == "unknown") {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     "could not identify SM version from headers")))
    }
    if (!(version %in% versions.keep)) {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     paste0(version, " file - not loaded by ", caller.name,
                                            " (use ", other.fn(version), ")"))))
    }

    expected <- schemas[[version]]
    present <- expected %in% names(raw)
    headers.missing <- !all(present)
    no.data <- nrow(raw) == 0

    if (headers.missing || no.data) {
      missing.list <- paste(expected[!present], collapse = ", ")
      reason <- if (headers.missing && no.data) {
        paste0("no data and These headers are missing: ", missing.list)
      } else if (headers.missing) {
        paste0("These headers are missing: ", missing.list)
      } else {
        "no data"
      }
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure", reason)))
    }

    output <- expected
    output[output == "lat"] <- "latitude"
    output[output == "lon"] <- "longitude"
    output[output == "power_v"] <- "power.v"
    output[output == "temp_c"] <- "temperature.c"
    output[output == "mic0_type"] <- "mic0.type"
    tmp <- raw[expected]
    names(tmp) <- output
    for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

    tmp$aru.name <- file.aru.name
    tmp$date <- convert.date(tmp$date)

    ns <- tolower(trimws(tmp$ns))
    ew <- tolower(trimws(tmp$ew))
    ## signed decimal degrees (2026-11-06, per Josh: replaces $X/$Y)
    lat <- suppressWarnings(abs(as.numeric(tmp$latitude)))
    lon <- suppressWarnings(abs(as.numeric(tmp$longitude)))
    tmp$latitude  <- ifelse(ns == "s", -lat, lat)
    tmp$longitude <- ifelse(ew == "w", -lon, lon)

    tmp <- tmp[c("aru.name", output)]

    date.vals <- tmp$date
    date.unique.n <- length(unique(date.vals))
    iso.ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date.vals) &
      !is.na(suppressWarnings(as.Date(date.vals, format = "%Y-%m-%d")))
    if (any(iso.ok)) {
      date.start.val <- min(date.vals[iso.ok])
      date.end.val   <- max(date.vals[iso.ok])
      date.range.val <- as.integer(as.Date(date.end.val) - as.Date(date.start.val)) + 1L
    } else {
      date.start.val <- NA_character_
      date.end.val   <- NA_character_
      date.range.val <- NA_integer_
    }

    list(data = tmp, version = version,
         log = make.log.row(file.aru.name, base.name, f, version, "Success",
                            "All headers present and observation in file",
                            date.start = date.start.val, date.end = date.end.val,
                            date.unique = date.unique.n, date.range = date.range.val,
                            records = nrow(tmp)))
  }

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                          recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  pieces   <- stats::setNames(lapply(versions.keep, function(v) list()), versions.keep)
  log.rows <- list()

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f)
      log.rows[[length(log.rows) + 1]] <- r$log
      if (is.null(r$data)) {
        cat("  [skipped] ", f, " - ", r$log$reason, "\n", sep = "")
      } else {
        cat("  loaded ", f, " [", r$version, "] (", nrow(r$data), " rows)\n", sep = "")
        pieces[[r$version]][[length(pieces[[r$version]]) + 1]] <- r$data
      }
    }
  }

  merged <- lapply(versions.keep, function(v) {
    p <- pieces[[v]]
    if (length(p) == 0) return(data.frame())
    df <- do.call(rbind, p)
    if (duplicates.remove) {
      dup.mask <- duplicated(df)
      if (any(dup.mask)) {
        cat("\n", sum(dup.mask), " duplicate row(s) removed from ", v, " data.\n", sep = "")
        df <- df[!dup.mask, , drop = FALSE]
      }
    }
    rownames(df) <- NULL
    df
  })
  names(merged) <- versions.keep

  log.df <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    cur.strip <- character(0)
    make.log.row(character(0), character(0), character(0), character(0),
                 character(0), character(0), character(0), character(0),
                 integer(0), integer(0), integer(0))[0, ]
  }

  list(data = merged, log = log.df)
}

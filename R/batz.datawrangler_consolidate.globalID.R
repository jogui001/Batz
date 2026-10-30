#' Give every survey of the same site the GlobalID of its earliest record
#'
#' Generates a global ID for each unique site for surveys collected at
#' different times. Reads data frames or Excel / csv files, finds each unique
#' combination of the \code{header.combo} headers (e.g. \code{project.name} +
#' \code{aru.name}), picks the earliest record of that combination by the
#' \code{header.dates} headers, and copies that record's unique ID
#' (\code{header.uniqueid}, e.g. \code{GlobalID}) to every record of the
#' combination under the new header \code{$globalID_consolidate}. Built
#' 2026-10-30, per Josh.
#'
#' @param data A data frame, a list of data frames (e.g. the output of
#'   \code{batz.merge_sitevisit.acoustics()}), or a character vector of
#'   \code{.xlsx} / \code{.xls} / \code{.csv} file paths (every sheet of a
#'   workbook is read). All tables are consolidated together, so a site's
#'   earliest record is found across every table given.
#' @param header.combo Character, default \code{c("project.name", "aru.name")}.
#'   Headers whose unique combinations define a site.
#' @param header.uniqueid Character, default \code{c("GlobalID")}. Header
#'   holding each record's unique ID. When more than one is given, the first
#'   one found in a table is used.
#' @param header.dates Character, default \code{c("date.habitat.assessment",
#'   "date.deployment", "date")}. Date headers used to find the earliest
#'   record. For each record the first of these headers (in the order given)
#'   that has a value is used.
#'
#' @return The same shape as \code{data} - a data frame, or a list of data
#'   frames (one per table / sheet when files are read; list elements that are
#'   not data frames are returned unchanged) - with \code{$globalID_consolidate}
#'   added just after the unique-ID column of every table that has the
#'   \code{header.combo} and \code{header.uniqueid} headers.
#'
#' @details
#' \strong{Steps.}
#' \enumerate{
#'   \item Tables missing any \code{header.combo} header, or every
#'     \code{header.uniqueid} header, are left unchanged (NOTE).
#'   \item Combination values are compared ignoring case and leading /
#'     trailing spaces (\code{"Seven Islands "} = \code{"seven islands"}).
#'   \item Dates may be dates, date-times, Excel serial numbers,
#'     \code{yyyymmdd} numbers or text (\code{yyyy-mm-dd}, \code{mm/dd/yyyy},
#'     with or without a time).
#'   \item For each combination the record with the earliest date is chosen.
#'     Records with no date are only chosen when no record of the combination
#'     has one. Ties go to the record read first (table order, then row order);
#'     a NOTE lists combinations where tied records have different IDs.
#'   \item That record's ID is written to \code{$globalID_consolidate} for
#'     every record of the combination. Records with a blank
#'     \code{header.combo} value keep their own ID (NOTE).
#' }
#'
#' @examples
#' \dontrun{
#' m <- batz.merge_sitevisit.acoustics(dir.load = "C:/data/Survey123")
#' m2 <- batz.datawrangler_consolidate.globalID(m[c("habitat", "service")])
#' table(m2$service$globalID_consolidate)
#'
#' x <- batz.datawrangler_consolidate.globalID("C:/data/mergedsurvey123.xlsx")
#' }
#'
#' @export
batz.datawrangler_consolidate.globalID <- function(data,
                                                   header.combo = c("project.name", "aru.name"),
                                                   header.uniqueid = c("GlobalID"),
                                                   header.dates = c("date.habitat.assessment", "date.deployment", "date")) {

  new.header <- "globalID_consolidate"
  if (!is.character(header.combo) || length(header.combo) == 0) stop("header.combo must be a character vector of header names", call. = FALSE)
  if (!is.character(header.uniqueid) || length(header.uniqueid) == 0) stop("header.uniqueid must be a character vector of header names", call. = FALSE)
  if (!is.character(header.dates)) stop("header.dates must be a character vector of header names", call. = FALSE)

  ## --- 1. read: data frame / list / files ----------------------------------
  single.df <- is.data.frame(data)
  if (single.df) {
    tabs <- list(data)
  } else if (is.character(data)) {
    miss <- data[!file.exists(data)]
    if (length(miss) > 0) stop("File(s) not found: ", paste(miss, collapse = ", "), call. = FALSE)
    tabs <- list()
    for (f in data) {
      nm <- basename(f)
      if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        tabs[[sub("\\.csv$", "", nm, ignore.case = TRUE)]] <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) {
        if (!requireNamespace("readxl", quietly = TRUE)) stop("The readxl package is needed: install.packages(\"readxl\")", call. = FALSE)
        if (file.exists(file.path(dirname(f), paste0("~$", nm))) || file.exists(file.path(dirname(f), paste0("~$", substring(nm, 3)))))
          stop(nm, " is open in another program and cannot be loaded. Close it and re-run code.", call. = FALSE)
        sh <- tryCatch(readxl::excel_sheets(f), error = function(e)
          stop(nm, " is open in another program and cannot be loaded. Close it and re-run code.", call. = FALSE))
        for (s in sh) {
          id <- if (length(data) == 1) s else paste0(sub("\\.xlsx?$", "", nm, ignore.case = TRUE), "_", s)
          tabs[[id]] <- as.data.frame(readxl::read_excel(f, sheet = s), check.names = FALSE)
        }
      } else {
        stop(nm, " is not a .xlsx, .xls or .csv file", call. = FALSE)
      }
    }
  } else if (is.list(data)) {
    tabs <- data
  } else {
    stop("data must be a data frame, a list of data frames, or .xlsx/.csv file path(s)", call. = FALSE)
  }
  tab.names <- if (is.null(names(tabs))) paste0("table", seq_along(tabs)) else
    ifelse(nzchar(names(tabs)), names(tabs), paste0("table", seq_along(tabs)))

  ## --- 2. date parser -------------------------------------------------------
  to.date <- function(v) {
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(format(v, "%Y-%m-%d")))
    if (is.numeric(v)) {
      out <- rep(as.Date(NA), length(v))
      ymd <- !is.na(v) & v >= 19000101 & v <= 21001231
      out[ymd] <- as.Date(sprintf("%08d", as.integer(v[ymd])), "%Y%m%d")
      ser <- !is.na(v) & !ymd & v > 0 & v < 100000
      out[ser] <- as.Date(floor(v[ser]), origin = "1899-12-30")
      return(out)
    }
    v <- trimws(as.character(v))
    out <- rep(as.Date(NA), length(v))
    for (fm in c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d", "%d-%b-%Y", "%B %d, %Y")) {
      todo <- is.na(out) & !is.na(v) & nzchar(v)
      if (!any(todo)) break
      out[todo] <- as.Date(v[todo], format = fm, optional = TRUE)
    }
    todo <- is.na(out) & grepl("^[0-9]+(\\.[0-9]+)?$", v)
    if (any(todo)) out[todo] <- to.date(as.numeric(v[todo]))
    out
  }

  ## --- 3. one row per record across every table ----------------------------
  is.blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  recs <- list(); used <- character(0); id.col <- list()
  for (i in seq_along(tabs)) {
    d <- tabs[[i]]
    if (!is.data.frame(d) || nrow(d) == 0) next
    miss <- setdiff(header.combo, names(d))
    idc <- intersect(header.uniqueid, names(d))[1]
    if (length(miss) > 0 || is.na(idc)) {
      message("NOTE: ", tab.names[i], " left unchanged - missing ",
              paste(c(miss, if (is.na(idc)) paste(header.uniqueid, collapse = "/")), collapse = ", "))
      next
    }
    id.col[[tab.names[i]]] <- idc
    dt <- rep(as.Date(NA), nrow(d))
    for (h in intersect(header.dates, names(d))) {
      fill <- is.na(dt)
      if (any(fill)) dt[fill] <- to.date(d[[h]])[fill]
    }
    combo <- do.call(paste, c(lapply(header.combo, function(h) tolower(trimws(as.character(d[[h]])))), sep = "\r"))
    label <- do.call(paste, c(lapply(header.combo, function(h) trimws(as.character(d[[h]]))), sep = " / "))
    blank <- Reduce(`|`, lapply(header.combo, function(h) is.blank(d[[h]])))
    recs[[length(recs) + 1]] <- data.frame(tab = tab.names[i], idx = i, row = seq_len(nrow(d)),
                                           combo = combo, label = label, blank = blank, date = dt,
                                           id = as.character(d[[idc]]), stringsAsFactors = FALSE)
    used <- c(used, tab.names[i])
  }
  if (length(recs) == 0) {
    message("NOTE: no table had all of ", paste(c(header.combo, header.uniqueid[1]), collapse = ", "), " - nothing consolidated")
    return(if (single.df) tabs[[1]] else tabs)
  }
  r <- do.call(rbind, recs)
  r$ord <- seq_len(nrow(r))

  ## --- 4. earliest record per combination -----------------------------------
  ok <- r[!r$blank & !is.blank(r$id), ]
  ok <- ok[order(ok$combo, is.na(ok$date), ok$date, ok$ord), ]
  first <- ok[!duplicated(ok$combo), ]
  pick <- stats::setNames(first$id, first$combo)
  lab <- stats::setNames(first$label, first$combo)
  ties <- merge(ok, first[, c("combo", "date")], by = "combo", suffixes = c("", ".first"))
  ties <- ties[(is.na(ties$date) & is.na(ties$date.first)) | (!is.na(ties$date) & !is.na(ties$date.first) & ties$date == ties$date.first), ]
  tie.combo <- unique(ties$combo[duplicated(ties$combo)])
  tie.combo <- tie.combo[vapply(tie.combo, function(cb) length(unique(ties$id[ties$combo == cb])) > 1, logical(1))]
  if (length(tie.combo) > 0)
    message("NOTE: earliest date tied for ", length(tie.combo), " site(s) - the record read first was used: ",
            paste(lab[tie.combo], collapse = "; "))
  nodate <- first$combo[is.na(first$date)]
  if (length(nodate) > 0)
    message("NOTE: no date for ", length(nodate), " site(s) - the record read first was used: ",
            paste(lab[nodate], collapse = "; "))
  nb <- sum(r$blank)
  if (nb > 0) message("NOTE: ", nb, " record(s) with a blank ", paste(header.combo, collapse = "/"), " keep their own ", header.uniqueid[1])

  r$new <- unname(pick[r$combo])
  r$new[r$blank | is.na(r$new)] <- r$id[r$blank | is.na(r$new)]

  ## --- 5. write the new header after the ID column --------------------------
  for (tn in used) {
    i <- r$idx[r$tab == tn][1]
    d <- tabs[[i]]
    v <- r$new[r$tab == tn]
    d[[new.header]] <- NULL
    pos <- match(id.col[[tn]], names(d))
    d <- cbind(d[seq_len(pos)], stats::setNames(data.frame(v, stringsAsFactors = FALSE), new.header),
               d[setdiff(seq_along(d), seq_len(pos))])
    tabs[[i]] <- d
  }
  cat(length(pick), " site(s) (", paste(header.combo, collapse = " + "), ") across ",
      length(used), " table(s): ", paste(used, collapse = ", "), "\n", sep = "")

  if (single.df) tabs[[1]] else tabs
}

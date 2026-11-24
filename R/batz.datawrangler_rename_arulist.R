#' Add $aru.label to data from an ARU list
#'
#' Adds \code{$aru.label} to \code{data}, taken from \code{aru.list} by
#' matching \code{$aru.name}. When \code{aru.list} has several rows for the
#' same \code{$aru.name} with different \code{$aru.label}s (the same ARU used
#' at two or more locations), \code{$date.start} and \code{$date.end} of those
#' rows decide which label each record gets.
#'
#' @param data A data frame with \code{$aru.name}, e.g. \code{vetted.merged}
#'   from \code{\link{batz.merge_vetted.acoustics}}. For ARUs with more than
#'   one label it also needs a date: \code{$date.monitoringnight}, else
#'   \code{$date}, else \code{$call.datetime} (see Details). Headers are
#'   matched ignoring case and "." / "_" / spaces.
#' @param aru.list A data frame, or the path of a CSV file, with
#'   \code{$aru.name} and \code{$aru.label}; \code{$date.start} and
#'   \code{$date.end} are needed only for ARUs listed with more than one
#'   label. E.g. an \verb{*arulist.csv}.
#'
#' @details
#' \strong{One label.} An \code{$aru.name} listed with one \code{$aru.label}
#' (on one or more rows) gets that label on every record; dates are not
#' checked.
#'
#' \strong{Several labels.} A record gets the label of the
#' \code{aru.list} row whose \code{$date.start} to \code{$date.end} (both
#' days included) holds the record's date. Full dates are required (day,
#' month and 4-digit year): the year must match too, and a blank or
#' incomplete \code{$date.start} / \code{$date.end} on those rows stops
#' the function. The record's date is
#' \code{$date.monitoringnight} (the night the call belongs to); without it,
#' \code{$date}; without that, the date of \code{$call.datetime}, less one
#' day for calls before 12:00 (so they count with the night before). Dates
#' may be \verb{m/d/yyyy}, \verb{yyyy-mm-dd}, \verb{yyyymmdd} or an Excel
#' date number. A record without a full date gets \code{NA} (NOTE with the
#' values); a record in no window gets \code{NA} (NOTE with the dates);
#' a record in more than one window gets the first of those rows in
#' \code{aru.list} (NOTE).
#'
#' \strong{No match.} Records whose \code{$aru.name} is blank or not in
#' \code{aru.list} get \code{NA} (NOTE). \code{$aru.name} is matched ignoring
#' case and leading / trailing spaces.
#'
#' A summary of records per \code{$aru.name} and \code{$aru.label} is
#' printed.
#'
#' @return \code{data} with \code{$aru.label} right after \code{$aru.name}.
#'   An \code{$aru.label} already in \code{data} is replaced (NOTE).
#'
#' @examples
#' \dontrun{
#' merged  <- read.csv("merged.csv")
#' arulist <- read.csv("Norcross.arulist.csv")
#' merged  <- batz.datawrangler_rename_arulist(merged, arulist)
#' table(merged$aru.name, merged$aru.label)
#' }
#'
#' @export
batz.datawrangler_rename_arulist <- function(data, aru.list) {
  FN <- "batz.datawrangler_rename_arulist"
  if (is.character(aru.list) && length(aru.list) == 1) {
    if (!file.exists(aru.list)) stop(FN, ": aru.list file not found: ", aru.list, call. = FALSE)
    aru.list <- special.read.csv(aru.list, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!is.data.frame(data)) stop(FN, ": data must be a data frame.", call. = FALSE)
  if (!is.data.frame(aru.list)) stop(FN, ": aru.list must be a data frame or a CSV file path.", call. = FALSE)

  ## headers (ignoring case and "." / "_" / spaces)
  cd <- canonicalize.headers(data, c("aru.name", "aru.label", "date.monitoringnight", "date", "call.datetime"))
  if ("aru.name" %in% cd$missing) stop(FN, ": missing headers: data $aru.name", call. = FALSE)
  data <- cd$df
  ca <- canonicalize.headers(aru.list, c("aru.name", "aru.label", "date.start", "date.end"))
  miss <- intersect(c("aru.name", "aru.label"), ca$missing)
  if (length(miss)) stop(FN, ": missing headers: aru.list $", paste(miss, collapse = ", $"), call. = FALSE)
  al <- ca$df
  key <- function(x) tolower(trimws(as.character(x)))
  al <- al[!is.na(al$aru.name) & nzchar(trimws(as.character(al$aru.name))), , drop = FALSE]
  al$aru.label <- trimws(as.character(al$aru.label))
  al$k <- key(al$aru.name)
  n.lab <- tapply(al$aru.label, al$k, function(x) length(unique(x)))
  multi <- names(n.lab)[n.lab > 1]

  dk <- key(data$aru.name)
  lab <- rep(NA_character_, nrow(data))
  ## one label: every record of that ARU
  single <- al[!al$k %in% multi & !duplicated(al$k), , drop = FALSE]
  hit <- dk %in% single$k
  lab[hit] <- single$aru.label[match(dk[hit], single$k)]

  ## several labels: by date
  need <- dk %in% multi
  if (any(need)) {
    if (any(c("date.start", "date.end") %in% ca$missing))
      stop(FN, ": aru.list has ARUs with more than one $aru.label (", paste(unique(al$aru.name[al$k %in% multi]), collapse = ", "),
           ") - it needs $date.start and $date.end to pick the label.", call. = FALSE)
    rd <- rep(as.Date(NA), nrow(data)); src <- NA_character_
    if ("date.monitoringnight" %in% names(data)) { rd <- sitevisit.to.date(data$date.monitoringnight); src <- "$date.monitoringnight" }
    if (any(need & is.na(rd)) && "date" %in% names(data)) {
      i <- need & is.na(rd); rd[i] <- sitevisit.to.date(data$date[i]); src <- paste(c(src[!is.na(src)], "$date"), collapse = " / ")
    }
    if (any(need & is.na(rd)) && "call.datetime" %in% names(data)) {
      i <- need & is.na(rd)
      ct <- as.character(data$call.datetime[i]); d0 <- sitevisit.to.date(substr(ct, 1, 10))
      hh <- suppressWarnings(as.numeric(sub("^\\S+\\s+([0-9]{1,2}):.*$", "\\1", ct)))
      d0[!is.na(hh) & hh < 12] <- d0[!is.na(hh) & hh < 12] - 1
      rd[i] <- d0; src <- paste(c(src[!is.na(src)], "$call.datetime"), collapse = " / ")
    }
    if (all(is.na(rd[need])))
      stop(FN, ": data needs $date.monitoringnight, $date or $call.datetime to pick the label of ARUs listed more than once (",
           paste(unique(data$aru.name[need]), collapse = ", "), ").", call. = FALSE)
    al$d0 <- sitevisit.to.date(al$date.start); al$d1 <- sitevisit.to.date(al$date.end)
    bad <- al$k %in% multi & (is.na(al$d0) | is.na(al$d1))         # full dates required
    if (any(bad)) stop(FN, ": aru.list rows of ARUs with more than one $aru.label need a full $date.start and $date.end ",
                       "(day, month and 4-digit year): ", paste(sprintf("%s %s (%s to %s)", al$aru.name[bad], al$aru.label[bad],
                       ifelse(is.na(al$date.start[bad]) | !nzchar(trimws(as.character(al$date.start[bad]))), "blank", as.character(al$date.start[bad])),
                       ifelse(is.na(al$date.end[bad]) | !nzchar(trimws(as.character(al$date.end[bad]))), "blank", as.character(al$date.end[bad]))),
                       collapse = "; "), call. = FALSE)
    for (k in multi) {
      w <- al[al$k == k, , drop = FALSE]
      i <- which(dk == k)
      if (!length(i)) next
      inside <- vapply(seq_len(nrow(w)), function(j) !is.na(rd[i]) & rd[i] >= w$d0[j] & rd[i] <= w$d1[j],
                       logical(length(i)))
      if (is.null(dim(inside))) inside <- matrix(inside, nrow = length(i))
      n.in <- rowSums(inside)
      first <- apply(inside, 1, function(v) if (any(v)) which(v)[1] else NA_integer_)
      lab[i] <- w$aru.label[first]
      nm <- w$aru.name[1]
      if (any(n.in > 1)) {
        dd <- sort(unique(rd[i][n.in > 1]))
        cat(sprintf("NOTE: %s - %s: %d record(s) fall in more than one aru.list window (%s) - the first row in aru.list used.\n",
                    FN, nm, sum(n.in > 1), paste(format(dd), collapse = ", ")))
      }
      nodate <- is.na(rd[i])
      if (any(nodate)) {
        raw <- if ("date.monitoringnight" %in% names(data)) as.character(data$date.monitoringnight[i][nodate]) else
          if ("date" %in% names(data)) as.character(data$date[i][nodate]) else as.character(data$call.datetime[i][nodate])
        raw[is.na(raw) | !nzchar(trimws(raw))] <- "blank"
        cat(sprintf("NOTE: %s - %s: %d record(s) without a full date (day, month, 4-digit year) - $aru.label NA. Values: %s\n",
                    FN, nm, sum(nodate), paste(unique(raw), collapse = ", ")))
      }
      if (any(n.in == 0 & !nodate)) {
        dd <- sort(unique(rd[i][n.in == 0 & !nodate]))
        cat(sprintf("NOTE: %s - %s: %d record(s) dated outside every $date.start - $date.end window - $aru.label NA. Dates: %s\n",
                    FN, nm, sum(n.in == 0 & !nodate), paste(format(dd), collapse = ", ")))
      }
    }
    cat(sprintf("%s: label of ARUs used at more than one location picked by date (%s).\n", FN, src))
  }

  blank <- is.na(dk) | !nzchar(dk)
  if (any(blank)) cat(sprintf("NOTE: %s - %d record(s) with a blank $aru.name - $aru.label NA.\n", FN, sum(blank)))
  nomatch <- !blank & !dk %in% al$k
  if (any(nomatch)) cat(sprintf("NOTE: %s - $aru.name not in aru.list (%d record(s)) - $aru.label NA: %s\n",
                                FN, sum(nomatch), paste(unique(as.character(data$aru.name[nomatch])), collapse = ", ")))

  if ("aru.label" %in% names(data)) {
    cat("NOTE: ", FN, " - data already had $aru.label - replaced.\n", sep = "")
    data$aru.label <- NULL
  }
  p <- match("aru.name", names(data))
  data <- cbind(data[seq_len(p)], aru.label = lab, data[-seq_len(p)], stringsAsFactors = FALSE)
  rownames(data) <- NULL
  s <- as.data.frame(table(aru.name = data$aru.name, aru.label = data$aru.label, useNA = "ifany"), stringsAsFactors = FALSE)
  s <- s[s$Freq > 0, , drop = FALSE]; names(s)[3] <- "records"
  print(s[order(s$aru.name, s$aru.label), ], row.names = FALSE)
  data
}

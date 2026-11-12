#' Summarize SM ARU system-check logs into a plot frame of operating minutes
#'
#' Summarizes the 60-second system checks of an SM-family ARU (the merged
#' activity logs from \code{\link{batz.merge_sm.logfiles}} or
#' \code{\link{batz.merge_sm4.logfiles}}) into one row per ARU and time
#' interval: how many minutes the microphone was connected and working, the
#' files and scrubbed files recorded, power and temperature. The result is
#' the \code{data} of \code{\link{batz.plotsm4_heatmap}}.
#'
#' @param data A data frame of system checks, one row per check, with
#'   \code{$aru.name}, \code{$date}, \code{$time}, \code{$mic0.type},
#'   \code{$power.v}, \code{$temperature.c}, \code{$files} and
#'   \code{$scrubbed} (headers matched ignoring case and "." / "_" /
#'   spaces), e.g. \code{SM4} from \code{batz.merge_sm.logfiles()}. The list
#'   returned by \code{batz.merge_sm.logfiles()} is also accepted: every
#'   element with these headers is used, the others are skipped with a NOTE
#'   (SM5 logs have no \code{$mic0.type}).
#' @param dir.save Character, default \code{getwd()}. Folder the plot frame
#'   CSV is written to (created if needed). \code{NULL} writes no file.
#' @param project.name Character, default \code{""}. First part of the file
#'   name: \verb{<project.name>_plotframe.sm4_<YYYYMMDDHHMMSS>.csv}
#'   (\verb{plotframe.sm4_<YYYYMMDDHHMMSS>.csv} when blank).
#' @param mic.type Character, default \code{c("U2")}. Microphone types that
#'   count as connected and working. A check whose \code{$mic0.type} is one
#'   of them (spaces removed, case ignored) is a working minute.
#' @param interval Numeric, default \code{15}. Length of each summary
#'   interval in minutes: 5 to 60, divisible by 5.
#' @param buffer Numeric, default \code{10}. Seconds a system check may be
#'   late: checks up to \code{60 + buffer} seconds apart count as one
#'   continuous run (see Details).
#' @param site.metadata Optional, default \code{NULL}. The site metadata from
#'   \code{\link{batz.merge_sitevisit.acoustics}}: the list it returns, or
#'   the path of a workbook it wrote. Its \code{deployment history} tab gives
#'   when and where each ARU was deployed and its \code{$aru.schedule}; its
#'   \code{aru.scheduals} tab is added to the schedules. Without it (or for
#'   an ARU not in it), each ARU runs from its first to its last system
#'   check, under \code{aru.schedule}. See Details, "Scheduled recording".
#' @param suntimes Optional, default \code{NULL}. Sun times for the ARUs, e.g.
#'   from \code{\link{batz.generate_suntimes.arulist}} run on the ARU list of
#'   \code{batz.merge_sitevisit.acoustics(generate.arulist = TRUE)}: a data
#'   frame or the path of its CSV. Needed for schedules that start or stop at
#'   sunset or sunrise; its \code{$time.zone} also gives the local time of
#'   the deployment history.
#' @param aru.schedule Character, default \code{"USFWS.compliance"}. The
#'   schedule used when \code{site.metadata} is absent, or has no schedule
#'   for an ARU: a \code{$aru.schedule} of the \code{aru.scheduals} tab of
#'   Batz_reference_db (or of \code{aru.scheduals_new}); any other name stops
#'   the function, listing the allowed ones. When not given in the call, the
#'   most likely schedule is worked out from the checks (Details).
#'   \code{""}: every interval between an ARU's first and last check with
#'   no system checks gets 0 (schedules are ignored).
#' @param aru.scheduals_new Optional, default \code{NULL}. Schedules not in
#'   Batz_reference_db: a data frame, or the path of a CSV, laid out like its
#'   \code{aru.scheduals} tab (at least \code{$aru.schedule},
#'   \code{$time.on}, \code{$time.off}, \code{$recording.interval}). A
#'   schedule with the name of one in the database replaces it.
#' @param schedule.buffer Numeric, default \code{5}. Minutes taken off each
#'   end of every scheduled recording period before intervals are given 0,
#'   so a unit whose own sunset/sunrise time differs by a few minutes from
#'   \code{suntimes} doesn't get 0s at the edges of each night.
#'
#' @return Invisibly, a data frame, one row per ARU and interval that has at
#'   least one system check, plus every scheduled interval with none (Details,
#'   "Scheduled recording"), ordered by \code{$aru.name} and time:
#'   \describe{
#'     \item{\code{$aru.name}}{As in \code{data}.}
#'     \item{\code{$date}}{Calendar date of the interval start,
#'       \code{YYYY-MM-DD}.}
#'     \item{\code{$time}}{Start of the interval, \code{HH:MM:SS} (e.g. a
#'       check at 12:02 is in the 12:00:00 interval).}
#'     \item{\code{$date.monitoringnight}}{The monitoring night, noon to
#'       noon: the date of the noon that starts it, \code{YYYY-MM-DD} (a
#'       check at 2026-07-02 04:05:33 is on the night of 2026-07-01).}
#'     \item{\code{$mins.oper}}{Minutes in the interval the microphone was
#'       connected and working (0 to \code{interval}), see Details. 0 for
#'       scheduled intervals with no system check.}
#'     \item{\code{$power.v}}{The interval's last \code{$power.v}.}
#'     \item{\code{$temperature.c}}{The interval's last
#'       \code{$temperature.c}.}
#'     \item{\code{$files}}{Sum of \code{$files}.}
#'     \item{\code{$scrubbed}}{Sum of \code{$scrubbed}.}
#'     \item{\code{$mins2.noon.mon}}{Minutes from the monitoring night's noon
#'       to the interval start (0 to 1439).}
#'     \item{\code{$mins2.midnight}}{Minutes from the start of the calendar
#'       day to the interval start (0 to 1439).}
#'     \item{\code{$aru.schedule}}{The recording schedule used for the
#'       ARU at that time (Details, "Scheduled recording"): the deployment
#'       history's (a change by \code{$aru.schedule_new} applies from that
#'       visit on), \code{aru.schedule}, or the most likely schedule worked
#'       out from the checks (a database name, or
#'       \code{"observed HH:MM-HH:MM"} / \code{"observed continuous"}). A
#'       row outside every deployment takes the schedule of the nearest one.
#'       Blank with \code{aru.schedule = ""}.}
#'   }
#'   The file written is named in a printed line.
#'
#' @details
#' \strong{System checks.} An SM ARU records a system check every 60
#' seconds: the connected microphone (\code{$mic0.type}), the files and
#' scrubbed files recorded since the last check, \code{$power.v} and
#' \code{$temperature.c}. A check due while a recording is being written is
#' made a few seconds late, so an interval can hold fewer checks than
#' minutes without any failure. Units set to record only at set times, or
#' from dusk to dawn, have breaks of several hours; these are not failures
#' and have no rows.
#'
#' \strong{Steps.}
#' \enumerate{
#'   \item Spaces are removed from \code{$mic0.type}. A check whose
#'     \code{$mic0.type} is in \code{mic.type} is a working check.
#'   \item Checks are put in order per ARU; exact duplicate checks (same ARU,
#'     date and time) are kept once (NOTE).
#'   \item Intervals start at noon (12:00:00) and every \code{interval}
#'     minutes after it, until the next noon (with an interval that doesn't
#'     divide 24 hours, the last one before noon is shorter).
#'   \item Each working check covers the time back to the previous check of
#'     the same ARU when that is at most \code{60 + buffer} seconds earlier,
#'     and the 60 seconds before it otherwise (the first check after a
#'     break). Covered time that crosses an interval start is split between
#'     the two intervals. \code{$mins.oper} is the interval's covered time in
#'     whole minutes (rounded), at most the interval length.
#' }
#' \strong{Scheduled recording.} Every interval an ARU should have been
#' recording but has no system checks gets a row with \code{$mins.oper = 0},
#' \code{$files} and \code{$scrubbed} 0, and \code{$power.v} /
#' \code{$temperature.c} blank. Intervals outside the schedule (e.g. daytime)
#' get no row.
#' \itemize{
#'   \item \strong{When and where.} With \code{site.metadata}, the
#'     deployment history (ARUs matched on \code{$aru.name} or \code{$site},
#'     ignoring case, spaces, "." and "_"): each deployment runs from its
#'     \code{"deployment"} row to the next \code{"retrieval"} visit; if the
#'     unit has not been recovered yet, the last service visit is the
#'     recovery date; with no visit after the deployment, the ARU's last
#'     system check (NOTE). Dates (UTC in Survey123 exports) are turned into
#'     local time with \code{suntimes} \code{$time.zone}. Without
#'     \code{site.metadata}, or for an ARU not in it: from the ARU's first to
#'     its last system check.
#'   \item \strong{Which schedule.} The deployment's \code{$aru.schedule}
#'     (changed by a visit's \code{$aru.schedule_new}); else
#'     \code{aru.schedule}; else, when \code{aru.schedule} is not given in the
#'     call, the most likely schedule: each schedule in \code{aru.scheduals}
#'     is compared with the intervals that have checks (share of intervals in
#'     common), and so is the unit's own nightly pattern (the median time its
#'     checks start and end each monitoring night, leaving out the first and
#'     last nights). A database schedule matching at least 0.9 (and within
#'     0.05 of the unit's own pattern) is used, else the unit's own pattern.
#'     A NOTE gives the schedule chosen and the matches. A sun-based schedule
#'     with no \code{suntimes} for the ARU can't be used; the unit's own
#'     pattern is used instead (NOTE).
#'   \item \strong{Recording times.} A schedule's rows (one per division)
#'     give \code{$time.on} / \code{$time.off} as \code{deployment} /
#'     \code{recovery}, \code{sunset} or \code{sunrise} plus or minus
#'     minutes (e.g. \code{sunset-30}; \code{suntimes} \code{$sunset} /
#'     \code{$sunrise} of each date), or \code{HH:MM}; a
#'     \code{$recording.interval} of \code{"XXonXXoff"} records XX minutes,
#'     then pauses XX minutes; any other value (e.g. \code{"constant"})
#'     records the whole period. All divisions together are the time the unit
#'     should be on. Each recording period is shortened by
#'     \code{schedule.buffer} minutes at each end.
#'   \item \code{aru.schedule = ""}: every interval from the first to the
#'     last check is scheduled.
#' }
#'
#' Dates are read as \code{YYYY-MM-DD}, \code{YYYY/MM/DD} or \code{MM/DD/YYYY};
#' times as \code{HH:MM:SS} or \code{HH:MM}, clock time as recorded (no time
#' zone change). Checks whose date or time can't be read are dropped (NOTE).
#'
#' @seealso \code{\link{batz.merge_sm.logfiles}},
#'   \code{\link{batz.plotsm4_heatmap}}
#'
#' @examples
#' \dontrun{
#' logs <- batz.merge_sm.logfiles(dir.logs, dir.sub = TRUE)
#' pf <- batz.generate_plotframe.sm4(logs$SM4, dir.save = wd.out, project.name = "Bristol NH")
#' batz.plotsm4_heatmap(data = pf, fig.list = fig.list, suntimes = suntimes, dir.save = wd.charts)
#' }
#'
#' @export
batz.generate_plotframe.sm4 <- function(data,
                                        dir.save = getwd(),
                                        project.name = "",
                                        mic.type = c("U2"),
                                        interval = 15,
                                        buffer = 10,
                                        site.metadata = NULL,
                                        suntimes = NULL,
                                        aru.schedule = "USFWS.compliance",
                                        aru.scheduals_new = NULL,
                                        schedule.buffer = 5) {
  FN <- "batz.generate_plotframe.sm4"
  REQUIRED <- c("aru.name", "date", "time", "mic0.type", "power.v", "temperature.c", "files", "scrubbed")

  ## ---- inputs ------------------------------------------------------------
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) || interval < 5 || interval > 60 ||
      interval %% 5 != 0)
    stop(FN, ": interval must be a number of minutes from 5 to 60, divisible by 5.", call. = FALSE)
  if (!is.numeric(buffer) || length(buffer) != 1 || is.na(buffer) || buffer < 0)
    stop(FN, ": buffer must be a number of seconds >= 0.", call. = FALSE)
  if (!is.numeric(schedule.buffer) || length(schedule.buffer) != 1 || is.na(schedule.buffer) || schedule.buffer < 0)
    stop(FN, ": schedule.buffer must be a number of minutes >= 0.", call. = FALSE)
  infer <- missing(aru.schedule)                          # not given: most likely schedule from the checks
  sched <- if (is.null(site.metadata)) NULL else plotframe.sm4.read.metadata(site.metadata, FN)
  sc.all <- plotframe.sm4.all.schedules(if (is.null(sched)) NULL else sched$sc, aru.scheduals_new, FN)
  if (length(aru.schedule) != 1 || is.na(aru.schedule)) aru.schedule <- ""
  aru.schedule <- trimws(as.character(aru.schedule))
  if (nzchar(aru.schedule) && !tolower(aru.schedule) %in% tolower(sc.all$aru.schedule))
    stop(FN, ': aru.schedule "', aru.schedule, '" is not an ARU schedule. Use "" or one of: ',
         paste(unique(sc.all$aru.schedule), collapse = ", "), " (or add it with aru.scheduals_new).", call. = FALSE)
  mic.type <- toupper(gsub("\\s+", "", as.character(mic.type)))
  mic.type <- mic.type[!is.na(mic.type) & nzchar(mic.type)]
  if (length(mic.type) == 0) stop(FN, ": mic.type is blank - give the microphone type(s), e.g. \"U2\".", call. = FALSE)

  if (!is.data.frame(data) && is.list(data)) {           # list from batz.merge_sm.logfiles()
    parts <- list()
    for (nm in names(data)) {
      x <- data[[nm]]
      if (!is.data.frame(x)) next
      ch <- canonicalize.headers(x, REQUIRED)
      if (length(ch$missing) > 0) {
        if (!grepl("^log", nm)) cat("NOTE: ", FN, " - '", nm, "' skipped: no ", paste0("$", ch$missing, collapse = ", "), "\n", sep = "")
        next
      }
      parts[[nm]] <- ch$df[, REQUIRED]
    }
    if (length(parts) == 0) stop(FN, ": no element of data has ", paste0("$", REQUIRED, collapse = ", "), call. = FALSE)
    data <- do.call(rbind, unname(parts))
  }
  if (!is.data.frame(data)) stop(FN, ": data must be a data frame of SM system checks.", call. = FALSE)
  ch <- canonicalize.headers(data, REQUIRED)
  if (length(ch$missing) > 0) stop(FN, ": data is missing ", paste0("$", ch$missing, collapse = ", "), call. = FALSE)
  d <- ch$df[, REQUIRED]
  if (nrow(d) == 0) stop(FN, ": data has no rows.", call. = FALSE)

  ## ---- clean -------------------------------------------------------------
  d$aru.name <- trimws(as.character(d$aru.name))
  d$mic0.type <- gsub("\\s+", "", as.character(d$mic0.type))
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  for (h in c("power.v", "temperature.c", "files", "scrubbed")) d[[h]] <- num(d[[h]])
  dt <- plotframe.sm4.datetime(d$date, d$time)
  bad <- is.na(dt) | is.na(d$aru.name) | !nzchar(d$aru.name)
  if (any(bad)) {
    cat("NOTE: ", FN, " - ", sum(bad), " check(s) with an unreadable date/time or no aru.name dropped.\n", sep = "")
    d <- d[!bad, , drop = FALSE]; dt <- dt[!bad]
  }
  if (nrow(d) == 0) stop(FN, ": no check has a readable date and time.", call. = FALSE)
  d$t <- as.numeric(dt)
  d <- d[order(d$aru.name, d$t), , drop = FALSE]
  dup <- duplicated(d[, c("aru.name", "t")])
  if (any(dup)) {
    cat("NOTE: ", FN, " - ", sum(dup), " duplicate check(s) (same aru.name, date and time) kept once.\n", sep = "")
    d <- d[!dup, , drop = FALSE]
  }
  d$working <- toupper(d$mic0.type) %in% mic.type
  d$working[is.na(d$mic0.type)] <- FALSE

  ## ---- intervals: noon-anchored -------------------------------------------
  I <- interval * 60
  day0 <- floor(d$t / 86400) * 86400                               # calendar midnight (clock time)
  sod <- d$t - day0                                                # seconds of the day
  noon <- ifelse(sod >= 43200, day0 + 43200, day0 - 43200)          # noon that starts the monitoring night
  bin.of <- function(tt, nn) nn + floor((tt - nn) / I) * I
  bin.end <- function(b, nn) pmin(b + I, nn + 86400)
  d$noon <- noon
  d$bin <- bin.of(d$t, noon)

  ## covered time of each working check: back to the previous check when <= 60 + buffer s, else 60 s
  prev.t <- c(NA, d$t[-nrow(d)])
  prev.t[c(TRUE, d$aru.name[-1] != d$aru.name[-nrow(d)])] <- NA
  gap <- d$t - prev.t
  cov <- ifelse(!is.na(gap) & gap <= 60 + buffer, gap, 60)
  cov[!d$working] <- 0
  start <- d$t - cov
  ## part inside the check's own interval, and the part before its start (previous interval)
  inside <- pmin(cov, d$t - d$bin)
  before <- cov - inside
  prev.noon <- ifelse(start < d$noon, d$noon - 86400, d$noon)
  prev.bin <- bin.of(d$bin - 1, prev.noon)
  sec <- rbind(data.frame(aru.name = d$aru.name, bin = d$bin, noon = d$noon, s = inside),
               data.frame(aru.name = d$aru.name[before > 0], bin = prev.bin[before > 0],
                          noon = prev.noon[before > 0], s = before[before > 0]))
  key <- function(a, b) paste(a, b, sep = "|")
  covered <- tapply(sec$s, key(sec$aru.name, sec$bin), sum)

  ## ---- summarize per ARU and interval --------------------------------------
  k <- key(d$aru.name, d$bin)
  f <- factor(k, levels = unique(k))
  last <- function(v) vapply(split(v, f), function(x) { x <- x[!is.na(x)]; if (length(x)) x[length(x)] else NA_real_ }, numeric(1))
  total <- function(v) vapply(split(v, f), function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE), numeric(1))
  first <- !duplicated(k)
  b <- d$bin[first]; nn <- d$noon[first]
  len.min <- (bin.end(b, nn) - b) / 60
  oper <- covered[levels(f)]; oper[is.na(oper)] <- 0
  fmt <- function(x, f2) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), f2, tz = "UTC")
  out <- data.frame(
    aru.name = d$aru.name[first],
    date = fmt(b, "%Y-%m-%d"),
    time = fmt(b, "%H:%M:%S"),
    date.monitoringnight = fmt(nn, "%Y-%m-%d"),
    mins.oper = pmin(round(as.numeric(oper) / 60), len.min),
    power.v = unname(last(d$power.v)),
    temperature.c = unname(last(d$temperature.c)),
    files = unname(total(d$files)),
    scrubbed = unname(total(d$scrubbed)),
    mins2.noon.mon = (b - nn) / 60,
    mins2.midnight = (b - floor(b / 86400) * 86400) / 60,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL

  n.part <- sum(out$mins.oper < len.min)
  n.checked <- nrow(out)

  ## ---- scheduled intervals with no checks: mins.oper = 0 --------------------
  {
    exp <- plotframe.sm4.expected(sched, sc.all, suntimes, d, I, schedule.buffer * 60, aru.schedule, infer, FN)
    if (nrow(exp) > 0) {
      ak <- function(x) gsub("[^a-z0-9]", "", tolower(x))
      log.name <- d$aru.name[match(ak(exp$aru), ak(d$aru.name))]          # the log's spelling
      exp$aru.name <- ifelse(is.na(log.name), exp$aru, log.name)
      have <- paste(ak(out$aru.name), out$date, out$time)
      e.date <- fmt(exp$bin, "%Y-%m-%d"); e.time <- fmt(exp$bin, "%H:%M:%S")
      new <- !paste(ak(exp$aru.name), e.date, e.time) %in% have & !duplicated(paste(ak(exp$aru.name), exp$bin))
      if (any(new)) {
        e <- exp[new, , drop = FALSE]
        zero <- data.frame(aru.name = e$aru.name, date = e.date[new], time = e.time[new],
                           date.monitoringnight = fmt(e$noon, "%Y-%m-%d"), mins.oper = 0,
                           power.v = NA_real_, temperature.c = NA_real_, files = 0, scrubbed = 0,
                           mins2.noon.mon = (e$bin - e$noon) / 60,
                           mins2.midnight = (e$bin - floor(e$bin / 86400) * 86400) / 60, stringsAsFactors = FALSE)
        out <- rbind(out, zero)
        ord.t <- as.numeric(as.POSIXct(paste(out$date, out$time), tz = "UTC"))
        out <- out[order(out$aru.name, ord.t), , drop = FALSE]
        rownames(out) <- NULL
      }
      cat(sprintf("%s: %d scheduled interval(s) with no system checks added with mins.oper = 0.\n", FN, sum(new)))
    }
  }

  ## ---- aru.schedule of each row ---------------------------------------------
  ord.b <- as.numeric(as.POSIXct(paste(out$date, out$time), tz = "UTC"))
  out$aru.schedule <- plotframe.sm4.row.schedule(out$aru.name, ord.b, attr(exp, "segments"))

  cat(sprintf("%s: %d check(s) from %d ARU(s) -> %d interval(s) of %d min (mic.type %s, buffer %s s); %d interval(s) with mins.oper < interval length.\n",
              FN, nrow(d), length(unique(d$aru.name)), n.checked, interval, paste(mic.type, collapse = ", "), buffer,
              n.part))

  ## ---- save --------------------------------------------------------------
  if (!is.null(dir.save) && length(dir.save) && !is.na(dir.save[1]) && nzchar(dir.save[1])) {
    if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    pj <- if (is.null(project.name) || !length(project.name) || is.na(project.name[1]) || !nzchar(trimws(project.name[1]))) "" else
      paste0(gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(project.name[1]))), "_")
    path <- file.path(dir.save, paste0(pj, "plotframe.sm4_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv"))
    utils::write.csv(out, path, row.names = FALSE)
    cat("Saved:", path, "\n")
  }
  invisible(out)
}


#' Date + time text to POSIXct clock time (UTC, no zone change) (internal)
#' @keywords internal
#' @noRd
plotframe.sm4.datetime <- function(date, time) {
  dte <- trimws(as.character(date)); tme <- trimws(as.character(time))
  dd <- rep(as.Date(NA), length(dte))
  for (fm in c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y")) {
    todo <- is.na(dd) & !is.na(dte)
    if (any(todo)) dd[todo] <- as.Date(dte[todo], format = fm, optional = TRUE)
  }
  tme[grepl("^\\d{1,2}:\\d{2}$", tme)] <- paste0(tme[grepl("^\\d{1,2}:\\d{2}$", tme)], ":00")
  ok <- !is.na(dd) & grepl("^\\d{1,2}:\\d{2}:\\d{2}", tme)
  out <- rep(as.POSIXct(NA, tz = "UTC"), length(dte))
  out[ok] <- as.POSIXct(paste(format(dd[ok]), substr(tme[ok], 1, 8)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out
}


#' Deployment history and schedules from batz.merge_sitevisit.acoustics output (internal)
#' @keywords internal
#' @noRd
plotframe.sm4.read.metadata <- function(x, FN) {
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) stop(FN, ": site.metadata file not found: ", x, call. = FALSE)
    sh <- readxl::excel_sheets(x)
    pick <- function(nm) { j <- match(gsub("[^a-z]", "", tolower(nm)), gsub("[^a-z]", "", tolower(sh)))
      if (is.na(j)) NULL else as.data.frame(readxl::read_excel(x, sh[j]), stringsAsFactors = FALSE) }
    dh <- pick("deployment history"); sc <- pick("aru.scheduals")
  } else if (is.list(x)) {
    dh <- x[["deployment.history"]]; if (is.null(dh)) dh <- x[["deployment history"]]
    sc <- x[["aru.scheduals"]]
  } else stop(FN, ": site.metadata must be the list from batz.merge_sitevisit.acoustics() or the path of its workbook.", call. = FALSE)
  if (is.null(dh) || !nrow(dh)) stop(FN, ": site.metadata has no deployment history - re-run batz.merge_sitevisit.acoustics().", call. = FALSE)
  if (is.null(sc)) sc <- data.frame()
  need <- c("aru.name", "date", "visit.reason", "aru.schedule")
  miss <- setdiff(need, names(dh))
  if (length(miss)) stop(FN, ": deployment history is missing ", paste0("$", miss, collapse = ", "), call. = FALSE)
  list(dh = dh, sc = plotframe.sm4.clean.schedules(sc))
}


#' Clock time (seconds, UTC-coded local clock) of a date-time in time zone tz (internal)
#' @keywords internal
#' @noRd
plotframe.sm4.local <- function(v, tz) {
  if (!inherits(v, "POSIXct")) {
    v <- trimws(as.character(v)); p <- rep(as.POSIXct(NA, tz = "UTC"), length(v))
    for (fm in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%Y-%m-%d", "%m/%d/%Y")) {
      todo <- is.na(p) & !is.na(v) & nzchar(v)
      if (any(todo)) p[todo] <- as.POSIXct(strptime(v[todo], fm, tz = "UTC"))
    }
    v <- p
  }
  if (is.na(tz) || !nzchar(tz)) return(as.numeric(v))
  as.numeric(as.POSIXct(format(v, "%Y-%m-%d %H:%M:%S", tz = tz), tz = "UTC"))
}


#' Schedule table as text, frequency spelled "frequencey" (internal)
#' @keywords internal
#' @noRd
plotframe.sm4.clean.schedules <- function(sc) {
  if (is.null(sc) || !nrow(sc)) return(data.frame(aru.schedule = character(0), time.on = character(0), time.off = character(0),
                                                  recording.interval = character(0), stringsAsFactors = FALSE))
  sc <- as.data.frame(sc, stringsAsFactors = FALSE)
  for (h in names(sc)) { v <- trimws(as.character(sc[[h]])); v[is.na(v)] <- ""; sc[[h]] <- v }
  if (!"frequencey" %in% names(sc) && "frequency" %in% names(sc)) names(sc)[names(sc) == "frequency"] <- "frequencey"
  for (h in setdiff(c("aru.schedule", "time.on", "time.off", "recording.interval"), names(sc))) sc[[h]] <- rep("", nrow(sc))
  sc[nzchar(sc$aru.schedule), , drop = FALSE]
}


#' All schedules: package aru.scheduals, then site.metadata's, then aru.scheduals_new (internal)
#'
#' A schedule name given later replaces the earlier rows with that name.
#' @keywords internal
#' @noRd
plotframe.sm4.all.schedules <- function(meta.sc, new, FN) {
  db <- plotframe.sm4.clean.schedules(tryCatch(batz.db("aru.scheduals"), error = function(e) NULL))
  if (!is.null(new)) {
    if (is.character(new) && length(new) == 1) {
      if (!file.exists(new)) stop(FN, ": aru.scheduals_new file not found: ", new, call. = FALSE)
      new <- special.read.csv(new, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
    }
    if (!is.data.frame(new)) stop(FN, ": aru.scheduals_new must be a data frame or the path of a CSV laid out like the aru.scheduals tab.", call. = FALSE)
    ch <- canonicalize.headers(new, c("aru.schedule", "time.on", "time.off", "recording.interval"))
    if (length(ch$missing)) stop(FN, ": aru.scheduals_new is missing ", paste0("$", ch$missing, collapse = ", "),
                                 " (lay it out like the aru.scheduals tab of Batz_reference_db).", call. = FALSE)
    new <- plotframe.sm4.clean.schedules(ch$df)
  }
  out <- db
  for (add in list(meta.sc, new)) {
    if (is.null(add) || !nrow(add)) next
    nm <- unique(tolower(add$aru.schedule))
    keep <- out[!tolower(out$aru.schedule) %in% nm, , drop = FALSE]
    cols <- union(names(keep), names(add))
    for (h in setdiff(cols, names(keep))) keep[[h]] <- rep("", nrow(keep))
    for (h in setdiff(cols, names(add))) add[[h]] <- rep("", nrow(add))
    out <- rbind(keep[, cols, drop = FALSE], add[, cols, drop = FALSE])
  }
  rownames(out) <- NULL
  out
}


#' Scheduled recording time per ARU and interval (internal)
#'
#' Periods: deployments from the deployment history (when given and the ARU
#' is in it), else the ARU's first to last system check. Schedule: the
#' deployment history's, else aru.schedule (given), else the most likely
#' schedule found from the checks (inferred). aru.schedule = "" : every
#' minute between the first and last check is scheduled.
#' Returns $aru, $bin, $noon, $secs (UTC-coded local clock seconds).
#' @keywords internal
#' @noRd
plotframe.sm4.expected <- function(sched, sc.all, suntimes, d, I, trim, aru.schedule, infer, FN) {
  ak <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))   # case, spaces, "." and "_" ignored
  dh <- if (is.null(sched)) NULL else sched$dh
  empty <- data.frame(aru = character(0), bin = numeric(0), noon = numeric(0), secs = numeric(0))
  ## suntimes, as clock seconds per ARU and date
  st <- NULL
  if (!is.null(suntimes)) {
    if (is.character(suntimes) && length(suntimes) == 1) suntimes <- special.read.csv(suntimes, check.names = FALSE, stringsAsFactors = FALSE)
    ch <- canonicalize.headers(suntimes, c("aru.name", "date", "time.zone", "sunset", "sunrise"))
    if (length(ch$missing)) stop(FN, ": suntimes is missing ", paste0("$", ch$missing, collapse = ", "), call. = FALSE)
    st <- ch$df
    st$k <- ak(st$aru.name)
    st$day <- as.character(sitevisit.to.date(st$date))
    tzv <- trimws(as.character(st$time.zone))
    unix.or <- function(h) {
      u <- if (paste0(h, ".unix") %in% names(st)) suppressWarnings(as.numeric(st[[paste0(h, ".unix")]])) else rep(NA_real_, nrow(st))
      out <- rep(NA_real_, nrow(st))
      for (z in unique(tzv)) { i <- which(tzv == z)
        out[i] <- ifelse(!is.na(u[i]), as.numeric(as.POSIXct(format(as.POSIXct(u[i], origin = "1970-01-01", tz = z), "%Y-%m-%d %H:%M:%S"), tz = "UTC")),
                         plotframe.sm4.local(as.character(st[[h]][i]), "")) }
      out
    }
    st$sunset.s <- unix.or("sunset"); st$sunrise.s <- unix.or("sunrise")
  }
  tz.of <- function(k) if (!is.null(st) && any(st$k == k)) st$time.zone[st$k == k][1] else ""
  first.check <- tapply(d$t, ak(d$aru.name), min); last.check <- tapply(d$t, ak(d$aru.name), max)
  noon.of <- function(t) { d0 <- floor(t / 86400) * 86400; ifelse(t - d0 >= 43200, d0 + 43200, d0 - 43200) }

  ## ---- recording windows of a set of schedule divisions over [from, to] ----
  parse.at <- function(x) {
    x <- gsub("\\s+", "", tolower(x))
    if (x %in% c("deployment", "recovery")) return(list(type = x, off = 0))
    m <- regmatches(x, regexec("^(sunset|sunrise)([+-][0-9]+)?$", x))[[1]]
    if (length(m)) return(list(type = m[2], off = if (nzchar(m[3])) as.numeric(m[3]) * 60 else 0))
    m <- regmatches(x, regexec("^([0-9]{1,2}):([0-9]{2})$", x))[[1]]
    if (length(m)) return(list(type = "clock", off = as.numeric(m[2]) * 3600 + as.numeric(m[3]) * 60))
    NULL
  }
  needs.sun <- function(rows) any(grepl("sun", tolower(c(rows$time.on, rows$time.off))))
  windows <- function(rows, from, to, k, label, quiet = FALSE) {
    label.aru <- if (k %in% ak(d$aru.name)) d$aru.name[ak(d$aru.name) == k][1] else k
    days <- seq(as.Date(as.POSIXct(from - 86400, origin = "1970-01-01", tz = "UTC")),
                as.Date(as.POSIXct(to + 86400, origin = "1970-01-01", tz = "UTC")), by = "day")
    sk <- if (is.null(st)) NULL else st[st$k == k, , drop = FALSE]
    if (needs.sun(rows) && (is.null(sk) || !nrow(sk))) {
      if (!quiet) cat("NOTE: ", FN, " - no suntimes for ", label.aru, " - sun-based schedule '", label, "' can't be used.\n", sep = "")
      return(NULL)
    }
    anchor <- function(a, dy) {
      if (a$type == "clock") return(as.numeric(as.POSIXct(paste(dy, "00:00:00"), tz = "UTC")) + a$off)
      sk[[paste0(a$type, ".s")]][match(as.character(dy), sk$day)] + a$off
    }
    w <- list()
    for (r in seq_len(nrow(rows))) {
      on <- parse.at(rows$time.on[r]); off <- parse.at(rows$time.off[r])
      if (is.null(on) || is.null(off)) {
        if (!quiet) cat("NOTE: ", FN, " - aru.schedule '", label, "': time.on/time.off '", rows$time.on[r], "'/'",
                        rows$time.off[r], "' not readable - division skipped.\n", sep = "")
        next
      }
      ons <- if (on$type == "deployment") from else vapply(days, function(dy) anchor(on, dy), numeric(1))
      for (a in ons[!is.na(ons)]) {
        if (off$type == "recovery") b <- to else {
          cand <- vapply(c(days, max(days) + 1), function(dy) anchor(off, dy), numeric(1))
          cand <- cand[!is.na(cand) & cand > a]
          if (!length(cand)) next
          b <- min(cand)
        }
        a2 <- max(a, from); b2 <- min(b, to)
        if (b2 <= a2) next
        m <- regmatches(gsub("\\s+", "", tolower(rows$recording.interval[r])),
                        regexec("^([0-9]+)on([0-9]+)off$", gsub("\\s+", "", tolower(rows$recording.interval[r]))))[[1]]
        if (length(m)) {                                        # XXonXXoff blocks from the start of the period
          onl <- as.numeric(m[2]) * 60; offl <- as.numeric(m[3]) * 60
          for (x in seq(a, b, by = onl + offl)) { s1 <- max(x, a2); s2 <- min(x + onl, b2); if (s2 > s1) w[[length(w) + 1]] <- c(s1, s2) }
        } else w[[length(w) + 1]] <- c(a2, b2)
      }
    }
    if (!length(w)) return(NULL)
    do.call(rbind, w)
  }
  ## windows -> scheduled seconds per noon-anchored bin
  to.bins <- function(w, aru) {
    if (is.null(w) || !nrow(w)) return(empty)
    out <- list()
    for (i in seq_len(nrow(w))) {
      t <- w[i, 1]
      while (t < w[i, 2]) {
        nn <- noon.of(t); bn <- nn + floor((t - nn) / I) * I; be <- min(bn + I, nn + 86400)
        e <- min(be, w[i, 2]); out[[length(out) + 1]] <- c(bn, nn, e - t); t <- e
      }
    }
    o <- as.data.frame(do.call(rbind, out)); names(o) <- c("bin", "noon", "secs")
    o <- stats::aggregate(secs ~ bin + noon, data = o, FUN = sum)
    data.frame(aru = rep(aru, nrow(o)), o, stringsAsFactors = FALSE)
  }
  trimw <- function(w) { if (is.null(w)) return(NULL); w[, 1] <- w[, 1] + trim; w[, 2] <- w[, 2] - trim; w[w[, 2] > w[, 1], , drop = FALSE] }
  rows.of <- function(name) sc.all[tolower(sc.all$aru.schedule) == tolower(name), , drop = FALSE]

  ## ---- most likely schedule from the checks (no site.metadata, no aru.schedule) ----
  infer.rows <- function(k, from, to, disp = k) {
    x <- d[ak(d$aru.name) == k & d$working, , drop = FALSE]
    obs <- unique(x$bin)
    score <- function(w) {
      b <- to.bins(w, k); b <- b$bin[b$secs >= 60]
      if (!length(b) || !length(obs)) return(0)
      length(intersect(b, obs)) / length(union(b, obs))
    }
    cand <- unique(sc.all$aru.schedule)
    sc.score <- vapply(cand, function(nm) score(windows(rows.of(nm), from, to, k, nm, quiet = TRUE)), numeric(1))
    ## the unit's own nightly pattern: median start and end of its recording each monitoring night
    nights <- split(x$t, noon.of(x$t))
    if (length(nights) > 2) nights <- nights[-c(1, length(nights))]          # drop the deployment / recovery nights
    st0 <- stats::median(vapply(nights, function(v) min(v) - noon.of(min(v)), numeric(1)))
    en0 <- stats::median(vapply(nights, function(v) max(v) - noon.of(min(v)), numeric(1)))
    hm <- function(s) { m <- round(((s + 43200) %% 86400) / 60) %% 1440; sprintf("%02d:%02d", m %/% 60, m %% 60) }
    own <- data.frame(aru.schedule = sprintf("observed %s-%s", hm(st0), hm(en0)), time.on = hm(st0), time.off = hm(en0),
                      recording.interval = "constant", stringsAsFactors = FALSE)
    if (en0 - st0 >= 86400 - 3600) own[1, c("time.on", "time.off", "aru.schedule")] <- c("deployment", "recovery", "observed continuous")
    own.score <- score(windows(own, from, to, k, own$aru.schedule, quiet = TRUE))
    best <- if (length(sc.score)) names(sc.score)[which.max(sc.score)] else NA
    use.db <- !is.na(best) && sc.score[[best]] >= 0.9 && sc.score[[best]] >= own.score - 0.05
    pick <- if (use.db) best else own$aru.schedule
    top <- sort(c(sc.score, stats::setNames(own.score, own$aru.schedule)), decreasing = TRUE)[1:min(3, length(sc.score) + 1)]
    cat(sprintf("NOTE: %s - %s: no schedule given - most likely schedule from the checks: %s (match %s; others: %s).\n",
                FN, disp, pick, round(top[pick], 2),
                paste(sprintf("%s %.2f", names(top)[names(top) != pick], top[names(top) != pick]), collapse = ", ")))
    if (use.db) rows.of(best) else own
  }

  fallback.rows <- function(k, from, to, disp) {
    if (!infer) return(rows.of(aru.schedule))
    infer.rows(k, from, to, disp)
  }

  res <- list()
  seg.tab <- list()                                        # schedule used per ARU and period: $aru, $schedule, $from, $to
  add.seg <- function(aru, schedule, from, to) seg.tab[[length(seg.tab) + 1]] <<- data.frame(aru = aru, schedule = schedule,
                                                                                             from = from, to = to, stringsAsFactors = FALSE)
  aru.all <- unique(c(ak(d$aru.name), if (!is.null(dh)) ifelse(nzchar(ak(dh$aru.name)), ak(dh$aru.name), ak(dh$site))))
  aru.all <- aru.all[nzchar(aru.all)]
  for (k in aru.all) {
    in.log <- k %in% names(first.check)
    disp <- if (in.log) d$aru.name[ak(d$aru.name) == k][1] else k
    ## aru.schedule = "": every minute between the first and last check
    if (!infer && !nzchar(aru.schedule)) {
      if (in.log) res[[length(res) + 1]] <- to.bins(trimw(cbind(first.check[[k]], last.check[[k]])), disp)
      if (in.log) add.seg(disp, "", first.check[[k]], last.check[[k]])
      next
    }
    ## periods and schedules: deployment history, else first to last check
    segs <- list()
    x <- if (is.null(dh)) NULL else dh[ifelse(nzchar(ak(dh$aru.name)), ak(dh$aru.name), ak(dh$site)) == k, , drop = FALSE]
    if (!is.null(x) && nrow(x)) {
      tz <- tz.of(k)
      if (!nzchar(tz)) cat("NOTE: ", FN, " - no suntimes $time.zone for ", x$aru.name[1], " - deployment history dates used as written.\n", sep = "")
      x$s <- plotframe.sm4.local(x$date, tz)
      x <- x[order(x$s), , drop = FALSE]
      vr <- tolower(as.character(x$visit.reason))
      dep <- which(grepl("deploy", vr) & !is.na(x$s))
      if (in.log && !length(dep)) cat("NOTE: ", FN, " - ", disp, ": no deployment in the deployment history - first to last check used.\n", sep = "")
      if (!in.log && length(dep)) cat("NOTE: ", FN, " - ", x$aru.name[1], " is in the deployment history but not in data - its scheduled intervals are all 0.\n", sep = "")
      for (j in seq_along(dep)) {
        from <- x$s[dep[j]]
        nxt <- if (j < length(dep)) x$s[dep[j + 1]] else Inf
        later <- which(x$s > from & x$s <= nxt & !grepl("deploy", vr))
        ret <- later[grepl("retriev", vr[later])]
        to <- if (length(ret)) x$s[ret[1]] else if (length(later)) x$s[max(later)] else NA
        if (is.na(to)) {
          to <- if (in.log) last.check[[k]] else NA
          cat("NOTE: ", FN, " - ", disp, ": no visit after the deployment of ",
              format(as.POSIXct(from, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
              " - its last system check is used as the recovery date.\n", sep = "")
          if (is.na(to)) next
        } else if (!length(ret)) cat("NOTE: ", FN, " - ", disp, " not recovered yet - last service visit (",
                                     format(as.POSIXct(to, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), ") used as the recovery date.\n", sep = "")
        cur <- trimws(as.character(x$aru.schedule[dep[j]])); cur[is.na(cur)] <- ""; seg.from <- from
        if ("aru.schedule_new" %in% names(x)) for (v in later[x$s[later] < to]) {
          nw <- trimws(as.character(x$aru.schedule_new[v]))
          if (!is.na(nw) && nzchar(nw) && nw != cur) { segs[[length(segs) + 1]] <- list(cur, seg.from, x$s[v]); cur <- nw; seg.from <- x$s[v] }
        }
        segs[[length(segs) + 1]] <- list(cur, seg.from, to)
      }
    }
    if (!length(segs)) {
      if (!in.log) next
      segs <- list(list("", first.check[[k]], last.check[[k]]))
    }
    for (sg in segs) {
      rows <- if (nzchar(sg[[1]])) rows.of(sg[[1]]) else NULL
      if (nzchar(sg[[1]]) && !nrow(rows))
        cat("NOTE: ", FN, " - ", disp, ": aru.schedule '", sg[[1]], "' is not in aru.scheduals (add it with aru.scheduals_new).\n", sep = "")
      if (is.null(rows) || !nrow(rows)) rows <- fallback.rows(k, sg[[2]], sg[[3]], disp)
      label <- if (nrow(rows)) rows$aru.schedule[1] else ""
      w <- windows(rows, sg[[2]], sg[[3]], k, label)
      if (is.null(w) && nrow(rows) && needs.sun(rows)) {
        rows <- infer.rows(k, sg[[2]], sg[[3]], disp); label <- if (nrow(rows)) rows$aru.schedule[1] else ""
        w <- windows(rows, sg[[2]], sg[[3]], k, label)
      }
      add.seg(disp, label, sg[[2]], sg[[3]])
      res[[length(res) + 1]] <- to.bins(trimw(w), disp)
    }
  }
  segs.out <- if (length(seg.tab)) do.call(rbind, seg.tab) else
    data.frame(aru = character(0), schedule = character(0), from = numeric(0), to = numeric(0))
  o <- if (length(res)) do.call(rbind, res) else empty
  if (!nrow(o)) return(structure(empty, segments = segs.out))
  o <- stats::aggregate(secs ~ aru + bin + noon, data = o, FUN = sum)
  structure(o[o$secs >= 60, c("aru", "bin", "noon", "secs"), drop = FALSE], segments = segs.out)
}


#' aru.schedule of each plot-frame row (internal)
#'
#' The schedule of the ARU's period (deployment) holding the row's interval;
#' a row outside every period takes the nearest period's schedule. Blank
#' when the ARU has no period (or aru.schedule = "").
#' @keywords internal
#' @noRd
plotframe.sm4.row.schedule <- function(aru, bin, segs) {
  ak <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))
  out <- rep("", length(aru))
  if (is.null(segs) || !nrow(segs)) return(out)
  sk <- ak(segs$aru)
  for (a in unique(ak(aru))) {
    i <- which(ak(aru) == a); g <- segs[sk == a, , drop = FALSE]
    if (!nrow(g)) next
    out[i] <- vapply(bin[i], function(b) {
      hit <- which(b >= g$from & b < g$to)
      if (length(hit)) return(g$schedule[hit[length(hit)]])
      g$schedule[which.min(pmin(abs(b - g$from), abs(b - g$to)))]
    }, character(1))
  }
  out
}

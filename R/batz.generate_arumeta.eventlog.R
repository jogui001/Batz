#' Build a combined deployment/service/recovery event log for ARUs
#'
#' Searches a directory (and, optionally, its subdirectories) for ARU
#' deployment-form and service-visit-form exports, classifies each file as a
#' deployment or a service record, lines up its columns against a fixed
#' header list, and combines everything into one chronological event log per
#' site: every deployment, service visit, recovery and equipment swap.
#' Optionally returns a per-file log and a per-site summary.
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard (glob) patterns, converted
#'   to a regular expression with \code{utils::glob2rx()} and matched
#'   case-insensitively. Default
#'   \code{c("*HabitatAssessments_20m.csv", "*Acoustic_SiteVisitARU.csv")}
#'   (deployment form, service-visit form).
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from \code{aru.eventlog}.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and assign) \code{aru.eventlog.filelog} and
#'   \code{aru.eventlog.sitelog} (see Value).
#' @param max.missing Integer, default \code{5}. The largest number of
#'   expected headers a file may be missing and still be merged; missing
#'   columns are filled with \code{NA}. A file missing more is not merged
#'   and is logged as \code{"failed"}.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols are simplified (\code{é} -> \code{e},
#'   \code{×} -> \code{X}, curly quotes -> straight quotes) and other
#'   non-ASCII characters (e.g. \code{°}, \code{µ}, \code{™}) are removed,
#'   in column names and data, right after each file is read. A changed
#'   column is re-typed, so \code{"42.5°"} becomes the number \code{42.5}.
#'   Files are always read as UTF-8 with a Latin-1 fallback, whatever this
#'   is set to.
#'
#' @return Invisibly, a named list: \code{aru.eventlog} (always), plus
#'   \code{aru.eventlog.filelog} and \code{aru.eventlog.sitelog} when
#'   \code{log.file = TRUE}. Each element is also assigned into the calling
#'   environment, so a bare call creates them in your workspace.
#'   \describe{
#'     \item{aru.eventlog}{One row per event, sorted by
#'       \code{$site.unified}, \code{$date.time}, then event type. Columns
#'       are the deployment and service headers (see Details; \code{NA} where
#'       a column belongs to the other form) plus \code{$event.type},
#'       \code{$site.unified} (\code{$site.name} for deployments,
#'       \code{$site_name} for service visits) and \code{$date.time}
#'       (character, date + time). An empty \code{data.frame()} if nothing
#'       loaded.}
#'     \item{aru.eventlog.filelog}{One row per input file: \code{$filename},
#'       \code{$file.type} (\code{"deployment"} or \code{"service"}),
#'       \code{$status} (\code{"success"}, \code{"missing"},
#'       \code{"duplicate"} or \code{"failed"}), \code{$notes}, then the
#'       special-character columns for that file.}
#'     \item{aru.eventlog.sitelog}{One row per site: \code{$client},
#'       \code{$project}, \code{$project.code}, \code{$site.name},
#'       \code{$deployment}, \code{$service.count}, \code{$recovery},
#'       \code{$duplicates.removed}, then the special-character columns
#'       (for all files read in the call).}
#'   }
#'   The special-character columns are \code{$strip.special}
#'   (\code{"FALSE"}, \code{"TRUE NONE"} or \code{"TRUE ; <header>; ..."}),
#'   \code{$accented.letters.header}, \code{$removed.symbols.header},
#'   \code{$accented.letters.data} and \code{$removed.symbols.data} (number
#'   of unique column names / data values with a character simplified or
#'   removed; \code{NA} when \code{strip.special = FALSE}).
#'
#' @details
#' \strong{Headers.} Every file's headers are standardized with
#' \code{standardize.headers()} (trimmed, lower case, runs of punctuation or
#' spaces become \code{"_"}) and matched exactly to the expected form fields.
#' Matching is not typo tolerant: a misspelled header (e.g. \code{"Clien"})
#' counts as missing. Matched fields are renamed:
#' \itemize{
#'   \item Deployment form: \code{$client}, \code{$project},
#'     \code{$project.code}, \code{$date.deployment}, \code{$aru.model},
#'     \code{$aru.make}, \code{$microphone.model}, \code{$microphone.make},
#'     \code{$site.name}, \code{$survey_type}, \code{$longitude} (X),
#'     \code{$latitude} (Y), \code{$aru.serial} (Serial Number of Detector),
#'     \code{$microphone.serial}, \code{$personnel},
#'     \code{$date.habitat.assessment}.
#'   \item Service form: \code{$client}, \code{$project},
#'     \code{$project.code}, \code{$date}, \code{$site_name},
#'     \code{$reason.sitevisit}, \code{$personnel}, \code{$notes},
#'     \code{$aru.serial}, \code{$microphone.serial}, \code{$power.serial}
#'     (Power Kit/Solar Serial Number), \code{$hobo.serial},
#'     \code{$select_all_actions_performed}, \code{$microphone.serial_new},
#'     \code{$aru.serial_new}.
#' }
#' Other columns are dropped.
#'
#' \strong{Deployment or service.} A file with a \code{Date of Deployment}
#' column is a deployment record; any other file is a service record. The
#' test uses the content, not the file name.
#'
#' \strong{Repeated columns.} If a file has the same column more than once,
#' the copies are merged: the non-blank value is kept per row, and
#' different non-blank values are joined with \code{"; "}. The file's
#' \code{$status} is then \code{"duplicate"} and \code{$notes} lists the
#' columns. Missing columns give \code{"missing"} and are listed in
#' \code{$notes}.
#'
#' \strong{Events.}
#' \itemize{
#'   \item Each deployment row is one \code{"deployment"} event.
#'   \item Each service row's \code{$select_all_actions_performed} is split
#'     on commas, and each action (e.g. \code{status_check},
#'     \code{download}, \code{aru_swap}, \code{mic_swap}, \code{recovery})
#'     becomes its own event row. A visit with no action becomes a
#'     \code{"service"} event.
#'   \item Events at the same site and time are ordered
#'     \code{deployment, status_check, download, aru_swap, mic_swap,
#'     recovery, service}, then any other action.
#' }
#'
#' \strong{Date and time.} \code{$date.time} is the date
#' (\code{$date.deployment} or \code{$date}) plus the time. If the date
#' already contains a time it is used as is; when there is no time,
#' 12:00:00 is used.
#'
#' \strong{Grouping by site.} Sorting and the site summary use
#' \code{$site.unified}, not \code{$aru.serial}; \code{$aru.serial} is kept
#' as a column. The console reports files that failed or were missing
#' headers, and sites with no deployment or no recovery event.
#'
#' \strong{Site summary.} \code{$service.count} counts \code{status_check},
#' \code{download}, \code{aru_swap}, \code{mic_swap} and \code{service}
#' events. \code{$duplicates.removed} counts exact duplicate rows found for
#' the site.
#'
#' @examples
#' \dontrun{
#' batz.generate_arumeta.eventlog()
#' # aru.eventlog is now in your workspace
#'
#' batz.generate_arumeta.eventlog(dir.sub = TRUE, log.file = TRUE, max.missing = 3)
#' # aru.eventlog, aru.eventlog.filelog, aru.eventlog.sitelog all created
#' }
#'
#' @export
batz.generate_arumeta.eventlog <- function(dir.load = getwd(),
                                            dir.sub           = FALSE,
                                            load.pattern      = c("*HabitatAssessments_20m.csv", "*Acoustic_SiteVisitARU.csv"),
                                            duplicates.remove = TRUE,
                                            log.file          = FALSE,
                                            max.missing       = 5,
                                            strip.special     = TRUE) {

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  ## Canonical header lists, standardized to snake_case (per project
  ## preference, added 2026-09-14 - see preferences.md, "Header
  ## standardization"). These lists ARE the literal deployment/service
  ## form's own field text (not a batz-invented shorthand the way
  ## $mon.ngh/$auto.kp are elsewhere), so both these canonical names AND
  ## every incoming raw file's headers are standardized the same way via
  ## standardize.headers() before matching - a real, visible change from
  ## the previous space/mixed-case column names (e.g. $Client -> $client,
  ## $Date of Deployment -> $date_of_deployment).
  ##
  ## Column-identifier renames (added 2026-09-27, per Josh's reference
  ## workbook "Change.to" column): a handful of these standardize.headers()-
  ## derived names are further remapped to a batz shorthand (e.g.
  ## detector_make -> aru.make) via column.rename.map/apply.column.renames()
  ## below. Matching against real incoming files still happens against the
  ## RAW standardize.headers() spelling (deployment.headers.raw/
  ## service.headers.raw), since that's what real file headers literally
  ## standardize to regardless of this function's own naming - only the
  ## FINAL column names (deployment.headers/service.headers, used to name
  ## `out` and everything downstream) are remapped. See the dated @details
  ## entry above for the full old -> new list.
  deployment.headers.raw <- standardize.headers(c(
    "Client", "Project", "Project Code", "Date of Deployment",
    "Detector Model", "Detector Make", "Microphone Model", "Microphone Make",
    "Site", "Survey Type", "X", "Y", "Serial Number of Detector",
    "Serial Number of Microphone", "Personnel", "Date of Habitat Assessment"))

  service.headers.raw <- standardize.headers(c(
    "Client", "Project", "Project Code", "Date", "Site Name",
    "Reason for site visit", "Personnel", "Notes", "ARU Serial Number",
    "Mic Serial Number", "Power Kit/Solar Serial Number", "HOBO Sensor Serial Number",
    "Select all actions performed", "New mic serial number", "New ARU serial number"))

  column.rename.map <- c(
    aru_serial_number           = "aru.serial",
    date_of_deployment          = "date.deployment",
    date_of_habitat_assessment  = "date.habitat.assessment",
    detector_make               = "aru.make",
    detector_model               = "aru.model",
    hobo_sensor_serial_number    = "hobo.serial",
    power_kit_solar_serial_number = "power.serial",            # 2026-11-04, per Josh (Header.names $Change.to)
    mic_serial_number            = "microphone.serial",
    microphone_make               = "microphone.make",
    microphone_model              = "microphone.model",
    new_aru_serial_number         = "aru.serial_new",
    new_mic_serial_number         = "microphone.serial_new",
    project_code                  = "project.code",
    reason_for_site_visit         = "reason.sitevisit",
    serial_number_of_detector     = "aru.serial",
    serial_number_of_microphone   = "microphone.serial",
    site                          = "site.name",
    x                             = "longitude",
    y                             = "latitude"
  )
  apply.column.renames <- function(nm) {
    hit <- nm %in% names(column.rename.map)
    nm[hit] <- column.rename.map[nm[hit]]
    nm
  }

  deployment.headers <- apply.column.renames(deployment.headers.raw)
  service.headers    <- apply.column.renames(service.headers.raw)

  event.priority <- c(deployment = 1, status_check = 2, download = 3, aru_swap = 4, mic_swap = 5,
                       recovery = 6, service = 7)

  resolve.duplicate.columns <- function(df) {
    nm <- names(df)
    dup.names <- unique(nm[duplicated(nm)])
    if (length(dup.names) == 0) return(list(df = df, dup.cols = character(0)))

    keep <- rep(TRUE, ncol(df))
    for (dn in dup.names) {
      idx <- which(nm == dn)
      cols <- lapply(idx, function(i) {
        v <- as.character(df[[i]])
        ifelse(is.na(v) | v == "", NA_character_, v)
      })
      merged <- cols[[1]]
      for (i in 2:length(cols)) {
        this.col <- cols[[i]]
        conflict <- !is.na(merged) & !is.na(this.col) & merged != this.col
        fill     <- is.na(merged) & !is.na(this.col)
        merged[fill]     <- this.col[fill]
        merged[conflict] <- paste(merged[conflict], this.col[conflict], sep = "; ")
      }
      df[[idx[1]]] <- merged
      keep[idx[-1]] <- FALSE
    }
    list(df = df[keep], dup.cols = dup.names)
  }

  match.headers <- function(tmp.names, master) {
    ## tmp.names is standardized in process.one.file() before this is ever
    ## called; master is already standardize.headers()-canonical (see
    ## above), so a plain equality check is enough here.
    vapply(master, function(m) {
      idx <- which(tmp.names == m)
      if (length(idx) >= 1) tmp.names[idx[1]] else NA_character_
    }, character(1))
  }

  full.outer.rbind <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    all.cols <- union(names(a), names(b))
    for (cn in setdiff(all.cols, names(a))) a[[cn]] <- NA
    for (cn in setdiff(all.cols, names(b))) b[[cn]] <- NA
    rbind(a[all.cols], b[all.cols])
  }

  process.one.file <- function(f) {
    raw <- special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    ## special characters (per Josh, 2026-09-30): strip right after reading;
    ## stripped headers are reported under their standardized names.
    s <- special.strip(raw, strip.special); raw <- s$df
    last.counts <<- s$counts
    strip.headers <- standardize.headers(sub("^\ufeff", "", s$headers))
    names(raw) <- sub("^\ufeff", "", names(raw))
    ## Standardize incoming raw headers to snake_case (trim whitespace,
    ## replace special characters, lowercase) before anything else touches
    ## them - per project preference, added 2026-09-14.
    names(raw) <- standardize.headers(names(raw))

    dc <- resolve.duplicate.columns(raw)
    tmp <- dc$df
    dup.cols <- dc$dup.cols

    ## NOTE: this classification check intentionally still matches the raw,
    ## standardize.headers()-derived spelling ("date_of_deployment"), not
    ## the renamed "date.deployment" - real incoming CSV headers, once run
    ## through standardize.headers(), will always literally produce this
    ## spelling regardless of this function's own canonical column naming,
    ## so the check has to keep matching it (see the dated column-rename
    ## @details entry above).
    is.deployment <- any(names(tmp) == "date_of_deployment")
    master.raw <- if (is.deployment) deployment.headers.raw else service.headers.raw
    master     <- if (is.deployment) deployment.headers     else service.headers
    file.type  <- if (is.deployment) "deployment" else "service"

    matched <- match.headers(names(tmp), master.raw)
    present <- !is.na(matched)
    missing.headers <- master[!present]
    n.missing <- length(missing.headers)

    if (n.missing > max.missing) {
      return(list(status = "failed", file.type = file.type,
                  notes = paste0("missing headers (", n.missing, " > max.missing): ",
                                  paste(missing.headers, collapse = ", ")),
                  data = NULL, strip.headers = strip.headers))
    }

    out <- as.data.frame(tmp[matched[present]], stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- master[present]
    for (mh in missing.headers) out[[mh]] <- NA
    out <- out[master]

    status <- if (length(dup.cols) > 0) "duplicate" else if (n.missing > 0) "missing" else "success"
    notes.parts <- character(0)
    if (length(dup.cols) > 0) {
      notes.parts <- c(notes.parts, paste0("duplicate columns merged: ", paste(dup.cols, collapse = ", ")))
    }
    if (n.missing > 0) {
      notes.parts <- c(notes.parts, paste0("missing columns (padded NA): ", paste(missing.headers, collapse = ", ")))
    }
    notes <- if (length(notes.parts) > 0) paste(notes.parts, collapse = " | ") else NA_character_

    list(status = status, file.type = file.type, notes = notes, data = out,
         strip.headers = strip.headers)
  }

  explode.service.events <- function(df) {
    # all five event types (status_check/download/aru_swap/mic_swap/
    # recovery) are drawn from "Select all actions performed" alone (per
    # Josh, 2026-08-23) - split on commas, one event row per token; a blank
    # action list falls back to a generic "service" event so it isn't lost.
    if (is.null(df) || nrow(df) == 0) return(NULL)
    out.rows <- vector("list", 0)
    for (i in seq_len(nrow(df))) {
      row <- df[i, , drop = FALSE]
      actions <- trimws(unlist(strsplit(as.character(row[["select_all_actions_performed"]]), ",")))
      actions <- actions[!is.na(actions) & actions != ""]

      events <- if (length(actions) > 0) actions else "service"

      for (ev in events) {
        r2 <- row
        r2$event.type <- ev
        out.rows[[length(out.rows) + 1]] <- r2
      }
    }
    do.call(rbind, out.rows)
  }

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  deployment <- NULL
  service    <- NULL
  file.log.rows <- list()
  all.strip.headers <- character(0)
  all.counts <- NULL
  last.counts <- NULL

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      last.counts <- NULL
      r <- process.one.file(f)
      all.strip.headers <- c(all.strip.headers, r$strip.headers)
      all.counts <- special.counts.add(all.counts, last.counts)
      file.log.rows[[length(file.log.rows) + 1]] <- data.frame(
        filename = basename(f), file.type = r$file.type, status = r$status,
        notes = r$notes,
        special.log.cols(strip.special, r$strip.headers, last.counts),
        stringsAsFactors = FALSE)

      cat("  ", basename(f), " -> ", r$file.type, " (", r$status, ")\n", sep = "")

      if (!is.null(r$data)) {
        if (r$file.type == "deployment") {
          deployment <- if (is.null(deployment)) r$data else rbind(deployment, r$data)
        } else {
          service <- if (is.null(service)) r$data else rbind(service, r$data)
        }
      }
    }
  }

  file.log <- if (length(file.log.rows) > 0) {
    do.call(rbind, file.log.rows)
  } else {
    data.frame(filename = character(0), file.type = character(0), status = character(0),
               notes = character(0), special.log.cols.empty(),
               stringsAsFactors = FALSE)
  }

  ## date + time for $date.time; a date with no time gets 12:00:00
  join.date.time <- function(d, t = NULL) {
    d <- trimws(as.character(d))
    t <- if (is.null(t)) rep("", length(d)) else trimws(as.character(t))
    t[is.na(t)] <- ""
    has.time <- grepl("[0-9]{1,2}:[0-9]{2}", d)
    out <- ifelse(has.time, d, paste(d, ifelse(nzchar(t), t, "12:00:00")))
    out[is.na(d) | !nzchar(d)] <- NA_character_
    out
  }

  if (!is.null(deployment) && nrow(deployment) > 0) {
    deployment$event.type   <- "deployment"
    deployment$aru.serial   <- deployment[["aru.serial"]]
    deployment$site.unified <- deployment[["site.name"]]
    deployment$date.time    <- join.date.time(deployment[["date.deployment"]])
  }

  service.events <- explode.service.events(service)
  if (!is.null(service.events) && nrow(service.events) > 0) {
    service.events$aru.serial   <- service.events[["aru.serial"]]
    service.events$site.unified <- service.events[["site_name"]]
    service.events$date.time    <- join.date.time(service.events[["date"]], service.events[["time"]])
  }

  aru.eventlog <- full.outer.rbind(deployment, service.events)
  dup.sites <- character(0)

  if (is.null(aru.eventlog)) {
    aru.eventlog <- data.frame()
    cat("\nNo deployment or service records were loaded - aru.eventlog is empty.\n")
  } else {
    rank <- ifelse(aru.eventlog$event.type %in% names(event.priority),
                    event.priority[aru.eventlog$event.type], 99)
    # grouped/ordered by site.unified, not aru.serial (per Josh, 2026-08-23:
    # "Site and ARU are the same thing... for now use the site" - form
    # headers aren't standardized across sources yet, so ARU serial numbers
    # can't be trusted to line up with a site the way Site names can).
    aru.eventlog <- aru.eventlog[order(aru.eventlog$site.unified, aru.eventlog$date.time, rank), ]

    dup.mask <- duplicated(aru.eventlog)
    n.dup <- sum(dup.mask)
    if (n.dup > 0) dup.sites <- aru.eventlog$site.unified[dup.mask]
    if (duplicates.remove && n.dup > 0) {
      cat("\n", n.dup, " exact duplicate row(s) removed from aru.eventlog.\n", sep = "")
      aru.eventlog <- aru.eventlog[!dup.mask, ]
    }
    rownames(aru.eventlog) <- NULL
  }

  dep.log <- file.log[file.log$file.type == "deployment", ]
  svc.log <- file.log[file.log$file.type == "service", ]

  report.category <- function(cat.log, label) {
    failed  <- cat.log$filename[cat.log$status == "failed"]
    missing <- cat.log$filename[cat.log$status == "missing"]
    if (length(failed) > 0) {
      cat("These ", label, " files failed to merge as they lacked enough matching headers: ",
          paste(failed, collapse = ", "), "\n", sep = "")
    } else {
      cat("all ", label, " files successfully merged\n", sep = "")
    }
    if (length(missing) > 0) {
      cat("these ", label, " files were missing headers please check outputs: ",
          paste(missing, collapse = ", "), "\n", sep = "")
    }
  }
  report.category(dep.log, "deployment")
  report.category(svc.log, "service")

  if (nrow(aru.eventlog) > 0) {
    all.sites <- unique(aru.eventlog$site.unified[!is.na(aru.eventlog$site.unified)])
    sites.with.deployment <- unique(aru.eventlog$site.unified[aru.eventlog$event.type == "deployment"])
    sites.with.recovery   <- unique(aru.eventlog$site.unified[aru.eventlog$event.type == "recovery"])

    missing.dep.sites <- setdiff(all.sites, sites.with.deployment)
    missing.rec.sites <- setdiff(all.sites, sites.with.recovery)

    if (length(missing.dep.sites) > 0) {
      cat("these sites are missing deployment entries: ", paste(missing.dep.sites, collapse = ", "), "\n", sep = "")
    }
    if (length(missing.rec.sites) > 0) {
      cat("these sites are missing recovery entries: ", paste(missing.rec.sites, collapse = ", "), "\n", sep = "")
    }
  }

  first.nonblank <- function(x) {
    v <- x[!is.na(x) & x != ""]
    if (length(v) > 0) v[1] else NA
  }

  if (nrow(aru.eventlog) > 0) {
    sites <- unique(aru.eventlog$site.unified[!is.na(aru.eventlog$site.unified)])
    site.rows <- lapply(sites, function(s) {
      sub <- aru.eventlog[aru.eventlog$site.unified == s, ]
      data.frame(
        client         = first.nonblank(sub$client),
        project        = first.nonblank(sub$project),
        project.code   = first.nonblank(sub[["project.code"]]),
        site.name      = s,
        deployment     = sum(sub$event.type == "deployment"),
        service.count  = sum(sub$event.type %in% c("status_check", "download", "aru_swap", "mic_swap", "service")),
        recovery       = sum(sub$event.type == "recovery"),
        duplicates.removed = sum(dup.sites == s),
        special.log.cols(strip.special, all.strip.headers, all.counts),
        stringsAsFactors = FALSE
      )
    })
    aru.eventlog.sitelog <- do.call(rbind, site.rows)
  } else {
    aru.eventlog.sitelog <- data.frame(client = character(0), project = character(0),
                                        project.code = character(0), site.name = character(0),
                                        deployment = integer(0), service.count = integer(0),
                                        recovery = integer(0), duplicates.removed = integer(0),
                                        special.log.cols.empty(),
                                        stringsAsFactors = FALSE)
  }

  result <- list(aru.eventlog = aru.eventlog)
  if (log.file) {
    result$aru.eventlog.filelog <- file.log
    result$aru.eventlog.sitelog <- aru.eventlog.sitelog
  }

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

#' Build a combined deployment/service/recovery event log for ARUs
#'
#' Searches a directory (and, optionally, its subdirectories) for the
#' deployment-form and service-visit-form files used to track ARU
#' (Autonomous Recording Unit) fieldwork, classifies each loaded file as a
#' \strong{deployment} or a \strong{service} (service/recovery) record,
#' lines up their columns against two canonical header lists, and combines
#' everything into one chronological event log per ARU/site: every
#' deployment, every service visit, every recovery, and any equipment swap
#' recorded along the way. Intended as an input to later functions that need
#' to know what happened to a given ARU/site and when (e.g. to explain gaps
#' in logger data or annotate plots).
#'
#' @param dir.load Character. Directory to search for files matching
#'   \code{load.pattern}. Default \code{getwd()}.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load}.
#' @param load.pattern Character vector of wildcard/glob suffix patterns
#'   (converted internally to a regex via \code{utils::glob2rx()}), default
#'   \code{c("*HabitatAssessments_20m.csv", "*Acoustic_SiteVisitARU.csv")} -
#'   the deployment-form and service-visit-form file name patterns. See
#'   \strong{Note} below on which file is treated as "deployment".
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from the final combined event log.
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   return (and auto-assign) \code{aru.eventlog.filelog} (one row per input
#'   file: \code{$filename}, \code{$file.type}, \code{$status}, \code{$notes})
#'   and \code{aru.eventlog.sitelog} (one row per site: \code{$client},
#'   \code{$project}, \code{$project.code}, \code{$site.name}, \code{$deployment},
#'   \code{$service.count}, \code{$recovery}, \code{$duplicates.removed}).
#' @param max.missing Integer, default \code{5}. The largest number of
#'   canonical headers a file is allowed to be missing (after exact,
#'   case-insensitive matching) and still be merged in; missing headers are
#'   padded with \code{NA}. A file missing more than this many headers is
#'   not merged and is logged with status \code{"failed"}.
#'
#' @return Invisibly, a named list: \code{aru.eventlog} (always), plus
#'   \code{aru.eventlog.filelog} and \code{aru.eventlog.sitelog} when
#'   \code{log.file = TRUE}. Every element is also auto-assigned into the
#'   calling environment (see \strong{Details}), so a bare call with no
#'   assignment creates \code{aru.eventlog} (and the two log tables, if
#'   requested) directly in your workspace.
#'
#' @details
#' \strong{Header standardization (added 2026-09-14, per project
#' preference):} every loaded file's raw headers are first run through
#' \code{standardize.headers()} (trim whitespace, replace runs of special
#' characters with an underscore, lowercase - i.e. snake_case) before
#' anything else touches them. Because \code{deployment.headers}/
#' \code{service.headers} are themselves just the literal deployment/
#' service form field text (not a \code{batz}-invented shorthand), those
#' canonical lists are standardized the same way - so \code{aru.eventlog}'s
#' own column names changed too (e.g. what used to be \code{$Client}/
#' \code{$"Date of Deployment"}/\code{$"Project Code"} are now
#' \code{$client}/\code{$date_of_deployment}/\code{$project_code}). This is
#' a real, visible output-schema change from before this preference existed
#' - please update any downstream script that referenced the old
#' space/mixed-case column names.
#'
#' \strong{Auto-assign into caller's environment:} following the same
#' pattern already used in \code{batz.arumeta_merge.format} and
#' \code{batz.datawrangler_load.files}, every returned object is also
#' \code{assign()}-ed into \code{parent.frame()}, so
#' \code{batz.generate_arumeta.eventlog(...)} with no assignment populates
#' the workspace directly; \code{result <- batz.generate_arumeta.eventlog(...)}
#' still works exactly the same for anyone who prefers \code{result$aru.eventlog}
#' -style access.
#'
#' \strong{Classifying a file as deployment vs. service:} a loaded file is
#' treated as a \emph{deployment} record if it has a column named
#' \code{"Date of Deployment"} (case-insensitive); otherwise it's treated as
#' a \emph{service} record. This is a content-based test, not a filename
#' test, so it works even if a file's name doesn't match the usual pattern.
#'
#' \strong{Within-file duplicate columns:} if a file has the same column
#' name more than once, all copies are compared row-by-row (\code{NA} and
#' \code{""} treated as equivalent blanks): identical everywhere - keep one,
#' drop the rest; differ anywhere - merge into one column, taking whichever
#' copy is non-blank per row and concatenating with \code{"; "} where both
#' are non-blank and disagree (same conflict-aware merge already used in
#' \code{batz.arumeta_merge.format}).
#'
#' \strong{Header matching against the canonical deployment/service lists}
#' is exact-string and case-insensitive, but NOT typo-tolerant. Real
#' \code{HabitatAssessments_20m.csv} data has genuine header typos
#' ("Clien", "Prject Code") that will always show up as missing columns
#' (padded \code{NA}) rather than being auto-corrected - see \code{$notes}
#' in \code{aru.eventlog.filelog} for exactly what didn't match on any given
#' run.
#'
#' \strong{Deriving event types for service rows:} a service row's
#' \code{"Select all actions performed"} column is split on commas into
#' individual action tokens (e.g. \code{"aru_swap"}, \code{"mic_swap"},
#' \code{"recovery"}) and each becomes its own event row (so a single visit
#' can generate more than one event row). A visit with no recorded action
#' becomes a generic \code{"service"} event so it's never silently dropped.
#' When more than one event shares the same site and timestamp, they're
#' ordered \code{deployment, status_check, download, aru_swap, mic_swap}
#' first (per spec), then \code{recovery, service} (not given an explicit
#' position in the spec's ordering list).
#'
#' \strong{Grouping key: site, not ARU serial.} Same-timestamp ordering and
#' the per-site summary/checks group by \code{site.unified}, not
#' \code{aru.serial} (per Josh: site and ARU are treated as the same thing
#' for now, since headers aren't yet standardized across the different
#' source forms). \code{aru.serial} is still carried through as a column,
#' just not used as a grouping/ordering key.
#'
#' \strong{Note - which file is "deployment":} the deployment header list
#' matches \code{HabitatAssessments_20m.csv}, not
#' \code{HabitatAssessments_quad.csv} (quad.csv shares none of the expected
#' deployment headers and is not read by this function at all). See the
#' dev script's header comment for the full reasoning - please confirm this
#' is the intended source file.
#'
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's
#' reference-workbook "Change.to" column.} The following raw/legacy header
#' spellings were renamed to their new canonical form throughout this
#' function's code and documentation: \code{ARU.serial} (output) ->
#' \code{aru.serial}; \code{aru_serial_number} (input) -> \code{aru.serial};
#' \code{date_of_deployment} -> \code{date.deployment};
#' \code{date_of_habitat_assessment} -> \code{date.habitat.assessment};
#' \code{detector_make} -> \code{aru.make}; \code{detector_model} ->
#' \code{aru.model}; \code{hobo_sensor_serial_number} -> \code{hobo.serial};
#' \code{mic_serial_number} -> \code{microphone.serial}; \code{microphone_make}
#' -> \code{microphone.make}; \code{microphone_model} ->
#' \code{microphone.model}; \code{new_aru_serial_number} ->
#' \code{aru.serial_new}; \code{new_mic_serial_number} ->
#' \code{microphone.serial_new}; \code{project_code} -> \code{project.code};
#' \code{reason_for_site_visit} -> \code{reason.sitevisit};
#' \code{serial_number_of_detector} (input) -> \code{aru.serial};
#' \code{serial_number_of_microphone} (input) -> \code{microphone.serial};
#' \code{site} -> \code{site.name}; \code{Site.unified} -> \code{site.unified}
#' (case only); \code{service} (the site-log service-event count, output) ->
#' \code{service.count}; \code{x} (raw longitude header alias) ->
#' \code{longitude}; \code{y} (raw latitude header alias) -> \code{latitude}.
#' \code{aru_serial_number}, \code{serial_number_of_detector}, and the old
#' \code{ARU.serial} output all converge on the same \code{aru.serial} name
#' (likewise \code{mic_serial_number}/\code{serial_number_of_microphone} on
#' \code{microphone.serial}) since they were different raw spellings this
#' function already accepted/produced for the same concept - that
#' convergence is intentional, not a duplicate-name bug. The file-
#' classification check against the raw, standardize.headers()-derived
#' \code{"date_of_deployment"} spelling is deliberately NOT renamed, since
#' real incoming CSV headers always standardize to that legacy spelling
#' regardless of this function's own canonical naming; likewise, other
#' unrenamed appearances of \code{"service"}/\code{"site"} in this file are
#' the general English words (a service visit, a site's rows) or unrelated
#' \code{event.type}/\code{file.type} category labels, not this function's
#' \code{service.count}/\code{site.name} columns.
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
                                            max.missing       = 5) {

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
    raw <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    names(raw) <- sub("^﻿", "", names(raw))
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
                  data = NULL))
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

    list(status = status, file.type = file.type, notes = notes, data = out)
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

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f)
      file.log.rows[[length(file.log.rows) + 1]] <- data.frame(
        filename = basename(f), file.type = r$file.type, status = r$status,
        notes = r$notes, stringsAsFactors = FALSE)

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
               notes = character(0), stringsAsFactors = FALSE)
  }

  if (!is.null(deployment) && nrow(deployment) > 0) {
    deployment$event.type   <- "deployment"
    deployment$aru.serial   <- deployment[["aru.serial"]]
    deployment$site.unified <- deployment[["site.name"]]
    deployment$date.time    <- as.character(deployment[["date.deployment"]])
  }

  service.events <- explode.service.events(service)
  if (!is.null(service.events) && nrow(service.events) > 0) {
    service.events$aru.serial   <- service.events[["aru.serial"]]
    service.events$site.unified <- service.events[["site_name"]]
    service.events$date.time    <- paste(service.events[["date"]], service.events[["time"]])
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
        stringsAsFactors = FALSE
      )
    })
    aru.eventlog.sitelog <- do.call(rbind, site.rows)
  } else {
    aru.eventlog.sitelog <- data.frame(client = character(0), project = character(0),
                                        project.code = character(0), site.name = character(0),
                                        deployment = integer(0), service.count = integer(0),
                                        recovery = integer(0), duplicates.removed = integer(0),
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

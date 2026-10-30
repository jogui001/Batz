#' Merge Survey123 ARU site-visit forms into one workbook with Batz headers
#'
#' Reads every Survey123 export used for ARU site deployments (habitat
#' assessment / deployment, quadrant, site maintenance & retrieval forms) and
#' merges them into one Excel file with standardized (Batz) headers - one tab
#' per form type (\code{quad}, \code{habitat}, \code{service}). Built
#' 2026-10-29, per Josh.
#'
#' @param dir.load Character, default \code{getwd()}. Folder searched for the
#'   Survey123 export workbooks.
#' @param load.pattern Character vector of file-name patterns (wildcards
#'   \code{*} and \code{?}), default \code{c("*SiteMaintenance*Retrieval*.xlsx",
#'   "*HabitatAssessment*.xlsx", "*deployments*.xlsx")} (2026-10-29: the first
#'   two match Josh's FY26 export names, e.g.
#'   \code{..._SiteMaintenanceRetrieval_Final.xlsx},
#'   \code{..._HabitatAssessments_Final.xlsx}). A file is read when its name
#'   matches any of them (case ignored). Excel lock files (\code{~$...}) are
#'   skipped.
#' @param dir.sub Logical, default \code{TRUE}. Also search sub-folders of
#'   \code{dir.load}.
#' @param form.list \code{NA} (default), the path to a workbook laid out like
#'   \code{Survey123_aruforms all headers.xlsx}, a data frame, or a named list
#'   of data frames. New form versions / tabs / choices to add to the internal
#'   database \code{Survey123.aruforms_headers.choices} for this run (see
#'   Details, "Adding forms").
#' @param dir.save Character, default \code{getwd()}. Folder the merged
#'   workbook is written to (named \code{dir.save} like the other Batz
#'   functions - Josh's spec had \code{dir.sav}).
#' @param save.name Character, default \code{"mergedsurvey123.xlsx"}. Name of
#'   the merged workbook (\code{.xlsx} is added if the name has no
#'   \code{.xlsx} ending).
#' @param header.combo Character, default \code{c("project.name", "aru.name")}.
#'   Passed to \code{batz.datawrangler_consolidate.globalID()}: headers whose
#'   unique combinations define a site (added 2026-10-30, per Josh).
#' @param header.uniqueid Character, default \code{c("GlobalID")}. Passed to
#'   \code{batz.datawrangler_consolidate.globalID()}: the unique-ID header.
#' @param header.dates Character, default \code{c("date.habitat.assessment",
#'   "date.deployment", "date")}. Passed to
#'   \code{batz.datawrangler_consolidate.globalID()}: date headers used to find
#'   each site's earliest record.
#'
#' @return Invisibly, a list: one data frame per form type (\code{$quad},
#'   \code{$habitat}, \code{$service}, plus any tab added through
#'   \code{form.list}), \code{$log} (one row per sheet read: \code{$file},
#'   \code{$sheet}, \code{$tab}, \code{$form.version}, \code{$rows},
#'   \code{$headers.matched}, \code{$headers.not.used}), \code{$headers.db}
#'   (the \code{Survey123.aruforms_headers.choices} database used, including
#'   anything added through \code{form.list}) and \code{$file} (the workbook
#'   written).
#'
#' @details
#' \strong{Internal database.} \code{Survey123.aruforms_headers.choices} (a
#' list of data frames, one per tab) holds Josh's \code{Survey123_aruforms all headers.xlsx}: for tabs
#' \code{quad}, \code{habitat}, \code{service}, the Batz header
#' (\code{Batz.headers}), the tab-qualified header (\code{aru.<tab>_Batz.headers},
#' e.g. \code{aru.quad$aru.name}, since some tabs share a header that different
#' functions such as \code{batz.generate_aru.appendix()} use at different
#' times) and one column per form version (e.g. \code{forest_2026},
#' \code{inland_2026}, \code{service_2026}) giving that version's Survey123
#' header. Tab \code{Choices}: \code{$list_name}, \code{$name} (the Survey123
#' code) and \code{$label} (the cleaned-up text). A sheet that already has
#' Batz headers may use the earlier spellings \code{measurment.units} /
#' \code{microphone.callibration_value} (and the others in
#' \code{survey123.ref.renamed()}); they are read as the corrected headers.
#'
#' \strong{Adding forms (\code{form.list}).} Each tab given is merged into the
#' database for this run: tabs \code{quad}, \code{habitat}, \code{service} and
#' \code{Choices} add their versions / choices to the existing ones, and any
#' other tab becomes a new form type (and a new tab in the output). Every
#' header tab needs at least three columns: \code{Batz.headers},
#' \code{aru.<tabname>_Batz.headers} (any column whose name starts with
#' \code{aru.} and contains \code{_Batz.headers}; it is renamed to
#' \code{aru.<tabname>_Batz.headers}) and one or more form-version
#' columns. The tab name comes from the sheet / list-element name, else from
#' the \code{aru.<tab>$} prefix of the tab-qualified headers. A \code{Choices}
#' tab needs \code{list_name}, \code{name} and \code{label}. The package's own
#' copy is not changed - send Josh's updated workbook to have it embedded.
#'
#' \strong{Steps.}
#' \enumerate{
#'   \item One empty data frame per form type, with \code{$form.version}
#'     first, then each Batz header once (in the database's order).
#'   \item Every sheet of every matching file is compared with each form
#'     version's Survey123 headers (and with the Batz headers themselves),
#'     ignoring case, spaces and punctuation. Survey123's own system fields
#'     (ObjectID, GlobalID, ParentGlobalID, CreationDate, Creator, EditDate,
#'     Editor, x, y) are in every form and are not counted. The version with
#'     the most matching headers is used, when at least 3 headers and at
#'     least half of the sheet's other headers match; other sheets (e.g.
#'     \code{repeat_photo_1}) are skipped with a NOTE.
#'   \item Headers missing from a sheet are left blank (versions gain and
#'     retire headers). Columns the database doesn't list are not carried
#'     over; they are named in a NOTE and in \code{$log$headers.not.used}.
#'     When two Survey123 columns share a Batz header (e.g. \code{Site Name}
#'     and \code{Specify Other Site} -> \code{aru.name}) the first non-blank
#'     value is used, and \code{"other"} is replaced by the "specify" value.
#'   \item The sheet's rows are appended to that form type's data frame with
#'     \code{$form.version} = the version name (e.g. \code{inland_2026}).
#'   \item One tab per form type is written to \code{file.path(dir.save,
#'     save.name)} (with the \code{openxlsx} package, else \code{writexl}).
#' }
#' Values are copied as they are; Survey123 codes are not changed to their
#' Choices labels.
#'
#' \strong{Workbooks that can't be opened} (2026-10-30, per Josh). Each file
#' is checked before it is read, and the run stops with a plain message:
#' \itemize{
#'   \item open in Excel (its \code{~$} lock file is in the folder, or the
#'     file can't be read): "<file> is open in another program and cannot be
#'     loaded. Close it and re-run code.";
#'   \item 0 bytes (e.g. a OneDrive file that hasn't finished downloading);
#'   \item password-protected (or an old \code{.xls} saved with an
#'     \code{.xlsx} ending);
#'   \item not a real \code{.xlsx} (damaged, or another file type renamed).
#' }
#' A workbook that passes these checks but still can't be unzipped gets the
#' "open in another program" message plus a note that it may be damaged.
#'
#' \strong{Consolidated GlobalID} (2026-10-30, per Josh). After the merge,
#' \code{batz.datawrangler_consolidate.globalID()} is run on the merged tabs
#' with \code{header.combo}, \code{header.uniqueid} and \code{header.dates}:
#' the GlobalID of each site's earliest record (across all tabs) is added to
#' every record of that site as \code{$globalID_consolidate}, just after
#' \code{$GlobalID}. A tab without all the \code{header.combo} headers (e.g.
#' \code{quad}, which has no \code{project.name}) is left unchanged (NOTE).
#'
#' @examples
#' \dontrun{
#' m <- batz.merge_sitevisit.acoustics(
#'   dir.load = "C:/data/Survey123",
#'   dir.save = "C:/data/Survey123/merged")
#' m$log
#' }
#'
#' @export
batz.merge_sitevisit.acoustics <- function(dir.load = getwd(),
                                           load.pattern = c("*SiteMaintenance*Retrieval*.xlsx", "*HabitatAssessment*.xlsx",
                                                            "*deployments*.xlsx"),
                                           dir.sub = TRUE,
                                           form.list = NA,
                                           dir.save = getwd(),
                                           save.name = "mergedsurvey123.xlsx",
                                           header.combo = c("project.name", "aru.name"),
                                           header.uniqueid = c("GlobalID"),
                                           header.dates = c("date.habitat.assessment", "date.deployment", "date")) {

  if (!requireNamespace("readxl", quietly = TRUE)) stop("The readxl package is needed: install.packages(\"readxl\")", call. = FALSE)
  if (!dir.exists(dir.load)) stop("dir.load not found: ", dir.load, call. = FALSE)
  key <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
  is.blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x))) | tolower(trimws(as.character(x))) %in% c("na", "n/a")

  ## --- 1. header database (+ form.list) ----------------------------------
  db <- Survey123.aruforms_headers.choices
  ch <- db$Choices
  tab.col <- function(d) grep("^aru\\..*_Batz\\.headers", names(d), ignore.case = TRUE)[1]
  ## the database's tabs as one long table: tab, row, batz.header, version,
  ## survey123.header
  to.long <- function(db) {
    out <- list()
    for (t in setdiff(names(db), "Choices")) {
      d <- db[[t]]; th <- tab.col(d); bh <- which(names(d) == "Batz.headers")[1]
      for (v in setdiff(seq_along(d), c(th, bh))) {
        out[[length(out) + 1]] <- data.frame(tab = t, row = seq_len(nrow(d)), batz.header = trimws(d[[bh]]),
                                             version = names(d)[v], survey123.header = trimws(d[[v]]),
                                             stringsAsFactors = FALSE)
      }
    }
    do.call(rbind, out)
  }

  ## 2026-10-30, per Josh: a workbook that can't be opened gets a plain
  ## message instead of readxl's "zip file ... cannot be opened"
  xl.check <- function(f) {
    nm <- basename(f)
    lock <- file.path(dirname(f), c(paste0("~$", nm), paste0("~$", substring(nm, 3))))
    if (any(file.exists(lock))) {
      stop(nm, " is open in another program and cannot be loaded. Close it and re-run code.", call. = FALSE)
    }
    sz <- file.size(f)
    if (is.na(sz) || sz == 0) {
      stop(nm, " is empty (0 bytes) - if it is in OneDrive, it may not have finished downloading. ",
           "Open the folder, wait for the file to sync, and re-run code.", call. = FALSE)
    }
    head4 <- tryCatch(readBin(f, "raw", 4), error = function(e) NULL)
    if (is.null(head4)) {
      stop(nm, " is open in another program and cannot be loaded. Close it and re-run code.", call. = FALSE)
    }
    if (grepl("\\.xlsx$", nm, ignore.case = TRUE) && !identical(head4[1:2], as.raw(c(0x50, 0x4b)))) {
      if (identical(head4, as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) {
        stop(nm, " is password-protected (or an old .xls saved as .xlsx) and cannot be loaded. ",
             "Remove the password / save it as .xlsx in Excel and re-run code.", call. = FALSE)
      }
      stop(nm, " is not a readable .xlsx file (it may be damaged or another file type renamed .xlsx).", call. = FALSE)
    }
    invisible(TRUE)
  }
  xl.sheets <- function(f) {
    xl.check(f)
    tryCatch(readxl::excel_sheets(f), error = function(e) {
      if (grepl("cannot be opened|zip file", conditionMessage(e))) {
        stop(basename(f), " is open in another program and cannot be loaded. Close it and re-run code.",
             "\n  (If it is not open, the file may be damaged - open it in Excel, save it, and re-run code.)", call. = FALSE)
      }
      stop(basename(f), " could not be read: ", conditionMessage(e), call. = FALSE)
    })
  }

  read.tab.list <- function(x) {
    if (is.null(x) || (length(x) == 1 && is.atomic(x) && is.na(x))) return(list())
    if (is.character(x)) {
      out <- list()
      for (f in x) {
        if (!file.exists(f)) stop("form.list file not found: ", f, call. = FALSE)
        for (s in xl.sheets(f)) {
          out[[s]] <- as.data.frame(readxl::read_excel(f, s, col_types = "text", .name_repair = "minimal"),
                                    check.names = FALSE, stringsAsFactors = FALSE)
        }
      }
      return(out)
    }
    if (is.data.frame(x)) return(list(x))
    if (is.list(x)) return(x)
    stop("form.list must be NA, a workbook path, a data frame or a named list of data frames.", call. = FALSE)
  }
  add <- read.tab.list(form.list)
  for (i in seq_along(add)) {
    d <- as.data.frame(add[[i]], check.names = FALSE, stringsAsFactors = FALSE)
    d <- d[, nzchar(names(d)), drop = FALSE]
    nm <- if (!is.null(names(add)) && nzchar(names(add)[i])) names(add)[i] else ""
    if (tolower(nm) == "choices" || all(c("list_name", "name", "label") %in% names(d))) {
      if (!all(c("list_name", "name", "label") %in% names(d))) stop("form.list Choices tab needs list_name, name and label columns.", call. = FALSE)
      d <- d[, c("list_name", "name", "label")]
      d[] <- lapply(d, function(v) trimws(as.character(v)))
      d <- d[!is.blank(d$list_name), , drop = FALSE]
      new.ch <- rbind(ch, d)
      ch <- new.ch[!duplicated(new.ch[, c("list_name", "name")]), , drop = FALSE]
      next
    }
    th <- tab.col(d)
    bh <- which(tolower(names(d)) == "batz.headers")[1]
    if (ncol(d) < 3 || is.na(th) || is.na(bh)) {
      stop("form.list tab '", nm, "' needs at least three columns: Batz.headers, aru.<tabname>_Batz.headers ",
           "and one or more form-version columns.", call. = FALSE)
    }
    if (!nzchar(nm)) {                        # tab name from aru.<tab>$...
      pre <- sub("^aru\\.([^$]+)\\$.*$", "\\1", d[[th]][grepl("^aru\\.[^$]+\\$", d[[th]])])
      nm <- if (length(pre) > 0) names(sort(table(pre), decreasing = TRUE))[1] else
        sub("^aru\\.(.+?)_Batz\\.headers.*$", "\\1", names(d)[th], ignore.case = TRUE)
    }
    d[] <- lapply(d, function(v) trimws(as.character(v)))
    d <- d[!is.blank(d[[bh]]), , drop = FALSE]
    names(d)[th] <- paste0("aru.", nm, "_Batz.headers"); names(d)[bh] <- "Batz.headers"
    vers <- names(d)[-c(th, bh)]
    if (!(nm %in% names(db))) {
      db[[nm]] <- d[, c(th, bh, setdiff(seq_along(d), c(th, bh))), drop = FALSE]
    } else {
      ## rows matched on the tab header + Batz header (+ how many times that
      ## pair has come up, for headers listed twice); a version already in
      ## the database is replaced
      old <- db[[nm]]; names(old)[tab.col(old)] <- names(d)[th]
      kc <- c(names(d)[th], "Batz.headers")
      old$.k <- paste(old[[kc[1]]], old[[kc[2]]], stats::ave(seq_len(nrow(old)), old[[kc[1]]], old[[kc[2]]], FUN = seq_along))
      d$.k   <- paste(d[[kc[1]]], d[[kc[2]]], stats::ave(seq_len(nrow(d)), d[[kc[1]]], d[[kc[2]]], FUN = seq_along))
      old <- old[, setdiff(names(old), vers), drop = FALSE]
      newrows <- d[!(d$.k %in% old$.k), c(".k", kc), drop = FALSE]
      if (nrow(newrows) > 0) old <- merge(old, newrows, all = TRUE, sort = FALSE)
      old <- old[order(match(old$.k, unique(c(old$.k[!(old$.k %in% newrows$.k)], newrows$.k)))), , drop = FALSE]
      for (v in vers) old[[v]] <- d[[v]][match(old$.k, d$.k)]
      old$.k <- NULL; rownames(old) <- NULL
      db[[nm]] <- old[, c(kc, setdiff(names(old), kc)), drop = FALSE]
    }
    cat("NOTE: form.list - tab '", nm, "' added/updated: ", paste(vers, collapse = ", "), "\n", sep = "")
  }
  db$Choices <- ch
  hdr <- to.long(db)
  hdr <- hdr[!is.blank(hdr$batz.header), , drop = FALSE]
  tabs <- unique(hdr$tab)
  ## earlier spellings of Batz headers, read as the corrected ones
  renamed <- if (exists("survey123.ref.renamed", mode = "function")) survey123.ref.renamed() else
    data.frame(header.old = character(0), header.new = character(0))

  ## --- 2. one empty data frame per form type -------------------------------
  batz.cols <- lapply(setNames(tabs, tabs), function(t) {
    h <- hdr[hdr$tab == t, , drop = FALSE]
    unique(h$batz.header[order(h$row)])
  })
  out <- lapply(batz.cols, function(cols) {
    df <- as.data.frame(setNames(replicate(length(cols) + 1, character(0), simplify = FALSE),
                                 c("form.version", cols)), check.names = FALSE, stringsAsFactors = FALSE)
    df
  })
  parts <- lapply(setNames(tabs, tabs), function(t) list())

  ## --- 3. find the files ---------------------------------------------------
  all.f <- list.files(dir.load, recursive = isTRUE(dir.sub), full.names = TRUE)
  rx <- vapply(load.pattern, utils::glob2rx, character(1))
  files <- all.f[vapply(basename(all.f), function(b) any(grepl(paste(rx, collapse = "|"), b, ignore.case = TRUE)), logical(1))]
  files <- files[!grepl("^~\\$", basename(files)) & grepl("\\.xlsx?$", files, ignore.case = TRUE)]
  if (length(files) == 0) {
    stop("No files in ", dir.load, " match load.pattern: ", paste(load.pattern, collapse = ", "), call. = FALSE)
  }

  ## --- 4. read, identify, rename, append ----------------------------------
  log <- list()
  versions <- unique(hdr[, c("tab", "version")])
  for (f in files) {
    for (s in xl.sheets(f)) {
      d <- as.data.frame(readxl::read_excel(f, s, .name_repair = "minimal"), check.names = FALSE, stringsAsFactors = FALSE)
      if (ncol(d) == 0) next
      src <- key(names(d))
      ## Survey123's own system fields (ObjectID, GlobalID, Creator, x, y ...)
      ## are in every form, so they don't count when identifying the form
      sys.keys <- c("objectid", "globalid", "parentglobalid", "creationdate", "creator", "editdate", "editor", "x", "y")
      score.src <- setdiff(unique(src[nzchar(src)]), sys.keys)
      ## score every version, plus each tab's own Batz headers
      cand <- rbind(versions, data.frame(tab = tabs, version = "Batz.headers", stringsAsFactors = FALSE))
      cand$matched <- vapply(seq_len(nrow(cand)), function(k) {
        h <- hdr[hdr$tab == cand$tab[k], , drop = FALSE]
        ref <- if (cand$version[k] == "Batz.headers") c(h$batz.header, renamed$header.old[renamed$header.new %in% h$batz.header]) else
          h$survey123.header[h$version == cand$version[k]]
        sum(score.src %in% key(ref[!is.blank(ref)]))
      }, numeric(1))
      best <- cand[order(-cand$matched), , drop = FALSE][1, ]
      n.src <- max(1, length(score.src))
      if (best$matched < 3 || best$matched / n.src < 0.5) {
        cat("NOTE: skipped ", basename(f), " [", s, "] - its headers don't match a form in the database\n", sep = "")
        log[[length(log) + 1]] <- data.frame(file = basename(f), sheet = s, tab = NA, form.version = NA, rows = nrow(d),
                                             headers.matched = best$matched, headers.not.used = "", stringsAsFactors = FALSE)
        next
      }
      t <- best$tab
      h <- hdr[hdr$tab == t, , drop = FALSE]
      if (best$version == "Batz.headers") {
        map <- data.frame(from = h$batz.header, to = h$batz.header, stringsAsFactors = FALSE)
        old <- renamed[renamed$header.new %in% h$batz.header, , drop = FALSE]
        map <- unique(rbind(map, data.frame(from = old$header.old, to = old$header.new, stringsAsFactors = FALSE)))
        ## an already-standardized sheet keeps its own form.version if it has one
        fv <- if ("form.version" %in% names(d)) as.character(d$form.version) else "Batz.headers"
      } else {
        h <- h[h$version == best$version & !is.blank(h$survey123.header), , drop = FALSE]
        map <- data.frame(from = h$survey123.header, to = h$batz.header, stringsAsFactors = FALSE)
        fv <- best$version
      }
      target <- map$to[match(src, key(map$from))]
      unused <- names(d)[is.na(target) & key(names(d)) != "formversion"]
      if (length(unused) > 0) {
        cat("NOTE: ", basename(f), " [", s, "] - ", length(unused), " column(s) not in the ", t,
            " database, not carried over: ", paste(utils::head(unused, 10), collapse = ", "),
            if (length(unused) > 10) " ..." else "", "\n", sep = "")
      }
      new <- out[[t]][rep(NA_integer_, nrow(d)), , drop = FALSE]
      rownames(new) <- NULL
      new$form.version <- rep(fv, length.out = nrow(d))
      for (tg in unique(target[!is.na(target)])) {
        idx <- which(target == tg)
        v <- d[[idx[1]]]
        for (j in idx[-1]) {                 # same Batz header twice: first non-blank
          w <- d[[j]]
          if (inherits(v, "POSIXct") || inherits(w, "POSIXct")) {
            fill <- is.na(v) & !is.na(w); if (any(fill)) v[fill] <- w[fill]
          } else {
            vc <- as.character(v); wc <- as.character(w)
            use <- (is.blank(vc) | tolower(trimws(vc)) == "other") & !is.blank(wc)
            vc[use] <- wc[use]; v <- vc
          }
        }
        new[[tg]] <- v
      }
      parts[[t]][[length(parts[[t]]) + 1]] <- new
      log[[length(log) + 1]] <- data.frame(file = basename(f), sheet = s, tab = t, form.version = fv[1], rows = nrow(d),
                                           headers.matched = best$matched, headers.not.used = paste(unused, collapse = "; "),
                                           stringsAsFactors = FALSE)
    }
  }

  ## --- 5. stack (a column whose type differs between files becomes text) ---
  as.text <- function(v) if (inherits(v, "POSIXct")) format(v, "%Y-%m-%d %H:%M:%S") else as.character(v)
  for (t in tabs) {
    p <- parts[[t]]
    if (length(p) == 0) next
    for (cl in names(out[[t]])) {
      cls <- unique(vapply(p, function(x) if (all(is.na(x[[cl]]))) NA_character_ else class(x[[cl]])[1], character(1)))
      cls <- cls[!is.na(cls)]
      if (length(cls) > 1) p <- lapply(p, function(x) { x[[cl]] <- as.text(x[[cl]]); x })
      else if (length(cls) == 1 && cls == "POSIXct") p <- lapply(p, function(x) {
        if (!inherits(x[[cl]], "POSIXct")) x[[cl]] <- as.POSIXct(x[[cl]], tz = "UTC"); x })
    }
    out[[t]] <- do.call(rbind, p)
    rownames(out[[t]]) <- NULL
  }
  log <- if (length(log) > 0) do.call(rbind, log) else
    data.frame(file = character(0), sheet = character(0), tab = character(0), form.version = character(0),
               rows = integer(0), headers.matched = integer(0), headers.not.used = character(0))

  ## --- 5b. consolidated GlobalID (2026-10-30, per Josh) ---------------------
  has.rows <- tabs[vapply(tabs, function(t) nrow(out[[t]]) > 0, logical(1))]
  if (length(has.rows) > 0) {
    out[has.rows] <- batz.datawrangler_consolidate.globalID(out[has.rows], header.combo = header.combo,
                                                            header.uniqueid = header.uniqueid, header.dates = header.dates)
  }

  ## --- 6. write -------------------------------------------------------------
  if (!grepl("\\.xlsx$", save.name, ignore.case = TRUE)) save.name <- paste0(sub("\\.[^.]*$", "", save.name), ".xlsx")
  if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
  path <- file.path(dir.save, save.name)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(out, path, overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(out, path)
  } else {
    stop("Writing .xlsx needs the openxlsx (or writexl) package: install.packages(\"openxlsx\")", call. = FALSE)
  }
  for (t in tabs) cat(t, ": ", nrow(out[[t]]), " row(s)\n", sep = "")
  cat("Saved:", path, "\n")

  invisible(c(out, list(log = log, headers.db = db, file = path)))
}

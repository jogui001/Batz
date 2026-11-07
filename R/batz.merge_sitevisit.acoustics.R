#' Merge Survey123 ARU site-visit forms into one workbook with Batz headers
#'
#' Reads every Survey123 export used for ARU site deployments (habitat
#' assessment / deployment, quadrant, site maintenance & retrieval forms) and
#' merges them into one Excel file with standardized (Batz) headers - one tab
#' per form type (\code{quad}, \code{habitat}, \code{service}).
#'
#' @param dir.load Character, default \code{getwd()}. Folder searched for the
#'   Survey123 exports (Excel workbooks or CSV files).
#' @param load.pattern Character vector of file-name patterns (wildcards
#'   \code{*} and \code{?}), default \code{c("*aruHabitat*.xlsx",
#'   "*aruRetrieval*.xlsx", "*aruDeployment*.xlsx", "*aruMaintenance*.xlsx",
#'   "*aruHabitat*.csv", "*aruRetrieval*.csv", "*aruDeployment*.csv",
#'   "*aruMaintenance*.csv")}. A file is read when its name matches any of
#'   them (case ignored) and ends in \code{.xlsx}, \code{.xls} or
#'   \code{.csv}. Excel lock files (\code{~$...}) are skipped. Each sheet of
#'   a workbook, and each CSV file, is one table.
#' @param dir.sub Logical, default \code{TRUE}. Also search sub-folders of
#'   \code{dir.load}.
#' @param form.list \code{NA} (default), the path to a workbook laid out like
#'   \code{Survey123_aruforms all headers.xlsx}, a data frame, or a named list
#'   of data frames. New form versions / tabs / choices to add to the internal
#'   Survey123 forms database for this run (see
#'   Details, "Adding forms").
#' @param dir.save Character, default \code{getwd()}. Folder the merged
#'   workbooks are written to (created if needed).
#' @param save.name Character, default \code{NULL}: each workbook is named
#'   \verb{<project.name>_aru.site.metadata_<YYYYMMDDHHMMSS>.xlsx}, and the
#'   all-projects workbook \verb{Multipleprojects_aru.site.metadata_<YYYYMMDDHHMMSS>.xlsx}
#'   (\verb{<YYYYMMDDHHMMSS>} = time of the run). A name given here replaces
#'   the \verb{aru.site.metadata_<YYYYMMDDHHMMSS>} part:
#'   \verb{<project.name>_<save.name>.xlsx} and
#'   \verb{Multipleprojects_<save.name>.xlsx} (a name without an
#'   \code{.xlsx} ending gets \code{.xlsx} in place of its extension).
#' @param project.name Character, default \code{""}. The projects placed
#'   into the output files: one or more project names (a vector, or one text
#'   with names separated by ";"), matched to \code{$project.name} ignoring
#'   case and spaces. Records of other projects are left out. Blank = every
#'   project in the files (including \code{"unknown.check"}). A name not in
#'   the files stops the run, listing the projects found.
#' @param project.merged \code{FALSE} (default), \code{TRUE} or
#'   \code{"BOTH"}. \code{FALSE}: one workbook for each project.
#'   \code{TRUE}: one workbook with all the projects.
#'   \code{"BOTH"}: one workbook for each project and one with all the
#'   projects. In file names, a project's name is spelled as in the data,
#'   with spaces as "-" and characters Windows does not allow removed.
#' @param header.combo Character, default \code{c("project.name", "aru.name")}.
#'   Headers whose unique combinations define a site: used for the
#'   deployment date (Details) and passed to
#'   \code{\link{batz.datawrangler_consolidate.globalID}}.
#' @param header.uniqueid Character, default \code{c("globalid")}. Passed to
#'   \code{batz.datawrangler_consolidate.globalID()}: the unique-ID header.
#' @param header.dates Character, default \code{c("date.habitat.assessment",
#'   "date.deployment", "date")}. Passed to
#'   \code{batz.datawrangler_consolidate.globalID()}: date headers used to find
#'   each site's earliest record (also for the deployment date, Details).
#' @param generate.arulist Logical, default \code{FALSE}. \code{TRUE} also
#'   writes an ARU list (\verb{*.arulist.csv}) of the sites kept (Details,
#'   "ARU list"), following \code{project.merged}:
#'   \verb{<project.name>_<YYYYMMDDHHMMSS>.arulist.csv} for each project and
#'   \verb{Multipleprojects_<YYYYMMDDHHMMSS>.arulist.csv} for all of them
#'   (with \code{save.name}: \verb{<project.name>_<save.name>.arulist.csv}).
#' @param sunregion \code{"discrete"} (default) or \code{"pooled"}. The ARU
#'   list's sun regions. \code{"discrete"}: \code{$sunregion},
#'   \code{$sunregion.longitude} and \code{$sunregion.latitude} are the
#'   site's \code{$aru.name}, \code{$longitude} and \code{$latitude}.
#'   \code{"pooled"}: \code{$sunregion} is the project name, and its
#'   coordinates are the central point (mean longitude and latitude) of all
#'   the project's sites.
#' @param date.buffer \code{"1 month"} (default), \code{"none"},
#'   \code{"<N> month"} (e.g. \code{"2 month"}; \code{"month"} = \code{"1
#'   month"}) or \code{"year"}. Sets \code{$sunregion_date.start} and
#'   \code{$sunregion_date.end} of the ARU list. \code{"none"}: the site's
#'   own \code{$date.start} and \code{$date.end}. \code{"1 month"}: the
#'   first day of the month of the project's earliest \code{$date.start}, and
#'   the last day of the month of its latest \code{$date.end}. \code{"<N>
#'   month"}: as \code{"1 month"}, extended by N - 1 more months at each end.
#'   \code{"year"}: January 1 of the year of the project's earliest
#'   \code{$date.start} and December 31 of the year of its latest
#'   \code{$date.end}.
#' @param time.zone Character, default \code{"America/New_York"}. The ARU
#'   list's \code{$time.zone} for every site.
#'
#' @return Invisibly, a list: one data frame per form type (\code{$quad},
#'   \code{$habitat}, \code{$service}, plus any tab added through
#'   \code{form.list}), \code{$log} (one row per sheet read: \code{$file},
#'   \code{$sheet}, \code{$tab}, \code{$form.version}, \code{$rows},
#'   \code{$headers.matched}, \code{$headers.not.used}), \code{$headers.db}
#'   (the Survey123 forms database used, including
#'   anything added through \code{form.list}) and \code{$file} (the
#'   workbooks written, named by project, \code{"Multipleprojects"} for the
#'   all-projects workbook). The data frames hold every project kept; each
#'   workbook holds its project's records. The row count of each tab of each
#'   workbook is printed. With \code{generate.arulist = TRUE}, also
#'   \code{$arulist} (the ARU list of every project kept, with
#'   \code{$project.name} first) and \code{$file.arulist} (the ARU list
#'   files written, named like \code{$file}).
#'
#' @details
#' \strong{Internal database.} The Survey123 forms database (a list of data
#' frames, one per tab; kept in Batz_reference_db, tabs
#' \code{DB_s123.forms.quad}, \code{DB_s123.forms.habitat},
#' \code{DB_s123.forms.service}, \code{DB_s123.forms.Choices}) holds the same
#' content as \code{Survey123_aruforms all
#' headers.xlsx}: for tabs \code{quad}, \code{habitat}, \code{service}, the
#' Batz header (\code{batz.headers}), the tab-qualified header
#' (\code{aru.<tab>_batz.headers}, e.g. \code{aru.quad$aru.name}, since some
#' tabs share a header that different functions such as
#' \code{batz.generate_aru.appendix()} use at different times) and one column
#' per form version (e.g. \code{forest_2026}, \code{inland_2026},
#' \code{service_2026}) giving that version's Survey123 header. Tab
#' \code{Choices}: \code{$list_name}, \code{$name} (the Survey123 code) and
#' \code{$label} (the cleaned-up text). Output Batz headers are lower case
#' (e.g. \code{objectid}, \code{globalid}, \code{parentglobalid},
#' \code{vegetation.tree_tree1.dbh}). A sheet that already has Batz headers
#' may use the old spellings \code{measurment.units} /
#' \code{microphone.callibration_value} (and the others in
#' \code{survey123.ref.renamed()}); they are read as the corrected headers.
#'
#' \strong{Adding forms (\code{form.list}).} Each tab given is merged into the
#' database for this run: tabs \code{quad}, \code{habitat}, \code{service} and
#' \code{Choices} add their versions / choices to the existing ones (a version
#' already in the database is replaced), and any other tab becomes a new form
#' type (and a new tab in the output). Every header tab needs at least three
#' columns: \code{batz.headers}, \code{aru.<tabname>_batz.headers} (any column
#' whose name starts with \code{aru.} and contains \code{_batz.headers}, case
#' ignored; it is renamed to \code{aru.<tabname>_batz.headers}) and one or
#' more form-version columns. The tab name comes from the sheet / list-element
#' name, else from the \code{aru.<tab>$} prefix of the tab-qualified headers.
#' A \code{Choices} tab needs \code{list_name}, \code{name} and \code{label}.
#' The package's own copy is not changed.
#'
#' \strong{Steps.}
#' \enumerate{
#'   \item One empty data frame per form type, with \code{$form.version}
#'     first, then each Batz header once (in the database's order).
#'   \item Every sheet of every matching workbook, and every matching CSV
#'     file, is compared with each form
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
#'     and \code{Specify Other Site} both give \code{aru.name}) the first
#'     non-blank value is used, and \code{"other"} is replaced by the
#'     "specify" value.
#'   \item The sheet's rows are appended to that form type's data frame with
#'     \code{$form.version} = the version name (e.g. \code{inland_2026}); a
#'     sheet that already has Batz headers keeps its own \code{$form.version}
#'     (else \code{"batz.headers"}).
#'   \item Project names are filled in; with \code{project.name}, records of
#'     other projects (including \code{"unknown.check"}) are left out (a line
#'     per tab gives how many), and a name not in the files stops the run
#'     listing the projects found. Blank deployment dates are filled,
#'     the habitat \code{$date} is set and the consolidated GlobalID is
#'     added (below). A column whose type differs between files becomes text.
#'   \item DBH of trees and snags with more than one trunk (below).
#'   \item The workbooks set by \code{project.merged} are written, one tab
#'     per form type, with the \code{openxlsx} package.
#' }
#' Values are copied as they are; Survey123 codes are not changed to their
#' Choices labels.
#'
#' \strong{CSV files.} A CSV file is read as text (UTF-8, with a Latin-1
#' fallback) and treated like one sheet; its \code{$log$sheet} is the file
#' name without \code{.csv}. A column whose values are all dates or
#' date-times (e.g. \code{6/26/2026 3:45:12 PM}, \code{2026-06-26 15:45:12})
#' is read as a date-time, like the same column in a workbook. For the
#' project name (below), the CSV files in one folder count as one file, so a
#' quadrant CSV finds its project name in the habitat CSV beside it.
#'
#' \strong{Workbooks that can't be opened.} Each file is checked before it is
#' read, and the run stops with a plain message when it is:
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
#' \strong{Project name from the same file.} Every tab gets
#' \code{$project.name} (added just before \code{$aru.name} where a tab has
#' none, e.g. \code{quad}). A record with no project name takes it from a
#' record in another tab (sheet) of the same input file that shares, in this
#' order: a GlobalID (any header containing "globalid", so a quadrant's
#' \code{parentglobalid} finds its habitat record's \code{globalid}), a site
#' (\code{site} / \code{site.name}), or an \code{aru.name}. When several
#' project names match, the most common is used. Records with no match get
#' \code{"unknown.check"}, and the run prints how many there are.
#'
#' \strong{Deployment date.} A record with \code{$date.habitat.assessment}
#' but a blank \code{$date.deployment} gets \code{$date.deployment} =
#' \code{$date.habitat.assessment} when no other record of the same site
#' (the \code{header.combo} headers, e.g. project and ARU, in any tab) has an
#' earlier date (the first non-blank of \code{header.dates}). A NOTE gives
#' the number of records filled. This is done before the consolidated
#' GlobalID, so the copied date counts there.
#'
#' \strong{Habitat date.} In the \code{habitat} tab, \code{$date} is
#' always set to the earlier of \code{$date.habitat.assessment} and
#' \code{$date.deployment} (after the deployment date above is filled),
#' with the time removed, as text \code{"YYYY-MM-DD"} (e.g.
#' \code{"2026-05-10"}); blank when both are blank. Any value from the
#' Survey123 field \code{Date_YYYYMMDD} is replaced.
#'
#' \strong{Consolidated GlobalID.} After the merge,
#' \code{batz.datawrangler_consolidate.globalID()} is run on the merged tabs
#' that have rows, with \code{header.combo}, \code{header.uniqueid} and
#' \code{header.dates}: the GlobalID of each site's earliest record (across
#' all tabs) is added to every record of that site as
#' \code{$globalid_consolidate}, just after \code{$globalid}. A tab without all
#' the \code{header.combo} headers is left unchanged (NOTE).
#'
#' \strong{ARU list.} With \code{generate.arulist = TRUE}, one row per site
#' (\code{header.combo}: project name + \code{$aru.name}) with the headers
#' \code{$aru.name}, \code{$longitude}, \code{$latitude},
#' \code{$date.start}, \code{$date.end}, \code{$time.zone},
#' \code{$sunregion}, \code{$sunregion.longitude},
#' \code{$sunregion.latitude}, \code{$sunregion_date.start},
#' \code{$sunregion_date.end} (dates as m/d/Y, e.g. \code{6/26/2026}). Sites
#' are listed in the order of the habitat tab, then sites found only in the
#' service tab.
#' \itemize{
#'   \item \code{$longitude}, \code{$latitude}: the site's habitat
#'     record; a site with none takes them from its latest service record.
#'   \item \code{$date.start}: the site's earliest habitat
#'     \code{$date.deployment}.
#'   \item \code{$date.end}: the site's latest service record with
#'     \code{$visit.reason} \code{"retrieval"}.
#' }
#' Coordinates are rounded to 8 decimals.
#' A site with no coordinates, deployment date or retrieval record keeps
#' that cell blank and is named in a NOTE. The files hold no project-name
#' column; the returned \code{$arulist} has one.
#' \code{\link{batz.generate_suntimes.arulist}} reads these files and
#' calculates sun times over \code{$sunregion_date.start} to
#' \code{$sunregion_date.end}.
#'
#' \strong{DBH of trees and snags.} For \code{$vegetation.tree_tree1.dbh},
#' \code{$vegetation.tree_tree2.dbh}, \code{$vegetation.tree_tree3.dbh},
#' \code{$vegetation.snag_snag1.dbh}, \code{$vegetation.snag_snag2.dbh} and
#' \code{$vegetation.snag_snag3.dbh}, any column with the same name plus a
#' number (e.g. \code{$vegetation.tree_tree1.dbh2},
#' \code{$vegetation.tree_tree1.dbh3}) or \code{_other}
#' (\code{$vegetation.tree_tree1.dbh_other}) is merged into it: the
#' non-blank values, in the order dbh, dbh2, dbh3, ..., dbh_other, separated
#' by ";" (e.g. \code{"6;8;10"}), and the extra columns are removed. A new
#' column \code{<name>.overall} (e.g. \code{$vegetation.tree_tree1.dbh.overall})
#' is placed right after each one: the square root of the sum of the squared
#' DBH values, to 3 decimals (6, 8 and 10 give
#' \eqn{\sqrt{6^2 + 8^2 + 10^2}} = 14.142; one value gives that value).
#' Values that are not numbers are left out of \code{.overall}.
#' Survey123's own average columns (\code{$vegetation.tree_tree1.dbh_average},
#' \code{tree2}, \code{tree3}) are removed.
#'
#' \strong{Survey123 field names.} Batz headers for some long Survey123
#' fields: \code{tree.1/2/3.average.dbh} become
#' \code{vegetation.tree_tree1/2/3.dbh_average} (removed from the output,
#' see "DBH of trees and snags");
#' \code{identify.the.species.of.the.3.saplings.closest.to.the.center.mic.point.}
#' becomes \code{vegetation.sapling_species};
#' \code{select.any.equipment.that.needs.to.be.replaced.} becomes
#' \code{equipment.replace}; \code{specify.other.equipment.} becomes
#' \code{equipment.replace_other}; \code{specify.other.action.performed}
#' becomes \code{action.performed_other}.
#'
#' @seealso \code{\link{batz.datawrangler_consolidate.globalID}}
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
                                           load.pattern = c("*aruHabitat*.xlsx",
                                                            "*aruRetrieval*.xlsx",
                                                            "*aruDeployment*.xlsx",
                                                            "*aruMaintenance*.xlsx",
                                                            "*aruHabitat*.csv",
                                                            "*aruRetrieval*.csv",
                                                            "*aruDeployment*.csv",
                                                            "*aruMaintenance*.csv"),
                                           dir.sub = TRUE,
                                           form.list = NA,
                                           dir.save = getwd(),
                                           save.name = NULL,
                                           project.name = "",
                                           project.merged = FALSE,
                                           header.combo = c("project.name", "aru.name"),
                                           header.uniqueid = c("globalid"),
                                           header.dates = c("date.habitat.assessment", "date.deployment", "date"),
                                           generate.arulist = FALSE,
                                           sunregion = "discrete",
                                           date.buffer = "1 month",
                                           time.zone = "America/New_York") {

  if (!requireNamespace("readxl", quietly = TRUE)) stop("The readxl package is needed: install.packages(\"readxl\")", call. = FALSE)
  if (!dir.exists(dir.load)) stop("dir.load not found: ", dir.load, call. = FALSE)
  pm <- if (length(project.merged) == 1 && !is.na(project.merged)) toupper(trimws(as.character(project.merged))) else ""
  if (!pm %in% c("FALSE", "TRUE", "BOTH"))
    stop('project.merged must be FALSE, TRUE or "BOTH".', call. = FALSE)
  if (isTRUE(generate.arulist)) {
    sunregion <- tolower(trimws(as.character(sunregion[1])))
    if (!sunregion %in% c("discrete", "pooled")) stop('sunregion must be "discrete" or "pooled".', call. = FALSE)
    sitevisit.date.buffer(date.buffer)              # stops on an unusable value, before any work
  }
  key <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
  is.blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x))) | tolower(trimws(as.character(x))) %in% c("na", "n/a")

  ## --- 1. header database (+ form.list) ----------------------------------
  db <- survey123.forms()                     # Batz_reference_db tabs DB_s123.forms.*
  ch <- db$Choices
  tab.col <- function(d) grep("^aru\\..*_Batz\\.headers", names(d), ignore.case = TRUE)[1]
  ## the database's tabs as one long table: tab, row, batz.header, version,
  ## survey123.header
  to.long <- function(db) {
    out <- list()
    for (t in setdiff(names(db), "Choices")) {
      d <- db[[t]]; th <- tab.col(d); bh <- which(tolower(names(d)) == "batz.headers")[1]
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
      stop("form.list tab '", nm, "' needs at least three columns: batz.headers, aru.<tabname>_batz.headers ",
           "and one or more form-version columns.", call. = FALSE)
    }
    if (!nzchar(nm)) {                        # tab name from aru.<tab>$...
      pre <- sub("^aru\\.([^$]+)\\$.*$", "\\1", d[[th]][grepl("^aru\\.[^$]+\\$", d[[th]])])
      nm <- if (length(pre) > 0) names(sort(table(pre), decreasing = TRUE))[1] else
        sub("^aru\\.(.+?)_Batz\\.headers.*$", "\\1", names(d)[th], ignore.case = TRUE)
    }
    d[] <- lapply(d, function(v) trimws(as.character(v)))
    d <- d[!is.blank(d[[bh]]), , drop = FALSE]
    names(d)[th] <- paste0("aru.", nm, "_batz.headers"); names(d)[bh] <- "batz.headers"
    vers <- names(d)[-c(th, bh)]
    if (!(nm %in% names(db))) {
      db[[nm]] <- d[, c(th, bh, setdiff(seq_along(d), c(th, bh))), drop = FALSE]
    } else {
      ## rows matched on the tab header + Batz header (+ how many times that
      ## pair has come up, for headers listed twice); a version already in
      ## the database is replaced
      old <- db[[nm]]; names(old)[tab.col(old)] <- names(d)[th]
      kc <- c(names(d)[th], "batz.headers")
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
  parts.file <- lapply(setNames(tabs, tabs), function(t) character(0))   # input file of each part (2026-10-30)

  ## --- 3. find the files ---------------------------------------------------
  all.f <- list.files(dir.load, recursive = isTRUE(dir.sub), full.names = TRUE)
  rx <- vapply(load.pattern, utils::glob2rx, character(1))
  files <- all.f[vapply(basename(all.f), function(b) any(grepl(paste(rx, collapse = "|"), b, ignore.case = TRUE)), logical(1))]
  files <- files[!grepl("^~\\$", basename(files)) & grepl("\\.(xlsx?|csv)$", files, ignore.case = TRUE)]
  if (length(files) == 0) {
    stop("No files in ", dir.load, " match load.pattern: ", paste(load.pattern, collapse = ", "), call. = FALSE)
  }

  ## --- 4. read, identify, rename, append ----------------------------------
  log <- list()
  versions <- unique(hdr[, c("tab", "version")])
  for (f in files) {
    is.csv <- grepl("\\.csv$", f, ignore.case = TRUE)
    for (s in if (is.csv) sitevisit.csv.name(f) else xl.sheets(f)) {
      d <- if (is.csv) sitevisit.read.csv(f) else
        as.data.frame(readxl::read_excel(f, s, .name_repair = "minimal"), check.names = FALSE, stringsAsFactors = FALSE)
      if (ncol(d) == 0) next
      src <- key(names(d))
      ## Survey123's own system fields (ObjectID, GlobalID, Creator, x, y ...)
      ## are in every form, so they don't count when identifying the form
      sys.keys <- c("objectid", "globalid", "parentglobalid", "creationdate", "creator", "editdate", "editor", "x", "y")
      score.src <- setdiff(unique(src[nzchar(src)]), sys.keys)
      ## score every version, plus each tab's own Batz headers
      cand <- rbind(versions, data.frame(tab = tabs, version = "batz.headers", stringsAsFactors = FALSE))
      cand$matched <- vapply(seq_len(nrow(cand)), function(k) {
        h <- hdr[hdr$tab == cand$tab[k], , drop = FALSE]
        ref <- if (cand$version[k] == "batz.headers") c(h$batz.header, renamed$header.old[renamed$header.new %in% h$batz.header]) else
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
      if (best$version == "batz.headers") {
        map <- data.frame(from = h$batz.header, to = h$batz.header, stringsAsFactors = FALSE)
        old <- renamed[renamed$header.new %in% h$batz.header, , drop = FALSE]
        map <- unique(rbind(map, data.frame(from = old$header.old, to = old$header.new, stringsAsFactors = FALSE)))
        ## an already-standardized sheet keeps its own form.version if it has one
        fv <- if ("form.version" %in% names(d)) as.character(d$form.version) else "batz.headers"
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
      ## CSV exports have one table per file: the CSV files of one folder count as one file
      parts.file[[t]] <- c(parts.file[[t]], if (is.csv) file.path(dirname(f), "*.csv") else f)
      log[[length(log) + 1]] <- data.frame(file = basename(f), sheet = s, tab = t, form.version = fv[1], rows = nrow(d),
                                           headers.matched = best$matched, headers.not.used = paste(unused, collapse = "; "),
                                           stringsAsFactors = FALSE)
    }
  }

  ## --- 4b. project.name from the other tabs of the same file (2026-10-30, per Josh)
  ## Every tab gets $project.name. A record without one takes it from a record
  ## in another tab of the same input file with the same GlobalID /
  ## ParentGlobalID (any *globalid* header), else the same site (site /
  ## site.name), else the same aru.name; if none is found: "unknown.check".
  pj <- "project.name"
  for (t in tabs) {
    if (pj %in% names(out[[t]])) next
    at <- match("aru.name", names(out[[t]]))
    at <- if (is.na(at)) match("form.version", names(out[[t]])) else at - 1
    add.pj <- function(x) {
      x[[pj]] <- if (nrow(x) > 0) NA_character_ else character(0)
      x[, append(setdiff(names(x), pj), pj, after = at), drop = FALSE]
    }
    out[[t]] <- add.pj(out[[t]])
    parts[[t]] <- lapply(parts[[t]], add.pj)
  }
  gid.cols <- function(x) grep("globalid", names(x), ignore.case = TRUE, value = TRUE)
  site.cols <- function(x) names(x)[key(names(x)) %in% c("site", "sitename")]
  key.vals <- function(x, cols) {           # one character vector of values per row, over several columns
    if (length(cols) == 0) return(rep(list(character(0)), nrow(x)))
    m <- vapply(cols, function(cl) tolower(trimws(as.character(x[[cl]]))), character(nrow(x)))
    m <- matrix(m, nrow = nrow(x))
    lapply(seq_len(nrow(x)), function(r) { v <- m[r, ]; v[!is.na(v) & nzchar(v) & v != "na"] })
  }
  n.unknown <- 0
  pieces <- do.call(rbind, lapply(tabs, function(t) if (length(parts[[t]]) > 0)
    data.frame(tab = t, k = seq_along(parts[[t]]), file = parts.file[[t]], stringsAsFactors = FALSE)))
  if (!is.null(pieces)) for (i in seq_len(nrow(pieces))) {
    x <- parts[[pieces$tab[i]]][[pieces$k[i]]]
    need <- which(is.blank(x[[pj]]))
    if (length(need) == 0) next
    others <- pieces[pieces$file == pieces$file[i] & !(pieces$tab == pieces$tab[i] & pieces$k == pieces$k[i]), , drop = FALSE]
    donors <- lapply(seq_len(nrow(others)), function(j) parts[[others$tab[j]]][[others$k[j]]])
    donors <- Filter(function(y) any(!is.blank(y[[pj]])), donors)
    for (getcols in list(gid.cols, site.cols, function(y) intersect("aru.name", names(y)))) {
      if (length(need) == 0 || length(donors) == 0) break
      lookup <- do.call(rbind, lapply(donors, function(y) {
        y <- y[!is.blank(y[[pj]]), , drop = FALSE]
        kv <- key.vals(y, getcols(y))
        if (length(unlist(kv)) == 0) return(NULL)
        data.frame(val = unlist(kv), proj = rep(as.character(y[[pj]]), lengths(kv)), stringsAsFactors = FALSE)
      }))
      if (is.null(lookup)) next
      kv <- key.vals(x[need, , drop = FALSE], getcols(x))
      found <- vapply(kv, function(v) {
        p <- lookup$proj[lookup$val %in% v]
        if (length(p) == 0) NA_character_ else names(sort(table(p), decreasing = TRUE))[1]
      }, character(1))
      x[[pj]][need[!is.na(found)]] <- found[!is.na(found)]
      need <- need[is.na(found)]
    }
    if (length(need) > 0) { x[[pj]][need] <- "unknown.check"; n.unknown <- n.unknown + length(need) }
    parts[[pieces$tab[i]]][[pieces$k[i]]] <- x
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

  ## --- 5-. keep only the projects in project.name ---------------------------
  pk <- function(x) gsub("\\s+", "", tolower(as.character(x)))
  pj.want <- if (is.null(project.name)) character(0) else trimws(unlist(strsplit(as.character(project.name), ";")))
  pj.want <- pj.want[!is.na(pj.want) & nzchar(pj.want)]
  if (length(pj.want) > 0) {
    found <- unique(unlist(lapply(tabs, function(t) if ("project.name" %in% names(out[[t]])) out[[t]]$project.name)))
    found <- sort(unique(found[!is.na(found) & nzchar(found)]))
    miss <- pj.want[!pk(pj.want) %in% pk(found)]
    if (length(miss) > 0)
      stop('project.name "', paste(miss, collapse = '", "'), '" not in the files. Projects found: ',
           paste(found, collapse = ", "), call. = FALSE)
    for (t in tabs) {
      d <- out[[t]]
      if (!"project.name" %in% names(d) || nrow(d) == 0) next
      keep <- pk(d$project.name) %in% pk(pj.want)
      if (any(!keep)) cat(t, ": ", sum(!keep), " record(s) of other projects left out\n", sep = "")
      out[[t]] <- d[keep, , drop = FALSE]; rownames(out[[t]]) <- NULL
    }
    n.unknown <- sum(vapply(tabs, function(t) sum(out[[t]]$project.name %in% "unknown.check"), numeric(1)))
  }

  ## --- 5a. deployment date from the habitat assessment date -----------------
  out[tabs] <- sitevisit.deploy.date(out[tabs], header.combo = header.combo, header.dates = header.dates)

  ## --- 5a2. habitat $date = the earlier of the two dates, as YYYY-MM-DD ------
  if ("habitat" %in% tabs) out$habitat <- sitevisit.habitat.date(out$habitat)

  ## --- 5b. consolidated GlobalID (2026-10-30, per Josh) ---------------------
  has.rows <- tabs[vapply(tabs, function(t) nrow(out[[t]]) > 0, logical(1))]
  if (length(has.rows) > 0) {
    out[has.rows] <- batz.datawrangler_consolidate.globalID(out[has.rows], header.combo = header.combo,
                                                            header.uniqueid = header.uniqueid, header.dates = header.dates)
  }

  ## --- 5c. DBH: extra trunks merged into one column + overall DBH -----------
  for (t in tabs) out[[t]] <- sitevisit.dbh.merge(out[[t]])

  ## --- 6. write (2026-11-06, per Josh: project.merged) -----------------------
  ## projects kept, spelled as in the data (first spelling of each)
  pj.all <- unlist(lapply(tabs, function(t) if ("project.name" %in% names(out[[t]])) as.character(out[[t]]$project.name)))
  pj.all <- pj.all[!is.na(pj.all) & nzchar(trimws(pj.all))]
  pj.all <- pj.all[!duplicated(pk(pj.all))]
  pj.all <- pj.all[order(tolower(pj.all))]
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  tail.name <- if (is.null(save.name) || !length(save.name) || is.na(save.name[1]) || !nzchar(trimws(save.name[1])))
    paste0("aru.site.metadata_", stamp, ".xlsx") else trimws(save.name[1])
  if (!grepl("\\.xlsx$", tail.name, ignore.case = TRUE)) tail.name <- paste0(sub("\\.[^.]*$", "", tail.name), ".xlsx")
  clean <- function(x) gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(x)))
  if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
  write.one <- function(tabs.out, lead) {
    path <- file.path(dir.save, paste0(clean(lead), "_", tail.name))
    openxlsx::write.xlsx(tabs.out, path, overwrite = TRUE)
    cat("Saved:", path, "\n")
    for (t in tabs) cat("  ", t, ": ", nrow(tabs.out[[t]]), " row(s)\n", sep = "")
    path
  }
  path <- character(0)
  if (pm %in% c("FALSE", "BOTH")) {
    if (length(pj.all) == 0) cat("NOTE: no project names in the files - no per-project workbook written.\n")
    for (p in pj.all) {
      one <- lapply(out[tabs], function(d) {
        if (!"project.name" %in% names(d)) return(d[0, , drop = FALSE])
        d <- d[pk(d$project.name) %in% pk(p), , drop = FALSE]; rownames(d) <- NULL; d
      })
      path[p] <- write.one(one, p)
    }
  }
  if (pm %in% c("TRUE", "BOTH") || length(pj.all) == 0) path["Multipleprojects"] <- write.one(out[tabs], "Multipleprojects")
  if (n.unknown > 0) cat(n.unknown, ' have no associated project name, the value  "unknown.check"\n', sep = "")

  ## --- 7. ARU list (2026-11-06, per Josh) -------------------------------------
  if (!isTRUE(generate.arulist)) return(invisible(c(out, list(log = log, headers.db = db, file = path))))
  al <- sitevisit.arulist(out, sunregion = sunregion, date.buffer = date.buffer, time.zone = time.zone)
  al.tail <- if (is.null(save.name) || !length(save.name) || is.na(save.name[1]) || !nzchar(trimws(save.name[1])))
    paste0(stamp, ".arulist.csv") else paste0(sub("\\.[^.]*$", "", trimws(save.name[1])), ".arulist.csv")
  write.al <- function(d, lead) {
    f <- file.path(dir.save, paste0(clean(lead), "_", al.tail))
    d <- d[, setdiff(names(d), "project.name"), drop = FALSE]
    utils::write.csv(d, f, row.names = FALSE, na = "")
    cat("Saved:", f, "(", nrow(d), "site(s) )\n")
    f
  }
  path.al <- character(0)
  al.pj <- unique(al$project.name)
  al.pj <- al.pj[order(tolower(al.pj))]
  if (pm %in% c("FALSE", "BOTH")) for (p in al.pj) path.al[p] <- write.al(al[al$project.name == p, , drop = FALSE], p)
  if (pm %in% c("TRUE", "BOTH") || length(al.pj) == 0) path.al["Multipleprojects"] <- write.al(al, "Multipleprojects")

  invisible(c(out, list(log = log, headers.db = db, file = path, arulist = al, file.arulist = path.al)))
}


#' Merge extra-trunk DBH columns and add the overall DBH (internal)
#'
#' For each of \code{vegetation.tree_tree1.dbh} ... \code{tree3.dbh} and
#' \code{vegetation.snag_snag1.dbh} ... \code{snag3.dbh}: columns with the
#' same name plus a number (\code{...dbh2}, \code{...dbh3}) or
#' \code{_other} (\code{...dbh_other}) are merged into it, values separated
#' by ";", and removed. \code{<column>.overall} = square root of the sum of
#' the squared DBH values, placed right after the column. \code{<column>_average}
#' (Survey123's average) is removed.
#' @keywords internal
#' @noRd
sitevisit.dbh.merge <- function(d) {
  if (is.null(d) || ncol(d) == 0) return(d)
  bases <- c(paste0("vegetation.tree_tree", 1:3, ".dbh"), paste0("vegetation.snag_snag", 1:3, ".dbh"))
  txt <- function(v) { v <- if (inherits(v, "POSIXct")) format(v) else as.character(v); v <- trimws(v); v[is.na(v)] <- ""; v }
  d <- d[, setdiff(names(d), paste0(bases, "_average")), drop = FALSE]   # Survey123's own averages
  for (b in bases) {
    if (!b %in% names(d)) next
    rx <- paste0("^", gsub(".", "\\.", b, fixed = TRUE), "(\\d+|_other)$")
    extra <- grep(rx, names(d), value = TRUE)
    ## order: dbh, dbh2, dbh3, ..., dbh_other
    num <- suppressWarnings(as.integer(sub(".*dbh", "", extra)))
    extra <- extra[order(is.na(num), num)]
    if (length(extra)) {
      vals <- lapply(c(b, extra), function(cl) txt(d[[cl]]))
      d[[b]] <- vapply(seq_len(nrow(d)), function(i) {
        v <- vapply(vals, `[`, character(1), i); v <- v[nzchar(v)]
        if (length(v)) paste(v, collapse = ";") else NA_character_
      }, character(1))
      d <- d[, setdiff(names(d), extra), drop = FALSE]
    }
    ov <- vapply(txt(d[[b]]), function(x) {
      v <- suppressWarnings(as.numeric(trimws(strsplit(x, ";", fixed = TRUE)[[1]])))
      v <- v[!is.na(v)]
      if (length(v)) round(sqrt(sum(v^2)), 3) else NA_real_
    }, numeric(1), USE.NAMES = FALSE)
    nm <- paste0(b, ".overall")
    d[[nm]] <- NULL
    at <- match(b, names(d))
    d <- cbind(d[seq_len(at)], stats::setNames(data.frame(ov), nm), d[-seq_len(at)])
  }
  d
}


#' Fill a blank deployment date from the habitat assessment date (internal)
#'
#' A record with \code{$date.habitat.assessment} but no
#' \code{$date.deployment} gets \code{$date.deployment} =
#' \code{$date.habitat.assessment} when no other record of the same site
#' (\code{header.combo}, any tab) has an earlier date (first non-blank of
#' \code{header.dates}). \code{tabs} is a named list of data frames.
#' @keywords internal
#' @noRd
sitevisit.deploy.date <- function(tabs, header.combo = c("project.name", "aru.name"),
                                  header.dates = c("date.habitat.assessment", "date.deployment", "date")) {
  ha <- "date.habitat.assessment"; dp <- "date.deployment"
  to.date <- sitevisit.to.date
  blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  key <- function(d) {
    if (!all(header.combo %in% names(d))) return(rep(NA_character_, nrow(d)))
    k <- do.call(paste, c(lapply(header.combo, function(h) tolower(trimws(as.character(d[[h]])))), sep = "\r"))
    k[Reduce(`|`, lapply(header.combo, function(h) blank(d[[h]])))] <- NA
    k
  }
  ## every record's date (first non-blank of header.dates) and site, across tabs
  recs <- do.call(rbind, lapply(names(tabs), function(t) {
    d <- tabs[[t]]
    if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
    dt <- rep(as.Date(NA), nrow(d))
    for (h in intersect(header.dates, names(d))) { f <- is.na(dt); dt[f] <- to.date(d[[h]])[f] }
    data.frame(tab = t, row = seq_len(nrow(d)), key = key(d), date = dt, stringsAsFactors = FALSE)
  }))
  if (is.null(recs)) return(tabs)
  recs <- recs[!is.na(recs$key) & !is.na(recs$date), , drop = FALSE]
  n <- 0
  for (t in names(tabs)) {
    d <- tabs[[t]]
    if (!is.data.frame(d) || nrow(d) == 0 || !all(c(ha, dp) %in% names(d))) next
    k <- key(d); hd <- to.date(d[[ha]])
    cand <- which(!blank(d[[ha]]) & !is.na(hd) & blank(d[[dp]]) & !is.na(k))
    for (i in cand) {
      other <- recs[recs$key == k[i] & !(recs$tab == t & recs$row == i), , drop = FALSE]
      if (any(other$date < hd[i])) next                              # an earlier record of this site
      v <- d[[ha]][i]
      if (inherits(d[[dp]], "POSIXct") && !inherits(v, "POSIXct")) v <- as.POSIXct(format(hd[i]), tz = "UTC")
      else if (!inherits(d[[dp]], "POSIXct") && inherits(v, "POSIXct")) {
        if (is.logical(d[[dp]]) && all(is.na(d[[dp]]))) d[[dp]] <- as.POSIXct(d[[dp]], tz = "UTC")
        else v <- format(v, "%Y-%m-%d %H:%M:%S")
      }
      if (inherits(d[[dp]], "POSIXct") && inherits(v, "POSIXct")) attr(d[[dp]], "tzone") <- attr(v, "tzone")   # same clock time
      d[[dp]][i] <- v; n <- n + 1
    }
    tabs[[t]] <- d
  }
  if (n > 0) cat("NOTE: batz.merge_sitevisit.acoustics - ", n, " record(s) had no date.deployment and no earlier record ",
                 "for that site: date.habitat.assessment copied to date.deployment.\n", sep = "")
  tabs
}


#' Date (no time) from a date / date-time / text / Excel-serial column (internal)
#' @keywords internal
#' @noRd
sitevisit.to.date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(format(v, "%Y-%m-%d")))
  v <- trimws(as.character(v)); out <- rep(as.Date(NA), length(v))
  for (fm in c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y", "%Y%m%d")) {
    todo <- is.na(out) & !is.na(v) & nzchar(v)
    if (any(todo)) out[todo] <- as.Date(substr(v[todo], 1, if (fm == "%Y%m%d") 8 else 10), format = fm, optional = TRUE)
  }
  todo <- is.na(out) & grepl("^[0-9]{5}(\\.[0-9]+)?$", v)               # Excel serial number
  if (any(todo)) out[todo] <- as.Date(floor(as.numeric(v[todo])), origin = "1899-12-30")
  out
}


#' Habitat tab $date from the habitat assessment and deployment dates (internal)
#'
#' \code{$date} = the earlier of \code{$date.habitat.assessment} and
#' \code{$date.deployment} (time removed), as text \code{"YYYY-MM-DD"};
#' blank when both are blank. Always replaces the existing value.
#' @keywords internal
#' @noRd
sitevisit.habitat.date <- function(d) {
  if (!is.data.frame(d) || nrow(d) == 0) return(d)
  src <- intersect(c("date.habitat.assessment", "date.deployment"), names(d))
  if (!length(src)) return(d)
  dts <- lapply(src, function(h) sitevisit.to.date(d[[h]]))
  dt <- do.call(pmin, c(dts, na.rm = TRUE))
  val <- ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
  if ("date" %in% names(d)) d$date <- val
  else {                                                     # add it after the last source column
    at <- max(match(src, names(d)))
    d <- cbind(d[seq_len(at)], data.frame(date = val, stringsAsFactors = FALSE), d[-seq_len(at)])
  }
  d
}


#' Table name of a CSV file: the file name without .csv (internal)
#' @keywords internal
#' @noRd
sitevisit.csv.name <- function(f) sub("\\.csv$", "", basename(f), ignore.case = TRUE)


#' Read one Survey123 CSV export like a workbook sheet (internal)
#'
#' Text read with special.read.csv() (UTF-8, Latin-1 fallback); blanks as
#' NA; a column whose non-blank values are all dates or date-times becomes
#' POSIXct (UTC clock time, as readxl reads Excel dates); a column of
#' numbers becomes numeric.
#' @keywords internal
#' @noRd
sitevisit.read.csv <- function(f) {
  nm <- basename(f)
  sz <- file.size(f)
  if (is.na(sz) || sz == 0) {
    stop(nm, " is empty (0 bytes) - if it is in OneDrive, it may not have finished downloading. ",
         "Open the folder, wait for the file to sync, and re-run code.", call. = FALSE)
  }
  d <- tryCatch(special.read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character",
                                 na.strings = c("", "NA")),
                error = function(e) stop(nm, " could not be read: ", conditionMessage(e), call. = FALSE))
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  rx.date <- "^[0-9]{1,4}[/-][0-9]{1,2}[/-][0-9]{1,4}([ T][0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?( ?[AaPp][Mm])?)?$"
  fmts <- c("%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
            "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d")
  for (j in seq_along(d)) {
    v <- trimws(d[[j]]); v[!is.na(v) & !nzchar(v)] <- NA
    ok <- !is.na(v)
    if (!any(ok)) { d[[j]] <- v; next }
    if (all(grepl(rx.date, v[ok]))) {
      out <- as.POSIXct(rep(NA_real_, length(v)), origin = "1970-01-01", tz = "UTC")
      for (fm in fmts) {
        todo <- ok & is.na(out)
        if (!any(todo)) break
        p <- as.POSIXct(strptime(v[todo], fm, tz = "UTC"))
        good <- !is.na(p)
        out[which(todo)[good]] <- p[good]
      }
      if (!any(ok & is.na(out))) { d[[j]] <- out; next }
    }
    num <- suppressWarnings(as.numeric(v[ok]))
    d[[j]] <- if (all(!is.na(num)) && !any(grepl("^0[0-9]", v[ok]))) suppressWarnings(as.numeric(v)) else v
  }
  d
}


#' date.buffer as months (internal): 0 = "none", N = "N month", -1 = "year"
#' @keywords internal
#' @noRd
sitevisit.date.buffer <- function(date.buffer) {
  b <- tolower(trimws(as.character(date.buffer[1])))
  if (is.na(b)) b <- ""
  if (b == "none") return(0)
  if (b == "year") return(-1)
  if (grepl("^([0-9]+)?\\s*months?$", b)) {
    n <- suppressWarnings(as.integer(sub("^([0-9]*).*$", "\\1", b)))
    if (is.na(n)) n <- 1L
    if (n >= 1) return(n)
  }
  stop('date.buffer must be "none", "1 month" (or "<N> month", e.g. "2 month") or "year" - got "',
       date.buffer[1], '".', call. = FALSE)
}


#' ARU list from the merged site-visit tabs (internal)
#'
#' One row per site (project.name + aru.name): coordinates from habitat
#' (else the latest service record), date.start = earliest habitat
#' date.deployment, date.end = latest service "retrieval" date; sun region
#' "discrete" (the site) or "pooled" (project name, mean coordinates);
#' sun-region dates from date.buffer. Returns a data frame with
#' $project.name first; dates as m/d/Y text.
#' @keywords internal
#' @noRd
sitevisit.arulist <- function(out, sunregion = "discrete", date.buffer = "1 month", time.zone = "America/New_York") {
  nb <- sitevisit.date.buffer(date.buffer)
  txt <- function(v) { v <- trimws(as.character(v)); v[is.na(v)] <- ""; v }
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  key <- function(p, a) paste(tolower(gsub("\\s+", "", txt(p))), tolower(gsub("\\s+", "", txt(a))), sep = "|")
  get <- function(d, h) if (!is.null(d) && h %in% names(d)) d[[h]] else rep(NA, if (is.null(d)) 0 else nrow(d))
  h <- out$habitat; sv <- out$service
  hk <- if (!is.null(h) && nrow(h)) key(get(h, "project.name"), get(h, "aru.name")) else character(0)
  sk <- if (!is.null(sv) && nrow(sv)) key(get(sv, "project.name"), get(sv, "aru.name")) else character(0)
  sites <- unique(c(hk, sk))
  sites <- sites[!grepl("\\|$", sites)]                          # no aru.name
  mdy <- function(d) ifelse(is.na(d), "", sprintf("%d/%d/%s", as.integer(format(d, "%m")), as.integer(format(d, "%d")), format(d, "%Y")))
  rows <- lapply(sites, function(k) {
    ih <- which(hk == k); is <- which(sk == k)
    src <- if (length(ih)) h[ih[1], ] else sv[is[1], ]
    lon <- num(get(h, "longitude")[ih]); lat <- num(get(h, "latitude")[ih])
    lon <- lon[!is.na(lon)]; lat <- lat[!is.na(lat)]
    if (!length(lon) || !length(lat)) {                           # latest service record with coordinates
      sl <- num(get(sv, "longitude")[is]); sa <- num(get(sv, "latitude")[is]); sd <- sitevisit.to.date(get(sv, "date")[is])
      ok <- !is.na(sl) & !is.na(sa)
      if (any(ok)) { o <- which(ok)[order(sd[ok], decreasing = TRUE, na.last = TRUE)][1]; lon <- sl[o]; lat <- sa[o] }
    }
    ds <- sitevisit.to.date(get(h, "date.deployment")[ih]); ds <- ds[!is.na(ds)]
    rr <- is[grepl("retriev", tolower(txt(get(sv, "visit.reason")[is])))]
    de <- sitevisit.to.date(get(sv, "date")[rr]); de <- de[!is.na(de)]
    data.frame(project.name = txt(src$project.name), aru.name = txt(src$aru.name),
               longitude = if (length(lon)) lon[1] else NA_real_, latitude = if (length(lat)) lat[1] else NA_real_,
               date.start = if (length(ds)) min(ds) else as.Date(NA), date.end = if (length(de)) max(de) else as.Date(NA),
               stringsAsFactors = FALSE)
  })
  al <- if (length(rows)) do.call(rbind, rows) else
    data.frame(project.name = character(0), aru.name = character(0), longitude = numeric(0), latitude = numeric(0),
               date.start = as.Date(character(0)), date.end = as.Date(character(0)))
  al$longitude <- round(al$longitude, 8); al$latitude <- round(al$latitude, 8)   # as in Survey123 (8 decimals)
  al$time.zone <- rep(time.zone[1], nrow(al))
  ## sun region
  if (sunregion == "pooled") {
    al$sunregion <- al$project.name
    al$sunregion.longitude <- stats::ave(al$longitude, al$project.name, FUN = function(v) mean(v, na.rm = TRUE))
    al$sunregion.latitude  <- stats::ave(al$latitude,  al$project.name, FUN = function(v) mean(v, na.rm = TRUE))
    al$sunregion.longitude[is.nan(al$sunregion.longitude)] <- NA
    al$sunregion.latitude[is.nan(al$sunregion.latitude)] <- NA
    al$sunregion.longitude <- round(al$sunregion.longitude, 8); al$sunregion.latitude <- round(al$sunregion.latitude, 8)
  } else {
    al$sunregion <- al$aru.name; al$sunregion.longitude <- al$longitude; al$sunregion.latitude <- al$latitude
  }
  ## sun-region dates
  mfirst <- function(d) as.Date(format(d, "%Y-%m-01"))
  madd <- function(d, n) { if (is.na(d) || n == 0) return(d); seq(d, by = paste(n, "months"), length.out = 2)[2] }
  sds <- rep(as.Date(NA), nrow(al)); sde <- rep(as.Date(NA), nrow(al))
  if (nb == 0) { sds <- al$date.start; sde <- al$date.end }
  else for (p in unique(al$project.name)) {
    i <- al$project.name == p
    a <- suppressWarnings(min(al$date.start[i], na.rm = TRUE)); b <- suppressWarnings(max(al$date.end[i], na.rm = TRUE))
    if (!is.finite(a)) a <- as.Date(NA); if (!is.finite(b)) b <- as.Date(NA)
    if (nb == -1) {
      if (!is.na(a)) a <- as.Date(paste0(format(a, "%Y"), "-01-01"))
      if (!is.na(b)) b <- as.Date(paste0(format(b, "%Y"), "-12-31"))
    } else {
      if (!is.na(a)) a <- madd(mfirst(a), -(nb - 1))
      if (!is.na(b)) b <- madd(mfirst(b), nb) - 1                 # last day of the month, + (N - 1) months
    }
    sds[i] <- a; sde[i] <- b
  }
  al$sunregion_date.start <- mdy(sds); al$sunregion_date.end <- mdy(sde)
  al$date.start <- mdy(al$date.start); al$date.end <- mdy(al$date.end)
  ## sites missing a value
  for (h2 in c("longitude", "latitude", "date.start", "date.end")) {
    miss <- is.na(al[[h2]]) | (is.character(al[[h2]]) & !nzchar(al[[h2]]))
    if (any(miss)) cat("NOTE: ARU list - no ", switch(h2, longitude = "longitude", latitude = "latitude",
                                                      date.start = "deployment date (date.start)",
                                                      date.end = "retrieval record (date.end)"),
                       " for ", sum(miss), " site(s): ", paste(al$project.name[miss], "/", al$aru.name[miss], collapse = "; "),
                       " - left blank.\n", sep = "")
  }
  rownames(al) <- NULL
  al[, c("project.name", "aru.name", "longitude", "latitude", "date.start", "date.end", "time.zone", "sunregion",
         "sunregion.longitude", "sunregion.latitude", "sunregion_date.start", "sunregion_date.end")]
}

#' Excel template of every fig.list heading, with descriptions
#'
#' Writes one Excel workbook with two tabs:
#' \describe{
#'   \item{\code{fig.list}}{Every fig.list heading used by the Batz plot
#'     functions, as columns in the same order for every plot type, with one
#'     starter row per plot function (\code{$plot.type} filled in). A
#'     heading that plot type does not use holds \code{"NA"}; the headings it
#'     uses are blank, ready to fill in. Fill in the rows you need and delete
#'     the others; each plot function only plots rows of its own
#'     \code{$plot.type}, and treats \code{"NA"} cells as blank.}
#'   \item{\code{descriptions}}{One row per heading: \code{$header},
#'     \code{$required.in} and \code{$optional.in} (the plot functions that
#'     need or can use it) and \code{$description}.}
#' }
#' The headings and descriptions are read from the installed Batz functions
#' and their help pages when the function runs, so the workbook always
#' matches the installed version.
#'
#' @param functions Character, default all plot functions that take a
#'   fig.list: \code{"batz.plot_bar.bat.group"},
#'   \code{"batz.plot_heat.site.species"},
#'   \code{"batz.plotactivity_observations"},
#'   \code{"batz.plotdetections_first.last"},
#'   \code{"batz.plotsm4_heatmap"}, \code{"batz.plotactivity_heatmap2"}.
#'   Plot functions to include.
#' @param project.name Character, default \code{""}. First part of the file
#'   name. Blank = no project part.
#' @param dir.save Character, default \code{getwd()}. Folder the workbook is
#'   saved in (created if needed). \code{NULL} = no file.
#'
#' @return Invisibly, a list: \code{$fig.list} (the template data frame),
#'   \code{$descriptions} (data frame: \code{$header}, \code{$required.in},
#'   \code{$optional.in}, \code{$description}) and \code{$file} (the
#'   workbook, or \code{NA}).
#'
#' @details
#' \strong{Headings.} For each function, the required and optional fig.list
#' headings are the ones the function checks for. \code{$plot.type} and
#' \code{$plot.name} come first, then the other headings in the order the
#' functions list them. Besides these, a fig.list row can hold a column for
#' any plotopts setting of that function (e.g. \code{legend.position}),
#' which is used for that row before plotopts; those columns are not listed
#' here.
#'
#' \strong{Descriptions.} Taken from the "fig.list columns" part of each
#' function's help page (for a heading that is also a function argument,
#' such as \code{bin.colors}, from that argument's description). When the functions describe a heading differently,
#' each description is given, starting with the function name.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]fig.list_template_<YYYYMMDDHHMMSS>.xlsx}.
#'
#' @seealso \code{\link{batz.generate_plotopts}}
#'
#' @examples
#' \dontrun{
#' f <- batz.generate_figlist(dir.save = "C:/data/figures")
#' f$descriptions
#' }
#' @export
batz.generate_figlist <- function(functions = c("batz.plot_bar.bat.group", "batz.plot_heat.site.species",
                                                "batz.plotactivity_observations", "batz.plotdetections_first.last",
                                                "batz.plotsm4_heatmap", "batz.plotactivity_heatmap2"),
                                  project.name = "", dir.save = getwd()) {
  FN <- "batz.generate_figlist"
  ns <- asNamespace("Batz")
  bad <- functions[!vapply(functions, exists, logical(1), envir = ns, inherits = FALSE)]
  if (length(bad)) stop(FN, ": not a Batz function: ", paste(bad, collapse = ", "), call. = FALSE)
  ## settings that heatmap2 reads from fig.list besides its checked headings
  EXTRA.OPTIONAL <- list(batz.plotactivity_heatmap2 = c("date.format", "xaxe.interval", "xaxe.title", "yaxe.title"))

  ## ---- headings each function checks for (read from the function itself) -------
  find.assign <- function(expr, name) {
    out <- NULL
    walk <- function(e) {
      if (!is.null(out) || !is.call(e)) return()
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name))) {
        out <<- tryCatch(eval(e[[3]], baseenv()), error = function(err) NULL); return() }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
    walk(expr); as.character(out)
  }
  heads <- lapply(functions, function(f) {
    b <- body(get(f, envir = ns))
    req <- find.assign(b, "FIG.LIST.REQUIRED")
    opt <- c(find.assign(b, "FIG.LIST.OPTIONAL"), if (f == "batz.plotactivity_heatmap2") find.assign(b, "arg.names"),
             EXTRA.OPTIONAL[[f]])
    list(required = unique(req), optional = setdiff(unique(opt), req))
  })
  names(heads) <- functions

  ## ---- descriptions from the help pages ("fig.list columns" items) ------------
  rd.db <- tryCatch(tools::Rd_db("Batz"), error = function(e) list())
  rd.text <- function(x) paste(unlist(lapply(x, function(e) if (is.list(e)) rd.text(e) else as.character(e))), collapse = "")
  descs <- lapply(functions, function(f) {
    rd <- rd.db[[paste0(f, ".Rd")]]
    if (is.null(rd)) return(list())
    items <- list()
    walk <- function(x) {
      if (!is.list(x)) return()
      if (identical(attr(x, "Rd_tag"), "\\item") && length(x) == 2) {
        nm <- regmatches(rd.text(x[[1]]), gregexpr("\\$[A-Za-z0-9._]+", rd.text(x[[1]])))[[1]]
        d <- gsub("\\s+", " ", trimws(rd.text(x[[2]])))
        for (n in sub("^\\$", "", nm)) if (is.null(items[[n]])) items[[n]] <<- d
      }
      for (e in x) walk(e)
    }
    for (sec in rd) if (attr(sec, "Rd_tag") %in% c("\\details", "\\section")) walk(sec)
    ## a heading that is also a function argument (e.g. heatmap2's bin.colors): its argument text
    for (sec in rd) if (identical(attr(sec, "Rd_tag"), "\\arguments")) for (x in sec)
      if (identical(attr(x, "Rd_tag"), "\\item") && length(x) == 2) {
        n <- trimws(rd.text(x[[1]]))
        if (is.null(items[[n]])) items[[n]] <- gsub("\\s+", " ", trimws(rd.text(x[[2]])))
      }
    items
  })
  names(descs) <- functions

  ## ---- heading order and tables ----------------------------------------------
  all.h <- unique(c("plot.type", "plot.name", unlist(lapply(heads, function(h) c(h$required, h$optional)))))
  ptype <- vapply(functions, function(f) {
    b <- body(get(f, envir = ns)); v <- find.assign(b, "PLOT.TYPE")
    if (length(v)) v[1] else c(batz.plotactivity_observations = "call.observations",
                               batz.plotdetections_first.last = "bat.detection")[[f]]
  }, character(1))
  fl <- as.data.frame(matrix("", nrow = length(functions), ncol = length(all.h), dimnames = list(NULL, all.h)),
                      stringsAsFactors = FALSE, check.names = FALSE)
  fl$plot.type <- unname(ptype)
  for (i in seq_along(functions)) {                    # "NA" = heading not used by this plot type
    used <- c(heads[[functions[i]]]$required, heads[[functions[i]]]$optional)
    fl[i, !names(fl) %in% c("plot.type", used)] <- "NA"
  }

  short <- function(f) sub("^batz\\.", "", f)
  desc <- do.call(rbind, lapply(all.h, function(h) {
    req <- functions[vapply(heads, function(x) h %in% x$required, logical(1))]
    opt <- functions[vapply(heads, function(x) h %in% x$optional, logical(1))]
    d <- unlist(lapply(c(req, opt), function(f) descs[[f]][[h]]))
    names(d) <- unlist(lapply(c(req, opt), function(f) if (!is.null(descs[[f]][[h]])) f))
    d.txt <- if (!length(d)) "" else if (length(unique(d)) == 1) unique(d)[1] else
      paste0(short(names(d)), ": ", d, collapse = "\n")
    data.frame(header = h, required.in = paste(req, collapse = "; "), optional.in = paste(opt, collapse = "; "),
               description = d.txt, stringsAsFactors = FALSE)
  }))
  rownames(desc) <- NULL

  ## ---- write ------------------------------------------------------------------
  file <- NA_character_
  if (!is.null(dir.save)) {
    if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    pj <- if (!is.null(project.name) && nzchar(trimws(project.name)))
      paste0(gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(project.name))), "_") else ""
    file <- file.path(dir.save, paste0(pj, "fig.list_template_", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx"))
    wb <- openxlsx::createWorkbook()
    bold <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "bottom")
    wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
    openxlsx::addWorksheet(wb, "fig.list")
    openxlsx::writeData(wb, "fig.list", fl, headerStyle = bold)
    openxlsx::setColWidths(wb, "fig.list", seq_along(all.h), widths = pmax(12, nchar(all.h) + 3))
    openxlsx::freezePane(wb, "fig.list", firstRow = TRUE)
    openxlsx::addWorksheet(wb, "descriptions")
    openxlsx::writeData(wb, "descriptions", desc, headerStyle = bold)
    openxlsx::setColWidths(wb, "descriptions", 1:4, widths = c(20, 34, 34, 100))
    openxlsx::addStyle(wb, "descriptions", wrap, rows = 2:(nrow(desc) + 1), cols = 1:4, gridExpand = TRUE)
    openxlsx::freezePane(wb, "descriptions", firstRow = TRUE)
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    cat("Saved:", file, "\n")
  }
  cat(sprintf("%d fig.list heading(s) from %d plot function(s).\n", length(all.h), length(functions)))
  invisible(list(fig.list = fl, descriptions = desc, file = file))
}

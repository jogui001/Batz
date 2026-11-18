#' How a Batz function hands over its output files: save.auto (internal)
#'
#' Every Batz function with a \code{dir.save} argument also has
#' \code{save.auto}:
#' \describe{
#'   \item{\code{TRUE} (default)}{The files are saved in \code{dir.save} as
#'     usual.}
#'   \item{\code{FALSE}}{No files are saved (and \code{dir.save} is not
#'     created). The plot functions print (draw) each plot instead.}
#'   \item{\code{"BOTH"}}{The files are saved as usual and each output
#'     (plot or table) is also printed.}
#'   \item{\code{"print"}}{No files are saved; each output (plot, table,
#'     document) is printed instead, under its file name.}
#'   \item{\code{"generate <number>"}}{No files are saved; up to
#'     \code{<number>} outputs (e.g. \code{"generate 5"}) are created as
#'     objects in the R global environment, each named after the file it
#'     would have been saved as, without the extension and the
#'     \code{YYYYMMDDHHMMSS} time stamp and made a valid R name (spaces and
#'     other symbols become "."), e.g. \code{acme_vegetation.dbh}. A name
#'     used twice in one call gets \code{.2}, \code{.3}, ... \code{"generate"}
#'     alone creates every output. Objects: a plot is the ggplot (or
#'     patchwork) object, a table is the data frame, a workbook is a list of
#'     its tabs' data frames, a Word file is the officer document.}
#' }
#' \code{util_saver()} checks \code{save.auto} and returns a list:
#' \code{$mode} (\code{"save"}, \code{"none"}, \code{"print"} or
#' \code{"generate"}), \code{$save} (\code{TRUE} only when files are saved)
#' and \code{$out(obj, file, write)}, which saves (calls \code{write(file)},
#' then prints \code{Saved: <file>}), skips, prints or creates the object,
#' and returns \code{file} when it was saved, otherwise \code{NA}.
#' @param save.auto The \code{save.auto} argument.
#' @param fn Name of the calling function (for messages).
#' @param envir Environment the generated objects are created in, default
#'   the global environment.
#' @param plot Logical, default \code{FALSE}. \code{TRUE} (the plot
#'   functions): with \code{save.auto = FALSE} each plot is printed (drawn)
#'   instead of saved; tables are not printed.
#' @keywords internal
#' @noRd
util_saver <- function(save.auto, fn, envir = globalenv(), plot = FALSE) {
  m <- util_save_mode(save.auto, fn)
  st <- new.env(parent = emptyenv())
  st$made <- character(0); st$skipped <- 0L; st$noted <- FALSE
  out <- function(obj, file, write = NULL) {
    file <- as.character(file)[1]
    if (m$mode %in% c("save", "both")) {
      if (is.function(write)) write(file)
      cat("Saved:", file, "\n")
      if (m$mode == "both") util_save_print(obj)               # BOTH: saved and printed
      return(invisible(file))
    }
    if (m$mode == "none") {
      if (!st$noted) { cat(sprintf("NOTE: %s - save.auto = FALSE: no files saved%s.\n", fn,
                                   if (isTRUE(plot)) " - the plots are printed instead" else "")); st$noted <- TRUE }
      if (isTRUE(plot) && inherits(obj, c("ggplot", "patchwork", "gtable", "grob"))) util_save_print(obj)   # plot functions: shown
      return(invisible(NA_character_))
    }
    if (m$mode == "print") {
      cat(sprintf("---- %s ----\n", basename(file)))
      util_save_print(obj)
      return(invisible(NA_character_))
    }
    ## generate
    if (length(st$made) >= m$n) {
      st$skipped <- st$skipped + 1L
      if (st$skipped == 1L) cat(sprintf("NOTE: %s - save.auto = \"generate %d\": %d object(s) made - further outputs not created (%s ...).\n",
                                        fn, m$n, length(st$made), basename(file)))
      return(invisible(NA_character_))
    }
    nm <- util_save_objname(file)
    base <- nm; k <- 1L
    while (nm %in% st$made) { k <- k + 1L; nm <- paste0(base, ".", k) }
    assign(nm, obj, envir = envir)
    st$made <- c(st$made, nm)
    cat(sprintf("Created object: %s (instead of %s)\n", nm, basename(file)))
    invisible(NA_character_)
  }
  list(mode = m$mode, save = m$mode %in% c("save", "both"), n = m$n, out = out, made = function() st$made)
}


#' Read save.auto (internal)
#'
#' TRUE / "TRUE" = "save", FALSE / "FALSE" = "none", "print",
#' "generate <number>" (or "generate" = every output). Anything else stops
#' with an error naming the accepted values.
#' @keywords internal
#' @noRd
util_save_mode <- function(save.auto, fn) {
  bad <- function() stop(fn, ': save.auto must be TRUE, FALSE, "BOTH", "print" or "generate <number>" (e.g. "generate 5") - not: ',
                         paste(format(save.auto), collapse = " "), call. = FALSE)
  if (is.null(save.auto) || length(save.auto) != 1 || is.na(save.auto)) bad()
  if (is.logical(save.auto)) return(list(mode = if (save.auto) "save" else "none", n = Inf))
  v <- tolower(trimws(as.character(save.auto)))
  if (v %in% c("true", "t")) return(list(mode = "save", n = Inf))
  if (v %in% c("false", "f")) return(list(mode = "none", n = Inf))
  if (v == "print") return(list(mode = "print", n = Inf))
  if (v == "both") return(list(mode = "both", n = Inf))
  m <- regmatches(v, regexec("^generate\\s*([0-9]+)?$", v))[[1]]
  if (length(m)) {
    n <- if (nzchar(m[2])) as.numeric(m[2]) else Inf
    if (n < 1) bad()
    return(list(mode = "generate", n = n))
  }
  bad()
}


#' Object name from a would-be file name (internal)
#'
#' Folder, extension and the YYYYMMDDHHMMSS time stamp removed, then made a
#' valid R name: "acme_Test AYERS_20260626to20260629_20261117104914.png" ->
#' "acme_Test.AYERS_20260626to20260629".
#' @keywords internal
#' @noRd
util_save_objname <- function(file) {
  x <- basename(as.character(file))
  x <- sub("\\.(png|csv|xlsx|xls|docx|pdf|txt|jpg|jpeg|tif|tiff|svg)$", "", x, ignore.case = TRUE)
  x <- gsub("_?[0-9]{14}", "", x)
  x <- gsub("_+$", "", x)
  if (!nzchar(x)) x <- "batz.output"
  make.names(x)
}


#' Print one output for save.auto = "print" (internal)
#' @keywords internal
#' @noRd
util_save_print <- function(obj) {
  if (inherits(obj, c("ggplot", "patchwork"))) {
    ## a font the open graphics device cannot use (e.g. Arial on a pdf device): drawn with the device's own font
    tryCatch(print(obj), error = function(e) {
      if (!grepl("font", conditionMessage(e), ignore.case = TRUE)) stop(e)
      cat("NOTE: this graphics device cannot use the plot's font - drawn with its default font.\n")
      fix <- ggplot2::theme(text = ggplot2::element_text(family = ""))
      print(if (inherits(obj, "patchwork")) obj & fix else obj + fix)
    })
  } else if (inherits(obj, c("gtable", "grob"))) {
    grid::grid.newpage(); grid::grid.draw(obj)
  } else if (is.data.frame(obj)) {
    print(obj, row.names = FALSE)
  } else if (is.list(obj) && !inherits(obj, "rdocx") && length(obj) && all(vapply(obj, is.data.frame, logical(1)))) {
    for (nm in names(obj)) { cat(sprintf("[%s]\n", nm)); print(obj[[nm]], row.names = FALSE) }
  } else {
    print(obj)
  }
  invisible(obj)
}

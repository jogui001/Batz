#' Box plot of tree and snag DBH at each site
#'
#' Draws the diameter at breast height (DBH) of the trees and snags measured
#' in the \code{quad} tab of the Survey123 site-visit workbook made by
#' \code{\link{batz.merge_sitevisit.acoustics}}: one box per site
#' (\code{$aru.name}) and type, snags and trees side by side, sites on the X
#' axis, DBH on the Y axis. Trees: \code{$vegetation.tree_tree1.dbh.overall}
#' ... \code{tree3.dbh.overall}; snags: \code{$vegetation.snag_snag1.dbh.overall}
#' ... \code{snag3.dbh.overall}. One PNG is saved.
#'
#' @param data The quad tab: a data frame, the list returned by
#'   \code{\link{batz.merge_sitevisit.acoustics}} (its \code{$quad} is used),
#'   or the path to the merged workbook (\code{.xlsx}; sheet \code{quad}).
#'   Needs \code{$aru.name} and the DBH columns; \code{$project.name} when
#'   \code{project.name} is given.
#' @param project.name Character, default \code{""}. Project to plot (rows
#'   whose \code{$project.name} matches, case ignored) and the first part of
#'   the file name. Blank = every row, and the file name has no project part.
#' @param site.names Character vector, default \code{NULL} (all sites,
#'   sorted). Sites (\code{$aru.name}) to plot, in X-axis order. Also accepts
#'   one string separated by ";" or ",".
#' @param type.colors Character, default \code{NULL}: \code{"Snag=grey75;Tree=white"}.
#'   Box fill of snags and trees: \code{type=color} pairs, or two colors in
#'   the order snag, tree.
#' @param units \code{"in"} (default) or \code{"cm"}. Units plotted. Each
#'   record's \code{$measurement.units} (in or cm) is read and converted
#'   (1 in = 2.54 cm); a record with no unit is taken to be in \code{units}.
#' @param legend.position \code{"right"} (default), \code{"bottom"},
#'   \code{"left"}, \code{"top"} (or \code{"none"}). Where the legend goes.
#' @param size \code{"page"} (default), \code{"half page"},
#'   \code{"powerpoint"}, \code{"half powerpoint"}, or \code{"<width>x<height>"}
#'   in inches (e.g. \code{"8x5"}). PNG size, width x height: page 6 x 7,
#'   half page 6 x 4, powerpoint 12 x 6, half powerpoint 6 x 6.
#' @param table Logical, default \code{TRUE}. TRUE also saves the summary
#'   table (one row per site and type: \code{$site}, \code{$type},
#'   \code{$n}, \code{$min}, \code{$median}, \code{$mean}, \code{$max})
#'   as a CSV next to the PNG.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG (and
#'   table) is saved in (created if needed). \code{NULL} = nothing saved.
#' @param plotopts Plot settings: \code{NULL} (default; built-in defaults),
#'   a data frame, or the path to a \code{.csv} or \code{.xlsx} file, laid
#'   out like \verb{plotopts_vegetation.dbh.csv}: columns \code{$category},
#'   \code{$parameter}, \code{$default.value}, and optionally
#'   \code{$override.value} and \code{$notes}. A parameter left out uses the
#'   built-in default. See "Settings" in Details.
#' @param ... Any other setting listed in Details ("Settings"), typed into
#'   the call by name (e.g. \code{yaxe.max = 20}); used before
#'   \code{plotopts}.
#' @param aes.style Character, default \code{"override.value"}. Name of the
#'   \code{plotopts} column read before \code{$default.value} (an old
#'   \code{$overide.value} column is still read).
#'
#' @return Invisibly, a list: \code{$plot} (the ggplot), \code{$dbh} (one
#'   row per tree or snag: \code{$site}, \code{$type}, \code{$column},
#'   \code{$dbh}), \code{$summary} (per site and type: \code{$n},
#'   \code{$min}, \code{$median}, \code{$mean}, \code{$max}), \code{$settings}
#'   (the value used for every setting and where it came from:
#'   \code{"input"}, \code{"plotopts"} or \code{"default"}), \code{$file}
#'   (the PNG, or \code{NA}) and \code{$file.table} (the CSV, or \code{NA}).
#'
#' @details
#' \strong{Settings.} Every setting is taken, in this order, from:
#' \enumerate{
#'   \item the value typed into the function call in this R session
#'     (\code{site.names}, \code{type.colors}, \code{units},
#'     \code{legend.position}, \code{size}, \code{table}, or any other setting by name in \code{...});
#'   \item \code{plotopts}: \code{$override.value} (\code{aes.style}) when
#'     filled, else \code{$default.value};
#'   \item the built-in default (\code{batz.plot_vegetation.dbh.defaults()}).
#' }
#' \code{plotopts} can hold the five inputs above as \code{$parameter} rows
#' as well as these: \code{site.column}, \code{tree.columns},
#' \code{snag.columns}, \code{snag.include} (TRUE/FALSE), \code{tree.label},
#' \code{snag.label}, \code{units.column}, \code{xaxe.title},
#' \code{yaxe.title} (blank = "Diameter at Breast Height (inches)" or
#' "(cm)"), \code{yaxe.max} (blank = from the data), \code{legend.title},
#' \code{box.width}, \code{box.outline.color}, \code{box.linewidth},
#' \code{whisker.caps},
#' \code{outliers.show}, \code{points.show}, \code{point.size},
#' \code{axis.title.size}, \code{axis.text.size}, \code{legend.text.size},
#' \code{legend.key.size}, \code{ggsave.dpi}. Lists are separated by ";".
#'
#' \strong{DBH values.} One value per tree or snag: its \code{.overall}
#' column (the square root of the sum of the squared DBH of each trunk, made
#' by \code{\link{batz.merge_sitevisit.acoustics}}). When a workbook has no
#' \code{.overall} column, it is worked out the same way from the DBH column
#' (e.g. \code{"6;8;10"} gives 14.142). Values that are not numbers are left
#' out.
#'
#' \strong{Boxes.} Standard box plots (median line, box from the 25th to the
#' 75th percentile, whiskers to the furthest value within 1.5 times the box
#' height). A site with only trees or only snags keeps the same box width,
#' centred on the site. The Y axis starts at 0.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]vegetation.dbh_<YYYYMMDDHHMMSS>.png}; the table
#' \verb{[<project.name>_]vegetation.dbh_table_<YYYYMMDDHHMMSS>.csv}.
#'
#' @seealso \code{\link{batz.merge_sitevisit.acoustics}},
#'   \code{\link{batz.plot_vegetation.species}}
#'
#' @examples
#' \dontrun{
#' batz.plot_vegetation.dbh("mergedsurvey123.xlsx", project.name = "Bristol NH")
#' batz.plot_vegetation.dbh(m, project.name = "Falmouth", size = "powerpoint",
#'                          type.colors = "Snag=grey60;Tree=white", units = "cm")
#' }
#' @export
batz.plot_vegetation.dbh <- function(data, project.name = "", site.names = NULL, type.colors = NULL,
                                     units = c("in", "cm"),
                                     legend.position = c("right", "bottom", "left", "top"),
                                     size = c("page", "half page", "powerpoint", "half powerpoint"),
                                     table = TRUE, dir.save = getwd(), plotopts = NULL, aes.style = "override.value", ...) {
  FN <- "batz.plot_vegetation.dbh"
  blank <- function(v) is.null(v) || length(v) == 0 || all(is.na(v)) || all(!nzchar(trimws(as.character(v))))
  split.list <- function(v) {
    if (blank(v)) return(character(0))
    x <- trimws(gsub('"', "", unlist(strsplit(as.character(v), "[;,]"))))
    x[nzchar(x)]
  }
  key <- function(v) tolower(trimws(as.character(v)))

  ## ---- settings: input > plotopts > default ---------------------------------
  defs <- batz.plot_vegetation.dbh.defaults()
  po <- NULL
  if (!is.null(plotopts)) {
    if (is.character(plotopts)) {
      if (!file.exists(plotopts[1])) stop(FN, ": plotopts file not found: ", plotopts[1], call. = FALSE)
      po <- if (grepl("\\.xlsx?$", plotopts[1], ignore.case = TRUE))
        as.data.frame(readxl::read_excel(plotopts[1], sheet = if (length(plotopts) > 1) plotopts[2] else 1, col_types = "text"))
      else utils::read.csv(plotopts[1], check.names = FALSE, colClasses = "character")
    } else if (is.data.frame(plotopts)) po <- plotopts
    else stop(FN, ": plotopts must be a data frame or the path to a .csv / .xlsx file.", call. = FALSE)
    names(po) <- tolower(trimws(names(po)))
    if (!all(c("parameter", "default.value") %in% names(po)))
      stop(FN, ": plotopts needs the columns $parameter and $default.value.", call. = FALSE)
    unknown <- po$parameter[!is.na(po$parameter) & !plotutil_param_key(po$parameter) %in% plotutil_param_key(defs$parameter)]
    if (length(unknown)) cat(sprintf("NOTE: %s - plotopts parameter(s) not used by this function: %s\n", FN, paste(unknown, collapse = ", ")))
  }
  style.col <- if (is.null(po)) NA_character_ else plotutil_aes_style_col(po, aes.style)
  inputs <- list()
  if (!missing(site.names)) inputs$site.names <- site.names
  if (!missing(type.colors)) inputs$type.colors <- type.colors
  if (!missing(units)) inputs$units <- units
  if (!missing(table)) inputs$table <- as.character(table)
  if (!missing(legend.position)) inputs$legend.position <- legend.position
  if (!missing(size)) inputs$size <- size
  dots <- list(...)                                  # any other plotopts setting typed into the call
  if (length(dots)) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) stop(FN, ': settings in the call must be named, e.g. yaxe.max = 20.', call. = FALSE)
    for (nm in names(dots)) {
      p <- defs$parameter[plotutil_param_key(defs$parameter) == plotutil_param_key(nm)][1]
      if (is.na(p)) cat(sprintf("NOTE: %s - setting in the call not used by this function: %s\n", FN, nm))
      else inputs[[p]] <- paste(as.character(dots[[nm]]), collapse = ";")
    }
  }
  source.of <- list()
  setting <- function(p) {
    if (!is.null(inputs[[p]]) && !blank(inputs[[p]])) { source.of[[p]] <<- "input"; return(inputs[[p]]) }
    if (!is.null(po)) {
      i <- which(plotutil_param_key(po$parameter) == plotutil_param_key(p))[1]
      if (!is.na(i)) {
        v <- if (!is.na(style.col)) po[[style.col]][i] else NA
        if (blank(v)) v <- po$default.value[i]
        if (!blank(v)) { source.of[[p]] <<- "plotopts"; return(as.character(v)) }
      }
    }
    source.of[[p]] <<- "default"
    v <- defs$default.value[defs$parameter == p]
    if (is.na(v)) "" else v
  }
  num <- function(p) {
    v <- suppressWarnings(as.numeric(setting(p)))
    if (length(v) != 1 || is.na(v)) stop(sprintf("%s: setting '%s' = '%s' is not a number.", FN, p, setting(p)), call. = FALSE)
    v
  }
  flag <- function(p) toupper(trimws(setting(p))) %in% c("TRUE", "T", "YES", "Y", "1")
  pick1 <- function(p, allowed) {
    v <- key(setting(p))[1]
    if (!v %in% allowed) stop(sprintf('%s: %s = "%s" - use %s.', FN, p, v, paste0('"', allowed, '"', collapse = ", ")), call. = FALSE)
    v
  }

  ## ---- data --------------------------------------------------------------------
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) stop(FN, ": file not found: ", data, call. = FALSE)
    sh <- readxl::excel_sheets(data)
    s <- sh[tolower(sh) == "quad"][1]
    if (is.na(s)) stop(FN, ": ", basename(data), " has no 'quad' sheet.", call. = FALSE)
    data <- as.data.frame(readxl::read_excel(data, sheet = s, col_types = "text"))
  } else if (!is.data.frame(data) && is.list(data)) {
    if (!"quad" %in% names(data)) stop(FN, ": data is a list without a $quad data frame.", call. = FALSE)
    data <- data$quad
  }
  if (!is.data.frame(data)) stop(FN, ": data must be a data frame, a list with $quad, or the merged workbook path.", call. = FALSE)
  names(data) <- tolower(trimws(names(data)))

  if (!blank(project.name)) {
    if (!"project.name" %in% names(data)) stop(FN, ": data has no $project.name column.", call. = FALSE)
    keep <- key(data$project.name) == key(project.name)
    if (!any(keep)) stop(FN, ': no rows for project.name = "', project.name, '". Projects in data: ',
                         paste(sort(unique(data$project.name)), collapse = ", "), call. = FALSE)
    data <- data[keep, , drop = FALSE]
  } else if ("project.name" %in% names(data) && length(unique(stats::na.omit(data$project.name))) > 1) {
    cat(sprintf("NOTE: %s - project.name is blank: all %d projects are plotted together.\n", FN,
                length(unique(stats::na.omit(data$project.name)))))
  }

  site.col <- tolower(setting("site.column"))
  if (!site.col %in% names(data)) stop(FN, ": data has no $", site.col, " column (site.column).", call. = FALSE)
  data$.site <- trimws(as.character(data[[site.col]]))
  sites <- split.list(setting("site.names"))
  if (length(sites)) {
    miss <- sites[!key(sites) %in% key(data$.site)]
    if (length(miss)) cat(sprintf("NOTE: %s - site(s) not in data: %s\n", FN, paste(miss, collapse = ", ")))
    data <- data[key(data$.site) %in% key(sites), , drop = FALSE]
    data$.site <- sites[match(key(data$.site), key(sites))]
  } else {
    sites <- sort(unique(data$.site[!is.na(data$.site) & nzchar(data$.site)]))
  }
  if (nrow(data) == 0 || !length(sites)) stop(FN, ": no rows left to plot.", call. = FALSE)

  ## ---- DBH values ------------------------------------------------------------
  units <- pick1("units", c("in", "cm"))
  ucol <- tolower(setting("units.column"))
  rec.unit <- if (ucol %in% names(data)) key(data[[ucol]]) else rep(NA_character_, nrow(data))
  rec.unit[!rec.unit %in% c("in", "cm")] <- units
  factor.to <- ifelse(rec.unit == units, 1, if (units == "in") 1 / 2.54 else 2.54)
  overall <- function(x) vapply(as.character(x), function(v) {
    if (is.na(v) || !nzchar(trimws(v))) return(NA_real_)
    n <- suppressWarnings(as.numeric(trimws(strsplit(v, ";", fixed = TRUE)[[1]])))
    n <- n[!is.na(n)]
    if (length(n)) sqrt(sum(n^2)) else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
  long <- function(cols, type) {
    cols <- tolower(split.list(cols))
    do.call(rbind, lapply(cols, function(cl) {
      ov <- paste0(cl, ".overall")
      v <- if (ov %in% names(data)) suppressWarnings(as.numeric(as.character(data[[ov]])))
           else if (cl %in% names(data)) overall(data[[cl]]) else NULL
      if (is.null(v)) { cat(sprintf("NOTE: %s - column not in data: %s\n", FN, cl)); return(NULL) }
      v <- v * factor.to
      ok <- !is.na(v)
      if (!any(ok)) return(NULL)
      data.frame(site = data$.site[ok], type = type, column = cl, dbh = round(v[ok], 3), stringsAsFactors = FALSE)
    }))
  }
  tree.lab <- setting("tree.label"); snag.lab <- setting("snag.label")
  dbh <- rbind(long(setting("tree.columns"), tree.lab),
               if (flag("snag.include")) long(setting("snag.columns"), snag.lab))
  if (is.null(dbh) || nrow(dbh) == 0) stop(FN, ": no DBH values found for these sites.", call. = FALSE)
  types <- c(snag.lab, tree.lab)[c(snag.lab, tree.lab) %in% dbh$type]
  dbh <- dbh[order(match(dbh$site, sites), match(dbh$type, types)), ]; rownames(dbh) <- NULL
  smry <- stats::aggregate(dbh["dbh"], by = dbh[c("site", "type")], FUN = function(v) c(n = length(v), min = min(v), median = stats::median(v), mean = mean(v), max = max(v)))
  smry <- cbind(smry[c("site", "type")], as.data.frame(smry$dbh))
  smry <- smry[order(match(smry$site, sites), match(smry$type, types)), ]; rownames(smry) <- NULL
  smry[c("min", "median", "mean", "max")] <- round(smry[c("min", "median", "mean", "max")], 3)

  ## ---- colors --------------------------------------------------------------------
  cols.in <- split.list(setting("type.colors"))
  if (length(cols.in) && all(grepl("=", cols.in))) {
    cmap <- stats::setNames(trimws(sub("^[^=]*=", "", cols.in)), key(sub("=.*$", "", cols.in)))
    fills <- unname(cmap[key(c(snag.lab, tree.lab))])
  } else fills <- cols.in[1:2]
  fills[is.na(fills) | !nzchar(fills)] <- c("grey75", "white")[is.na(fills) | !nzchar(fills)]
  names(fills) <- c(snag.lab, tree.lab)

  ## ---- plot ------------------------------------------------------------------------
  legend.position <- pick1("legend.position", c("right", "bottom", "left", "top", "none"))
  sz <- key(setting("size"))
  sizes <- list(page = c(6, 7), `half page` = c(6, 4), powerpoint = c(12, 6), `half powerpoint` = c(6, 6))
  wh <- if (sz %in% names(sizes)) sizes[[sz]] else suppressWarnings(as.numeric(strsplit(sz, "x", fixed = TRUE)[[1]]))
  if (length(wh) != 2 || any(is.na(wh)) || any(wh <= 0))
    stop(FN, ': size = "', sz, '" - use "page", "half page", "powerpoint", "half powerpoint" or "<width>x<height>" (inches).', call. = FALSE)

  pd <- dbh
  pd$site <- factor(pd$site, levels = sites)
  pd$type <- factor(pd$type, levels = types)
  ymax <- suppressWarnings(as.numeric(setting("yaxe.max")))
  if (is.na(ymax)) ymax <- max(pretty(c(0, max(pd$dbh)), n = 4))
  brks <- pretty(c(0, ymax), n = 4); brks <- brks[brks <= ymax]
  ytitle <- setting("yaxe.title")
  if (blank(ytitle)) ytitle <- paste0("Diameter at Breast Height (", if (units == "in") "inches" else "cm", ")")
  bw <- num("box.width")
  dodge <- ggplot2::position_dodge2(preserve = "single", padding = 0.1)

  g <- ggplot2::ggplot(pd, ggplot2::aes(x = site, y = dbh, fill = type))
  if (flag("whisker.caps"))                       # short line across the end of each whisker
    g <- g + ggplot2::stat_boxplot(geom = "errorbar", position = ggplot2::position_dodge2(preserve = "single", padding = 0.5),
                                   width = bw, colour = setting("box.outline.color"), linewidth = num("box.linewidth"))
  g <- g +
    ggplot2::geom_boxplot(position = dodge, width = bw, colour = setting("box.outline.color"),
                          linewidth = num("box.linewidth"),
                          outlier.shape = if (flag("outliers.show")) 19 else NA)
  if (flag("points.show"))
    g <- g + ggplot2::geom_point(position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = bw),
                                 size = num("point.size"), colour = "black", show.legend = FALSE)
  g <- g +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = brks, limits = c(0, ymax), expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_fill_manual(values = fills, breaks = types,
                               name = if (blank(setting("legend.title"))) NULL else setting("legend.title")) +
    ggplot2::labs(x = setting("xaxe.title"), y = ytitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = num("axis.title.size")),
                   axis.text = ggplot2::element_text(size = num("axis.text.size")),
                   legend.text = ggplot2::element_text(size = num("legend.text.size")),
                   legend.key.size = ggplot2::unit(num("legend.key.size"), "lines"),
                   legend.position = legend.position)
  if (legend.position %in% c("top", "bottom")) g <- g + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  file <- NA_character_
  if (!is.null(dir.save)) {
    if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    cl <- function(x) gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(x)))
    file <- file.path(dir.save, paste0(if (!blank(project.name)) paste0(cl(project.name), "_"), "vegetation.dbh_",
                                       format(Sys.time(), "%Y%m%d%H%M%S"), ".png"))
    k <- 1; base <- sub("\\.png$", "", file)
    while (file.exists(file)) { k <- k + 1; file <- paste0(base, "_", k, ".png") }   # same second
    ggplot2::ggsave(file, g, width = wh[1], height = wh[2], units = "in", dpi = num("ggsave.dpi"), bg = "white")
    cat("Saved:", file, "\n")
  }

  used <- c("site.names", "type.colors", "units", "legend.position", "size", "table", defs$parameter)
  used <- unique(used[used %in% names(source.of)])
  settings <- data.frame(parameter = used,
                         value = vapply(used, function(p) paste(setting(p), collapse = ";"), character(1)),
                         source = unlist(source.of[used]), row.names = NULL, stringsAsFactors = FALSE)
  file.table <- NA_character_
  if (flag("table") && !is.null(dir.save)) {
    file.table <- sub("\\.png$", ".csv", sub("vegetation\\.dbh_", "vegetation.dbh_table_", file))
    utils::write.csv(smry[c("site", "type", "n", "min", "median", "mean", "max")], file.table, row.names = FALSE)
    cat("Saved:", file.table, "\n")
  }
  invisible(list(plot = g, dbh = dbh, summary = smry, settings = settings, file = file, file.table = file.table))
}


#' Built-in plot settings of batz.plot_vegetation.dbh()
#'
#' The settings used when neither the function call nor \code{plotopts}
#' gives a value. They are kept in Batz_reference_db (tab \code{PLOTOPTS_plot_veg.dbh}) and
#' carried by the package (\code{\link{batz.util_refdb.sync}}). Write it out as a starting \code{plotopts} file with
#' \code{write.csv(batz.plot_vegetation.dbh.defaults(),
#' "plotopts_vegetation.dbh.csv", row.names = FALSE)}.
#'
#' @return Data frame: \code{$category}, \code{$parameter},
#'   \code{$default.value}, \code{$override.value} (blank), \code{$notes}.
#' @export
batz.plot_vegetation.dbh.defaults <- function() batz.db("plotopts.batz.plot_vegetation.dbh")   # Batz_reference_db, tab PLOTOPTS_plot_veg.dbh

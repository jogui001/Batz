#' Bar plot of tree and sapling species counted at each site
#'
#' Counts the tree species (\code{$vegetation.tree_tree1.species},
#' \code{$vegetation.tree_tree2.species}, \code{$vegetation.tree_tree3.species})
#' and sapling species (\code{$vegetation.sapling_species1},
#' \code{$vegetation.sapling_species2}, \code{$vegetation.sapling_species3})
#' in the \code{quad} tab of the Survey123 site-visit workbook made by
#' \code{\link{batz.merge_sitevisit.acoustics}}, and draws one panel for
#' saplings above one panel for trees: species on the X axis, number of each
#' species on the Y axis, one color per site (\code{$aru.name}), bars
#' stacked or side by side. Snags (\code{$vegetation.snag_snag1.dbh} ...
#' \code{snag3.dbh}) are counted as a "Snag" bar in the tree panel. One PNG
#' is saved.
#'
#' @param data The quad tab: a data frame, the list returned by
#'   \code{\link{batz.merge_sitevisit.acoustics}} (its \code{$quad} is used),
#'   or the path to the merged workbook (\code{.xlsx}; sheet \code{quad}).
#'   Needs \code{$aru.name} and the species columns; \code{$project.name}
#'   when \code{project.name} is given.
#' @param project.name Character, default \code{""}. Project to plot (rows
#'   whose \code{$project.name} matches, case ignored) and the first part of
#'   the file name. Blank = every row, and the file name has no project part.
#' @param site.names Character vector, default \code{NULL} (all sites,
#'   sorted). Sites (\code{$aru.name}) to plot, in legend order. Also
#'   accepts one string separated by ";" or ",".
#' @param site.colors Character vector, default \code{NULL}. Colors of the
#'   sites: one per site in \code{site.names} order (e.g.
#'   \code{c("black", "grey75")}) or \code{site=color} pairs (e.g.
#'   \code{"Falmouth1=black;Falmouth2=grey75"}). Default: shades of grey from
#'   black to light grey (two sites = black and grey75, as in the example).
#' @param legend.position \code{"right"} (default), \code{"bottom"},
#'   \code{"left"}, \code{"top"} (or \code{"none"}). Where the legend goes.
#' @param bar \code{"stacked"} (default) or \code{"grouped"}. Stacked puts
#'   the sites' bars on top of each other; grouped puts them side by side.
#' @param size \code{"page"} (default), \code{"half page"},
#'   \code{"powerpoint"}, \code{"half powerpoint"}, or \code{"<width>x<height>"}
#'   in inches (e.g. \code{"8x5"}). PNG size, width x height: page 6 x 7,
#'   half page 6 x 4, powerpoint 12 x 6, half powerpoint 6 x 6.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG is
#'   saved in (created if needed). \code{NULL} = no PNG.
#' @param plotopts Plot settings: \code{NULL} (default; built-in defaults),
#'   a data frame, or the path to a \code{.csv} or \code{.xlsx} file, laid
#'   out like \verb{plotopts_vegetation.species.csv}: columns
#'   \code{$category}, \code{$parameter}, \code{$default.value}, and
#'   optionally \code{$override.value} and \code{$notes}. A parameter left
#'   out uses the built-in default. See "Settings" in Details.
#' @param aes.style Character, default \code{"override.value"}. Name of the
#'   \code{plotopts} column read before \code{$default.value} (an old
#'   \code{$overide.value} column is still read).
#'
#' @return Invisibly, a list: \code{$plot} (the ggplot), \code{$counts} (one
#'   row per panel, species and site: \code{$panel}, \code{$species},
#'   \code{$site}, \code{$count}), \code{$settings} (the value used for
#'   every setting and where it came from: \code{"input"}, \code{"plotopts"}
#'   or \code{"default"}) and \code{$file} (the PNG, or \code{NA}).
#'
#' @details
#' \strong{Settings.} Every setting is taken, in this order, from:
#' \enumerate{
#'   \item the value typed into the function call in this R session
#'     (\code{site.names}, \code{site.colors}, \code{legend.position},
#'     \code{bar}, \code{size});
#'   \item \code{plotopts}: \code{$override.value} (\code{aes.style}) when
#'     filled, else \code{$default.value};
#'   \item the built-in default (\code{batz.plot_vegetation.species.defaults()}).
#' }
#' \code{plotopts} can hold the five inputs above as \code{$parameter} rows
#' as well as these: \code{tree.columns}, \code{sapling.columns},
#' \code{snag.columns}, \code{snag.include} (TRUE/FALSE), \code{snag.label},
#' \code{site.column}, \code{species.titlecase} (TRUE/FALSE),
#' \code{panel.order} ("sapling;tree"), \code{panel.title.sapling},
#' \code{panel.title.tree}, \code{xaxe.title}, \code{yaxe.title},
#' \code{legend.title}, \code{yaxe.max} (blank = from the data),
#' \code{xaxe.text.angle} ("auto" = 45 degrees when there are more than 8
#' species),
#' \code{bar.width}, \code{bar.outline.color}, \code{strip.text.size},
#' \code{axis.title.size}, \code{axis.text.size}, \code{legend.text.size},
#' \code{legend.key.size}, \code{ggsave.dpi}. Lists are separated by ";".
#'
#' \strong{Counting.} Each filled species cell is one plant: a quadrant with
#' Maple as tree 1 and tree 3 adds 2 to Maple. Species names are trimmed
#' and, with \code{species.titlecase = TRUE}, written in title case so
#' "red spruce" and "Red Spruce" are counted together. With
#' \code{snag.include = TRUE} every filled snag DBH adds 1 to the
#' \code{snag.label} bar in the tree panel. Both panels show the same
#' species (alphabetical) and the same Y axis.
#'
#' \strong{File name.}
#' \verb{[<project.name>_]vegetation.species_<YYYYMMDDHHMMSS>.png}.
#'
#' @seealso \code{\link{batz.merge_sitevisit.acoustics}}
#'
#' @examples
#' \dontrun{
#' batz.plot_vegetation.species("mergedsurvey123.xlsx", project.name = "Bristol NH")
#' batz.plot_vegetation.species(m, project.name = "Falmouth",
#'                              site.names = c("Falmouth1", "Falmouth2"),
#'                              site.colors = c("black", "grey75"),
#'                              bar = "grouped", legend.position = "bottom",
#'                              size = "powerpoint")
#' }
#' @export
batz.plot_vegetation.species <- function(data, project.name = "", site.names = NULL, site.colors = NULL,
                                         legend.position = c("right", "bottom", "left", "top"),
                                         bar = c("stacked", "grouped"),
                                         size = c("page", "half page", "powerpoint", "half powerpoint"),
                                         dir.save = getwd(), plotopts = NULL, aes.style = "override.value") {
  FN <- "batz.plot_vegetation.species"
  blank <- function(v) is.null(v) || length(v) == 0 || all(is.na(v)) || all(!nzchar(trimws(as.character(v))))
  split.list <- function(v) {
    if (blank(v)) return(character(0))
    x <- trimws(gsub('"', "", unlist(strsplit(as.character(v), "[;,]"))))
    x[nzchar(x)]
  }
  key <- function(v) tolower(trimws(as.character(v)))

  ## ---- settings: input > plotopts > default ---------------------------------
  defs <- batz.plot_vegetation.species.defaults()
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
  if (!missing(site.colors)) inputs$site.colors <- site.colors
  if (!missing(legend.position)) inputs$legend.position <- legend.position
  if (!missing(bar)) inputs$bar <- bar
  if (!missing(size)) inputs$size <- size
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

  ## ---- counts ------------------------------------------------------------------
  titlecase <- function(x) gsub("(^|\\s)(\\w)", "\\1\\U\\2", tolower(x), perl = TRUE)
  clean <- function(x) {
    x <- gsub("\\s+", " ", trimws(as.character(x)))
    x[is.na(x) | !nzchar(x)] <- NA
    if (flag("species.titlecase")) x <- titlecase(x)
    x
  }
  long <- function(cols, panel) {
    cols <- tolower(split.list(cols))
    have <- intersect(cols, names(data))
    if (length(setdiff(cols, have))) cat(sprintf("NOTE: %s - column(s) not in data: %s\n", FN, paste(setdiff(cols, have), collapse = ", ")))
    do.call(rbind, lapply(have, function(cl) {
      sp <- clean(data[[cl]]); ok <- !is.na(sp)
      if (!any(ok)) return(NULL)
      data.frame(panel = panel, species = sp[ok], site = data$.site[ok], stringsAsFactors = FALSE)
    }))
  }
  recs <- rbind(long(setting("tree.columns"), "tree"), long(setting("sapling.columns"), "sapling"))
  if (flag("snag.include")) {
    sc <- intersect(tolower(split.list(setting("snag.columns"))), names(data))
    for (cl in sc) {
      ok <- !is.na(data[[cl]]) & nzchar(trimws(as.character(data[[cl]])))
      if (any(ok)) recs <- rbind(recs, data.frame(panel = "tree", species = setting("snag.label"),
                                                  site = data$.site[ok], stringsAsFactors = FALSE))
    }
  }
  if (is.null(recs) || nrow(recs) == 0) stop(FN, ": no species found in the tree / sapling columns for these sites.", call. = FALSE)
  counts <- stats::aggregate(list(count = rep(1L, nrow(recs))), by = recs[c("panel", "species", "site")], FUN = sum)
  panels <- key(split.list(setting("panel.order")))
  panels <- c(panels[panels %in% c("sapling", "tree")], setdiff(c("sapling", "tree"), panels))
  titles <- c(sapling = setting("panel.title.sapling"), tree = setting("panel.title.tree"))
  spp <- sort(unique(counts$species))
  counts <- counts[order(match(counts$panel, panels), counts$species, match(counts$site, sites)), ]
  rownames(counts) <- NULL

  ## ---- colors ------------------------------------------------------------------
  cols.in <- split.list(setting("site.colors"))
  if (length(cols.in) && all(grepl("=", cols.in))) {
    cmap <- stats::setNames(trimws(sub("^[^=]*=", "", cols.in)), key(sub("=.*$", "", cols.in)))
    fills <- unname(cmap[key(sites)])
  } else fills <- cols.in[seq_along(sites)]
  no.col <- is.na(fills) | !nzchar(fills)
  if (any(no.col)) {
    if (length(cols.in)) cat(sprintf("NOTE: %s - no color given for: %s - grey used.\n", FN, paste(sites[no.col], collapse = ", ")))
    fills[no.col] <- grDevices::gray.colors(length(sites), start = 0, end = 0.75)[no.col]
  }
  names(fills) <- sites

  ## ---- plot --------------------------------------------------------------------
  legend.position <- pick1("legend.position", c("right", "bottom", "left", "top", "none"))
  bar <- pick1("bar", c("stacked", "grouped"))
  sz <- key(setting("size"))
  sizes <- list(page = c(6, 7), `half page` = c(6, 4), powerpoint = c(12, 6), `half powerpoint` = c(6, 6))
  wh <- if (sz %in% names(sizes)) sizes[[sz]] else suppressWarnings(as.numeric(strsplit(sz, "x", fixed = TRUE)[[1]]))
  if (length(wh) != 2 || any(is.na(wh)) || any(wh <= 0))
    stop(FN, ': size = "', sz, '" - use "page", "half page", "powerpoint", "half powerpoint" or "<width>x<height>" (inches).', call. = FALSE)

  pd <- counts
  pd$panel <- factor(titles[pd$panel], levels = titles[panels])
  pd$species <- factor(pd$species, levels = spp)
  pd$site <- factor(pd$site, levels = sites)
  tot <- stats::aggregate(pd["count"], by = pd[c("panel", "species")], FUN = sum)
  ymax.data <- if (bar == "stacked") max(tot$count) else max(pd$count)
  ymax <- suppressWarnings(as.numeric(setting("yaxe.max")))
  if (is.na(ymax)) ymax <- max(pretty(c(0, ymax.data), n = 3))
  brks <- pretty(c(0, ymax), n = 3); brks <- brks[brks <= ymax]
  pos <- if (bar == "stacked") ggplot2::position_stack(reverse = TRUE) else ggplot2::position_dodge(preserve = "single")
  outline <- setting("bar.outline.color")

  g <- ggplot2::ggplot(pd, ggplot2::aes(x = species, y = count, fill = site)) +
    ggplot2::geom_col(position = pos, width = num("bar.width"),
                      colour = if (blank(outline) || key(outline) == "none") NA else outline) +
    ggplot2::facet_wrap(~panel, ncol = 1, drop = FALSE) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = brks, limits = c(0, ymax), expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_fill_manual(values = fills, breaks = sites, drop = FALSE,
                               name = if (blank(setting("legend.title"))) NULL else setting("legend.title")) +
    ggplot2::labs(x = setting("xaxe.title"), y = setting("yaxe.title")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = num("strip.text.size")),
                   axis.title = ggplot2::element_text(size = num("axis.title.size")),
                   axis.text = ggplot2::element_text(size = num("axis.text.size")),
                   legend.text = ggplot2::element_text(size = num("legend.text.size")),
                   legend.key.size = ggplot2::unit(num("legend.key.size"), "lines"),
                   legend.position = legend.position)
  ang <- key(setting("xaxe.text.angle"))
  ang <- if (ang == "auto") (if (length(spp) > 8) 45 else 0) else suppressWarnings(as.numeric(ang))
  if (is.na(ang)) stop(FN, ': xaxe.text.angle must be "auto" or a number.', call. = FALSE)
  if (ang != 0) g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ang, hjust = 1, vjust = 1))
  if (legend.position %in% c("top", "bottom")) g <- g + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  file <- NA_character_
  if (!is.null(dir.save)) {
    if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    cl <- function(x) gsub("\\s+", "-", gsub('[\\\\/:*?"<>|]+', "", trimws(x)))
    file <- file.path(dir.save, paste0(if (!blank(project.name)) paste0(cl(project.name), "_"), "vegetation.species_",
                                       format(Sys.time(), "%Y%m%d%H%M%S"), ".png"))
    k <- 1; base <- sub("\\.png$", "", file)
    while (file.exists(file)) { k <- k + 1; file <- paste0(base, "_", k, ".png") }   # same second
    ggplot2::ggsave(file, g, width = wh[1], height = wh[2], units = "in", dpi = num("ggsave.dpi"), bg = "white")
    cat("Saved:", file, "\n")
  }

  used <- c("site.names", "site.colors", "legend.position", "bar", "size", defs$parameter)
  used <- unique(used[used %in% names(source.of)])
  settings <- data.frame(parameter = used,
                         value = vapply(used, function(p) paste(setting(p), collapse = ";"), character(1)),
                         source = unlist(source.of[used]), row.names = NULL, stringsAsFactors = FALSE)
  invisible(list(plot = g, counts = counts, settings = settings, file = file))
}


#' Built-in plot settings of batz.plot_vegetation.species()
#'
#' The settings used when neither the function call nor \code{plotopts}
#' gives a value. Write it out as a starting \code{plotopts} file with
#' \code{write.csv(batz.plot_vegetation.species.defaults(),
#' "plotopts_vegetation.species.csv", row.names = FALSE)}.
#'
#' @return Data frame: \code{$category}, \code{$parameter},
#'   \code{$default.value}, \code{$override.value} (blank), \code{$notes}.
#' @export
batz.plot_vegetation.species.defaults <- function() {
  r <- function(category, parameter, default.value, notes)
    data.frame(category = category, parameter = parameter, default.value = default.value,
               override.value = "", notes = notes, stringsAsFactors = FALSE)
  rbind(
    r("Inputs", "site.names", "", "Sites ($aru.name) to plot, in legend order, separated by ';'. Blank = all sites, sorted."),
    r("Inputs", "site.colors", "", "One color per site in site.names order, or site=color pairs, separated by ';'. Blank = black to light grey."),
    r("Inputs", "legend.position", "right", "right, bottom, left, top or none."),
    r("Inputs", "bar", "stacked", "stacked (sites on top of each other) or grouped (side by side)."),
    r("Inputs", "size", "page", "page (6 x 7 in), half page (6 x 4), powerpoint (12 x 6), half powerpoint (6 x 6), or <width>x<height> in inches."),
    r("Data", "site.column", "aru.name", "Column holding the site names."),
    r("Data", "tree.columns", "vegetation.tree_tree1.species;vegetation.tree_tree2.species;vegetation.tree_tree3.species", "Tree species columns."),
    r("Data", "sapling.columns", "vegetation.sapling_species1;vegetation.sapling_species2;vegetation.sapling_species3", "Sapling species columns."),
    r("Data", "snag.include", "TRUE", "TRUE counts every filled snag DBH as one snag in the tree panel."),
    r("Data", "snag.columns", "vegetation.snag_snag1.dbh;vegetation.snag_snag2.dbh;vegetation.snag_snag3.dbh", "Snag columns (a filled cell = one snag)."),
    r("Data", "snag.label", "Snag", "X-axis label of the snag bar."),
    r("Data", "species.titlecase", "TRUE", "TRUE writes species in title case so 'red spruce' and 'Red Spruce' are counted together."),
    r("Panels", "panel.order", "sapling;tree", "Panel order, top to bottom."),
    r("Panels", "panel.title.sapling", "Sapling", "Heading of the sapling panel."),
    r("Panels", "panel.title.tree", "Tree", "Heading of the tree panel."),
    r("Axes", "xaxe.title", "Species", "X-axis title."),
    r("Axes", "yaxe.title", "Number of each species", "Y-axis title."),
    r("Axes", "yaxe.max", "", "Top of the Y axis. Blank = from the data."),
    r("Axes", "xaxe.text.angle", "auto", "Angle of the species labels: auto (0, or 45 when there are more than 8 species) or a number of degrees."),
    r("Legend", "legend.title", "", "Legend title. Blank = none."),
    r("Bars", "bar.width", "0.9", "Bar width (1 = bars touch)."),
    r("Bars", "bar.outline.color", "", "Bar outline color. Blank = none."),
    r("Text", "strip.text.size", "12", "Font size of the panel headings (pt)."),
    r("Text", "axis.title.size", "13", "Font size of the axis titles (pt)."),
    r("Text", "axis.text.size", "10", "Font size of the axis labels (pt)."),
    r("Text", "legend.text.size", "11", "Font size of the legend labels (pt)."),
    r("Legend", "legend.key.size", "2", "Size of the legend color boxes (lines)."),
    r("Save", "ggsave.dpi", "300", "Resolution of the PNG.")
  )
}

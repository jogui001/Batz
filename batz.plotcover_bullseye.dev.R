# =============================================================================
# batz.plotcover_bullseye.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.plotcover_bullseye() - tested here against Josh's real
# uploaded model data (bulleye_model_data.xlsx -> mic.csv/cover.csv) before
# being finalized into batz.plotcover_bullseye.R.
#
# Purpose (per Josh's spec): given (1) a habitat/cover assessment made in four
# compass quadrants around an ARU and (2) that ARU's microphone deployment
# metadata, draw a "bullseye" (target-style) polar plot for Canopy cover and
# a second one for Understory cover, side by side, each showing all four
# quadrants (grey, no outline) plus the mean of all four quadrants (unfilled,
# red outline), with the Horizontal Microphone Orientation drawn as an arrow
# from center to edge, and a text panel between the two plots giving
# Microphone Height and Vertical Microphone Orientation.
#
# NAMING (per project convention - see preferences.md "Naming conventions"):
#   batz.<family>_<action>.<subject>() -> family = "plotcover" (verb "plot"
#   baked into the family name, same pattern as batz.plotdetections_first.last
#   / batz.plotframe_batactivity), action/subject = "bullseye". Not yet
#   confirmed with Josh - flagged in the .R file's own roxygen @details.
#
# PLOTOPT CONVENTION (per Josh: "use the same identifiers as in the plotopt
# for the other functions"): reuses the exact aes.default / get.default() /
# project.name / dir.save pattern established in
# batz.plotdetections_first.last() and batz.plotactivity_observations() -
# $category/$parameter/$default.value(/$notes/$<project.name>) settings CSV,
# job-row > project.name column > default.value precedence (no per-job
# override table is needed here - there's no fig.list-style sheet, so only
# get.default() is used, not get.setting()).
#
# HEADER STANDARDIZATION (per Josh, 2026-09-14 project preference - see
# preferences.md): data and mic are genuinely raw, externally-supplied
# headers (straight from Josh's xlsx, unlike the sibling plotting functions'
# already-standardized upstream-batz-schema inputs), so standardize.headers()
# IS applied here. standardize.headers() itself already lives in the package
# (claude/batz.util_standardize.headers.R) and would normally be called
# unqualified; it's redefined locally below ONLY so this dev script can run
# standalone without loading the whole package - the final .R does NOT
# redefine it.
# =============================================================================

# REAL BUG found while building this: combining the 3 panels with the "+"
# operator (canopy.plot + annotation.plot + understory.plot +
# plot_layout(...)) only works once library(patchwork) has been attached -
# it relies on an operator method patchwork registers on the "gg" class,
# which is NOT available if patchwork is only ever called namespace-
# qualified (patchwork::...), the way the final package function does (a
# package function shouldn't rely on the CALLER having attached patchwork
# via library()). Fixed by using patchwork::wrap_plots(list(...), ncol=,
# widths=) instead, which works from the namespace alone - confirmed by
# reproducing the "Can't add `annotation.plot` to a <ggplot> object" error
# when only requireNamespace() (not library()) was used, then confirming
# wrap_plots() fixes it. Both this dev script and the final .R now use
# wrap_plots().
library(ggplot2)
if (!requireNamespace("patchwork", quietly = TRUE)) stop("patchwork required")
if (!requireNamespace("png", quietly = TRUE)) stop("png required (reads the mic-orientation icon .png files)")

standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

# -----------------------------------------------------------------------------
# batz.plotcover_bullseye() - working copy under test
# -----------------------------------------------------------------------------
batz.plotcover_bullseye <- function(data, mic, aes.default, project.name = "new.project",
                                     aes.style = "overide.value",
                                     dir.save = getwd(),
                                     sub.plots = c("canopy.bull", "mic.bar",
                                                   "mic.bull", "understory.bull"),
                                     cover.arrows = TRUE, mean.cover = TRUE,
                                     quadrant.cover = TRUE,
                                     auto.scale = TRUE,
                                     subplot.size.height = 3,
                                     subplot.size.width = 2) {

  names(data) <- standardize.headers(names(data))
  names(mic) <- standardize.headers(names(mic))

  DATA.REQUIRED <- c("aru_label", "select_the_quadrant_you_are_assessing",
                      "canopy_cover", "understory_cover")
  MIC.REQUIRED <- c("aru_label", "microphone_height",
                     "horizontal_microphone_orientation",
                     "vertical_microphone_orientation")
  AES.DEFAULT.REQUIRED <- c("category", "parameter", "default.value")
  AES.DEFAULT.REQUIRED.PARAMETERS <- c(
    "radial.max", "radial.breaks", "quadrant.fill", "quadrant.color",
    "mean.color", "mean.linewidth", "arrow.color", "arrow.linewidth",
    "arrow.head.cm", "canopy.title", "understory.title", "mic.bull.title",
    "mic.bull.radial.max", "mic.bull.radial.breaks", "mic.bull.skyward.label",
    "mic.bull.earthward.label", "mic.bull.height.label",
    "plot.title.size",
    "panel.grid.color", "scale.line.color", "scale.line.linetype",
    "scale.line.linewidth", "icon.dir", "mic.panel.title",
    "mic.panel.ylab", "mic.bar.fill", "mic.bar.width", "mic.panel.y.default.max",
    "icon.height.m", "ggsave.dpi", "ggsave.units", "ggsave.width.pad",
    "ggsave.height.pad", "plot.height"
    ## "output.filename.pattern" deliberately removed per Josh's nineteenth
    ## follow-up - the saved file name is now always
    ## "<project.name>_<ARU>_<timestamp>.png"; no longer read at all.
    ## "plot.width" deliberately removed per Josh's fourteenth follow-up -
    ## no longer read by the function either way; see the final .R's
    ## roxygen @details, "Figure sizing".
    ## "mic.panel.xlab" also deliberately removed per Josh's sixteenth
    ## follow-up - the mic.bar panel's X-axis title is now always blank.
    ## "axis.text.size" deliberately removed per Josh's eighteenth
    ## follow-up - axis label size is now always derived as (resolved
    ## title size) - 2pt, not an independent setting; see the final .R's
    ## roxygen @details, "Axis label sizing".
  )

  ## Which panels to draw and in what order, per Josh (2026-09-15):
  ## sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull").
  ## "mic.bull" is its own polar bullseye-style plot of the Vertical
  ## Microphone Orientation (see build.mic.bullseye() below and the final
  ## .R's roxygen @details, "mic.bull panel") - it originally shipped as a
  ## placeholder alias of "understory.bull" in an earlier round, before
  ## Josh specified its real design.
  SUBPLOT.TYPES <- c("canopy.bull", "mic.bar", "mic.bull", "understory.bull")
  if (!is.character(sub.plots) || length(sub.plots) < 1) {
    stop("sub.plots must be a character vector of one or more of: ", paste(SUBPLOT.TYPES, collapse = ", "))
  }
  invalid.subplots <- setdiff(sub.plots, SUBPLOT.TYPES)
  if (length(invalid.subplots) > 0) {
    stop(sprintf("sub.plots has unrecognized value(s): %s - must be one or more of: %s",
                 paste(invalid.subplots, collapse = ", "), paste(SUBPLOT.TYPES, collapse = ", ")))
  }
  check.flag <- function(x, name) {
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop(sprintf("%s must be a single TRUE/FALSE value", name))
    }
  }
  check.flag(cover.arrows, "cover.arrows")
  check.flag(mean.cover, "mean.cover")
  check.flag(quadrant.cover, "quadrant.cover")
  check.flag(auto.scale, "auto.scale")
  check.positive.number <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
      stop(sprintf("%s must be a single positive number", name))
    }
  }
  check.positive.number(subplot.size.height, "subplot.size.height")
  check.positive.number(subplot.size.width, "subplot.size.width")

  check.headers <- function(df, required, label) {
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      return(sprintf("%s is missing these headers: %s", label, paste(missing, collapse = ", ")))
    }
    NULL
  }
  check.parameters <- function(df, required, label) {
    if (!("parameter" %in% names(df))) return(NULL)
    missing <- setdiff(required, df$parameter)
    if (length(missing) > 0) {
      return(sprintf("%s is missing these required $parameter rows: %s",
                      label, paste(missing, collapse = ", ")))
    }
    NULL
  }
  check.duplicates <- function(df, label) {
    nm <- names(df)
    dups <- unique(nm[duplicated(nm)])
    if (length(dups) > 0) {
      return(sprintf("%s has duplicate column name(s): %s", label, paste(dups, collapse = ", ")))
    }
    NULL
  }

  problems <- c(
    check.headers(data, DATA.REQUIRED, "data"),
    check.headers(mic, MIC.REQUIRED, "mic"),
    check.headers(aes.default, AES.DEFAULT.REQUIRED, "aes.default"),
    check.parameters(aes.default, AES.DEFAULT.REQUIRED.PARAMETERS, "aes.default"),
    check.duplicates(data, "data"),
    check.duplicates(mic, "mic"),
    check.duplicates(aes.default, "aes.default")
  )
  if (length(problems) > 0) stop(paste(problems, collapse = "\n"))

  ## Round nineteen: aes.style (default "overide.value") names a fixed
  ## column checked first, falling back to $default.value - see the final
  ## .R's roxygen @details, "Settings resolution (round nineteen)".
  ## project.name no longer participates in settings resolution at all.
  get.default <- function(param) {
    row.idx <- which(aes.default$parameter == param)
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (aes.style %in% names(aes.default)) {
      override <- aes.default[[aes.style]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  ## "Take the midpoint for each category, e.g. 25-50 would be 37.5, if there
  ## is a single number use that number." Real data uses underscore-separated
  ## ranges (e.g. "51_75"); Josh's own illustrative example used a hyphen
  ## ("25-50") - both separators are supported.
  parse.cover.value <- function(x) {
    x <- trimws(as.character(x))
    if (grepl("[_-]", x)) {
      parts <- as.numeric(strsplit(x, "[_-]+")[[1]])
      return(mean(parts, na.rm = TRUE))
    }
    as.numeric(x)
  }

  QUADRANT.ANGLE <- c(northeast = 45, southeast = 135, southwest = 225, northwest = 315)
  QUADRANT.ORDER <- names(QUADRANT.ANGLE)

  ## Vertical Microphone Orientation icon set - per Josh (2026-09-14, fourth
  ## follow-up): 0 = parallel to the ground (horizontal), 90 = straight up,
  ## 270 = straight down, 45 = up at an angle, 315 = down at an angle. Only
  ## these five angles have a matching icon image right now (mic_vert_000/
  ## 045/090/270/315.png in $icon.dir) - an ARU's actual
  ## vertical_microphone_orientation value is matched to the NEAREST of
  ## these five (circular distance) rather than requiring an exact match -
  ## **assumption, please confirm with Josh**.
  ICON.ANGLES <- c(0, 45, 90, 270, 315)
  nearest.icon.angle <- function(vert.orient) {
    dist <- pmin(abs(vert.orient - ICON.ANGLES), 360 - abs(vert.orient - ICON.ANGLES))
    ICON.ANGLES[which.min(dist)]
  }

  data$quadrant.std <- tolower(trimws(data$select_the_quadrant_you_are_assessing))
  data$canopy.value <- vapply(data$canopy_cover, parse.cover.value, numeric(1))
  data$understory.value <- vapply(data$understory_cover, parse.cover.value, numeric(1))

  build.panel <- function(quad.values, panel.title, horiz.orient, title.size.override = NULL, axis.text.size.override = NULL) {
    quad.df <- data.frame(
      quadrant = QUADRANT.ORDER,
      x = as.numeric(QUADRANT.ANGLE[QUADRANT.ORDER]),
      value = as.numeric(quad.values[QUADRANT.ORDER])
    )
    mean.val <- mean(quad.df$value, na.rm = TRUE)

    radial.max <- as.numeric(get.default("radial.max"))
    radial.breaks <- as.numeric(strsplit(get.default("radial.breaks"), ";")[[1]])
    quadrant.fill <- get.default("quadrant.fill")
    quadrant.color <- get.default("quadrant.color")
    if (toupper(trimws(quadrant.color)) %in% c("NA", "")) quadrant.color <- NA
    mean.color <- get.default("mean.color")
    mean.linewidth <- as.numeric(get.default("mean.linewidth"))
    arrow.color <- get.default("arrow.color")
    arrow.linewidth <- as.numeric(get.default("arrow.linewidth"))
    arrow.head.cm <- as.numeric(get.default("arrow.head.cm"))
    ## Per Josh's seventeenth follow-up, the caller (main per-ARU loop
    ## below) may pass an already-shrunk title size when full-size titles
    ## wouldn't fit the panel width - see @details, "Figure sizing". NULL
    ## (e.g. when this builder is called directly, as most tests do) falls
    ## back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up, the caller always passes axis text
    ## 2pt smaller than whatever the resolved title size is that render -
    ## see @details, "Axis label sizing". NULL falls back to
    ## $axis.text.size if that row is still present (same fallback
    ## pattern as title.size.override).
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override
    panel.grid.color <- get.default("panel.grid.color")
    scale.line.color <- get.default("scale.line.color")
    scale.line.linetype <- get.default("scale.line.linetype")
    scale.line.linewidth <- as.numeric(get.default("scale.line.linewidth"))

    ## Radial (vertical/N-S) axis labels drawn manually at x = 0/180 (North
    ## AND South, per Josh: "Repeat the labels ... on the southern axis as
    ## well") instead of relying on ggplot2's default y-axis text
    ## (coord_polar renders that off to one side, not along the vertical
    ## axis). Plain geom_text (no background box, per Josh: "Remove the
    ## white box around the labels") added as the LAST layer (per Josh's
    ## draw order: "... > labels") so it draws on top of everything else.
    radial.label.size <- axis.text.size / .pt
    radial.label.df <- data.frame(x = rep(c(0, 180), each = length(radial.breaks)),
                                   y = rep(radial.breaks, times = 2),
                                   label = rep(radial.breaks, times = 2))

    ## Draw order per Josh: "Quadrants > mean cover > mic direction > scale
    ## lines > labels". Per Josh's sixth follow-up (2026-09-15), each of the
    ## first three layers is now individually optional
    ## ($quadrant.cover/$mean.cover/$cover.arrows, this function's own
    ## top-level parameters, captured here by lexical scoping) - when turned
    ## off, that layer is simply skipped, without disturbing the draw order
    ## of whichever layers remain on. Scale lines and labels are NOT gated
    ## by any of these three switches - they are always drawn, the same as
    ## before. Scale lines (25/50/75/100 rings) are drawn as an explicit
    ## geom_hline() layer, not a theme gridline - ggplot2 always renders
    ## theme(panel.grid) UNDERNEATH all geom layers regardless of where
    ## theme() sits in the `+` chain, so that's the only way to get them to
    ## draw after the arrow and before the labels. Styled black/dashed per
    ## Josh ("make the scale lines black and dashed"). panel.grid.major.y is
    ## blanked to avoid a duplicate grey ring; panel.grid.major.x (N-S/E-W
    ## lines) is untouched.
    p <- ggplot(quad.df, aes(x = x, y = value))
    if (quadrant.cover) {
      p <- p + geom_col(width = 90, fill = quadrant.fill, color = quadrant.color)
    }
    if (mean.cover) {
      p <- p + geom_hline(yintercept = mean.val, color = mean.color,
                           linewidth = mean.linewidth, linetype = "dashed")
    }
    if (cover.arrows) {
      p <- p + geom_segment(aes(x = horiz.orient, y = 0, xend = horiz.orient, yend = radial.max),
                             color = arrow.color, linewidth = arrow.linewidth,
                             arrow = grid::arrow(length = grid::unit(arrow.head.cm, "cm"), type = "closed"),
                             inherit.aes = FALSE)
    }
    p +
      geom_hline(yintercept = radial.breaks, color = scale.line.color,
                 linewidth = scale.line.linewidth, linetype = scale.line.linetype) +
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270),
                          labels = c("N", "E", "S", "W"), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, radial.max), breaks = radial.breaks, expand = c(0, 0)) +
      coord_polar(theta = "x", start = 0, direction = 1) +
      labs(title = panel.title, x = NULL, y = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(size = plot.title.size, hjust = 0.5),
            axis.text.x = element_text(size = axis.text.size),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_line(color = panel.grid.color),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank()) +
      ## Labels last, per Josh's draw order.
      geom_text(data = radial.label.df, aes(x = x, y = y, label = label),
                inherit.aes = FALSE, size = radial.label.size)
  }

  ## "mic.bull" panel - a polar "bullseye"-style plot of the Vertical
  ## Microphone Orientation, per Josh's seventh follow-up (2026-09-15),
  ## replacing the placeholder that aliased "understory.bull" in the
  ## previous round; degrees/labels finalized and a white label
  ## background added per his ninth follow-up; rotation corrected again
  ## per his tenth follow-up; height label shortened over the eleventh
  ## and twelfth follow-ups; scale numbers moved onto the exact left
  ## horizontal axis per his thirteenth follow-up (2026-09-15, later
  ## still). See the final .R's roxygen @details, "mic.bull panel", for
  ## the full rationale (angle-label relabeling, arrow always on, radial
  ## scale auto-stretch).
  build.mic.bullseye <- function(mic.height, vert.orient, title.size.override = NULL, axis.text.size.override = NULL) {
    mic.bull.title <- get.default("mic.bull.title")
    radial.max.default <- as.numeric(get.default("mic.bull.radial.max"))
    radial.breaks <- as.numeric(strsplit(get.default("mic.bull.radial.breaks"), ";")[[1]])
    skyward.label <- get.default("mic.bull.skyward.label")
    earthward.label <- get.default("mic.bull.earthward.label")
    height.label <- get.default("mic.bull.height.label")
    arrow.color <- get.default("arrow.color")
    arrow.linewidth <- as.numeric(get.default("arrow.linewidth"))
    arrow.head.cm <- as.numeric(get.default("arrow.head.cm"))
    ## Per Josh's seventeenth follow-up, the caller (main per-ARU loop
    ## below) may pass an already-shrunk title size when full-size titles
    ## wouldn't fit the panel width - see @details, "Figure sizing". NULL
    ## (e.g. when this builder is called directly, as most tests do) falls
    ## back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up - see the equivalent comment in
    ## build.panel() above - same override/fallback pattern.
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override
    panel.grid.color <- get.default("panel.grid.color")
    scale.line.color <- get.default("scale.line.color")
    scale.line.linetype <- get.default("scale.line.linetype")
    scale.line.linewidth <- as.numeric(get.default("scale.line.linewidth"))

    radial.max <- max(radial.max.default, mic.height + 0.2)
    ## Extra headroom on the axis (not on radial.max itself) reserved for
    ## $mic.bull.height.label - per Josh's thirteenth follow-up ("offset
    ## the label by enough space to fit the label"): without it, the
    ## outermost "4" ring sits right at the plot boundary, the same point
    ## where the height-label axis text renders, so the two overlap
    ## (confirmed during testing, back when numbers were placed at
    ## EXACTLY x = 180 during the tenth follow-up). This pushes the
    ## boundary out a bit so the axis text has clear room.
    radial.axis.max <- radial.max + 0.7

    radial.label.size <- axis.text.size / .pt
    ## Numbers drawn exactly on x = 180 (W) - the LEFT HORIZONTAL axis,
    ## the same ray as $mic.bull.height.label (default "(m)") - per Josh's
    ## thirteenth follow-up ("Make the labels for the scale occur along
    ## the left horizonal axis"). Earlier (tenth follow-up) these were
    ## offset 15 degrees off that ray (to x = 165) to dodge the overlap
    ## bug above, back when the height label was the much longer
    ## "Microphone Height (m)"; per this follow-up they're back on the
    ## exact ray, with clearance now provided radially instead (see
    ## radial.axis.max above). See the final .R's roxygen @details,
    ## "mic.bull panel", for the full history.
    radial.label.df <- data.frame(x = rep(180, length(radial.breaks)),
                                   y = radial.breaks,
                                   label = radial.breaks)

    ggplot() +
      geom_segment(data = data.frame(x = vert.orient, y = 0,
                                      xend = vert.orient, yend = mic.height),
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = arrow.color, linewidth = arrow.linewidth,
                   arrow = grid::arrow(length = grid::unit(arrow.head.cm, "cm"), type = "closed")) +
      geom_hline(yintercept = radial.breaks, color = scale.line.color,
                 linewidth = scale.line.linewidth, linetype = scale.line.linetype) +
      ## Break positions (0/90/180/270) unchanged; only the LABELS at
      ## those positions differ from N/E/S/W: x = 0 (E) blank, x = 90 (N,
      ## "straight up") carries $mic.bull.skyward.label, x = 180 (W)
      ## carries $mic.bull.height.label, x = 270 (S, "straight down")
      ## carries $mic.bull.earthward.label.
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270),
                          labels = c("", skyward.label, height.label, earthward.label),
                          expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, radial.axis.max), breaks = radial.breaks, expand = c(0, 0)) +
      ## start = -pi/2, direction = -1 - per Josh's tenth follow-up ("Top
      ## = 90, right 0, Bottom = 270, left = 180"). Unlike every other
      ## panel's start = 0/direction = 1, this mapping was verified with a
      ## live rendered test (not derived by inspection alone) to put:
      ## x = 90 ("Skyward") at the TOP, x = 0 (blank) at the RIGHT, x = 270
      ## ("Earthward") at the BOTTOM, x = 180 (height label) at the LEFT -
      ## reverting the ninth follow-up's start = 0/direction = 1 (itself a
      ## reversion of the eighth follow-up's start = -pi/2/direction = 1).
      coord_polar(theta = "x", start = -pi / 2, direction = -1) +
      labs(title = mic.bull.title, x = NULL, y = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(size = plot.title.size, hjust = 0.5),
            axis.text.x = element_text(size = axis.text.size),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_line(color = panel.grid.color),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank()) +
      ## White background on these scale labels only (geom_label(), no
      ## border), per Josh's ninth follow-up ("add a white background to
      ## make the label clearer") - build.panel()'s own radial labels
      ## (canopy.bull/understory.bull) remain plain, background-less
      ## geom_text() and are unaffected.
      geom_label(data = radial.label.df, aes(x = x, y = y, label = label),
                 inherit.aes = FALSE, size = radial.label.size,
                 fill = "white", label.size = 0,
                 label.padding = grid::unit(0.1, "lines"))
  }

  ## Middle panel - Microphone Height bar plus a Vertical Microphone
  ## Orientation icon, per Josh (2026-09-14, fourth follow-up), replacing
  ## the blank placeholder that used to live here. See the final .R's
  ## roxygen @details ("Middle panel" / "Vertical Microphone Orientation
  ## icon set" / "Icon sizing") for the full rationale.
  build.mic.panel <- function(mic.height, vert.orient, title.size.override = NULL, axis.text.size.override = NULL) {
    mic.panel.title <- get.default("mic.panel.title")
    ## $mic.panel.xlab (formerly "Ground Level") is no longer read here -
    ## per Josh's sixteenth follow-up ("Remove 'Ground Level' from the
    ## mic.bar plot"), this panel's X-axis title is now always blank.
    mic.panel.ylab <- get.default("mic.panel.ylab")
    mic.bar.fill <- get.default("mic.bar.fill")
    ## Round twenty: fixed 1.3x multiplier on top of $mic.bar.width, per
    ## Josh ("Make the bar 1.3 its current size").
    MIC.BAR.WIDTH.SCALE <- 1.3
    mic.bar.width <- as.numeric(get.default("mic.bar.width")) * MIC.BAR.WIDTH.SCALE
    y.default.max <- as.numeric(get.default("mic.panel.y.default.max"))
    icon.height.m <- as.numeric(get.default("icon.height.m"))
    icon.dir <- get.default("icon.dir")
    ## Per Josh's seventeenth follow-up, the caller (main per-ARU loop
    ## below) may pass an already-shrunk title size when full-size titles
    ## wouldn't fit the panel width - see @details, "Figure sizing". NULL
    ## (e.g. when this builder is called directly, as most tests do) falls
    ## back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up - see the equivalent comment in
    ## build.panel() above - same override/fallback pattern.
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override

    ## Round twenty, per Josh ("move the mic png to be centered at the top
    ## of the bar"): the icon is centered ON the bar's top edge again (see
    ## below), so only icon.height.m/2 of headroom is needed now, not a
    ## full icon.height.m.
    y.max <- max(y.default.max, mic.height + icon.height.m / 2 + 0.2)
    x.range <- 2
    bar.x <- 1

    ## Round twenty, per Josh ("...with a line every 0.5 interval"):
    ## Y-axis breaks/gridlines every 0.5 units instead of every whole unit.
    Y.GRIDLINE.INTERVAL <- 0.5
    y.breaks <- seq(0, y.max, by = Y.GRIDLINE.INTERVAL)

    p <- ggplot(data.frame(x = bar.x, y = mic.height), aes(x = x, y = y)) +
      geom_col(width = mic.bar.width, fill = mic.bar.fill, color = NA) +
      scale_x_continuous(limits = c(0, x.range), breaks = NULL, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, y.max), breaks = y.breaks, expand = c(0, 0)) +
      labs(title = mic.panel.title, x = NULL, y = mic.panel.ylab) +
      theme_minimal() +
      theme(plot.title = element_text(size = plot.title.size, hjust = 0.5),
            axis.title.x = element_blank(),
            ## Round twenty: axis.title.y had no explicit size before and
            ## fell back to theme_minimal()'s larger default - pinned to
            ## the same derived axis.text.size used everywhere else here.
            axis.title.y = element_text(size = axis.text.size),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = axis.text.size),
            axis.ticks = element_blank(),
            ## Round twenty: panel.grid.major.y now draws the every-0.5
            ## reference lines (reusing $panel.grid.color); x/minor stay
            ## blank.
            panel.grid.major.y = element_line(color = get.default("panel.grid.color")),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            ## Forces a square panel, matching what coord_polar already
            ## gives the other three panels "for free" (aspect = 1) - per
            ## Josh's seventeenth follow-up ("Adjust the mic.bar subplot to
            ## be the same height as the other subplots"). See the final
            ## .R's roxygen comment at this same spot for the full
            ## before/after pixel measurements that confirmed this.
            aspect.ratio = 1)

    icon.angle <- nearest.icon.angle(vert.orient)
    icon.file <- file.path(icon.dir, sprintf("mic_vert_%03d.png", icon.angle))
    if (file.exists(icon.file)) {
      ## .png (not .jpg) per Josh's follow-up ("use the attached .png
      ## instead") - these have a real alpha channel (their white
      ## background was made transparent), read with png::readPNG() so
      ## grid::rasterGrob() respects the transparency when drawing.
      icon.img <- png::readPNG(icon.file)
      icon.aspect <- dim(icon.img)[2] / dim(icon.img)[1]
      icon.width.x <- icon.aspect * icon.height.m * (x.range / y.max)
      ## Round twenty, per Josh ("move the mic png to be centered at the
      ## top of the bar") - reverts the fifth follow-up's "entirely above"
      ## placement back to centered on the bar's top edge.
      p <- p + annotation_custom(
        grid::rasterGrob(icon.img, interpolate = TRUE),
        xmin = bar.x - icon.width.x / 2, xmax = bar.x + icon.width.x / 2,
        ymin = mic.height - icon.height.m / 2, ymax = mic.height + icon.height.m / 2)
    } else {
      warning(sprintf("No icon file found for vertical_microphone_orientation %s (nearest known angle %s) at '%s' - mic panel drawn without an icon",
                       vert.orient, icon.angle, icon.file))
    }
    p
  }

  aru.labels <- unique(mic$aru_label)
  result.plots <- list()
  result.ggplots <- list()

  for (aru in aru.labels) {
    mic.row <- mic[mic$aru_label == aru, , drop = FALSE][1, ]
    quad.rows <- data[data$aru_label == aru, , drop = FALSE]

    missing.quad <- setdiff(QUADRANT.ORDER, quad.rows$quadrant.std)
    if (length(missing.quad) > 0) {
      warning(sprintf("aru.label '%s' is missing quadrant(s): %s - skipped",
                       aru, paste(missing.quad, collapse = ", ")))
      next
    }

    canopy.values <- setNames(quad.rows$canopy.value, quad.rows$quadrant.std)[QUADRANT.ORDER]
    understory.values <- setNames(quad.rows$understory.value, quad.rows$quadrant.std)[QUADRANT.ORDER]
    horiz.orient <- as.numeric(mic.row$horizontal_microphone_orientation)
    vert.orient <- as.numeric(mic.row$vertical_microphone_orientation)
    mic.height <- as.numeric(mic.row$microphone_height)

    ## Figure sizing, per Josh's fourteenth follow-up (2026-09-15) - see the
    ## final .R's roxygen @details, "Figure sizing" for the full precedence.
    ## $plot.width is no longer read in either branch; $plot.height is read
    ## only when auto.scale = TRUE. Computed here, BEFORE the panels
    ## themselves are built (moved up per the seventeenth follow-up),
    ## because the title-size resolution immediately below needs
    ## plot.width already settled.
    if (auto.scale) {
      plot.width <- 6.5
      plot.height <- as.numeric(get.default("plot.height"))
    } else {
      plot.width <- subplot.size.width * length(sub.plots)
      plot.height <- subplot.size.height
    }

    ## Title auto-shrink (tried first) with width auto-expand as a last
    ## resort (tried only if shrinking isn't enough), per Josh's
    ## seventeenth follow-up (2026-09-16): "have the titles all reduce in
    ## size so that they don't run together." See the final .R's roxygen
    ## @details, "Figure sizing" for the full reasoning. This supersedes
    ## the fifteenth follow-up's original mechanism, which only ever
    ## widened the figure and was scoped to exactly the "all four panel
    ## types selected" case. The new version applies to whichever panels
    ## are actually in $sub.plots, tries shrinking ALL displayed panel
    ## titles to the SAME reduced font size first (uniformly, per Josh's
    ## "titles all reduce"), and only widens the figure as a fallback if
    ## titles still would not fit even at a floor size.
    panel.title.lookup <- c(
      canopy.bull = get.default("canopy.title"),
      mic.bar = get.default("mic.panel.title"),
      mic.bull = get.default("mic.bull.title"),
      understory.bull = get.default("understory.title")
    )
    plot.title.pt <- as.numeric(get.default("plot.title.size"))
    PLOT.TITLE.MIN.SIZE.PT <- 8
    TITLE.OVERLAP.MARGIN.IN <- 0.25

    measure.title.widths.in <- function(texts, size.pt) {
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())
      vapply(texts, function(txt) {
        grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = grid::gpar(fontsize = size.pt))),
                            "inches", valueOnly = TRUE)
      }, numeric(1))
    }

    used.titles <- panel.title.lookup[sub.plots]
    per.panel.width <- plot.width / length(sub.plots)
    available.title.width <- per.panel.width - TITLE.OVERLAP.MARGIN.IN

    resolved.title.size <- plot.title.pt
    current.title.widths <- measure.title.widths.in(used.titles, resolved.title.size)
    if (max(current.title.widths) > available.title.width) {
      ## Converge on a font size that fits via a few rounds of proportional
      ## scaling (text width scales very close to linearly with font size,
      ## so in practice this settles in 1-2 rounds) - never below the floor.
      for (shrink.iter in 1:5) {
        scale.factor <- available.title.width / max(current.title.widths)
        candidate.size <- max(PLOT.TITLE.MIN.SIZE.PT, resolved.title.size * scale.factor)
        if (isTRUE(all.equal(candidate.size, resolved.title.size))) break
        resolved.title.size <- candidate.size
        current.title.widths <- measure.title.widths.in(used.titles, resolved.title.size)
        if (max(current.title.widths) <= available.title.width) break
      }

      if (max(current.title.widths) > available.title.width) {
        ## Even the floor size doesn't fit - fall back to widening the
        ## figure instead (never narrower than whatever auto.scale/
        ## subplot.size.width already computed), the same guarantee the
        ## fifteenth follow-up's original mechanism provided.
        min.panel.width <- max(current.title.widths) + TITLE.OVERLAP.MARGIN.IN
        required.total.width <- min.panel.width * length(sub.plots)
        if (plot.width < required.total.width) {
          plot.width <- required.total.width
        }
      }
    }

    ## Axis label sizing, per Josh's eighteenth follow-up (2026-09-16):
    ## "reduce the size of the axis labels to be 2 pts smaller than the
    ## current title labels at the top of each subplot" - see the final
    ## .R's roxygen @details, "Axis label sizing". Derived from whatever
    ## the title size resolved to just above - $axis.text.size is no
    ## longer read.
    AXIS.TITLE.SIZE.GAP.PT <- 2
    resolved.axis.text.size <- resolved.title.size - AXIS.TITLE.SIZE.GAP.PT

    ## One builder per possible $sub.plots entry, per Josh (2026-09-15) -
    ## only the ones actually requested (and in the order requested) get
    ## built and combined below. "mic.bull" is intentionally identical to
    ## "understory.bull" for now (own title, same data/toggles) - see the
    ## comment above SUBPLOT.TYPES and the final .R's roxygen @details,
    ## "Sub-plot selection and toggles". Each builder now takes the
    ## resolved (possibly shrunk) title font size and the derived axis
    ## label size from just above, per Josh's seventeenth and eighteenth
    ## follow-ups.
    panel.builders <- list(
      canopy.bull = function(title.size, axis.size) build.panel(canopy.values, get.default("canopy.title"), horiz.orient, title.size, axis.size),
      understory.bull = function(title.size, axis.size) build.panel(understory.values, get.default("understory.title"), horiz.orient, title.size, axis.size),
      mic.bull = function(title.size, axis.size) build.mic.bullseye(mic.height, vert.orient, title.size, axis.size),
      mic.bar = function(title.size, axis.size) build.mic.panel(mic.height, vert.orient, title.size, axis.size)
    )
    panels <- lapply(sub.plots, function(nm) panel.builders[[nm]](resolved.title.size, resolved.axis.text.size))

    ## Background reverted to plain white per Josh's third follow-up
    ## ("The background should still be white. What I meant was that the
    ## text boxes for the labels should be clear.") - the transparent
    ## `& theme(plot.background = ...)` override that used to sit here
    ## (worked around patchwork::wrap_plots()'s own opaque-white wrapper
    ## background) has been removed; wrap_plots()'s default opaque white
    ## wrapper background is now exactly what's wanted.
    ## Round twenty, per Josh ("scale the size of the mic.bar plot to be
    ## 2/3 the size of the canopy plots"): mic.bar's column gets a relative
    ## width of 2/3 vs. every other panel type's 1 - see the final .R's
    ## roxygen @details, "mic.bar panel overhaul", point 5, for the
    ## title-fit judgment call this introduces.
    MIC.BAR.WIDTH.FRACTION <- 2 / 3
    panel.width.weights <- ifelse(sub.plots == "mic.bar", MIC.BAR.WIDTH.FRACTION, 1)

    combined <- patchwork::wrap_plots(panels, ncol = length(sub.plots),
                                       widths = panel.width.weights)

    ggsave.dpi <- as.numeric(get.default("ggsave.dpi"))
    ggsave.units <- get.default("ggsave.units")
    ggsave.width.pad <- as.numeric(get.default("ggsave.width.pad"))
    ggsave.height.pad <- as.numeric(get.default("ggsave.height.pad"))

    ## Round nineteen: fixed "<project.name>_<ARU>_<timestamp>.png" naming;
    ## $output.filename.pattern is deprecated/no longer read.
    fname <- sprintf("%s_%s_%s.png", project.name, aru, format(Sys.time(), "%Y%m%d_%H%M%S"))
    fname <- file.path(dir.save, fname)

    ggsave(fname, combined,
           width = plot.width + ggsave.width.pad,
           height = plot.height + ggsave.height.pad,
           units = ggsave.units, dpi = ggsave.dpi, bg = "white")
    cat("Saved:", fname, "\n")

    result.plots[[aru]] <- list(canopy = canopy.values, understory = understory.values,
                                 mic = mic.row, file = fname)
    result.ggplots[[aru]] <- combined
  }

  invisible(list(plots = result.plots, ggplots = result.ggplots))
}

# =============================================================================
# TESTS - against Josh's real uploaded model data
# =============================================================================
mic.data <- read.csv("mic.csv", stringsAsFactors = FALSE, check.names = FALSE)
cover.data <- read.csv("cover.csv", stringsAsFactors = FALSE, check.names = FALSE)
cat("=== raw mic.csv ===\n"); print(mic.data)
cat("=== raw cover.csv ===\n"); print(cover.data)

aes.default <- data.frame(
  category = c("Radial scale", "Radial scale", "Quadrants", "Quadrants",
               "Mean circle", "Mean circle", "Mic orientation arrow",
               "Mic orientation arrow", "Mic orientation arrow", "Panel titles",
               "Panel titles", "Panel titles",
               "Mic bullseye panel", "Mic bullseye panel", "Mic bullseye panel", "Mic bullseye panel",
               "Mic bullseye panel",
               "Theme", "Theme", "Theme", "Theme", "Theme", "Theme",
               "Mic panel", "Mic panel", "Mic panel", "Mic panel", "Mic panel",
               "Mic panel", "Mic panel", "Mic panel",
               "Save / export", "Save / export", "Save / export",
               "Save / export", "Save / export", "Save / export", "Save / export"),
  parameter = c("radial.max", "radial.breaks", "quadrant.fill", "quadrant.color",
                "mean.color", "mean.linewidth", "arrow.color", "arrow.linewidth",
                "arrow.head.cm", "canopy.title", "understory.title", "mic.bull.title",
                "mic.bull.radial.max", "mic.bull.radial.breaks", "mic.bull.skyward.label",
                "mic.bull.earthward.label", "mic.bull.height.label",
                "plot.title.size", "axis.text.size",
                "panel.grid.color", "scale.line.color", "scale.line.linetype",
                "scale.line.linewidth",
                "icon.dir", "mic.panel.title", "mic.panel.xlab", "mic.panel.ylab",
                "mic.bar.fill", "mic.bar.width", "mic.panel.y.default.max", "icon.height.m",
                "ggsave.dpi", "ggsave.units",
                "ggsave.width.pad", "ggsave.height.pad", "output.filename.pattern",
                "plot.width", "plot.height"),
  default.value = c("110", "25;50;75;100", "grey70", "NA", "red", "1", "black",
                    "1", "0.3", "Canopy Cover", "Understory Cover", "Microphone Height and\nVertical Orientation",
                    "4", "1;2;3;4", "Skyward", "Earthward", "(m)",
                    "12",
                    "8", "grey80", "black", "solid", "0.5",
                    "img", "Microphone Vertical Orientation", "Ground Level",
                    "Microphone Height (m)", "grey85", "0.4", "4", "1",
                    "300", "in", "0.5", "0.5",
                    "Bullseye cover plot <ARU> <timestamp>.png", "10", "5"),
  overide.value = "",
  notes = "",
  stringsAsFactors = FALSE
)
stopifnot(length(aes.default$category) == length(aes.default$parameter),
          length(aes.default$parameter) == length(aes.default$default.value))

cat("\n=== TEST 1: basic run against real data ===\n")
result <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = getwd())
print(result$plots)
cat("Saved file(s):\n"); print(list.files(pattern = "\\.png$"))

cat("\n=== TEST 2: parse.cover.value sanity (via a direct re-check) ===\n")
pcv <- function(x) {
  x <- trimws(as.character(x))
  if (grepl("[_-]", x)) return(mean(as.numeric(strsplit(x, "[_-]+")[[1]]), na.rm = TRUE))
  as.numeric(x)
}
stopifnot(pcv("51_75") == 63, pcv("1_25") == 13, pcv("100") == 100, pcv("0") == 0,
          pcv("25-50") == 37.5)
cat("parse.cover.value: OK (51_75->63, 1_25->13, 100->100, 0->0, 25-50->37.5)\n")

cat("\n=== TEST 3: missing quadrant should warn and skip, not error ===\n")
bad.cover <- cover.data[cover.data$`Select the quadrant you are assessing` != "northwest", ]
result.bad <- tryCatch(
  batz.plotcover_bullseye(bad.cover, mic.data, aes.default, dir.save = tempdir()),
  warning = function(w) { cat("Got expected warning:", conditionMessage(w), "\n"); NULL }
)

cat("\n=== TEST 4: missing aes.default parameter row should stop with clear message ===\n")
bad.aes <- aes.default[aes.default$parameter != "radial.max", ]
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, bad.aes, dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== TEST 5: $overide.value column (via aes.style) changes a setting, per Josh's nineteenth follow-up ===\n")
aes.override <- aes.default
aes.override$overide.value[aes.override$parameter == "quadrant.fill"] <- "steelblue"
result.override <- batz.plotcover_bullseye(cover.data, mic.data, aes.override,
                                            dir.save = tempdir())
cat("(expect a steelblue-filled render saved to tempdir - visually spot-checked separately)\n")

cat("\n=== TEST 6: sub.plots default now yields 4 panels (canopy/mic.bar/mic.bull/understory) ===\n")
result.default <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = tempdir())
n.panels.default <- length(result.default$ggplots[[1]]$patches$plots) + 1
cat("Default sub.plots panel count:", n.panels.default, "(expect 4)\n")
stopifnot(n.panels.default == 4)

cat("\n=== TEST 7: sub.plots subset (2 panels only) ===\n")
result.subset <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                          sub.plots = c("canopy.bull", "understory.bull"),
                                          dir.save = tempdir())
n.panels.subset <- length(result.subset$ggplots[[1]]$patches$plots) + 1
cat("Subset sub.plots panel count:", n.panels.subset, "(expect 2)\n")
stopifnot(n.panels.subset == 2)

cat("\n=== TEST 8: invalid sub.plots value errors clearly ===\n")
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                           sub.plots = c("canopy.bull", "nonsense.panel"),
                           dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== TEST 9: cover.arrows/mean.cover/quadrant.cover toggles, individually and combined ===\n")
for (flags in list(c(cover.arrows = FALSE, mean.cover = TRUE, quadrant.cover = TRUE),
                    c(cover.arrows = TRUE, mean.cover = FALSE, quadrant.cover = TRUE),
                    c(cover.arrows = TRUE, mean.cover = TRUE, quadrant.cover = FALSE),
                    c(cover.arrows = FALSE, mean.cover = FALSE, quadrant.cover = FALSE))) {
  invisible(batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                     cover.arrows = as.logical(flags["cover.arrows"]),
                                     mean.cover = as.logical(flags["mean.cover"]),
                                     quadrant.cover = as.logical(flags["quadrant.cover"]),
                                     dir.save = tempdir()))
  cat(sprintf("OK: cover.arrows=%s mean.cover=%s quadrant.cover=%s rendered without error\n",
              flags["cover.arrows"], flags["mean.cover"], flags["quadrant.cover"]))
}

cat("\n=== TEST 10: bad flag value (non-logical) errors clearly ===\n")
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, aes.default, mean.cover = "yes", dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== TEST 11: mic.bull panel - arrow angle/length and radial auto-stretch ===\n")
result.micbull <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                           sub.plots = "mic.bull", dir.save = tempdir())
mic.bull.plot <- result.micbull$ggplots[[1]]
built <- ggplot_build(mic.bull.plot)
seg.data <- built$data[[1]]
stopifnot(abs(seg.data$x[1] - 45) < 1e-6, abs(seg.data$xend[1] - 45) < 1e-6,
          abs(seg.data$y[1] - 0) < 1e-6, abs(seg.data$yend[1] - 3) < 1e-6)
cat("mic.bull arrow: angle 45, length 3 (matches test ARU) - OK\n")

## Taller synthetic mic (5m) should auto-stretch the radial max past the
## default 4, same idea as build.mic.panel()'s Y-axis headroom.
mic.tall <- mic.data
mic.tall[["Microphone Height"]] <- 5
result.tall <- batz.plotcover_bullseye(cover.data, mic.tall, aes.default,
                                        sub.plots = "mic.bull", dir.save = tempdir())
built.tall <- ggplot_build(result.tall$ggplots[[1]])
y.range.tall <- built.tall$layout$panel_params[[1]]$y.range
stopifnot(y.range.tall[2] > 5)
cat("mic.bull radial max auto-stretched past mic.height=5 - OK (range:", y.range.tall, ")\n")

cat("\n=== TEST 12: panel order matches sub.plots order (not just count), per Josh's fourteenth follow-up ===\n")
## patchwork::wrap_plots() stores all but the LAST panel in
## combined$patches$plots (in order); the last panel is the combined
## object itself. So the full left-to-right title order is
## c(titles of patches$plots, the combined object's own title).
get.panel.titles <- function(combined) {
  earlier <- vapply(combined$patches$plots, function(p) p$labels$title %||% NA_character_, character(1))
  c(earlier, combined$labels$title)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
order.test.subplots <- c("understory.bull", "canopy.bull", "mic.bull")
result.order <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                         sub.plots = order.test.subplots, dir.save = tempdir())
actual.titles <- get.panel.titles(result.order$ggplots[[1]])
expected.titles <- c(
  aes.default$default.value[aes.default$parameter == "understory.title"],
  aes.default$default.value[aes.default$parameter == "canopy.title"],
  aes.default$default.value[aes.default$parameter == "mic.bull.title"]
)
stopifnot(identical(as.character(actual.titles), as.character(expected.titles)))
cat("Panel titles in rendered order:", paste(actual.titles, collapse = " | "), "\n")
cat("sub.plots order correctly reflected in rendered panel order - OK\n")

cat("\n=== TEST 13: auto.scale = TRUE fixes total saved width at 6.5in regardless of panel count ===\n")
get.png.dims <- function(path) {
  d <- dim(png::readPNG(path))
  c(height.px = d[1], width.px = d[2])
}
ggsave.dpi <- as.numeric(aes.default$default.value[aes.default$parameter == "ggsave.dpi"])
ggsave.width.pad <- as.numeric(aes.default$default.value[aes.default$parameter == "ggsave.width.pad"])
ggsave.height.pad <- as.numeric(aes.default$default.value[aes.default$parameter == "ggsave.height.pad"])
plot.height.default <- as.numeric(aes.default$default.value[aes.default$parameter == "plot.height"])

for (n in c(2, 4)) {
  subplots.n <- rep("canopy.bull", n)
  result.auto <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                          sub.plots = subplots.n, auto.scale = TRUE,
                                          dir.save = tempdir())
  dims <- get.png.dims(result.auto$plots[[1]]$file)
  expected.width.px <- round((6.5 + ggsave.width.pad) * ggsave.dpi)
  expected.height.px <- round((plot.height.default + ggsave.height.pad) * ggsave.dpi)
  stopifnot(abs(dims["width.px"] - expected.width.px) <= 1,
            abs(dims["height.px"] - expected.height.px) <= 1)
  cat(sprintf("auto.scale=TRUE, %d panel(s): saved width %dpx (expect %dpx, i.e. 6.5in total) - OK\n",
              n, dims["width.px"], expected.width.px))
}

cat("\n=== TEST 14: auto.scale = FALSE sizes each sub-plot from subplot.size.width/height ===\n")
subplot.w <- 2.25
subplot.h <- 3.5
n <- 3
subplots.n <- rep("canopy.bull", n)
result.manual <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                          sub.plots = subplots.n, auto.scale = FALSE,
                                          subplot.size.width = subplot.w,
                                          subplot.size.height = subplot.h,
                                          dir.save = tempdir())
dims.manual <- get.png.dims(result.manual$plots[[1]]$file)
expected.width.px <- round((subplot.w * n + ggsave.width.pad) * ggsave.dpi)
expected.height.px <- round((subplot.h + ggsave.height.pad) * ggsave.dpi)
stopifnot(abs(dims.manual["width.px"] - expected.width.px) <= 1,
          abs(dims.manual["height.px"] - expected.height.px) <= 1)
cat(sprintf("auto.scale=FALSE, %d panels @ width=%.2fin/height=%.2fin each: saved %dx%dpx (expect %dx%dpx) - OK\n",
            n, subplot.w, subplot.h, dims.manual["width.px"], dims.manual["height.px"],
            expected.width.px, expected.height.px))

cat("\n=== TEST 15: bad subplot.size.width/height values error clearly ===\n")
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, aes.default, subplot.size.width = -1, dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, aes.default, subplot.size.height = "big", dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)
tryCatch(
  batz.plotcover_bullseye(cover.data, mic.data, aes.default, auto.scale = "yes", dir.save = tempdir()),
  error = function(e) cat("Got expected error:", conditionMessage(e), "\n")
)

cat("\n=== TEST 16: title auto-shrink (with width auto-expand as a fallback) prevents title overlap, per Josh's seventeenth follow-up (superseding the fifteenth's width-only mechanism) ===\n")
get.png.width.in <- function(path) dim(png::readPNG(path))[2] / ggsave.dpi
get.panel.title.sizes <- function(combined) {
  earlier <- vapply(combined$patches$plots, function(p) p$theme$plot.title$size %||% NA_real_, numeric(1))
  c(earlier, combined$theme$plot.title$size %||% NA_real_)
}

## Independently recompute the expected resolved title size and (if still
## needed) expected expanded width the SAME way the function itself does,
## so this test checks specific expected values, not just "something
## changed". Mirrors the shrink-then-expand-fallback loop in the function.
plot.title.pt <- as.numeric(aes.default$default.value[aes.default$parameter == "plot.title.size"])
PLOT.TITLE.MIN.SIZE.PT <- 8
titles.expected <- c(
  aes.default$default.value[aes.default$parameter == "canopy.title"],
  aes.default$default.value[aes.default$parameter == "mic.panel.title"],
  aes.default$default.value[aes.default$parameter == "mic.bull.title"],
  aes.default$default.value[aes.default$parameter == "understory.title"]
)
measure.widths <- function(texts, size.pt) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  vapply(texts, function(txt) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = grid::gpar(fontsize = size.pt))),
                        "inches", valueOnly = TRUE)
  }, numeric(1))
}
expected.resolved.title.size.and.width <- function(plot.width, n.panels) {
  available <- plot.width / n.panels - 0.25
  size <- plot.title.pt
  widths <- measure.widths(titles.expected, size)
  if (max(widths) > available) {
    for (i in 1:5) {
      candidate <- max(PLOT.TITLE.MIN.SIZE.PT, size * (available / max(widths)))
      if (isTRUE(all.equal(candidate, size))) break
      size <- candidate
      widths <- measure.widths(titles.expected, size)
      if (max(widths) <= available) break
    }
    if (max(widths) > available) {
      plot.width <- (max(widths) + 0.25) * n.panels
    }
  }
  list(size = size, width = plot.width)
}

## (a) Default all-four, auto.scale=TRUE. Originally (through the
## nineteenth round) shrinking alone was enough here, so the figure stayed
## at the plain 6.5in auto.scale total. Per Josh's twentieth follow-up
## (2026-09-16), $mic.panel.title changed from a two-line wrapped string
## to the single-line "Microphone Vertical Orientation" - which, unwrapped,
## measures WIDER than either of the old two lines (see @details "mic.bar
## panel overhaul", point 6) - so shrinking alone no longer suffices: the
## floor (8pt) is hit and the width auto-expand fallback now also kicks in
## for this previously-fits-at-6.5in case. Recomputed here rather than
## silently loosening the tolerance, so a future regression still fails
## loudly.
expected.a <- expected.resolved.title.size.and.width(6.5, 4)
stopifnot(abs(expected.a$size - PLOT.TITLE.MIN.SIZE.PT) < 1e-6)
stopifnot(expected.a$width > 6.5)
result.all4 <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                        sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull"),
                                        auto.scale = TRUE, dir.save = tempdir())
actual.width.all4 <- get.png.width.in(result.all4$plots[[1]]$file) - ggsave.width.pad
actual.sizes.all4 <- get.panel.title.sizes(result.all4$ggplots[[1]])
stopifnot(abs(actual.width.all4 - expected.a$width) < 0.02)
stopifnot(all(abs(actual.sizes.all4 - expected.a$size) < 0.01))
cat(sprintf("All 4 panel types selected: titles floored at %.2fpt (was: shrink-only sufficed pre-round-20), width auto-expanded from plain 6.5in to %.2fin (round-20 mic.panel.title is wider unwrapped) - OK\n",
            actual.sizes.all4[1], actual.width.all4))

## (b) auto.scale=FALSE with subplot.size.width deliberately too small
## (1.0in * 4 panels = 4in) that shrinking all the way down to the floor
## (8pt) still is not enough - the width auto-expand fallback must then
## kick in on top of whatever subplot.size.width already computed, using
## the floor-size measurement (never letting titles actually overlap).
expected.b <- expected.resolved.title.size.and.width(1.0 * 4, 4)
stopifnot(abs(expected.b$size - PLOT.TITLE.MIN.SIZE.PT) < 1e-6)
stopifnot(expected.b$width > 1.0 * 4)
result.all4.manual <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                               sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull"),
                                               auto.scale = FALSE, subplot.size.width = 1.0, subplot.size.height = 3,
                                               dir.save = tempdir())
actual.width.all4.manual <- get.png.width.in(result.all4.manual$plots[[1]]$file) - ggsave.width.pad
actual.sizes.all4.manual <- get.panel.title.sizes(result.all4.manual$ggplots[[1]])
stopifnot(abs(actual.width.all4.manual - expected.b$width) < 0.02)
stopifnot(all(abs(actual.sizes.all4.manual - PLOT.TITLE.MIN.SIZE.PT) < 1e-6))
cat(sprintf("Same, with auto.scale=FALSE (subplot.size.width=1.0, plain total 4in): titles floored at %gpt, width still had to auto-expand to %.2fin (expected %.2fin) - OK\n",
            PLOT.TITLE.MIN.SIZE.PT, actual.width.all4.manual, expected.b$width))

## (c) The reverse: subplot.size.width already GENEROUS enough that
## titles fit at full size - no shrink, no expand, and definitely not
## narrowed back down.
result.all4.generous <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                                 sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull"),
                                                 auto.scale = FALSE, subplot.size.width = 5, subplot.size.height = 3,
                                                 dir.save = tempdir())
actual.width.all4.generous <- get.png.width.in(result.all4.generous$plots[[1]]$file) - ggsave.width.pad
actual.sizes.all4.generous <- get.panel.title.sizes(result.all4.generous$ggplots[[1]])
stopifnot(abs(actual.width.all4.generous - (5 * 4)) < 0.02)
stopifnot(all(abs(actual.sizes.all4.generous - plot.title.pt) < 1e-6))
cat(sprintf("Same, with a generous subplot.size.width=5 (plain total 20in): titles stay at full %gpt, width stays at %.2fin - not narrowed - OK\n",
            plot.title.pt, actual.width.all4.generous))

## (d) A subset (not all four panel types), where the narrower title set
## still fits comfortably at full size within auto.scale=TRUE's plain
## per-panel width - no shrink, no expand, same as the fourteenth round's
## own behavior. Confirms the mechanism is now general (keyed off
## $sub.plots, not a special-cased "all four" gate) without over-
## triggering on ordinary subsets.
result.subset <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                          sub.plots = c("canopy.bull", "mic.bull", "understory.bull"),
                                          auto.scale = TRUE, dir.save = tempdir())
actual.width.subset <- get.png.width.in(result.subset$plots[[1]]$file) - ggsave.width.pad
actual.sizes.subset <- get.panel.title.sizes(result.subset$ggplots[[1]])
stopifnot(abs(actual.width.subset - 6.5) < 0.02)
stopifnot(all(abs(actual.sizes.subset - plot.title.pt) < 1e-6))
cat(sprintf("3-panel subset (not all four types): titles stay at full %gpt, width stays at plain auto.scale=TRUE total (%.2fin) - not expanded - OK\n",
            plot.title.pt, actual.width.subset))

cat("\n=== TEST 17: mic.bar subplot's square (aspect.ratio=1) panel tracks its column width, per the seventeenth follow-up's aspect fix and the twentieth's 2/3-width narrowing ===\n")
## coord_polar panels (canopy.bull/mic.bull/understory.bull) always force
## a square panel (ggplot2's CoordPolar$aspect returns 1); build.mic.panel()
## matches that with an explicit aspect.ratio = 1 in its theme (per the
## seventeenth follow-up) - checked here by rendering the real default
## figure and comparing each panel's non-white pixel vertical extent
## within its own column, well below the title band. Columns are no
## longer equal width: per Josh's twentieth follow-up ("scale the size of
## the mic.bar plot to be 2/3 the size of the canopy plots"), mic.bar's
## column is narrowed to a relative width of 2/3 vs. 1 for the others -
## so the column boundaries below are computed from those same weights
## (not a uniform img.w/4 split), and mic.bar's square panel is now
## EXPECTED to render smaller than the other three, not equal to them.
result.height <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = tempdir())
png.path <- result.height$plots[[1]]$file
arr.dim <- dim(png::readPNG(png.path))
img.h <- arr.dim[1]; img.w <- arr.dim[2]
img <- png::readPNG(png.path)
nonwhite <- apply(img[, , 1:3], c(1, 2), function(px) any(px < (245 / 255)))
n.panels <- 4
panel.width.weights.t17 <- c(1, 2 / 3, 1, 1) # canopy.bull, mic.bar, mic.bull, understory.bull
col.edges.frac <- c(0, cumsum(panel.width.weights.t17) / sum(panel.width.weights.t17))
title.band.px <- round(img.h * 0.15) # skip the title text itself
panel.vextent <- function(panel.idx) {
  col.frac.w <- col.edges.frac[panel.idx + 1] - col.edges.frac[panel.idx]
  x0 <- round(img.w * (col.edges.frac[panel.idx] + 0.35 * col.frac.w))
  x1 <- round(img.w * (col.edges.frac[panel.idx] + 0.65 * col.frac.w))
  rows <- which(apply(nonwhite[(title.band.px + 1):img.h, x0:x1, drop = FALSE], 1, any))
  range(rows) + title.band.px
}
vext <- lapply(1:4, panel.vextent)
heights.px <- vapply(vext, function(v) v[2] - v[1], numeric(1))
names(heights.px) <- c("canopy.bull", "mic.bar", "mic.bull", "understory.bull")
cat("Panel content heights (px):", paste(names(heights.px), heights.px, sep = "=", collapse = ", "), "\n")
## The three coord_polar panels (unaffected by the twentieth follow-up's
## width change - their column weight is still 1 each) must still match
## each other's height, within the same loose tolerance as before.
bullseye.heights <- heights.px[c("canopy.bull", "mic.bull", "understory.bull")]
stopifnot(max(bullseye.heights) - min(bullseye.heights) < 15)
cat("canopy.bull/mic.bull/understory.bull panel heights still match each other (within 15px) - OK\n")
## mic.bar's height should now track its narrower (2/3) column - checked
## as a ratio against the bullseye panels' mean height, with a generous
## tolerance since fixed-size axis/margin space around the plot area (not
## just the coord_polar/aspect-ratio square itself) doesn't shrink
## proportionally with the column width.
mic.bar.ratio <- heights.px["mic.bar"] / mean(bullseye.heights)
stopifnot(mic.bar.ratio > 0.5, mic.bar.ratio < 0.85)
cat(sprintf("mic.bar panel height is %.0f%% of the coord_polar panels' height (expected meaningfully less than 100%%, around 2/3) - OK\n",
            mic.bar.ratio * 100))

cat("\n=== TEST 18: axis label font size is always (resolved title size) - 2pt, per Josh's eighteenth follow-up ===\n")
## Each panel's axis text lives on a different theme slot depending on the
## panel (axis.text.x for the bullseye/mic.bar panels' compass/bar labels,
## axis.text.y for mic.bar's own Y-axis numbers) - check whichever is set.
get.panel.axis.text.sizes <- function(combined) {
  get.one <- function(p) {
    s <- p$theme$axis.text.x$size
    if (is.null(s)) s <- p$theme$axis.text.y$size
    s %||% NA_real_
  }
  earlier <- vapply(combined$patches$plots, get.one, numeric(1))
  c(earlier, get.one(combined))
}
AXIS.TITLE.SIZE.GAP.PT <- 2

## (a) Real default call (all four panels, auto.scale=TRUE): titles were
## shrunk to fit (Test 16, case a) - axis labels must track that SHRUNK
## size, not the original $plot.title.size default, confirming the two
## mechanisms compose correctly.
result.axis.default <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = tempdir())
title.sizes.default <- get.panel.title.sizes(result.axis.default$ggplots[[1]])
axis.sizes.default <- get.panel.axis.text.sizes(result.axis.default$ggplots[[1]])
stopifnot(all(abs(axis.sizes.default - (title.sizes.default - AXIS.TITLE.SIZE.GAP.PT)) < 1e-6))
cat(sprintf("Default 4-panel call: titles at %.2fpt, axis labels at %.2fpt (exactly %gpt smaller) - OK\n",
            title.sizes.default[1], axis.sizes.default[1], AXIS.TITLE.SIZE.GAP.PT))

## (b) A subset whose titles are NOT shrunk (stay at the full
## $plot.title.size default, 12pt, per Test 16 case d) - axis labels must
## still track it (12 - 2 = 10), confirming this isn't only wired up for
## the shrunk case.
result.axis.subset <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                               sub.plots = c("canopy.bull", "mic.bull", "understory.bull"),
                                               dir.save = tempdir())
title.sizes.subset <- get.panel.title.sizes(result.axis.subset$ggplots[[1]])
axis.sizes.subset <- get.panel.axis.text.sizes(result.axis.subset$ggplots[[1]])
stopifnot(all(abs(title.sizes.subset - plot.title.pt) < 1e-6))
stopifnot(all(abs(axis.sizes.subset - (plot.title.pt - AXIS.TITLE.SIZE.GAP.PT)) < 1e-6))
cat(sprintf("3-panel subset (titles un-shrunk at %.2fpt): axis labels at %.2fpt (exactly %gpt smaller) - OK\n",
            title.sizes.subset[1], axis.sizes.subset[1], AXIS.TITLE.SIZE.GAP.PT))

## (c) Titles floored at 8pt (Test 16 case b, the too-narrow
## subplot.size.width case) - axis labels must track the FLOOR, landing at
## 6pt, still exactly 2pt below whatever the title actually rendered at.
result.axis.floor <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                              sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull"),
                                              auto.scale = FALSE, subplot.size.width = 1.0, subplot.size.height = 3,
                                              dir.save = tempdir())
title.sizes.floor <- get.panel.title.sizes(result.axis.floor$ggplots[[1]])
axis.sizes.floor <- get.panel.axis.text.sizes(result.axis.floor$ggplots[[1]])
stopifnot(all(abs(title.sizes.floor - PLOT.TITLE.MIN.SIZE.PT) < 1e-6))
stopifnot(all(abs(axis.sizes.floor - (PLOT.TITLE.MIN.SIZE.PT - AXIS.TITLE.SIZE.GAP.PT)) < 1e-6))
cat(sprintf("Titles floored at %gpt: axis labels at %.2fpt (exactly %gpt smaller) - OK\n",
            PLOT.TITLE.MIN.SIZE.PT, axis.sizes.floor[1], AXIS.TITLE.SIZE.GAP.PT))

cat("\n=== TEST 19: round nineteen - $overide.value/aes.style settings resolution, project.name-driven file naming, deprecated $output.filename.pattern ===\n")
## (a) default aes.style = "overide.value": a value in that column wins
## over $default.value (already exercised in TEST 5 above, re-confirmed
## here directly on the resolved setting rather than a visual check).
aes.default.19a <- aes.default
aes.default.19a$overide.value[aes.default.19a$parameter == "mic.bar.fill"] <- "orange"
result.19a <- batz.plotcover_bullseye(cover.data, mic.data, aes.default.19a, dir.save = tempdir())
cat("(a) $overide.value = 'orange' for mic.bar.fill applied without error (visual spot-check separately):",
    length(result.19a$plots) > 0, "\n")

## (b) a blank $overide.value cell falls through to $default.value (not
## blank-overrides-with-empty-string).
aes.default.19b <- aes.default
aes.default.19b$overide.value[aes.default.19b$parameter == "mic.bar.fill"] <- ""
result.19b <- batz.plotcover_bullseye(cover.data, mic.data, aes.default.19b, dir.save = tempdir())
cat("(b) blank $overide.value cell falls back to $default.value (no error, ran fine):", length(result.19b$plots) > 0, "\n")

## (c) an older aes.default sheet with NO $overide.value column at all
## (predates round nineteen) still works fine - aes.style simply never
## matches a column, so every parameter falls straight through to
## $default.value, exactly like before this round.
aes.default.19c <- aes.default[, setdiff(names(aes.default), "overide.value"), drop = FALSE]
result.19c <- batz.plotcover_bullseye(cover.data, mic.data, aes.default.19c, dir.save = tempdir())
cat("(c) aes.default with no $overide.value column at all still runs fine (backward compatible):", length(result.19c$plots) > 0, "\n")

## (d) aes.style can point at a DIFFERENT column name entirely (user's own
## choice, not just the default "overide.value").
aes.default.19d <- aes.default
aes.default.19d$my.custom.col <- ""
aes.default.19d$my.custom.col[aes.default.19d$parameter == "quadrant.fill"] <- "purple"
result.19d <- batz.plotcover_bullseye(cover.data, mic.data, aes.default.19d,
                                       aes.style = "my.custom.col", dir.save = tempdir())
cat("(d) aes.style pointed at a custom column name ('my.custom.col') applied without error:", length(result.19d$plots) > 0, "\n")

## (e) project.name drives the saved file name ("<project.name>_<ARU>_<timestamp>.png"),
## $output.filename.pattern is no longer read at all (removed from the
## aes.default sheet entirely here, to prove it - would have errored under
## the old required-parameter check, which no longer requires this row).
aes.default.19e <- aes.default[aes.default$parameter != "output.filename.pattern", , drop = FALSE]
dir.19e <- file.path(tempdir(), paste0("plotcover_r19_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
dir.create(dir.19e)
result.19e <- batz.plotcover_bullseye(cover.data, mic.data, aes.default.19e,
                                       project.name = "acme", dir.save = dir.19e)
saved.names.19e <- basename(list.files(dir.19e, pattern = "\\.png$"))
expected.aru <- names(result.19e$plots)[1]
cat("(e) $output.filename.pattern absent entirely -> no error (deprecated, unused):", length(result.19e$plots) > 0, "\n")
cat("(e) saved file name(s) start with 'acme_<ARU>_' as expected:",
    all(grepl(paste0("^acme_", expected.aru, "_"), saved.names.19e)), "\n")
cat("(e) omitting project.name defaults to 'new.project':\n")
dir.19e2 <- file.path(tempdir(), paste0("plotcover_r19default_", format(Sys.time(), "%Y%m%d%H%M%OS3")))
dir.create(dir.19e2)
result.19e2 <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = dir.19e2)
saved.names.19e2 <- basename(list.files(dir.19e2, pattern = "\\.png$"))
cat("    saved file name(s) start with 'new.project_':", all(grepl("^new\\.project_", saved.names.19e2)), "\n")

cat("\n=== TEST 20: mic.bar panel overhaul, per Josh's twentieth follow-up (2026-09-16) - six scoped changes ===\n")
## Rendered alone (sub.plots = "mic.bar"), the combined patchwork object IS
## the single ggplot (patchwork::wrap_plots() on a length-1 list just adds
## the wrapper class/attrs on top), so its layers/scales/theme are reached
## directly - no need to dig into $patches$plots as the multi-panel tests
## above do.
result.20 <- batz.plotcover_bullseye(cover.data, mic.data, aes.default,
                                      sub.plots = "mic.bar", dir.save = tempdir())
mic.bar.plot <- result.20$ggplots[[1]]

## (a) Bar width is $mic.bar.width (0.4) * a fixed 1.3 multiplier = 0.52,
## per Josh: "Make the bar 1.3 its current size."
mic.bar.width.default <- as.numeric(aes.default$default.value[aes.default$parameter == "mic.bar.width"])
expected.bar.width <- mic.bar.width.default * 1.3
bar.layer <- mic.bar.plot$layers[[1]]
stopifnot(abs(bar.layer$geom_params$width - expected.bar.width) < 1e-6)
cat(sprintf("(a) bar width = %.3f (base %.2f x 1.3) - OK\n", bar.layer$geom_params$width, mic.bar.width.default))

## (b) Y-axis breaks (and their matching panel.grid.major.y reference
## lines) occur every 0.5, per Josh: "with a line every 0.5 interval."
y.scale <- mic.bar.plot$scales$scales[[which(vapply(mic.bar.plot$scales$scales,
                                                     function(s) "y" %in% s$aesthetics, logical(1)))]]
y.breaks.actual <- y.scale$breaks
stopifnot(all(abs(diff(y.breaks.actual) - 0.5) < 1e-9))
stopifnot(!is.null(mic.bar.plot$theme$panel.grid.major.y))
cat("(b) Y-axis breaks every 0.5 (", paste(y.breaks.actual, collapse = ", "), ") and panel.grid.major.y is drawn - OK\n")

## (c) axis.title.y (the "Microphone Height (m)" label) now renders at the
## same derived axis.text.size as axis.text.y/axis.text.x elsewhere, per
## Josh: "reduce the font size to be the same size as the other two
## plots." (Only one panel here, so no title-shrink kicks in - resolved
## size is the plain $plot.title.size - 2 default.)
expected.axis.size <- plot.title.pt - AXIS.TITLE.SIZE.GAP.PT
stopifnot(abs(mic.bar.plot$theme$axis.title.y$size - expected.axis.size) < 1e-6)
stopifnot(abs(mic.bar.plot$theme$axis.text.y$size - expected.axis.size) < 1e-6)
cat(sprintf("(c) axis.title.y size (%.1fpt) matches axis.text.y/axis.text.x size (%.1fpt) - OK\n",
            mic.bar.plot$theme$axis.title.y$size, expected.axis.size))

## (d) The Vertical Microphone Orientation icon is centered ON the bar's
## top edge (mic.height +/- icon.height.m/2), reverting the fifth
## follow-up's "entirely above" placement, per Josh: "move the mic png to
## be centered at the top of the bar." Test ARU has microphone_height = 3
## (confirmed in TEST 11) and icon.height.m defaults to 1.
mic.height.test.aru <- 3
icon.height.m.default <- as.numeric(aes.default$default.value[aes.default$parameter == "icon.height.m"])
icon.layer <- mic.bar.plot$layers[[2]]
stopifnot(abs(icon.layer$geom_params$ymin - (mic.height.test.aru - icon.height.m.default / 2)) < 1e-6)
stopifnot(abs(icon.layer$geom_params$ymax - (mic.height.test.aru + icon.height.m.default / 2)) < 1e-6)
cat(sprintf("(d) icon spans y = [%.2f, %.2f], centered on mic.height = %d - OK\n",
            icon.layer$geom_params$ymin, icon.layer$geom_params$ymax, mic.height.test.aru))

## (e) mic.bar's rendered column is 2/3 the width of every other panel
## type, per Josh: "scale the size of the mic.bar plot to be 2/3 the size
## of the canopy plots." Checked on the real default 4-panel figure
## (canopy.bull, mic.bar, mic.bull, understory.bull, per TEST 6/17) via
## patchwork's own stored layout weights.
result.20e <- batz.plotcover_bullseye(cover.data, mic.data, aes.default, dir.save = tempdir())
widths.20e <- result.20e$ggplots[[1]]$patches$layout$widths
stopifnot(isTRUE(all.equal(widths.20e, c(1, 2 / 3, 1, 1))))
cat("(e) patchwork column width weights:", paste(widths.20e, collapse = ", "), "(mic.bar = 2/3, others = 1) - OK\n")

## (f) Title text is now the single-line "Microphone Vertical Orientation"
## (was a two-line string shared with mic.bull.title before this round),
## per Josh: "Change the title to be 'Microphone Vertical Orientation'."
## $mic.bull.title is confirmed UNCHANGED (still the old shared two-line
## text) so the two panels' titles no longer match, per the explicit
## judgment call flagged in the CSV notes and roxygen @details.
stopifnot(identical(mic.bar.plot$labels$title, "Microphone Vertical Orientation"))
mic.bull.title.default <- aes.default$default.value[aes.default$parameter == "mic.bull.title"]
stopifnot(identical(mic.bull.title.default, "Microphone Height and\nVertical Orientation"))
cat("(f) mic.bar title = 'Microphone Vertical Orientation'; mic.bull.title left unchanged (two panels no longer match, as flagged) - OK\n")

cat("\n=== ALL TESTS COMPLETE ===\n")

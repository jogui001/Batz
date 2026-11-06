#' Plot habitat-cover bullseye diagrams for each ARU
#'
#' Draws one figure per ARU: a row of panels showing canopy and understory
#' cover by quadrant as polar "bullseye" plots, plus the microphone height and
#' vertical orientation. By default the panels are, left to right, canopy
#' cover, microphone height bar, microphone orientation bullseye and
#' understory cover (see \code{sub.plots}). Each figure is saved as a PNG.
#'
#' @param data Data frame of the habitat-cover assessment, one row per
#'   quadrant per ARU. Headers are standardized on receipt, so raw spreadsheet
#'   headers work. Required columns (any spelling that standardizes to these):
#'   \code{"ARU Label"} (\code{$aru.label}), \code{"Select the quadrant you are
#'   assessing"}, \code{"Canopy cover"}, \code{"Understory cover"}. Every ARU
#'   to be plotted needs the four quadrants \code{"northeast"},
#'   \code{"southeast"}, \code{"southwest"} and \code{"northwest"} (case and
#'   spaces ignored).
#' @param mic Data frame of microphone deployment data, one row per ARU (the
#'   first row is used if there are more). Headers are standardized on
#'   receipt. Required columns: \code{"ARU Label"}, \code{"Microphone Height"}
#'   (m), \code{"Horizontal Microphone Orientation"} (compass bearing, degrees)
#'   and \code{"Vertical Microphone Orientation"} (degrees: 0 = level,
#'   90 = straight up, 270 = straight down).
#' @param aes.default Data frame of plot settings, one row per parameter
#'   (e.g. \code{plotopts_bullseye.csv}, see
#'   \code{\link{batz.generate_plotopts}}). Needs \code{$category},
#'   \code{$parameter} and \code{$default.value}; an \code{$override.value}
#'   column and \code{$notes} are optional. Its headers are not standardized.
#'   See Details for the parameters read.
#' @param project.name Character, default \code{"new.project"}. First part of
#'   each saved file name.
#' @param aes.style Character, default \code{"override.value"}. Column of
#'   \code{aes.default} checked first for each parameter; \code{$default.value}
#'   is used when that column is missing or blank for the row. An old
#'   \code{$overide.value} column is still read when \code{aes.style =
#'   "override.value"} and that column is absent.
#' @param dir.save Character, default \code{getwd()}. Folder the PNG files are
#'   saved in (it must exist).
#' @param sub.plots Character vector, default \code{c("canopy.bull", "mic.bar",
#'   "mic.bull", "understory.bull")}. Which panels to draw, left to right.
#'   Allowed values: \code{"canopy.bull"} (canopy cover bullseye),
#'   \code{"mic.bar"} (microphone height bar with orientation icon),
#'   \code{"mic.bull"} (microphone orientation bullseye) and
#'   \code{"understory.bull"} (understory cover bullseye). Values may repeat;
#'   any other value stops with an error.
#' @param cover.arrows Logical, default \code{TRUE}. Draw the horizontal
#'   microphone orientation arrow on the canopy/understory bullseyes (see
#'   \code{arrow.panel}). Does not affect \code{"mic.bar"} or \code{"mic.bull"}.
#' @param arrow.panel Character, default \code{"first"}. With
#'   \code{cover.arrows = TRUE}: \code{"first"} draws the arrow on the first
#'   canopy/understory bullseye in \code{sub.plots} only; \code{"all"} draws it
#'   on every one.
#' @param panel.labels Character vector or \code{NULL} (default). One label
#'   per \code{sub.plots} entry (e.g. \code{c("(a)", "(b)", "(c)", "(d)")}),
#'   drawn at the bottom right of each panel. \code{NULL} draws none.
#' @param title.size.adjust Number, default \code{0}. Points added to the panel
#'   title size after automatic sizing (e.g. \code{-2}); the result is at
#'   least 4 pt. Axis text and \code{panel.labels} are not changed.
#' @param mean.cover Logical, default \code{TRUE}. Draw the dashed circle at the
#'   mean of the four quadrants on the canopy/understory bullseyes.
#' @param quadrant.cover Logical, default \code{TRUE}. Draw the four quadrant
#'   wedges on the canopy/understory bullseyes.
#' @param auto.scale Logical, default \code{TRUE}. \code{TRUE}: figure width is
#'   6.5 in and height is \code{$plot.height}. \code{FALSE}: each panel is
#'   \code{subplot.size.width} wide and the figure is
#'   \code{subplot.size.height} high.
#' @param subplot.size.height Positive number, default \code{3}. Figure height
#'   in inches when \code{auto.scale = FALSE}; otherwise ignored.
#' @param subplot.size.width Positive number, default \code{2}. Width of each
#'   panel in inches when \code{auto.scale = FALSE} (figure width =
#'   \code{subplot.size.width * length(sub.plots)}); otherwise ignored.
#' @param strip.special Logical, default \code{TRUE}. Simplify accented
#'   letters and symbols (\code{é} to \code{e}, \code{×} to \code{X}) and
#'   remove other non-ASCII characters (\code{°}, \code{µ}, ...) in \code{data}
#'   and \code{mic}, headers included, before anything else.
#' @param strip.special.plotopts Logical, default \code{FALSE}. Same for
#'   \code{aes.default}. With \code{FALSE}, \code{aes.default} is left as is (so
#'   labels keep symbols such as \code{°}), but the function stops with an
#'   error naming the rows if it holds characters that cannot be read in this
#'   R session.
#'
#' @details
#' \strong{Steps}
#' \enumerate{
#'   \item \code{strip.special}/\code{strip.special.plotopts} are applied.
#'   \item Headers of \code{data} and \code{mic} are standardized to snake_case
#'     and \code{aru_label}, \code{canopy_cover}, \code{microphone_height},
#'     \code{horizontal_microphone_orientation} and
#'     \code{vertical_microphone_orientation} are renamed to \code{aru.label},
#'     \code{canopy.cover}, \code{microphone.height},
#'     \code{microphone.orientation_horizontal} and
#'     \code{microphone.orientation_vertical}.
#'   \item The arguments are checked (each bad value stops with an error).
#'     Then required headers, required \code{aes.default} parameter rows and
#'     duplicate column names are checked, and all problems found are reported
#'     together in one error.
#'   \item Cover values are converted to numbers: a range such as
#'     \code{"51_75"} or \code{"25-50"} becomes its midpoint (63, 37.5); a
#'     single number is used as is.
#'   \item For each distinct \code{$aru.label} in \code{mic}, the panels are
#'     built, combined side by side with \code{patchwork::wrap_plots()} and
#'     saved. An ARU missing any of the four quadrants in \code{data} is
#'     skipped with a warning.
#' }
#'
#' \strong{Settings.} Each parameter is looked up in \code{aes.default$parameter}
#' (case-insensitive); the value in the \code{aes.style} column is used when
#' present and not blank, otherwise \code{$default.value}. Required parameters:
#' \describe{
#'   \item{Cover bullseyes}{\code{radial.max} (outer edge of the plot),
#'     \code{radial.breaks} (scale rings, separated by \code{;}),
#'     \code{quadrant.fill}, \code{quadrant.color} (wedge outline; \code{"NA"}
#'     or blank = none), \code{mean.color}, \code{mean.linewidth},
#'     \code{canopy.title}, \code{understory.title}.}
#'   \item{Arrows}{\code{arrow.color}, \code{arrow.linewidth},
#'     \code{arrow.head.cm} (used by the cover bullseyes and \code{"mic.bull"}).}
#'   \item{Scale lines and grid}{\code{scale.line.color},
#'     \code{scale.line.linetype}, \code{scale.line.linewidth},
#'     \code{panel.grid.color}.}
#'   \item{mic.bull panel}{\code{mic.bull.title}, \code{mic.bull.radial.max},
#'     \code{mic.bull.radial.breaks}, \code{mic.bull.skyward.label},
#'     \code{mic.bull.earthward.label}, \code{mic.bull.height.label}.}
#'   \item{mic.bar panel}{\code{mic.panel.title}, \code{mic.panel.ylab},
#'     \code{mic.bar.fill}, \code{mic.bar.width}, \code{mic.panel.y.default.max},
#'     \code{icon.dir}, \code{icon.height.m}.}
#'   \item{Text and saving}{\code{plot.title.size}, \code{plot.height},
#'     \code{ggsave.dpi}, \code{ggsave.units}, \code{ggsave.width.pad},
#'     \code{ggsave.height.pad}.}
#' }
#' Other rows (e.g. \code{plot.width}, \code{axis.text.size},
#' \code{mic.panel.xlab}, \code{output.filename.pattern}) are ignored. A title
#' can contain a line break to draw it on two lines.
#'
#' \strong{Canopy and understory bullseyes} (\code{"canopy.bull"},
#' \code{"understory.bull"}). Polar plot with 0 = North at the top and angles
#' increasing clockwise (N/E/S/W marked). Layers, bottom to top:
#' \itemize{
#'   \item quadrant wedges (\code{quadrant.cover}): each quadrant is a 90-degree
#'     wedge centered on 45, 135, 225 or 315 degrees, with radius = its cover
#'     value;
#'   \item dashed circle at the mean of the four quadrants (\code{mean.cover});
#'   \item arrow from the center to \code{$radial.max} at the horizontal
#'     microphone orientation (\code{cover.arrows}, \code{arrow.panel});
#'   \item scale rings at \code{$radial.breaks};
#'   \item the ring values, written at both the North and South ends of the
#'     vertical axis.
#' }
#' The radial axis runs from 0 to \code{$radial.max} with no padding.
#'
#' \strong{Microphone orientation bullseye} (\code{"mic.bull"}). Polar plot of
#' the vertical microphone orientation: 90 (straight up) at the top, labelled
#' \code{$mic.bull.skyward.label}; 270 (straight down) at the bottom, labelled
#' \code{$mic.bull.earthward.label}; 0 at the right (no label); 180 at the
#' left, labelled \code{$mic.bull.height.label}. One arrow is always drawn at
#' the orientation angle with length = microphone height. Rings are at
#' \code{$mic.bull.radial.breaks}, with their values on a white background
#' along the left axis. The plot extends to \code{$mic.bull.radial.max}, or to
#' microphone height + 0.2 if larger, plus 0.7 of room for the side label.
#'
#' \strong{Microphone height bar} (\code{"mic.bar"}). A bar from 0 to the
#' microphone height, \code{1.3 * $mic.bar.width} wide, with grid lines every
#' 0.5 and no x-axis title. The y axis runs from 0 to
#' \code{$mic.panel.y.default.max}, or higher when the bar plus half the icon
#' needs more room. An icon of the vertical orientation,
#' \code{$icon.height.m} tall and undistorted, is centered on the top of the
#' bar. Icons are the files \verb{mic_vert_000.png}, \verb{mic_vert_045.png},
#' \verb{mic_vert_090.png}, \verb{mic_vert_270.png} and
#' \verb{mic_vert_315.png} in \code{$icon.dir} (relative to the working
#' directory); the orientation is matched to the nearest of these angles
#' (e.g. 350 uses 0). If the file is missing, a warning is given and the bar is
#' drawn without an icon. The panel is square, and its column is 2/3 as wide
#' as the other panels.
#'
#' \strong{Figure size and text.} Panel titles start at
#' \code{$plot.title.size}. If the widest title in \code{sub.plots} does not
#' fit an equal share of the figure width (less 0.25 in), all titles are
#' shrunk to the same size, down to 8 pt; if they still do not fit, the figure
#' is widened (never narrowed). \code{title.size.adjust} is then added to the
#' titles. Axis text and ring values are 2 pt smaller than the fitted title
#' size; \code{panel.labels} use the fitted title size.
#' \code{ggsave()} uses width + \code{$ggsave.width.pad}, height +
#' \code{$ggsave.height.pad}, \code{$ggsave.units}, \code{$ggsave.dpi} and a
#' white background.
#'
#' \strong{Files.} One PNG per ARU in \code{dir.save}, named
#' \verb{<project.name>_<aru.label>_<YYYYMMDDHHMMSS>.png}; each file name is
#' printed as \code{Saved: ...}.
#'
#' @return Invisibly, a list with \code{$plots} (one entry per ARU plotted,
#'   named by \code{aru.label}: \code{$canopy} and \code{$understory} quadrant
#'   values, the \code{$mic} row and the saved \code{$file} path) and
#'   \code{$ggplots} (the combined \code{patchwork} figure for each ARU).
#'
#' @seealso \code{\link{batz.generate_plotopts}} (settings file),
#'   \code{\link{batz.generate_aru.appendix}} (uses this function for its
#'   habitat-cover figure).
#'
#' @export
#' @examples
#' \dontrun{
#' aes.default <- read.csv("plotopts_bullseye.csv", stringsAsFactors = FALSE)
#' cover <- read.csv("cover.csv", stringsAsFactors = FALSE, check.names = FALSE)
#' mic <- read.csv("mic.csv", stringsAsFactors = FALSE, check.names = FALSE)
#' batz.plotcover_bullseye(cover, mic, aes.default, project.name = "site1",
#'                         dir.save = getwd())
#'
#' # canopy and understory only, arrow on both, panel letters
#' batz.plotcover_bullseye(cover, mic, aes.default,
#'                         sub.plots = c("canopy.bull", "understory.bull"),
#'                         arrow.panel = "all", panel.labels = c("(a)", "(b)"))
#' }
batz.plotcover_bullseye <- function(data, mic, aes.default, project.name = "new.project",
                                     aes.style = "override.value",
                                     dir.save = getwd(),
                                     sub.plots = c("canopy.bull", "mic.bar",
                                                   "mic.bull", "understory.bull"),
                                     cover.arrows = TRUE, arrow.panel = "first",
                                     panel.labels = NULL, title.size.adjust = 0,
                                     mean.cover = TRUE,
                                     quadrant.cover = TRUE,
                                     auto.scale = TRUE,
                                     subplot.size.height = 3,
                                     subplot.size.width = 2,
                                     strip.special = TRUE,
                                     strip.special.plotopts = FALSE) {

  ## strip.special (2026-09-30, per Josh - see @details): remove non-ASCII
  ## special characters from every data-frame input before anything else
  ## (header standardization/checks included). special.strip() returns
  ## non-data-frames unchanged and only modifies these local copies.
  objname.aes.default <- paste(deparse(substitute(aes.default)), collapse = "")
  data <- special.strip(data, strip.special)$df
  mic <- special.strip(mic, strip.special)$df
  ## aes.default is a settings table, not data - stripped only when
  ## strip.special.plotopts = TRUE (2026-10-02, per Josh); otherwise checked
  ## for characters that can't be read, with a readable error.
  if (isTRUE(strip.special.plotopts)) {
    aes.default <- special.strip(aes.default, TRUE)$df
  } else {
    special.check.plotopts(aes.default, "aes.default", objname.aes.default, "batz.plotcover_bullseye")
  }

  names(data) <- standardize.headers(names(data))
  names(mic) <- standardize.headers(names(mic))

  ## Legacy input alias (2026-09-28, later the same day, see @details,
  ## "Column identifier renamed, 2026-09-28"): the real raw spreadsheet
  ## header ("Canopy cover") always standardizes to "canopy_cover"
  ## (underscore) - standardize.headers() can never produce a dot - but
  ## this function's own catalog identifier is now "canopy.cover" per
  ## Josh's reference-workbook Change.to. Renamed here, in this function's
  ## own working copy, before the required-header check below runs - same
  ## idiom already used in batz.merge_vetted.acoustics() for its own
  ## monitoringnight -> date.monitoringnight / serial -> aru.serial aliases.
  if ("canopy_cover" %in% names(data) && !("canopy.cover" %in% names(data))) {
    names(data)[names(data) == "canopy_cover"] <- "canopy.cover"
  }

  ## Bugfix (2026-09-28, later still - see @details, "Bugfix: missing
  ## legacy-input aliases for the 2026-09-27 rename"): the 2026-09-27
  ## round renamed DATA.REQUIRED's/MIC.REQUIRED's aru_label ->
  ## aru.label and MIC.REQUIRED's horizontal_microphone_orientation/
  ## microphone_height/vertical_microphone_orientation to their dot-spelled
  ## forms, but never added the accompanying legacy-input-alias rename
  ## that canopy_cover -> canopy.cover (just above) DOES have - so those
  ## four identifiers could never actually be satisfied by any real raw
  ## header, since standardize.headers() can only ever produce underscores.
  ## Added now, same idiom as the canopy_cover alias above.
  if ("aru_label" %in% names(data) && !("aru.label" %in% names(data))) {
    names(data)[names(data) == "aru_label"] <- "aru.label"
  }
  if ("aru_label" %in% names(mic) && !("aru.label" %in% names(mic))) {
    names(mic)[names(mic) == "aru_label"] <- "aru.label"
  }
  if ("microphone_height" %in% names(mic) && !("microphone.height" %in% names(mic))) {
    names(mic)[names(mic) == "microphone_height"] <- "microphone.height"
  }
  if ("horizontal_microphone_orientation" %in% names(mic) &&
      !("microphone.orientation_horizontal" %in% names(mic))) {
    names(mic)[names(mic) == "horizontal_microphone_orientation"] <- "microphone.orientation_horizontal"
  }
  if ("vertical_microphone_orientation" %in% names(mic) &&
      !("microphone.orientation_vertical" %in% names(mic))) {
    names(mic)[names(mic) == "vertical_microphone_orientation"] <- "microphone.orientation_vertical"
  }

  DATA.REQUIRED <- c("aru.label", "select_the_quadrant_you_are_assessing",
                      "canopy.cover", "understory_cover")
  MIC.REQUIRED <- c("aru.label", "microphone.height",
                     "microphone.orientation_horizontal",
                     "microphone.orientation_vertical")
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
    ## NOTE: "output.filename.pattern" deliberately removed from this
    ## required list per Josh's nineteenth follow-up (2026-09-16) - the
    ## saved file name is now always "<project.name>_<ARU>_<timestamp>.png"
    ## (see @details, "Settings resolution (round nineteen)"); this setting
    ## is no longer read at all. An $output.filename.pattern row left in an
    ## existing aes.default sheet is harmless (simply ignored).
    ## NOTE: "mic.panel.xlab" also deliberately removed from this required
    ## list per Josh's sixteenth follow-up (2026-09-15) - no longer read by
    ## this function at all (the mic.bar panel's X-axis title is now
    ## always blank); harmless if an older sheet still has the row.
    ## NOTE: "plot.width" deliberately removed from this required list per
    ## Josh's fourteenth follow-up (2026-09-15) - it is no longer read by
    ## this function at all (see @details, "Figure sizing"): auto.scale =
    ## TRUE now hardcodes a fixed 6.5in total width, and auto.scale =
    ## FALSE computes total width from subplot.size.width instead. A
    ## $plot.width row left in an existing aes.default sheet is harmless
    ## (simply ignored), so this is a required-check relaxation only, not
    ## a breaking change for anyone with an older settings CSV.
    ## NOTE: "axis.text.size" deliberately removed from this required list
    ## per Josh's eighteenth follow-up (2026-09-16: "reduce the size of the
    ## axis labels to be 2 pts smaller than the current title labels at
    ## the top of each subplot") - it is no longer read by this function at
    ## all. Axis label font size is now always DERIVED, per ARU, as
    ## (whatever $plot.title.size resolves to that render, after the
    ## seventeenth follow-up's auto-shrink - see @details, "Figure sizing")
    ## minus 2pt, rather than an independent fixed setting - see @details,
    ## "Axis label sizing". An $axis.text.size row left in an existing
    ## aes.default sheet is harmless (simply ignored).
  )

  ## Which panels to draw and in what order, per Josh (2026-09-15):
  ## sub.plots = c("canopy.bull", "mic.bar", "mic.bull", "understory.bull").
  ## "mic.bull" is its own polar bullseye-style plot of the Vertical
  ## Microphone Orientation (see build.mic.bullseye() below and this
  ## function's own @details, "mic.bull panel") - it originally shipped as
  ## a placeholder alias of "understory.bull" in an earlier round, before
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
  if (!(is.character(arrow.panel) && length(arrow.panel) == 1 && arrow.panel %in% c("first", "all"))) {
    stop('arrow.panel must be "first" or "all"')
  }
  if (!(is.numeric(title.size.adjust) && length(title.size.adjust) == 1 && is.finite(title.size.adjust))) {
    stop("title.size.adjust must be a single number (points)")
  }
  if (!is.null(panel.labels) && length(panel.labels) != length(sub.plots)) {
    stop("panel.labels must have one label per sub.plots entry (", length(sub.plots), ")")
  }
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
    missing <- required[!plotutil_param_key(required) %in% plotutil_param_key(df$parameter)]
    if (length(missing) > 0) {
      return(sprintf("%s is missing these required $parameter rows: %s - it may be an older copy missing settings added since it was last saved",
                      label, paste(missing, collapse = ", ")))
    }
    NULL
  }
  check.duplicates <- function(df, label) {
    nm <- names(df)
    dups <- unique(nm[duplicated(nm)])
    if (length(dups) > 0) {
      return(sprintf("%s has duplicate column name(s): %s - every column name must be unique",
                      label, paste(dups, collapse = ", ")))
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
  if (length(problems) > 0) stop(paste(problems, collapse = "\n\n"))

  ## Settings resolution (round nineteen, per Josh's 2026-09-16 follow-up):
  ## $aes.style names a FIXED column to check first (default
  ## "override.value" - a blank column the user fills in directly on their
  ## own copy of the CSV), falling back to $default.value when that column
  ## doesn't exist or is blank for this row. This replaces the old
  ## project.name-matches-a-column-name mechanism entirely - project.name
  ## no longer has any role in settings resolution, only in the saved file
  ## name (see the main loop below).
  get.default <- function(param) {
    row.idx <- which(plotutil_param_key(aes.default$parameter) == plotutil_param_key(param))
    if (length(row.idx) == 0) return(NA_character_)
    val <- as.character(aes.default$default.value[row.idx[1]])
    if (!is.na(aes.col <- plotutil_aes_style_col(aes.default, aes.style))) {
      override <- aes.default[[aes.col]][row.idx[1]]
      if (!is.na(override) && nzchar(trimws(as.character(override)))) {
        val <- as.character(override)
      }
    }
    val
  }

  ## "Take the midpoint for each category, e.g. 25-50 would be 37.5, if
  ## there is a single number use that number." Real data uses underscore-
  ## separated ranges (e.g. "51_75"); Josh's own written example used a
  ## hyphen ("25-50") - both separators are supported.
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
  ## microphone.orientation_vertical value is matched to the NEAREST of
  ## these five (circular distance, so e.g. 350 is closer to 0 than to
  ## 270) rather than requiring an exact match, so the mic panel always
  ## draws an icon - **assumption, please confirm with Josh**, and let him
  ## know if/when icons for other angles (e.g. 135, 180, 225) are added.
  ICON.ANGLES <- c(0, 45, 90, 270, 315)
  nearest.icon.angle <- function(vert.orient) {
    dist <- pmin(abs(vert.orient - ICON.ANGLES), 360 - abs(vert.orient - ICON.ANGLES))
    ICON.ANGLES[which.min(dist)]
  }

  data$quadrant.std <- tolower(trimws(data$select_the_quadrant_you_are_assessing))
  data$canopy.value <- vapply(data$canopy.cover, parse.cover.value, numeric(1))
  data$understory.value <- vapply(data$understory_cover, parse.cover.value, numeric(1))

  build.panel <- function(quad.values, panel.title, horiz.orient, title.size.override = NULL, axis.text.size.override = NULL,
                          draw.arrow = TRUE) {
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
    ## Per Josh's seventeenth follow-up (2026-09-16), the caller (the main
    ## per-ARU loop below) may pass an already-shrunk title size when the
    ## full-size titles wouldn't fit the panel width - see @details,
    ## "Figure sizing". NULL (e.g. when this builder is called directly,
    ## as the tests do) falls back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up (2026-09-16), the caller (the main
    ## per-ARU loop below) always passes axis text 2pt smaller than
    ## whatever the resolved title size is that render - see @details,
    ## "Axis label sizing". NULL (e.g. when this builder is called
    ## directly, as some tests do) falls back to $axis.text.size if that
    ## row is still present in aes.default (harmless if it is - see the
    ## required-parameters note above), same fallback pattern as
    ## title.size.override.
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override
    panel.grid.color <- get.default("panel.grid.color")
    scale.line.color <- get.default("scale.line.color")
    scale.line.linetype <- get.default("scale.line.linetype")
    scale.line.linewidth <- as.numeric(get.default("scale.line.linewidth"))

    ## Radial (vertical/N-S) axis labels are drawn manually at x = 0/180
    ## (the North AND South ends of the vertical line, per Josh: "Repeat
    ## the labels ... on the southern axis as well") instead of relying on
    ## ggplot2's default y-axis text (which coord_polar renders off to one
    ## side, not along the vertical axis) - see this function's own
    ## @details, "Radial scale labels". Plain geom_text (no background
    ## box, per Josh: "Remove the white box around the labels") - added as
    ## the LAST layer, per Josh ("Plot the labels last on the graph" /
    ## draw order "... > labels"), so it draws on top of every other layer.
    radial.label.size <- axis.text.size / ggplot2::.pt
    radial.label.df <- data.frame(x = rep(c(0, 180), each = length(radial.breaks)),
                                   y = rep(radial.breaks, times = 2),
                                   label = rep(radial.breaks, times = 2))

    ## Draw order per Josh: "Quadrants > mean cover > mic direction > scale
    ## lines > labels". Per Josh's sixth follow-up (2026-09-15), each of
    ## the first three layers is now individually optional
    ## ($quadrant.cover/$mean.cover/$cover.arrows, this function's own
    ## top-level parameters, captured here by lexical scoping) - when
    ## turned off, that layer is simply skipped, without disturbing the
    ## draw order of whichever layers remain on. Scale lines and labels
    ## are NOT gated by any of these three switches - they are always
    ## drawn, the same as before.
    p <- ggplot2::ggplot(quad.df, ggplot2::aes(x = x, y = value))
    if (quadrant.cover) {
      p <- p + ggplot2::geom_col(width = 90, fill = quadrant.fill, color = quadrant.color)
    }
    if (mean.cover) {
      p <- p + ggplot2::geom_hline(yintercept = mean.val, color = mean.color,
                                    linewidth = mean.linewidth, linetype = "dashed")
    }
    if (cover.arrows && draw.arrow) {
      p <- p + ggplot2::geom_segment(ggplot2::aes(x = horiz.orient, y = 0, xend = horiz.orient, yend = radial.max),
                                      color = arrow.color, linewidth = arrow.linewidth,
                                      arrow = grid::arrow(length = grid::unit(arrow.head.cm, "cm"), type = "closed"),
                                      inherit.aes = FALSE)
    }
    p +
      ## Scale lines (the 25/50/75/100 reference rings), per Josh's
      ## draw-order request. These used to be drawn via theme(panel.grid.
      ## major) - but ggplot2 always renders theme gridlines UNDERNEATH all
      ## geom/data layers, regardless of where the theme() call sits in the
      ## `+` chain, so satisfying "scale lines after mic direction, before
      ## labels" is only possible by drawing them as an explicit geom layer
      ## instead. Reuses the same "geom_hline inside coord_polar draws a
      ## full circle" trick as the mean-cover circle above, one line per
      ## $radial.breaks value, styled black/dashed per Josh ("make the
      ## scale lines black and dashed"). panel.grid.major.y is blanked
      ## below to avoid a duplicate grey ring underneath this one;
      ## panel.grid.major.x (the N-S/E-W compass cross lines, not part of
      ## this draw-order request) is left alone.
      ggplot2::geom_hline(yintercept = radial.breaks, color = scale.line.color,
                           linewidth = scale.line.linewidth, linetype = scale.line.linetype) +
      ggplot2::scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270),
                                   labels = c("N", "E", "S", "W"), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, radial.max), breaks = radial.breaks,
                                   expand = c(0, 0)) +
      ggplot2::coord_polar(theta = "x", start = 0, direction = 1) +
      ggplot2::labs(title = panel.title, x = NULL, y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = plot.title.size, hjust = 0.5),
                      axis.text.x = ggplot2::element_text(size = axis.text.size),
                      axis.text.y = ggplot2::element_blank(),
                      axis.ticks = ggplot2::element_blank(),
                      panel.grid.major.x = ggplot2::element_line(color = panel.grid.color),
                      panel.grid.major.y = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
      ## Labels last (per Josh: "Plot the labels last on the graph" / draw
      ## order "... > labels") so they render on top of every other layer.
      ggplot2::geom_text(data = radial.label.df, ggplot2::aes(x = x, y = y, label = label),
                          inherit.aes = FALSE, size = radial.label.size)
  }

  ## "mic.bull" panel - a polar "bullseye"-style plot of the Vertical
  ## Microphone Orientation, per Josh's seventh follow-up (2026-09-15),
  ## replacing the placeholder that aliased "understory.bull" in the
  ## previous round. Uses the SAME 0-360 angle convention already
  ## established for vert.orient (0 = parallel to ground/"level", 90 =
  ## straight up, 270 = straight down - see "Vertical Microphone
  ## Orientation icon set" above) for the arrow's data angle. Per Josh's
  ## ninth follow-up (2026-09-15, later the same day: "E = 0, and W = 180,
  ## N = 90, S = 270" plus confirming, via a direct choice, "E=0(top)/
  ## N=90(right)/W=180(bottom)/S=270(left)"), this panel's coord_polar() is
  ## back to start = 0/direction = 1 - the SAME screen mapping used by
  ## every other panel in this function (0 = top, 90 = right, 180 =
  ## bottom, 270 = left, clockwise) - reverting the eighth follow-up's
  ## quarter-turn rotation from the eighth follow-up, and per Josh's tenth
  ## follow-up (2026-09-15, later still: "Top = 90, right 0, Bottom = 270,
  ## left = 180"), this panel's own coord_polar() is now
  ## \code{start = -pi / 2, direction = -1} - unlike every other panel's
  ## \code{start = 0, direction = 1} - which is the mapping that actually
  ## produces that exact screen layout (verified with a live rendered
  ## test, not derived by inspection alone - see the .dev.R/final .R
  ## comments for the check). Angle 90 ("straight up") renders at the TOP
  ## and carries \code{$mic.bull.skyward.label} (default \code{"Skyward"});
  ## angle 270 ("straight down") renders at the BOTTOM and carries
  ## \code{$mic.bull.earthward.label} (\code{"Earthward"}), per Josh's
  ## tenth follow-up ("Earthward label at bottom / Skyward label at top").
  ## Angle 0 (E) renders at the RIGHT and stays blank, per Josh's original
  ## "No label for E or W"; angle 180 (W) renders at the LEFT and carries
  ## \code{$mic.bull.height.label} (default \code{"(m)"}, shortened from
  ## "Microphone Height (m)" via "Height (m)" over the eleventh and
  ## twelfth follow-ups), per the tenth follow-up ("Microphone Height (m)
  ## on left"). A single
  ## arrow shows the Vertical Microphone Orientation itself - angle =
  ## vert.orient, length = mic.height ("the tip of the arrow ending at the
  ## total height of the mic", per Josh) - and is always drawn: this panel
  ## has no quadrant/mean-circle concept, so quadrant.cover/mean.cover
  ## don't apply to it, and cover.arrows (which Josh's sixth follow-up
  ## scoped to "canopy.bull, and/or understory.bull" only) doesn't gate it
  ## either - see this function's own @details, "mic.bull panel". Radial
  ## scale lines default to $mic.bull.radial.breaks (1/2/3/4, i.e. meters
  ## of mic height), with the plot area extending to $mic.bull.radial.max
  ## (default 4) - auto-stretched taller when the actual mic.height needs
  ## more room, the same pattern used by build.mic.panel()'s bar/icon Y
  ## axis below. Per Josh's thirteenth follow-up ("Make the labels for the
  ## scale occur along the left horizontal axis, offset the label by
  ## enough space to fit the label"), the numeric scale labels are drawn
  ## exactly on angle 180 (W) - the same ray as $mic.bull.height.label
  ## (default "(m)", shortened from "Microphone Height (m)" via "Height
  ## (m)" over the eleventh and twelfth follow-ups) - with clearance
  ## between them provided RADIALLY instead: the plot's axis extends a bit
  ## past $mic.bull.radial.max so the height-label axis text (which
  ## renders right at that boundary) has room, rather than sitting on top
  ## of the outermost ring/number as it did (a real bug, fixed in the
  ## tenth follow-up by angularly offsetting the numbers instead - now
  ## reverted per this follow-up's explicit "left horizontal axis"
  ## request). With a white background (via geom_label()) "to make the
  ## label clearer" (per the ninth follow-up), unlike every other
  ## bullseye panel's plain geom_text() labels (see "Label styling and
  ## draw order" below), which are unaffected.
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
    ## Per Josh's seventeenth follow-up (2026-09-16), the caller (the main
    ## per-ARU loop below) may pass an already-shrunk title size when the
    ## full-size titles wouldn't fit the panel width - see @details,
    ## "Figure sizing". NULL (e.g. when this builder is called directly,
    ## as the tests do) falls back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up (2026-09-16), see the equivalent
    ## comment in build.panel() above - same override/fallback pattern.
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override
    panel.grid.color <- get.default("panel.grid.color")
    scale.line.color <- get.default("scale.line.color")
    scale.line.linetype <- get.default("scale.line.linetype")
    scale.line.linewidth <- as.numeric(get.default("scale.line.linewidth"))

    ## Plot area auto-stretches past the default radial max (4) when the
    ## actual mic.height (plus a small margin so the arrowhead never
    ## touches the outer edge) needs more room - same idea as
    ## build.mic.panel()'s Y-axis headroom calculation below.
    radial.max <- max(radial.max.default, mic.height + 0.2)
    ## Extra headroom on the axis (not on radial.max itself, so the
    ## rings/breaks/arrow auto-stretch logic above is untouched) reserved
    ## for $mic.bull.height.label - per Josh's thirteenth follow-up
    ## ("offset the label by enough space to fit the label"): without it,
    ## the outermost "4" ring sits right at the plot boundary, the same
    ## point where the height-label axis text renders, so the two overlap
    ## (confirmed during testing). This pushes the boundary out a bit so
    ## the axis text has clear room while the rings themselves are
    ## unchanged.
    radial.axis.max <- radial.max + 0.7

    radial.label.size <- axis.text.size / ggplot2::.pt
    ## Numeric scale labels (1/2/3/4) drawn exactly on x = 180 (W) - the
    ## LEFT HORIZONTAL axis, the same ray as $mic.bull.height.label
    ## (default "(m)") - per Josh's thirteenth follow-up ("Make the labels
    ## for the scale occur along the left horizontal axis"). Earlier
    ## (tenth follow-up) these were offset 15 degrees off that ray (to
    ## x = 165) to dodge a real overlap bug against the height-label axis
    ## text, back when that label was the much longer "Microphone Height
    ## (m)"; per this follow-up they're back on the exact ray, with
    ## clearance instead provided radially - see the radial.max padding a
    ## few lines below ("offset the label by enough space to fit the
    ## label") - so the two labels no longer compete for the same point.
    radial.label.df <- data.frame(x = rep(180, length(radial.breaks)),
                                   y = radial.breaks,
                                   label = radial.breaks)

    ggplot2::ggplot() +
      ## The Vertical Microphone Orientation arrow - always drawn (see
      ## comment above this function).
      ggplot2::geom_segment(data = data.frame(x = vert.orient, y = 0,
                                               xend = vert.orient, yend = mic.height),
                             ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                             color = arrow.color, linewidth = arrow.linewidth,
                             arrow = grid::arrow(length = grid::unit(arrow.head.cm, "cm"), type = "closed")) +
      ggplot2::geom_hline(yintercept = radial.breaks, color = scale.line.color,
                           linewidth = scale.line.linewidth, linetype = scale.line.linetype) +
      ## Break POSITIONS (0/90/180/270) are the same compass positions used
      ## everywhere else in this function; only the LABELS at those
      ## positions differ from the N/E/S/W used on the other bullseye
      ## panels: x = 0 (E) blank, x = 90 (N, "straight up") carries
      ## $mic.bull.skyward.label, x = 180 (W) carries $mic.bull.height.label,
      ## and x = 270 (S, "straight down") carries $mic.bull.earthward.label.
      ggplot2::scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270),
                                   labels = c("", skyward.label, height.label, earthward.label),
                                   expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, radial.axis.max), breaks = radial.breaks,
                                   expand = c(0, 0)) +
      ## start = -pi/2, direction = -1 - per Josh's tenth follow-up
      ## (2026-09-15, later still: "Top = 90, right 0, Bottom = 270, left =
      ## 180"). Unlike every other panel's start = 0/direction = 1, this
      ## mapping was verified with a live rendered test (not derived by
      ## inspection alone) to put: x = 90 ("Skyward") at the TOP, x = 0
      ## (blank) at the RIGHT, x = 270 ("Earthward") at the BOTTOM, and
      ## x = 180 ($mic.bull.height.label) at the LEFT - reverting the ninth
      ## follow-up's start = 0/direction = 1 (itself a reversion of the
      ## eighth follow-up's start = -pi/2/direction = 1).
      ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = -1) +
      ggplot2::labs(title = mic.bull.title, x = NULL, y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = plot.title.size, hjust = 0.5),
                      axis.text.x = ggplot2::element_text(size = axis.text.size),
                      axis.text.y = ggplot2::element_blank(),
                      axis.ticks = ggplot2::element_blank(),
                      panel.grid.major.x = ggplot2::element_line(color = panel.grid.color),
                      panel.grid.major.y = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
      ## Labels last, same draw-order principle as the other bullseye
      ## panels - but with a white background here (geom_label(), fill =
      ## "white", no border), per Josh's ninth follow-up ("add a white
      ## background to make the label clearer"). This is scoped ONLY to
      ## this panel's numeric scale labels - build.panel()'s own radial
      ## labels (canopy.bull/understory.bull) are deliberately left as
      ## plain, background-less geom_text() (see "Label styling and draw
      ## order" below) and are NOT affected by this change.
      ggplot2::geom_label(data = radial.label.df, ggplot2::aes(x = x, y = y, label = label),
                           inherit.aes = FALSE, size = radial.label.size,
                           fill = "white", label.size = 0,
                           label.padding = grid::unit(0.1, "lines"))
  }

  ## Middle panel - Microphone Height bar plus a Vertical Microphone
  ## Orientation icon, per Josh (2026-09-14, fourth follow-up), replacing
  ## the blank placeholder reserved for it in an earlier round. A single
  ## grey bar rises from 0 to $microphone.height; the matching icon (see
  ## "Vertical Microphone Orientation icon set" above) is drawn entirely
  ## ABOVE the bar's top (per Josh's fifth follow-up fixing a real bug -
  ## see "Icon file format and placement" - it used to straddle the bar's
  ## top and visually cover part of it), sized $icon.height.m tall in data
  ## (y) units - its width in x units is computed from the actual image
  ## pixel aspect ratio so it renders undistorted, ASSUMING the panel
  ## itself renders square (true for this function's default 3-equal-
  ## column/plot.width/plot.height layout, which also keeps the two
  ## coord_polar bullseye panels perfectly circular - not literally
  ## guaranteed for every possible aes.default override).
  build.mic.panel <- function(mic.height, vert.orient, title.size.override = NULL, axis.text.size.override = NULL) {
    mic.panel.title <- get.default("mic.panel.title")
    ## $mic.panel.xlab (formerly "Ground Level") is no longer read here -
    ## per Josh's sixteenth follow-up ("Remove 'Ground Level' from the
    ## mic.bar plot"), this panel's X-axis title is now always blank (see
    ## axis.title.x = element_blank() below), regardless of that setting.
    mic.panel.ylab <- get.default("mic.panel.ylab")
    mic.bar.fill <- get.default("mic.bar.fill")
    ## Round twenty, per Josh (2026-09-16): "Make the bar 1.3 its current
    ## size" - a fixed 1.3x multiplier applied to whatever $mic.bar.width
    ## resolves to (so an aes.style override to $mic.bar.width still scales
    ## proportionally, rather than baking 1.3x into the CSV default itself).
    MIC.BAR.WIDTH.SCALE <- 1.3
    mic.bar.width <- as.numeric(get.default("mic.bar.width")) * MIC.BAR.WIDTH.SCALE
    y.default.max <- as.numeric(get.default("mic.panel.y.default.max"))
    icon.height.m <- as.numeric(get.default("icon.height.m"))
    icon.dir <- get.default("icon.dir")
    ## Per Josh's seventeenth follow-up (2026-09-16), the caller (the main
    ## per-ARU loop below) may pass an already-shrunk title size when the
    ## full-size titles wouldn't fit the panel width - see @details,
    ## "Figure sizing". NULL (e.g. when this builder is called directly,
    ## as the tests do) falls back to $plot.title.size as before.
    plot.title.size <- if (is.null(title.size.override)) as.numeric(get.default("plot.title.size")) else title.size.override
    ## Per Josh's eighteenth follow-up (2026-09-16), see the equivalent
    ## comment in build.panel() above - same override/fallback pattern.
    axis.text.size <- if (is.null(axis.text.size.override)) as.numeric(get.default("axis.text.size")) else axis.text.size.override

    ## Y axis: 0-$mic.panel.y.default.max (4) by default, per Josh ("Y axes
    ## default range is between 0-4 unless the mic height [needs more]") -
    ## only stretched taller when the actual microphone.height plus the
    ## icon's own headroom (plus a small margin) would otherwise run past
    ## the default top. Round twenty, per Josh ("move the mic png to be
    ## centered at the top of the bar"): the icon is now centered ON the
    ## bar's top edge again (see the icon placement comment below), so it
    ## only needs icon.height.m/2 of headroom above mic.height now, not a
    ## full icon.height.m as when it was drawn entirely above the bar.
    y.max <- max(y.default.max, mic.height + icon.height.m / 2 + 0.2)
    x.range <- 2
    bar.x <- 1

    ## Round twenty, per Josh ("...with a line every 0.5 interval"): Y-axis
    ## breaks (and their matching horizontal reference lines) are now every
    ## 0.5 units instead of every whole unit - both the tick labels and the
    ## visible gridlines below are driven off this same sequence.
    Y.GRIDLINE.INTERVAL <- 0.5
    y.breaks <- seq(0, y.max, by = Y.GRIDLINE.INTERVAL)

    p <- ggplot2::ggplot(data.frame(x = bar.x, y = mic.height),
                          ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_col(width = mic.bar.width, fill = mic.bar.fill, color = NA) +
      ggplot2::scale_x_continuous(limits = c(0, x.range), breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, y.max), breaks = y.breaks,
                                   expand = c(0, 0)) +
      ggplot2::labs(title = mic.panel.title, x = NULL, y = mic.panel.ylab) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = plot.title.size, hjust = 0.5),
                      axis.title.x = ggplot2::element_blank(),
                      ## Round twenty, per Josh ("reduce the font size to be
                      ## the same size as the other two plots"): the Y-axis
                      ## TITLE ("Microphone Height (m)") had no explicit
                      ## size set, so it fell back to theme_minimal()'s own
                      ## default (larger than the derived axis.text.size
                      ## used everywhere else on this panel and its
                      ## siblings) - a real, previously-unnoticed mismatch,
                      ## now pinned to the same derived axis.text.size.
                      axis.title.y = ggplot2::element_text(size = axis.text.size),
                      axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_text(size = axis.text.size),
                      axis.ticks = ggplot2::element_blank(),
                      ## Round twenty: panel.grid is no longer fully blanked
                      ## - panel.grid.major.y now draws the requested
                      ## every-0.5-unit reference lines (see y.breaks
                      ## above); panel.grid.major.x/panel.grid.minor stay
                      ## blank (no vertical lines - there's nothing to mark
                      ## on the X axis - and no unlabeled sub-0.5 lines).
                      ## Reuses $panel.grid.color for visual consistency
                      ## with the angular gridlines on the other panels.
                      panel.grid.major.y = ggplot2::element_line(color = get.default("panel.grid.color")),
                      panel.grid.major.x = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      ## Forces this panel's plot area to render as a
                      ## SQUARE, same as every other panel here already
                      ## gets "for free" from coord_polar (ggplot2's
                      ## CoordPolar always renders a square panel, aspect
                      ## = 1 - see @details, "Panel height matching"). Per
                      ## Josh's seventeenth follow-up (2026-09-16): "Adjust
                      ## the mic.bar subplot to be the same height as the
                      ## other subplots" - without this, this panel (the
                      ## only one of the four using plain Cartesian
                      ## coordinates) stretched to fill its ENTIRE cell,
                      ## while the three coord_polar bullseye panels were
                      ## squeezed down to a smaller centered square,
                      ## leaving blank space above/below them - confirmed
                      ## via a rendered comparison (bar panel's drawn
                      ## content spanned ~513px tall vs. ~509-510px for the
                      ## bullseye panels once this was added, previously
                      ## the bar panel spanned the full ~950+px cell
                      ## height). ggplot2 centers a fixed-aspect panel
                      ## within its allotted cell exactly the same way
                      ## coord_polar already does, so this keeps the bar
                      ## anchored/sized consistently with its neighbors
                      ## under any subplot.size.width/height combination,
                      ## not just the default.
                      aspect.ratio = 1)

    icon.angle <- nearest.icon.angle(vert.orient)
    icon.file <- file.path(icon.dir, sprintf("mic_vert_%03d.png", icon.angle))
    if (file.exists(icon.file)) {
      ## HISTORY (kept for context - this behavior is REVERTED this round,
      ## see below): a bug was originally fixed here, per Josh: "the plot
      ## is short of [the microphone height] as the mic pic is obscuring
      ## the top of the bar." The icon used to be CENTERED on the bar's
      ## top (spanning mic.height +/- icon.height.m/2), so its own
      ## bounding box painted over the top half of the bar's true colored
      ## height - even though the bar's data value was always correct,
      ## the visible grey column looked like it stopped well short of
      ## microphone.height. That was fixed by drawing the icon entirely
      ## ABOVE the bar instead (ymin = mic.height, not mic.height -
      ## icon.height.m/2), plus switching the 5 icon images to real-alpha
      ## .png files (see the .dev.R processing note) so even the icon's
      ## own bounding box doesn't paint a solid white square over
      ## anything nearby.
      ##
      ## Round twenty, per Josh ("move the mic png to be centered at the
      ## top of the bar"): explicitly reverts the "entirely above" half of
      ## that fix - the icon is now centered ON the bar's top edge again
      ## (ymin/ymax = mic.height -/+ icon.height.m/2). This is a deliberate
      ## request, not an oversight; the icon's own transparent background
      ## (still real per-pixel alpha, unchanged) means it no longer paints
      ## a solid box over the bar the way the original opaque .jpg icons
      ## did, so the original "obscuring" complaint mostly doesn't apply
      ## to the current .png icon set - but the icon's drawn (non-
      ## transparent) content can still visually sit on top of the bar's
      ## own top edge again. Flagged: if this looks like the original
      ## problem once rendered, this is the section to revert.
      icon.img <- grDevices::as.raster(magick::image_read(icon.file))   # keeps the PNG transparency
      ## dim() is (height.px, width.px) - used to keep the icon
      ## undistorted (see function-level comment above).
      icon.aspect <- dim(icon.img)[2] / dim(icon.img)[1]
      icon.width.x <- icon.aspect * icon.height.m * (x.range / y.max)
      p <- p + ggplot2::annotation_custom(
        grid::rasterGrob(icon.img, interpolate = TRUE),
        xmin = bar.x - icon.width.x / 2, xmax = bar.x + icon.width.x / 2,
        ymin = mic.height - icon.height.m / 2, ymax = mic.height + icon.height.m / 2)
    } else {
      warning(sprintf("No icon file found for microphone.orientation_vertical %s (nearest known angle %s) at '%s' - mic panel drawn without an icon",
                       vert.orient, icon.angle, icon.file))
    }
    p
  }

  aru.labels <- unique(mic$aru.label)
  result.plots <- list()
  result.ggplots <- list()

  for (aru in aru.labels) {
    mic.row <- mic[mic$aru.label == aru, , drop = FALSE][1, ]
    quad.rows <- data[data$aru.label == aru, , drop = FALSE]

    missing.quad <- setdiff(QUADRANT.ORDER, quad.rows$quadrant.std)
    if (length(missing.quad) > 0) {
      warning(sprintf("aru.label '%s' is missing quadrant(s): %s - skipped",
                       aru, paste(missing.quad, collapse = ", ")))
      next
    }

    canopy.values <- setNames(quad.rows$canopy.value, quad.rows$quadrant.std)[QUADRANT.ORDER]
    understory.values <- setNames(quad.rows$understory.value, quad.rows$quadrant.std)[QUADRANT.ORDER]
    horiz.orient <- as.numeric(mic.row$microphone.orientation_horizontal)
    vert.orient <- as.numeric(mic.row$microphone.orientation_vertical)
    mic.height <- as.numeric(mic.row$microphone.height)

    ## Figure sizing, per Josh's fourteenth follow-up (2026-09-15) - see
    ## @details, "Figure sizing" for the full precedence. $plot.width is no
    ## longer read in either branch; $plot.height is read only when
    ## auto.scale = TRUE. Computed here, BEFORE the panels themselves are
    ## built (moved up per the seventeenth follow-up), because the title-
    ## size resolution immediately below needs plot.width already settled.
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
    ## size so that they don't run together." See @details, "Figure
    ## sizing" for the full reasoning. This supersedes the fifteenth
    ## follow-up's original mechanism, which only ever widened the figure
    ## and was scoped to exactly the "all four panel types selected" case.
    ## The new version: (1) applies to whichever panels are actually in
    ## $sub.plots, not just the all-four case, since narrow panels can
    ## make ANY subset's titles run together, not only the four-panel one;
    ## (2) tries shrinking ALL displayed panel titles to the SAME reduced
    ## font size first (uniformly, per Josh's "titles all reduce"), so the
    ## figure stays at whatever width auto.scale/subplot.size.width already
    ## chose (honoring the fourteenth follow-up's "total plot size must be
    ## 6.5 inches wide") whenever shrinking alone is enough - which it is
    ## for the default all-four-panel case (measured to settle around
    ## 9.5-9.6pt, down from the $plot.title.size default of 12pt); (3)
    ## still never narrows the figure below what auto.scale/
    ## subplot.size.width computed, and only widens it - same guarantee as
    ## the fifteenth follow-up - if titles still would not fit even once
    ## shrunk all the way down to a floor (so titles never become
    ## illegibly small).
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
    ## current title labels at the top of each subplot" - see @details,
    ## "Axis label sizing". Derived from whatever the title size resolved
    ## to just above (whether that's still the full $plot.title.size, or
    ## shrunk by the block above), NOT an independent setting anymore -
    ## $axis.text.size is no longer read.
    AXIS.TITLE.SIZE.GAP.PT <- 2
    resolved.axis.text.size <- resolved.title.size - AXIS.TITLE.SIZE.GAP.PT

    ## One builder per possible $sub.plots entry, per Josh (2026-09-15) -
    ## only the ones actually requested (and in the order requested) get
    ## built and combined below. Each now takes the resolved (possibly
    ## shrunk) title font size and the derived axis label size from just
    ## above, per Josh's seventeenth and eighteenth follow-ups.
    ## 2026-10-28, per Josh: the direction arrow only on the first
    ## canopy/understory bullseye (arrow.panel = "first")
    bull.idx <- which(sub.plots %in% c("canopy.bull", "understory.bull"))
    arrow.on <- seq_along(sub.plots) %in% (if (arrow.panel == "first") bull.idx[1] else bull.idx)
    panel.builders <- list(
      canopy.bull = function(title.size, axis.size, arrow = TRUE) build.panel(canopy.values, get.default("canopy.title"), horiz.orient, title.size, axis.size, arrow),
      understory.bull = function(title.size, axis.size, arrow = TRUE) build.panel(understory.values, get.default("understory.title"), horiz.orient, title.size, axis.size, arrow),
      mic.bull = function(title.size, axis.size, arrow = TRUE) build.mic.bullseye(mic.height, vert.orient, title.size, axis.size),
      mic.bar = function(title.size, axis.size, arrow = TRUE) build.mic.panel(mic.height, vert.orient, title.size, axis.size)
    )
    ## 2026-10-28, per Josh: title.size.adjust changes the titles only
    panel.title.size <- max(4, resolved.title.size + title.size.adjust)
    panels <- lapply(seq_along(sub.plots), function(k)
      panel.builders[[sub.plots[k]]](panel.title.size, resolved.axis.text.size, arrow.on[k]))
    ## 2026-10-28, per Josh: (a), (b), (c) ... at the bottom right of each panel
    if (!is.null(panel.labels)) {
      panels <- lapply(seq_along(panels), function(k)
        panels[[k]] + ggplot2::labs(caption = panel.labels[k]) +
          ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = resolved.title.size)))
    }

    ## Round twenty, per Josh ("scale the size of the mic.bar plot to be
    ## 2/3 the size of the canopy plots"): mic.bar's own column now gets a
    ## relative width of 2/3 versus every other panel type's 1 - combined
    ## with aspect.ratio = 1 (see build.mic.panel()), a narrower column
    ## renders as a proportionally smaller SQUARE panel, matching "2/3 the
    ## size" directly. This only changes the relative WIDTHS wrap_plots()
    ## assigns to each column; it does not change the "Figure sizing"/
    ## "Title auto-shrink" calculations above, which still assume every
    ## column gets an equal share of plot.width when deciding whether
    ## titles need to shrink - so mic.bar's own title measures against
    ## that same equal-share width, matching the "same font size as the
    ## other two plots" request, but its RENDERED column is narrower than
    ## that share. Flagged: if "Microphone Vertical Orientation" (this
    ## round's new mic.bar title, see plotopts_bullseye.csv) reads as
    ## visually tight/crowded in its now-narrower column, the fix would be
    ## generalizing the auto-shrink fit-check to each panel's own actual
    ## (possibly unequal) column width rather than a uniform share - not
    ## done here since it wasn't asked for and the rendered examples below
    ## didn't show a problem.
    MIC.BAR.WIDTH.FRACTION <- 2 / 3
    panel.width.weights <- ifelse(sub.plots == "mic.bar", MIC.BAR.WIDTH.FRACTION, 1)

    combined <- patchwork::wrap_plots(panels, ncol = length(sub.plots),
                                       widths = panel.width.weights)

    ggsave.dpi <- as.numeric(get.default("ggsave.dpi"))
    ggsave.units <- get.default("ggsave.units")
    ggsave.width.pad <- as.numeric(get.default("ggsave.width.pad"))
    ggsave.height.pad <- as.numeric(get.default("ggsave.height.pad"))

    ## Round nineteen, per Josh (2026-09-16): every saved file name is now
    ## always "<project.name>_<ARU>_<timestamp>.png" - $output.filename.pattern
    ## is DEPRECATED and no longer read (see @details, "Settings resolution
    ## (round nineteen)"). Timestamp format (round twenty-two), per Josh
    ## (2026-09-24): see @details, "Timestamp format (round twenty-two)" -
    ## no underscore between the date and time halves.
    fname <- sprintf("%s_%s_%s.png", project.name, aru, format(Sys.time(), "%Y%m%d%H%M%S"))
    fname <- file.path(dir.save, fname)

    ggplot2::ggsave(fname, combined,
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

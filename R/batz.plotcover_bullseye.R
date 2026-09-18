#' Plot a "bullseye" habitat-cover diagram (Canopy / Understory) for an ARU
#'
#' Draws a row of polar "bullseye" and bar panels for an ARU's habitat-cover
#' assessment and microphone deployment metadata - by default, Canopy
#' cover, Microphone Height, Vertical Microphone Orientation, and
#' Understory cover, left to right (see \code{sub.plots} to change which
#' panels appear, and in what order). The \code{"canopy.bull"}/
#' \code{"understory.bull"} panels each show: the four quadrants
#' (northeast/southeast/southwest/northwest) as a solid grey, unoutlined
#' wedge sized to that quadrant's cover value; the mean of all four
#' quadrants as an unfilled, red-outlined dashed circle; and an arrow from
#' the plot's center to its edge for the Horizontal Microphone Orientation
#' - each independently toggleable (see \code{quadrant.cover},
#' \code{mean.cover}, \code{cover.arrows}). \code{"mic.bar"} shows that
#' ARU's mic height as a bar with an icon representing the Vertical
#' Microphone Orientation on top (see Details, "Middle panel").
#' \code{"mic.bull"} shows that same Vertical Microphone Orientation as a
#' polar "bullseye" arrow instead, its length equal to the mic height (see
#' Details, "mic.bull panel"). One combined figure is produced per
#' distinct \code{aru.label} found in \code{mic}.
#'
#' @param data A data frame of the raw habitat-cover assessment (e.g.
#'   Josh's \code{bulleye_model_data.xlsx}, "cover" sheet). Column headers
#'   are run through \code{standardize.headers()} on receipt (see Details,
#'   "Header standardization"), after which it must have (in any original
#'   spelling that standardizes to): \code{aru.label}, \code{"Select the
#'   quadrant you are assessing"}, \code{"Canopy cover"}, \code{"Understory
#'   cover"}. One row per quadrant per \code{aru.label} - every
#'   \code{aru.label} that will be plotted needs exactly the four quadrant
#'   values \code{"northeast"}/\code{"southeast"}/\code{"southwest"}/
#'   \code{"northwest"} (case/whitespace-insensitive).
#' @param mic A data frame of microphone deployment metadata (e.g. Josh's
#'   \code{bulleye_model_data.xlsx}, "mic" sheet). Also header-standardized
#'   on receipt; must have (in any spelling that standardizes to):
#'   \code{aru.label}, \code{"Microphone Height"}, \code{"Horizontal
#'   Microphone Orientation"}, \code{"Vertical Microphone Orientation"}.
#'   One row per \code{aru.label}.
#' @param aes.default A data frame of default plot settings, one row per
#'   parameter (e.g. \code{plotopts_bullseye.csv}). Must have
#'   \code{$category}, \code{$parameter}, \code{$default.value}; an
#'   \code{$overide.value} column (blank, user-fillable) and \code{$notes}
#'   are optional - see \code{project.name}/\code{aes.style} below and
#'   Details, "Settings resolution (round nineteen)".
#' @param project.name Character, default \code{"new.project"}. Per Josh's
#'   nineteenth follow-up (2026-09-16), this NO LONGER selects an
#'   \code{aes.default} override column - it is now used ONLY to build the
#'   first part of every saved file name: \code{"<project.name>_<ARU>_
#'   <timestamp>.png"}. See Details, "Settings resolution (round
#'   nineteen)" for what replaced the old column-matching behavior.
#' @param aes.style Character, default \code{"overide.value"}. Names which
#'   column of \code{aes.default} is checked FIRST for each parameter
#'   (falling back to \code{$default.value} when that column doesn't exist,
#'   or is blank for that row) - replaces the old \code{project.name}
#'   column-matching mechanism entirely. See Details, "Settings resolution
#'   (round nineteen)".
#' @param dir.save Character, default \code{getwd()}. Directory each
#'   generated PNG is saved into. Per Josh's nineteenth follow-up, the file
#'   name itself is now always \code{"<project.name>_<ARU>_<timestamp>.png"}
#'   (see \code{project.name} above) - \code{aes.default}'s
#'   \code{$output.filename.pattern} is DEPRECATED and no longer read.
#' @param sub.plots Character vector, default \code{c("canopy.bull",
#'   "mic.bar", "mic.bull", "understory.bull")} (i.e. all four, in that
#'   order) - which panels to draw, and in what left-to-right order.
#'   Every element must be one of \code{"canopy.bull"} (the Canopy cover
#'   bullseye), \code{"mic.bar"} (the Microphone Height bar + Vertical
#'   Orientation icon), \code{"mic.bull"} (a polar bullseye-style plot of
#'   the Vertical Microphone Orientation - see Details, "mic.bull panel"),
#'   or \code{"understory.bull"} (the Understory cover bullseye); an
#'   unrecognized value stops with an error naming the allowed set.
#'   Repeats are allowed (the same panel type can appear more than once).
#' @param cover.arrows Logical, default \code{TRUE}. Whether the
#'   Horizontal Microphone Orientation arrow is drawn on
#'   \code{"canopy.bull"}/\code{"understory.bull"} panels. Does not
#'   affect \code{"mic.bar"} or \code{"mic.bull"} - \code{"mic.bull"}'s
#'   own Vertical Microphone Orientation arrow is a required part of that
#'   panel and is always drawn (see Details, "mic.bull panel").
#' @param mean.cover Logical, default \code{TRUE}. Whether the dashed
#'   mean-of-quadrants circle is drawn on
#'   \code{"canopy.bull"}/\code{"understory.bull"} panels. Does not
#'   affect \code{"mic.bar"} or \code{"mic.bull"} (neither has quadrants
#'   or a mean-of-quadrants circle).
#' @param quadrant.cover Logical, default \code{TRUE}. Whether the four
#'   quadrant wedges are drawn on
#'   \code{"canopy.bull"}/\code{"understory.bull"} panels. Does not
#'   affect \code{"mic.bar"} or \code{"mic.bull"} (neither has quadrants
#'   or a mean-of-quadrants circle).
#' @param auto.scale Logical, default \code{TRUE}, per Josh's fourteenth
#'   follow-up (2026-09-15). When \code{TRUE}, the saved combined figure's
#'   total width is fixed at \code{6.5} inches (per Josh: "the total plot
#'   size must be 6.5 inches wide"), divided evenly across however many
#'   panels \code{sub.plots} draws - same auto-fit mechanism as before,
#'   just a fixed total instead of \code{$plot.width}. Height is
#'   unaffected by this argument and still comes from \code{$plot.height}.
#'   When \code{FALSE}, \code{subplot.size.width}/\code{subplot.size.height}
#'   set each individual panel's size directly instead - see those two
#'   arguments below. See Details, "Figure sizing" for the full precedence
#'   (including why \code{$plot.width} is no longer read either way).
#' @param subplot.size.height Single positive number, default \code{3}.
#'   Only used when \code{auto.scale = FALSE} - the height, in inches, of
#'   EACH sub-plot (and thus of the whole combined figure, since every
#'   panel shares one row). Ignored when \code{auto.scale = TRUE}.
#' @param subplot.size.width Single positive number, default \code{2}.
#'   Only used when \code{auto.scale = FALSE} - the width, in inches, of
#'   EACH sub-plot; the combined figure's total width is this times
#'   \code{length(sub.plots)}. Ignored when \code{auto.scale = TRUE}.
#'
#' @return Invisibly, a list with \code{plots} (one entry per
#'   \code{aru.label} actually plotted: parsed \code{canopy}/
#'   \code{understory} quadrant values, the matched \code{mic} row, and the
#'   saved file path) and \code{ggplots} (the corresponding combined
#'   \code{patchwork} objects).
#'
#' @details
#' \strong{Naming - not yet confirmed with Josh.} Requested as "a model
#' bulls eye plot" with no function name given. Named
#' \code{batz.plotcover_bullseye()} following this package's
#' \code{batz.<family>_<action>.<subject>()} convention with the verb
#' baked into the family name (\code{family = "plotcover"}, same pattern as
#' \code{batz.plotdetections_first.last}/\code{batz.plotframe_batactivity})
#' - \strong{please confirm this name is acceptable.}
#'
#' \strong{Header standardization (per Josh, 2026-09-14 project preference)
#' - DOES apply here, unlike the sibling plotting functions.} \code{data}
#' and \code{mic} are genuinely raw, externally-supplied headers (straight
#' from a spreadsheet Josh fills in by hand), not the already-standardized
#' output schema of an upstream \code{batz} function - so both are run
#' through the shared package helper \code{standardize.headers()}
#' immediately on receipt. This is why the required headers above are
#' written as their original human-readable text ("Canopy cover", etc.) -
#' they arrive there, then get folded to \code{canopy_cover} etc. before
#' this function's own required-header check runs. \code{aes.default}
#' is NOT standardized, for the same reason it isn't in
#' \code{batz.plotdetections_first.last()}: it's this function's own
#' invented parameter-name settings sheet, not raw external data.
#'
#' \strong{Plotopt convention (per Josh: "use the same identifiers as in
#' the plotopt for the other functions").} Reuses
#' \code{aes.default}/\code{get.default()}/\code{project.name}/
#' \code{dir.save} exactly as established in
#' \code{batz.plotdetections_first.last()} and
#' \code{batz.plotactivity_observations()} - \code{$category}/
#' \code{$parameter}/\code{$default.value} rows, optional \code{$notes}
#' and \code{project.name}-matching override column(s), with a job-row
#' value (when one exists) beating a \code{project.name} column value,
#' which beats \code{$default.value}. This function has no \code{fig.list}-
#' style per-job settings sheet (each \code{aru.label} present in
#' \code{mic} IS the "job"), so only the \code{project.name}-column /
#' \code{$default.value} half of that precedence applies - there is no
#' \code{get.setting()} here, only \code{get.default()}. \strong{This
#' paragraph describes the OLD mechanism, kept as dated history - see the
#' next entry for what replaced it.}
#'
#' \strong{Settings resolution (round nineteen), per Josh's 2026-09-16
#' follow-up ("reorder the headings in all plotopts files ... add
#' arguments aes.style = \"overide.value\" ... Function logic will first
#' look in the column with the header = aes.style ... then if that element
#' is blank use $default.value"):} the \code{project.name}-matches-a-
#' column-name mechanism described just above is REPLACED entirely. Every
#' \code{aes.default} sheet now has a fixed \code{$overide.value} column
#' (blank by default, between \code{$default.value} and \code{$notes} -
#' see \code{\link{batz.generate_plotopts}}), which the user fills in
#' directly on their own copy of the CSV to override a setting. The new
#' \code{aes.style} argument (default \code{"overide.value"}) names which
#' column \code{get.default()} checks FIRST; when that column doesn't
#' exist (an older sheet) or is blank for a given row, \code{$default.value}
#' is used, exactly as before. \code{project.name} no longer participates
#' in settings resolution AT ALL - it is now used purely to build the
#' saved file name (see "File naming (round nineteen)" below). This is a
#' judgment call on backward compatibility, flagged for Josh: an older
#' \code{aes.default} sheet with its own \code{project.name}-matching
#' column (e.g. a column literally named \code{"gome"}) will simply be
#' ignored now (that column is neither \code{$default.value} nor
#' \code{$overide.value}) - re-fill any values that were in that column
#' into the new \code{$overide.value} column instead.
#'
#' \strong{File naming (round nineteen), same follow-up:} every saved PNG's
#' file name is now always \code{"<project.name>_<ARU>_<timestamp>.png"} -
#' \code{aes.default}'s \code{$output.filename.pattern} is DEPRECATED and
#' no longer read at all (same deprecation pattern as \code{$plot.width}/
#' \code{$axis.text.size} in earlier rounds - the row is left in place in
#' \code{plotopts_bullseye.csv}, harmless, simply ignored). Each save also
#' now prints its file name to the console (\code{cat("Saved:", fname,
#' "\\n")}), matching the convention already used by
#' \code{batz.plotdetections_first.last()}/
#' \code{batz.plotactivity_observations()}.
#'
#' \strong{Cover-value parsing ("Take the midpoint for each category, e.g.
#' 25-50 would be 37.5, if there is a single number use that number").} A
#' value containing an underscore OR a hyphen (Josh's real uploaded data
#' uses underscore-separated ranges like \code{"51_75"}; his own written
#' example used a hyphen, \code{"25-50"}) is split on \code{"[_-]+"} and
#' averaged (e.g. \code{"51_75"} -> 63, \code{"25-50"} -> 37.5); anything
#' else is used as a plain number (e.g. \code{100}, \code{"0"}).
#'
#' \strong{Quadrant angles and compass orientation.} \code{coord_polar(theta
#' = "x", start = 0, direction = 1)} is used so that a plain 0-360 compass
#' bearing maps directly onto the polar angle - 0 = North = top, 90 = East
#' = right, 180 = South = bottom, 270 = West = left, increasing clockwise -
#' confirmed with a synthetic rendered test before writing this function
#' (see this function's own \code{.dev.R} header comment). Each quadrant is
#' drawn as a \code{geom_col(width = 90)} bar centered on its own midpoint
#' angle - northeast = 45, southeast = 135, southwest = 225, northwest =
#' 315 - so its 90-degree span exactly covers the compass range Josh gave
#' (e.g. northeast's bar spans 0-90). The angle-axis grid is labeled
#' N/E/S/W at the same 0/90/180/270 breaks as a readability aid (not
#' explicitly requested, easy to remove if unwanted).
#'
#' \strong{Quadrant wedges and mean circle.} Each of the four quadrant
#' wedges is filled with one flat color (\code{$quadrant.fill}, default
#' \code{"grey70"}) and no outline (\code{$quadrant.color}, default
#' \code{"NA"} - any value that reads as \code{"NA"} or blank is treated
#' as no outline; a real color name/hex draws one instead, if ever
#' wanted). The mean of all four quadrants is drawn via
#' \code{geom_hline(yintercept = mean(...))} inside \code{coord_polar} -
#' confirmed by a live rendered test that this draws a full circle (not a
#' straight line) at that radius - colored \code{$mean.color} (default
#' \code{"red"}), DASHED (\code{linetype = "dashed"}, per Josh's
#' follow-up - was solid in the first draft); a \code{geom_hline} is a
#' line already, so "no fill" is automatic, not a separate setting.
#'
#' \strong{Horizontal Microphone Orientation arrow - drawn on BOTH panels,
#' not explicitly specified which.} Josh's spec didn't say whether the
#' arrow belongs on the Canopy panel, the Understory panel, or both; since
#' it's a property of the ARU's physical deployment (not of either cover
#' variable), it's drawn identically on both panels - \strong{flagged, easy
#' to change to one panel only if Josh prefers.} Implemented as a single
#' \code{geom_segment()} from the center (radius 0) to the plot edge
#' (radius \code{$radial.max}) at a constant angle (the bearing itself),
#' which stays perfectly radial under \code{coord_polar} since only the
#' radius changes along the segment; arrow head via \code{grid::arrow()}.
#'
#' \strong{Radial scale labels - moved onto the vertical (N-S) axis, per
#' Josh's follow-up, and drawn twice (2026-09-14, later same day, per a
#' second follow-up).} The first draft relied on \code{ggplot2}'s default
#' radial (y) axis text, which \code{coord_polar} renders off to one side
#' of the plot rather than along the vertical N-S line - and the default
#' scale expansion padded a few percent beyond \code{$radial.max} on both
#' ends, making the plotted area visually read as extending past 100
#' toward roughly 125 instead of stopping at a clean \code{$radial.max}
#' (110). Both fixed together: \code{scale_x_continuous()}/
#' \code{scale_y_continuous()} now pass \code{expand = c(0, 0)} (no
#' padding beyond the stated limits), the default \code{axis.text.y} is
#' hidden entirely, and the four \code{$radial.breaks} values (25/50/75/
#' 100) are drawn manually via \code{geom_text()} - at BOTH \code{x = 0}
#' (North) and \code{x = 180} (South), per Josh: "Repeat the labels ...
#' on the southern axis as well".
#'
#' \strong{Label styling and draw order.} The labels were originally
#' \code{geom_label()} (a white background box behind each number, so it
#' stayed legible over a grey wedge or the red circle); Josh asked for the
#' box removed ("Remove the white box around the labels"), so this is now
#' plain \code{geom_text()} with no background - legibility instead comes
#' from draw order (see below).
#'
#' \strong{Draw order, per Josh's third follow-up} ("draw order should be
#' Quadrats > mean cover > mic direction > scale lines > labels"). The
#' panel's \code{geom_*} layers are added in exactly that sequence:
#' \code{geom_col()} (quadrant wedges) first, then \code{geom_hline()}
#' (dashed mean-of-quadrants circle), then \code{geom_segment()} (the
#' Horizontal Microphone Orientation arrow), then a second
#' \code{geom_hline()} (the 25/50/75/100 scale-line rings - see "Scale
#' lines" below), then \code{geom_text()} (the numeric labels) last, so
#' each layer renders on top of everything before it. (Draw order in
#' \code{ggplot2} follows the sequence of \code{geom_*}/\code{stat_*}
#' layers only - \code{scale_*}/\code{coord_*}/\code{theme()} calls don't
#' affect it, which is also why the scale lines had to move off
#' \code{theme(panel.grid)} entirely - see below.)
#'
#' \strong{Scale lines - converted from theme gridlines to an explicit
#' geom layer, styled black/dashed, per Josh's third follow-up} ("make the
#' scale lines black and dashed"). The 25/50/75/100 reference rings were
#' originally drawn via \code{theme(panel.grid.major = element_line(...))}
#' colored \code{$panel.grid.color} (grey). \code{ggplot2} always renders
#' theme-based gridlines UNDERNEATH all geom/data layers regardless of
#' where the \code{theme()} call sits in the \code{+} chain, so there is
#' no way to satisfy Josh's draw-order request ("scale lines" after "mic
#' direction", before "labels") while the rings remain theme-driven. They
#' are now drawn as an explicit \code{geom_hline(yintercept =
#' radial.breaks)} layer instead - the same "\code{geom_hline} inside
#' \code{coord_polar} draws a full circle" trick already used for the
#' mean-cover circle, one circle per \code{$radial.breaks} value - colored
#' \code{$scale.line.color} (default \code{"black"}), styled
#' \code{$scale.line.linetype} (default \code{"solid"} - was \code{"dashed"}
#' earlier the same day, reversed by Josh's very next follow-up: "make the
#' bulls eye plot scale lines solid"), sized \code{$scale.line.linewidth},
#' and positioned in the \code{+} chain right where the draw order calls
#' for it. \code{panel.grid.major.y} is now
#' blanked (so the old grey rings don't also draw, underneath, redundantly)
#' while \code{panel.grid.major.x} (the N-S/E-W compass cross lines, not
#' part of Josh's draw-order or color/linetype request) is left as before,
#' colored by \code{$panel.grid.color}. \code{panel.grid.minor} stays
#' blank (unchanged from the previous follow-up - see "Grid lines" below).
#'
#' \strong{Grid lines - diagonal spokes and phantom outer ring removed,
#' per Josh's second follow-up.} The first draft left \code{ggplot2}'s
#' default MINOR gridlines on: for the x (angle) scale, with major breaks
#' only at 0/90/180/270, the auto-computed minor breaks land at
#' 45/135/225/315 - exactly the quadrant CENTER angles - which
#' \code{coord_polar} renders as two diagonal lines through the center
#' (southeast-to-northwest and northeast-to-southwest), matching what
#' Josh described as "access lines that run southeast to south[west]...
#' northwest and northwest to southeast". Minor gridlines on the y
#' (radial) scale similarly added an extra, unlabeled ring reinforcing the
#' "goes past 100 toward 125" impression from the scale-expansion bug
#' below. Both removed together with \code{panel.grid.minor =
#' element_blank()}. \code{panel.border}/\code{axis.line} are also blanked
#' defensively (per Josh's third follow-up restating "remove the scale
#' line at 125") so no border or axis line can visually read as an extra
#' ring/edge beyond the outermost labeled 100 ring; the plot area itself
#' still legitimately extends to \code{$radial.max} (110, unlabeled
#' margin) as in the original spec.
#'
#' \strong{Background - reverted to plain white, per Josh's third
#' follow-up correcting a misreading of his second follow-up.} His second
#' follow-up said "the background around the outside to be removed. It
#' should be clear see through" - taken at the time to mean the whole
#' plot/PNG background, and implemented as a real alpha-transparent PNG
#' (\code{panel.background}/\code{plot.background} set transparent in
#' each panel's theme, \code{ggsave(..., bg = "transparent")}, plus a
#' transparent \code{theme()} applied to the combined \code{patchwork}
#' object to work around \code{wrap_plots()}'s own opaque white wrapper
#' background). Josh's third follow-up clarifies that was not the intent:
#' "The background should still be white. What I meant was that the text
#' boxes for the labels should be clear" - i.e. only the small white boxes
#' behind the axis labels (already removed, see "Label styling and draw
#' order" above, by switching \code{geom_label()} to \code{geom_text()}
#' - satisfied even before the transparency work below was mistakenly
#' added). All of the transparency changes are now reverted: no
#' transparent \code{panel.background}/\code{plot.background} theme
#' overrides (falls back to \code{theme_minimal()}'s plain white), no
#' transparent override on the combined \code{patchwork} object, and
#' \code{ggsave()} now passes \code{bg = "white"} explicitly.
#'
#' \strong{Middle panel - now a real third plot, per Josh's fourth
#' follow-up (2026-09-14), replacing the blank placeholder from an earlier
#' round.} A single grey \code{geom_col()} bar rises from 0 to that ARU's
#' \code{$microphone_height}; its Y axis defaults to 0-\code{$mic.panel.y.
#' default.max} (4), per Josh ("Y axes default range is between 0-4 unless
#' the mic height [needs more]") - only stretched taller when the actual
#' \code{microphone_height} (plus the icon's own half-height and a small
#' margin) would otherwise run past the default top; X axis has NO title
#' at all as of Josh's sixteenth follow-up (2026-09-15) ("Remove 'Ground
#' Level' from the mic.bar plot") - \code{$mic.panel.xlab} (formerly
#' \code{"Ground Level"}) is no longer read; the X-axis title area is
#' hardcoded blank (\code{axis.title.x = element_blank()}) regardless of
#' that setting's value, and its row remains in
#' \code{plotopts_bullseye.csv} only for backward compatibility (marked
#' deprecated in its own \code{$notes}). Y axis title \code{$mic.panel.ylab}
#' (default \code{"Microphone Height (m)"}); panel title
#' \code{$mic.panel.title} (default \code{"Microphone Height and"} plus
#' \code{"Vertical Orientation"} on a second line - an embedded line break
#' per Josh's sixteenth follow-up, see "Panel title wrapping" below -
#' styled the same as the Canopy/Understory panel titles).
#'
#' \strong{Vertical Microphone Orientation icon set, per Josh's fourth
#' follow-up, images swapped to .png in a fifth follow-up same day - a
#' real assumption, please confirm.} Josh supplied 5 icon images
#' diagramming a microphone tilted at 5 specific angles, using this angle
#' convention (his own words): "0 [is] parrell [parallel] with ground...
#' 90 is straight up... 270 is straight down... 45 is up on an angle and
#' 315 [is] pointing down on an angle." These 5 files
#' (\code{mic_vert_000.png}/\code{045}/\code{090}/\code{270}/\code{315.png},
#' matched to those angles by their sound-wave-squiggle direction) live
#' in \code{$icon.dir} (default \code{"img"}, a directory path resolved
#' relative to the current working directory when the function is
#' called - NOT relative to \code{dir.save} or to this function's own
#' file location). Originally supplied as solid-black-icon .jpg files,
#' Josh re-sent them as outline-style .png files in his very next
#' follow-up ("use the attached .png instead") - see "Icon file format"
#' below for why that swap mattered, beyond just a style change. An
#' ARU's actual \code{vertical_microphone_orientation} value is matched
#' to the NEAREST of these 5 known angles by circular distance (e.g. 350
#' degrees is closer to 0 than to 270) - \strong{not yet confirmed with
#' Josh whether "nearest" is the right behavior for a value that doesn't
#' exactly match, versus only drawing an icon on an exact match, or
#' treating a non-matching value as an error} - and only these 5 angles
#' have an icon at all right now; if Josh adds icons for other angles
#' (e.g. 135, 180, 225) later, only \code{ICON.ANGLES} inside this
#' function needs updating to include them. If the matched icon file is
#' missing from \code{$icon.dir}, a \code{warning()} is issued (naming
#' the ARU, the requested angle, and the file path it looked for) and the
#' bar is still drawn without an icon on top, rather than stopping the
#' whole call.
#'
#' \strong{Icon file format and placement - a real bug fixed, per Josh's
#' fifth follow-up} ("The microphone height in the example file was 3m,
#' [the] plot is short of that as the mic pic is obscuring the top of the
#' bar, use the attached .png instead"). Two compounding problems, both
#' now fixed: (1) the icon used to be drawn CENTERED on the bar's top
#' (spanning \code{mic.height +/- icon.height.m/2}), so the icon's own
#' image - a solid opaque square, even after being cropped tightly to its
#' drawn content - painted over the TOP HALF of the bar's true colored
#' height; the bar's underlying data/height was always correct (verified
#' with \code{ggplot_build()}), but visually the grey column looked like
#' it stopped well short of \code{microphone_height}. Fixed by drawing
#' the icon entirely ABOVE the bar instead (\code{ymin = mic.height},
#' \code{ymax = mic.height + icon.height.m}), so nothing ever paints over
#' the bar's own colored area, at the cost of needing a full
#' \code{icon.height.m} of headroom above \code{mic.height} (not just
#' half) when computing the Y axis's upper limit. (2) Separately, the
#' original 5 icon images were opaque .jpg files - solid white outside
#' the drawn icon shape, with no way to make that background see-through.
#' Even with fix (1) in place, an opaque icon can still visually cover
#' whatever it's drawn over; Josh's re-sent .png versions were processed
#' (a near-white color threshold, with a small feather band to keep
#' anti-aliased edges smooth) to make their background genuinely
#' transparent (a real per-pixel alpha channel, not just nominally an
#' RGBA file with alpha stuck at fully opaque, which is what the original
#' upload turned out to be), then cropped tightly to their non-
#' transparent content - read with \code{png::readPNG()} (returns an
#' RGBA array) rather than \code{jpeg::readJPEG()} (RGB only, no alpha),
#' and \code{grid::rasterGrob()} respects that alpha automatically when
#' drawing, so the icon's own bounding box is no longer a solid block
#' over anything it happens to be positioned near.
#'
#' \strong{Icon sizing - kept undistorted by computing its width from the
#' image's own pixel aspect ratio, not a fixed guess.} The icon is drawn
#' via \code{grid::rasterGrob()} inside \code{ggplot2::annotation_custom()},
#' \code{$icon.height.m} (default \code{1}) tall in Y-axis data units.
#' Because the middle panel's X and Y axes don't necessarily span the
#' same number of data units per unit of physical panel size, drawing
#' the icon at a fixed WIDTH in X units (whatever that width) would
#' stretch or squash it relative to its real image proportions; instead,
#' the icon's width in X units is computed from its real pixel
#' width/height ratio (\code{dim(icon.img)} from \code{png::readPNG()})
#' times its Y-unit height, scaled by the ratio of the axes' data-unit
#' spans (\code{x.range / y.max}) - this correctly keeps the icon's
#' on-page proportions matching its actual image whenever the panel
#' itself renders as a SQUARE (true for this function's default
#' 3-equal-column layout at \code{$plot.width}/\code{$plot.height} =
#' 10/5 inches, the same assumption that keeps the two \code{coord_polar}
#' bullseye panels perfectly circular - not literally guaranteed under
#' every possible \code{aes.default} override, e.g. a very different
#' \code{plot.width}/\code{plot.height} ratio).
#'
#' \strong{Sub-plot selection and toggles, per Josh's sixth follow-up
#' (2026-09-15) adding \code{sub.plots}/\code{cover.arrows}/\code{mean.cover}/
#' \code{quadrant.cover}.} \code{sub.plots} controls both WHICH panels are
#' drawn and their left-to-right order (an \code{lapply()} over
#' \code{sub.plots}, dispatching to one builder function per recognized
#' name, then combined with \code{patchwork::wrap_plots(..., ncol =
#' length(sub.plots))} - so the combined figure always has exactly as many
#' columns as \code{sub.plots} has entries, replacing the original fixed
#' 3-panel/\code{ncol = 3} layout). Panel order was already tied directly
#' to \code{sub.plots}'s own order by construction (\code{lapply()}
#' preserves the order of what it iterates over, and \code{wrap_plots()}
#' lays panels out left to right in the order given) - re-verified per
#' Josh's fourteenth follow-up (2026-09-15) with an explicit test checking
#' the actual rendered panel titles come back in the same order as
#' \code{sub.plots}, not just the same COUNT that earlier rounds' tests
#' checked (see this function's own \code{.dev.R}, Test 12). The default,
#' \code{c("canopy.bull",
#' "mic.bar", "mic.bull", "understory.bull")}, produces FOUR panels -
#' Canopy, Microphone Height, Vertical Microphone Orientation
#' (\code{"mic.bull"} - see "mic.bull panel" below), and Understory.
#' \code{sub.plots} entries may repeat (e.g. \code{c("canopy.bull",
#' "canopy.bull")} draws that panel twice); an unrecognized entry stops
#' with an error listing the allowed set (\code{"canopy.bull"},
#' \code{"mic.bar"}, \code{"mic.bull"}, \code{"understory.bull"}).
#'
#' The three logical toggles - \code{cover.arrows}, \code{mean.cover},
#' \code{quadrant.cover} (all default \code{TRUE}, i.e. unchanged from every
#' earlier round's fixed behavior) - each independently gate ONE of
#' \code{build.panel()}'s optional layers (the quadrant wedges'
#' \code{geom_col()}, the dashed mean-of-quadrants circle's
#' \code{geom_hline()}, and the Horizontal Microphone Orientation arrow's
#' \code{geom_segment()}, respectively) on the two quadrant-based bullseye
#' panels - \code{"canopy.bull"} and \code{"understory.bull"} - per Josh's
#' own wording scoping each toggle to "canopy.bull, and/or understory.bull";
#' per Josh, "If mean cover = TRUE then plot the mean cover for canopy
#' and/or understory" (etc.) reads as one shared switch across whichever of
#' those two panels are being drawn, not a separate per-panel toggle -
#' \strong{a real assumption, please confirm} - since a per-panel on/off
#' (e.g. arrows on Canopy but not Understory) would need different
#' parameters entirely. None of the three toggles affect \code{"mic.bar"}
#' or \code{"mic.bull"} (neither has quadrants, a mean circle, or an
#' optional orientation arrow to begin with - \code{"mic.bull"}'s own
#' arrow is a required, always-drawn part of that panel, see "mic.bull
#' panel" below) - and neither the scale-line rings nor the radial numeric
#' labels on \code{"canopy.bull"}/\code{"understory.bull"} are gated by
#' any of these three switches; both remain unconditionally drawn last,
#' preserving the existing "Quadrants > mean cover > mic direction > scale
#' lines > labels" draw order among whichever of the first three layers
#' are actually turned on.
#'
#' \strong{Figure sizing - \code{auto.scale}/\code{subplot.size.height}/
#' \code{subplot.size.width} added, per Josh's fourteenth follow-up
#' (2026-09-15), REPLACING \code{$plot.width} entirely.} Previously the
#' combined figure's saved width/height came from the \code{aes.default}
#' settings \code{$plot.width} (divided evenly across however many
#' \code{sub.plots} panels were drawn) and \code{$plot.height}. Per Josh's
#' explicit sizing request, \code{$plot.width} is no longer read at all
#' (in either mode below); \code{$plot.height} is still read, but only
#' when \code{auto.scale = TRUE}. With \code{auto.scale = TRUE} (the
#' default), the saved figure's total width is fixed at \code{6.5} inches
#' - per Josh, "the total plot size must be 6.5 inches wide" - divided
#' evenly across the actual \code{sub.plots} panel count (same
#' even-division mechanism as before, just against a fixed total instead
#' of a settings value); height still comes from \code{$plot.height}.
#' With \code{auto.scale = FALSE}, \code{subplot.size.width} (default
#' \code{2}) and \code{subplot.size.height} (default \code{3}) set each
#' INDIVIDUAL panel's size directly instead - the saved figure's total
#' width becomes \code{subplot.size.width * length(sub.plots)} and its
#' height becomes \code{subplot.size.height} (one height for the whole
#' row, since every panel shares it) - \code{$plot.height} is NOT
#' consulted in this branch. \strong{A real assumption, please confirm:}
#' Josh's request only specified a fixed total WIDTH for the
#' \code{auto.scale = TRUE} case ("the total plot size must be 6.5 inches
#' wide") and said nothing about height there, so height keeps using the
#' pre-existing \code{$plot.height} setting for that case only - it was
#' not obviously part of what he meant to change. \code{$plot.width}'s
#' settings-sheet row (still present in \code{plotopts_bullseye.csv} for
#' backward compatibility) is now unused by the function in every mode -
#' flagged rather than silently left ambiguous; Josh may want to remove
#' that row or repurpose it later. \code{ggsave.width.pad}/
#' \code{ggsave.height.pad} are added on top of whichever width/height
#' this section computes, unchanged from every earlier round.
#' \strong{A real, visible cosmetic consequence, flagged rather than
#' silently worked around:} both new defaults are considerably narrower
#' per panel than the old \code{$plot.width} default (10in / up to 4
#' panels = 2.5in/panel) - the default \code{auto.scale = TRUE} (6.5in / 4
#' panels = 1.625in/panel) and even \code{auto.scale = FALSE}'s own
#' default (\code{subplot.size.width = 2}in/panel) both made the default
#' 4-panel figure's panel TITLES visibly overlap each other (not just the
#' already-flagged single-label edge-clipping from earlier rounds) -
#' confirmed by rendering the real 4-panel figure under both new
#' defaults. \strong{Resolved by the fifteenth follow-up immediately
#' below}, which auto-expands the width in exactly this situation instead
#' of leaving it as a standing trade-off for Josh to decide on.
#'
#' \strong{Auto-expand when all four panel types are selected, per Josh's
#' fifteenth follow-up (2026-09-15): "if user selects all four options
#' sub.plots then expand the size of the plot by the required amount to
#' prevent overlap."} Directly resolves the title-overlap consequence
#' flagged in the "Figure sizing" section just above. Scoped exactly to
#' Josh's wording - triggers only when \code{sub.plots} contains ALL FOUR
#' panel types (\code{all(c("canopy.bull", "mic.bar", "mic.bull",
#' "understory.bull") \%in\% sub.plots)} - true regardless of their order or
#' whether any type repeats; a 2- or 3-panel subset, even a long one via
#' repeats, is NOT affected, since Josh's wording specifically named the
#' all-four case). When it triggers, \code{$plot.title.size} and each of
#' the four fixed panel titles (\code{$canopy.title}, \code{$mic.panel.title},
#' \code{$mic.bull.title}, \code{$understory.title}) are measured for
#' their REAL rendered text width in inches (via
#' \code{grid::textGrob()}/\code{grid::grobWidth()}/
#' \code{grid::convertWidth()}, on a throwaway null \code{pdf(NULL)}
#' device opened and closed around just that measurement, so nothing is
#' actually drawn or shown) - not guessed or hardcoded, since the actual
#' overlap depends on the real title text/font size in
#' \code{aes.default}, which is exactly what the earlier flagged
#' consequence showed (the longest title, \code{$mic.panel.title}'s
#' default "Microphone Height and Vertical Orientation", drives the whole
#' figure's minimum width). The widest of the four titles, plus a small
#' \code{0.25}in margin (\code{TITLE.OVERLAP.MARGIN.IN}) so text isn't
#' touching the exact panel edge, becomes the minimum width EVERY column
#' must have (since \code{patchwork::wrap_plots()}'s
#' \code{widths = rep(1, length(sub.plots))} always gives every column
#' the same width, a fix sized for the worst-offending title covers every
#' other column too); multiplied by \code{length(sub.plots)}, this becomes
#' the minimum acceptable total figure width. This is compared against
#' whatever \code{auto.scale}/\code{subplot.size.width} already computed
#' (see "Figure sizing" above) - the figure is WIDENED to the minimum only
#' when that's larger than what was already going to be used; it is NEVER
#' narrowed (so a generous \code{subplot.size.width} that already gives
#' enough room is left exactly as the caller set it). On the real test
#' ARU's default \code{aes.default}, this expands the default 4-panel
#' figure's total width to about \code{13.7}in (about \code{3.43}in per
#' panel) regardless of whether \code{auto.scale} is \code{TRUE} (its own
#' 6.5in default) or \code{FALSE} (\code{subplot.size.width = 2}in's own
#' 8in default) - both confirmed, by rendering the real combined figure
#' under each, to eliminate the previously-flagged title overlap
#' entirely. Height is untouched by this section (the overlap was purely
#' horizontal, from panel titles bleeding sideways into their neighbors'
#' space - there was never a vertical/height component to it).
#'
#' \strong{mic.bull panel, per Josh's seventh follow-up (2026-09-15),
#' replacing the placeholder (an exact alias of \code{"understory.bull"})
#' shipped in the sixth round; repositioned per his eighth follow-up;
#' degrees/labels finalized and a white label background added per his
#' ninth follow-up; rotation corrected again per his tenth follow-up
#' (2026-09-15, later still: "Top = 90, right 0, Bottom = 270, left =
#' 180"); the height label's text shortened over his eleventh and twelfth
#' follow-ups; the scale-number labels moved onto the exact left
#' horizontal axis per his thirteenth follow-up; its panel title replaced
#' with a two-line, shared-with-mic.bar title per his sixteenth
#' follow-up.} A polar "bullseye"-style
#' plot of the Vertical Microphone
#' Orientation, using the same 0-360 angle convention already used for
#' \code{vertical_microphone_orientation} elsewhere in this function for
#' its arrow's data angle (0 = parallel to ground/"level", 90 = straight
#' up, 270 = straight down - see "Vertical Microphone Orientation icon
#' set" above). This panel's own
#' \code{coord_polar(theta = "x", start = -pi / 2, direction = -1)} -
#' unlike every other panel's \code{start = 0, direction = 1} - is the
#' mapping that produces the tenth follow-up's exact screen layout
#' (verified with a live rendered test, not derived by inspection alone).
#' Angle 90 ("straight up") renders at the TOP and carries
#' \code{$mic.bull.skyward.label} (default \code{"Skyward"}); angle 270
#' ("straight down") renders at the BOTTOM and carries
#' \code{$mic.bull.earthward.label} (\code{"Earthward"}), per the tenth
#' follow-up ("Earthward label at bottom / Skyward label at top"). Angle
#' 0 (E) renders at the RIGHT and stays blank, per Josh's original "No
#' label for E or W"; angle 180 (W) renders at the LEFT and carries
#' \code{$mic.bull.height.label} (default \code{"(m)"}, shortened from
#' "Microphone Height (m)" via "Height (m)" over the eleventh and twelfth
#' follow-ups), per the tenth follow-up ("Microphone Height (m) on
#' left"). A single arrow shows the Vertical Microphone Orientation
#' itself: angle = that ARU's \code{vertical_microphone_orientation}, length = its
#' \code{microphone_height} - "the tip of the arrow ending at the total
#' height of the mic", per Josh - drawn via the same
#' \code{geom_segment()}/\code{grid::arrow()} approach, reusing
#' \code{$arrow.color}/\code{$arrow.linewidth}/\code{$arrow.head.cm}. This
#' arrow is always drawn (not gated by \code{cover.arrows} - see "Sub-plot
#' selection and toggles" above). Radial scale lines default to
#' \code{$mic.bull.radial.breaks} (\code{"1;2;3;4"}, i.e. meters of mic
#' height, reusing \code{$scale.line.color}/\code{$scale.line.linetype}/
#' \code{$scale.line.linewidth}), with the plot area extending to
#' \code{$mic.bull.radial.max} (default \code{4}) - auto-stretched taller
#' when the actual \code{microphone_height} (plus a small margin so the
#' arrowhead never touches the outer edge) needs more room, the same
#' pattern \code{build.mic.panel()}'s bar/icon Y axis already uses. Per
#' the thirteenth follow-up ("Make the labels for the scale occur along
#' the left horizonal axis, offset the label by enough space to fit the
#' label"), the 1/2/3/4 numeric scale labels are drawn exactly on angle
#' 180 (W) - the same ray as \code{$mic.bull.height.label} - with
#' clearance provided RADIALLY instead of angularly: the plot's Y-axis
#' extends \code{0.7} past \code{$mic.bull.radial.max} so the height-label
#' axis text (which renders right at that boundary) has room. This
#' replaces the tenth follow-up's fix for the same underlying collision -
#' back then, with both labels anchored at the exact same point (the plot
#' boundary), the numbers were nudged 15 degrees off that ray instead
#' (angle 165); that worked, but per this follow-up's explicit request
#' for the exact left horizontal axis, the offset is now radial, not
#' angular. These labels are drawn with \code{geom_label()} (white fill,
#' no border), per the ninth follow-up ("add a white background to make
#' the label clearer"), instead of the plain \code{geom_text()} used
#' everywhere else - scoped ONLY to this
#' panel; \code{build.panel()}'s own radial labels on
#' \code{canopy.bull}/\code{understory.bull} remain background-less, per
#' the deliberate choice described in "Label styling and draw order"
#' below. Panel title \code{$mic.bull.title} (default \code{"Microphone
#' Height and"} / \code{"Vertical Orientation"} on two lines, per Josh's
#' sixteenth follow-up (2026-09-15) - "use that as the title for mic.bull
#' plot as well" - now IDENTICAL text to \code{$mic.panel.title} (the
#' mic.bar panel's own title), replacing this panel's previously distinct
#' "Microphone Vertical Orientation" text set in the seventh follow-up;
#' the sixth round's original placeholder default, "Understory Cover", no
#' longer applies either way now that this panel has its own real
#' design). See "Panel title wrapping" below for the two-line mechanism
#' itself.
#'
#' \strong{Panel title wrapping, per Josh's sixteenth follow-up
#' (2026-09-15): "make [the] title occur over two lines instead of one."}
#' \code{$mic.panel.title}/\code{$mic.bull.title}'s shared default text
#' now has a literal embedded line break (an actual newline character
#' inside the \code{aes.default} CSV's quoted field, round-tripped
#' correctly by \code{read.csv()}/\code{write.csv()} - confirmed
#' empirically) splitting it into "Microphone Height and" / "Vertical
#' Orientation" instead of one long line. This needed NO code change:
#' \code{ggplot2}'s \code{plot.title} element already renders an embedded
#' \code{"\\n"} as a real second line, the same way \code{geom_text()}/
#' \code{geom_label()} do elsewhere in this function. A useful side
#' effect, re-confirmed by testing: the "Figure sizing" auto-expand step
#' (see @details, "Figure sizing" and "Auto-expand when all four panel
#' types are selected") measures a title's rendered width via
#' \code{grid::grobWidth()} on a (possibly multi-line) \code{textGrob()},
#' which correctly returns the width of the WIDEST LINE, not the full
#' string's length - so wrapping this title onto two shorter lines
#' substantially reduces the minimum width that step computes (from
#' about 13.7in down to a bit over 8in for the real default 4-panel
#' figure, re-verified by rendering it) rather than requiring any change
#' to that step's own logic.
#'
#' \strong{mic.bar panel overhaul, per Josh's twentieth follow-up
#' (2026-09-16), six changes scoped to \code{"mic.bar"} only:}
#' \enumerate{
#'   \item \strong{Bar width x1.3} ("Make the bar 1.3 its current size"):
#'     \code{$mic.bar.width} is now multiplied by a fixed \code{1.3}
#'     (\code{MIC.BAR.WIDTH.SCALE} inside \code{build.mic.panel()}) rather
#'     than baking the multiplier into the CSV default, so an
#'     \code{$aes.style}/\code{$overide.value} override to
#'     \code{$mic.bar.width} still scales proportionally.
#'   \item \strong{Y-axis reference lines every 0.5 units} ("with a line
#'     every 0.5 interval"): breaks (and their matching horizontal
#'     gridlines) are now \code{seq(0, y.max, by = 0.5)} instead of one
#'     per whole unit; \code{panel.grid.major.y} is drawn (reusing
#'     \code{$panel.grid.color} for visual consistency with the angular
#'     gridlines on the other panels) instead of being fully blanked -
#'     \code{panel.grid.major.x}/\code{panel.grid.minor} stay blank (no
#'     X-axis lines, no unlabeled sub-0.5 lines).
#'   \item \strong{Font size matched to the other panels} ("reduce the
#'     font size to be the same size as the other two plots"): a real,
#'     previously-unnoticed gap - \code{axis.title.y} (the "Microphone
#'     Height (m)" label) had no explicit \code{size} set at all, so it
#'     fell back to \code{theme_minimal()}'s own default rather than the
#'     derived \code{axis.text.size} every other element on this panel
#'     (and its siblings) already uses. Now explicitly pinned to
#'     \code{axis.text.size}.
#'   \item \strong{Icon re-centered on the bar's top edge} ("move the mic
#'     png to be centered at the top of the bar") - \strong{a deliberate
#'     reversion, flagged explicitly.} This undoes the CENTERED-vs-ABOVE
#'     half of the fifth follow-up's bugfix (see "Icon file format and
#'     placement" above): \code{ymin}/\code{ymax} are back to
#'     \code{mic.height -/+ icon.height.m / 2} (was entirely above:
#'     \code{mic.height} to \code{mic.height + icon.height.m}). The
#'     original complaint that motivated drawing it entirely above was an
#'     OPAQUE icon painting over the bar's true height; the icon set has
#'     used real per-pixel transparency since the very next follow-up
#'     that same day, so the original failure mode mostly shouldn't recur
#'     - but the icon's own drawn (non-transparent) content can still
#'     visually sit on the bar's top edge again. If this reads as the
#'     original problem once rendered, this is the section to revert.
#'     Y-axis headroom above \code{mic.height} for the icon was reduced to
#'     match (\code{icon.height.m / 2} instead of a full
#'     \code{icon.height.m}).
#'   \item \strong{Column width = 2/3 of the other panels} ("scale the
#'     size of the mic.bar plot to be 2/3 the size of the canopy plots"):
#'     \code{patchwork::wrap_plots()}'s \code{widths=} argument is no
#'     longer a flat \code{rep(1, length(sub.plots))} - \code{"mic.bar"}'s
#'     own column now gets a relative weight of \code{2/3} against every
#'     other panel type's \code{1}. Combined with \code{aspect.ratio = 1}
#'     (see "Panel height matching" above), a narrower column renders as
#'     a proportionally smaller SQUARE panel - "2/3 the size" directly.
#'     \strong{Judgment call, flagged:} the "Figure sizing"/"Title
#'     auto-shrink" sections above still assume every column gets an
#'     EQUAL share of \code{plot.width} when deciding whether titles need
#'     to shrink - i.e. \code{mic.bar}'s own title is measured against
#'     that same equal share (matching item 3's "same font size as the
#'     other two plots"), not against its actual, now-narrower rendered
#'     column. This was left as a uniform calculation rather than
#'     generalized to per-panel actual widths, since it wasn't asked for -
#'     worth a second look if a different title/width combination ever
#'     looks tight (the item-6 title change below already forces a
#'     shrink/expand for an unrelated reason, so this simplification's own
#'     effect on fit hasn't yet been isolated in practice).
#'   \item \strong{Title changed} ("Change the title to be 'Microphone
#'     Vertical Orientation'"): \code{$mic.panel.title}'s default value in
#'     \code{plotopts_bullseye.csv} changed from the two-line
#'     "Microphone Height and\\nVertical Orientation" (shared with
#'     \code{$mic.bull.title} since the sixteenth follow-up) to the
#'     single-line "Microphone Vertical Orientation" - a settings-data
#'     change only, no code change. \strong{This un-syncs \code{"mic.bar"}'s
#'     title from \code{"mic.bull"}'s} (\code{$mic.bull.title} is
#'     untouched and keeps the two-line shared text) - scoped this way
#'     because Josh's request named \code{"mic.bar"} specifically, not
#'     both panels; flagged in case the sixteenth follow-up's "share the
#'     same title" intent was meant to still apply going forward.
#'     \strong{Side effect, confirmed via the dev-script test suite:} the
#'     new single-line text measures WIDER unwrapped (at the full 12pt
#'     \code{$plot.title.size} default) than either individual line of the
#'     old two-line wrapped text did - so the "Title auto-shrink" mechanism
#'     (see "Figure sizing" above), which previously left the default
#'     4-panel \code{auto.scale = TRUE} figure untouched at a plain 6.5in,
#'     now floors ALL panel titles at 8pt AND still has to widen the total
#'     figure to about 7.21in to avoid overlap. This is a direct, expected
#'     consequence of shortening the text to one line rather than a bug -
#'     but it does mean the default combined figure is visibly wider and
#'     smaller-titled than it was before this round.
#' }
#' Rendered and visually verified at \code{vertical_microphone_orientation}
#' = 90, 45, 0, 315, and 270 (the same five angles with a known icon) on
#' the real test ARU (\code{microphone_height} = 3m) - see this function's
#' delivered round-twenty example renders.
#'
#' \strong{Missing quadrants.} An \code{aru.label} present in \code{mic}
#' but missing one or more of the four required quadrants in \code{data}
#' is skipped (with a \code{warning()} naming which quadrant(s) are
#' missing) rather than stopping the whole call - so one bad/incomplete
#' ARU doesn't block plotting the rest.
#'
#' \strong{Tested against Josh's real uploaded model data}
#' (\code{bulleye_model_data.xlsx}, one ARU: \code{Canopy cover} = 63/13/13/100
#' and \code{Understory cover} = 63/13/13/0 for NE/SE/SW/NW, Horizontal
#' Microphone Orientation 192 degrees, Vertical Microphone Orientation 45
#' degrees, Microphone Height 3 meters) - rendered and visually verified
#' after each follow-up round, most recently: correct quadrant
#' placement/sizing, correctly-sized dashed mean circles (~47 for Canopy,
#' ~22 for Understory), an arrow pointing just past South toward West
#' (matching 192 degrees), a clean 0-110 radial scale with no phantom
#' padding or diagonal gridlines, solid black scale-line rings drawn in
#' the requested order (on top of the quadrant wedges/mean circle/arrow,
#' underneath the labels), the 25/50/75/100 labels reading cleanly (no
#' background box) at both the North and South ends of the vertical line
#' on both panels, a plain opaque white saved PNG background (confirmed
#' by reading the file's own pixel data back, not just by eye), and the
#' middle panel correctly drawing a bar to height 3 (m) on a 0-4 axis
#' with the 45-degree icon (\code{mic_vert_045.png}, matching this ARU's
#' real Vertical Microphone Orientation of 45 degrees) sitting flush and
#' undistorted on the bar's top - most recently re-verified that the bar
#' visibly reaches all the way to the 3m gridline with nothing painted
#' over it, per Josh's fifth follow-up bugfix above. Per Josh's sixth
#' follow-up (2026-09-15), also re-verified: the default \code{sub.plots}
#' renders all FOUR panels, left to right; a 2-panel \code{sub.plots =
#' c("canopy.bull", "understory.bull")} subset renders correctly with no
#' gap or leftover mic panel; each of \code{cover.arrows}/\code{mean.cover}/
#' \code{quadrant.cover} individually, and all three together, correctly
#' hides only its own layer on \code{canopy.bull}/\code{understory.bull}
#' while leaving the scale lines/radial labels/mic panels untouched; and an
#' unrecognized \code{sub.plots} value, or a non-\code{TRUE}/\code{FALSE}
#' toggle value, both stop with a clear error naming the offending value.
#' Per Josh's seventh follow-up (2026-09-15, same day), the real
#' \code{"mic.bull"} panel was also re-verified against the same test ARU
#' (Vertical Microphone Orientation 45 degrees, Microphone Height 3
#' meters): an arrow from center to radius 3 at angle 45, the 1/2/3/4
#' scale-line rings all inside a 0-4 plot area (not auto-stretched, since
#' 3 < 4), and no text at the angle-0/180 positions - plus a taller
#' synthetic 5m-mic test confirming the radial max auto-stretches so the
#' arrow tip stays inside the plot area. Per Josh's eighth follow-up
#' (2026-09-15, later the same day, repositioning "Sky ward"/"Earthward"
#' to top/bottom and the numeric scale to the left side only), re-rendered
#' test plots at all 5 known Vertical Microphone Orientation angles (0,
#' 45, 90, 270, 315 - matching the 5 icon images used by \code{"mic.bar"})
#' confirmed: "Sky ward" reads at the top and "Earthward" at the bottom in
#' every case; the 1/2/3/4 labels read only on the left, never duplicated
#' on the right; the arrow at 90 degrees points straight up (to "Sky
#' ward"), at 270 straight down (to "Earthward"), at 0 straight left, at
#' 45 up-and-left, and at 315 down-and-left - all consistent with that
#' round's quarter-turn rotation. Per Josh's ninth follow-up (2026-09-15,
#' later still: reverting to the standard \code{start = 0} mapping used
#' by every other panel, adding \code{$mic.bull.height.label} at the W/
#' bottom position, a white background on the scale numbers, and the
#' "Skyward" one-word spelling), the same 5 angles were re-rendered again
#' and confirmed: "Skyward" read at the RIGHT and "Earthward" at the LEFT
#' in every case (the mirror image of the eighth follow-up's top/bottom
#' placement); "Microphone Height (m)" read at the BOTTOM; the 1/2/3/4
#' labels read at the TOP, each on a visible white background; and the
#' arrow at 90 degrees pointed right (to "Skyward"), at 270 left (to
#' "Earthward"), at 0 straight up, at 45 up-and-right, and at 315
#' down-and-right. Per Josh's tenth follow-up (2026-09-15, later still:
#' "Top = 90, right 0, Bottom = 270, left = 180 / Earthward label at
#' bottom / Skyward label at top / Microphone Height (m) on left / Scale
#' labels on left"), the rotation was corrected once more
#' (\code{start = -pi / 2, direction = -1}) and the same 5 angles
#' re-rendered a third time confirmed: "Skyward" read at the TOP and
#' "Earthward" at the BOTTOM in every case; "Microphone Height (m)" read
#' at the LEFT; the 1/2/3/4 labels read near the left (offset slightly
#' toward the top to avoid overlapping the height label - a real
#' text-collision bug found and fixed that round, see "mic.bull panel"
#' above), each on a visible white background; and the arrow at 90
#' degrees pointed straight up (to "Skyward"), at 270 straight down (to
#' "Earthward"), at 0 straight right, at 45 up-and-right, and at 315
#' down-and-right. Per Josh's eleventh follow-up (2026-09-15, later
#' still: "orientation is correct / Change 'Microphone Height (m)' to
#' 'Height (m)'"), confirming the tenth follow-up's rotation as final,
#' \code{$mic.bull.height.label}'s default was shortened to
#' \code{"Height (m)"} - a text-only change (\code{plotopts_bullseye.csv}/
#' \code{.dev.R} test data, no code change), re-verified with the same 5
#' angles re-rendered once more: "Height (m)" read at the LEFT in every
#' case, shorter and with less risk of the panel-width clipping noted
#' above; the 1/2/3/4 labels remained clear of it. Per Josh's twelfth
#' follow-up (2026-09-15, later still: "Change left label '(m)'"),
#' \code{$mic.bull.height.label}'s default was shortened once more, to
#' just \code{"(m)"} - again a text-only change, re-verified with the
#' same 5 angles re-rendered again: "(m)" read at the LEFT in every case,
#' essentially eliminating the panel-width clipping (only 2 characters
#' plus the parentheses); the 1/2/3/4 labels' 15-degree offset remained
#' clear of it, with even more margin than before. Per Josh's thirteenth
#' follow-up (2026-09-15, later still: "Make the labels for the scale
#' occur along the left horizonal axis, offset the label by enough space
#' to fit the label"), the 1/2/3/4 labels were moved off their 15-degree
#' offset (angle 165) back onto the exact left horizontal axis (angle
#' 180, the same ray as \code{$mic.bull.height.label}), with clearance now
#' provided by extending the Y-axis \code{0.7} past
#' \code{$mic.bull.radial.max} rather than by an angular offset,
#' re-verified with the same 5 angles re-rendered a final time and the
#' real combined 4-panel figure: the 1/2/3/4 labels read exactly along
#' the horizontal left line in every case, clearly separated from
#' "(m)" (no text-on-text overlap), and the rings/arrow directions are
#' otherwise unchanged. Per Josh's fourteenth follow-up (2026-09-15,
#' later still: order-matching re-verification plus \code{auto.scale}/
#' \code{subplot.size.height}/\code{subplot.size.width}), a new panel-order
#' test (comparing the actual rendered panel titles, not just their count)
#' confirmed \code{sub.plots} order was already correctly reflected in the
#' combined figure and remains so; the new sizing arguments were verified
#' by reading each saved PNG's own real pixel dimensions back
#' (\code{png::readPNG()}) against the expected inches x dpi, for both
#' \code{auto.scale = TRUE} (fixed 6.5in total width at 2 and 4 panels)
#' and \code{auto.scale = FALSE} (a non-default
#' \code{subplot.size.width}/\code{subplot.size.height} combination) -
#' all matched to within rounding. Also confirmed (see @details, "Figure
#' sizing"): both new sizing defaults produce visibly overlapping panel
#' titles in the default 4-panel figure, a real cosmetic consequence
#' flagged for Josh's review rather than silently patched. Per Josh's
#' fifteenth follow-up (2026-09-15, later still: "if user selects all
#' four options sub.plots then expand the size of the plot by the
#' required amount to prevent overlap"), this was resolved: a new test
#' independently re-computes the expected minimum width from the real
#' title text/font size (the same measurement approach the function
#' itself uses) and confirms the actual saved figure matches it exactly,
#' for all four types under both \code{auto.scale = TRUE} and
#' \code{auto.scale = FALSE}; a separate test confirms a subset that is
#' NOT all four types is left at its plain, un-expanded width. The real
#' combined 4-panel figure was also re-rendered under both
#' \code{auto.scale} settings and visually confirmed clean - all four
#' panel titles now read with clear separation, no overlap, at the
#' auto-expanded ~13.7in total width. Per Josh's sixteenth follow-up
#' (2026-09-15, later still: two-line panel title, shared with mic.bull,
#' and removing the mic.bar panel's X-axis title), re-verified: the real
#' combined figure now shows "Microphone Height and" / "Vertical
#' Orientation" on two lines on BOTH the mic.bar and mic.bull panels, no
#' "Ground Level" text anywhere on the mic.bar panel's X axis, and (a
#' useful side effect of the shorter wrapped lines) the "Figure sizing"
#' auto-expand step now only needs to widen the default 4-panel figure to
#' about 8.4in instead of the fifteenth round's ~13.7in - all 16 existing
#' tests still pass (Test 16's own expected-width values recomputed to
#' match the new, shorter title text), and the real combined figure was
#' visually re-inspected close up to confirm the two adjacent two-line
#' titles (mic.bar's and mic.bull's) don't touch or overlap each other.
#'
#' @examples
#' \dontrun{
#' aes.default <- read.csv("plotopts_bullseye.csv", stringsAsFactors = FALSE)
#' cover <- read.csv("cover.csv", stringsAsFactors = FALSE, check.names = FALSE)
#' mic <- read.csv("mic.csv", stringsAsFactors = FALSE, check.names = FALSE)
#' batz.plotcover_bullseye(cover, mic, aes.default, dir.save = getwd())
#' }
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
  if (length(problems) > 0) stop(paste(problems, collapse = "\n"))

  ## Settings resolution (round nineteen, per Josh's 2026-09-16 follow-up):
  ## $aes.style names a FIXED column to check first (default
  ## "overide.value" - a blank column the user fills in directly on their
  ## own copy of the CSV), falling back to $default.value when that column
  ## doesn't exist or is blank for this row. This replaces the old
  ## project.name-matches-a-column-name mechanism entirely - project.name
  ## no longer has any role in settings resolution, only in the saved file
  ## name (see the main loop below).
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
  ## vertical_microphone_orientation value is matched to the NEAREST of
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
    if (cover.arrows) {
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
  ## scale occur along the left horizonal axis, offset the label by
  ## enough space to fit the label"), the numeric scale labels are drawn
  ## exactly on angle 180 (W) - the same ray as $mic.bull.height.label
  ## (default "(m)", shortened from "Microphone Height (m)" via "Height
  ## (m)" over the eleventh and twelfth follow-ups) - with clearance
  ## between them provided RADIALLY instead: the plot's axis extends a bit
  ## past $mic.bull.radial.max so the height-label axis text (which
  ## renders right at that boundary) has room, rather than sitting on top
  ## of the outermost ring/number as it did (a real bug, fixed in the
  ## tenth follow-up by angularly offsetting the numbers instead - now
  ## reverted per this follow-up's explicit "left horizonal axis"
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
    ## for the scale occur along the left horizonal axis"). Earlier
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
  ## grey bar rises from 0 to $microphone_height; the matching icon (see
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
    ## only stretched taller when the actual microphone_height plus the
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
      ## microphone_height. That was fixed by drawing the icon entirely
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
      icon.img <- png::readPNG(icon.file)
      ## dim() is (height.px, width.px, channels) - used to keep the icon
      ## undistorted (see function-level comment above).
      icon.aspect <- dim(icon.img)[2] / dim(icon.img)[1]
      icon.width.x <- icon.aspect * icon.height.m * (x.range / y.max)
      p <- p + ggplot2::annotation_custom(
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
    panel.builders <- list(
      canopy.bull = function(title.size, axis.size) build.panel(canopy.values, get.default("canopy.title"), horiz.orient, title.size, axis.size),
      understory.bull = function(title.size, axis.size) build.panel(understory.values, get.default("understory.title"), horiz.orient, title.size, axis.size),
      mic.bull = function(title.size, axis.size) build.mic.bullseye(mic.height, vert.orient, title.size, axis.size),
      mic.bar = function(title.size, axis.size) build.mic.panel(mic.height, vert.orient, title.size, axis.size)
    )
    panels <- lapply(sub.plots, function(nm) panel.builders[[nm]](resolved.title.size, resolved.axis.text.size))

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
    ## (round nineteen)").
    fname <- sprintf("%s_%s_%s.png", project.name, aru, format(Sys.time(), "%Y%m%d_%H%M%S"))
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

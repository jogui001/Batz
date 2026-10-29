#' Generate standard ARU site appendices (Word/PDF) from Survey123 deployment and servicing forms
#'
#' Takes the Survey123 deployment (habitat), quadrant and servicing forms for
#' a project and writes one standard site report ("appendix") per ARU site,
#' laid out like Josh's \code{Model ARU Appendix by Identifier.docx}: project
#' and detector information, servicing events, recovery, habitat assessment,
#' ARU photos (Figures 1-3), the canopy/understory bullseye plot (Figure 4)
#' and the SM4 activity heatmap (Figure 5). (Stage 1, 2026-10-27, had the
#' first four sections only.)
#'
#' @param data Either the path to the Survey123 export workbook (\code{.xlsx},
#'   one tab per form, e.g. \code{FY26_SevenIslands_NABat_Aru_DeploymentService.xlsx})
#'   or a named list of data frames, one per form. The three forms are found
#'   by tab name - \code{deploy} (also \code{deployment}, \code{habitat}),
#'   \code{quad} (also \code{quadrant}) and \code{service} (also
#'   \code{survice}, \code{servicing}) - or, failing that, by which tab's
#'   headers best match each form's Survey123 headers. Other tabs (e.g.
#'   \code{repeat_photo_1}) are ignored.
#' @param client,client.contact,project.label,project.code,project.manager
#'   Character, default \code{""}. Used for the report header only when the
#'   data has no column of that name (Batz spelling, any case/separator) with
#'   a value for that site.
#' @param project.name Character, default \code{""}. Keeps only records whose
#'   \code{$project.name} (Survey123 "Project") matches, ignoring case and
#'   spaces. Blank uses every record, which is only allowed when the data
#'   holds one project. Also the \code{<PROJECTNAME>} part of the file names
#'   (the data's own project name when blank).
#' @param word Logical, default \code{TRUE}. Write Word (\code{.docx}) files.
#' @param pdf Logical, default \code{FALSE}. Write PDF files (converted from
#'   the Word file - see Details). At least one of \code{word}/\code{pdf}
#'   must be \code{TRUE}.
#' @param collate Logical, default \code{TRUE}. One file with every site,
#'   each starting on a new page:
#'   \code{<PROJECTNAME>_Siteappendixall_<timestamp>}.
#' @param separate Logical, default \code{TRUE}. One file per site:
#'   \code{<PROJECTNAME>_<aru.name>_<timestamp>}.
#' @param dir.save Character, default \code{getwd()}. Where files are written.
#' @param time.zone Character, default \code{"America/New_York"}. Survey123
#'   stores date-times in UTC; they are shown in this time zone. Use
#'   \code{"UTC"} to show them as stored.
#' @param continue.missing Logical or \code{NULL} (default). What to do when
#'   one or two of the three forms are missing: \code{NULL} asks
#'   (\code{Y/N}) in an interactive session and stops otherwise;
#'   \code{TRUE} continues; \code{FALSE} stops.
#' @param template Character, default \code{NULL} (= \code{"terrestrial"}).
#'   Either the name of a built-in template - \code{"terrestrial"} uses
#'   \code{inst/templates/batz.aru.appendix.terrestrial.template.docx}, the
#'   only one so far - or the path to a Word file with
#'   \code{\{\{placeholder\}\}} fields (see Details).
#' @param show.time Logical, default \code{FALSE}. Added 2026-10-27. Date
#'   fields show the date only (\code{7/19/2026}); \code{TRUE} adds the time
#'   (\code{7/19/2026 18:51}).
#' @param equipment.inventory Data frame or the path to the equipment
#'   database (\code{.xlsx}/\code{.csv}), default \code{NULL}. Added
#'   2026-10-27. An equipment list with a serial-number column (\code{Serial},
#'   \code{Serial Number}, \code{Serial No} or \code{SN}) and, optionally,
#'   \code{Make} (or \code{Manufacturer}), \code{Model} and an equipment-type
#'   column (\code{Type}, \code{Equipment Type}, \code{Equipment},
#'   \code{Category}). Headers are matched ignoring case and punctuation and a
#'   leading \code{batz.equipment_} / \code{equipment_}, so Josh's
#'   \code{survey123 equipment database.xlsx} (\code{batz.equipment_type},
#'   \code{_make}, \code{_model}, \code{_serial}; \code{_QRcode} and
#'   \code{_notes} are not used yet) works as is. When
#'   given: (1) a blank make or model in the report is filled from the
#'   inventory row with the same serial number; (2) every value in a field
#'   whose Batz name contains \code{serial} (deployment and service forms) is
#'   checked - a serial not in the inventory, or listed there as a different
#'   kind of equipment (ARU / microphone / power / HOBO), is reported in
#'   \code{$serial.check}; (3) a serial listed under more than one equipment
#'   type in the inventory is reported in \code{$serial.check} (form
#'   \code{"equipment.inventory"}), and one listed twice under the same type
#'   is named in a NOTE; (4) a serial whose \code{working.status} (or
#'   \code{status}) is not \code{Working} - e.g. \code{Retired},
#'   \code{Out of Commission}, \code{Missing} - is reported in
#'   \code{$serial.check}.
#' @param photo.dir Character, default \code{NULL}. Added 2026-10-28. The
#'   folder of ARU photos (e.g. \code{.../NABAT Test Data/ARU Photos}), searched
#'   with its sub-folders; folders named \code{Archive} are skipped. A photo
#'   belongs to a site when its folder is named after the site
#'   (\code{107802-NW1/ATT57_107802-_North_....HEIC}) or one \code{_}-separated
#'   part of its file name is the site name (\code{ATT123_5175-SE1_North_...}),
#'   ignoring case and punctuation. The view is read from the file name:
#'   \code{Detector_Setup}, \code{Microphone_View}, \code{North},
#'   \code{East}, \code{South}, \code{West}. Files with a \code{New} part
#'   (\code{..._55-NW1_New_Detector_Setup_...}) are used only when there is no
#'   other photo of that view. \code{NULL}: the photo cells read
#'   \code{"No photo folder given (photo.dir)"}.
#' @param photo.width Numeric, default \code{3}. Added 2026-10-28. Photo width
#'   in inches (height kept in proportion, at most 3.5 in).
#' @param sm4.data Data frame or the path to a merged SM4 log \code{.csv}
#'   (e.g. \code{mergedsm4.csv}), default \code{NULL}. Added 2026-10-28. Needs
#'   \code{aru.name}, \code{date} and \code{time} columns; used for Figure 5.
#' @param plotopts.bullseye,plotopts.sm4heatmap Data frame or the path to the
#'   \code{plotopts_bullseye.csv} / \code{plotopts_sm4heatmap.csv} settings
#'   tables, default \code{NULL} (= the copies in \code{inst/plotopts}, else
#'   a copy in the working directory).
#'   Added 2026-10-28. Passed to \code{batz.plotcover_bullseye()} and
#'   \code{batz.plotsm4_heatmap()}.
#' @param strip.special Logical, default \code{TRUE}. Simplifies accented
#'   letters and removes other non-ASCII characters from the loaded forms,
#'   as in the other Batz loaders.
#'
#' @return Invisibly, a list: \code{$sites} (one row per site with every
#'   value written into the report), \code{$habitat} (added 2026-10-28: one
#'   row per Habitat Assessment table, with \code{$aru.name} and every value
#'   written into it), \code{$files} (paths written),
#'   \code{$aru.habitat}, \code{$aru.quad}, \code{$aru.service} (the forms
#'   with Batz headers, after the \code{project.name} subset), and
#'   \code{$serial.check} (one row per serial-number problem: \code{$aru.name},
#'   \code{$form}, \code{$field}, \code{$serial}, \code{$issue}; a NOTE is
#'   printed when it has rows).
#'
#' @details
#' \strong{Steps.}
#' \enumerate{
#'   \item The workbook (or list) is read and the \code{deploy},
#'     \code{quad} and \code{service} forms are found. If one or two are
#'     missing, \code{"Tabs missing from input data: <tabs> do you still
#'     wish to continue?"} is printed and \code{Y}/\code{N} asked (see
#'     \code{continue.missing}); if all three are missing the function stops
#'     with \code{"none of the required tabs are present: quad, deploy,
#'     service"}.
#'   \item Each form's Survey123 headers are renamed to Batz headers using
#'     \code{survey123.ref.headers()} (Josh's \code{Header conversion and
#'     outputs.xlsx}), matched ignoring case, spaces and punctuation. When two
#'     Survey123 columns become the same Batz column (e.g. service
#'     \code{Site Name} + \code{Specify Other Site} -> \code{$aru.name}, the
#'     two \code{Personnel} columns) the first non-blank value is used, and a
#'     value of \code{"other"} is replaced by its "Specify other" column.
#'     The forms become \code{aru.habitat}, \code{aru.quad} and
#'     \code{aru.service}.
#'   \item Records are subset to \code{project.name}.
#'   \item One report per \code{$aru.name} in the deployment form (service
#'     records are matched to it ignoring case and spaces). Service records
#'     whose \code{$aru.name} has no deployment are listed in a NOTE.
#'   \item Each report is filled from the template and written as Word
#'     and/or PDF, collated and/or separate. Saved file names are printed.
#' }
#'
#' \strong{Where each value comes from} (field IDs from \code{Model ARU
#' Appendix by Identifier stage1}): header - \code{client},
#' \code{client.contact}, \code{project.label}, \code{project.code},
#' \code{project.manager} (data column, else the input). Project
#' information - \code{aru.habitat$biologist.permitted},
#' \code{$permit.federal}, \code{$personnel.bri}, \code{$date.habitat.assessment},
#' \code{$date.deployment}, \code{$date.recovery_planned} (blank - the
#' deployment form has no such field yet), final recovery =
#' \code{max(aru.service$date)}, servicing dates = dates of the non-retrieval
#' service visits, \code{$survey.type}, \code{$site.type},
#' \code{$landowner.info}, \code{$aru.label} (= \code{$aru.name} when there
#' is no such column), \code{$aru.name}, \code{$county}, \code{$state}
#' (full name), coordinates = \code{$latitude, $longitude} (6 decimals).
#' Detector info - \code{aru.habitat$aru.make, $aru.model, $aru.serial};
#' \code{$power.make, $power.model, $power.serial};
#' \code{$microphone.make, $microphone.model, $microphone.serial};
#' \code{$microphone.height}; \code{$microphone.orientation_horizontal};
#' \code{$microphone.orientation_vertical}; \code{$system.check_timings};
#' \code{$system.check_hardware}; \code{$survey.schedule} (blank when the form
#' has no such field). Columns that don't exist are left blank. Servicing
#' events - one row per non-retrieval visit: \code{aru.service$date},
#' \code{$personnel.bri}, \code{$actions.performed}, \code{$notes}. Recovery
#' - the last \code{"retrieval"} visit: date, \code{$aru.serial_new} (else
#' \code{$aru.serial}), \code{$power.serial_new} (else \code{$power.serial}),
#' \code{$microphone.serial_new} (else \code{$microphone.serial}), height /
#' orientations / status checks when the service form has them, and
#' \code{$notes}.
#'
#' \strong{Formatting.} Coded Survey123 answers are shown with their
#' Survey123 labels from \code{survey123.ref.choices()}, per field:
#' \code{aru.make} (\code{wildlife_acoustics} -> \code{Wildlife Acoustics}),
#' \code{aru.model}, \code{survey.type}, \code{site.type} (\code{inland} ->
#' \code{Inland}), \code{state} (\code{ME} -> \code{Maine}),
#' \code{system.check_*} (\code{yes} -> \code{Yes}), \code{actions.performed}
#' (\code{sdcard_swap,new_location} -> \code{SD cards swapped, Location
#' Moved}). Personnel codes are kept (\code{BRF, ECG}) - the footnote lists
#' the names. Dates are \code{m/d/yyyy}, or \code{m/d/yyyy hh:mm} with \code{show.time = TRUE}. The text \code{"NA"} (any case) or \code{"N/A"} counts as
#' blank.
#'
#' \strong{Template.} The default (terrestrial) template is Josh's
#' \code{Model ARU Appendix2.docx stage1} with each value replaced by a
#' \code{\{\{placeholder\}\}} (the servicing table has one template row that is
#' repeated per visit). The duplicate second recovery table at the end of the
#' model document was left out. To change wording or layout, edit a copy of
#' the template in Word, keeping each \code{\{\{...\}\}} as one unformatted
#' piece of text, and pass it as \code{template}.
#'
#' \strong{Footnotes} (2026-10-27) are marked with symbols (\code{*},
#' dagger, double dagger, section, pilcrow, double bar, then doubled) instead
#' of numbers. Footnotes with the same text get the same symbol, e.g. the
#' staff-initials note on \emph{Personnel} and on \emph{Staff}; each page
#' still shows the note at its foot.
#'
#' \strong{Replaced equipment} (2026-10-27). When a servicing visit records a
#' new serial (\code{aru.serial_new}, \code{microphone.serial_new},
#' \code{power.serial_new}, \code{hobo.serial_new}), the visit's Notes get
#' \verb{<old serial> -> <new serial>} added (several are separated by
#' \code{;}). The old serial is the one on that service form, else the one
#' deployed or last put in (\code{unknown} if neither is known). A later
#' service form whose serial doesn't match the serial deployed / last put in
#' is reported in \code{$serial.check}, with or without an inventory. No
#' footnote text is bold.
#'
#' \strong{Defaults} (2026-10-28). When a power row (deployment or recovery)
#' has no make, model or serial, it reads \code{"Internal power, 4 x D
#' Batteries"}. When the retrieval visit has no microphone height,
#' direction or orientation, the deployment values are repeated.
#'
#' \strong{New model} (2026-10-28, per Josh: \code{Model ARU Appendix by
#' Identifier.docx}). The terrestrial template was rebuilt from this model:
#' \itemize{
#'   \item Project information reads \emph{Final Recovery/Last Service} and
#'     \emph{Landowner Information}, with \emph{Site} before \emph{Site Label}.
#'   \item Recovery: \emph{All status checks completed} is replaced by
#'     \emph{Status Check: Settings} (\code{aru.service$system.check_timings}),
#'     \emph{Status Check: Hardware} (\code{$system.check_hardware}) and
#'     \emph{Survey Schedule} (\code{$survey.schedule}), from the retrieval
#'     visit (blank when the form has no such field). The \emph{Notes} row is
#'     kept.
#'   \item \strong{Habitat Assessment} table, one per deployment record:
#'     assessment date (\code{$date.deployment}, as in the model), area (fixed
#'     text), \code{$site.description_basic}, \code{$targeted.feature},
#'     \code{$roosting.habitat_bats}, \code{$water.resources_site},
#'     \code{$water.resources_distance.to.microphone},
#'     \code{$water.resources_type.nearestbody}, tree species, understory
#'     species, \code{$vegetation.tree_estimated.DBH},
#'     \code{$vegetation.tree_estimated.agerange},
#'     \code{$vegetation.understory_clutter},
#'     \code{$vegetation.understory_clutter.distance}, \code{$landcover}
#'     (coded answers shown with their Survey123 labels).
#'   \item Species rows: \emph{Tree Species Dominant} =
#'     \code{$vegetation.tree_dominant.species}; \emph{Tree Species All
#'     Present} = that plus \code{$vegetation.tree_all.species} (if present)
#'     and the quadrant form's Tree 1-3 species; \emph{Understory Species
#'     Dominant} = \code{$vegetation.understory_dominant.species}, else the
#'     quadrant Sapling 1-3 species; \emph{Understory Species All Present} =
#'     the dominant understory species, \code{$vegetation.understory_all.species}
#'     (if present) and the Sapling 1-3 species. Lists are split on \code{,}
#'     and \code{;}, each species is listed once (case and spacing ignored)
#'     and they are ordered by how often they were recorded, most often first
#'     (ties keep the order first seen).
#'   \item \strong{ARU Photos} (Figures 1-3): see \code{photo.dir}. HEIC
#'     photos (iPhone) need the \code{magick} package; with it every photo
#'     is turned upright, shrunk to at most 1600 px and saved as JPEG for
#'     the report. Without \code{magick}, JPEG/PNG photos are used as they
#'     are and HEIC cells say the package is needed. A view with no photo
#'     reads \code{"No photo found"}. (2026-10-28: each photo's memory is
#'     freed as soon as it is converted, and a failed conversion is tried
#'     once more - a full set of 12 MP phone photos had filled ImageMagick's
#'     cache - and an empty converted file counts as unreadable.)
#'   \item \strong{Figure 4}: \code{batz.plotcover_bullseye()} for the site,
#'     from the quadrant form (\code{$quadrant}, \code{$canopy.cover},
#'     \code{$Understory.cover}) and the deployment microphone height and
#'     orientations. When \code{icon.dir} in the settings table is not
#'     found, the icons in \code{inst/img} are used.
#'   \item \strong{Figure 5}: \code{batz.plotsm4_heatmap()} for the site,
#'     from \code{sm4.data}. Each SM4 log line is one minute of recording;
#'     lines are counted per 15-minute interval of the monitoring night
#'     (noon to noon; log times are taken as local time), giving
#'     \code{$aru.name}, \code{$date.monitoringnight},
#'     \code{$mins2.noon.mon}, \code{$mins.oper}. Dawn/dusk lines come from
#'     \code{batz.generate_suntimes.arulist()} run on the deployment
#'     coordinates in \code{time.zone}. Sites are matched to the log's
#'     \code{aru.name} ignoring case and punctuation; a site with no log
#'     rows reads \code{"No SM4 log data for this site"} and is named in a
#'     NOTE.
#' }
#'
#' \strong{Layout and species update} (2026-10-28, second round, per Josh).
#' \itemize{
#'   \item \emph{Tree / Understory Species All Present}: the quadrant species
#'     (Tree 1-3, or Sapling 1-3) come first, most often recorded first (ties
#'     in the order first named); then the form's dominant and other species
#'     in that order, skipping any species already listed. Each species
#'     appears once; a name one letter off an earlier one (e.g.
#'     \code{Balsalm Fir} / \code{Balsam Fir}) is treated as the same
#'     species, the first spelling is kept and the pair is named in a NOTE.
#'     This replaces the frequency order described above. \emph{Understory
#'     Species Dominant} is the form's field, else the sapling species in the
#'     same quadrant order.
#'   \item "(within view)" is removed from the two \emph{All Present} labels;
#'     the area reads "20m x 20m plot with Microphone at the center".
#'   \item \emph{Microphone Distance to Water (m)} (was \emph{Distance Mic to
#'     Nearest Water (m)}) and \emph{Microphone Distance to Clutter (m)} moved
#'     from the Habitat Assessment table to the Detector Info table, below
#'     \emph{Microphone Orientation}, from the first habitat record
#'     (\code{$site.water.distance}, \code{$site.clutter.distance} in
#'     \code{$sites}).
#'   \item The recovery table no longer has the two Status Check rows.
#'   \item \emph{Survey Schedule}: when the form has none, it reads
#'     "-30min before dusk and +30min after dawn" (Josh's wording); the
#'     recovery row repeats the deployment value.
#'   \item Figure 4: panels are the microphone bar, then Understory, then
#'     Canopy Cover (\code{sub.plots = c("mic.bar", "understory.bull",
#'     "canopy.bull")}); the microphone-height bullseye is left out. Drawn
#'     2.5 in tall (\code{plot.height}, unless its override is set).
#'   \item Figure 5: the y axis runs from the even hour at or before the
#'     earliest dusk - 30 min to the even hour at or after the latest
#'     dawn + 30 min, widened to the next even hour if any recorded minutes
#'     fall outside it, with 2-hour labels. Caption: "Number of successful
#'     monitoring minutes for the SM4BAT deployed at <aru.name> from
#'     <date.deployment> through <last service date> calculated in 15min
#'     intervals." The last service date is \code{max(aru.service$date)}
#'     (the service form has no \code{date.deployment}).
#'   \item Figures 4 and 5 are drawn smaller (at most 3.0 in and 3.6 in tall)
#'     and start a new page together. Blank pages are avoided: page breaks
#'     are set on the section headings, and between collated sites the break
#'     goes at the end of the previous site's last paragraph.
#' }
#'
#' \strong{Third round} (2026-10-28, per Josh; replaces the species order
#' and Survey Schedule text above).
#' \itemize{
#'   \item Species rows list every unique species across the fields once, in
#'     this order: the dominant species field, then the "all species" field,
#'     then the quadrant species by
#'     \code{names(sort(table(quad.species), decreasing = TRUE))}; a species
#'     already listed is not repeated. Spelling errors are fixed: a name one
#'     letter off another name in the project's forms takes the spelling
#'     recorded most often across all sites (e.g. \code{Balsalm Fir} ->
#'     \code{Balsam Fir}), in the Dominant rows too; a NOTE lists each fix.
#'   \item Detector Info: the microphone rows are grouped under a
#'     \emph{Microphone} row - \emph{Make, Model & Serial #}, \emph{Height},
#'     \emph{Direction(degrees)}, \emph{Vertical Orientation},
#'     \emph{Distance to Water}, \emph{Distance to Clutter}.
#'   \item Heights and distances are meters, written with their unit
#'     (\code{"4 m"}); over 1000 m they are shown in km (\code{"1.5 km"}).
#'     The recovery row label is now \emph{Microphone Height:} (was
#'     \emph{Microphone Height (m)}), since the value carries the unit.
#'   \item A blank Survey Schedule reads "Continuous -30min Dusk to +30min
#'     Dawn".
#'   \item Figure 5 has no site name above the plot.
#'   \item \emph{Power Make, Model & Serial #} moved to just below
#'     \emph{Distance to Clutter} in Detector Info.
#'   \item \emph{Distance to Water}: when blank and Water Resources is "No
#'     water resources present at site", it reads "None present".
#'   \item Figure 4 (fourth round): panel titles "Microphone Height and
#'     vertical orientation", "Microphone Direction and Understory Clutter",
#'     "Microphone Direction and Canopy Cover" (unless overridden in
#'     \code{plotopts.bullseye}); labels (a), (b), (c) at the bottom right
#'     of each panel; the direction arrow on the Understory panel only
#'     (\code{arrow.panel = "first"}); the blank margin under the panels
#'     halved; new caption naming (a), (b), (c), the red dashed site
#'     averages and the black direction arrow.
#'   \item ARU photos: no table borders; each photo is centered at the
#'     bottom of its cell.
#'   \item Figure 4 (fifth round): titles "Height and vertical orientation",
#'     "Direction and Understory Clutter", "Canopy Cover", 2 pt smaller
#'     (\code{title.size.adjust = -2}); Figure 4 now shares a page with
#'     Figure 3, and Figure 5 starts its own page.
#'   \item Sixth round: the outer box around the header (logo/client) table
#'     is removed; Figure 4 caption now says "canopy cover" and categories
#'     "0, 1-25,25-50,50-75,75-100\%"; in the collated file the figure
#'     numbers run on from site to site (site 2's figures are 6-10, ...);
#'     separate files start at Figure 1.
#'   \item Seventh round: no borders at all on the header (logo/client)
#'     table; the Habitat Assessment section follows the recovery table on
#'     the same page when the whole table fits (its heading and rows are
#'     kept together, so it moves to the next page as a block when it does
#'     not); \emph{Target Recovery} = deployment date + 28 days when the
#'     data has no target recovery date.
#' }
#'
#' \strong{Spelling corrections} (2026-10-28, per Josh). Batz headers renamed:
#' \code{vegetation.tree_all.speacies} -> \code{vegetation.tree_all.species};
#' \code{vegetation.understroy_*} -> \code{vegetation.understory_*};
#' \code{*_dominate.species} -> \code{*_dominant.species};
#' \code{water.resoruces _*} -> \code{water.resources_*};
#' \code{vegetation.tree_esimated.*} -> \code{vegetation.tree_estimated.*};
#' \code{microphone.orientation_horizonal} ->
#' \code{microphone.orientation_horizontal}; the temporary arulist's
#' \code{schedual1}/\code{schedual2} -> \code{schedule1}/\code{schedule2}.
#' A form (or data frame) that still uses an old spelling is renamed on load
#' (\code{survey123.ref.renamed()}). The missing-tab prompt now reads
#' "do you still wish to continue?" (was "wished"), and the Figure 4 caption
#' "taking the mean of the mid-points of each quadrant" (was "meaning of the
#' mid-point"). The Survey123 service tab name \code{survice} is still
#' accepted because the Survey123 form itself uses it.
#'
#' \strong{PDF.} Converted from the Word file with the \code{doconv} package
#' if it is installed (uses Word on Windows), otherwise with LibreOffice
#' (\code{soffice}) if found. If neither is available the PDF is skipped with
#' a warning.
#'
#' @examples
#' \dontrun{
#' batz.generate_aru.appendix(
#'   data = "FY26_SevenIslands_NABat_Aru_DeploymentService.xlsx",
#'   client = "Wildlife Management Institute", client.contact = "Meghan Gilbart",
#'   project.name = "NA Bat", project.label = "Maine NABAT",
#'   project.code = "MAMM-NG26-007", project.manager = "Joshua Guilbert",
#'   word = TRUE, pdf = FALSE, collate = TRUE, separate = TRUE,
#'   equipment.inventory = "survey123 equipment database.xlsx",
#'   photo.dir = "ARU Photos", sm4.data = "mergedsm4.csv")
#' }
#'
#' @export
batz.generate_aru.appendix <- function(data,
                                       client = "",
                                       client.contact = "",
                                       project.name = "",
                                       project.label = "",
                                       project.code = "",
                                       project.manager = "",
                                       word = TRUE,
                                       pdf = FALSE,
                                       collate = TRUE,
                                       separate = TRUE,
                                       dir.save = getwd(),
                                       time.zone = "America/New_York",
                                       continue.missing = NULL,
                                       template = NULL,
                                       strip.special = TRUE,
                                       show.time = FALSE,
                                       equipment.inventory = NULL,
                                       photo.dir = NULL,
                                       sm4.data = NULL,
                                       plotopts.bullseye = NULL,
                                       plotopts.sm4heatmap = NULL,
                                       photo.width = 3) {

  if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
    stop("batz.generate_aru.appendix() needs the 'officer' and 'xml2' packages: install.packages(\"officer\")",
         call. = FALSE)
  }
  if (!isTRUE(word) && !isTRUE(pdf)) stop("At least one of `word` and `pdf` must be TRUE.", call. = FALSE)
  if (!isTRUE(collate) && !isTRUE(separate)) stop("At least one of `collate` and `separate` must be TRUE.", call. = FALSE)
  ## 2026-10-27: the shipped template is the terrestrial appendix; other
  ## survey types can be added later as their own named templates
  if (is.null(template)) template <- "terrestrial"
  if (length(template) == 1 && !grepl("\\.docx$", template, ignore.case = TRUE)) {
    tname <- paste0("batz.aru.appendix.", tolower(template), ".template.docx")
    tpath <- system.file("templates", tname, package = "Batz")
    if (!nzchar(tpath)) tpath <- system.file("templates", tname, package = "batz")
    if (!nzchar(tpath)) stop("No built-in template \"", template, "\" (looked for inst/templates/", tname,
                             ") - pass `template` = path to a .docx.", call. = FALSE)
    template <- tpath
  }
  if (!nzchar(template) || !file.exists(template)) {
    stop("Template not found - pass `template` = path to batz.aru.appendix.template.docx.", call. = FALSE)
  }
  if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)

  is.blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x))) |
    tolower(trimws(as.character(x))) %in% c("na", "n/a")
  clean <- function(x) { x <- trimws(as.character(x)); x[is.blank(x)] <- ""; x }
  key <- function(x) tolower(gsub("[^A-Za-z0-9]+", "", as.character(x)))

  ## --- 1. load the forms ----------------------------------------------------
  if (is.character(data) && length(data) == 1) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Reading an .xlsx needs the 'readxl' package.", call. = FALSE)
    if (!file.exists(data)) stop("File not found: ", data, call. = FALSE)
    sheets <- readxl::excel_sheets(data)
    tabs.in <- lapply(sheets, function(s) as.data.frame(
      readxl::read_excel(data, sheet = s, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE))
    names(tabs.in) <- sheets
  } else if (is.list(data) && !is.data.frame(data) && length(data) > 0 && !is.null(names(data))) {
    tabs.in <- lapply(data, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("`data` must be the path to the Survey123 .xlsx export or a named list of data frames.", call. = FALSE)
  }
  tabs.in <- lapply(tabs.in, function(df) special.strip(df, strip.special)$df)

  ref <- survey123.ref.headers()
  required.tabs <- c("quad", "deploy", "service")
  tab.alias <- list(quad = c("quad", "quadrant", "quads"),
                    deploy = c("deploy", "deployment", "habitat", "deployments"),
                    service = c("service", "survice", "servicing", "services"))   # "survice": the Survey123 form's own tab name
  found <- setNames(rep(NA_character_, 3), required.tabs)
  for (t in required.tabs) {
    hit <- names(tabs.in)[key(names(tabs.in)) %in% key(tab.alias[[t]])]
    if (length(hit) > 0) found[t] <- hit[1]
  }
  ## fallback: the unused tab whose headers best match the form (>= 50%)
  for (t in required.tabs[is.na(found)]) {
    want <- key(ref$survey123.header[ref$tab == t])
    cand <- setdiff(names(tabs.in), found)
    if (length(cand) == 0) next
    score <- vapply(cand, function(s) mean(want %in% key(names(tabs.in[[s]]))), numeric(1))
    if (max(score) >= 0.5) found[t] <- cand[which.max(score)]
  }
  missing.tabs <- required.tabs[is.na(found)]
  if (length(missing.tabs) == length(required.tabs)) {
    stop("none of the required tabs are present: ", paste(required.tabs, collapse = ", "), call. = FALSE)
  }
  if (length(missing.tabs) > 0) {
    msg <- paste0("Tabs missing from input data: ", paste(missing.tabs, collapse = ", "),
                  " do you still wish to continue?")
    go <- continue.missing
    if (is.null(go)) {
      if (interactive()) {
        ans <- readline(paste0(msg, " (Y/N): "))
        go <- toupper(substr(trimws(ans), 1, 1)) == "Y"
      } else {
        stop(msg, "\n(not an interactive session - set continue.missing = TRUE to continue)", call. = FALSE)
      }
    } else {
      cat(msg, if (isTRUE(go)) "Y (continue.missing = TRUE)\n" else "N (continue.missing = FALSE)\n")
    }
    if (!isTRUE(go)) stop("Stopped - missing tabs: ", paste(missing.tabs, collapse = ", "), call. = FALSE)
  }

  ## --- 2. Survey123 headers -> Batz headers ----------------------------------
  to.batz <- function(df, tab) {
    if (is.null(df)) return(NULL)
    r <- ref[ref$tab == tab, ]
    src <- key(names(df))
    target <- r$batz.header[match(src, key(r$survey123.header))]
    ## a Batz header with its earlier spelling (2026-10-28 corrections, e.g.
    ## vegetation.understroy_clutter) is renamed to the corrected spelling
    renamed <- survey123.ref.renamed()
    hit <- is.na(target) & src %in% key(renamed$header.old)
    target[hit] <- renamed$header.new[match(src[hit], key(renamed$header.old))]
    ## a column already named with the Batz header is kept
    already <- is.na(target) & src %in% key(r$batz.header)
    target[already] <- r$batz.header[match(src[already], key(r$batz.header))]
    target[is.na(target)] <- names(df)[is.na(target)]
    out <- list()
    for (tg in unique(target)) {
      idx <- which(target == tg)
      v <- df[[idx[1]]]
      for (j in idx[-1]) {
        w <- df[[j]]
        if (inherits(v, "POSIXct") || inherits(w, "POSIXct")) {
          fill <- is.na(v) & !is.na(w)
          if (any(fill)) v[fill] <- w[fill]
        } else {
          vc <- as.character(v); wc <- as.character(w)
          use <- (is.blank(vc) | tolower(trimws(vc)) == "other") & !is.blank(wc)
          vc[use] <- wc[use]
          v <- vc
        }
      }
      out[[tg]] <- v
    }
    as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  }
  aru.habitat <- to.batz(if (is.na(found["deploy"])) NULL else tabs.in[[found["deploy"]]], "deploy")
  aru.quad    <- to.batz(if (is.na(found["quad"])) NULL else tabs.in[[found["quad"]]], "quad")
  aru.service <- to.batz(if (is.na(found["service"])) NULL else tabs.in[[found["service"]]], "service")

  ## --- 3. project subset -----------------------------------------------------
  projects <- unique(clean(unlist(lapply(list(aru.habitat, aru.service), function(df)
    if (!is.null(df) && "project.name" %in% names(df)) df$project.name))))
  projects <- projects[nzchar(projects)]
  if (nzchar(trimws(project.name))) {
    keep.proj <- function(df) {
      if (is.null(df) || !("project.name" %in% names(df))) return(df)
      df[key(df$project.name) == key(project.name), , drop = FALSE]
    }
    aru.habitat <- keep.proj(aru.habitat); aru.service <- keep.proj(aru.service)
    if (!is.null(aru.quad) && !is.null(aru.habitat) && all(c("ParentGlobalID") %in% names(aru.quad)) &&
        "GlobalID" %in% names(aru.habitat)) {
      aru.quad <- aru.quad[aru.quad$ParentGlobalID %in% aru.habitat$GlobalID, , drop = FALSE]
    }
    proj.label.file <- project.name
  } else {
    if (length(projects) > 1) {
      stop("The data holds more than one project (", paste(projects, collapse = ", "),
           ") - set project.name to the one to report on.", call. = FALSE)
    }
    proj.label.file <- if (length(projects) == 1) projects else "project"
  }
  site.source <- if (!is.null(aru.habitat)) aru.habitat else aru.service
  if (is.null(site.source) || nrow(site.source) == 0 || !("aru.name" %in% names(site.source))) {
    stop("No records with an aru.name (Site) left for project.name = \"", project.name, "\".", call. = FALSE)
  }

  ## --- 4. value helpers --------------------------------------------------------
  choices <- survey123.ref.choices()
  label <- function(x, list.name) {
    x <- clean(x)
    ch <- choices[choices$list.name == list.name, ]
    vapply(x, function(v) {
      if (!nzchar(v)) return("")
      parts <- trimws(strsplit(v, ",")[[1]])
      lab <- ch$label[match(tolower(parts), tolower(ch$name))]
      paste(ifelse(is.na(lab), parts, lab), collapse = ", ")
    }, character(1), USE.NAMES = FALSE)
  }
  fmt.dt <- function(x) {
    if (inherits(x, "POSIXct")) {
      ok <- !is.na(x)
      out <- rep("", length(x))
      lt <- as.POSIXlt(x[ok], tz = time.zone)
      out[ok] <- if (isTRUE(show.time)) {
        sprintf("%d/%d/%d %02d:%02d", lt$mon + 1, lt$mday, lt$year + 1900, lt$hour, lt$min)
      } else sprintf("%d/%d/%d", lt$mon + 1, lt$mday, lt$year + 1900)
      out
    } else if (inherits(x, "Date")) {
      out <- rep("", length(x)); ok <- !is.na(x)
      lt <- as.POSIXlt(x[ok])
      out[ok] <- sprintf("%d/%d/%d", lt$mon + 1, lt$mday, lt$year + 1900); out
    } else clean(x)
  }
  people <- function(x) gsub("\\s*,\\s*", ", ", clean(x))
  ## 2026-10-28, per Josh: no target recovery date in the data -> 28 days
  ## after deployment (date only)
  target.recovery <- function(dep) {
    d <- if (inherits(dep, "POSIXct")) as.Date(format(dep, tz = time.zone, "%Y-%m-%d"))
         else if (inherits(dep, "Date")) dep
         else suppressWarnings(as.Date(clean(dep), tryFormats = c("%m/%d/%Y", "%Y-%m-%d"), optional = TRUE))
    if (length(d) != 1 || is.na(d)) return("")
    lt <- as.POSIXlt(d + 28)
    sprintf("%d/%d/%d", lt$mon + 1, lt$mday, lt$year + 1900)
  }
  col <- function(df, name, i = 1) {
    if (is.null(df) || nrow(df) == 0 || !(name %in% names(df))) return("")
    df[[name]][i]
  }
  join <- function(...) { v <- clean(c(...)); paste(v[nzchar(v)], collapse = ", ") }
  first.of <- function(name) {
    for (df in list(hab, svc, qd)) {
      if (!is.null(df) && nrow(df) > 0 && name %in% names(df)) {
        v <- clean(df[[name]]); v <- v[nzchar(v)]
        if (length(v) > 0) return(v[1])
      }
    }
    ""
  }
  num6 <- function(x) { x <- suppressWarnings(as.numeric(x)); if (is.na(x)) "" else sprintf("%.6f", x) }
  newer <- function(df, new, old) { v <- clean(col(df, new)); if (nzchar(v)) v else clean(col(df, old)) }

  ## --- equipment inventory (optional, 2026-10-27) -----------------------------
  ## equipment.type of a serial field, from its Batz name (aru.serial,
  ## microphone.serial_new, ...) or from an inventory "type" value
  eq.of.field <- function(f) sub("\\..*$", "", f)
  eq.of.text <- function(x) {
    x <- tolower(clean(x))
    ifelse(!nzchar(x), NA_character_,
    ifelse(grepl("mic", x), "microphone",
    ifelse(grepl("hobo|temp|logger", x), "hobo",
    ifelse(grepl("power|batt|solar", x), "power",
    ifelse(grepl("aru|detector|recorder|song ?meter|sm[0-9]|chorus|audiomoth|ranger|d500|smart", x), "aru",
           NA_character_)))))
  }
  inv <- NULL
  if (!is.null(equipment.inventory)) {
    ## 2026-10-28: a path to the equipment database (.xlsx/.csv) also works
    if (is.character(equipment.inventory) && length(equipment.inventory) == 1) {
      if (!file.exists(equipment.inventory)) stop("equipment.inventory file not found: ", equipment.inventory, call. = FALSE)
      equipment.inventory <- if (grepl("\\.csv$", equipment.inventory, ignore.case = TRUE)) {
        special.read.csv(equipment.inventory, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        as.data.frame(readxl::read_excel(equipment.inventory, .name_repair = "minimal"), check.names = FALSE)
      }
    }
    if (!is.data.frame(equipment.inventory)) {
      stop("`equipment.inventory` must be a data frame or the path to the equipment database.", call. = FALSE)
    }
    inv <- special.strip(as.data.frame(equipment.inventory, stringsAsFactors = FALSE), strip.special)$df
    names(inv) <- standardize.headers(names(inv))
    ## 2026-10-28: Josh's equipment database uses batz.equipment_<field>
    ## headers - the "batz.equipment_" / "equipment_" prefix is ignored here
    short <- sub("^(batz_)?(equipment_)?", "", names(inv))
    pick <- function(cands) { h <- names(inv)[match(cands, short)]; h <- h[!is.na(h)]
                              if (length(h) > 0) h[1] else NA_character_ }
    inv.cols <- c(serial = pick(c("serial", "serial_number", "serial_no", "sn", "equipment_serial")),
                  make   = pick(c("make", "manufacturer", "brand")),
                  model  = pick(c("model")),
                  type   = pick(c("type", "equipment_type", "equipment", "category", "item")),
                  status = pick(c("working_status", "status", "condition")))
    if (is.na(inv.cols[["serial"]])) {
      stop("`equipment.inventory` needs a serial-number column (serial, serial.number, serial.no or sn).", call. = FALSE)
    }
    inv$.key  <- key(inv[[inv.cols[["serial"]]]])
    inv$.type <- if (!is.na(inv.cols[["type"]])) eq.of.text(inv[[inv.cols[["type"]]]]) else NA_character_
    inv$.type.label <- if (!is.na(inv.cols[["type"]])) clean(inv[[inv.cols[["type"]]]]) else ""
    inv <- inv[nzchar(inv$.key), , drop = FALSE]
  }
  inv.get <- function(serial, what) {
    if (is.null(inv) || is.na(inv.cols[[what]]) || !nzchar(clean(serial))) return("")
    i <- match(key(serial), inv$.key)
    if (is.na(i)) "" else clean(inv[[inv.cols[[what]]]][i])
  }
  ## "<make>, <model>, <serial>" for one piece of equipment; a blank make or
  ## model is filled from the inventory by serial number
  mms <- function(df, eq, serial = clean(col(df, paste0(eq, ".serial")))) {
    make  <- clean(col(df, paste0(eq, ".make")));  if (!nzchar(make))  make  <- inv.get(serial, "make")
    model <- clean(col(df, paste0(eq, ".model"))); if (!nzchar(model)) model <- inv.get(serial, "model")
    join(label(make, "aru_make"), if (eq == "aru") label(model, "aru_model") else model, serial)
  }
  ## 2026-10-28, per Josh: no power make, model or serial at all = the ARU's
  ## internal batteries
  power.default <- "Internal power, 4 x D Batteries"
  or.else <- function(a, b) { a <- clean(a); if (nzchar(a)) a else clean(b) }
  serial.issues <- list()
  add.issue <- function(site, form, field, serial, issue) {
    serial.issues[[length(serial.issues) + 1]] <<- data.frame(aru.name = site, form = form, field = field,
      serial = serial, issue = issue, stringsAsFactors = FALSE)
  }

  sites <- sort(unique(clean(site.source$aru.name)))
  sites <- sites[nzchar(sites)]
  if (!is.null(aru.habitat) && !is.null(aru.service) && "aru.name" %in% names(aru.service)) {
    orphan <- setdiff(unique(clean(aru.service$aru.name)), "")
    orphan <- orphan[!(key(orphan) %in% key(sites))]
    if (length(orphan) > 0) {
      cat("NOTE: service records with no matching deployment (not reported): ",
          paste(orphan, collapse = ", "), "\n", sep = "")
    }
  }

  ## 2026-10-28, per Josh: species lists - every unique species across the
  ## fields, each listed once. Third revision the same day, per Josh: order
  ## = the dominant species field, then the "all species" field, then the
  ## quadrant species by names(sort(table(quad.species), decreasing = TRUE));
  ## a species already listed from an earlier field is not repeated, and
  ## spelling errors are fixed: a name one letter off another name used in
  ## the project's forms (Balsalm Fir / Balsam Fir) is shown with whichever
  ## spelling is recorded most often across all sites (a NOTE names them).
  ## 2026-10-28, per Josh: heights and distances are meters - "4 m";
  ## over 1000 m shown in km ("1.5 km"); non-numbers are left as they are
  fmt.m <- function(x) {
    x <- clean(x); v <- suppressWarnings(as.numeric(x))
    if (length(x) != 1 || is.na(v)) return(x)
    if (v > 1000) paste(format(round(v / 1000, 3), drop0trailing = TRUE, trim = TRUE), "km")
    else paste(format(v, drop0trailing = TRUE, trim = TRUE), "m")
  }
  species.merged <- character(0)
  survey.schedule.default <- "Continuous -30min Dusk to +30min Dawn"
  split.species <- function(v) {
    x <- unlist(lapply(v, function(e) unlist(strsplit(clean(e), "\\s*[,;]\\s*"))))
    x <- trimws(gsub("\\s+", " ", x)); x[nzchar(x)]
  }
  ## every species name in the project's deployment and quadrant forms
  species.cols <- c("vegetation.tree_dominant.species", "vegetation.tree_all.species",
                    "vegetation.understory_dominant.species", "vegetation.understory_all.species",
                    "vegetation.tree_Tree1.species", "vegetation.tree_Tree2.species", "vegetation.tree_Tree3.species",
                    "vegetation.sapling_species1", "vegetation.sapling_species2", "vegetation.sapling_species3")
  species.vocab <- split.species(c(
    unlist(lapply(intersect(species.cols, names(aru.habitat)), function(cl) aru.habitat[[cl]])),
    unlist(lapply(intersect(species.cols, names(aru.quad)), function(cl) aru.quad[[cl]]))))
  vocab.n <- table(tolower(species.vocab))
  fix.species <- function(sp) {
    cand <- unique(species.vocab[nchar(species.vocab) >= 5 & nchar(sp) >= 5 &
                                   as.vector(utils::adist(tolower(sp), tolower(species.vocab))) <= 1])
    cand <- cand[!duplicated(tolower(cand))]
    if (length(cand) == 0) return(sp)
    n <- as.numeric(vocab.n[tolower(cand)]); n[is.na(n)] <- 0
    best <- cand[order(-n, tolower(cand) != tolower(sp))][1]
    if (tolower(best) == tolower(sp)) return(sp)
    if (max(n) <= as.numeric(vocab.n[tolower(sp)])) return(sp)   # no more common spelling
    species.merged <<- unique(c(species.merged, paste0(sp, " -> ", best)))
    best
  }
  species.list <- function(..., quad = list()) {
    q <- split.species(quad)
    q.order <- if (length(q) > 0) names(sort(table(q), decreasing = TRUE)) else character(0)
    x <- vapply(c(split.species(list(...)), q.order), fix.species, character(1), USE.NAMES = FALSE)
    if (length(x) == 0) return("")
    paste(x[!duplicated(tolower(x))], collapse = ", ")
  }
  qcol <- function(df, name) if (!is.null(df) && nrow(df) > 0 && name %in% names(df)) df[[name]] else character(0)
  habitat.values <- function(h, qd) {
    list(
      hab.assessment.date        = fmt.dt(col(h, "date.deployment")),
      site.description_basic     = clean(col(h, "site.description_basic")),
      targeted.feature           = clean(col(h, "targeted.feature")),
      roosting.habitat_bats      = species.list(label(col(h, "roosting.habitat_bats"), "roost_habitat"),
                                                label(col(h, "roosting.habitat_bats_collated"), "roost_habitat")),
      water.resources_site       = label(col(h, "water.resources_site"), "water_resources"),
      water.resources_distance   = clean(col(h, "water.resources_distance.to.microphone")),
      water.resources_type       = label(col(h, "water.resources_type.nearestbody"), "water_type"),
      tree.species.dominant      = species.list(col(h, "vegetation.tree_dominant.species")),
      tree.species.all           = species.list(col(h, "vegetation.tree_dominant.species"), col(h, "vegetation.tree_all.species"),
                                                quad = list(qcol(qd, "vegetation.tree_Tree1.species"), qcol(qd, "vegetation.tree_Tree2.species"),
                                                            qcol(qd, "vegetation.tree_Tree3.species"))),
      ## the form's dominant understory species; when blank, the quadrant
      ## sapling species (most often recorded first)
      understory.species.dominant = or.else(species.list(col(h, "vegetation.understory_dominant.species")),
                                            species.list(quad = list(qcol(qd, "vegetation.sapling_species1"), qcol(qd, "vegetation.sapling_species2"),
                                                                     qcol(qd, "vegetation.sapling_species3")))),
      understory.species.all     = species.list(col(h, "vegetation.understory_dominant.species"), col(h, "vegetation.understory_all.species"),
                                                quad = list(qcol(qd, "vegetation.sapling_species1"), qcol(qd, "vegetation.sapling_species2"),
                                                            qcol(qd, "vegetation.sapling_species3"))),
      tree.dbh.stand             = label(col(h, "vegetation.tree_estimated.DBH"), "dbh_stand"),
      tree.stand.age             = label(col(h, "vegetation.tree_estimated.agerange"), "age_stand"),
      understory.clutter         = label(col(h, "vegetation.understory_clutter"), "understory"),
      understory.clutter.distance = clean(col(h, "vegetation.understory_clutter.distance")),
      landcover                  = label(col(h, "landcover"), "land_cover")
    )
  }
  site.values <- list(); service.rows <- list(); habitat.rows <- list()
  for (s in sites) {
    hab <- if (!is.null(aru.habitat)) aru.habitat[key(aru.habitat$aru.name) == key(s), , drop = FALSE] else NULL
    if (!is.null(hab) && nrow(hab) > 1) {
      if ("date.deployment" %in% names(hab)) hab <- hab[order(hab$date.deployment), , drop = FALSE]
      cat("NOTE: ", nrow(hab), " deployment records for ", s, " - the first is used.\n", sep = "")
    }
    svc <- if (!is.null(aru.service) && "aru.name" %in% names(aru.service))
      aru.service[key(aru.service$aru.name) == key(s), , drop = FALSE] else NULL
    if (!is.null(svc) && nrow(svc) > 0 && "date" %in% names(svc)) svc <- svc[order(svc$date), , drop = FALSE]
    qd <- if (!is.null(aru.quad) && "aru.name" %in% names(aru.quad))
      aru.quad[key(aru.quad$aru.name) == key(s), , drop = FALSE] else NULL

    is.retrieval <- if (!is.null(svc) && nrow(svc) > 0 && "visit.reason" %in% names(svc))
      tolower(clean(svc$visit.reason)) == "retrieval" else logical(if (is.null(svc)) 0 else nrow(svc))
    maint <- if (!is.null(svc)) svc[!is.retrieval, , drop = FALSE] else NULL
    retr  <- if (!is.null(svc) && any(is.retrieval)) svc[max(which(is.retrieval)), , drop = FALSE] else NULL
    svc.dates <- if (!is.null(svc) && nrow(svc) > 0 && "date" %in% names(svc)) svc$date else NULL
    maint.dates <- if (!is.null(maint) && nrow(maint) > 0 && "date" %in% names(maint)) fmt.dt(maint$date) else character(0)
    final.rec <- if (length(svc.dates) > 0 && any(!is.na(svc.dates))) fmt.dt(svc.dates[which.max(svc.dates)]) else ""

    ## 2026-10-27, per Josh: equipment replaced on a visit is added to that
    ## visit's notes as "<old serial> -> <new serial>". The old serial is the
    ## one on the service form, else the one deployed / last put in.
    eqs <- c("aru", "microphone", "power", "hobo")
    cur <- setNames(lapply(eqs, function(e) clean(col(hab, paste0(e, ".serial")))), eqs)
    repl.txt <- if (!is.null(svc)) rep("", nrow(svc)) else character(0)
    for (i in seq_len(if (is.null(svc)) 0 else nrow(svc))) {
      for (e in eqs) {
        old.in <- clean(col(svc, paste0(e, ".serial"), i))
        new    <- clean(col(svc, paste0(e, ".serial_new"), i))
        if (nzchar(old.in) && nzchar(cur[[e]]) && key(old.in) != key(cur[[e]])) {
          add.issue(s, "aru.service", paste0(e, ".serial"), old.in,
                    paste0("differs from the serial deployed / last put in (", cur[[e]], ")"))
        }
        if (nzchar(new)) {
          old <- if (nzchar(old.in)) old.in else if (nzchar(cur[[e]])) cur[[e]] else "unknown"
          repl.txt[i] <- paste(c(repl.txt[i][nzchar(repl.txt[i])], paste(old, "->", new)), collapse = "; ")
          cur[[e]] <- new
        }
      }
    }
    maint.repl <- if (!is.null(svc)) repl.txt[!is.retrieval] else character(0)

    v <- list(
      client          = { x <- first.of("client"); if (nzchar(x)) x else client },
      client.contact  = { x <- first.of("client.contact"); if (nzchar(x)) x else client.contact },
      project.label   = { x <- first.of("project.label"); if (nzchar(x)) x else project.label },
      project.code    = { x <- first.of("project.code"); if (nzchar(x)) x else project.code },
      project.manager = { x <- first.of("project.manager"); if (nzchar(x)) x else project.manager },
      biologist.permitted     = clean(col(hab, "biologist.permitted")),
      permit.federal          = clean(col(hab, "permit.federal")),
      personnel.bri           = people(col(hab, "personnel.bri")),
      date.habitat.assessment = fmt.dt(col(hab, "date.habitat.assessment")),
      date.deployment         = fmt.dt(col(hab, "date.deployment")),
      date.recovery_planned   = or.else(fmt.dt(col(hab, "date.recovery_planned")), target.recovery(col(hab, "date.deployment"))),
      date.recovery_final     = final.rec,
      dates.service           = if (length(maint.dates) > 0) paste(maint.dates, collapse = "; ") else "None",
      survey.type             = label(col(hab, "survey.type"), "survey_type"),
      site.type               = label(col(hab, "site.type"), "site_type"),
      landowner.info          = clean(col(hab, "landowner.info")),
      aru.label               = { x <- clean(col(hab, "aru.label")); if (nzchar(x)) x else s },
      aru.name                = s,
      county                  = clean(col(hab, "county")),
      state                   = label(col(hab, "state"), "state"),
      coordinates             = join(num6(col(hab, "latitude")), num6(col(hab, "longitude"))),
      aru.make.model.serial   = mms(hab, "aru"),
      power.make.model.serial = { x <- mms(hab, "power"); if (nzchar(x)) x else power.default },
      microphone.make.model.serial = mms(hab, "microphone"),
      microphone.height       = fmt.m(col(hab, "microphone.height")),
      microphone.orientation_horizontal = clean(col(hab, "microphone.orientation_horizontal")),
      microphone.orientation_vertical  = clean(col(hab, "microphone.orientation_vertical")),
      system.check_timings    = label(col(hab, "system.check_timings"), "yes_no"),
      system.check_hardware   = label(col(hab, "system.check_hardware"), "yes_no"),
      ## 2026-10-28, per Josh: blank = the standard NABat schedule
      survey.schedule         = or.else(col(hab, "survey.schedule"), survey.schedule.default),
      recovery.date           = if (!is.null(retr)) fmt.dt(col(retr, "date")) else "No retrieval visit recorded",
      recovery.aru            = if (!is.null(retr)) mms(retr, "aru", newer(retr, "aru.serial_new", "aru.serial")) else "",
      recovery.power          = if (!is.null(retr)) {
        x <- mms(retr, "power", newer(retr, "power.serial_new", "power.serial")); if (nzchar(x)) x else power.default
      } else "",
      recovery.microphone     = if (!is.null(retr)) mms(retr, "microphone", newer(retr, "microphone.serial_new", "microphone.serial")) else "",
      ## 2026-10-28, per Josh: blank at recovery -> the deployment values
      recovery.microphone.height = if (is.null(retr)) "" else
        fmt.m(or.else(col(retr, "microphone.height"), col(hab, "microphone.height"))),
      recovery.microphone.orientation_horizontal = if (is.null(retr)) "" else
        or.else(col(retr, "microphone.orientation_horizontal"), col(hab, "microphone.orientation_horizontal")),
      recovery.microphone.orientation_vertical  = if (is.null(retr)) "" else
        or.else(col(retr, "microphone.orientation_vertical"), col(hab, "microphone.orientation_vertical")),
      recovery.status.checks  = label(col(retr, "system.check_status"), "yes_no"),
      recovery.notes          = clean(col(retr, "notes")),
      recovery.system.check_timings  = label(col(retr, "system.check_timings"), "yes_no"),
      recovery.system.check_hardware = label(col(retr, "system.check_hardware"), "yes_no"),
      recovery.survey.schedule       = if (is.null(retr)) "" else
        or.else(col(retr, "survey.schedule"), or.else(col(hab, "survey.schedule"), survey.schedule.default))
    )
    site.values[[s]] <- v
    ## one Habitat Assessment table per deployment (habitat) record
    habitat.rows[[s]] <- if (!is.null(hab) && nrow(hab) > 0) {
      lapply(seq_len(nrow(hab)), function(i) {
        qd.i <- if (!is.null(qd) && "ParentGlobalID" %in% names(qd) && "GlobalID" %in% names(hab)) {
          q2 <- qd[qd$ParentGlobalID %in% hab$GlobalID[i], , drop = FALSE]; if (nrow(q2) > 0) q2 else qd
        } else qd
        habitat.values(hab[i, , drop = FALSE], qd.i)
      })
    } else list(habitat.values(NULL, qd))
    ## 2026-10-28, per Josh: the two distances moved to the Detector Info
    ## table (from the first habitat record)
    site.values[[s]]$site.water.distance  <- fmt.m(habitat.rows[[s]][[1]]$water.resources_distance)
    ## 2026-10-28, per Josh: no distance and "No water resources present at
    ## site" -> "None present"
    if (!nzchar(site.values[[s]]$site.water.distance) &&
        tolower(trimws(habitat.rows[[s]][[1]]$water.resources_site)) == "no water resources present at site") {
      site.values[[s]]$site.water.distance <- "None present"
    }
    site.values[[s]]$site.clutter.distance <- fmt.m(habitat.rows[[s]][[1]]$understory.clutter.distance)
    service.rows[[s]] <- if (!is.null(maint) && nrow(maint) > 0) {
      data.frame(service.date      = fmt.dt(if ("date" %in% names(maint)) maint$date else rep(NA, nrow(maint))),
                 service.personnel = people(if ("personnel.bri" %in% names(maint)) maint$personnel.bri else ""),
                 service.actions   = label(if ("actions.performed" %in% names(maint)) maint$actions.performed else "",
                                           "sitevisit_maintenance_action"),
                 service.notes     = {
                   nt <- clean(if ("notes" %in% names(maint)) maint$notes else rep("", nrow(maint)))
                   ifelse(nzchar(maint.repl), ifelse(nzchar(nt), paste(nt, maint.repl, sep = "; "), maint.repl), nt)
                 },
                 stringsAsFactors = FALSE)
    } else {
      data.frame(service.date = "None", service.personnel = "", service.actions = "", service.notes = "",
                 stringsAsFactors = FALSE)
    }
  }

  ## --- serial-number check against the inventory (2026-10-27) ------------------
  if (!is.null(inv)) {
    ## 2026-10-28, per Josh: a serial number must not appear under more than
    ## one equipment type in the inventory (same-type repeats are only listed
    ## in a NOTE)
    by.serial <- split(inv$.type.label, inv$.key)
    multi <- by.serial[vapply(by.serial, function(t) length(unique(tolower(t))) > 1, logical(1))]
    for (k in names(multi)) {
      add.issue("", "equipment.inventory", "serial", clean(inv[[inv.cols[["serial"]]]][match(k, inv$.key)]),
                paste0("listed under more than one equipment type: ", paste(unique(multi[[k]]), collapse = ", ")))
    }
    same <- names(by.serial)[vapply(by.serial, function(t) length(t) > 1 && length(unique(tolower(t))) == 1, logical(1))]
    if (length(same) > 0) {
      cat("NOTE: serial(s) listed more than once in equipment.inventory (same equipment type): ",
          paste(clean(inv[[inv.cols[["serial"]]]][match(same, inv$.key)]), collapse = ", "), "\n", sep = "")
    }
    for (form in c("aru.habitat", "aru.service")) {
      df <- get(form)
      if (is.null(df) || !("aru.name" %in% names(df))) next
      df <- df[key(df$aru.name) %in% key(sites), , drop = FALSE]
      for (f in grep("serial", names(df), value = TRUE)) {
        vals <- clean(df[[f]])
        for (i in which(nzchar(vals))) {
          j <- match(key(vals[i]), inv$.key)
          if (is.na(j)) {
            add.issue(clean(df$aru.name[i]), form, f, vals[i], "not in equipment.inventory")
          } else if (!is.na(inv$.type[j]) && !is.na(eq.of.text(eq.of.field(f))) &&
                     inv$.type[j] != eq.of.text(eq.of.field(f))) {
            add.issue(clean(df$aru.name[i]), form, f, vals[i],
                      paste0("listed in equipment.inventory as ", inv$.type[j]))
          }
          ## 2026-10-28: equipment not marked working (Retired, Out of
          ## Commission, Missing) in the database
          if (!is.na(j) && !is.na(inv.cols[["status"]])) {
            st <- clean(inv[[inv.cols[["status"]]]][j])
            if (nzchar(st) && tolower(st) != "working") {
              add.issue(clean(df$aru.name[i]), form, f, vals[i], paste0("equipment.inventory status: ", st))
            }
          }
        }
      }
    }
  }
  serial.check <- if (length(serial.issues) > 0) unique(do.call(rbind, serial.issues)) else
    data.frame(aru.name = character(0), form = character(0), field = character(0), serial = character(0),
               issue = character(0), stringsAsFactors = FALSE)
  if (nrow(serial.check) > 0) {
    cat("NOTE: ", nrow(serial.check), " serial-number issue(s) - see $serial.check:\n", sep = "")
    for (k in seq_len(min(10, nrow(serial.check)))) {
      cat("  ", serial.check$aru.name[k], " ", serial.check$field[k], " ", serial.check$serial[k], ": ",
          serial.check$issue[k], "\n", sep = "")
    }
    if (nrow(serial.check) > 10) cat("  ... (", nrow(serial.check) - 10, " more)\n", sep = "")
  }

  if (length(species.merged) > 0) {
    cat("NOTE: species spellings corrected to the spelling recorded most often: ",
        paste(species.merged, collapse = "; "), "\n", sep = "")
  }

  ## --- 4b. photos and figures (2026-10-28) --------------------------------------
  img.keys <- c("photo.detector_setup", "photo.microphone_view", "photo.north", "photo.east",
                "photo.south", "photo.west", "figure.bullseye", "figure.sm4")
  site.images <- setNames(lapply(sites, function(s) list()), sites)
  img.dir <- file.path(tempdir(), paste0("aru.appendix.img.", stamp0 <- format(Sys.time(), "%H%M%OS3")))
  dir.create(img.dir, recursive = TRUE, showWarnings = FALSE)
  has.magick <- requireNamespace("magick", quietly = TRUE)
  ## an image ready for Word (jpg/png, oriented, max 1600 px), or NA + reason
  prep.image <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (has.magick) {
      out <- file.path(img.dir, paste0(sub("\\.[^.]*$", "", basename(f)), "_", sample.int(1e6, 1), ".jpg"))
      ## 2026-10-28: full-size phone photos (12 MP) fill ImageMagick's
      ## pixel cache over a long run ("cache resources exhausted"), so each
      ## image is freed (gc) after use and a failed read is tried once more
      convert.one <- function() {
        im <- magick::image_read(f)
        ## image_auto_orient() is magick >= 2.8; image_orient() before that
        im <- if ("image_auto_orient" %in% getNamespaceExports("magick")) magick::image_auto_orient(im) else magick::image_orient(im)
        im <- magick::image_resize(im, "1600x1600>")
        magick::image_write(im, out, format = "jpeg", quality = 85)
        rm(im)
        ## an empty result (e.g. a photo still syncing) would make
        ## Word/LibreOffice refuse to open the report
        file.exists(out) && file.size(out) > 0
      }
      ok <- tryCatch(convert.one(), error = function(e) FALSE)
      invisible(gc(verbose = FALSE))
      if (!ok) {
        ok <- tryCatch(convert.one(), error = function(e) FALSE)
        invisible(gc(verbose = FALSE))
      }
      if (ok) return(list(path = out, note = ""))
      return(list(path = NA_character_, note = paste0("Photo could not be read: ", basename(f))))
    }
    if (ext %in% c("jpg", "jpeg", "png")) return(list(path = f, note = ""))
    list(path = NA_character_, note = paste0("Photo is ", toupper(ext), " - install the 'magick' package to include it: ",
                                             basename(f)))
  }
  img.dims <- function(f) {
    d <- tryCatch({
      if (has.magick) { info <- magick::image_info(magick::image_read(f)); c(info$width, info$height) }
      else if (grepl("\\.png$", f, ignore.case = TRUE)) dim(png::readPNG(f))[2:1]
      else dim(jpeg::readJPEG(f))[2:1]
    }, error = function(e) c(4, 3))
    as.numeric(d)
  }

  ## photos: <photo.dir>/<site>/..._<view>_... (any sub-folder; "Archive" skipped)
  if (!is.null(photo.dir)) {
    if (!dir.exists(photo.dir)) stop("photo.dir not found: ", photo.dir, call. = FALSE)
    pf <- list.files(photo.dir, recursive = TRUE, full.names = TRUE)
    pf <- pf[grepl("\\.(jpe?g|png|heic|heif|tiff?)$", pf, ignore.case = TRUE)]
    pf <- pf[!grepl("(^|/)archive(/|$)", tolower(dirname(pf)))]
    views <- c(photo.detector_setup = "detector[ _-]*setup", photo.microphone_view = "mic(rophone)?[ _-]*view",
               photo.north = "(^|_)north(_|\\.|$)", photo.east = "(^|_)east(_|\\.|$)",
               photo.south = "(^|_)south(_|\\.|$)", photo.west = "(^|_)west(_|\\.|$)")
    for (s in sites) {
      ## a file belongs to a site when its folder name, or one "_" part of
      ## its file name, is the site name (case/punctuation ignored)
      tok.hit <- vapply(strsplit(basename(pf), "_"), function(tk) key(s) %in% key(tk), logical(1))
      mine <- pf[key(basename(dirname(pf))) == key(s) | tok.hit]
      for (v in names(views)) {
        cand <- mine[grepl(views[[v]], tolower(basename(mine)))]
        ## photos marked "New" are from a later move/re-setup - used only if nothing else
        if (any(!grepl("(^|_)new(_|$)", tolower(basename(cand))))) cand <- cand[!grepl("(^|_)new(_|$)", tolower(basename(cand)))]
        cand <- sort(cand)
        if (length(cand) == 0) { site.values[[s]][[v]] <- "No photo found"; next }
        if (length(cand) > 1) cat("NOTE: ", s, " - ", length(cand), " photos match ", sub("photo.", "", v),
                                  "; using ", basename(cand[1]), "\n", sep = "")
        pi <- prep.image(cand[1])
        if (is.na(pi$path)) { site.values[[s]][[v]] <- pi$note; cat("NOTE: ", s, " - ", pi$note, "\n", sep = ""); next }
        site.images[[s]][[v]] <- list(path = pi$path, width = photo.width)
        site.values[[s]][[v]] <- ""
      }
    }
  } else {
    for (s in sites) for (v in img.keys[1:6]) site.values[[s]][[v]] <- "No photo folder given (photo.dir)"
  }

  ## settings tables for the two plot functions: a data frame or a path;
  ## default = the copy shipped in inst/plotopts
  load.opts <- function(x, file) {
    if (is.null(x)) {
      x <- system.file("plotopts", file, package = "Batz")
      if (!nzchar(x)) x <- system.file("plotopts", file, package = "batz")
      if (!nzchar(x) && file.exists(file)) x <- file          # a copy in the working directory
      if (!nzchar(x)) return(NULL)
    }
    if (is.character(x)) x <- utils::read.csv(x, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
    x
  }
  run.quiet <- function(expr) {
    res <- NULL
    msg <- utils::capture.output(res <- tryCatch(expr, error = function(e) e))
    res
  }

  ## Figures 4 and 5 share one page (2026-10-28, per Josh)
  fig4.max.height <- 3.0
  fig5.max.height <- 3.6
  sm4.yaxis <- function(pp, g) {
    ref <- pp$y.start                               # noon at the start of the night
    half <- pp$bin.minutes * 30                     # half a tile, in seconds
    lo <- hi <- NULL
    if (!is.null(pp$sdb) && nrow(pp$sdb) > 0) {
      lo <- min(pp$sdb$dusk.time, na.rm = TRUE) - 30 * 60
      hi <- max(pp$sdb$dawn.time, na.rm = TRUE) + 30 * 60
    }
    dat <- pp$pd[!is.na(pp$pd$mins.oper) & pp$pd$mins.oper > 0, , drop = FALSE]
    if (nrow(dat) > 0) {
      lo <- min(c(lo, min(dat$tile.time) - half)); hi <- max(c(hi, max(dat$tile.time) + half))
      lo <- as.POSIXct(lo, origin = "1970-01-01", tz = attr(ref, "tzone"))
      hi <- as.POSIXct(hi, origin = "1970-01-01", tz = attr(ref, "tzone"))
    }
    if (is.null(lo) || !is.finite(as.numeric(lo)) || !is.finite(as.numeric(hi))) return(g)
    h.lo <- max(0, floor(as.numeric(difftime(lo, ref, units = "hours")) / 2) * 2)
    h.hi <- min(24, ceiling(as.numeric(difftime(hi, ref, units = "hours")) / 2) * 2)
    lim <- ref + c(h.lo, h.hi) * 3600
    br <- seq(lim[1], lim[2], by = "2 hours")
    lab <- format(br, "%H:%M"); lab[lab == "00:00"] <- "Midnight"; lab[lab == "12:00"] <- "Noon"
    suppressMessages(g + ggplot2::scale_y_datetime(limits = lim, breaks = br, labels = lab, expand = c(0, 0),
                                                   oob = scales::oob_squish, name = "\nTime") +
                       ggplot2::labs(title = NULL))   # 2026-10-28: no site name above the plot
  }

  ## 2026-10-28, per Josh: halve the blank margin under the Figure 4 panels
  trim.bottom <- function(f) {
    if (!requireNamespace("png", quietly = TRUE)) return(invisible(FALSE))
    tryCatch({
      a <- png::readPNG(f, info = TRUE); dpi <- attr(a, "info")$dpi
      rgb <- a[, , 1:3, drop = FALSE]
      used <- which(apply(rgb, 1, function(r) any(r < 0.98)))
      blank <- dim(a)[1] - max(used)
      if (blank > 1) png::writePNG(a[seq_len(dim(a)[1] - floor(blank / 2)), , , drop = FALSE], f,
                                   dpi = dpi)
      invisible(TRUE)
    }, error = function(e) invisible(FALSE))
  }

  ## Figure 4 - batz.plotcover_bullseye() per site
  aes.bull <- load.opts(plotopts.bullseye, "plotopts_bullseye.csv")
  if (!is.null(aes.bull)) {
    ## the mic icons ship in inst/img
    icon <- aes.bull$default.value[aes.bull$parameter == "icon.dir"]
    if (length(icon) == 1 && !dir.exists(icon)) {
      inst.img <- system.file("img", package = "Batz")
      if (!nzchar(inst.img)) inst.img <- system.file("img", package = "batz")
      if (nzchar(inst.img)) aes.bull$default.value[aes.bull$parameter == "icon.dir"] <- inst.img
    }
    ## 2026-10-28: a shorter figure (2.5 in panels, unless plot.height has
    ## its own override) so Figures 4 and 5 fit on one page
    ov <- intersect(c("overide.value", "override.value"), names(aes.bull))
    i.h <- which(aes.bull$parameter == "plot.height")
    if (length(ov) > 0 && length(i.h) == 1 && (is.na(aes.bull[[ov[1]]][i.h]) || !nzchar(trimws(aes.bull[[ov[1]]][i.h])))) {
      aes.bull[[ov[1]]][i.h] <- "2.5"
    }
    ## 2026-10-28, per Josh: Figure 4 panel titles (unless overridden)
    ## (second change the same day: shorter titles, two sizes smaller)
    fig4.titles <- c(mic.panel.title  = "Height and vertical orientation",
                     understory.title = "Direction and Understory Clutter",
                     canopy.title     = "Canopy Cover")
    for (tn in names(fig4.titles)) {
      i.t <- which(aes.bull$parameter == tn)
      if (length(ov) > 0 && length(i.t) == 1 && (is.na(aes.bull[[ov[1]]][i.t]) || !nzchar(trimws(aes.bull[[ov[1]]][i.t])))) {
        aes.bull[[ov[1]]][i.t] <- fig4.titles[[tn]]
      }
    }
  }
  if (!is.null(aru.quad) && !is.null(aru.habitat) && !is.null(aes.bull) &&
      all(c("aru.name", "quadrant", "canopy.cover", "Understory.cover") %in% names(aru.quad))) {
    cover <- data.frame(aru.label = clean(aru.quad$aru.name), quadrant = clean(aru.quad$quadrant),
                        canopy = clean(aru.quad$canopy.cover), understory = clean(aru.quad$Understory.cover),
                        stringsAsFactors = FALSE)
    names(cover) <- c("aru.label", "Select the quadrant you are assessing", "Canopy cover", "Understory cover")
    for (s in sites) {
      h <- aru.habitat[key(aru.habitat$aru.name) == key(s), , drop = FALSE][1, , drop = FALSE]
      cv <- cover[key(cover$aru.label) == key(s), , drop = FALSE]
      if (nrow(cv) == 0) { site.values[[s]]$figure.bullseye <- "No quadrant cover data for this site"; next }
      cv$aru.label <- s
      mic <- data.frame(aru.label = s, h1 = clean(col(h, "microphone.height")),
                        h2 = clean(col(h, "microphone.orientation_horizontal")),
                        h3 = clean(col(h, "microphone.orientation_vertical")), stringsAsFactors = FALSE)
      names(mic) <- c("aru.label", "Microphone Height", "Horizontal Microphone Orientation", "Vertical Microphone Orientation")
      d.out <- file.path(img.dir, paste0("bull.", key(s))); dir.create(d.out, showWarnings = FALSE)
      ## 2026-10-28, per Josh: bar, then Understory, then Canopy Cover; the
      ## microphone-height bullseye panel is left out
      res <- run.quiet(batz.plotcover_bullseye(cv, mic, aes.bull, project.name = "fig4", dir.save = d.out,
                                               sub.plots = c("mic.bar", "understory.bull", "canopy.bull"),
                                               arrow.panel = "first", panel.labels = c("(a)", "(b)", "(c)"),
                                               title.size.adjust = -2))
      png.f <- list.files(d.out, pattern = "\\.png$", full.names = TRUE)
      if (inherits(res, "error") || length(png.f) == 0) {
        site.values[[s]]$figure.bullseye <- paste0("Figure not made: ", if (inherits(res, "error")) conditionMessage(res) else "no plot")
        cat("NOTE: ", s, " - bullseye plot not made\n", sep = "")
      } else {
        trim.bottom(png.f[1])
        site.images[[s]]$figure.bullseye <- list(path = png.f[1], width = 6.5, max.height = fig4.max.height)
        site.values[[s]]$figure.bullseye <- ""
      }
    }
  } else {
    for (s in sites) site.values[[s]]$figure.bullseye <- if (is.null(aes.bull))
      "No plotopts_bullseye.csv found (plotopts.bullseye)" else "No quadrant cover data"
  }

  ## Figure 5 - batz.plotsm4_heatmap() per site, from the merged SM4 log
  aes.sm4 <- load.opts(plotopts.sm4heatmap, "plotopts_sm4heatmap.csv")
  if (!is.null(sm4.data) && !is.null(aes.sm4)) {
    if (is.character(sm4.data)) sm4.data <- special.read.csv(sm4.data, stringsAsFactors = FALSE, check.names = FALSE)
    sm <- as.data.frame(sm4.data, stringsAsFactors = FALSE)
    names(sm) <- standardize.headers(names(sm))
    if ("aru" %in% names(sm) && !("aru_name" %in% names(sm))) names(sm)[names(sm) == "aru"] <- "aru_name"
    if (!all(c("aru_name", "date", "time") %in% names(sm))) stop("sm4.data needs aru.name, date and time columns.", call. = FALSE)
    ## each SM4 log line is one minute of recording: count lines per 15-min
    ## interval of the monitoring night (noon to noon, log times are local)
    dt.txt <- paste(clean(sm$date), clean(sm$time))
    dt <- as.POSIXct(rep(NA_real_, length(dt.txt)), origin = "1970-01-01", tz = "UTC")
    for (fmt in c("%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M")) {
      todo <- is.na(dt)
      if (any(todo)) dt[todo] <- as.POSIXct(dt.txt[todo], format = fmt, tz = "UTC")
    }
    if (any(is.na(dt))) cat("NOTE: ", sum(is.na(dt)), " sm4.data row(s) with an unreadable date/time were skipped\n", sep = "")
    ok <- !is.na(dt) & nzchar(clean(sm$aru_name))
    sm <- sm[ok, , drop = FALSE]; dt <- dt[ok]
    night <- as.Date(dt) - ifelse(as.integer(format(dt, "%H")) < 12, 1, 0)
    m2n <- as.numeric(difftime(dt, as.POSIXct(paste(night, "12:00:00"), tz = "UTC"), units = "mins"))
    sm4.sum <- stats::aggregate(list(mins.oper = rep(1, length(dt))),
                                list(aru.name = clean(sm$aru_name), date.monitoringnight = format(night),
                                     mins2.noon.mon = floor(m2n / 15) * 15), sum)
    sm4.sites <- unique(sm4.sum$aru.name)
    missing.sm4 <- sites[!(key(sites) %in% key(sm4.sites))]
    if (length(missing.sm4) > 0) cat("NOTE: no SM4 log data for: ", paste(missing.sm4, collapse = ", "), "\n", sep = "")
    ## sunrise/sunset for the Dawn/Dusk lines: batz.generate_suntimes.arulist()
    ## run on a temporary arulist built from the deployment coordinates
    have <- sites[key(sites) %in% key(sm4.sites)]
    al <- do.call(rbind, lapply(have, function(s) {
      h <- aru.habitat[key(aru.habitat$aru.name) == key(s), , drop = FALSE][1, , drop = FALSE]
      nm <- sm4.sites[match(key(s), key(sm4.sites))]
      nts <- as.Date(sm4.sum$date.monitoringnight[sm4.sum$aru.name == nm])
      lat <- suppressWarnings(as.numeric(col(h, "latitude"))); lon <- suppressWarnings(as.numeric(col(h, "longitude")))
      if (is.na(lat) || is.na(lon)) return(NULL)
      data.frame(aru.name = nm, longitude = lon, latitude = lat, sunregion = nm, sunregion.longitude = lon,
                 sunregion.latitude = lat, date.start = format(min(nts)), date.end = format(max(nts) + 1),
                 time_zone = time.zone, sunregion.type = "fixed.unique", schedule1 = "", schedule2 = "",
                 stringsAsFactors = FALSE)
    }))
    st <- NULL
    if (!is.null(al) && nrow(al) > 0) {
      al.dir <- file.path(img.dir, "arulist"); dir.create(al.dir, showWarnings = FALSE)
      utils::write.csv(al, file.path(al.dir, "appendix.arulist.csv"), row.names = FALSE)
      st <- run.quiet(batz.generate_suntimes.arulist(dir.load = al.dir, write.output = FALSE))
      st <- if (inherits(st, "error")) NULL else st$aru.suntimes
    }
    if (is.null(st)) {
      st <- data.frame(aru.name = character(0), date = character(0), date.monitoringnight = character(0),
                       sunregion = character(0), time.zone = character(0), sunregion.type = character(0),
                       schedule1 = character(0), schedule2 = character(0), sunset = character(0),
                       sunset.unix = numeric(0), sunrise = character(0), sunrise.unix = numeric(0),
                       sunrise.monitoringnight = character(0), sunrise.monitoringnight.unix = numeric(0))
    }
    for (s in sites) {
      nm <- sm4.sites[match(key(s), key(sm4.sites))]
      if (is.na(nm)) { site.values[[s]]$figure.sm4 <- "No SM4 log data for this site"; next }
      nts <- sort(unique(sm4.sum$date.monitoringnight[sm4.sum$aru.name == nm]))
      fl <- data.frame(plot.type = "sm4.heatmap", plot.name = s, plot.set = nm, date.format = "%m/%d",
                       date.start = nts[1], date.end = nts[length(nts)], xaxe.interval = "6",
                       xaxe.title = "Monitoring Night", yaxe.title = "Time", stringsAsFactors = FALSE)
      d.out <- file.path(img.dir, paste0("sm4.", key(s))); dir.create(d.out, showWarnings = FALSE)
      res <- run.quiet(batz.plotsm4_heatmap(sm4.sum, fl, st, aes.sm4, project.name = "fig5", dir.save = d.out))
      png.f <- list.files(d.out, pattern = "\\.png$", full.names = TRUE)
      ## 2026-10-28, per Josh: the y axis runs from the even hour at or before
      ## (dusk - 30 min) to the even hour at or after (dawn + 30 min) - the
      ## survey schedule - widened if any recorded minutes fall outside it;
      ## re-saved smaller so Figures 4 and 5 share a page
      if (!inherits(res, "error") && length(res$ggplots) > 0) {
        g5 <- tryCatch(sm4.yaxis(res$plots[[1]], res$ggplots[[1]]), error = function(e) NULL)
        if (!is.null(g5)) {
          f5 <- file.path(d.out, paste0("fig5_", key(s), "_appendix.png"))
          ok5 <- tryCatch({ ggplot2::ggsave(f5, g5, width = 9.5, height = 5.2, units = "in", dpi = 200); TRUE },
                          error = function(e) FALSE)
          if (ok5) png.f <- f5
        }
      }
      if (inherits(res, "error") || length(png.f) == 0) {
        site.values[[s]]$figure.sm4 <- paste0("Figure not made: ", if (inherits(res, "error")) conditionMessage(res) else "no plot")
        cat("NOTE: ", s, " - SM4 heatmap not made\n", sep = "")
      } else {
        site.images[[s]]$figure.sm4 <- list(path = png.f[1], width = 6.5, max.height = fig5.max.height)
        site.values[[s]]$figure.sm4 <- ""
      }
    }
  } else {
    for (s in sites) site.values[[s]]$figure.sm4 <- if (is.null(sm4.data)) "No SM4 log data given (sm4.data)" else
      "No plotopts_sm4heatmap.csv found (plotopts.sm4heatmap)"
  }

  ## --- 5. fill the template ----------------------------------------------------
  W <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  fill.text <- function(nodes, vals) {
    for (nd in nodes) {
      ts <- xml2::xml_find_all(nd, ".//w:t[contains(., '{{')]", ns = W)
      for (t in ts) {
        txt <- xml2::xml_text(t)
        for (k in names(vals)) txt <- gsub(paste0("{{", k, "}}"), vals[[k]], txt, fixed = TRUE)
        txt <- gsub("\\{\\{[^}]*\\}\\}", "", txt)
        xml2::xml_set_text(t, txt)
        xml2::xml_set_attr(t, "xml:space", "preserve")
      }
    }
  }
  fill.service <- function(nodes, rows) {
    for (nd in nodes) {
      tr <- xml2::xml_find_first(nd, ".//w:tr[.//w:t[contains(., '{{service.date}}')]]", ns = W)
      if (inherits(tr, "xml_missing")) next
      trs <- list(tr)
      if (nrow(rows) > 1) for (i in 2:nrow(rows)) {
        trs[[i]] <- xml2::xml_add_sibling(trs[[i - 1]], tr, .where = "after", .copy = TRUE)
      }
      for (i in seq_len(nrow(rows))) fill.text(list(trs[[i]]), as.list(rows[i, , drop = FALSE]))
    }
  }
  ## an image replaces the run holding its {{placeholder}}
  img.ns <- paste0('<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
                   'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" ',
                   'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
                   'xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" ',
                   'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">')
  fill.images <- function(nodes, s) {
    for (k in names(site.images[[s]])) {
      im <- site.images[[s]][[k]]
      d <- img.dims(im$path)
      w <- im$width; h <- w * d[2] / d[1]
      if (k %in% img.keys[1:6] && h > 3.5) { h <- 3.5; w <- h * d[1] / d[2] }   # tall photos
      if (!is.null(im$max.height) && h > im$max.height) { h <- im$max.height; w <- h * d[1] / d[2] }
      wml <- officer::to_wml(officer::external_img(src = im$path, width = w, height = h))
      run <- xml2::xml_find_first(xml2::read_xml(paste0(img.ns, wml, "</w:p>")), "//w:r", ns = W)
      for (nd in nodes) {
        for (t in xml2::xml_find_all(nd, sprintf(".//w:t[contains(., '{{%s}}')]", k), ns = W)) {
          r <- xml2::xml_parent(t)
          xml2::xml_add_sibling(r, run, .where = "after", .copy = TRUE)
          xml2::xml_set_text(t, "")
        }
      }
    }
  }
  ## one Habitat Assessment table per habitat record
  fill.habitat <- function(nodes, s) {
    rows <- habitat.rows[[s]]
    for (nd in nodes) {
      tb <- if (xml2::xml_name(nd) == "tbl" && length(xml2::xml_find_all(nd, ".//w:t[contains(., '{{hab.assessment.date}}')]", ns = W)) > 0) nd else NULL
      if (is.null(tb)) next
      tbs <- list(tb)
      if (length(rows) > 1) for (i in 2:length(rows)) {
        sp <- xml2::xml_add_sibling(tbs[[i - 1]], "w:p", .where = "after")
        tbs[[i]] <- xml2::xml_add_sibling(sp, tb, .where = "after", .copy = TRUE)
      }
      for (i in seq_along(rows)) fill.text(list(tbs[[i]]), rows[[i]])
    }
  }
  fill.site <- function(nodes, s) {
    fill.habitat(nodes, s)
    fill.images(nodes, s)
    fill.service(nodes, service.rows[[s]])
    fill.text(nodes, site.values[[s]])
  }
  body.parts <- function(doc) {
    body <- xml2::xml_find_first(officer::docx_body_xml(doc), "/w:document/w:body", ns = W)
    kids <- xml2::xml_children(body)
    list(body = body, sect = kids[xml2::xml_name(kids) == "sectPr"][[1]],
         content = kids[xml2::xml_name(kids) != "sectPr"])
  }
  safe <- function(x) gsub("^\\.+|\\.+$", "", gsub("[^A-Za-z0-9.-]+", ".", x))
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  files <- character(0)

  to.pdf <- function(docx.path) {
    pdf.path <- sub("\\.docx$", ".pdf", docx.path)
    if (requireNamespace("doconv", quietly = TRUE)) {
      ok <- tryCatch({ doconv::to_pdf(docx.path, output = pdf.path); TRUE }, error = function(e) FALSE)
      if (ok && file.exists(pdf.path)) return(pdf.path)
    }
    soffice <- Sys.which(c("soffice", "libreoffice"))
    soffice <- soffice[nzchar(soffice)]
    if (length(soffice) == 0) {
      win <- c("C:/Program Files/LibreOffice/program/soffice.exe",
               "C:/Program Files (x86)/LibreOffice/program/soffice.exe")
      soffice <- win[file.exists(win)]
    }
    if (length(soffice) > 0) {
      ## LibreOffice adds the symbol to a footnote itself, so its copy has the
      ## footnotes' own symbol text removed (otherwise it shows twice). Word
      ## (doconv, above) needs the text, so the .docx itself keeps it.
      lo.dir <- tempfile("aru.appendix.pdf"); dir.create(lo.dir)
      lo.docx <- file.path(lo.dir, basename(docx.path))
      d2 <- officer::read_docx(docx.path)
      fx <- d2$footnotes$get()
      xml2::xml_remove(xml2::xml_find_all(
        fx, "//w:footnote//w:r[w:rPr/w:rStyle[@w:val='FootnoteReference']][w:t]", ns = W))
      print(d2, target = lo.docx)
      args <- c("--headless", "--convert-to", "pdf", "--outdir", shQuote(lo.dir), shQuote(lo.docx))
      ## on Linux/macOS R's own LD_LIBRARY_PATH can stop LibreOffice starting
      if (.Platform$OS.type == "unix") {
        system2("env", c("-u", "LD_LIBRARY_PATH", shQuote(unname(soffice[1])), args), stdout = FALSE, stderr = FALSE)
      } else {
        system2(unname(soffice[1]), args, stdout = FALSE, stderr = FALSE)
      }
      lo.pdf <- sub("\\.docx$", ".pdf", lo.docx)
      if (file.exists(lo.pdf)) { file.copy(lo.pdf, pdf.path, overwrite = TRUE); unlink(lo.dir, recursive = TRUE) }
      if (file.exists(pdf.path)) return(pdf.path)
    }
    warning("PDF not written for ", basename(docx.path),
            " - install the 'doconv' package (uses Word) or LibreOffice.", call. = FALSE)
    NA_character_
  }
  ## 2026-10-27, per Josh: footnotes are marked with symbols instead of
  ## numbers, and footnotes with the same text share the same symbol (e.g.
  ## the staff list note on Personnel and on Staff). Symbols are given in
  ## order of first use: * dagger double-dagger section pilcrow double-bar,
  ## then doubled (** ...).
  fn.symbols <- c("*", "\u2020", "\u2021", "\u00a7", "\u00b6", "\u2016")
  symbolize.footnotes <- function(doc) {
    body.xml <- officer::docx_body_xml(doc)
    fn.xml <- doc$footnotes$get()
    refs <- xml2::xml_find_all(body.xml, "//w:footnoteReference", ns = W)
    ## drop footnotes nothing refers to (e.g. one left behind when a table was
    ## deleted from the template) - LibreOffice pairs notes by order and
    ## would shift every later note by one
    used <- xml2::xml_attr(refs, "id")
    for (f in xml2::xml_find_all(fn.xml, "//w:footnote", ns = W)) {
      id <- suppressWarnings(as.integer(xml2::xml_attr(f, "id")))
      if (!is.na(id) && id > 0 && !(as.character(id) %in% used)) xml2::xml_remove(f)
    }
    ## 2026-10-27, per Josh: no footnote text is bold
    xml2::xml_remove(xml2::xml_find_all(fn.xml, "//w:footnote//w:rPr/w:b | //w:footnote//w:rPr/w:bCs", ns = W))
    if (length(refs) == 0) return(invisible(doc))
    fn.text <- function(id) {
      f <- xml2::xml_find_first(fn.xml, sprintf("//w:footnote[@w:id='%s']", id), ns = W)
      if (inherits(f, "xml_missing")) NA_character_ else trimws(xml2::xml_text(f))
    }
    texts <- vapply(xml2::xml_attr(refs, "id"), fn.text, character(1))
    uniq <- unique(texts[!is.na(texts)])
    sym.of <- function(i) strrep(fn.symbols[(i - 1) %% length(fn.symbols) + 1], (i - 1) %/% length(fn.symbols) + 1)
    syms <- vapply(seq_along(uniq), sym.of, character(1))
    for (i in seq_along(refs)) {
      if (is.na(texts[i])) next
      sym <- syms[match(texts[i], uniq)]
      ref <- refs[[i]]
      xml2::xml_set_attr(ref, "w:customMarkFollows", "1")
      t <- xml2::xml_add_sibling(ref, "w:t", .where = "after")
      xml2::xml_set_text(t, sym)
      ## the mark inside the footnote itself
      f <- xml2::xml_find_first(fn.xml, sprintf("//w:footnote[@w:id='%s']", xml2::xml_attr(ref, "id")), ns = W)
      for (fr in xml2::xml_find_all(f, ".//w:footnoteRef", ns = W)) {
        ft <- xml2::xml_add_sibling(fr, "w:t", .where = "after")
        xml2::xml_set_text(ft, sym)
        xml2::xml_remove(fr)
      }
    }
    invisible(doc)
  }
  write.out <- function(doc, base) {
    symbolize.footnotes(doc)
    docx.path <- file.path(if (isTRUE(word)) dir.save else tempdir(), paste0(base, ".docx"))
    print(doc, target = docx.path)
    out <- character(0)
    if (isTRUE(word)) out <- c(out, docx.path)
    if (isTRUE(pdf)) {
      p <- to.pdf(docx.path)
      if (!is.na(p)) {
        if (!isTRUE(word)) { file.copy(p, file.path(dir.save, basename(p)), overwrite = TRUE); p <- file.path(dir.save, basename(p)) }
        out <- c(out, p)
      }
      if (!isTRUE(word)) unlink(docx.path)
    }
    for (f in out) cat("Saved:", basename(f), "\n")
    out
  }

  if (isTRUE(separate)) {
    for (s in sites) {
      doc <- officer::read_docx(template)
      fill.site(body.parts(doc)$content, s)
      files <- c(files, write.out(doc, paste0(safe(proj.label.file), "_", safe(s), "_", stamp)))
    }
  }

  ## captions start with "Figure <n>:"; renumber.figures() adds an offset
  fig.caption.ts <- function(nodes) unlist(lapply(nodes, function(nd)
    as.list(xml2::xml_find_all(nd, ".//w:t[starts-with(normalize-space(.), 'Figure ')]", ns = W))), recursive = FALSE)
  cap.txt <- vapply(fig.caption.ts(body.parts(officer::read_docx(template))$content), xml2::xml_text, "")
  n.fig.template <- length(unique(sub("^\\s*Figure ([0-9]+).*$", "\\1", cap.txt[grepl("^\\s*Figure [0-9]+", cap.txt)])))
  renumber.figures <- function(nodes, offset) {
    for (t in fig.caption.ts(nodes)) {
      tx <- xml2::xml_text(t)
      m <- regmatches(tx, regexec("^(\\s*Figure )([0-9]+)(.*)$", tx))[[1]]
      if (length(m) == 4) xml2::xml_set_text(t, paste0(m[2], as.integer(m[3]) + offset, m[4]))
    }
  }

  if (isTRUE(collate)) {
    doc <- officer::read_docx(template)
    bp <- body.parts(doc)
    ## a second, untouched copy of the template supplies fresh nodes per site
    pristine <- body.parts(officer::read_docx(template))$content
    fn.xml <- doc$footnotes$get()
    xml2::xml_remove(bp$content)
    next.fn <- function() {
      ids <- suppressWarnings(as.integer(xml2::xml_attr(xml2::xml_find_all(fn.xml, "//w:footnote", ns = W), "id")))
      max(c(0L, ids), na.rm = TRUE) + 1L
    }
    for (i in seq_along(sites)) {
      if (i > 1) {
        ## 2026-10-28: the page break goes at the end of the previous site's
        ## last paragraph - a separate break paragraph could spill onto a
        ## page of its own when Figure 5 fills the page (a blank page)
        p <- xml2::xml_find_first(bp$sect, "preceding-sibling::w:p[1]", ns = W)
        if (inherits(p, "xml_missing")) p <- xml2::xml_add_sibling(bp$sect, "w:p", .where = "before")
        r <- xml2::xml_add_child(p, "w:r")
        br <- xml2::xml_add_child(r, "w:br")
        xml2::xml_set_attr(br, "w:type", "page")
      }
      added <- lapply(pristine, function(n) xml2::xml_add_sibling(bp$sect, n, .where = "before", .copy = TRUE))
      if (i > 1) {
        ## each copy gets its own footnotes and drawing ids, so Word sees unique ids
        for (nd in added) {
          for (ref.node in xml2::xml_find_all(nd, ".//w:footnoteReference", ns = W)) {
            old <- xml2::xml_attr(ref.node, "id")
            src <- xml2::xml_find_first(fn.xml, sprintf("//w:footnote[@w:id='%s']", old), ns = W)
            if (inherits(src, "xml_missing")) next
            new.id <- next.fn()
            cp <- xml2::xml_add_sibling(xml2::xml_find_all(fn.xml, "//w:footnote", ns = W)[[length(xml2::xml_find_all(fn.xml, "//w:footnote", ns = W))]],
                                        src, .where = "after", .copy = TRUE)
            xml2::xml_set_attr(cp, "w:id", as.character(new.id))
            xml2::xml_set_attr(ref.node, "w:id", as.character(new.id))
          }
          for (dp in xml2::xml_find_all(nd, ".//*[local-name()='docPr']")) {
            xml2::xml_set_attr(dp, "id", as.character(1000L * i + as.integer(xml2::xml_attr(dp, "id"))))
          }
        }
      }
      fill.site(added, sites[i])
      ## 2026-10-28, per Josh: figure numbers run on through the collated
      ## file (site 2's Figure 1 becomes Figure 6, ...)
      if (i > 1) renumber.figures(added, n.fig.template * (i - 1))
    }
    files <- c(files, write.out(doc, paste0(safe(proj.label.file), "_Siteappendixall_", stamp)))
  }

  site.table <- do.call(rbind, lapply(names(site.values), function(s)
    as.data.frame(site.values[[s]], stringsAsFactors = FALSE, check.names = FALSE)))
  habitat.table <- do.call(rbind, lapply(names(habitat.rows), function(s) do.call(rbind, lapply(habitat.rows[[s]], function(h)
    data.frame(aru.name = s, as.data.frame(h, stringsAsFactors = FALSE, check.names = FALSE),
               stringsAsFactors = FALSE, check.names = FALSE)))))
  invisible(list(sites = site.table, habitat = habitat.table, files = files, serial.check = serial.check,
                 aru.habitat = aru.habitat, aru.quad = aru.quad, aru.service = aru.service))
}

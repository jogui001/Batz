#' Generate the Batz package's reference table of deprecated/renamed headers
#'
#' Reads and returns the package's master header-rename reference table -
#' every raw/legacy column-name spelling (\code{$header.old}) that has been
#' phased out somewhere in the \code{batz} package, what it was (or is
#' meant to be) replaced with (\code{$header.new}), and which function(s)
#' accepted it as an input (\code{$functions.in}) or produced it as an
#' output (\code{$functions.out}). This is the single source of truth
#' \code{\link{batz.datawrangler_headers.acceptold}} consults to silently
#' fix up an old header spelling on load, so existing files/scripts built
#' against an older \code{batz} naming convention keep working.
#'
#' @param dir.load Character, default \code{getwd()}. Directory containing
#'   the master reference CSV (\code{file.name}).
#' @param file.name Character, default \code{"batz_headers_acceptold.csv"}.
#'   Name of the reference CSV to read.
#'
#' @return A data frame with \code{$header.old} (the phased-out header
#'   name, in its own original spelling/casing), \code{$header.new} (its
#'   current replacement name - \strong{blank for a header still flagged
#'   pending review, see Details}), \code{$functions.in} (a
#'   semicolon-separated list of every \code{batz} function that accepts
#'   \code{$header.old} as an input column, blank if none), and
#'   \code{$functions.out} (a semicolon-separated list of every
#'   \code{batz} function that produces \code{$header.old} as an output
#'   column, blank if none).
#'
#' @details
#' \strong{Built 2026-09-25, per Josh's uploaded reference workbook
#' ("Batz reference db update 20260925.csv") and his request to
#' standardize header names package-wide wherever that workbook's own
#' \code{$change.to} column had an entry.} That workbook is this
#' function's own source data, reduced to just the rows where
#' \code{$change.to} named a real rename (a row whose \code{$change.to}
#' was blank, or literally repeated its own \code{$name.standard}, meant
#' "already correct, no change" and isn't a row here at all).
#'
#' \strong{8 of the 41 real renames in Josh's workbook were originally
#' shipped here with \code{$header.new} deliberately left BLANK - flagged
#' for Josh's review, not silently applied - because applying them as
#' given would either collide with an already-distinct existing column,
#' silently merge two fields that may not actually be the same thing, or
#' (in one case) looked like a data-entry/row-shift error in the workbook
#' itself. \code{\link{batz.datawrangler_headers.acceptold}} treats a
#' blank \code{$header.new} as "recognized old header, no confirmed
#' replacement yet" - it never renames a column to a blank target, and
#' always warns (logging \code{header.new = "NOMATCH"} when
#' \code{log.file = TRUE}) so these stay visible rather than silently
#' doing nothing.}
#'
#' \strong{Update, 2026-09-25: 7 of the 8 are now resolved, 1 remains
#' flagged.} Josh's original follow-up gave six numbered directives; items
#' 1-5 resolved five of these rows (and one package-wide code rename), item
#' 6 added a review-before-update process to this function going forward
#' (see the very end of these Details). A second follow-up the same day
#' resolved a sixth row (\code{mic_serial_number}/
#' \code{serial_number_of_microphone}), after Claude demonstrated the
#' collision-detection code path Josh asked to see.
#' \itemize{
#'   \item \strong{RESOLVED: \code{mon.ngh}, \code{monitoringnight}, and
#'     \code{monnight.date} -> \code{date.monitoringnight}} (Josh's items
#'     1-3: "I changed my mine and want to use date.monitoringnight instead
#'     of date.mon to be more consistent with collaborators" / "same thing
#'     make the change" x2). This also resolves the urgent
#'     \code{date.monitoringnight}-vs-\code{date.mon} question this
#'     function's Details previously flagged as blocking: \code{date.mon}
#'     is now a RETIRED spelling, superseded package-wide by
#'     \code{date.monitoringnight} - not the other way around. Every
#'     \code{batz} function that used to output/require \code{$date.mon}
#'     (\code{\link{batz.plotactivity_daily.count}},
#'     \code{\link{batz.plotactivity_heatmap}},
#'     \code{\link{batz.generate_suntimes.arulist}},
#'     \code{\link{batz.generate_plotframe.bat}},
#'     \code{\link{batz.merge_vetted.acoustics}}) and
#'     \code{\link{batz.merge_vetted.acoustics2}} (which used \code{$mon.ngh})
#'     have all been updated to \code{$date.monitoringnight} in this same
#'     round, so the package now agrees on one name for this field
#'     end to end. \strong{One dependency this rename does NOT reach:}
#'     \code{\link{batz.merge_vetted.acoustics2}}'s own
#'     \code{arumerge.headerrename.csv} (an external file, read at runtime
#'     via its \code{header.rename.path} parameter, not one of this
#'     package's \code{R}/\code{man} files) still maps raw headers to the
#'     OLD \code{mon.ngh} target as of this round - see that function's own
#'     round-twenty-five \code{@details} entry for what needs to change
#'     there before it will work against real data again.
#'   \item \strong{RESOLVED: \code{suns} -> \code{sunset} (new row) and
#'     \code{suns.unix} -> \code{sunset.unix}; \code{survey_type} removed
#'     entirely (no rename)} (Josh's item 4: "there was a shift should be:
#'     suns -> sunset; sunset.unix -> suns.unix"). Read literally this
#'     wording is internally reversed (it would have \code{sunset.unix}
#'     - which doesn't exist as a raw header anywhere - renamed TO
#'     \code{suns.unix}), so rather than guess, the original uploaded
#'     workbook (\code{Batz_reference_db_update_20260925.csv}) was
#'     re-inspected directly: it shows \code{suns} was the row genuinely
#'     missing a \code{$change.to} entry (should be \code{sunset}, a new
#'     row - filling what was previously an entirely absent row, not one
#'     of the original 8 blanks), \code{suns.unix} should be
#'     \code{sunset.unix} (this row WAS one of the 8 original blanks -
#'     matching every sibling \code{sunr*} rename's \code{.unix}-preserving
#'     pattern, e.g. \code{sunr.unix -> sunrise.unix}), and
#'     \code{survey_type} (a \code{\link{batz.generate_arumeta.eventlog}}
#'     column with no connection to solar times, described in the workbook
#'     as "Type of survey being conducted") does not change at all - its
#'     row is removed from this table entirely rather than kept with a
#'     blank/self-mapping, since it was never a real deprecated header
#'     candidate to begin with (a row-shift artifact in the original
#'     workbook). \strong{This is Claude's resolved reading of an
#'     internally-inconsistent instruction, verified against the actual
#'     underlying workbook data rather than guessed - please double-check
#'     it's right.}
#'   \item \strong{RESOLVED: \code{obs} -> \code{observations_count}}
#'     (Josh's item 5: "Change my mind obs -> species_count change to
#'     obs -> observations_count"), superseding the previously-shipped
#'     \code{obs -> species_count} target. \code{$functions.in}/
#'     \code{$functions.out} for this row are unchanged - only the target
#'     name itself changed.
#'   \item \strong{RESOLVED: \code{mic_serial_number} and
#'     \code{serial_number_of_microphone} -> \code{microphone.serial},
#'     both} (Josh, 2026-09-25 follow-up: confirmed these are the same
#'     underlying field - a microphone's serial number, both raw columns
#'     of \code{\link{batz.generate_arumeta.eventlog}} - and picked
#'     \code{microphone.serial} as the shared target, matching this
#'     package's dot-style convention for \code{aru.serial}). Demonstrated
#'     first, not just applied: a synthetic file carrying BOTH legacy
#'     spellings at once (plausible for an older eventlog sheet that picked
#'     up the field under one name and later got a second column added
#'     under the other, never cleaned up) was fed through
#'     \code{\link{batz.datawrangler_headers.acceptold}} with both rows
#'     pointed at \code{microphone.serial} - the same-target collision
#'     check (see that function's own \code{@details}) caught it exactly
#'     as designed: both columns left unchanged, warned, and logged with
#'     \code{header.new = "microphone.serial"} rather than being silently
#'     merged or one overwriting the other. That safety net only fires
#'     case-by-case, on whichever file actually has both columns present -
#'     it won't retroactively fix data already merged some other way.
#'   \item \strong{STILL FLAGGED, pending Josh's review (not addressed
#'     yet):}
#'     \itemize{
#'       \item \code{project_code} -> (blank). \code{project.code} already
#'         exists as its OWN distinct standard column in
#'         \code{\link{batz.generate_arumeta.eventlog}} - the workbook's own
#'         description for it reads "Project code (dot-style output
#'         spelling, \strong{distinct from} project_code)". Renaming the
#'         raw \code{project_code} input to \code{project.code} would merge
#'         two columns the workbook itself says are meant to stay separate,
#'         inside the very same function. Please confirm whether
#'         \code{project.code} should absorb \code{project_code} after all,
#'         or whether \code{project_code} needs a different target name.
#'     }
#' }
#'
#' \strong{Process change (round twenty-five), 2026-09-25, per Josh's item
#' 6} ("batz.generate_headers.acceptold() should keep an internal database
#' of changes and update whenever we change anything. add one step in for
#' these updates, before they are pushed through, list them and ask if I
#' want to update the table"). \strong{This function's own reference table
#' (\code{batz_headers_acceptold.csv}) already IS that internal database} -
#' every resolution above (items 1-5) was applied by editing that one CSV
#' directly, so there's no separate database to keep in sync. What item 6
#' adds is a PROCESS commitment for every future change to this table,
#' regardless of who or what triggers it: before any row is added, removed,
#' or has its \code{$header.new} target changed, the proposed change(s) are
#' listed explicitly (old value -> new value, and which row) and Josh
#' confirms before they're written to the CSV - the same pattern already
#' used above for the 3 still-flagged rows, now made a standing rule rather
#' than a one-off. This is a process note for how this table gets
#' maintained going forward, not a new parameter or code path on
#' \code{batz.generate_headers.acceptold()} itself, which remains a plain
#' read of whatever the CSV currently says.
#'
#' \strong{A lighter, non-blocking note (not held back, applied as
#' given): \code{plot.sets -> plot.set}} means
#' \code{\link{batz.plotactivity_observations}}'s own job-sheet column
#' (currently allowing more than one site/ARU value) and
#' \code{\link{batz.plotdetections_first.last}}'s job-sheet column
#' (a single site/ARU value) end up sharing the exact same column name
#' across their two, never-combined job-sheet CSVs - each function still
#' reads/parses its own sheet independently, so this doesn't create a
#' runtime collision, just worth knowing the same name now carries
#' different cardinality in each function's own job sheet.
#'
#' @seealso \code{\link{batz.datawrangler_headers.acceptold}}, which
#'   applies this table's renames (and reports on anything it can't
#'   resolve) when a file/data frame is loaded into a \code{batz}
#'   function.
#'
#' @examples
#' \dontrun{
#' headers.table <- batz.generate_headers.acceptold()
#' head(headers.table)
#'
#' # rows still pending Josh's review (see @details)
#' subset(headers.table, header.new == "")
#' }
#'
#' @export
batz.generate_headers.acceptold <- function(dir.load = getwd(),
                                             file.name = "batz_headers_acceptold.csv") {

  path <- file.path(dir.load, file.name)
  if (!file.exists(path)) {
    stop("batz.generate_headers.acceptold(): reference file not found at: ", path,
         " - expected the package's own batz_headers_acceptold.csv (ships alongside the R/ source, see dir.load/file.name).")
  }

  tbl <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                          colClasses = "character")

  required <- c("header.old", "header.new", "functions.in", "functions.out")
  missing <- setdiff(required, names(tbl))
  if (length(missing) > 0) {
    stop("batz.generate_headers.acceptold(): reference file at ", path,
         " is missing these column(s): ", paste(missing, collapse = ", "))
  }

  tbl <- tbl[, required, drop = FALSE]

  ## defensive: NA/whitespace-only in any column collapses to a clean "" -
  ## a blank $header.new means "flagged, pending review" (see @details);
  ## blank $functions.in/$functions.out just mean "not accepted/produced
  ## as that role anywhere".
  for (col in required) {
    tbl[[col]] <- ifelse(is.na(tbl[[col]]), "", trimws(tbl[[col]]))
  }

  tbl
}

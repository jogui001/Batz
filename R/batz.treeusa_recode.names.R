#' Recode Maine tree/shrub identifiers between scientific name, common names, and other reference fields
#'
#' Given a vector containing any mix of scientific (Latin) name or either of
#' two common names for a Maine tree or shrub species, looks each value up
#' in a reference database loaded from disk and returns it re-expressed in
#' one or more requested output headers (\code{head.out}). Matching ignores
#' case and all whitespace/punctuation, so formatting differences between
#' the input and the reference table (or between different inputs) don't
#' cause a false mismatch. Shares its core lookup/pass-through/reporting
#' logic with \code{\link{batz.batusa_recode.names}}, generalized to load
#' its reference table from disk (rather than an embedded table) and to
#' support more than one output header at once.
#'
#' As of 2026-09-14 (per Josh), the base reference table can optionally be
#' supplemented with one or more additional CSV files found on disk -
#' see \code{reference.data}/\code{pattern} below and \strong{"Supplemental
#' reference data (reference.data/pattern)"} in Details.
#'
#' @param data A vector of tree/shrub identifiers to recode. Must be a plain
#'   vector, not a data frame (unlike \code{batz.batusa_recode.names}, which
#'   accepts either).
#' @param head.out Character vector, default \code{"common.name"}. One or
#'   more of the reference database's own column names to return. If a
#'   single header is requested, the return value is a plain vector; if
#'   more than one, the return value is a data frame with one column per
#'   requested header, in the same row order as \code{data}. Matching an
#'   input element to a reference row always uses \code{scientific_name}/
#'   \code{common.name}/\code{common.name2} only, regardless of what's
#'   requested in \code{head.out}. An unrecognized header is an error.
#' @param dir.load Character. Directory to search for the base reference
#'   database file (\code{load.pattern}) and, when \code{reference.data !=
#'   "default"}, for supplemental files (\code{pattern}). Default
#'   \code{getwd()}.
#' @param load.pattern Character, default
#'   \code{c("*USA.treeshrub_recode.names*", "*tree_species_and_shrubs*")}
#'   (wildcard/glob pattern(s), converted internally to a regex via
#'   \code{utils::glob2rx()}). See \strong{Details} for why this now
#'   searches for more than one literal file name.
#' @param dir.sub Logical, default \code{TRUE} (changed from \code{FALSE},
#'   per Josh, 2026-09-14 - see Details). If \code{TRUE}, also search
#'   subdirectories of \code{dir.load} - for \code{load.pattern} and, when
#'   applicable, for \code{pattern}.
#' @param pattern Character, default \code{"plant.names.csv"}. One or more
#'   (a vector is fine) file name(s)/glob pattern(s) identifying
#'   supplemental reference CSVs to search for in \code{dir.load} (and its
#'   subdirectories, if \code{dir.sub = TRUE}) when \code{reference.data !=
#'   "default"}. Ignored entirely when \code{reference.data = "default"}.
#' @param reference.data Character, default \code{"default"}. One of:
#'   \code{"default"} - use only the base reference table loaded via
#'   \code{load.pattern}, exactly as before this feature was added;
#'   \code{"append"} - also search for and load every file matching
#'   \code{pattern}, and add their (validated/reconciled) rows onto the
#'   base table; \code{"overwrite"} - also search for and load every file
#'   matching \code{pattern}, but use ONLY those rows in place of the base
#'   table (see Details for the fallback when nothing usable is found).
#'
#' @return A vector (if \code{head.out} has length 1) or data frame (if
#'   \code{head.out} has length > 1) of the same length/row count as
#'   \code{data}, with every element re-expressed in the requested
#'   header(s). An input element with no match anywhere in the reference
#'   table is returned unchanged (not \code{NA}, no error) in every
#'   requested header.
#'
#' @details
#' \strong{Header standardization (per Josh, 2026-09-14 project preference).}
#' The reference database's own column names are a literal, uninvented copy
#' of that file's real header text (not a \code{batz}-invented shorthand), so
#' they're run through the shared package helper \code{standardize.headers()}
#' (trim whitespace, collapse every run of non-alphanumeric characters to a
#' single underscore, lowercase) immediately after loading, the same as any
#' other loaded file's headers - both for the base reference file and for
#' every supplemental file read under \code{reference.data = "append"/
#' "overwrite"}. This is a no-op against the CURRENT real reference file for
#' every column except \code{common_one}/\code{common_two} - see
#' \strong{"Column identifiers renamed, 2026-09-29"} below for why those two
#' need an extra legacy-input-alias step after standardization. \code{match.cols}
#' and the default \code{head.out} are written as their already-standardized
#' spellings, so no further change was needed there; if the reference file's
#' real headers ever change to something standardize.headers() would alter,
#' \code{head.out} should be called with the new standardized spelling.
#'
#' \strong{Reference file location - real naming mismatch, flagged not
#' silently fixed:} the original spec named the search pattern
#' \code{"maine_tree_species_and_shrubs"} and said the file lives in a
#' "reference database files" folder. On Josh's real "4 Current  test
#' data" folder, a file named exactly \code{maine_tree_species_and_shrubs.csv}
#' does exist, but it sits at the top level of that folder - the copy
#' actually inside "reference database files" is named
#' \code{tree_species_and_shrubs.csv} (missing the \code{"maine_"} prefix;
#' confirmed byte-for-byte identical content to the other copy).
#' \strong{Renamed 2026-09-14, per Josh:} the internal reference data frame
#' is now called \code{reference.plants} (previously \code{reference}),
#' loaded from a file named \code{"USA.treeshrub_recode.names.csv"} -
#' matching the naming convention already used elsewhere in this package
#' (\code{NAbat.names.csv}, \code{USAstates.names.csv} - see
#' \code{\link{batz.batusa_list.species}}). Since this is a THIRD name for
#' what appears to be the same underlying reference table, \code{load.pattern}'s
#' default was widened to a vector matching \emph{either} the new canonical
#' name or the previously-established real file name(s), so the function
#' keeps working with whichever copy Josh actually has in \code{dir.load}
#' at the time. \strong{Please confirm with Josh which single name should
#' be treated as canonical going forward} (and, if a rename is wanted,
#' whether the "reference database files" copy should also be renamed to
#' match) - until then, this function does not silently prefer one name
#' over another beyond "first match wins" (see \strong{Reference file
#' structure} below for the multiple-match notice).
#'
#' \strong{dir.sub default changed 2026-09-14, per Josh:} \code{dir.sub} now
#' defaults to \code{TRUE} (previously \code{FALSE}) - subdirectories of
#' \code{dir.load} are searched by default, for both the base reference
#' file and any supplemental files. Pass \code{dir.sub = FALSE} to restore
#' the old top-level-only search.
#'
#' \strong{Reference file structure} (verified against the real data, 121
#' species/shrub rows x 11 columns): \code{$scientific_name}, \code{$common.name}
#' (the raw file's own header text is still literally \code{common_one} -
#' see \strong{"Column identifiers renamed, 2026-09-29"} below for how this
#' is bridged), \code{$common.name2} (raw file header \code{common_two}),
#' \code{$genus}, \code{$family}, \code{$native.status},
#' \code{$growth.habit}, \code{$wood.type}, \code{$grouping.one},
#' \code{$grouping.two}, \code{$grouping.three}. \code{$common.name2} is
#' blank for many rows (not every species has a second common name); a
#' blank never matches anything, and if \code{$common.name2} itself is
#' requested via \code{head.out} for a row with no second name, that
#' header just comes back \code{""} for that row. \code{$wood.type} is not
#' one of the 10 columns \code{reference.data = "append"/"overwrite"}
#' reconciles supplemental files against (see below) - it is carried
#' through from the base table untouched, and comes back \code{NA} for any
#' row contributed by a supplemental file that didn't happen to also match
#' a \code{wood.type}-like column.
#'
#' \strong{Supplemental reference data (\code{reference.data}/\code{pattern}),
#' added 2026-09-14 per Josh.} When \code{reference.data = "append"} or
#' \code{"overwrite"}, after \code{reference.plants} is loaded as above,
#' \code{dir.load} (and its subdirectories, if \code{dir.sub = TRUE}) is
#' searched for every file matching \code{pattern} (one or more file
#' names/glob patterns; the default \code{"plant.names.csv"} is a literal
#' name, but a wildcard like \code{"*.csv"} works the same way as
#' \code{load.pattern}). Each matched file is processed independently, in
#' the order \code{list.files()} returns, and folded into a single
#' \code{reference.plants.temp} data frame:
#' \enumerate{
#'   \item \strong{Column matching (name AND content).} The file's own
#'     headers are standardized (see above), then matched to
#'     \code{reference.plants}'s 10 non-\code{wood.type} columns
#'     (\code{scientific_name}, \code{common.name}, \code{common.name2}, \code{genus},
#'     \code{family}, \code{native.status}, \code{growth.habit},
#'     \code{grouping.one}, \code{grouping.two}, \code{grouping.three}) in
#'     three passes: (1) an exact match on the standardized header text;
#'     (2) for anything left over, a keyword match on the standardized
#'     header (e.g. a header containing \code{"latin"}/\code{"scientific"}
#'     maps to \code{scientific_name}; \code{"common"} to \code{common.name} then
#'     \code{common.name2}, in file column order; \code{"group"}/
#'     \code{"grouping"} to \code{grouping.one}/\code{two}/\code{three}, in
#'     file column order; \code{"genus"}, \code{"family"},
#'     \code{"native"}/\code{"status"}, \code{"growth"}/\code{"habit"} to
#'     their like-named column); (3) for anything STILL unmapped, a
#'     content signature check against whichever of \code{scientific_name}/
#'     \code{native.status}/\code{growth.habit} remain unclaimed (a column
#'     that's mostly two-word capitalized text - e.g. \code{"Acer negundo"}
#'     - is treated as \code{scientific_name}; a column whose values mostly fall in
#'     a small native/introduced/invasive vocabulary is treated as
#'     \code{native.status}; a column whose values mostly fall in a small
#'     tree/shrub/vine vocabulary is treated as \code{growth.habit}). This
#'     is a best-effort heuristic, not a guarantee - \strong{flagged, not
#'     silently perfect:} an odd or ambiguous input file may map incorrectly
#'     or not at all; any input column that never gets claimed by a
#'     canonical name is simply dropped (not merged as an extra column).
#'     Note that a real supplemental file's own \code{common_one}/
#'     \code{common_two} header text still standardizes to those same
#'     snake_case strings (never to \code{common.name}/\code{common.name2}
#'     - see \strong{"Column identifiers renamed, 2026-09-29"} below), so
#'     this case is expected to fall through to the pass-2 keyword match
#'     (both headers contain \code{"common"}) rather than the pass-1 exact
#'     match - the same first-column-wins/second-column-wins assignment
#'     order is preserved either way.
#'   \item \strong{Required headers.} A file must end up with (after step
#'     1) at least \code{scientific_name}, at least one of
#'     \code{common.name}/\code{common.name2}, and at least one of
#'     \code{grouping.one}/\code{grouping.two}/\code{grouping.three} - if
#'     any of those three categories is entirely missing, the file is
#'     skipped (not added to \code{reference.plants.temp}) and
#'     \code{"<basename> <full path> skipped as missing required headers:
#'     <list>"} is printed via \code{cat()}.
#'   \item \strong{Recycling/derivation/padding.} For a file that passes
#'     step 2: if only one of \code{common.name}/\code{common.name2} is
#'     present, the other is set equal to it (recycled, not left blank);
#'     if only one or two of \code{grouping.one}/\code{two}/\code{three}
#'     are present, the missing slot(s) are filled by recycling the
#'     FIRST present grouping column's value (an arbitrary but consistent
#'     choice - \strong{flagged}: there's no principled way to pick which
#'     grouping level to recycle from without more information from Josh);
#'     if \code{genus} is missing, it's derived as the first
#'     whitespace-separated word of \code{scientific_name}; if \code{family},
#'     \code{native.status}, or \code{growth.habit} is missing, it's added
#'     as \code{NA} for every row. Every column that had to be
#'     recycled/derived/padded this way is collected into one list.
#'   \item \strong{Notice.} If that list is empty (the file's own headers,
#'     once matched in step 1, already covered all 10 columns natively -
#'     nothing needed recycling, deriving, or padding),
#'     \code{"<basename> <full path> complete success very nice!"} is
#'     printed. Otherwise, \code{"<basename> <full path> loaded but padded
#'     as missing these headers: <list>"} is printed, naming every
#'     recycled/derived/padded column.
#'   \item \strong{Merge.} The file's reconciled 10-column data frame is
#'     added onto \code{reference.plants.temp} (row-bound; a file's
#'     \code{wood.type} is not part of this reconciliation and is simply
#'     absent - i.e. \code{NA} - for these rows).
#' }
#' Once every matched file has been processed: \code{reference.plants.all}
#' starts as a copy of \code{reference.plants}. If \code{reference.data =
#' "append"}, \code{reference.plants.temp}'s rows are added onto
#' \code{reference.plants.all} (column union with the base table, so
#' \code{wood.type} is simply \code{NA} for the new rows). If
#' \code{reference.data = "overwrite"}, \code{reference.plants.all} is
#' REPLACED entirely by \code{reference.plants.temp} (the base
#' \code{reference.plants} table is discarded) - \strong{flagged:} if
#' \code{pattern} matches nothing usable (every candidate file is skipped,
#' or none matches at all), there is nothing to overwrite with, so
#' \code{reference.plants.all} falls back to the unmodified
#' \code{reference.plants} rather than becoming empty. \code{reference.plants.all}
#' (not \code{reference.plants}) is what the rest of this function - matching,
#' \code{head.out} validation, everything below this point - actually uses.
#'
#' \strong{Matching normalization} is stronger than
#' \code{batz.batusa_recode.names}': case is folded AND every space/
#' punctuation character is stripped out entirely (not just collapsed to a
#' single space), per the spec's "ignore missing spaces and special
#' characters" - so e.g. \code{"Ash-leaved Maple"}, \code{"ash leaved
#' maple"}, \code{"Ashleaved_Maple"}, and \code{"ASHLEAVEDMAPLE"} all match
#' the same reference row. This normalization is matching-only; the value
#' returned always comes from the reference table's original (whitespace-
#' trimmed, not de-punctuated) text.
#'
#' \strong{Duplicate reference keys (real data):} three genuine collisions
#' exist in the real reference file - \code{"Juneberry"} (\code{$common.name2})
#' is shared by three different \emph{Amelanchier} species, and
#' \code{"Filbert"} (\code{$common.name2}) by two \emph{Corylus} species. No
#' tie-break parameter was requested for this function, so the FIRST match
#' wins (checked in \code{scientific_name}/\code{common.name}/\code{common.name2}
#' column order, then row order in \code{reference.plants.all}) - an input
#' of \code{"Juneberry"} alone is genuinely ambiguous in the source data
#' and always resolves to the first of the three. Loading supplemental
#' data via \code{reference.data = "append"} can introduce further
#' collisions (including a supplemental row colliding with a base row);
#' the same first-match rule applies.
#'
#' \strong{Unmatched inputs:} passed through unchanged in every requested
#' \code{head.out} column. Whenever at least one input doesn't match,
#' \code{"These inputs were missing:"} is printed followed by the vector of
#' unique unmatched values, and a data frame called \code{treesmismatch.log}
#' is auto-assigned into the calling environment (same bare-call-populates-
#' workspace convention used by \code{\link{batz.generate_arumeta.eventlog}}
#' /\code{\link{batz.merge_aru.meta}}), with columns:
#' \code{$input} (each unique unmatched value), \code{$missmatch.count}
#' (how many times that exact value occurs in \emph{this call's}
#' \code{data} - an instance count, not a distinct-value count), and
#' \code{$closest.match} (the single nearest reference entry across
#' \code{scientific_name}/\code{common.name}/\code{common.name2}, by Levenshtein edit
#' distance via \code{utils::adist()} on the normalized strings). When
#' every input matches, \code{treesmismatch.log} is not created/updated.
#'
#' \strong{Column identifiers renamed, 2026-09-27, per Josh's reference-workbook "Change.to" column.} The following column identifiers were renamed throughout this function's inputs/outputs and this reference table's own columns: \code{grouping_one} -> \code{grouping.one}, \code{grouping_two} -> \code{grouping.two}, \code{grouping_three} -> \code{grouping.three}, \code{growth_habit} -> \code{growth.habit}, \code{native_status} -> \code{native.status}, \code{wood_type} -> \code{wood.type}, \code{missmatch_count} -> \code{missmatch.count} (an output-only column of \code{treesmismatch.log}), and the reference table's own species-identifier column \code{species} -> \code{scientific_name} (this function loads its reference table from disk rather than embedding one, so there is no embedded lookup table to update here - only \code{match.cols}/\code{plant.schema.cols} and every \code{$}/\code{[["..."]]}/string-literal reference to these column names). General prose uses of the word "species" (e.g. describing tree species, species identifiers) were left unchanged, as were unrelated identifiers such as \code{batz.batusa_list.species}.
#'
#' \strong{Column identifiers renamed, 2026-09-29, per Josh's reference-workbook
#' review and his follow-up "These are the same things" / "Make changes":
#' \code{common_one} -> \code{common.name}, \code{common_two} ->
#' \code{common.name2}.} \code{common_one}/\code{"Bat Species"} had been
#' flagged as colliding with \code{\link{batz.batusa_list.species}}'s own
#' \code{"Bat Species"} -> \code{common.name} pending rename; Josh confirmed
#' these are a single shared, reused identifier (the same pattern already
#' used for \code{date.start}/\code{date.end} across sibling functions in
#' this catalog), not a real collision - the reference workbook's two
#' separate pending rows are merged into one \code{Header.names} row,
#' \code{common.name}, listing both functions; \code{common_two}, held
#' pending on that same resolution, is applied alongside it as
#' \code{common.name2}. \strong{Unlike \code{\link{batz.batusa_list.species}},
#' this function's reference table is loaded fresh from disk on every call
#' (not embedded), and its real file's own header text is still literally
#' \code{common_one}/\code{common_two} - \code{standardize.headers()} can
#' only ever produce snake_case (lowercase, non-alphanumeric runs collapsed
#' to a single underscore), so it can never itself turn either raw header
#' into a dot-spelled identifier.} To bridge this, \code{load.tree.reference()}
#' now renames \code{common_one} -> \code{common.name} and \code{common_two}
#' -> \code{common.name2} immediately after \code{standardize.headers()} (a
#' legacy-input-alias step, mirroring the same pattern already used
#' elsewhere in this package - e.g. \code{\link{batz.merge_vetted.acoustics}}'s
#' \code{monitoringnight}/\code{serial} aliases - for exactly this "a plain
#' \code{setdiff()}/name lookup against a dot-spelled identifier can never
#' be satisfied by any real raw file" bug class). \code{match.cols},
#' \code{plant.schema.cols}, \code{keyword.map}'s keys, and
#' \code{build.plant.row.set()}'s field checks are all updated to the new
#' dot-spelled names to match. \strong{Verified:} re-ran this function
#' end to end against the real reference file with no error, confirmed
#' \code{head.out = "common.name"} (the new default) and
#' \code{head.out = "common.name2"} both resolve correctly, and confirmed
#' \code{reference.data = "append"}/\code{"overwrite"} still correctly
#' identify a supplemental file's own (still snake_case) \code{common_one}/
#' \code{common_two} headers via the pass-2 keyword match (see the
#' "Column matching" note above) - no behavior change there beyond which
#' pass does the matching.
#'
#' \strong{BUGFIX, 2026-09-29, found while verifying the rename above end to
#' end against the real reference file: the 2026-09-27 rename round (see
#' that entry above) renamed \code{grouping_one}/\code{grouping_two}/
#' \code{grouping_three}/\code{growth_habit}/\code{native_status}/
#' \code{wood_type} to dot-spelled identifiers in \code{plant.schema.cols}/
#' \code{keyword.map}/this documentation, but never added the
#' legacy-input-alias bridge these six identifiers need to actually reach
#' the base reference table - the same bridge \code{common_one}/
#' \code{common_two} needed above, and for the identical reason:
#' \code{standardize.headers()} can only ever produce a snake_case
#' identifier, so a plain lookup for a dot-spelled column against
#' \code{reference.plants} (loaded fresh from disk, never embedded) could
#' never succeed. This meant every one of these six identifiers had been
#' completely unreachable via \code{head.out} - including this function's
#' own documented \code{@examples} usage,
#' \code{head.out = c("scientific_name", "family", "growth.habit")} -
#' since the 2026-09-27 round shipped.} \strong{Fixed} by adding the same
#' legacy-input-alias step to \code{load.tree.reference()} for all six
#' identifiers. \strong{Verified}: re-ran
#' \code{batz.treeusa_recode.names(data, head.out = c("scientific_name",
#' "family", "growth.habit"))} (this function's own documented example)
#' against the real reference file - previously an immediate error
#' (\code{head.out} not found in \code{names(reference)}), now resolves
#' correctly - and confirmed \code{grouping.one}/\code{grouping.two}/
#' \code{grouping.three}/\code{native.status}/\code{wood.type} all resolve
#' the same way.
#'
#' @examples
#' \dontrun{
#' batz.treeusa_recode.names(c("sugar maple", "RED OAK", "not.a.real.tree"))
#' # -> "Sugar Maple"   "Northern Red Oak"   "not.a.real.tree"
#' # treesmismatch.log now in your workspace (1 row: "not.a.real.tree")
#'
#' batz.treeusa_recode.names("Ash-leaved Maple", head.out = "scientific_name")
#' # -> "Acer negundo"
#'
#' batz.treeusa_recode.names(c("Sugar Maple", "Red Oak"),
#'                            head.out = c("scientific_name", "family", "growth.habit"))
#' # -> data frame with $scientific_name, $family, $growth.habit columns
#'
#' # pull in every "plant.names.csv" found under dir.load (recursively, since
#' # dir.sub defaults to TRUE) and add their rows onto the base reference table
#' batz.treeusa_recode.names("sugar maple", reference.data = "append")
#'
#' # same, but searching for several possible supplemental file names, and
#' # using ONLY their rows (base reference.plants table discarded)
#' batz.treeusa_recode.names("sugar maple",
#'                            pattern = c("plant.names.csv", "shrub_extra.csv"),
#'                            reference.data = "overwrite")
#' }
#'
#' @export
batz.treeusa_recode.names <- function(data,
                                       head.out       = "common.name",
                                       dir.load       = getwd(),
                                       load.pattern   = c("*USA.treeshrub_recode.names*",
                                                           "*tree_species_and_shrubs*"),
                                       dir.sub        = TRUE,
                                       pattern        = "plant.names.csv",
                                       reference.data = "default") {

  match.cols <- c("scientific_name", "common.name", "common.name2")
  plant.schema.cols <- c("scientific_name", "common.name", "common.name2", "genus", "family",
                          "native.status", "growth.habit",
                          "grouping.one", "grouping.two", "grouping.three")

  if (!(is.character(reference.data) && length(reference.data) == 1 &&
        reference.data %in% c("default", "append", "overwrite"))) {
    stop("`reference.data` must be one of \"default\", \"append\", or \"overwrite\" (got: \"",
         paste(reference.data, collapse = ", "), "\").")
  }

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  normalize.tree <- function(x) {
    x <- as.character(x)
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "", x)
    x
  }

  read.ref.file <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "csv") {
      df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reference file '", basename(f), "' is an Excel file, but the 'readxl' package is not installed.")
      }
      df <- as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
    } else {
      stop("Reference file '", basename(f), "' has an unsupported extension (expected .csv/.xlsx/.xls).")
    }
    df[] <- lapply(df, function(col) trimws(as.character(col)))
    df
  }

  load.tree.reference <- function(dir.load, load.pattern, dir.sub) {
    matches <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                           recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)
    if (length(matches) == 0) {
      stop(sprintf("No reference database file found in '%s' matching load.pattern '%s'.",
                    dir.load, paste(load.pattern, collapse = "', '")))
    }
    if (length(matches) > 1) {
      cat("NOTE: more than one file matched load.pattern - using the first: ",
          basename(matches[1]), "\n", sep = "")
    }
    ref <- read.ref.file(matches[1])

    ## header standardization (per Josh, 2026-09-14 project preference):
    ## the reference database's own column names are a literal, uninvented
    ## copy of that file's real header text, so they're run through the
    ## shared package helper the same as any other loaded file's headers -
    ## see @details "Header standardization" above.
    names(ref) <- standardize.headers(names(ref))

    ## legacy-input-alias rename, added 2026-09-29 (see @details "Column
    ## identifiers renamed, 2026-09-29" above): standardize.headers() can
    ## never itself produce a dot, so the real file's own common_one/
    ## common_two headers are bridged to this function's now-dot-spelled
    ## match.cols/plant.schema.cols entries here, immediately after
    ## standardization and before anything else in this function looks at
    ## `ref`'s column names.
    if ("common_one" %in% names(ref) && !("common.name" %in% names(ref))) {
      names(ref)[names(ref) == "common_one"] <- "common.name"
    }
    if ("common_two" %in% names(ref) && !("common.name2" %in% names(ref))) {
      names(ref)[names(ref) == "common_two"] <- "common.name2"
    }

    ## BUGFIX, 2026-09-29 (see @details "BUGFIX, 2026-09-29" above): the
    ## 2026-09-27 rename round renamed grouping_one/grouping_two/
    ## grouping_three/growth_habit/native_status/wood_type to dot-spelled
    ## identifiers in plant.schema.cols/keyword.map/prose, but never added
    ## this same legacy-input-alias bridge for them here - so every one of
    ## those six identifiers has been unreachable from the real reference
    ## file (which, after standardize.headers(), can only ever have the
    ## snake_case spelling) since that round shipped. Fixed the same way as
    ## common_one/common_two above.
    legacy.aliases <- c(
      grouping_one   = "grouping.one",
      grouping_two   = "grouping.two",
      grouping_three = "grouping.three",
      growth_habit   = "growth.habit",
      native_status  = "native.status",
      wood_type      = "wood.type"
    )
    for (old.name in names(legacy.aliases)) {
      new.name <- legacy.aliases[[old.name]]
      if (old.name %in% names(ref) && !(new.name %in% names(ref))) {
        names(ref)[names(ref) == old.name] <- new.name
      }
    }

    ref
  }

  ## ---- content-signature helpers, used only to disambiguate a
  ## supplemental file's column when its (standardized) header name alone
  ## doesn't identify it - see @details "Supplemental reference data" ----
  looks.like.binomial <- function(x) {
    x <- x[nzchar(x)]
    if (length(x) == 0) return(FALSE)
    mean(grepl("^[A-Z][a-z]+[ _][a-z]+", x)) > 0.7
  }
  native.status.vocab <- c("native", "introduced", "nonnative", "non native",
                            "invasive", "naturalized", "exotic", "adventive")
  growth.habit.vocab   <- c("tree", "shrub", "vine", "tree shrub", "treeshrub",
                             "groundcover", "herb", "graminoid")
  looks.like.vocab <- function(x, vocab) {
    x <- normalize.tree(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) return(FALSE)
    mean(x %in% normalize.tree(vocab)) > 0.6
  }

  match.file.headers <- function(df, canonical.cols) {
    std <- standardize.headers(names(df))
    names(df) <- std

    mapped <- rep(NA_character_, length(std))

    ## pass 1: exact standardized-name match to a canonical column. A real
    ## supplemental file's own common_one/common_two headers still
    ## standardize to those same snake_case strings (never to
    ## common.name/common.name2 - standardize.headers() can't produce a
    ## dot), so this pass no longer catches them directly - see @details
    ## "Column identifiers renamed, 2026-09-29" above. They're still
    ## caught below, by the pass-2 keyword match on "common".
    exact <- std %in% canonical.cols
    mapped[exact] <- std[exact]
    already.used <- unique(mapped[exact])

    ## pass 2: keyword match on the standardized header for anything left over
    keyword.map <- list(
      scientific_name = c("species", "latin", "scientific", "sciname"),
      genus          = c("genus"),
      family         = c("family"),
      native.status  = c("native", "status"),
      growth.habit   = c("growth", "habit"),
      common.name    = c("common"),
      common.name2   = c("common"),
      grouping.one   = c("group", "grouping"),
      grouping.two   = c("group", "grouping"),
      grouping.three = c("group", "grouping")
    )
    for (i in which(!exact)) {
      h <- std[i]
      hit <- NA_character_
      for (cc in canonical.cols) {
        if (cc %in% already.used) next
        kws <- keyword.map[[cc]]
        if (!is.null(kws) && any(vapply(kws, function(k) grepl(k, h, fixed = TRUE), logical(1)))) {
          hit <- cc
          break
        }
      }
      if (!is.na(hit)) {
        mapped[i] <- hit
        already.used <- c(already.used, hit)
      }
    }

    ## pass 3: content-signature fallback for anything still unmapped
    for (i in which(is.na(mapped))) {
      col.vals <- df[[i]]
      if (!("scientific_name" %in% already.used) && looks.like.binomial(col.vals)) {
        mapped[i] <- "scientific_name"; already.used <- c(already.used, "scientific_name")
      } else if (!("native.status" %in% already.used) && looks.like.vocab(col.vals, native.status.vocab)) {
        mapped[i] <- "native.status"; already.used <- c(already.used, "native.status")
      } else if (!("growth.habit" %in% already.used) && looks.like.vocab(col.vals, growth.habit.vocab)) {
        mapped[i] <- "growth.habit"; already.used <- c(already.used, "growth.habit")
      }
    }

    names(df) <- ifelse(is.na(mapped), std, mapped)
    ## a header that never got claimed by a canonical name is dropped, not
    ## merged in as an extra column (out of scope for this reconciliation)
    df <- df[names(df) %in% canonical.cols]
    ## if two headers somehow mapped to the same canonical name, keep only
    ## the first occurrence
    df[!duplicated(names(df))]
  }

  build.plant.row.set <- function(df, canonical.cols) {
    padded <- character(0)

    has.c1 <- "common.name" %in% names(df)
    has.c2 <- "common.name2" %in% names(df)
    if (has.c1 && !has.c2) { df$common.name2 <- df$common.name; padded <- c(padded, "common.name2") }
    if (has.c2 && !has.c1) { df$common.name <- df$common.name2; padded <- c(padded, "common.name") }

    grp.cols     <- c("grouping.one", "grouping.two", "grouping.three")
    present.grp  <- grp.cols[grp.cols %in% names(df)]
    if (length(present.grp) > 0 && length(present.grp) < 3) {
      source.col  <- present.grp[1]
      missing.grp <- setdiff(grp.cols, present.grp)
      for (gc in missing.grp) df[[gc]] <- df[[source.col]]
      padded <- c(padded, missing.grp)
    }

    if (!"genus" %in% names(df)) {
      df$genus <- vapply(strsplit(df$scientific_name, "\\s+"),
                          function(w) if (length(w) >= 1) w[1] else NA_character_,
                          character(1))
      padded <- c(padded, "genus")
    }

    for (cc in c("family", "native.status", "growth.habit")) {
      if (!cc %in% names(df)) {
        df[[cc]] <- NA_character_
        padded <- c(padded, cc)
      }
    }

    df <- df[canonical.cols]
    list(data = df, padded = padded)
  }

  rbind.fill <- function(a, b) {
    all.cols <- union(names(a), names(b))
    for (cc in setdiff(all.cols, names(a))) a[[cc]] <- NA
    for (cc in setdiff(all.cols, names(b))) b[[cc]] <- NA
    rbind(a[all.cols], b[all.cols])
  }

  recode.one <- function(x, reference, head.col, lookup.values, lookup.rowidx) {
    x.chr  <- as.character(x)
    x.norm <- normalize.tree(x.chr)

    match.idx <- match(x.norm, lookup.values)
    row.idx   <- lookup.rowidx[match.idx]
    found     <- !is.na(row.idx)

    out <- x.chr
    out[found] <- as.character(reference[[head.col]][row.idx[found]])

    list(values = out, found = found)
  }

  closest.match.for <- function(x.norm.one, ref.pool.norm, ref.pool.raw) {
    d <- utils::adist(x.norm.one, ref.pool.norm)[1, ]
    ref.pool.raw[which.min(d)]
  }

  if (is.data.frame(data)) {
    stop("`data` must be a plain vector for batz.treeusa_recode.names() (not a data frame).")
  }

  reference.plants <- load.tree.reference(dir.load, load.pattern, dir.sub)

  reference.plants.all <- reference.plants

  if (reference.data %in% c("append", "overwrite")) {
    supp.matches <- list.files(dir.load, pattern = pattern.regex(pattern),
                                recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

    if (length(supp.matches) == 0) {
      cat(sprintf("NOTE: no files matching pattern '%s' were found in '%s' (dir.sub = %s) - %s using only reference.plants.\n",
                   paste(pattern, collapse = "', '"), dir.load, dir.sub,
                   if (reference.data == "overwrite") "nothing to overwrite with;" else "nothing to append;"))
    }

    reference.plants.temp <- NULL

    for (f in supp.matches) {
      df <- tryCatch(read.ref.file(f), error = function(e) {
        cat(sprintf("%s %s skipped as unreadable: %s\n", basename(f), f, conditionMessage(e)))
        NULL
      })
      if (is.null(df)) next

      df <- match.file.headers(df, plant.schema.cols)

      has.species <- "scientific_name" %in% names(df)
      has.common  <- any(c("common.name", "common.name2") %in% names(df))
      has.group   <- any(c("grouping.one", "grouping.two", "grouping.three") %in% names(df))

      if (!(has.species && has.common && has.group)) {
        missing.req <- c(
          if (!has.species) "scientific_name (latin name)",
          if (!has.common)  "a common name column",
          if (!has.group)   "a grouping column"
        )
        cat(sprintf("%s %s skipped as missing required headers: %s\n",
                    basename(f), f, paste(missing.req, collapse = ", ")))
        next
      }

      built <- build.plant.row.set(df, plant.schema.cols)

      if (length(built$padded) == 0) {
        cat(sprintf("%s %s complete success very nice!\n", basename(f), f))
      } else {
        cat(sprintf("%s %s loaded but padded as missing these headers: %s\n",
                    basename(f), f, paste(built$padded, collapse = ", ")))
      }

      reference.plants.temp <- if (is.null(reference.plants.temp)) {
        built$data
      } else {
        rbind.fill(reference.plants.temp, built$data)
      }
    }

    if (!is.null(reference.plants.temp)) {
      if (reference.data == "append") {
        reference.plants.all <- rbind.fill(reference.plants.all, reference.plants.temp)
      } else if (reference.data == "overwrite") {
        reference.plants.all <- reference.plants.temp
      }
    }
    ## if reference.plants.temp is still NULL here (every candidate file was
    ## unreadable/skipped, or nothing matched pattern at all), reference.plants.all
    ## stays exactly reference.plants - see @details "flagged" note.
  }

  reference <- reference.plants.all

  bad.head <- setdiff(head.out, names(reference))
  if (length(bad.head) > 0) {
    stop(sprintf("head.out must be (a) header(s) from the reference database's own columns: %s (got unrecognized: %s)",
                  paste(names(reference), collapse = ", "), paste(bad.head, collapse = ", ")))
  }

  lookup.values <- unlist(lapply(match.cols, function(cn) normalize.tree(reference[[cn]])),
                           use.names = FALSE)
  lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

  x.chr <- as.character(data)

  per.head <- lapply(head.out, function(hc) {
    recode.one(x.chr, reference, hc, lookup.values, lookup.rowidx)
  })
  names(per.head) <- head.out

  # "found" is identical across every head.out column (same match step) -
  # take it from the first.
  found <- per.head[[1]]$found

  if (length(head.out) == 1) {
    out <- per.head[[1]]$values
  } else {
    out <- as.data.frame(lapply(per.head, function(r) r$values), stringsAsFactors = FALSE)
    names(out) <- head.out
  }

  # ---- treesmismatch.log + console notice for unmatched inputs ----
  if (any(!found)) {
    unmatched.instances <- x.chr[!found]
    unmatched.unique    <- unique(unmatched.instances)

    ref.pool.raw  <- unlist(lapply(match.cols, function(cn) reference[[cn]]), use.names = FALSE)
    ref.pool.norm <- normalize.tree(ref.pool.raw)
    keep.pool     <- ref.pool.norm != ""   # blank $common.name2 cells contribute nothing to match
    ref.pool.raw  <- ref.pool.raw[keep.pool]
    ref.pool.norm <- ref.pool.norm[keep.pool]

    closest <- vapply(unmatched.unique, function(v) {
      v.norm <- normalize.tree(v)
      closest.match.for(v.norm, ref.pool.norm, ref.pool.raw)
    }, character(1))

    treesmismatch.log <- data.frame(
      input           = unmatched.unique,
      missmatch.count = as.integer(vapply(unmatched.unique, function(v) sum(unmatched.instances == v), integer(1))),
      closest.match   = closest,
      stringsAsFactors = FALSE
    )

    cat("These inputs were missing:\n")
    print(unmatched.unique)

    assign("treesmismatch.log", treesmismatch.log, envir = parent.frame())
  }

  out
}

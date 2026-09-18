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
#' @param head.out Character vector, default \code{"common_one"}. One or
#'   more of the reference database's own column names to return. If a
#'   single header is requested, the return value is a plain vector; if
#'   more than one, the return value is a data frame with one column per
#'   requested header, in the same row order as \code{data}. Matching an
#'   input element to a reference row always uses \code{species}/
#'   \code{common_one}/\code{common_two} only, regardless of what's
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
#' "overwrite"}. This is a no-op against the CURRENT real reference file -
#' every column listed under \strong{Reference file structure} below is
#' already in exactly this snake_case shape - but it guards against a
#' future copy of the file picking up stray whitespace, mixed case, or
#' punctuation differences. \code{match.cols} and the default \code{head.out}
#' are written as their already-standardized spellings, so no further
#' change was needed there; if the reference file's real headers ever
#' change to something standardize.headers() would alter, \code{head.out}
#' should be called with the new standardized spelling.
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
#' species/shrub rows x 11 columns): \code{$species}, \code{$common_one},
#' \code{$common_two}, \code{$genus}, \code{$family}, \code{$native_status},
#' \code{$growth_habit}, \code{$wood_type}, \code{$grouping_one},
#' \code{$grouping_two}, \code{$grouping_three}. \code{$common_two} is
#' blank for many rows (not every species has a second common name); a
#' blank never matches anything, and if \code{$common_two} itself is
#' requested via \code{head.out} for a row with no second name, that
#' header just comes back \code{""} for that row. \code{$wood_type} is not
#' one of the 10 columns \code{reference.data = "append"/"overwrite"}
#' reconciles supplemental files against (see below) - it is carried
#' through from the base table untouched, and comes back \code{NA} for any
#' row contributed by a supplemental file that didn't happen to also match
#' a \code{wood_type}-like column.
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
#'     \code{reference.plants}'s 10 non-\code{wood_type} columns
#'     (\code{species}, \code{common_one}, \code{common_two}, \code{genus},
#'     \code{family}, \code{native_status}, \code{growth_habit},
#'     \code{grouping_one}, \code{grouping_two}, \code{grouping_three}) in
#'     three passes: (1) an exact match on the standardized header text;
#'     (2) for anything left over, a keyword match on the standardized
#'     header (e.g. a header containing \code{"latin"}/\code{"scientific"}
#'     maps to \code{species}; \code{"common"} to \code{common_one} then
#'     \code{common_two}, in file column order; \code{"group"}/
#'     \code{"grouping"} to \code{grouping_one}/\code{two}/\code{three}, in
#'     file column order; \code{"genus"}, \code{"family"},
#'     \code{"native"}/\code{"status"}, \code{"growth"}/\code{"habit"} to
#'     their like-named column); (3) for anything STILL unmapped, a
#'     content signature check against whichever of \code{species}/
#'     \code{native_status}/\code{growth_habit} remain unclaimed (a column
#'     that's mostly two-word capitalized text - e.g. \code{"Acer negundo"}
#'     - is treated as \code{species}; a column whose values mostly fall in
#'     a small native/introduced/invasive vocabulary is treated as
#'     \code{native_status}; a column whose values mostly fall in a small
#'     tree/shrub/vine vocabulary is treated as \code{growth_habit}). This
#'     is a best-effort heuristic, not a guarantee - \strong{flagged, not
#'     silently perfect:} an odd or ambiguous input file may map incorrectly
#'     or not at all; any input column that never gets claimed by a
#'     canonical name is simply dropped (not merged as an extra column).
#'   \item \strong{Required headers.} A file must end up with (after step
#'     1) at least \code{species}, at least one of
#'     \code{common_one}/\code{common_two}, and at least one of
#'     \code{grouping_one}/\code{grouping_two}/\code{grouping_three} - if
#'     any of those three categories is entirely missing, the file is
#'     skipped (not added to \code{reference.plants.temp}) and
#'     \code{"<basename> <full path> skipped as missing required headers:
#'     <list>"} is printed via \code{cat()}.
#'   \item \strong{Recycling/derivation/padding.} For a file that passes
#'     step 2: if only one of \code{common_one}/\code{common_two} is
#'     present, the other is set equal to it (recycled, not left blank);
#'     if only one or two of \code{grouping_one}/\code{two}/\code{three}
#'     are present, the missing slot(s) are filled by recycling the
#'     FIRST present grouping column's value (an arbitrary but consistent
#'     choice - \strong{flagged}: there's no principled way to pick which
#'     grouping level to recycle from without more information from Josh);
#'     if \code{genus} is missing, it's derived as the first
#'     whitespace-separated word of \code{species}; if \code{family},
#'     \code{native_status}, or \code{growth_habit} is missing, it's added
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
#'     \code{wood_type} is not part of this reconciliation and is simply
#'     absent - i.e. \code{NA} - for these rows).
#' }
#' Once every matched file has been processed: \code{reference.plants.all}
#' starts as a copy of \code{reference.plants}. If \code{reference.data =
#' "append"}, \code{reference.plants.temp}'s rows are added onto
#' \code{reference.plants.all} (column union with the base table, so
#' \code{wood_type} is simply \code{NA} for the new rows). If
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
#' exist in the real reference file - \code{"Juneberry"} (\code{$common_two})
#' is shared by three different \emph{Amelanchier} species, and
#' \code{"Filbert"} (\code{$common_two}) by two \emph{Corylus} species. No
#' tie-break parameter was requested for this function, so the FIRST match
#' wins (checked in \code{species}/\code{common_one}/\code{common_two}
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
#' \code{$input} (each unique unmatched value), \code{$missmatch_count}
#' (how many times that exact value occurs in \emph{this call's}
#' \code{data} - an instance count, not a distinct-value count), and
#' \code{$closest.match} (the single nearest reference entry across
#' \code{species}/\code{common_one}/\code{common_two}, by Levenshtein edit
#' distance via \code{utils::adist()} on the normalized strings). When
#' every input matches, \code{treesmismatch.log} is not created/updated.
#'
#' @examples
#' \dontrun{
#' batz.treeusa_recode.names(c("sugar maple", "RED OAK", "not.a.real.tree"))
#' # -> "Sugar Maple"   "Northern Red Oak"   "not.a.real.tree"
#' # treesmismatch.log now in your workspace (1 row: "not.a.real.tree")
#'
#' batz.treeusa_recode.names("Ash-leaved Maple", head.out = "species")
#' # -> "Acer negundo"
#'
#' batz.treeusa_recode.names(c("Sugar Maple", "Red Oak"),
#'                            head.out = c("species", "family", "growth_habit"))
#' # -> data frame with $species, $family, $growth_habit columns
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
                                       head.out       = "common_one",
                                       dir.load       = getwd(),
                                       load.pattern   = c("*USA.treeshrub_recode.names*",
                                                           "*tree_species_and_shrubs*"),
                                       dir.sub        = TRUE,
                                       pattern        = "plant.names.csv",
                                       reference.data = "default") {

  match.cols <- c("species", "common_one", "common_two")
  plant.schema.cols <- c("species", "common_one", "common_two", "genus", "family",
                          "native_status", "growth_habit",
                          "grouping_one", "grouping_two", "grouping_three")

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

    ## pass 1: exact standardized-name match to a canonical column
    exact <- std %in% canonical.cols
    mapped[exact] <- std[exact]
    already.used <- unique(mapped[exact])

    ## pass 2: keyword match on the standardized header for anything left over
    keyword.map <- list(
      species        = c("species", "latin", "scientific", "sciname"),
      genus          = c("genus"),
      family         = c("family"),
      native_status  = c("native", "status"),
      growth_habit   = c("growth", "habit"),
      common_one     = c("common"),
      common_two     = c("common"),
      grouping_one   = c("group", "grouping"),
      grouping_two   = c("group", "grouping"),
      grouping_three = c("group", "grouping")
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
      if (!("species" %in% already.used) && looks.like.binomial(col.vals)) {
        mapped[i] <- "species"; already.used <- c(already.used, "species")
      } else if (!("native_status" %in% already.used) && looks.like.vocab(col.vals, native.status.vocab)) {
        mapped[i] <- "native_status"; already.used <- c(already.used, "native_status")
      } else if (!("growth_habit" %in% already.used) && looks.like.vocab(col.vals, growth.habit.vocab)) {
        mapped[i] <- "growth_habit"; already.used <- c(already.used, "growth_habit")
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

    has.c1 <- "common_one" %in% names(df)
    has.c2 <- "common_two" %in% names(df)
    if (has.c1 && !has.c2) { df$common_two <- df$common_one; padded <- c(padded, "common_two") }
    if (has.c2 && !has.c1) { df$common_one <- df$common_two; padded <- c(padded, "common_one") }

    grp.cols     <- c("grouping_one", "grouping_two", "grouping_three")
    present.grp  <- grp.cols[grp.cols %in% names(df)]
    if (length(present.grp) > 0 && length(present.grp) < 3) {
      source.col  <- present.grp[1]
      missing.grp <- setdiff(grp.cols, present.grp)
      for (gc in missing.grp) df[[gc]] <- df[[source.col]]
      padded <- c(padded, missing.grp)
    }

    if (!"genus" %in% names(df)) {
      df$genus <- vapply(strsplit(df$species, "\\s+"),
                          function(w) if (length(w) >= 1) w[1] else NA_character_,
                          character(1))
      padded <- c(padded, "genus")
    }

    for (cc in c("family", "native_status", "growth_habit")) {
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

      has.species <- "species" %in% names(df)
      has.common  <- any(c("common_one", "common_two") %in% names(df))
      has.group   <- any(c("grouping_one", "grouping_two", "grouping_three") %in% names(df))

      if (!(has.species && has.common && has.group)) {
        missing.req <- c(
          if (!has.species) "species (latin name)",
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
    keep.pool     <- ref.pool.norm != ""   # blank $common_two cells contribute nothing to match
    ref.pool.raw  <- ref.pool.raw[keep.pool]
    ref.pool.norm <- ref.pool.norm[keep.pool]

    closest <- vapply(unmatched.unique, function(v) {
      v.norm <- normalize.tree(v)
      closest.match.for(v.norm, ref.pool.norm, ref.pool.raw)
    }, character(1))

    treesmismatch.log <- data.frame(
      input           = unmatched.unique,
      missmatch_count = as.integer(vapply(unmatched.unique, function(v) sum(unmatched.instances == v), integer(1))),
      closest.match   = closest,
      stringsAsFactors = FALSE
    )

    cat("These inputs were missing:\n")
    print(unmatched.unique)

    assign("treesmismatch.log", treesmismatch.log, envir = parent.frame())
  }

  out
}

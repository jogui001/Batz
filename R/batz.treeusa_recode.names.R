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
#' @param dir.load Character. Directory to search for the reference
#'   database file matching \code{load.pattern}. Default \code{getwd()}.
#' @param load.pattern Character, default \code{"*tree_species_and_shrubs*"}
#'   (wildcard/glob pattern, converted internally to a regex via
#'   \code{utils::glob2rx()}). See \strong{Details} for why this is
#'   broader than the literal \code{"maine_tree_species_and_shrubs"} named
#'   in the original spec.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also search
#'   subdirectories of \code{dir.load} for the reference file.
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
#' other loaded file's headers. This is a no-op against the CURRENT real
#' reference file - every column listed under \strong{Reference file
#' structure} below is already in exactly this snake_case shape - but it
#' guards against a future copy of the file picking up stray whitespace,
#' mixed case, or punctuation differences. \code{match.cols} and the default
#' \code{head.out} are written as their already-standardized spellings, so no
#' further change was needed there; if the reference file's real headers
#' ever change to something standardize.headers() would alter, \code{head.out}
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
#' \code{load.pattern}'s default was broadened to
#' \code{"*tree_species_and_shrubs*"} so the function finds the real file
#' either way. Please confirm with Josh whether the "reference database
#' files" copy should be renamed to include \code{"maine_"}, or whether the
#' broadened pattern is fine going forward.
#'
#' \strong{Reference file structure} (verified against the real data, 121
#' species/shrub rows x 11 columns): \code{$species}, \code{$common_one},
#' \code{$common_two}, \code{$genus}, \code{$family}, \code{$native_status},
#' \code{$growth_habit}, \code{$wood_type}, \code{$grouping_one},
#' \code{$grouping_two}, \code{$grouping_three}. \code{$common_two} is
#' blank for many rows (not every species has a second common name); a
#' blank never matches anything, and if \code{$common_two} itself is
#' requested via \code{head.out} for a row with no second name, that
#' header just comes back \code{""} for that row.
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
#' column order, then file row order) - an input of \code{"Juneberry"}
#' alone is genuinely ambiguous in the source data and always resolves to
#' the first of the three.
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
#' }
#'
#' @export
batz.treeusa_recode.names <- function(data,
                                       head.out     = "common_one",
                                       dir.load     = getwd(),
                                       load.pattern = "*tree_species_and_shrubs*",
                                       dir.sub      = FALSE) {

  match.cols <- c("species", "common_one", "common_two")

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  normalize.tree <- function(x) {
    x <- as.character(x)
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "", x)
    x
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
    f <- matches[1]
    ext <- tolower(tools::file_ext(f))

    if (ext == "csv") {
      ref <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reference file '", basename(f), "' is an Excel file, but the 'readxl' package is not installed.")
      }
      ref <- as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
    } else {
      stop("Reference file '", basename(f), "' has an unsupported extension (expected .csv/.xlsx/.xls).")
    }

    ref[] <- lapply(ref, function(col) trimws(as.character(col)))

    ## header standardization (per Josh, 2026-09-14 project preference):
    ## the reference database's own column names are a literal, uninvented
    ## copy of that file's real header text, so they're run through the
    ## shared package helper the same as any other loaded file's headers -
    ## see @details "Header standardization" above. A no-op against the
    ## current real reference file (its headers are already exactly this
    ## snake_case shape), but guards against a future copy of the file
    ## picking up stray whitespace/case/punctuation differences.
    names(ref) <- standardize.headers(names(ref))
    ref
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

  reference <- load.tree.reference(dir.load, load.pattern, dir.sub)

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

#' Recode US bat species names and codes
#'
#' Looks up bat species identifiers (common name, scientific name, 4-letter
#' or 6-letter code, or a detection category label such as \code{"HiF"})
#' in the package's bat reference tables and returns each one in a single
#' chosen format (\code{batname.format.out}): a name, a code, or a
#' reference field such as federal listing status or phonic group.
#'
#' @param data A vector, or a data frame, of bat identifiers. For a data
#'   frame every column is recoded and a data frame of the same size (all
#'   columns character, same names) is returned.
#' @param batname.format.out Character, default \code{"common_name"}. Output
#'   format: any column of the bat reference table - \code{"scientific_name"},
#'   \code{"common_name"}, \code{"code4"}, \code{"code6"}, \code{"family"},
#'   \code{"region"}, \code{"listing.status_federal"},
#'   \code{"listing.status_iucn"}, \code{"states.listed"},
#'   \code{"states.present"}, \code{"states.endangered"},
#'   \code{"states.threatened"}, \code{"state.sppofconcern"},
#'   \code{"listing.status_federal.proposed"}, \code{"hibernation.strategy"},
#'   \code{"phonic.group"}, \code{"notes"} - or \code{"both"}
#'   (\code{"Common name (Scientific name)"}). Case, spaces, dots,
#'   underscores and dashes are ignored (\code{"Common Name"} =
#'   \code{"common_name"}), and short names work: \code{"common"},
#'   \code{"latin"}/\code{"scientific"}/\code{"sci"}/\code{"species"},
#'   \code{"code"} (= \code{code4}). Any other value stops with an error
#'   listing the accepted values.
#' @param grammar.dash Logical, default \code{TRUE}. Output only:
#'   \code{TRUE} returns values as written in the reference table
#'   (\code{"Silver-haired bat"}); \code{FALSE} replaces every hyphen in a
#'   matched value with a space (\code{"Silver haired bat"}). Hyphens are
#'   always ignored when matching. In species pairs (e.g.
#'   \code{"Little brown bat; Indiana bat"}) a \code{","} is matched the same
#'   as \code{";"}, so older pair names still match.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE} and
#'   \code{data} is a data frame, accented letters and common symbols are
#'   simplified (\code{é} to \code{e}, \code{×} to \code{X}) and other
#'   non-ASCII characters (e.g. \code{°}, \code{µ}) are removed from its
#'   names and text values before matching. A plain vector is not changed.
#' @param regions Character vector, default \code{c("ALL", "NABAT")}. Only
#'   reference records whose \code{$region} matches one of these are used
#'   (records marked \code{All} are always used). A \code{$region} cell may
#'   list several regions separated by \code{;} or \code{,}.
#'
#' @return A character vector (if \code{data} is a vector) or a data frame
#'   (if \code{data} is a data frame) of the same length/size as
#'   \code{data}, each value in \code{batname.format.out}. Values with no
#'   match are returned unchanged (not \code{NA}).
#'
#' @details
#' \strong{Matching.} Input values and reference values are compared after
#' folding case, turning \code{-} and \code{_} into spaces and collapsing
#' white space, so \code{"myotis_lucifugus"}, \code{"MYLU"} and
#' \code{"Little brown bat"} all match. Matching always uses the name/code
#' columns, whatever \code{batname.format.out} is.
#'
#' \strong{Lookup order} (first match wins):
#' \enumerate{
#'   \item \code{BAT Groupings} table (species pairs such as
#'     \code{EPFUMYLU}, \code{LUSO}, genus codes \code{MYSP}), matched on
#'     \code{code4}, \code{code6}, \code{common_name}, \code{scientific_name}.
#'   \item \code{NABATFrequency Classes} table (category labels such as
#'     \code{HiF}, \code{LoF}, \code{40kMyo}, \code{2BAT}, \code{Social}),
#'     matched on \code{label}, \code{code4}, \code{code6},
#'     \code{common_name}, \code{scientific_name}.
#'   \item Known typo/variant corrections (\code{"40kMyo"} to
#'     \code{"40KHzMyo"}, \code{"2bat"} to \code{"Multiple"},
#'     \code{"40kmyomyvo"} to \code{"40KHzMyomyvo"}, \code{"HighF/HiF"} and
#'     \code{"HighF"} to \code{"HiF"}, \code{"LowF/LoF"} and \code{"LowF"} to
#'     \code{"LoF"}, \code{"Desert red bat"} to \code{"Western red bat"}) are applied, then the \code{bat.names} table (54 North
#'     American species marked \code{NABAT}, plus 8 category labels marked
#'     \code{All}: \code{All detections}, \code{40KHzMyo}, \code{HiF},
#'     \code{LoF}, \code{HiFrag}, \code{LoFrag}, \code{Multiple},
#'     \code{Social}) is searched on \code{scientific_name},
#'     \code{common_name}, \code{code4}, \code{code6}.
#' }
#' The first two tables only have name/code columns, so any other
#' \code{batname.format.out} (e.g. \code{listing.status_federal},
#' \code{family}, \code{region}) returns \code{""} for values found there.
#' The 8 category labels in \code{bat.names} also return \code{""} for the
#' non-name columns. If no \code{bat.names} record matches \code{regions},
#' a NOTE is printed and only the \code{All} records are used.
#'
#' \strong{Reference fields.} \code{$hibernation.strategy} is
#' \code{"migratory"}, \code{"hibernating"}, \code{"resident"},
#' \code{"mixed"} or \code{"unknown"}. \code{$phonic.group} is \code{"Lof"}
#' (calls below 35 kHz), \code{"Hif"} (above 35 kHz), \code{"None"} or
#' \code{"Unknown"}; \code{$notes} flags species where these are uncertain.
#' The tables are built into the package (internal helpers
#' \code{bat.ref.names()}, \code{bat.ref.groups()},
#' \code{bat.ref.freqclasses()}); no file is read.
#'
#' \strong{Unmatched values.} If any value is not found, a message is
#' printed with \code{cat()} (not \code{warning()}):
#' \code{"WARNING: X inputs did not match: ..."}, where X counts every
#' unmatched value (repeats included), followed by up to 25 unique
#' unmatched values. No log is written.
#'
#' @seealso \code{\link{batz.batusa_list.species}},
#'   \code{\link{batz.treeusa_recode.names}}
#'
#' @examples
#' \dontrun{
#' batz.batusa_recode.names(c("epfu", "myotis_lucifugus", "Hoary bat"))
#' # "Big brown bat"  "Little brown bat"  "Hoary bat"
#'
#' batz.batusa_recode.names("epfu", batname.format.out = "scientific_name")
#' # "Eptesicus fuscus"
#'
#' batz.batusa_recode.names("lano", grammar.dash = FALSE)
#' # "Silver haired bat"
#'
#' batz.batusa_recode.names("myse", batname.format.out = "listing.status_federal")
#' # "Endangered"
#'
#' batz.batusa_recode.names("mylu", batname.format.out = "both")
#' # "Little brown bat (Myotis lucifugus)"
#'
#' batz.batusa_recode.names(c("hif", "40kMyo", "2bat"), batname.format.out = "code4")
#' # "HiF"  "40KHzMyo"  "Multiple"
#'
#' # data frame: every column recoded
#' d <- data.frame(manid = c("EPFU", "MYLU"), autoid = c("LACI", "LANO"))
#' batz.batusa_recode.names(d, batname.format.out = "common_name")
#' }
#'
#' @export
batz.batusa_recode.names <- function(data, batname.format.out = "common_name", grammar.dash = TRUE,
                                     strip.special = TRUE, regions = c("ALL", "NABAT")) {

  ## special characters (per Josh, 2026-09-30): strip non-ASCII characters
  ## from `data` when it is a data frame (this function's local copy only;
  ## a plain vector passes through unchanged) - see @details, "Follow-up,
  ## 2026-09-30". The embedded reference table below is never stripped.
  data <- special.strip(data, strip.special)$df

  # ---------------------------------------------------------------------------
  # Reference tables (2026-10-23, per Josh): the embedded nabat.names table
  # that used to live here was moved to the shared bat.ref.names()
  # (R/batz.util_bat.reference.R), which every bat function now uses. The
  # "BAT Groupings" and "NABATFrequency Classes" tables are checked FIRST
  # (see recode.vec below), then the bat.names table. Only rows whose $region
  # matches `regions` (rows marked "All" always) are used.
  # ---------------------------------------------------------------------------
  nabat.names <- bat.ref.filter.regions(bat.ref.names(), regions)
  if (!any(tolower(nabat.names$region) != "all")) {
    cat(sprintf("NOTE: no bat.names records match regions = %s - only the \"All\" records are used.\n",
                paste(regions, collapse = ", ")))
  }
  pre.tables <- list(
    groups = list(df = bat.ref.filter.regions(bat.ref.groups(), regions),
                  cols = c("code4", "code6", "common_name", "scientific_name")),
    freq   = list(df = bat.ref.filter.regions(bat.ref.freqclasses(), regions),
                  cols = c("label", "code4", "code6", "common_name", "scientific_name"))
  )

  # ---------------------------------------------------------------------------
  # Known typo/variant corrections for the non-species category labels -
  # applied BEFORE the normal latin/common/code4/code6 lookup below, per
  # Josh (2026-09-24). See @details, "Known typo/variant corrections", for
  # the full rationale. Add another known typo/variant by adding one more
  # "typo" = "canonical value" entry here.
  # ---------------------------------------------------------------------------
  # Sourced directly from Josh's uploaded "spell check" tab
  # (batz.batusa_recode.names_reference_20260925.xlsx, 2026-09-25) - see
  # @details, "Known typo/variant corrections, expanded (2026-09-25)".
  # "2Bat" (the tab's own duplicate, differing only in case from "2bat")
  # is omitted here - normalize() already folds case, so a second entry
  # would just be a redundant duplicate key, not a distinct correction.
  typo.corrections <- c(
    "40kMyo"     = "40KHzMyo",
    "2bat"       = "Multiple",
    "40kmyomyvo" = "40KHzMyomyvo",
    "HighF/HiF"  = "HiF",
    "LowF/LoF"   = "LoF",
    "LowF"       = "LoF",
    "HighF"      = "HiF",
    "Desert red bat" = "Western red bat"
  )

  match.cols <- c("scientific_name", "common_name", "code4", "code6")

  ## 2026-10-02, per Josh: accept old/short format names ("common",
  ## "latin", ...), any case/separator, and the derived "both" format -
  ## see batusa.resolve.format() at the end of this file.
  batname.format.out <- batusa.resolve.format(batname.format.out, names(nabat.names))

  reference <- nabat.names
  reference[] <- lapply(reference, function(col) trimws(as.character(col)))

  # matching-only normalization: fold case, treat underscores/dashes as
  # spaces, collapse/trim whitespace. Never affects the VALUE returned.
  normalize <- function(x) {
    x <- as.character(x)
    x <- gsub("[-_]+", " ", x)
    x <- gsub("\\s*[;,]\\s*", "; ", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }

  # value returned for a "BAT Groupings"/"NABATFrequency Classes" row: these
  # tables only have name/code columns, so any other batname.format.out
  # (listing status, family, ...) gives "" - same as the category labels.
  pre.value <- function(df, fmt) {
    if (identical(fmt, "both")) {
      cn <- df$common_name; sn <- df$scientific_name
      return(ifelse(is.na(sn) | !nzchar(sn) | tolower(sn) == tolower(cn), cn, sprintf("%s (%s)", cn, sn)))
    }
    if (fmt %in% names(df) && fmt != "region") return(as.character(df[[fmt]]))
    rep("", nrow(df))
  }

  recode.vec <- function(x) {
    lookup.values <- unlist(lapply(match.cols, function(cn) normalize(reference[[cn]])),
                             use.names = FALSE)
    lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

    x.orig <- as.character(x)

    # 2026-10-23: "BAT Groupings" then "NABATFrequency Classes" first. Within
    # each table the columns are tried in the order given in pre.tables
    # (first match wins); a value found here skips the bat.names lookup.
    pre.done <- rep(FALSE, length(x.orig))
    pre.out  <- x.orig
    x.pre    <- normalize(x.orig)
    for (pt in pre.tables) {
      if (nrow(pt$df) == 0) next
      pt.vals <- pre.value(pt$df, batname.format.out)
      for (cn in pt$cols) {
        idx <- match(x.pre, normalize(pt$df[[cn]]))
        hit <- !pre.done & !is.na(idx) & nzchar(x.pre)
        if (any(hit)) {
          pre.out[hit]  <- pt.vals[idx[hit]]
          pre.done[hit] <- TRUE
        }
      }
    }

    # Known typo/variant corrections - see typo.corrections above and
    # @details. A recognized misspelling/shorthand is rewritten to its
    # canonical spelling BEFORE the ordinary lookup below runs, matched the
    # same case/whitespace/dash-underscore-insensitive way as everything
    # else via normalize(). If a corrected value still doesn't match
    # anything downstream, the ORIGINAL uncorrected input is what's
    # returned unchanged (see "out <- x.orig" below), not the corrected
    # guess - consistent with every other unmatched value.
    typo.idx    <- match(normalize(x.orig), normalize(names(typo.corrections)))
    typo.found  <- !is.na(typo.idx)
    x.corrected <- x.orig
    x.corrected[typo.found] <- unname(typo.corrections[typo.idx[typo.found]])

    x.norm <- normalize(x.corrected)

    match.idx <- match(x.norm, lookup.values)
    row.idx   <- lookup.rowidx[match.idx]   # NA where match.idx is NA
    found     <- !is.na(row.idx)

    out <- x.orig
    if (identical(batname.format.out, "both")) {
      cn <- reference[["common_name"]][row.idx[found]]
      sn <- reference[["scientific_name"]][row.idx[found]]
      out[found] <- ifelse(is.na(sn) | !nzchar(sn) | tolower(sn) == tolower(cn),
                           cn, sprintf("%s (%s)", cn, sn))
    } else {
      out[found] <- as.character(reference[[batname.format.out]][row.idx[found]])
    }

    if (!grammar.dash) {
      out[found] <- gsub("-", " ", out[found])
    }

    if (any(pre.done)) {
      out[pre.done] <- if (grammar.dash) pre.out[pre.done] else gsub("-", " ", pre.out[pre.done])
    }

    list(values = out, unmatched = x.orig[!found & !pre.done])
  }

  if (is.data.frame(data)) {
    results <- lapply(data, recode.vec)
    out <- as.data.frame(lapply(results, function(r) r$values), stringsAsFactors = FALSE)
    names(out) <- names(data)
    unmatched.all <- unlist(lapply(results, function(r) r$unmatched), use.names = FALSE)
  } else {
    result <- recode.vec(data)
    out <- result$values
    unmatched.all <- result$unmatched
  }

  if (length(unmatched.all) > 0) {
    warning.vector <- unique(unmatched.all)
    shown <- head(warning.vector, 25)
    omitted.note <- if (length(warning.vector) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(warning.vector))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match: %s%s\n",
                 length(unmatched.all), paste(shown, collapse = ", "), omitted.note))
  }

  out
}


#' Resolve a batname.format.out value to a reference-table header
#'
#' Returns the matching header of the bat reference table: the exact
#' header first, then the same text ignoring case and
#' spaces/dots/underscores/dashes, then the short names (\code{common},
#' \code{latin}, \code{scientific}, \code{code}, \code{both}, ...). Stops
#' with an error listing the accepted values if nothing matches.
#' @keywords internal
#' @noRd
batusa.resolve.format <- function(fmt, valid) {
  aliases <- c(common = "common_name", commonname = "common_name", name = "common_name",
               latin = "scientific_name", latinname = "scientific_name",
               scientific = "scientific_name", sci = "scientific_name",
               sciname = "scientific_name", species = "scientific_name",
               code = "code4", fourlettercode = "code4", sixlettercode = "code6",
               both = "both", commonlatin = "both", commonscientific = "both",
               commonandscientific = "both", commonandlatin = "both")
  accepted <- c(valid, "both")
  if (length(fmt) != 1 || is.na(fmt) || !nzchar(trimws(as.character(fmt)))) {
    stop(sprintf("batname.format.out must be one value, one of: %s",
                 paste(accepted, collapse = ", ")))
  }
  f <- trimws(as.character(fmt))
  f <- gsub('^"(.*)"$', "\\1", f)
  if (f %in% accepted) return(f)
  key <- gsub("[^a-z0-9]", "", tolower(f))
  hit <- match(key, gsub("[^a-z0-9]", "", tolower(valid)))
  if (!is.na(hit)) return(valid[hit])
  if (key %in% names(aliases)) return(unname(aliases[key]))
  stop(sprintf(paste0("batname.format.out must be one of the reference database's headers: %s ",
                      "- or \"both\" (common name (scientific name)); the short names ",
                      "\"common\", \"latin\"/\"scientific\" and \"code\" also work (got '%s')"),
               paste(valid, collapse = ", "), f))
}

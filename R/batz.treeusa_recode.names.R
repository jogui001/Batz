#' Recode tree and shrub names
#'
#' Looks up tree/shrub identifiers (scientific name or either of two common
#' names) in a reference table read from disk and returns the requested
#' reference column(s) (\code{head.out}), e.g. common name, scientific name,
#' family or growth habit. The base table can be extended or replaced with
#' extra plant lists (\code{reference.data}, \code{pattern}).
#'
#' @param data A character (or factor) vector of tree/shrub names. A data
#'   frame is not accepted (error).
#' @param head.out Character vector, default \code{"common.name"}. One or
#'   more reference-table columns to return (see Details for the columns).
#'   An unknown column stops with an error listing the valid ones.
#' @param dir.load Character, default \code{getwd()}. Folder searched for the
#'   base reference file (\code{load.pattern}) and for extra files
#'   (\code{pattern}).
#' @param load.pattern Character vector of file-name wildcards, default
#'   \code{c("*USA.treeshrub_recode.names*", "*tree_species_and_shrubs*")}.
#'   The base reference file (CSV, XLSX or XLS); matching ignores case. If
#'   several files match, the first is used and a NOTE is printed.
#' @param dir.sub Logical, default \code{TRUE}. Also search subfolders of
#'   \code{dir.load} (for both \code{load.pattern} and \code{pattern}).
#' @param pattern Character vector of file names/wildcards, default
#'   \code{"plant.names.csv"}. Extra reference files to load when
#'   \code{reference.data} is \code{"append"} or \code{"overwrite"};
#'   ignored for \code{"default"}.
#' @param reference.data Character, default \code{"default"}. One of
#'   \code{"default"} (base table only), \code{"append"} (base table plus
#'   the rows of every usable \code{pattern} file) or \code{"overwrite"}
#'   (only the rows of the \code{pattern} files; the base table is used if
#'   none is usable).
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   accented letters and common symbols are simplified (\code{é} to
#'   \code{e}, \code{×} to \code{X}, curly quotes to straight quotes) and
#'   other non-ASCII characters (e.g. \code{°}, \code{µ}, \code{™}) are
#'   removed, in every reference file (headers and values) and in
#'   \code{data}.
#'
#' @return If \code{head.out} has one value, a character vector the same
#'   length as \code{data}; otherwise a data frame with one column per
#'   \code{head.out} value, in \code{data} order. Unmatched values are
#'   returned unchanged in every column. When any value is unmatched, a data
#'   frame \code{treesmismatch.log} is also assigned in the calling
#'   environment (see Details).
#'
#' @details
#' \strong{Reference table.} The base file's headers are standardized
#' (lower case, non-alphanumeric runs turned into \code{_}), then the old
#' names \code{common_one}, \code{common_two}, \code{grouping_one},
#' \code{grouping_two}, \code{grouping_three}, \code{growth_habit},
#' \code{native_status} and \code{wood_type} are renamed to
#' \code{common.name}, \code{common.name2}, \code{grouping.one},
#' \code{grouping.two}, \code{grouping.three}, \code{growth.habit},
#' \code{native.status} and \code{wood.type}. Expected columns:
#' \code{scientific_name}, \code{common.name}, \code{common.name2},
#' \code{genus}, \code{family}, \code{native.status}, \code{growth.habit},
#' \code{wood.type}, \code{grouping.one}, \code{grouping.two},
#' \code{grouping.three}. Any column present can be used in
#' \code{head.out}. All values are read as text and trimmed. CSV files are
#' read as UTF-8, falling back to Latin-1; Excel files need \pkg{readxl}.
#'
#' \strong{Matching.} Each value is compared with \code{scientific_name},
#' \code{common.name} and \code{common.name2} after lower-casing and
#' removing every character that is not a-z or 0-9, so
#' \code{"Ash-leaved Maple"}, \code{"ashleaved_maple"} and
#' \code{"ASHLEAVEDMAPLE"} all match. The first match wins (column order
#' above, then row order), so a common name shared by several species
#' always returns the first one. Returned values are the reference text,
#' not the normalized text.
#'
#' \strong{Extra reference files} (\code{reference.data = "append"} or
#' \code{"overwrite"}). Every file matching \code{pattern} is read in turn:
#' \enumerate{
#'   \item Headers are standardized and mapped to the ten plant columns
#'     (all except \code{wood.type}): exact name first; then keywords
#'     (\code{species}/\code{latin}/\code{scientific}/\code{sciname},
#'     \code{genus}, \code{family}, \code{native}/\code{status},
#'     \code{growth}/\code{habit}, \code{common} for \code{common.name} then
#'     \code{common.name2}, \code{group} for \code{grouping.one} to
#'     \code{grouping.three}, in file column order); then content (mostly
#'     two-word capitalized text = \code{scientific_name}; mostly
#'     native/introduced/invasive words = \code{native.status}; mostly
#'     tree/shrub/vine words = \code{growth.habit}). Unmapped columns are
#'     dropped. This mapping is a best guess.
#'   \item A file without \code{scientific_name}, a common-name column and a
#'     grouping column is skipped, with the message
#'     \code{"<file> <path> skipped as missing required headers: ..."}. An
#'     unreadable file is skipped with \code{"... skipped as unreadable"}.
#'   \item Missing columns are filled: one common name copies the other;
#'     missing grouping columns copy the first grouping column present;
#'     \code{genus} is the first word of \code{scientific_name};
#'     \code{family}, \code{native.status} and \code{growth.habit} are
#'     \code{NA}. The message is \code{"<file> <path> complete success very
#'     nice!"} when nothing was filled, otherwise \code{"<file> <path> loaded
#'     but padded as missing these headers: ..."}.
#'   \item The rows of all usable files are combined. \code{"append"} adds
#'     them under the base table (\code{wood.type} is \code{NA} for them);
#'     \code{"overwrite"} uses only them. If no file matches or none is
#'     usable, a NOTE is printed (when nothing matched) and the base table
#'     is used.
#' }
#'
#' \strong{Unmatched values.} \code{"These inputs were missing:"} and the
#' unique unmatched values are printed, and \code{treesmismatch.log} is
#' assigned in the calling environment with one row per unique unmatched
#' value: \code{$input}, \code{$mismatch.count} (number of times it occurs
#' in \code{data}), \code{$closest.match} (nearest
#' \code{scientific_name}/\code{common.name}/\code{common.name2} value by
#' edit distance on the normalized text), \code{$strip.special}
#' (\code{"FALSE"}, \code{"TRUE NONE"}, or \code{"TRUE ; <header>; ..."}
#' naming the reference-file columns that changed),
#' \code{$accented.letters.header}, \code{$removed.symbols.header},
#' \code{$accented.letters.data}, \code{$removed.symbols.data} (number of
#' unique column names / values in the reference files with a character
#' simplified or removed; \code{NA} when \code{strip.special = FALSE}).
#' When every value matches, \code{treesmismatch.log} is not created or
#' updated.
#'
#' @seealso \code{\link{batz.batusa_recode.names}}
#'
#' @examples
#' \dontrun{
#' batz.treeusa_recode.names(c("sugar maple", "RED OAK", "not.a.real.tree"))
#' # unmatched "not.a.real.tree" is returned as is; see treesmismatch.log
#'
#' batz.treeusa_recode.names("Ash-leaved Maple", head.out = "scientific_name")
#'
#' batz.treeusa_recode.names(c("Sugar Maple", "Red Oak"),
#'                            head.out = c("scientific_name", "family", "growth.habit"))
#'
#' # add the rows of every "plant.names.csv" under dir.load
#' batz.treeusa_recode.names("sugar maple", reference.data = "append")
#'
#' # use only the extra files
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
                                       reference.data = "default",
                                       strip.special  = TRUE) {

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

  ## special characters (per Josh, 2026-09-30): every header stripped in
  ## any reference/supplemental file this call loads, for
  ## treesmismatch.log's $strip.special column - see @details.
  strip.headers.all <- character(0)
  strip.counts.all  <- NULL   # 2026-10-02: simplified/removed counts across reference files

  read.ref.file <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "csv") {
      df <- special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reference file '", basename(f), "' is an Excel file, but the 'readxl' package is not installed.")
      }
      df <- as.data.frame(readxl::read_excel(f), stringsAsFactors = FALSE)
    } else {
      stop("Reference file '", basename(f), "' has an unsupported extension (expected .csv/.xlsx/.xls).")
    }
    s  <- special.strip(df, strip.special)
    df <- s$df
    strip.headers.all <<- unique(c(strip.headers.all, s$headers))
    strip.counts.all  <<- special.counts.add(strip.counts.all, s$counts)
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
  ## 2026-10-02, per Josh: simplify special characters in the input names
  ## too (e.g. "Quercus \u00d7 bebbiana" -> "Quercus X bebbiana"), so they
  ## still match the simplified reference table.
  if (isTRUE(strip.special) && (is.character(data) || is.factor(data))) {
    data <- special.simplify(as.character(data))$x
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
      mismatch.count = as.integer(vapply(unmatched.unique, function(v) sum(unmatched.instances == v), integer(1))),
      closest.match   = closest,
      stringsAsFactors = FALSE
    )
    treesmismatch.log <- cbind(treesmismatch.log,
      special.log.cols(strip.special, strip.headers.all, strip.counts.all)[rep(1L, nrow(treesmismatch.log)), , drop = FALSE],
      row.names = NULL)

    cat("These inputs were missing:\n")
    print(unmatched.unique)

    assign("treesmismatch.log", treesmismatch.log, envir = parent.frame())
  }

  out
}

#' Merge vetted bat-acoustic-call files (k-Pro/Kaleidoscope or SonoBat) into one master table
#'
#' Searches a directory for vetted bat-call files exported from
#' k-Pro/Kaleidoscope or SonoBat, merges every file that has the required
#' columns into one data frame, splits coordinates, reads the ARU name, date
#' and time from the wav file name, adds \code{$call.datetime}, recodes
#' species IDs to one format and (optionally) fills blank manual IDs from
#' the auto IDs. Noise/NoID rows are kept; filter them downstream if needed.
#'
#' @details
#' \strong{Loading.} Files matching \code{load.pattern} (case-insensitive)
#' are read with \code{special.read.csv()} (UTF-8 with a Latin-1 fallback).
#' With \code{strip.special = TRUE}, accented letters and common symbols are
#' simplified (e.g. an accented e becomes \code{e}, curly quotes become
#' straight quotes) and other non-ASCII characters (e.g. a degree sign) are
#' removed from column names and data. Headers are then standardized with
#' \code{standardize.headers()} (lower case, spaces and punctuation become
#' \code{"_"}).
#'
#' \strong{Required columns} (after standardization; alternatives in
#' brackets are used only when the standard column is missing):
#' \itemize{
#'   \item \code{Filename} (\code{IN FILE}, \code{File Name}, \code{File}).
#'   \item \code{MonitoringNight} (\code{date.monitoringnight},
#'     \code{DATE-12}); renamed \code{$date.monitoringnight}. Its values are
#'     kept as they are (not converted to dates).
#'   \item \code{Species Manual ID}, or another header made only of the words
#'     Manual, Species, ID and Identification that ends in ID or
#'     Identification and contains Manual or Species (e.g. \code{Manual ID},
#'     \code{Species ID}). The first one found is used. A
#'     \code{$species.code} column is not read as the manual ID.
#'   \item \code{Lat} (\code{Latitude}). It normally holds
#'     \verb{"<lat> <lon>"}; if a separate \code{Longitude} (\code{Long},
#'     \code{Lon}) column exists, it is joined onto \code{Lat} first.
#'   \item At least one auto-ID column: k-Pro \code{WA|Kaleidoscope|Auto ID},
#'     SonoBat \code{SppAccp}, or a header named in \code{autoid.header_in}.
#'     A missing k-Pro or SonoBat column is filled with \code{NA}.
#' }
#' Optional columns: \code{Serial} or \code{aru.serial} (becomes
#' \code{$aru.serial}, else \code{NA}), \code{sunregion} (copied to
#' \code{$sunregion}, else \code{NA}), and \code{aru.name} or \code{site}
#' (see below).
#'
#' \strong{Skipped files.} A file is skipped with \code{$reason}
#' \code{"could not read file"}, \code{"mismatched headers"} (with the
#' missing headers in \code{$headers.missing}) or \code{"no records"}.
#'
#' \strong{Processing after merging.}
#' \enumerate{
#'   \item If \code{duplicates.remove = TRUE}, exact duplicate rows are
#'     removed.
#'   \item \code{$lat} (\verb{"<lat> <lon>"}) is split into numeric
#'     \code{$lat} and \code{$longitude}.
#'   \item \code{$filename} is parsed as SM4
#'     \verb{<ARU>_<YYYYMMDD>_<HHMMSS>_...} or SM5
#'     \verb{<ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_...} into \code{$aru.name},
#'     \code{$date} (\code{"YYYYMMDD"} text), \code{$time} (\code{"HHMMSS"}
#'     text) and \code{$aru.model} (\code{"SM4"}, \code{"SM5"}, or \code{NA}
#'     if neither). A recorder from another maker with the same naming shows
#'     as \code{"SM4"}. If the file has an \code{aru.name} or \code{site}
#'     column and a serial column, and the first part of a row's wav name
#'     equals that row's serial number, \code{$aru.name} is taken from the
#'     \code{aru.name}/\code{site} column instead.
#'   \item Columns are renamed: Species Manual ID to \code{$manid} (the
#'     species the reviewer assigned to the file), k-Pro auto ID to
#'     \code{$autoid.kp}, SonoBat auto ID to \code{$autoid.sb}.
#'   \item \code{\link{batz.datawrangler_call.datetime}} combines
#'     \code{$date} and \code{$time} into \code{$call.datetime}.
#'   \item \code{$manid}, \code{$autoid.kp}, \code{$autoid.sb} and any extra
#'     auto-ID column are recoded with \code{\link{batz.batusa_recode.names}}
#'     (\code{batname.format.out = bat.names.out}, \code{regions =
#'     regions}). Blank values, \code{"NOISE"} and \code{"NoID"} are not
#'     sent and stay as they are; other unmatched values are reported by
#'     that function.
#'   \item If \code{manid.kp = TRUE}, \code{$manid.kp} is a copy of
#'     \code{$manid} with blanks (\code{NA} or empty text) filled from
#'     \code{$autoid.kp}. \code{manid.sb} does the same from
#'     \code{$autoid.sb}. No \code{$manid.<other>} column is made for an
#'     extra auto-ID column.
#' }
#'
#' \strong{Log.} With \code{log.file = TRUE}, \code{vetted.merged_log.file}
#' lists the skipped files only. If nothing was skipped, no log is created
#' (a log left from an earlier call is removed) and
#' \code{"All <n> files successfully merged, no log file created"} is
#' printed (or a "no files found" message). The log is kept in R only.
#'
#' @param dir.load Character, default \code{getwd()}. Directory to search.
#' @param load.pattern Character vector, default \code{c("*vetted.csv")}.
#'   Wildcard (glob) file-name pattern(s) of the files to load; matching is
#'   case-insensitive.
#' @param dir.sub Logical, default \code{FALSE}. If \code{TRUE}, also
#'   search subfolders of \code{dir.load}.
#' @param duplicates.remove Logical, default \code{TRUE}. Remove exact
#'   duplicate rows from the merged table (before the columns are split,
#'   parsed and recoded).
#' @param log.file Logical, default \code{FALSE}. If \code{TRUE}, also
#'   create \code{vetted.merged_log.file} when at least one file was
#'   skipped (see Details).
#' @param bat.names.out Character, default \code{"code4"}. Output name
#'   format passed as \code{batname.format.out} to
#'   \code{\link{batz.batusa_recode.names}} (e.g. \code{"code4"},
#'   \code{"code6"}, \code{"common_name"}, \code{"scientific_name"},
#'   \code{"label_name"}, \code{"label2_name"}, \code{"label3_name"}).
#' @param manid.kp Logical, default \code{TRUE}. Create \code{$manid.kp}
#'   (\code{$manid} with blanks filled from \code{$autoid.kp}).
#' @param manid.sb Logical, default \code{TRUE}. Create \code{$manid.sb}
#'   (\code{$manid} with blanks filled from \code{$autoid.sb}).
#' @param snake_case Logical, default \code{FALSE}. If \code{TRUE}, every
#'   \code{"."} in the output column names of \code{vetted.merged} and the
#'   log is replaced by \code{"_"} (\code{snake.headers()}), e.g.
#'   \code{$aru_name}. The log's \code{$strip.special} and count columns
#'   keep their names.
#' @param strip.special Logical, default \code{TRUE}. If \code{TRUE},
#'   simplify or remove non-ASCII characters in each loaded file (see
#'   Details). Files are always read with a UTF-8/Latin-1 fallback.
#' @param regions Character vector, default \code{c("ALL", "NABAT")}.
#'   Region(s) the bats were recorded in, passed to
#'   \code{\link{batz.batusa_recode.names}}. Only records of the
#'   \code{BAT Groupings}, \code{NABATFrequency Classes} and
#'   \code{bat.names} tables whose \code{$region} matches one of these (or
#'   is \code{All}) are used. A \code{$region} cell may list several regions
#'   separated by \code{;} or \code{,}; case and quotes are ignored.
#' @param autoid.header_in Character vector, default \code{c("autoid")}.
#'   Other names for an auto-ID column, for vetting sheets not made with the
#'   standard BRI pipeline. Compared as a whole header after
#'   standardization (e.g. \code{"AUTO ID"} matches \code{AUTO ID} or
#'   \code{auto_id}, not \code{WA|Kaleidoscope|Auto ID}). Used only when the
#'   file does not already have the target column.
#' @param autoid.header_out Character vector, default \code{c("auto.kp")},
#'   the same length as \code{autoid.header_in} (otherwise the function
#'   stops). The program each \code{autoid.header_in} header comes from:
#'   \code{"auto.kp"} (merged into \code{$autoid.kp}) or \code{"auto.sb"}
#'   (merged into \code{$autoid.sb}). Any other name is treated as another
#'   vetting program and becomes its own output column with exactly that
#'   name (after \code{$autoid.sb}; \code{NA} for files without it), recoded
#'   like the other ID columns.
#'
#' @return Invisibly, a named list with \code{vetted.merged} and, when
#'   \code{log.file = TRUE} and a file was skipped,
#'   \code{vetted.merged_log.file}. The same objects are also assigned in
#'   the calling environment.
#'   \describe{
#'     \item{vetted.merged}{Columns \code{$filename},
#'       \code{$date.monitoringnight}, \code{$aru.name}, \code{$aru.model},
#'       \code{$aru.serial}, \code{$sunregion}, \code{$lat},
#'       \code{$longitude}, \code{$manid}, \code{$autoid.kp},
#'       \code{$autoid.sb}, any extra auto-ID columns, \code{$date},
#'       \code{$time}, \code{$call.datetime}, then \code{$manid.kp} and
#'       \code{$manid.sb} if selected. An empty data frame if no file was
#'       merged.}
#'     \item{vetted.merged_log.file}{One row per skipped file:
#'       \code{$filepath}, \code{$reason}, \code{$headers.missing},
#'       \code{$aru.model} (\code{"SM4"}, \code{"SM5"} or \code{"SM4; SM5"}
#'       found in the file), \code{$strip.special} (\code{"FALSE"},
#'       \code{"TRUE NONE"}, or \code{"TRUE ; <header>; ..."} listing the
#'       headers where characters were changed), and
#'       \code{$accented.letters.header}, \code{$removed.symbols.header},
#'       \code{$accented.letters.data}, \code{$removed.symbols.data} (number
#'       of unique column names / data values with a character simplified or
#'       removed; \code{NA} when \code{strip.special = FALSE}).}
#'   }
#'
#' @examples
#' \dontrun{
#' # creates `vetted.merged` (and `vetted.merged_log.file`, if a file was
#' # skipped) in the calling environment
#' batz.merge_vetted.acoustics(dir.load = "path/to/data", dir.sub = TRUE,
#'                             log.file = TRUE)
#' head(vetted.merged)
#'
#' # common names, snake_case headers, extra auto-ID column from another sheet
#' res <- batz.merge_vetted.acoustics(bat.names.out = "common_name",
#'                                    snake_case = TRUE,
#'                                    autoid.header_in = c("autoid", "Auto ID SB"),
#'                                    autoid.header_out = c("auto.kp", "auto.sb"))
#' }
#'
#' @export
batz.merge_vetted.acoustics <- function(dir.load = getwd(),
                                               load.pattern = c("*vetted.csv"),
                                               dir.sub = FALSE,
                                               duplicates.remove = TRUE,
                                               log.file = FALSE,
                                               bat.names.out = "code4",
                                               manid.kp = TRUE,
                                               manid.sb = TRUE,
                                               snake_case = FALSE,
                                               strip.special = TRUE,
                                               regions = c("ALL", "NABAT"),
                                               autoid.header_in = c("autoid"),
                                               autoid.header_out = c("auto.kp")) {

  ## 2026-10-23, per Josh: alternative auto-ID headers (non-BRI vetting
  ## sheets). autoid.header_in[i] is another name for autoid.header_out[i].
  if (length(autoid.header_in) != length(autoid.header_out)) {
    stop(paste0("the inputs \"autoid.header_in\" and \"autoid.header_out\" are of unequal lengths ",
                "so they are mis-matched, make both have the same vector length:\n",
                "autoid.header_in has a length of ", length(autoid.header_in), "\n",
                "autoid.header_out has a length of ", length(autoid.header_out)), call. = FALSE)
  }
  autoid.header_out <- trimws(as.character(autoid.header_out))
  ## "auto.kp"/"auto.sb" (or "autoid.kp"/"autoid.sb") are the standard k-Pro/
  ## SonoBat columns; anything else is another vetting program and becomes
  ## its own output column, named exactly as given.
  autoid.target <- ifelse(tolower(autoid.header_out) %in% c("auto.kp", "autoid.kp"), "wa_kaleidoscope_auto_id",
                   ifelse(tolower(autoid.header_out) %in% c("auto.sb", "autoid.sb"), "sppaccp",
                          autoid.header_out))
  autoid.in.std <- standardize.headers(autoid.header_in)
  extra.autoid <- unique(autoid.target[!(autoid.target %in% c("wa_kaleidoscope_auto_id", "sppaccp"))])

  ## 2026-10-23: a header made only of the words Manual / Species / ID /
  ## Identification, ending in ID or Identification and containing Manual or
  ## Species (e.g. "Manual ID", "Species ID", "Manual Species Identification")
  ## is read as "Species Manual ID".
  is.manual.id <- function(h) {
    vapply(strsplit(h, "_", fixed = TRUE), function(tok) {
      length(tok) >= 2 && all(tok %in% c("manual", "species", "id", "identification")) &&
        tok[length(tok)] %in% c("id", "identification") && any(tok %in% c("manual", "species"))
    }, logical(1))
  }

  ## header standardization (per Josh, 2026-09-14 project preference):
  ## expected.headers is a literal, uninvented copy of the vetting
  ## software's own real column text, so it is standardized right along
  ## with incoming raw headers via the shared standardize.headers()
  ## helper - see @details "Header standardization" above.
  expected.headers <- c("filename", "date.monitoringnight", "species_manual_id",
                         "wa_kaleidoscope_auto_id", "sppaccp", "lat")

  regex.pattern <- paste(utils::glob2rx(load.pattern), collapse = "|")
  files <- list.files(dir.load, pattern = regex.pattern, recursive = dir.sub,
                       full.names = TRUE, ignore.case = TRUE)

  vetted.merged <- data.frame()
  log.rows <- list()

  ## 2026-10-02, per Josh: SM4 vs SM5 recordings have different file names -
  ##   SM4: <ARU>_<YYYYMMDD>_<HHMMSS>_<junk>
  ##   SM5: <ARU>_<MIC>_<YYYYMMDD>_<HHMMSS>_<junk>
  ## parse.aru.filename() works out which, row by row, and pulls out the ARU
  ## name, date and time. The part after HHMMSS may also be just the file
  ## extension (e.g. ".wav") or nothing.
  parse.aru.filename <- function(fn) {
    fn <- as.character(fn)
    rx5 <- "^([^_]+)_([^_]+)_(\\d{8})_(\\d{6})(_|\\.|$)"
    rx4 <- "^([^_]+)_(\\d{8})_(\\d{6})(_|\\.|$)"
    m4 <- regmatches(fn, regexec(rx4, fn))
    m5 <- regmatches(fn, regexec(rx5, fn))
    out <- data.frame(aru.name = rep(NA_character_, length(fn)), aru.model = NA_character_,
                      date = NA_character_, time = NA_character_, stringsAsFactors = FALSE)
    for (i in seq_along(fn)) {
      if (length(m4[[i]]) >= 4) {
        out[i, ] <- c(m4[[i]][2], "SM4", m4[[i]][3], m4[[i]][4])
      } else if (length(m5[[i]]) >= 5) {
        out[i, ] <- c(m5[[i]][2], "SM5", m5[[i]][4], m5[[i]][5])
      }
    }
    out
  }
  ## one log value per file: "SM4", "SM5", "SM4; SM5" (mixed) or NA
  file.aru.model <- function(df) {
    if (is.null(df) || !("filename" %in% names(df)) || nrow(df) == 0) return(NA_character_)
    mods <- sort(unique(stats::na.omit(parse.aru.filename(df$filename)$aru.model)))
    if (length(mods) == 0) NA_character_ else paste(mods, collapse = "; ")
  }

  add.log <- function(filepath, reason, headers.missing, special.headers = character(0)) {
    log.rows[[length(log.rows) + 1]] <<- data.frame(
      filepath = filepath, reason = reason, headers.missing = headers.missing,
      aru.model = cur.model,
      special.log.cols(strip.special, special.headers, cur.counts),
      stringsAsFactors = FALSE
    )
  }

  cur.counts <- NULL   # 2026-10-02: simplified/removed counts for the current file's log row
  cur.model <- NA_character_
  for (f in files) {
    cur.counts <- NULL
    cur.model <- NA_character_
    tmp <- tryCatch(special.read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { add.log(f, "could not read file", "none"); next }
    ## strip special characters right after loading (2026-09-30)
    s.strip <- special.strip(tmp, strip.special)
    tmp <- s.strip$df
    special.hdrs <- standardize.headers(s.strip$headers)
    cur.counts <- s.strip$counts
    special.hdrs[special.hdrs == "monitoringnight"] <- "date.monitoringnight"
    names(tmp) <- standardize.headers(names(tmp))
    cur.model <- file.aru.model(tmp)
    ## Column identifiers renamed 2026-09-27 (see @details, "Column
    ## identifiers renamed" above): this function's own required raw-column
    ## identifier is now "date.monitoringnight", but the real vetting-
    ## software header ("MonitoringNight") still standardizes to the OLD
    ## spelling "monitoringnight" - accepted here as a legacy input alias so
    ## real incoming files keep matching.
    ## 2026-10-02, per Josh: also accept "DATE-12" (Kaleidoscope's
    ## noon-shifted date) and a literal "date.monitoringnight" (which
    ## standardizes to "date_monitoringnight") - first one found is used.
    if (!("date.monitoringnight" %in% names(tmp))) {
      mn.alias <- intersect(c("date_monitoringnight", "monitoringnight", "date_12"), names(tmp))
      if (length(mn.alias) > 0) names(tmp)[names(tmp) == mn.alias[1]] <- "date.monitoringnight"
    }
    ## 2026-10-23 (later), per Josh - other export layouts (e.g. a
    ## Kaleidoscope export with "IN FILE", separate LATITUDE/LONGITUDE):
    ## "IN FILE"/"File Name"/"File" is read as Filename, "Latitude" as Lat,
    ## and a separate Longitude/Long/Lon column is joined onto Lat as
    ## "<lat> <lon>" (the format the standard files use, split again below).
    if (!("filename" %in% names(tmp))) {
      fa <- intersect(c("in_file", "file_name", "file"), names(tmp))
      if (length(fa) > 0) names(tmp)[names(tmp) == fa[1]] <- "filename"
    }
    if (!("lat" %in% names(tmp)) && "latitude" %in% names(tmp)) {
      names(tmp)[names(tmp) == "latitude"] <- "lat"
    }
    lon.col <- intersect(c("longitude", "long", "lon"), names(tmp))
    if ("lat" %in% names(tmp) && length(lon.col) > 0) {
      la <- trimws(ifelse(is.na(tmp$lat), "", as.character(tmp$lat)))
      lo <- trimws(ifelse(is.na(tmp[[lon.col[1]]]), "", as.character(tmp[[lon.col[1]]])))
      join <- nzchar(la) & nzchar(lo) & !grepl("\\s", la)
      la[join] <- paste(la[join], lo[join])
      tmp$lat <- la
    }
    ## "Species Manual ID" alternatives (2026-10-23) - first one found is used
    if (!("species_manual_id" %in% names(tmp))) {
      mid <- names(tmp)[is.manual.id(names(tmp))]
      ## 2026-11-04 (later), per Josh: $species.code is NOT read as the manual ID -
      ## $manid (reviewer's ID from the vetting sheet) and $species.code
      ## (batz.batusa_list.species() code) are separate categories.
      if (length(mid) > 0) names(tmp)[names(tmp) == mid[1]] <- "species_manual_id"
    }
    ## alternative auto-ID headers (2026-10-23) - only used when the file
    ## doesn't already have the standard column
    for (i in seq_along(autoid.in.std)) {
      if (autoid.in.std[i] %in% names(tmp) && !(autoid.target[i] %in% names(tmp))) {
        names(tmp)[names(tmp) == autoid.in.std[i]] <- autoid.target[i]
      }
    }
    ## 2026-10-23: the k-Pro (wa_kaleidoscope_auto_id) and SonoBat (sppaccp)
    ## columns are no longer both required - a file needs at least one auto-ID
    ## column (k-Pro, SonoBat or one named in autoid.header_out); a missing one
    ## is filled with NA.
    auto.headers <- c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)
    missing.headers <- setdiff(setdiff(expected.headers, auto.headers[1:2]), names(tmp))
    if (!any(auto.headers %in% names(tmp))) {
      missing.headers <- c(missing.headers, paste(auto.headers, collapse = " or "))
    }
    if (length(missing.headers) > 0) {
      add.log(f, "mismatched headers", paste(missing.headers, collapse = ", "), special.hdrs); next
    }
    if (nrow(tmp) == 0) { add.log(f, "no records", "none", special.hdrs); next }
    ## $aru.serial is OPTIONAL (2026-08-30 follow-up), not one of the required
    ## expected.headers - a file is never skipped for lacking it (e.g. a
    ## Mobile-transect export, which has no fixed instrument serial number at
    ## all). Captured BEFORE trimming to expected.headers below, exactly like
    ## $sunregion, since that trim would otherwise silently drop it. The real
    ## vetting-software header ("Serial") standardizes to the OLD spelling
    ## "serial" - kept here as a legacy input alias (renamed 2026-09-27, see
    ## @details, "Column identifiers renamed" above) while this function's
    ## own internal/output column is now $aru.serial.
    ## 2026-10-23 (later): "aru.serial" (standardized aru_serial) accepted too
    ser.col <- intersect(c("aru_serial", "serial"), names(tmp))
    aru.serial.vals <- if (length(ser.col) > 0) as.character(tmp[[ser.col[1]]]) else
      rep(NA_character_, nrow(tmp))
    ## 2026-10-23 (later), per Josh: if the file has an aru.name or site column
    ## AND a serial column, and the first part of the wav name is that row's
    ## serial number (e.g. "656346_20230506_180008_000.wav", serial 656346),
    ## the ARU name is taken from the aru.name/site column instead of the wav
    ## name. Otherwise it is read from the wav name as before.
    aru.name.vals <- rep(NA_character_, nrow(tmp))
    name.col <- intersect(c("aru_name", "site"), names(tmp))
    if (length(name.col) > 0 && length(ser.col) > 0) {
      fn.aru <- parse.aru.filename(tmp$filename)$aru.name
      nm.vals <- trimws(as.character(tmp[[name.col[1]]]))
      use <- !is.na(fn.aru) & !is.na(aru.serial.vals) & trimws(fn.aru) == trimws(aru.serial.vals) &
        !is.na(nm.vals) & nzchar(nm.vals)
      aru.name.vals[use] <- nm.vals[use]
    }
    ## $sunregion is OPTIONAL, not one of the required expected.headers - a
    ## file is never skipped for lacking it. If a file's own (standardized)
    ## headers happen to include it, copy those values straight through;
    ## otherwise this file's rows get NA for $sunregion (still needs to be
    ## joined in separately, exactly as before, for files that don't already
    ## carry it). Captured BEFORE trimming to expected.headers below, since
    ## that trim would otherwise silently drop it.
    sunregion.vals <- if ("sunregion" %in% names(tmp)) as.character(tmp$sunregion) else
      rep(NA_character_, nrow(tmp))
    for (h in c(auto.headers)) if (!(h %in% names(tmp))) tmp[[h]] <- NA_character_
    tmp <- tmp[, c(expected.headers, extra.autoid), drop = FALSE]
    tmp[c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)] <-
      lapply(tmp[c("wa_kaleidoscope_auto_id", "sppaccp", extra.autoid)], as.character)
    tmp$aru.serial <- aru.serial.vals
    tmp$sunregion <- sunregion.vals
    tmp$aru.name.file <- aru.name.vals
    vetted.merged <- rbind(vetted.merged, tmp)
  }

  if (duplicates.remove && nrow(vetted.merged) > 0) {
    vetted.merged <- vetted.merged[!duplicated(vetted.merged), ]
  }

  if (nrow(vetted.merged) > 0) {
    ## $lat holds "<lat> <lon>" - split into two numeric columns
    latlon <- strsplit(trimws(vetted.merged$lat), "\\s+")
    vetted.merged$lat <- vapply(latlon, function(x) as.numeric(x[1]), numeric(1))
    vetted.merged$lon <- vapply(latlon, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA_real_, numeric(1))

    ## $filename -> $aru.name/$date/$time, SM4 or SM5 layout (2026-10-02)
    fn.parts <- parse.aru.filename(vetted.merged$filename)
    vetted.merged$aru.name <- ifelse(is.na(vetted.merged$aru.name.file), fn.parts$aru.name,
                                     vetted.merged$aru.name.file)
    vetted.merged$date     <- fn.parts$date
    vetted.merged$time     <- fn.parts$time
    aru.model.vals         <- fn.parts$aru.model

    ## --- rename, reorder, call.datetime, recode.names, manid.kp/sb
    ## fill-in - see @details above -----------------

    ## positional rename ($sunregion, appended right after $aru.serial back
    ## in the per-file loop above, keeps its own name here - no rename
    ## needed)
    ## 2026-10-23: renamed by name instead of position, so extra auto-ID
    ## columns (autoid.header_out) can sit in between
    rename.map <- c(species_manual_id = "manid", wa_kaleidoscope_auto_id = "autoid.kp",
                    sppaccp = "autoid.sb", lon = "longitude")
    hit <- names(vetted.merged) %in% names(rename.map)
    names(vetted.merged)[hit] <- rename.map[names(vetted.merged)[hit]]

    ## reorder ($sunregion placed with the other detector-level columns,
    ## next to $aru.serial/$aru.name)
    vetted.merged$aru.model <- aru.model.vals
    vetted.merged <- vetted.merged[, c("filename", "date.monitoringnight", "aru.name", "aru.model",
                                        "aru.serial", "sunregion", "lat", "longitude",
                                        "manid", "autoid.kp", "autoid.sb", extra.autoid,
                                        "date", "time")]

    ## $call.datetime
    vetted.merged$call.datetime <- batz.datawrangler_call.datetime(
      date = vetted.merged$date, time = vetted.merged$time)

    is.empty <- function(x) is.na(x) | !nzchar(trimws(x))

    ## recode manid/autoid.kp/autoid.sb (batname.format.out = bat.names.out, default "code4")
    ## 2026-10-23, per Josh: blanks, "NOISE" and "NoID" are not species, so
    ## they are left as they are and not sent to batz.batusa_recode.names()
    ## (which listed them as "inputs did not match"). Any other value that
    ## doesn't match is still reported.
    recode.skip <- c("noise", "noid")
    recode.ids <- function(x) {
      x <- as.character(x)
      send <- !is.empty(x) & !(tolower(trimws(x)) %in% recode.skip)
      if (any(send)) x[send] <- batz.batusa_recode.names(x[send], batname.format.out = bat.names.out,
                                                         regions = regions)
      x
    }
    vetted.merged$manid     <- recode.ids(vetted.merged$manid)
    vetted.merged$autoid.kp <- recode.ids(vetted.merged$autoid.kp)
    vetted.merged$autoid.sb <- recode.ids(vetted.merged$autoid.sb)
    for (h in extra.autoid) vetted.merged[[h]] <- recode.ids(vetted.merged[[h]])

    if (manid.kp) {
      vetted.merged$manid.kp <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.kp)
      vetted.merged$manid.kp[blank] <- vetted.merged$autoid.kp[blank]
    }

    if (manid.sb) {
      vetted.merged$manid.sb <- vetted.merged$manid
      blank <- is.empty(vetted.merged$manid.sb)
      vetted.merged$manid.sb[blank] <- vetted.merged$autoid.sb[blank]
    }
  }

  rownames(vetted.merged) <- NULL

  ## snake_case output option (per Josh's request to audit and extend this
  ## package-wide, 2026-09-22) - applied last, only to this function's own
  ## invented output column names, mirroring batz.generate_plotframe.bat()'s
  ## established pattern. See @param snake_case.
  if (snake_case) names(vetted.merged) <- snake.headers(names(vetted.merged))

  result <- list(vetted.merged = vetted.merged)

  ## 2026-10-23, per Josh: when no file was skipped there is nothing to log,
  ## so no (empty) log is created - a one-line message is printed instead. A
  ## log left over from an earlier call is removed so it isn't mistaken for
  ## this run's.
  if (log.file && length(log.rows) == 0) {
    if (length(files) == 0) {
      cat("No files matching load.pattern found, no log file created\n")
    } else {
      cat(sprintf("All %d files successfully merged, no log file created\n", length(files)))
    }
    if (exists("vetted.merged_log.file", envir = parent.frame(), inherits = FALSE)) {
      rm("vetted.merged_log.file", envir = parent.frame())
    }
  } else if (log.file) {
    vetted.merged_log.file <- do.call(rbind, log.rows)
    rownames(vetted.merged_log.file) <- NULL
    if (snake_case) {
      names(vetted.merged_log.file) <- snake.headers(names(vetted.merged_log.file))
      ## keep the log column's exact name (2026-09-30)
      special.cols <- c("strip.special", "accented.letters.header", "removed.symbols.header",
                        "accented.letters.data", "removed.symbols.data")
      hit <- match(names(vetted.merged_log.file), snake.headers(special.cols))
      names(vetted.merged_log.file)[!is.na(hit)] <- special.cols[hit[!is.na(hit)]]
    }
    result$vetted.merged_log.file <- vetted.merged_log.file
  }

  for (nm in names(result)) assign(nm, result[[nm]], envir = parent.frame())
  invisible(result)
}

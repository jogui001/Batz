#' Tidy a vetted acoustic detection file before it goes into other batz functions
#'
#' Applies the clean-up steps done by hand on the Amboseli/NKRCP vetted
#' files: lower-case headers, rename the site and manual-ID columns to
#' \code{$aru.name} and \code{$species}, recode ARU names, add a
#' \code{$community} column (taken from the letters at the start of each
#' ARU name) and a \code{$project} column, and force the sign of the
#' latitude/longitude (e.g. all latitudes south of the equator). Each step
#' can be switched off. Returns the tidied data frame and, by default,
#' writes it to a CSV.
#'
#' @param data Data frame of vetted detections, one row per recording.
#' @param headers.lower Logical, default \code{TRUE}. Lower-case every
#'   header (spaces and punctuation are kept, e.g. \code{"DATE-12"} ->
#'   \code{"date-12"}, \code{"IN FILE"} -> \code{"in file"}).
#' @param headers.rename Named character vector, default
#'   \code{c("site" = "aru.name", "manual id" = "species")}: old header =
#'   new header. Old headers are matched ignoring case, spaces and
#'   punctuation (\code{"MANUAL ID"}, \code{"Manual_ID"} and
#'   \code{"manual.id"} all match \code{"manual id"}). A rename is skipped,
#'   with a NOTE, when the new header already exists. \code{NULL} = no
#'   renaming.
#' @param aru.recode Optional data frame for recoding ARU names: column 1 =
#'   name in the file, column 2 = new name (read by position, like
#'   \code{\link{batz.datawrangler_rename}}). Matching ignores case and
#'   outer spaces. Default \code{NULL} = no recoding.
#' @param community.add Logical, default \code{TRUE}. Fill \code{$community}
#'   from \code{$aru.name} (see \code{community.pattern},
#'   \code{community.suffix.drop}, \code{community.recode}).
#' @param community.pattern Character, default \code{"^[A-Za-z]+"}. Regular
#'   expression for the part of \code{$aru.name} that is the community
#'   code - by default the letters before the first digit or other
#'   character (\code{"OLMN3"} -> \code{"OLMN"}, \code{"ES902"} ->
#'   \code{"ES"}).
#' @param community.suffix.drop Character vector, default \code{"BC"}.
#'   Endings removed from the community code, so base-camp ARUs join their
#'   community (\code{"KABC_06"} -> \code{"KABC"} -> \code{"KA"}). Use
#'   \code{character(0)} to keep them separate.
#' @param community.recode Optional data frame, column 1 = community code as
#'   derived, column 2 = community to use (by position). Applied after
#'   \code{community.suffix.drop}. Default \code{NULL}.
#' @param community.overwrite Logical, default \code{FALSE}. \code{FALSE}
#'   only fills blank \code{$community} values (or creates the column);
#'   \code{TRUE} replaces every value.
#' @param project Character, default \code{""}. Value written to
#'   \code{$project} for every row (e.g. \code{"South"}). \code{""} leaves
#'   \code{$project} as it is (or absent).
#' @param latitude.sign Character, default \code{"asis"}. \code{"south"}
#'   makes every latitude negative, \code{"north"} positive, \code{"asis"}
#'   leaves them. The column is found as \code{$latitude} or \code{$lat}.
#' @param longitude.sign Character, default \code{"asis"}. \code{"west"}
#'   makes every longitude negative, \code{"east"} positive. Found as
#'   \code{$longitude}, \code{$long} or \code{$lon}.
#' @param duplicates.remove Logical, default \code{FALSE}. \code{TRUE}
#'   drops exact duplicate rows (reported). Off by default so the tidied
#'   file keeps every record; \code{\link{batz.generate_plotframe.bat.group}}
#'   and \code{\link{batz.plot_heat.site.species}} remove duplicates
#'   themselves.
#' @param dir.save Directory the CSV is written to. Default \code{getwd()}.
#' @param write.output Logical, default \code{TRUE}. Write the result as
#'   \verb{<project.name>_vetted.format_<DATE1>to<DATE2>_<timestamp>.csv}
#'   (dates = first/last value of \code{$date-12}/\code{$date.monitoringnight}
#'   when one of them exists, otherwise \code{nodata}; timestamp =
#'   \code{YYYYMMDDHHMMSS}). The saved name is printed.
#' @param project.name Character, default \code{""}: start of the file
#'   name. \code{""} uses \code{project} if given, otherwise \code{"vetted"}.
#' @param snake_case Logical, default \code{FALSE}. \code{TRUE} converts
#'   every output header to snake_case with \code{standardize.headers()}
#'   (\code{"date-12"} -> \code{"date_12"}, \code{"aru.name"} ->
#'   \code{"aru_name"}).
#' @param strip.special Logical, default \code{TRUE}. Simplifies accented
#'   letters and removes other non-ASCII characters (headers and values),
#'   same as every other \code{batz} function.
#'
#' @return The tidied data frame. Columns are those of \code{data}, renamed
#'   as above, plus \code{$community} and \code{$project} when added
#'   (placed straight after \code{$species}, as in the hand-cleaned files;
#'   at the end if there is no \code{$species}). A one-row-per-step summary is attached as
#'   \code{attr(result, "format.log")} with columns \code{$step},
#'   \code{$rows.changed}, \code{$note}.
#'
#' @details
#' \strong{Added 2026-10-21, per Josh}, to turn the 2026-10-14 hand
#' clean-up of \code{Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv}
#' into a reusable function. The rules were taken from the cleaned files
#' themselves (\code{South_North_combined_vetted_1.csv}): every
#' \code{$community} is the letters at the start of \code{$aru.name}, with
#' the base-camp \code{"BC"} ending removed (\code{ILBC_08} -> \code{IL},
#' \code{KABC_06} -> \code{KA}, \code{MEBC_09} -> \code{ME},
#' \code{WGBC_07} -> \code{WG}); South-project latitudes are negative,
#' North-project latitudes positive. The original Basecamp ARU names
#' before recoding are not in those files, so the recode table is an input
#' (\code{aru.recode}) rather than built in.
#'
#' Steps run in this order: special characters, lower-case headers,
#' header renames, duplicate removal, ARU recode, community, project,
#' latitude/longitude sign, snake_case, write. A step that needs a column
#' the data doesn't have (e.g. \code{$aru.name} for the community step) is
#' skipped with a NOTE rather than stopping the run.
#'
#' @examples
#' \dontrun{
#' raw <- read.csv("Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv",
#'                 check.names = FALSE)
#' basecamp <- data.frame(old = c("Basecamp 6", "Basecamp 7"),
#'                        new = c("KABC_06", "KABC_07"))
#' south <- batz.datawrangler_format.vetted(raw, aru.recode = basecamp,
#'                                          project = "South",
#'                                          latitude.sign = "south")
#' }
#'
#' @export
batz.datawrangler_format.vetted <- function(data,
                                            headers.lower = TRUE,
                                            headers.rename = c("site" = "aru.name", "manual id" = "species"),
                                            aru.recode = NULL,
                                            community.add = TRUE,
                                            community.pattern = "^[A-Za-z]+",
                                            community.suffix.drop = "BC",
                                            community.recode = NULL,
                                            community.overwrite = FALSE,
                                            project = "",
                                            latitude.sign = "asis",
                                            longitude.sign = "asis",
                                            duplicates.remove = FALSE,
                                            dir.save = getwd(),
                                            write.output = TRUE,
                                            project.name = "",
                                            snake_case = FALSE,
                                            strip.special = TRUE) {

  FN <- "batz.datawrangler_format.vetted"
  if (!is.data.frame(data)) stop(FN, ": `data` must be a data frame.", call. = FALSE)
  latitude.sign <- match.arg(tolower(latitude.sign), c("asis", "south", "north"))
  longitude.sign <- match.arg(tolower(longitude.sign), c("asis", "west", "east"))
  if (!is.null(headers.rename) && (is.null(names(headers.rename)) || any(!nzchar(names(headers.rename))))) {
    stop(FN, ": `headers.rename` must be a named vector, old header = new header.", call. = FALSE)
  }

  log <- data.frame(step = character(0), rows.changed = integer(0), note = character(0),
                    stringsAsFactors = FALSE)
  add.log <- function(step, n, note) {
    log[nrow(log) + 1, ] <<- list(step, as.integer(n), note)
    cat(sprintf("%s: %s\n", step, note))
  }
  find.col <- function(df, candidates) {
    hit <- match(standardize.headers(candidates), standardize.headers(names(df)))
    hit <- hit[!is.na(hit)]
    if (length(hit) == 0) NA_integer_ else hit[1]
  }
  key <- function(v) tolower(trimws(as.character(v)))
  ## put a newly added column straight after column `after` (if present)
  place.after <- function(df, col, after) {
    a <- find.col(df, after)
    if (is.na(a)) return(df)
    others <- setdiff(names(df), col)
    df[, append(others, col, after = match(names(df)[a], others)), drop = FALSE]
  }

  ## ---- special characters ---------------------------------------------------
  s <- special.strip(data, strip.special)
  data <- s$df
  if (isTRUE(strip.special) && length(s$headers) > 0) {
    add.log("strip.special", NA, paste("special characters removed in:", paste(s$headers, collapse = ", ")))
  }

  ## ---- headers --------------------------------------------------------------
  if (isTRUE(headers.lower)) {
    n <- sum(names(data) != tolower(names(data)))
    names(data) <- tolower(names(data))
    add.log("headers.lower", n, sprintf("%d header(s) lower-cased", n))
  }
  if (!is.null(headers.rename) && length(headers.rename) > 0) {
    for (old in names(headers.rename)) {
      new <- headers.rename[[old]]
      i <- find.col(data, old)
      if (is.na(i)) next
      if (names(data)[i] == new) next
      if (new %in% names(data)) {
        add.log("headers.rename", 0, sprintf("NOTE '%s' not renamed - '%s' already exists", names(data)[i], new))
        next
      }
      add.log("headers.rename", 0, sprintf("'%s' -> '%s'", names(data)[i], new))
      names(data)[i] <- new
    }
  }
  dup.names <- unique(names(data)[duplicated(names(data))])
  if (length(dup.names) > 0) {
    stop(FN, ": duplicate header(s) after renaming: ", paste(dup.names, collapse = ", "), call. = FALSE)
  }

  ## ---- duplicates -----------------------------------------------------------
  if (isTRUE(duplicates.remove)) {
    dup <- duplicated(data)
    data <- data[!dup, , drop = FALSE]
    add.log("duplicates.remove", sum(dup), sprintf("%d exact duplicate row(s) removed", sum(dup)))
  }

  aru.col <- find.col(data, "aru.name")

  ## ---- ARU recode -------------------------------------------------------------
  if (!is.null(aru.recode)) {
    if (!is.data.frame(aru.recode) || ncol(aru.recode) < 2) {
      stop(FN, ": `aru.recode` must be a data frame with at least two columns (old, new).", call. = FALSE)
    }
    if (is.na(aru.col)) {
      add.log("aru.recode", 0, "NOTE skipped - no $aru.name column")
    } else {
      old <- as.character(data[[aru.col]])
      m <- match(key(old), key(aru.recode[[1]]))
      hit <- !is.na(m)
      data[[aru.col]][hit] <- trimws(as.character(aru.recode[[2]][m[hit]]))
      unused <- setdiff(key(aru.recode[[1]]), key(old))
      add.log("aru.recode", sum(hit), sprintf("%d record(s) recoded across %d ARU name(s)%s", sum(hit),
              length(unique(old[hit])),
              if (length(unused) > 0) paste0("; not found in data: ", paste(unused, collapse = ", ")) else ""))
    }
  }

  ## ---- community ---------------------------------------------------------------
  if (isTRUE(community.add)) {
    if (is.na(aru.col)) {
      add.log("community", 0, "NOTE skipped - no $aru.name column")
    } else {
      aru <- trimws(as.character(data[[aru.col]]))
      code <- regmatches(aru, regexpr(community.pattern, aru))
      comm <- rep(NA_character_, length(aru))
      comm[regexpr(community.pattern, aru) > 0 & !is.na(aru)] <- code
      for (suf in community.suffix.drop[nzchar(community.suffix.drop)]) {
        ends <- !is.na(comm) & nchar(comm) > nchar(suf) &
          toupper(substring(comm, nchar(comm) - nchar(suf) + 1)) == toupper(suf)
        comm[ends] <- substring(comm[ends], 1, nchar(comm[ends]) - nchar(suf))
      }
      if (!is.null(community.recode)) {
        if (!is.data.frame(community.recode) || ncol(community.recode) < 2) {
          stop(FN, ": `community.recode` must be a data frame with at least two columns.", call. = FALSE)
        }
        m <- match(key(comm), key(community.recode[[1]]))
        comm[!is.na(m)] <- trimws(as.character(community.recode[[2]][m[!is.na(m)]]))
      }
      com.col <- find.col(data, "community")
      if (is.na(com.col)) {
        data$community <- comm
        data <- place.after(data, "community", "species")
        n <- sum(!is.na(comm))
        add.log("community", n, sprintf("$community added: %s", paste(sort(unique(comm[!is.na(comm)])), collapse = ", ")))
      } else {
        cur <- as.character(data[[com.col]])
        fill <- if (isTRUE(community.overwrite)) rep(TRUE, length(cur)) else (is.na(cur) | !nzchar(trimws(cur)))
        fill <- fill & !is.na(comm)
        changed <- fill & (is.na(cur) | cur != comm)
        data[[com.col]][fill] <- comm[fill]
        add.log("community", sum(changed), sprintf("%d $community value(s) %s", sum(changed),
                if (isTRUE(community.overwrite)) "replaced" else "filled where blank"))
      }
      if (any(is.na(comm))) {
        add.log("community", 0, sprintf("NOTE %d record(s) have an $aru.name that doesn't match community.pattern", sum(is.na(comm))))
      }
    }
  }

  ## ---- project -----------------------------------------------------------------
  if (!is.null(project) && length(project) == 1 && !is.na(project) && nzchar(trimws(project))) {
    p.col <- find.col(data, "project")
    if (is.na(p.col)) {
      data$project <- trimws(project)
      data <- place.after(data, "project", c("community", "species"))
      add.log("project", nrow(data), sprintf("$project added = '%s'", trimws(project)))
    } else {
      changed <- sum(is.na(data[[p.col]]) | as.character(data[[p.col]]) != trimws(project))
      data[[p.col]] <- trimws(project)
      add.log("project", changed, sprintf("$project set to '%s' (%d value(s) changed)", trimws(project), changed))
    }
  }

  ## ---- coordinate signs ---------------------------------------------------------
  set.sign <- function(cands, sign.word, neg.word, label) {
    if (sign.word == "asis") return(invisible(NULL))
    i <- find.col(data, cands)
    if (is.na(i)) {
      add.log(label, 0, "NOTE skipped - no column found")
      return(invisible(NULL))
    }
    v <- suppressWarnings(as.numeric(as.character(data[[i]])))
    bad <- !is.na(data[[i]]) & nzchar(trimws(as.character(data[[i]]))) & is.na(v)
    want.neg <- sign.word == neg.word
    flip <- !is.na(v) & ((want.neg & v > 0) | (!want.neg & v < 0))
    v[flip] <- -v[flip]
    data[[i]] <<- v
    add.log(label, sum(flip), sprintf("%d value(s) of $%s made %s%s", sum(flip), names(data)[i],
            if (want.neg) "negative" else "positive",
            if (any(bad)) sprintf("; %d non-numeric value(s) set to NA", sum(bad)) else ""))
  }
  set.sign(c("latitude", "lat"), latitude.sign, "south", "latitude.sign")
  set.sign(c("longitude", "long", "lon"), longitude.sign, "west", "longitude.sign")

  ## ---- output ----------------------------------------------------------------------
  rownames(data) <- NULL
  if (isTRUE(snake_case)) names(data) <- standardize.headers(names(data))

  if (isTRUE(write.output)) {
    d.col <- find.col(data, c("date-12", "date.monitoringnight"))
    token <- "nodata"
    if (!is.na(d.col)) {
      x <- as.character(data[[d.col]])
      x <- x[!is.na(x) & nzchar(trimws(x))]
      for (fmt in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y", "%Y/%m/%d")) {
        p <- as.Date(x, format = fmt)
        if (length(p) > 0 && !any(is.na(p))) {
          token <- sprintf("%sto%s", format(min(p), "%Y%m%d"), format(max(p), "%Y%m%d"))
          break
        }
      }
    }
    base <- if (nzchar(project.name)) project.name else if (nzchar(trimws(project))) trimws(project) else "vetted"
    fname <- sprintf("%s_vetted.format_%s_%s.csv", base, token, format(Sys.time(), "%Y%m%d%H%M%S"))
    if (!dir.exists(dir.save)) dir.create(dir.save, recursive = TRUE)
    utils::write.csv(data, file.path(dir.save, fname), row.names = FALSE)
    cat("Saved:", fname, "\n")
  }

  attr(data, "format.log") <- log
  data
}

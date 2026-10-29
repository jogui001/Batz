#' Summarize bat-acoustic detections per community (conservancy) into a plotting-ready frame
#'
#' Builds one row per year x community x species (per project) giving the
#' total number of observations, how many sites (\code{$aru.name}) were
#' sampled in that community and how many of them detected the species, and
#' the matching site-night counts. A second run pools every year together
#' per project with \code{$year = "all"}. Species on \code{species.list}
#' that were never detected in a community get zeros, so every
#' community/year has a row for every species.
#'
#' \strong{Added 2026-10-14, per Josh.} Requested as
#' \code{BatZ.generate_plotframe.conservancyrate()} - normalized to the
#' package's lowercase \code{batz.} prefix; the rest already follows the
#' \code{batz.<family>_<action>.<subject>()} convention (same family/action
#' as \code{\link{batz.generate_plotframe.bat}}). Built and tested against
#' the South project's \code{Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv}.
#'
#' \strong{Renamed 2026-10-21, per Josh:}
#' \code{batz.generate_plotframe.conservancyrate()} ->
#' \code{batz.generate_plotframe.bat.group()}. Name only - inputs, logic
#' and output columns are unchanged, except that the saved CSV's name now
#' uses \code{bat.group} instead of \code{conservancyrate} (see
#' \code{write.output}). The internal helpers were renamed to match
#' (\code{bat.group.genus.family()}, \code{bat.group.lookup.family()}).
#'
#' \strong{Required input columns} (matched with \code{canonicalize.headers()},
#' so dot-, underscore-, space- or mixed-case spellings are all accepted -
#' e.g. \code{aru.name}/\code{aru_name}/\code{ARU Name}): \code{year},
#' \code{project}, \code{community}, \code{aru.name}, \code{species}, and
#' the monitoring-night date column named by \code{groupby.date} (default
#' \code{"date.monitoringnight"}). A raw Kaleidoscope-style export usually
#' has the monitoring night as \code{DATE-12} (the date of the recording
#' minus 12 hours); pass \code{groupby.date = "date-12"} for that, or rename
#' it first.
#'
#' \strong{Definitions} (each row = one \code{project}/\code{year}/\code{community}/\code{species}):
#' \itemize{
#'   \item \code{$observation_count} - number of records (rows) of that
#'     species. Each row of the input is treated as one observation (one
#'     recording file).
#'   \item \code{$sites_sampled} - number of unique \code{$aru.name} in the
#'     community, counting every record (including noise/NoID and species
#'     not on \code{species.list}).
#'   \item \code{$sites_detected} - number of unique \code{$aru.name} in the
#'     community where the species was recorded.
#'   \item \code{$date.monitoringnight_sampled} - for every \code{$aru.name}
#'     in the community, its number of unique monitoring nights, summed
#'     (i.e. the number of unique ARU-nights).
#'   \item \code{$date.monitoringnight_detected} - the same, counting only
#'     nights where that ARU recorded the species.
#' }
#'
#' \strong{Sampling effort comes from the records themselves.} A night when
#' an ARU was running but saved no recordings at all is not in the data, so
#' it is not counted in \code{$date.monitoringnight_sampled}. If deployment
#' metadata is available later, effort could be taken from that instead.
#'
#' \strong{\code{$year = "all"}.} The pooled run counts unique ARUs and
#' unique ARU-nights across all years of a project. An ARU name reused in
#' two years counts as one site in the pooled row. \code{$year} is
#' returned as text because of the \code{"all"} value.
#'
#' \strong{Species not on \code{species.list}.} Their records still count
#' toward sampling effort (\code{$sites_sampled},
#' \code{$date.monitoringnight_sampled}) but get no row of their own; a
#' \code{warning()} lists them with record counts, so a spelling mismatch
#' (e.g. \code{"Rhinilophus"} vs \code{"Rhinolophus"}) is easy to spot.
#' \code{species.list = NULL} (default) uses every species in the data.
#'
#' \strong{\code{$family}} (added 2026-10-14, per Josh). Each species is
#' matched, in this order, to: (1) a \code{species} row in
#' \code{family.ref}; (2) a \code{genus} row in \code{family.ref}; (3) a
#' built-in genus table of East African bat genera (first word of the
#' species name, case-insensitive - see
#' \code{bat.group.genus.family()}). Non-bat classes (\code{insect/noise},
#' \code{NoID}, \code{Unid_*}) are left blank. Any other species that
#' finds no family is left blank and named in a \code{warning()} - this
#' also catches misspelt genera (e.g. \code{"Rhinilophus"}); fix the data,
#' or pass \code{family.ref = data.frame(genus = "Rhinilophus", family =
#' "Rhinolophidae")}.
#'
#' \strong{Incomplete rows.} A record with a blank \code{year},
#' \code{project}, \code{community}, \code{aru.name} or monitoring night
#' cannot be placed and is dropped with a \code{warning()}. A record with a
#' blank \code{species} still counts toward sampling effort but is never a
#' species row.
#'
#' @param data Data frame of vetted detections, one row per recording (see
#'   Details for required columns).
#' @param species.list Character vector of every species to report. Every
#'   species gets a row for every project/year/community, zero-filled where
#'   not detected. Default \code{NULL} uses the unique species in
#'   \code{data}.
#' @param groupby.date Character, default \code{"date.monitoringnight"}.
#'   Name of the monitoring-night date column (same parameter name and
#'   default as \code{\link{batz.generate_plotframe.bat}}).
#' @param duplicates.remove Logical, default \code{TRUE}. Drop exact
#'   duplicate rows from \code{data} before counting (same parameter as
#'   \code{\link{batz.generate_plotframe.bat}}); the number dropped is
#'   reported with \code{message()}. On the model data this removed 290
#'   rows - MAI10's 2024 records had been entered twice.
#' @param dir.save Directory to write the output CSV into when
#'   \code{write.output = TRUE}. Default \code{getwd()}.
#' @param write.output Logical, default \code{TRUE}. Write the output CSV
#'   to \code{dir.save} as
#'   \verb{<project.name>_bat.group_<DATE1>to<DATE2>_<timestamp>.csv}
#'   (dates = first/last monitoring night, \code{YYYYMMDD}; timestamp =
#'   save time, \code{YYYYMMDDHHMMSS}). The saved name is printed.
#' @param project.name Character, default \code{""}: base of the output
#'   file name. \code{""} uses \code{"plotframe"}.
#' @param family.ref Optional data frame used to fill \code{$family}, with
#'   a \code{family} column and a \code{species} and/or \code{genus}
#'   column (headers matched with \code{canonicalize.headers()}). Default
#'   \code{NULL} uses only the built-in genus table. See
#'   \strong{\code{$family}} in Details.
#' @param snake_case Logical, default \code{FALSE}. \code{TRUE} converts
#'   the output column names with \code{standardize.headers()} (e.g.
#'   \code{$date_monitoringnight_sampled}).
#' @param strip.special Logical, default \code{TRUE}. Simplifies accented
#'   letters and removes other non-ASCII characters from \code{data} (this
#'   function's local copy only) before matching, same as every other
#'   \code{batz} function.
#'
#' @return A data frame with columns \code{$year}, \code{$community},
#'   \code{$species}, \code{$family}, \code{$observation_count}, \code{$sites_sampled},
#'   \code{$sites_detected}, \code{$date.monitoringnight_sampled},
#'   \code{$date.monitoringnight_detected}, \code{$project} (snake_case if
#'   \code{snake_case = TRUE}), sorted by project, year (\code{"all"} last),
#'   community and \code{species.list} order.
#'
#' @seealso \code{\link{batz.generate_plotframe.bat}}
#'
#' @examples
#' \dontrun{
#' vetted <- read.csv("Complete_Acoustic_Amboseli_Bat_Survey_SKvetted.csv",
#'                    check.names = FALSE)
#' spp <- c("Mops condylurus/pumilus", "Otomops harrisoni", "Coleura afra")
#' plfr.bat.group <- batz.generate_plotframe.bat.group(
#'   vetted, species.list = spp, groupby.date = "date-12",
#'   project.name = "south")
#' }
#'
#' @export
batz.generate_plotframe.bat.group <- function(data,
                                              species.list = NULL,
                                              groupby.date = "date.monitoringnight",
                                              duplicates.remove = TRUE,
                                              dir.save = getwd(),
                                              write.output = TRUE,
                                              project.name = "",
                                              family.ref = NULL,
                                              snake_case = FALSE,
                                              strip.special = TRUE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.null(species.list) && !is.character(species.list)) {
    stop("`species.list` must be a character vector (or NULL to use every species in `data`).")
  }

  ## ---- special characters + header check ---------------------------------
  data <- special.strip(data, strip.special)$df
  required.headers <- c("year", "project", "community", "aru.name", "species", groupby.date)
  data.canon <- canonicalize.headers(data, required.headers)
  if (length(data.canon$missing) > 0) {
    stop("`data` is missing these headers: ", paste(data.canon$missing, collapse = ", "))
  }
  data <- data.canon$df

  if (duplicates.remove) {
    dup <- duplicated(data)
    if (any(dup)) {
      message(sum(dup), " exact duplicate row(s) removed from `data`.")
      data <- data[!dup, , drop = FALSE]
    }
  }

  clean <- function(v) { v <- trimws(as.character(v)); v[is.na(v)] <- ""; v }
  d <- data.frame(year      = clean(data$year),
                  project   = clean(data$project),
                  community = clean(data$community),
                  aru.name  = clean(data$aru.name),
                  species   = clean(data$species),
                  night     = clean(data[[groupby.date]]),
                  stringsAsFactors = FALSE)

  ## ---- drop records that can't be placed ---------------------------------
  incomplete <- !nzchar(d$year) | !nzchar(d$project) | !nzchar(d$community) |
    !nzchar(d$aru.name) | !nzchar(d$night)
  if (any(incomplete)) {
    warning(sum(incomplete), " record(s) dropped: blank year, project, community, ",
            "aru.name or ", groupby.date, ".")
    d <- d[!incomplete, , drop = FALSE]
  }
  if (nrow(d) == 0) stop("No usable records left in `data`.")

  ## ---- species list -------------------------------------------------------
  if (is.null(species.list)) {
    species.list <- sort(unique(d$species[nzchar(d$species)]))
  } else {
    species.list <- unique(trimws(species.list))
    species.list <- species.list[!is.na(species.list) & nzchar(species.list)]
    if (length(species.list) == 0) stop("`species.list` is empty.")
  }
  off.list <- d$species[nzchar(d$species) & !(d$species %in% species.list)]
  if (length(off.list) > 0) {
    tab <- sort(table(off.list), decreasing = TRUE)
    warning("Species in `data` but not on `species.list` (counted toward sampling effort only): ",
            paste0(names(tab), " (", as.integer(tab), ")", collapse = "; "))
  }

  ## ---- add the pooled all-years run ---------------------------------------
  d <- rbind(d, transform(d, year = "all"))
  sep <- "\r"
  d$grp      <- paste(d$project, d$year, d$community, sep = sep)
  d$aru.key  <- paste(d$grp, d$aru.name, sep = sep)
  d$night.key <- paste(d$aru.key, d$night, sep = sep)

  ## ---- sampling effort per project/year/community -------------------------
  effort <- data.frame(grp = sort(unique(d$grp)), stringsAsFactors = FALSE)
  effort$sites_sampled <- as.integer(tapply(d$aru.key, d$grp, function(x) length(unique(x)))[effort$grp])
  effort$date.monitoringnight_sampled <-
    as.integer(tapply(d$night.key, d$grp, function(x) length(unique(x)))[effort$grp])

  ## ---- detections per project/year/community/species ----------------------
  det <- d[d$species %in% species.list, , drop = FALSE]
  det$key <- paste(det$grp, det$species, sep = sep)
  if (nrow(det) > 0) {
    det.sum <- data.frame(key = sort(unique(det$key)), stringsAsFactors = FALSE)
    det.sum$observation_count <- as.integer(table(det$key)[det.sum$key])
    det.sum$sites_detected <- as.integer(tapply(det$aru.key, det$key, function(x) length(unique(x)))[det.sum$key])
    det.sum$date.monitoringnight_detected <-
      as.integer(tapply(det$night.key, det$key, function(x) length(unique(x)))[det.sum$key])
  } else {
    det.sum <- data.frame(key = character(0), observation_count = integer(0),
                          sites_detected = integer(0), date.monitoringnight_detected = integer(0))
  }

  ## ---- full grid: every group x every listed species, zero-filled ---------
  grid <- expand.grid(species = species.list, grp = effort$grp, stringsAsFactors = FALSE)
  grid$key <- paste(grid$grp, grid$species, sep = sep)
  m <- match(grid$key, det.sum$key)
  fill0 <- function(v) { v[is.na(v)] <- 0L; v }
  parts <- strsplit(grid$grp, sep, fixed = TRUE)
  e <- match(grid$grp, effort$grp)

  out <- data.frame(
    year                          = vapply(parts, `[`, "", 2),
    community                     = vapply(parts, `[`, "", 3),
    species                       = grid$species,
    family                        = NA_character_,
    observation_count             = fill0(det.sum$observation_count[m]),
    sites_sampled                 = effort$sites_sampled[e],
    sites_detected                = fill0(det.sum$sites_detected[m]),
    date.monitoringnight_sampled  = effort$date.monitoringnight_sampled[e],
    date.monitoringnight_detected = fill0(det.sum$date.monitoringnight_detected[m]),
    project                       = vapply(parts, `[`, "", 1),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  ## ---- family -----------------------------------------------------------------
  fam <- bat.group.lookup.family(species.list, family.ref)
  out$family <- fam[match(out$species, species.list)]

  ## ---- sort: project, year (numeric, "all" last), community, species order
  yr.num <- suppressWarnings(as.numeric(out$year))
  out <- out[order(out$project, is.na(yr.num), yr.num, out$year, out$community,
                   match(out$species, species.list)), , drop = FALSE]
  rownames(out) <- NULL

  if (snake_case) names(out) <- standardize.headers(names(out))

  ## ---- write --------------------------------------------------------------
  if (write.output) {
    nights <- unique(d$night)
    parsed <- NULL
    for (fmt in c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y", "%Y/%m/%d")) {
      p <- as.Date(nights, format = fmt)
      if (!any(is.na(p))) { parsed <- p; break }
    }
    daterange.token <- if (!is.null(parsed)) {
      sprintf("%sto%s", format(min(parsed), "%Y%m%d"), format(max(parsed), "%Y%m%d"))
    } else "nodata"
    base.name <- if (identical(project.name, "")) "plotframe" else project.name
    file.name <- sprintf("%s_bat.group_%s_%s.csv", base.name, daterange.token,
                         format(Sys.time(), "%Y%m%d%H%M%S"))
    utils::write.csv(out, file.path(dir.save, file.name), row.names = FALSE)
    cat("Saved:", file.name, "\n")
  }

  out
}


#' Built-in genus -> family table for batz.generate_plotframe.bat.group() (internal)
#'
#' Added 2026-10-14 (as conservancyrate.genus.family(); renamed 2026-10-21).
#' East African bat genera (Kenya and neighbours), current family
#' placements. Genus-level so it is not affected by species-name spelling
#' or species-group labels like \code{"Mops condylurus/pumilus"}. Checked
#' with \code{stopifnot()} so the two columns can never drift apart.
#' @keywords internal
#' @noRd
bat.group.genus.family <- function() {
  fam <- list(
    Vespertilionidae = c("Afronycteris", "Cnephaeus", "Eptesicus", "Glauconycteris", "Hypsugo",
                         "Kerivoula", "Laephotis", "Mimetillus", "Myotis", "Neoromicia",
                         "Nycticeinops", "Pipistrellus", "Pseudoromicia", "Scotoecus",
                         "Scotophilus", "Scotozous"),
    Molossidae       = c("Chaerephon", "Mops", "Mormopterus", "Otomops", "Platymops", "Tadarida"),
    Rhinolophidae    = c("Rhinolophus"),
    Hipposideridae   = c("Asellia", "Doryrhina", "Hipposideros", "Macronycteris"),
    Rhinonycteridae  = c("Cloeotis", "Paratriaenops", "Triaenops"),
    Emballonuridae   = c("Coleura", "Saccolaimus", "Taphozous"),
    Megadermatidae   = c("Cardioderma", "Lavia"),
    Nycteridae       = c("Nycteris"),
    Miniopteridae    = c("Miniopterus"),
    Rhinopomatidae   = c("Rhinopoma"),
    Pteropodidae     = c("Eidolon", "Epomophorus", "Epomops", "Lissonycteris", "Micropteropus",
                         "Myonycteris", "Rousettus")
  )
  tab <- data.frame(genus = unlist(fam, use.names = FALSE),
                    family = rep(names(fam), lengths(fam)), stringsAsFactors = FALSE)
  stopifnot(nrow(tab) == sum(lengths(fam)), !anyDuplicated(tolower(tab$genus)))
  tab
}

#' @keywords internal
#' @noRd
bat.group.lookup.family <- function(species, family.ref = NULL) {
  key <- function(x) tolower(trimws(as.character(x)))
  genus <- key(sub("\\s.*$", "", trimws(species)))
  out <- rep(NA_character_, length(species))

  if (!is.null(family.ref)) {
    if (!is.data.frame(family.ref)) stop("`family.ref` must be a data frame.")
    ref <- canonicalize.headers(family.ref, c("species", "genus", "family"))$df
    if (!"family" %in% names(ref) || !any(c("species", "genus") %in% names(ref))) {
      stop("`family.ref` needs a `family` column and a `species` and/or `genus` column.")
    }
    ok <- !is.na(ref$family) & nzchar(trimws(ref$family))
    if ("species" %in% names(ref)) {
      i <- match(key(species), key(ref$species[ok]))
      out[is.na(out)] <- trimws(ref$family[ok])[i][is.na(out)]
    }
    if ("genus" %in% names(ref)) {
      i <- match(genus, key(ref$genus[ok]))
      out[is.na(out)] <- trimws(ref$family[ok])[i][is.na(out)]
    }
  }
  tab <- bat.group.genus.family()
  i <- match(genus, key(tab$genus))
  out[is.na(out)] <- tab$family[i][is.na(out)]

  non.bat <- grepl("^(noid|unid|unk|insect|noise)", key(species))
  miss <- is.na(out) & !non.bat
  if (any(miss)) {
    warning("No $family found for: ", paste(species[miss], collapse = ", "),
            " - check the genus spelling or add it with `family.ref`.")
  }
  out[is.na(out)] <- ""
  out
}

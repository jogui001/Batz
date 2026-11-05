#' List bat species in given US states, or the states where given bat species occur
#'
#' Takes a vector or data frame holding any mix of bat species identifiers
#' (latin name, common name, 4-letter or 6-letter code) and US
#' state/territory identifiers (official name, short name, 2-letter code),
#' and returns a table crossing the requested species with the requested
#' states, either as one row per state-species pair (\code{table.type =
#' "state"}) or as a species x state grid (\code{table.type = "matrix"}).
#'
#' @details
#' \strong{Reference tables.} Species come from the shared bat reference
#' table used by \code{\link{batz.batusa_recode.names}} (species rows only;
#' category labels such as \code{"HiF"} are skipped). The 56 US
#' states/territories (50 states, DC, PR, VI, GU, AS, MP) are embedded in
#' the function. No files are read.
#'
#' \strong{Matching.}
#' \itemize{
#'   \item With \code{strip.special = TRUE} and a data frame \code{data},
#'     accented letters are simplified (e.g. e-acute to \code{e}) and other
#'     non-ASCII symbols (e.g. degree or micro signs) are removed first. A plain
#'     vector is not stripped.
#'   \item A data frame is flattened to one vector; its column names are not
#'     used.
#'   \item Every value is compared with both tables (bat: latin, common,
#'     code4, code6; state: official name, short name, code2), ignoring case,
#'     extra white space, and treating \code{-} and \code{_} as spaces.
#'   \item A value matching both tables is treated as a species (a WARNING is
#'     printed). Values matching neither are listed in a WARNING (first 25
#'     shown). If nothing matches, the function stops.
#'   \item If any species matched, only those species are used, otherwise
#'     all species. Likewise for states. Order follows first appearance in
#'     \code{data}.
#' }
#'
#' \strong{Cell values} (state status, per species and state):
#' \itemize{
#'   \item \code{"SC"}: the state is in the species' state species-of-concern
#'     list.
#'   \item \code{"L"}: the state is in the species' \code{states.listed}
#'     list (listed, severity not given in the reference table).
#'   \item \code{symbol_presence.absence[1]}: present, not listed (blank in
#'     \code{table.type = "state"}, where a row already means present).
#'   \item \code{symbol_presence.absence[2]}: absent (\code{"matrix"} only;
#'     \code{"state"} has no row).
#'   \item \code{"#"} is appended for extirpated states only if the
#'     reference table has a \code{states.extirpated} column. It does not
#'     yet, so with \code{species.extirpated = TRUE} a NOTE is printed and no
#'     marks are added.
#' }
#'
#' \strong{Federal status} (\code{$listing.status_federal}) is abbreviated:
#' \code{"Not Listed"} to blank, \code{"Under Review"} to \code{"UR"},
#' \code{"Endangered"} to \code{"E"}, \code{"Proposed Endangered"} to
#' \code{"PE"}; any other value is kept as written.
#'
#' @param data A vector or data frame of bat species and/or US
#'   state/territory identifiers, in any mix.
#' @param grammar.dash Logical, default \code{TRUE}. Output labels only:
#'   \code{TRUE} keeps hyphens as in the reference tables, \code{FALSE}
#'   replaces them with spaces. Hyphens are always ignored when matching.
#' @param statename.format.out Character, default \code{"code2"}. State label
#'   in the output: \code{"code2"}, \code{"official.name"} or
#'   \code{"short.name"}.
#' @param batname.format.out Character, default \code{"full"}. Species label
#'   in \code{$common.name}: \code{"full"} = \code{"<common> (<latin>)"}
#'   followed directly (no separator) by the hibernation strategy;
#'   \code{"both"} = \code{"<common> (<latin>)"}; \code{"common"}; or
#'   \code{"latin"}.
#' @param bat.code Character, default \code{"code4"}. Code shown in
#'   \code{$species.code}: \code{"code4"}, \code{"code6"}, or \code{"none"}
#'   (column left out).
#' @param table.type Character, default \code{"state"}. \code{"state"} or
#'   \code{"matrix"}; see Value.
#' @param symbol_presence.absence Character vector of length 2, default
#'   \code{c("*", "-")}: symbol for present-but-not-listed and for absent.
#'   See Details.
#' @param phonic.group Logical, default \code{TRUE}. Adds a
#'   \code{$phonic.group} column.
#' @param species.extirpated Logical, default \code{TRUE}. Appends \code{"#"}
#'   for extirpated states when the reference table has that information
#'   (it currently does not; see Details).
#' @param strip.special Logical, default \code{TRUE}. Simplify or remove
#'   special characters in \code{data} when it is a data frame.
#'
#' @return A data frame (printed WARNING/NOTE lines go to the console;
#'   nothing is written to disk).
#' \describe{
#'   \item{\code{table.type = "state"}}{One row per state-species pair where
#'     the species is present. Columns: \code{state}, \code{common.name},
#'     \code{species.code} (unless \code{bat.code = "none"}),
#'     \code{listing.status_federal}, \code{state} (a second column with the
#'     same name, holding the state status), \code{phonic.group} (if
#'     \code{phonic.group = TRUE}). Stops if no requested species occurs in
#'     any requested state.}
#'   \item{\code{table.type = "matrix"}}{One row per species. Columns:
#'     \code{common.name}, \code{species.code} (unless \code{bat.code =
#'     "none"}), \code{phonic.group} (if \code{TRUE}),
#'     \code{listing.status_federal}, then one column per state, named per
#'     \code{statename.format.out}, holding the state status.}
#' }
#'
#' @examples
#' \dontrun{
#' ## states where big brown bat and little brown bat occur
#' batz.batusa_list.species(c("epfu", "mylu"))
#'
#' ## same species as a species x state grid
#' batz.batusa_list.species(c("epfu", "mylu"), table.type = "matrix")
#'
#' ## species found in Ohio and Vermont
#' batz.batusa_list.species(c("OH", "VT"))
#'
#' ## given species x given states, full state names, 6-letter codes
#' batz.batusa_list.species(c("epfu", "mylu", "OH", "VT"), table.type = "matrix",
#'   statename.format.out = "short.name", bat.code = "code6")
#' }
#'
#' @seealso \code{\link{batz.batusa_recode.names}}
#'
#' @export
batz.batusa_list.species <- function(data,
                                      grammar.dash = TRUE,
                                      statename.format.out = "code2",
                                      batname.format.out = "full",
                                      bat.code = "code4",
                                      table.type = "state",
                                      symbol_presence.absence = c("*", "-"),
                                      phonic.group = TRUE,
                                      species.extirpated = TRUE,
                                      strip.special = TRUE) {

  ## special characters (per Josh, 2026-09-30): strip non-ASCII characters
  ## from `data` when it is a data frame (this function's local copy only;
  ## a plain vector passes through unchanged) - see @details, "Follow-up,
  ## 2026-09-30". The embedded reference tables below are never stripped.
  data <- special.strip(data, strip.special)$df

  # ---------------------------------------------------------------------------
  # Reference database 1: bat species (Josh's NAbat.names.csv, embedded as
  # currently supplied - 54 species x 15 columns). See @details above for
  # how to update this.
  #
  # Header standardization (per Josh, 2026-09-14 project preference): NOT
  # applied to this table's own column names, or to `states`' column names
  # below - both are this function's own fixed, already-established
  # output-schema names, not raw headers freshly copied from a file for
  # this rollout. Nor is it applied to `data`: every element is matched as
  # a VALUE (a species or state/territory identifier), never as a header,
  # and `data`'s own column names (when it's a data frame) aren't even
  # carried through to the output - they're discarded by the flattening
  # step below. See @details "Header standardization" above.
  # ---------------------------------------------------------------------------
  # 2026-10-23, per Josh: the bat table is no longer embedded here - it is
  # the shared bat.names table (bat.ref.names(), R/batz.util_bat.reference.R)
  # also used by batz.batusa_recode.names(). Only species rows are used (the
  # category labels such as "HiF" have no $family), and the columns are
  # given this function's older internal names so the code below is
  # unchanged.
  nabat <- bat.ref.names()
  nabat <- nabat[nzchar(trimws(nabat$family)), , drop = FALSE]
  old.names <- c(scientific_name = "latin", common_name = "common",
                 listing.status_federal = "fedstatus", listing.status_iucn = "iucnstatus",
                 states.endangered = "states.end", states.threatened = "states.the",
                 state.sppofconcern = "state.soc", listing.status_federal.proposed = "fed.proposed",
                 hibernation.strategy = "hibernation.strat")
  hit <- names(nabat) %in% names(old.names)
  names(nabat)[hit] <- old.names[names(nabat)[hit]]
  rownames(nabat) <- NULL

  # ---------------------------------------------------------------------------
  # Reference database 2: US states/territories (confirmed to be Josh's
  # "USAstate.names.database" - see @details - embedded from
  # NAstates.names.csv, 56 rows x 8 columns).
  #
  # Header standardization (per Josh, 2026-09-14 project preference): NOT
  # applied here either, for the same reason as `nabat` above - this
  # table's column names are this function's own fixed, already-established
  # schema. See @details "Header standardization" above.
  # ---------------------------------------------------------------------------
  states <- structure(list(official.name = c("State of Alabama", "State of Alaska",
"State of Arizona", "State of Arkansas", "State of California",
"State of Colorado", "State of Connecticut", "State of Delaware",
"State of Florida", "State of Georgia", "State of Hawaii", "State of Idaho",
"State of Illinois", "State of Indiana", "State of Iowa", "State of Kansas",
"Commonwealth of Kentucky", "State of Louisiana", "State of Maine",
"State of Maryland", "Commonwealth of Massachusetts", "State of Michigan",
"State of Minnesota", "State of Mississippi", "State of Missouri",
"State of Montana", "State of Nebraska", "State of Nevada", "State of New Hampshire",
"State of New Jersey", "State of New Mexico", "State of New York",
"State of North Carolina", "State of North Dakota", "State of Ohio",
"State of Oklahoma", "State of Oregon", "Commonwealth of Pennsylvania",
"State of Rhode Island", "State of South Carolina", "State of South Dakota",
"State of Tennessee", "State of Texas", "State of Utah", "State of Vermont",
"Commonwealth of Virginia", "State of Washington", "State of West Virginia",
"State of Wisconsin", "State of Wyoming", "District of Columbia",
"Commonwealth of Puerto Rico", "Virgin Islands of the United States",
"Territory of Guam", "Territory of American Samoa", "Commonwealth of the Northern Mariana Islands"
), short.name = c("Alabama", "Alaska", "Arizona", "Arkansas",
"California", "Colorado", "Connecticut", "Delaware", "Florida",
"Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
"Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
"Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
"Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
"New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
"Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
"Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
"West Virginia", "Wisconsin", "Wyoming", "Washington, D.C.",
"Puerto Rico", "U.S. Virgin Islands", "Guam", "American Samoa",
"Northern Mariana Islands"), code2 = c("AL", "AK", "AZ", "AR",
"CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA",
"KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
"NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
"PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
"WI", "WY", "DC", "PR", "VI", "GU", "AS", "MP"), entity.type = c("State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"State", "State", "State", "State", "State", "State", "State",
"Federal District", "Territory", "Territory", "Territory", "Territory",
"Territory"), usfws.region = c("4", "7", "2", "4", "8", "6",
"5", "5", "4", "4", "1", "1", "3", "3", "3", "6", "4", "4", "5",
"5", "5", "3", "3", "4", "3", "6", "6", "8", "5", "5", "2", "5",
"4", "6", "3", "2", "1", "5", "5", "4", "6", "4", "2", "6", "5",
"5", "1", "5", "3", "6", "5", "4", "4", "1", "1", "1"), time.zone = c("6",
"9", "7", "6", "8", "7", "5", "5", "5", "5", "10", "7", "6",
"5", "6", "6", "5", "6", "5", "5", "5", "5", "6", "6", "6", "7",
"6", "8", "5", "5", "7", "5", "5", "6", "5", "6", "8", "5", "5",
"5", "6", "6", "6", "7", "5", "5", "8", "5", "6", "7", "5", "4",
"4", "-10", "11", "-10"), dst.observed = c("TRUE", "TRUE", "FALSE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "FALSE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE",
"FALSE", "FALSE", "FALSE", "FALSE", "FALSE"), notes = c("", "",
"Does not observe DST (Mountain Standard Time year-round; the Navajo Nation portion of AZ does observe DST, not broken out here)",
"", "", "", "", "", "Panhandle (west of Apalachicola River, e.g. Pensacola) is Central; majority Eastern used here",
"", "Does not observe DST", "Northern panhandle (~10 counties, e.g. Coeur d'Alene, Lewiston) is Pacific; majority Mountain used here",
"", "NW corner (Chicago area) and SW corner (Evansville area) are Central; majority Eastern used here",
"", "Far western counties are Mountain; majority Central used here",
"", "", "", "", "", "Western Upper Peninsula (a few counties) is Central; majority Eastern used here",
"", "", "", "", "Panhandle (Scottsbluff area) is Mountain; majority Central used here",
"Nevada is Pacific by state law statewide; a few easternmost points are geographically nearer Mountain but not officially observed",
"", "", "", "", "", "", "West is Mountain (~13 westernmost counties); majority Central used here",
"", "Malheur County (far east) is Mountain; majority Pacific used here",
"", "", "", "West is Mountain (~10 westernmost counties); majority Central used here",
"East third (Knoxville/Chattanooga area) is Eastern; majority Central used here",
"El Paso area (far west) is Mountain; majority Central used here",
"", "", "", "", "", "", "", "", "Does not observe DST (Atlantic Standard Time year-round)",
"Does not observe DST (Atlantic Standard Time year-round)", "Does not observe DST (Chamorro Standard Time, UTC+10 year-round)",
"Does not observe DST (Samoa Standard Time, UTC-11 year-round)",
"Does not observe DST (Chamorro Standard Time, UTC+10 year-round)"
)), row.names = c(NA, -56L), class = "data.frame")

  nabat[]  <- lapply(nabat,  function(col) trimws(as.character(col)))
  states[] <- lapply(states, function(col) trimws(as.character(col)))

  normalize <- function(x) {
    x <- as.character(x)
    x <- gsub("[-_]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }

  split.codes <- function(cell) {
    cell <- trimws(as.character(cell))
    if (is.na(cell) || cell == "") return(character(0))
    trimws(strsplit(cell, ",")[[1]])
  }

  federal.abbrev <- function(fedstatus) {
    switch(fedstatus,
      "Not Listed"          = "",
      "Under Review"        = "UR",
      "Endangered"          = "E",
      "Proposed Endangered" = "PE",
      fedstatus)
  }

  status.value <- function(bat.row, state.code2, symbol_presence.absence) {
    present <- state.code2 %in% split.codes(bat.row[["states.present"]])
    soc     <- state.code2 %in% split.codes(bat.row[["state.soc"]])
    listed  <- state.code2 %in% split.codes(bat.row[["states.listed"]])
    if (!present) return(symbol_presence.absence[2])
    if (soc) return("SC")
    if (listed) return("L")
    symbol_presence.absence[1]
  }

  bat.cols   <- c("latin", "common", "code4", "code6")
  state.cols <- c("official.name", "short.name", "code2")

  x <- if (is.data.frame(data)) unlist(lapply(data, as.character), use.names = FALSE) else as.character(data)
  x.norm <- normalize(x)

  bat.lookup   <- unlist(lapply(bat.cols,   function(cn) normalize(nabat[[cn]])),  use.names = FALSE)
  bat.rowidx   <- rep(seq_len(nrow(nabat)),  times = length(bat.cols))
  state.lookup <- unlist(lapply(state.cols, function(cn) normalize(states[[cn]])), use.names = FALSE)
  state.rowidx <- rep(seq_len(nrow(states)), times = length(state.cols))

  bat.match   <- bat.rowidx[match(x.norm, bat.lookup)]
  state.match <- state.rowidx[match(x.norm, state.lookup)]

  both <- !is.na(bat.match) & !is.na(state.match)
  if (any(both)) {
    cat(sprintf("WARNING: %d input(s) matched BOTH the bat and state reference tables (%s) - treated as bat matches (documented tie-break, not expected on real data).\n",
                sum(both), paste(unique(x[both]), collapse = ", ")))
  }

  unmatched <- x[is.na(bat.match) & is.na(state.match)]

  # preserve FIRST-SEEN order (not sorted) - matters for matrix column order
  bat.seen   <- bat.match[!is.na(bat.match)]
  state.seen <- state.match[!is.na(state.match)]
  bat.rows   <- bat.seen[!duplicated(bat.seen)]
  state.rows <- state.seen[!duplicated(state.seen)]

  if (length(bat.rows) == 0 && length(state.rows) == 0) {
    stop("No recognizable bat species or US state/territory identifiers were found anywhere in `data`.")
  }

  species.idx <- if (length(bat.rows) > 0) bat.rows else seq_len(nrow(nabat))
  state.idx   <- if (length(state.rows) > 0) state.rows else seq_len(nrow(states))

  if (length(unmatched) > 0) {
    shown <- head(unique(unmatched), 25)
    omitted.note <- if (length(unique(unmatched)) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(unique(unmatched)))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match either reference table: %s%s\n",
                 length(unmatched), paste(shown, collapse = ", "), omitted.note))
  }

  has.extirpated.col <- "states.extirpated" %in% names(nabat)
  if (species.extirpated && !has.extirpated.col) {
    cat("NOTE: species.extirpated = TRUE was requested, but the reference database has no 'states.extirpated' column yet - no '#' extirpation markers were applied.\n")
  }
  is.extirpated <- function(bat.row, state.code2) {
    if (!has.extirpated.col) return(FALSE)
    state.code2 %in% split.codes(bat.row[["states.extirpated"]])
  }

  if (!(table.type %in% c("state", "matrix"))) {
    stop(sprintf("table.type must be one of \"state\", \"matrix\" (got '%s')", table.type))
  }

  format.bat.label <- function(row) {
    name <- switch(batname.format.out,
      "full"   = sprintf("%s (%s)%s", row[["common"]], row[["latin"]], row[["hibernation.strat"]]),
      "both"   = sprintf("%s (%s)", row[["common"]], row[["latin"]]),
      "common" = row[["common"]],
      "latin"  = row[["latin"]],
      stop(sprintf("batname.format.out must be one of \"full\", \"both\", \"common\", \"latin\" (got '%s')", batname.format.out))
    )
    if (!grammar.dash) name <- gsub("-", " ", name)
    name
  }

  format.state.label <- function(row) {
    label <- switch(statename.format.out,
      "code2"         = row[["code2"]],
      "official.name" = row[["official.name"]],
      "short.name"    = row[["short.name"]],
      stop(sprintf("statename.format.out must be one of \"code2\", \"official.name\", \"short.name\" (got '%s')", statename.format.out))
    )
    if (!grammar.dash) label <- gsub("-", " ", label)
    label
  }

  species.labels <- vapply(species.idx, function(i) format.bat.label(nabat[i, ]), character(1))
  state.labels   <- vapply(state.idx,   function(j) format.state.label(states[j, ]), character(1))
  code.col       <- if (bat.code %in% c("code4", "code6")) bat.code else if (bat.code == "none") NULL else
                       stop(sprintf("bat.code must be one of \"code4\", \"code6\", \"none\" (got '%s')", bat.code))
  federal.codes  <- vapply(nabat[species.idx, "fedstatus"], federal.abbrev, character(1))

  if (table.type == "matrix") {

    out <- data.frame("common.name" = species.labels, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(code.col)) out[["species.code"]] <- nabat[species.idx, code.col]
    if (phonic.group) out[["phonic.group"]] <- nabat[species.idx, "phonic.group"]
    out[["listing.status_federal"]] <- federal.codes

    for (k in seq_along(state.idx)) {
      j <- state.idx[k]
      state.code2 <- states[j, "code2"]
      col <- vapply(species.idx, function(i) {
        val <- status.value(nabat[i, ], state.code2, symbol_presence.absence)
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        val
      }, character(1))
      out[[state.labels[k]]] <- col
    }

    out

  } else {
    # table.type == "state": one row per (state, species) PRESENT pair

    rows <- list()
    for (k in seq_along(state.idx)) {
      j <- state.idx[k]
      state.code2 <- states[j, "code2"]
      for (m in seq_along(species.idx)) {
        i <- species.idx[m]
        present <- state.code2 %in% split.codes(nabat[i, "states.present"])
        if (!present) next
        val <- status.value(nabat[i, ], state.code2, symbol_presence.absence)
        if (val == symbol_presence.absence[1]) val <- ""   # a row existing already implies presence
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        # Built POSITIONALLY, not as a named list: the model reuses "State"
        # as the header for BOTH the row's state and the listing-status
        # column, and a second `row[["State"]] <- val` would silently
        # overwrite the first entry instead of adding a column. Names are
        # applied once, after rbind, below.
        row <- list(state.labels[k], species.labels[m])
        if (!is.null(code.col)) row <- c(row, list(nabat[i, code.col]))
        row <- c(row, list(federal.codes[m]), list(val))
        if (phonic.group) row <- c(row, list(nabat[i, "phonic.group"]))
        rows[[length(rows) + 1]] <- row
      }
    }

    if (length(rows) == 0) {
      stop("No requested species are present in any of the requested states - nothing to build a 'state' table.type table from.")
    }

    out <- do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))
    col.names <- c("state", "common.name")
    if (!is.null(code.col)) col.names <- c(col.names, "species.code")
    col.names <- c(col.names, "listing.status_federal", "state")
    if (phonic.group) col.names <- c(col.names, "phonic.group")
    names(out) <- col.names   # set directly so the duplicate "state" header survives literally
    out
  }
}

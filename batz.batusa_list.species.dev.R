# =============================================================================
# batz.batusa_list.species.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.batusa_list.species() - tested here before being wrapped
# into the final function (batz.batusa_list.species.R).
#
# REVISION HISTORY:
#   - First built 2026-08-25 from the prose spec alone, since "batlist
#     model1.csv"/"batlist model2.csv" (the real output-format templates)
#     were unreachable that session (device folder-access dialog timed
#     out). That version guessed a "state" layout of one row PER STATE with
#     an aggregated comma-list of species, and a "matrix" layout with blank
#     cells for "present and listed".
#   - REBUILT the same day once Josh granted access to the
#     "4 Current  test data" folder and the real model1/model2 CSVs (plus
#     the real NAbat.namestest.csv, and copies of NAbat.names.csv /
#     USAstates.names.csv in a "reference database files" subfolder) became
#     reachable. The real models revealed the actual layout is meaningfully
#     different from the first guess - see below. **This version replaces
#     the first guess entirely; nothing from the first attempt's table
#     layout survives.**
#
# NAME NORMALIZATION (unchanged from the first build - see project
# preferences.md): "batz.batusa_species.list()" -> "batz.batusa_list.species()"
# (action.subject order); "gramma.dash"/"grammar-dash" -> "grammar.dash".
#
# WHAT THE REAL MODEL FILES SHOWED (this is the important correction over
# the first build):
#   - Confirmed: "USAstate.names.database" IS the same file as this
#     project's own "NAstates.names.csv" - Josh's own
#     "reference database files/USAstates.names.csv" and
#     "reference database files/NAbat.names.csv" are byte-identical (after
#     whitespace-trim) to this project's current `NAstates.names.csv` /
#     `NAbat.names.csv` - he'd saved copies of our own earlier deliverables
#     into that folder. No reference-data changes needed.
#   - `table.type = "state"` (batlist model1.csv) is NOT one row per state
#     with an aggregated species list (the first build's guess) - it's ONE
#     ROW PER (STATE, SPECIES) PRESENT PAIR. A species absent from a given
#     state simply gets no row for that state at all (confirmed: the real
#     model1.csv has no Indiana bat / MYSO row for ME, because MYSO isn't
#     present there). Columns, in this exact order:
#       "State"        - the state (statename.format)
#       "Bat Species"  - see batname.format below
#       "Species Code" - see bat.code below
#       "Federal"      - federal listing status, ABBREVIATED (see below)
#       "State"        - state-specific listing status, abbreviated (see
#                        below) - Josh's own model file reuses "State" as
#                        the header for BOTH the row's state (column 1) and
#                        this listing-status column (column 5) - replicated
#                        literally (R allows two identically-named columns
#                        when set via `names(df) <-`, just not when
#                        constructed via `data.frame(State = ..., State =
#                        ...)`, which would auto-rename the second).
#       "Phonic Group" - see phonic.group below (present as its own column
#                        here too - the first build's guess of folding it
#                        into the species-name token was wrong).
#   - `table.type = "matrix"` (batlist model2.csv) IS one row per species,
#     one column per state - this part of the first build's guess was
#     right. Columns, in this exact order: "Bat Species", "Species Code",
#     "Phonic Group", "Federal", then one column per state (named per
#     statename.format, in the order states were matched/given).
#   - `batname.format = "full"` (default) is NOT just the common name (the
#     first build's guess) - the real "Bat Species" field is
#     "<common name> (<latin name>)<hibernation.strat>", all concatenated
#     into ONE string with NO separator before hibernation.strat (confirmed
#     by exact comma-field-counting in the raw CSV - this is a real,
#     consistent feature of the model, not a stray typo). Replicated
#     literally, including the no-space join, even though it reads a
#     little cramped (e.g. "Big brown bat (Eptesicus fuscus)hibernating").
#     `batname.format = "both"` (per Josh's own Steps-section definition,
#     still missing from his Optional Inputs enumeration - flagged in the
#     first build, unchanged) is just "<common> (<latin>)", with NO
#     hibernation.strat suffix - "full" and "both" are genuinely different
#     now, not synonyms as the first build guessed.
#   - `bat.code`: "Species Code" is its OWN column (code4/code6), not
#     appended in parentheses onto the species label as the first build
#     did. `bat.code = "none"` omits the column entirely.
#   - "Federal" is always included (not gated by any optional input) -
#     abbreviated from `$fedstatus`: "Not Listed" -> "" (blank), "Under
#     Review" -> "UR", "Endangered" -> "E", "Proposed Endangered" -> "PE".
#     This mapping was reverse-engineered from the model's own values for
#     species whose real fedstatus is known (e.g. Myotis sodalis/
#     septentrionalis = "E" for real Endangered species, Myotis lucifugus =
#     "UR" for the real Under Review status, Perimyotis subflavus = "PE"
#     for Proposed Endangered in the MA block - the ME block shows "P" for
#     the same species/status, and "R" instead of "UR" for M. lucifugus -
#     both read as typos in Josh's hand-built mockup, not a different
#     intended scheme, since MA and PA agree with each other and only the
#     ME block (added third/last, most abbreviated overall) disagrees).
#   - "State" (listing-status column, table.type = "state") / each state's
#     column value (table.type = "matrix"): the model clearly wants a
#     THREE-WAY per-(species,state) value - present-not-listed, absent, or
#     the specific listing severity when listed (e.g. "E", "T", "SC") -
#     richer than the two-symbol `presence.absence` description in Josh's
#     own Optional Inputs ("the two symbols used to mark... present but not
#     listed vs absent"). **This is the one place the current reference
#     database genuinely lacks the needed detail**: `NAbat.names.csv` only
#     has a flat $states.listed (listed or not, no severity level) and
#     $states.soc (species-of-concern states) - no per-state Endangered-
#     vs-Threatened-vs-Special-Concern breakdown. Implemented as the best
#     available proxy from what the two real columns DO carry:
#       - state in $state.soc                       -> "SC"
#       - state in $states.listed (and not $state.soc) -> "L" (generic
#         "Listed" - NOT a real abbreviation from Josh's model; a stand-in
#         since Endangered/Threatened/etc. per state isn't in the database
#         yet)
#       - present, in neither                        -> presence.absence[1]
#         ("*" default) for matrix; blank for the "state" table.type (a
#         species only gets a row there if it's present, so blank vs. a
#         severity code is enough - no "*" needed when presence is already
#         implied by the row existing)
#       - absent                                      -> presence.absence[2]
#         ("-" default) for matrix; no row at all for "state" table.type
#     **Flagging prominently: if Josh has (or wants to build) real
#     per-state Endangered/Threatened/Special-Concern data, the "L"
#     placeholder above should be replaced with it - this is the single
#     biggest remaining gap versus the real model files.**
#   - `phonic.group = TRUE` (default) is a plain standalone column in BOTH
#     table.type layouts (the first build's guess of folding it into the
#     species-name token for "state" type was unnecessary once the "state"
#     layout turned out to be one-row-per-species-per-state, same grain as
#     matrix).
#   - Column headers in the actual output DELIBERATELY use Josh's own
#     literal header text ("Bat Species", "Species Code", "State",
#     "Federal", "Phonic Group") rather than this project's usual
#     `$collum.name` dot-convention, since the whole point of table.type is
#     to match his external template's exact layout - flagged as an
#     intentional, spec-driven exception to the project's naming
#     convention, not an oversight.
#
# STILL-OPEN ITEMS (unchanged from the first build, or newly surfaced):
#   - The per-state listing-SEVERITY data gap above (the main one).
#   - `species.extirpated = TRUE` (default) remains an inert no-op with a
#     one-time notice - neither real model file has an extirpated species
#     example, and the reference database still has no $states.extirpated
#     column.
#   - The real `NAbat.namestest.csv` in the test-data folder turned out to
#     be built for testing `batz.batusa_recode.names()` (an `$input` column
#     of mixed-format strings against $latin/$common/$code4/$code6), not
#     this function - not used here, but worth re-verifying
#     `batz.batusa_recode.names` against it in a future session since a
#     real test file for THAT function is now available too.
# =============================================================================

# -----------------------------------------------------------------------------
# Real reference databases, loaded from disk here for dev/testing. Embedded
# directly in the final function (as with batz.batusa_recode.names) - no
# reference-file-path input in the spec.
# -----------------------------------------------------------------------------
nabat.reference   <- read.csv("NAbat.names.csv",   stringsAsFactors = FALSE, check.names = FALSE)
states.reference  <- read.csv("NAstates.names.csv", stringsAsFactors = FALSE, check.names = FALSE)
nabat.reference[]  <- lapply(nabat.reference,  function(col) trimws(as.character(col)))
states.reference[] <- lapply(states.reference, function(col) trimws(as.character(col)))

cat("=== nabat.reference ===\n"); cat("dim:", dim(nabat.reference), "\n")
cat("=== states.reference ===\n"); cat("dim:", dim(states.reference), "\n")

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
    "Not Listed"           = "",
    "Under Review"         = "UR",
    "Endangered"           = "E",
    "Proposed Endangered"  = "PE",
    fedstatus)   # fall through unchanged for any value not in the map
}

classify.inputs <- function(data, nabat, states) {

  x <- if (is.data.frame(data)) unlist(lapply(data, as.character), use.names = FALSE) else as.character(data)
  x.norm <- normalize(x)

  bat.cols   <- c("latin", "common", "code4", "code6")
  state.cols <- c("official.name", "short.name", "code2")

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

  list(
    bat.rows   = bat.seen[!duplicated(bat.seen)],
    state.rows = state.seen[!duplicated(state.seen)],
    unmatched  = unmatched
  )
}

format.bat.label <- function(row, batname.format, grammar.dash) {
  name <- switch(batname.format,
    "full"   = sprintf("%s (%s)%s", row[["common"]], row[["latin"]], row[["hibernation.strat"]]),
    "both"   = sprintf("%s (%s)", row[["common"]], row[["latin"]]),
    "common" = row[["common"]],
    "latin"  = row[["latin"]],
    stop(sprintf("batname.format must be one of \"full\", \"both\", \"common\", \"latin\" (got '%s')", batname.format))
  )
  if (!grammar.dash) name <- gsub("-", " ", name)
  name
}

format.state.label <- function(row, statename.format, grammar.dash) {
  label <- switch(statename.format,
    "code2"         = row[["code2"]],
    "official.name" = row[["official.name"]],
    "short.name"    = row[["short.name"]],
    stop(sprintf("statename.format must be one of \"code2\", \"official.name\", \"short.name\" (got '%s')", statename.format))
  )
  if (!grammar.dash) label <- gsub("-", " ", label)
  label
}

# status.value(): the per-(species,state) cell value.
#   "present.not.listed" -> presence.absence[1] ("*")
#   "absent"             -> presence.absence[2] ("-")
#   in $state.soc        -> "SC"
#   in $states.listed (not soc) -> "L" (generic placeholder - see header note)
status.value <- function(bat.row, state.code2, presence.absence) {
  present <- state.code2 %in% split.codes(bat.row[["states.present"]])
  soc     <- state.code2 %in% split.codes(bat.row[["state.soc"]])
  listed  <- state.code2 %in% split.codes(bat.row[["states.listed"]])
  if (!present) return(presence.absence[2])
  if (soc) return("SC")
  if (listed) return("L")
  presence.absence[1]
}

batz.batusa_list.species <- function(data,
                                      grammar.dash = TRUE,
                                      statename.format = "code2",
                                      batname.format = "full",
                                      bat.code = "code4",
                                      table.type = "state",
                                      presence.absence = c("*", "-"),
                                      phonic.group = TRUE,
                                      species.extirpated = TRUE) {

  nabat  <- nabat.reference
  states <- states.reference

  classified <- classify.inputs(data, nabat, states)

  if (length(classified$bat.rows) == 0 && length(classified$state.rows) == 0) {
    stop("No recognizable bat species or US state/territory identifiers were found anywhere in `data`.")
  }

  species.idx <- if (length(classified$bat.rows) > 0) classified$bat.rows else seq_len(nrow(nabat))
  state.idx   <- if (length(classified$state.rows) > 0) classified$state.rows else seq_len(nrow(states))

  if (length(classified$unmatched) > 0) {
    shown <- head(unique(classified$unmatched), 25)
    omitted.note <- if (length(unique(classified$unmatched)) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(unique(classified$unmatched)))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match either reference table: %s%s\n",
                 length(classified$unmatched), paste(shown, collapse = ", "), omitted.note))
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

  species.labels <- vapply(species.idx, function(i) format.bat.label(nabat[i, ], batname.format, grammar.dash), character(1))
  state.labels   <- vapply(state.idx,   function(j) format.state.label(states[j, ], statename.format, grammar.dash), character(1))
  code.col       <- if (bat.code %in% c("code4", "code6")) bat.code else if (bat.code == "none") NULL else
                       stop(sprintf("bat.code must be one of \"code4\", \"code6\", \"none\" (got '%s')", bat.code))
  federal.codes  <- vapply(nabat[species.idx, "fedstatus"], federal.abbrev, character(1))

  if (table.type == "matrix") {

    out <- data.frame("Bat Species" = species.labels, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(code.col)) out[["Species Code"]] <- nabat[species.idx, code.col]
    if (phonic.group) out[["Phonic Group"]] <- nabat[species.idx, "phonic.group"]
    out[["Federal"]] <- federal.codes

    for (k in seq_along(state.idx)) {
      j <- state.idx[k]
      state.code2 <- states[j, "code2"]
      col <- vapply(species.idx, function(i) {
        val <- status.value(nabat[i, ], state.code2, presence.absence)
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        val
      }, character(1))
      out[[state.labels[k]]] <- col
    }

    return(out)

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
        val <- status.value(nabat[i, ], state.code2, presence.absence)
        if (val == presence.absence[1]) val <- ""   # implicit - a row existing already means "present"
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        # NOTE: built POSITIONALLY (not as a named list) on purpose - Josh's
        # own model reuses the header "State" for both the row's state
        # (column 1) and the listing-status column (column 5+). A named
        # list can't hold two entries under the same name (a second
        # `row[["State"]] <- val` assignment would silently overwrite the
        # first instead of adding a column - hit exactly this bug on the
        # first pass). Column names are applied ONCE, after rbind, instead.
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
    col.names <- c("State", "Bat Species")
    if (!is.null(code.col)) col.names <- c(col.names, "Species Code")
    col.names <- c(col.names, "Federal", "State")
    if (phonic.group) col.names <- c(col.names, "Phonic Group")
    names(out) <- col.names   # set directly (not via data.frame()) so the duplicate "State" header survives literally
    out
  }
}

# -----------------------------------------------------------------------------
# tests - reproducing the real batlist model1.csv / model2.csv structurally
# (MA/ME/PA x the 9 species in those models). Real severity CODES in the
# model files are Josh's own hand-typed, admittedly-inconsistent mockup
# values (see header note) - not re-derived exactly; the STRUCTURE (columns,
# one-row-per-present-pair, matrix shape) is what's being verified here.
# -----------------------------------------------------------------------------
test.species <- c("epfu", "lano", "labo", "laci", "myle", "mylu", "myse", "myso", "pesu")
test.states  <- c("MA", "ME", "PA")

cat("\n=== table.type = 'state' (batlist model1 structure) ===\n")
print(batz.batusa_list.species(c(test.species, test.states), table.type = "state"))

cat("\n=== table.type = 'matrix' (batlist model2 structure) ===\n")
print(batz.batusa_list.species(c(test.species, test.states), table.type = "matrix"))

cat("\n=== batname.format = 'both' (no hibernation.strat suffix, unlike 'full') ===\n")
print(batz.batusa_list.species("epfu", table.type = "matrix", batname.format = "both"))

cat("\n=== batname.format = 'latin' / 'common' ===\n")
print(batz.batusa_list.species("epfu", table.type = "matrix", batname.format = "latin"))
print(batz.batusa_list.species("epfu", table.type = "matrix", batname.format = "common"))

cat("\n=== bat.code = 'code6' / 'none' ===\n")
print(batz.batusa_list.species("epfu", table.type = "matrix", bat.code = "code6"))
print(batz.batusa_list.species("epfu", table.type = "matrix", bat.code = "none"))

cat("\n=== phonic.group = FALSE ===\n")
print(batz.batusa_list.species("epfu", table.type = "matrix", phonic.group = FALSE))

cat("\n=== a species with real SC/L status somewhere (myle - state-listed +\n",
    "species-of-concern in several states) ===\n", sep = "")
print(batz.batusa_list.species("myle", table.type = "matrix"))
print(batz.batusa_list.species(c("myle", "PA", "MA", "VT"), table.type = "state"))

cat("\n=== species-driven query, table.type = 'state' - which states have\n",
    "Big brown bat? (all 56 states/territories checked) ===\n", sep = "")
print(head(batz.batusa_list.species("epfu"), 15))

cat("\n=== state-driven query, table.type = 'matrix' - all 54 species x OH ===\n")
print(head(batz.batusa_list.species("OH", table.type = "matrix")[, c("Bat Species", "OH")], 15))

cat("\n=== custom presence.absence symbols ===\n")
print(batz.batusa_list.species(c("epfu", "myse"), table.type = "matrix", presence.absence = c("Y", "N")))

cat("\n=== grammar.dash = FALSE ===\n")
print(batz.batusa_list.species("lano", table.type = "matrix", grammar.dash = FALSE))

cat("\n=== unmatched element mixed in -> WARNING, real one still resolves ===\n")
print(batz.batusa_list.species(c("epfu", "not_a_real_bat_or_state"), table.type = "matrix"))

cat("\n=== data frame input (flattened for classification) ===\n")
test.df <- data.frame(col.a = c("epfu", "OH"), col.b = c("mylu", "VT"), stringsAsFactors = FALSE)
print(batz.batusa_list.species(test.df, table.type = "matrix"))

cat("\n=== error: nothing matches ===\n")
tryCatch(batz.batusa_list.species(c("nope", "nada")), error = function(e) cat("Got expected error:", conditionMessage(e), "\n"))

cat("\n=== error: invalid table.type ===\n")
tryCatch(batz.batusa_list.species("epfu", table.type = "bogus"), error = function(e) cat("Got expected error:", conditionMessage(e), "\n"))

cat("\n=== Corynorhinus subspecies disambiguation still holds ===\n")
print(batz.batusa_list.species(c("coto", "coti", "cotv"), table.type = "matrix", statename.format = "short.name"))

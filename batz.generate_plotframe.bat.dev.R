# batz.generate_plotframe.bat.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.plotframe_*
# Action:  batactivity   (Josh's own given name already fits the
#                          batz.<family>_<action>.<subject>() pattern with
#                          no separate subject needed - same shape as
#                          batz.datawrangler_rename() - no rename needed)
#
# Purpose: take a merged/joined vetted-and-suntimes data frame and build a
# summary "plot frame" (plfr.batsummary) of detection counts and
# earliest/latest call time per monitoring night, species, and detector -
# the thing you'd feed straight into a plotting function.
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS/NORMALIZATIONS FLAGGED FOR JOSH:
#
#  1. **Required header standardized on $call.datetime (2026-08-26,
#     resolved).** The spec's Required Inputs originally listed this
#     function's input data frame as needing a column literally called
#     $call.time, while batz.vettedacoustics_merge.format() (the function
#     that produces everything else in this list) outputs a column
#     called $call.datetime instead - a real pipeline-naming mismatch,
#     confirmed (not assumed) against Josh's own real test data. Flagged
#     to Josh at delivery; he replied with an explicit instruction to
#     standardize on $call.datetime as the header whenever an element
#     holds a combined date+time value (reserving $call.time/bare "time"
#     naming for time-only values), and to update every function using
#     either name to reflect it. This function's required header,
#     internal candidate-list/parse-function variable names, and error
#     messages were all renamed accordingly - no functional change, and
#     no rename step is needed anymore when chaining this function after
#     batz.vettedacoustics_merge.format(), since that function's own
#     output column is already named $call.datetime. Josh's own small
#     static test file (vetted.merged.csv) still has a raw column
#     literally named "call.time" (it's a file on his machine, unaffected
#     by this code change) - see TEST 8 below, which renames it going in.
#
#  2. **$sunregion isn't produced by any existing batz function at all** -
#     it has to come from a separate join (e.g. against an *arulist.csv
#     via $aru.name, the way WTG.arulist.csv relates $aru to $sunregion).
#     ORIGINALLY (2026-08-27) this function just required the column to
#     already be present in `data` - it did no joining itself. **This has
#     since changed - see assumption #13 below: the function now loads the
#     arulist and does this join itself.** Left this note as-is (historical
#     - it was true when written) rather than rewritten, per this file's
#     usual convention.
#
#  3. **Parameter renamed: "date" -> "groupby.date".** The spec's Optional
#     Inputs literally name this parameter "date" (default $date.mon) -
#     but the input data frame ALSO has its own real $date column (the
#     raw recording date, distinct from $date.mon). Naming the parameter
#     "date" would be genuinely confusing (and shadows R's own base
#     `date()` function) when the whole point of this parameter is "which
#     column to group by," exactly like the sibling parameter
#     "groupby" already is. Renamed to "groupby.date" to match that
#     naming pattern and avoid the collision - told Josh about the rename
#     per project convention.
#
#  4. **Parameter renamed: "trim.noID" -> "trim.noid".** Lowercase to match
#     the identically-purposed parameter already shipped in
#     batz.vettedacoustics_merge.format() (cross-function parameter-name
#     consistency, per the project's own established rule) - told Josh.
#
#  5. **"duplicates.remove ... within each individual loaded object (never
#     across objects)" is spec-template-leftover phrasing**, copied over
#     from batz.datawrangler_load.files (a function that loads MANY
#     separate file objects). This function takes exactly ONE data frame -
#     there's only one "object" here, so it's just implemented as: drop
#     exact duplicate rows from `data`. Also dropped a stray trailing
#     "duplicates" word at the very end of the spec's Optional Inputs list
#     with no definition attached - read as another leftover fragment, not
#     a real parameter.
#
#  6. **Header check: "stop... and list the missing headers as a warning"**
#     is contradictory taken literally (stop() halts; warning() doesn't) -
#     implemented as stop() with an informative message that lists every
#     missing header by name, so the "warning" content is delivered via
#     the stop error itself.
#
#  7. **$mins2.noon.min/max computation.** "$date.mon... starts at noon
#     each calendar day" - so for each row, the anchor is noon on the DATE
#     given by the groupby.date column (default $date.mon), and
#     mins2.noon = minutes from that noon anchor to $call.datetime (the
#     given literal spec formula, "$call.datetime - $date.mon", is read
#     using whatever column groupby.date/call.datetime actually point
#     at). Reused
#     batz.datawrangler_call.datetime()'s own date-format auto-detection
#     to parse the groupby.date column robustly (it may not be in a fixed
#     format - real $date.mon/$monitoringnight values look like
#     "6/26/2026") by feeding it alongside a constant "12:00:00" time
#     vector - this is exactly what that function is for, so no new
#     date-parsing logic was written from scratch.
#
#  7b. **$call.datetime ALSO needed its own format auto-detection - a real
#     bug caught during testing, not a hypothetical.** A naive
#     `as.POSIXct(data$call.datetime, tz = "UTC")` (no format given) works
#     fine for a batz.datawrangler_call.datetime-style string
#     ("2026-05-17 21:21:44") but throws "character string is not in a
#     standard unambiguous format" on a real raw Timestamp-style value
#     like "5/17/2026 21:21" (no seconds) - exactly what Josh's own small
#     vetted.merged.csv test file's (renamed) $call.datetime column
#     actually contains. Added a small regex-guarded candidate-format
#     list (same exact-full-string-match-before-parsing pattern already
#     used in batz.datawrangler_call.datetime, to avoid the same
#     silent-partial-match trap documented there) covering both shapes
#     (with/without seconds, 2-digit/4-digit year) - errors clearly,
#     listing every candidate tried, if none of them match every value.
#
#  8. **alldetections = TRUE behavior - the most open-ended part of the
#     spec.** Steps literally read: build the per-species/date/aru summary
#     (step 4) -> "if alldetections = FALSE ... return plfr.batsummary" ->
#     "if alldetections = TRUE then [trim noise/noID] ... Repeat summary
#     for above summarizing by only $date and $detector with $spp.id =
#     'All Detections' ... return plfr.batsummary." Three readings seemed
#     plausible: (a) alldetections=TRUE returns ONLY the collapsed
#     all-detections table, discarding the per-species breakdown; (b) it
#     returns ONLY the per-species breakdown (the "repeat" line being
#     read as decorative); (c) it returns BOTH, row-bound into one table
#     (per-species rows AND "All Detections" rows, distinguished by the
#     $spp.id column). Went with (c) - it's the only reading that makes
#     both "Create summary... $spp.id = [column] which header is being
#     summarized" (step 4, unconditional) and "Repeat summary for above"
#     (implying a SECOND table, not a replacement) make sense together,
#     and it's the most useful single output (per-species stats AND an
#     overall-activity total in one table). **Please confirm** - this is
#     a real interpretive judgment call, not a small one.
#
#  9. **Noise/NoID trimming only affects the "All Detections" collapsed
#     summary, not the per-species breakdown** - read literally from the
#     Steps order (trimming is described AFTER step 4's per-species
#     summary already exists, and only inside the alldetections=TRUE
#     branch). This means "Noise"/"NoID" still show up as their own
#     species rows in the per-species table (so you can see how much
#     noise/unidentified activity there was per night/detector), while
#     the "All Detections" total only counts genuine wildlife activity.
#     Flagging in case Josh actually wants noise/NoID excluded from BOTH
#     tables.
#
# 10. **No auto-assign-into-caller's-environment side effect** - unlike
#     the merge/load-style batz functions, this spec's own wording just
#     says "return plfr.batsummary" (one data frame), never "create a new
#     object in R called X" - so this function returns its result
#     directly (`plfr.batsummary <- batz.generate_plotframe.bat(data)`),
#     with no bare-call auto-assign. Flagging the deliberate departure
#     from the other functions' convention, since it's driven by this
#     function's own literal wording.
#
# 11. **No write.output/dir.save/file-writing step** - "Final output" in
#     the spec was left blank and nothing in Steps mentions writing to
#     disk, so this function just returns an R object.
#
# 12. **Follow-up (2026-08-27, later still, per Josh: "include $sunregion
#     from the merged data") - $sunregion now carried through into
#     plfr.batsummary, placed right after $groupby (now $group).** $sunregion was
#     already a REQUIRED input column (assumption #2 above) but was
#     validated and then silently dropped - never appearing in the
#     output. Collapsed the same way as every other summary column, one
#     value per spp.id/groupby.date/groupby group. Since $sunregion
#     is expected to be constant for a given detector (joined in via
#     $aru.name upstream - assumption #2), each group is checked for
#     internal consistency rather than just taking the first value: a
#     group containing more than one distinct $sunregion value (e.g. if
#     groupby is overridden to a column, like "serial", that doesn't
#     line up 1:1 with $sunregion the way $aru.name does) now stops with
#     a message naming the exact group and the conflicting values,
#     instead of silently picking one. See TEST 9/TEST 10 below.
#
# 13. **Follow-up (2026-08-27, later still, per Josh: new dir.load/
#     load.pattern/dir.sub params, "load arulist from the file ending
#     with 'arulist.csv'", "append the $sunregion to the bioactivity file
#     by matching the $aru or $aru.name between the two files") -
#     $sunregion is no longer a required column of `data` at all (removed
#     from required.headers - see assumption #2 above, left as historical
#     since it was true when written).** This function now loads its own
#     arulist file(s) and joins $sunregion internally, matching the
#     directory-scanning pattern already used by
#     batz.vettedacoustics_merge.format() (dir.load/load.pattern/dir.sub,
#     glob2rx()-based regex, per-file tryCatch(read.csv()) with
#     header-normalization, skip-with-message for an invalid file) - three
#     new params: dir.load (default getwd()), load.pattern (default
#     "*.arulist.csv"), dir.sub (default TRUE - note this default is TRUE
#     here, unlike that function's own dir.sub = FALSE default, per
#     Josh's explicit spec for this parameter). The join is always
#     data$aru.name vs arulist$aru specifically (never whatever
#     groupby happens to be set to, since groupby can be
#     overridden to something like "serial" that has no relationship to
#     the arulist's $aru values). No arulist file found at all, or file(s)
#     found but none with both $aru and $sunregion columns, is a hard
#     stop() - there'd be no way to populate $sunregion. An unmatched
#     $aru.name value gets $sunregion = NA with a warning() listing every
#     such value (not a hard stop, matching this function's existing
#     tolerant-but-vocal style elsewhere). Two interpretive calls flagged
#     for Josh to confirm: (1) any $sunregion already present in the
#     `data` passed in is silently OVERWRITTEN by this fresh join, not
#     preserved or checked for agreement - see TEST 15; (2) multiple files
#     matching load.pattern are all loaded and row-bound together rather
#     than treated as an error - see TEST 13. Verified against the real
#     WTG.arulist.csv (every real test-data $aru.name resolves to
#     "penobscotbay", matching exactly) plus new synthetic-file tests for
#     the loading mechanics themselves - see TEST 4/6/9/10 (reworked) and
#     TEST 11-15 (new) below.
#
# 14. **Follow-up (2026-08-28, per Josh: function renamed to
#     batz.generate_plotframe.bat(); "date.groupby" -> "groupby.date";
#     "aru.groupby" -> "groupby"; output gains $groupby.date/$groupedby,
#     and the old $aru.groupby output column is renamed to $group).**
#     Mechanical rename only for the function/parameter names - every
#     internal reference, error message, and this file's own comments were
#     updated to match (see the global sed-driven rename applied to this
#     dev script; TEST 3/10 below exercise the renamed `groupby` param
#     directly). The two NEW metadata columns ($groupby.date/$groupedby)
#     and the renamed value column ($group) are a real interpretive call,
#     not just a rename - Josh's own spec text gives $group the exact same
#     example values ("aru.name"/"deployment.type") as $groupedby, which
#     would make them redundant if read completely literally. Implemented
#     instead as: $groupedby/$groupby.date are METADATA (constant per
#     call - the literal NAME of the column that was grouped by, e.g.
#     "aru.name"/"date.mon"), while $group is the actual PER-ROW grouping
#     VALUE (e.g. "105059-NW3") - the same column the old $aru.groupby
#     output used to hold, just renamed. This is the only reading under
#     which $group's own wording ("for that observation") and $groupedby's
#     wording ("what the data was grouped by") aren't simply duplicates of
#     each other. **Please confirm** - flagged in the .R file's own Details
#     too. See TEST 16 below for the new columns.
# ---------------------------------------------------------------------------

## ===========================================================================
## SECTION 0: dependency (batz.datawrangler_call.datetime, reused for
## robust date-format auto-detection of the groupby.date column - see
## assumption #7 above)
## ===========================================================================
source("/home/claude/datetime_work/batz.datawrangler_call.datetime.R")

## ===========================================================================
## SECTION 1: function under test (same body as the final .R file)
## ===========================================================================
batz.generate_plotframe.bat <- function(data,
                                         duplicates.remove = TRUE,
                                         spp.id = "manid.sb",
                                         groupby.date = "date.mon",
                                         groupby = "aru.name",
                                         alldetections = TRUE,
                                         trim.noise = TRUE,
                                         trim.noid = TRUE,
                                         dir.load = getwd(),
                                         load.pattern = c("*.arulist.csv"),
                                         dir.sub = TRUE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  ## $sunregion is NOT in this list (and no longer needs to already be a
  ## column of `data`) - it's now loaded from an *arulist.csv file and
  ## joined on below, replacing the old "join it in yourself first"
  ## requirement. See Details/Follow-up.
  required.headers <- c("filename", "date.mon", "manid", "autoid.kp",
                         "autoid.sb", "lat", "serial", "lon", "aru.name",
                         "date", "time", "call.datetime")
  missing.headers <- setdiff(required.headers, names(data))
  if (length(missing.headers) > 0) {
    stop("`data` is missing required header(s): ",
         paste(missing.headers, collapse = ", "))
  }

  for (colname in c(spp.id, groupby.date, groupby)) {
    if (!(colname %in% names(data))) {
      stop("`", colname, "` (from spp.id/groupby.date/groupby) is not ",
           "a column of `data`.")
    }
  }

  ## --- load $sunregion from an *arulist.csv file and join it onto `data`
  ## by matching `data$aru.name` against the arulist file's own `$aru`
  ## column (always $aru.name specifically, regardless of what groupby
  ## points to - groupby can be overridden to an unrelated column like
  ## "serial", which wouldn't correspond to the arulist's $aru values at
  ## all) - see Details/Follow-up ----------------------------------------
  normalize.header <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
  arulist.regex <- paste(utils::glob2rx(load.pattern), collapse = "|")
  arulist.files <- list.files(dir.load, pattern = arulist.regex, recursive = dir.sub,
                               full.names = TRUE, ignore.case = TRUE)
  if (length(arulist.files) == 0) {
    stop("No file matching `load.pattern` (\"", paste(load.pattern, collapse = ", "),
         "\") found in `dir.load` (\"", dir.load, "\", dir.sub = ", dir.sub, ") - ",
         "an arulist file is required to look up $sunregion.")
  }

  arulist <- data.frame(aru = character(0), sunregion = character(0), stringsAsFactors = FALSE)
  arulist.skipped <- character(0)
  for (f in arulist.files) {
    tmp <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                     error = function(e) NULL)
    if (is.null(tmp)) { arulist.skipped <- c(arulist.skipped, paste0(f, " (could not read file)")); next }
    names(tmp) <- normalize.header(names(tmp))
    if (!all(c("aru", "sunregion") %in% names(tmp))) {
      arulist.skipped <- c(arulist.skipped, paste0(f, " (missing $aru and/or $sunregion column)")); next
    }
    arulist <- rbind(arulist, tmp[, c("aru", "sunregion"), drop = FALSE])
  }
  if (length(arulist.skipped) > 0) {
    message("batz.generate_plotframe.bat: skipped arulist file(s) that didn't have ",
            "both an $aru and $sunregion column: ", paste(arulist.skipped, collapse = "; "))
  }
  if (nrow(arulist) == 0) {
    stop("Found ", length(arulist.files), " file(s) matching `load.pattern` in `dir.load`, ",
         "but none had both an `$aru` and `$sunregion` column - cannot look up $sunregion.")
  }

  data$sunregion <- arulist$sunregion[match(data$aru.name, arulist$aru)]
  unmatched.arus <- unique(data$aru.name[is.na(data$sunregion)])
  if (length(unmatched.arus) > 0) {
    warning("$aru.name value(s) not found in the loaded arulist - $sunregion will be NA for: ",
            paste(unmatched.arus, collapse = ", "))
  }

  if (duplicates.remove) {
    data <- data[!duplicated(data), , drop = FALSE]
  }

  data$obs <- 1

  ## noon-of-monitoring-night anchor, reusing batz.datawrangler_call.datetime()'s
  ## own date-format auto-detection
  noon.anchor <- as.POSIXct(
    batz.datawrangler_call.datetime(date = as.character(data[[groupby.date]]),
                                     time = rep("120000", nrow(data))),
    tz = "UTC")

  ## $call.datetime format auto-detection - regex-guarded candidate list, same
  ## exact-full-string-match pattern already used in
  ## batz.datawrangler_call.datetime to avoid a silent partial-match
  call.datetime.candidates <- list(
    list(fmt = "%Y-%m-%d %H:%M:%S", rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%Y-%m-%d %H:%M",    rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%Y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M:%S", rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$"),
    list(fmt = "%m/%d/%y %H:%M",    rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2} [0-9]{1,2}:[0-9]{1,2}$")
  )
  parse.call.datetime <- function(x) {
    x <- trimws(as.character(x))
    present <- x[!is.na(x) & nzchar(x)]
    chosen <- NULL
    if (length(present) > 0) {
      for (cand in call.datetime.candidates) {
        if (!all(grepl(cand$rx, present))) next
        parsed <- as.POSIXct(present, format = cand$fmt, tz = "UTC")
        if (any(is.na(parsed))) next
        chosen <- cand$fmt
        break
      }
      if (is.null(chosen)) {
        stop("Could not auto-detect the format of $call.datetime - none of the ",
             "built-in candidate formats (",
             paste(vapply(call.datetime.candidates, function(c) c$fmt, character(1)), collapse = ", "),
             ") matched every non-blank value.")
      }
    }
    as.POSIXct(x, format = chosen, tz = "UTC")
  }
  call.dt <- parse.call.datetime(data$call.datetime)
  data$.mins2.noon <- as.numeric(difftime(call.dt, noon.anchor, units = "mins"))

  safe.min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe.max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  ## $sunregion is a per-detector attribute (now joined onto `data` above,
  ## from the loaded arulist, by $aru.name), so every row within a single
  ## spp.id/groupby.date/groupby group should already agree on it
  ## whenever groupby == "aru.name" (the default). Collapsed with a
  ## consistency check
  ## rather than silently taking the first value, so a real data problem
  ## (e.g. groupby overridden to a column that doesn't line up 1:1
  ## with $sunregion) surfaces as a clear error instead of a silently
  ## arbitrary pick.
  safe.sunregion <- function(x, group.label) {
    present <- unique(x[!is.na(x) & nzchar(trimws(x))])
    if (length(present) == 0) return(NA_character_)
    if (length(present) > 1) {
      stop("`$sunregion` has more than one distinct value (",
           paste(present, collapse = ", "), ") within a single ",
           "spp.id/groupby.date/groupby group (", group.label, ") - ",
           "sunregion is expected to be constant per groupby.")
    }
    present[[1]]
  }

  build.summary <- function(df, spp.override = NULL) {
    spp.vals <- if (!is.null(spp.override)) rep(spp.override, nrow(df)) else as.character(df[[spp.id]])
    date.vals  <- as.character(df[[groupby.date]])
    group.vals <- as.character(df[[groupby]])
    sun.vals   <- as.character(df$sunregion)

    key <- paste(spp.vals, date.vals, group.vals, sep = "\r")
    ag.obs <- tapply(df$obs, key, sum)
    ag.min <- tapply(df$.mins2.noon, key, safe.min)
    ag.max <- tapply(df$.mins2.noon, key, safe.max)

    keys  <- names(ag.obs)
    parts <- strsplit(keys, "\r", fixed = TRUE)

    ## computed per-key (not via tapply) so a stop() from safe.sunregion()
    ## names the actual offending spp.id/date/groupby combination in a
    ## human-readable form (the raw key itself is \r-joined and unreadable
    ## if ever printed)
    group.labels <- vapply(parts, function(p) sprintf("spp.id=%s, date=%s, groupby=%s", p[1], p[2], p[3]), character(1))
    names(group.labels) <- keys
    ag.sun <- vapply(keys, function(k) safe.sunregion(sun.vals[key == k], group.label = group.labels[[k]]),
                      character(1))

    out <- data.frame(
      spp.id         = vapply(parts, `[`, character(1), 1),
      date           = vapply(parts, `[`, character(1), 2),
      group          = vapply(parts, `[`, character(1), 3),
      groupedby      = groupby,
      groupby.date   = groupby.date,
      sunregion      = as.character(ag.sun[keys]),
      obs            = as.numeric(ag.obs[keys]),
      mins2.noon.min = as.numeric(ag.min[keys]),
      mins2.noon.max = as.numeric(ag.max[keys]),
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    out
  }

  if (!alldetections) {
    plfr.batsummary <- build.summary(data)
    plfr.batsummary$vetting.type <- spp.id
    return(plfr.batsummary)
  }

  ## alldetections = TRUE: per-species breakdown (on all rows, pre-trim)
  ## + a collapsed "All Detections" summary on the noise/NoID-trimmed
  ## data, combined into one table - see Details
  species.summary <- build.summary(data)

  trimmed <- data
  if (trim.noise) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noise"), , drop = FALSE]
  }
  if (trim.noid) {
    trimmed <- trimmed[!(tolower(trimws(as.character(trimmed[[spp.id]]))) == "noid"), , drop = FALSE]
  }

  all.summary <- build.summary(trimmed, spp.override = "All Detections")

  plfr.batsummary <- rbind(species.summary, all.summary)
  plfr.batsummary$vetting.type <- spp.id
  plfr.batsummary <- plfr.batsummary[order(plfr.batsummary$group,
                                            plfr.batsummary$date,
                                            plfr.batsummary$spp.id), ]
  rownames(plfr.batsummary) <- NULL

  plfr.batsummary
}

## ===========================================================================
## SECTION 2: build a realistic test input by running the ACTUAL,
## already-built batz.vettedacoustics_merge.format() against a real raw
## vetted file. Unlike before assumption #13, test.data is NOT manually
## bolted with a $sunregion column here anymore - the function under test
## now loads and joins $sunregion itself (from the real WTG.arulist.csv,
## via the new dir.load/load.pattern/dir.sub params), so every call below
## passes dir.load = arulist.dir (the folder the real WTG.arulist.csv
## lives in) instead. No $call.datetime rename step needed since
## batz.vettedacoustics_merge.format() already outputs a column with that
## exact name (see assumption #1).
## ===========================================================================
source("/home/claude/vettedacoustics_work2/batz.batusa_recode.names.R")
source("/home/claude/vettedacoustics_work2/batz.vettedacoustics_merge.format.R")

vetted.dir <- "/mnt/user-data/uploads/4 Current  test data/05.08.2026_05.27.2026/Vetted"
res <- batz.vettedacoustics_merge.format(dir.load = vetted.dir,
                                          load.pattern = "*FinalVetted.csv",
                                          trim.noise = FALSE)   # keep NOISE rows in this test set on purpose
test.data <- res$vetted.merged

## the folder the real WTG.arulist.csv lives in - passed as dir.load to
## every test call below so the function under test can find it (its
## default dir.load = getwd() would look in this script's own working
## directory instead, which doesn't have it)
arulist.dir <- "/mnt/user-data/uploads/4 Current  test data"

## loaded here too (independently of the function under test) purely so
## TEST 9 below has something to cross-check the function's OWN internal
## join against
real.arulist <- read.csv(file.path(arulist.dir, "WTG.arulist.csv"),
                          stringsAsFactors = FALSE, check.names = FALSE)

cat("Built test.data:", nrow(test.data), "rows,", ncol(test.data), "cols (no $sunregion column yet - ",
    "the function under test loads/joins that itself now)\n")
print(names(test.data))
cat("\n")

## ===========================================================================
## SECTION 3: tests
## ===========================================================================
cat("=== TEST 1: defaults (spp.id = manid.sb, alldetections = TRUE) ===\n")
r1 <- batz.generate_plotframe.bat(test.data, dir.load = arulist.dir)
print(head(r1, 8))
cat("n rows total:", nrow(r1), "\n")
cat("has 'All Detections' rows?", any(r1$spp.id == "All Detections"), "\n")
cat("$vetting.type unique:", paste(unique(r1$vetting.type), collapse=", "), "\n")
cat("mins2.noon range:", range(c(r1$mins2.noon.min, r1$mins2.noon.max), na.rm = TRUE), "(sanity: should be within roughly 0-1440)\n\n")

cat("=== TEST 2: alldetections = FALSE (species breakdown only) ===\n")
r2 <- batz.generate_plotframe.bat(test.data, alldetections = FALSE, dir.load = arulist.dir)
print(head(r2, 5))
cat("has 'All Detections' rows (should be FALSE)?", any(r2$spp.id == "All Detections"), "\n\n")

cat("=== TEST 3: custom spp.id = 'manid.kp', custom groupby = 'serial' ===\n")
r3 <- batz.generate_plotframe.bat(test.data, spp.id = "manid.kp", groupby = "serial", dir.load = arulist.dir)
print(head(r3, 5))
cat("$vetting.type:", unique(r3$vetting.type), "\n\n")

cat("=== TEST 4: missing-header error ===\n")
## $sunregion is no longer in required.headers (assumption #13) so this
## no longer strips it - just $lat, the remaining required column that's
## cheapest to drop for this check
bad.data <- test.data[, setdiff(names(test.data), c("lat"))]
tryCatch(batz.generate_plotframe.bat(bad.data, dir.load = arulist.dir),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 5: invalid spp.id column ===\n")
tryCatch(batz.generate_plotframe.bat(test.data, spp.id = "not.a.real.column", dir.load = arulist.dir),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 6: trim.noise/trim.noid on a bigger, real dataset with actual NOISE/blank rows ===\n")
res.big <- batz.vettedacoustics_merge.format(dir.load = "/mnt/user-data/uploads/4 Current  test data",
                                              load.pattern = "*FinalVetted.csv", dir.sub = FALSE,
                                              trim.noise = FALSE, bat.names = "common")
big.data <- res.big$vetted.merged
## FinalVetted.csv's ARUs aren't in the real WTG.arulist.csv, so this is
## also exercising the unmatched-$aru.name warning path (see assumption
## #13) - every row should come back with $sunregion = NA here, on
## purpose, rather than being manually stubbed like before #13
r6.notrim <- withCallingHandlers(
  batz.generate_plotframe.bat(big.data, trim.noise = FALSE, trim.noid = FALSE, dir.load = arulist.dir),
  warning = function(w) {
    cat("Got expected unmatched-$aru.name warning:", substr(conditionMessage(w), 1, 90), "...\n")
    invokeRestart("muffleWarning")
  })
r6.trim <- suppressWarnings(
  batz.generate_plotframe.bat(big.data, trim.noise = TRUE, trim.noid = TRUE, dir.load = arulist.dir))
cat("all $sunregion NA in TEST 6 output (expected, since big.data's ARUs aren't in the real arulist)?",
    all(is.na(r6.notrim$sunregion)), "\n")
all.notrim <- sum(r6.notrim[r6.notrim$spp.id == "All Detections", "obs"])
all.trim   <- sum(r6.trim[r6.trim$spp.id == "All Detections", "obs"])
species.noise.count <- sum(r6.notrim[tolower(r6.notrim$spp.id) == "noise" & r6.notrim$spp.id != "All Detections", "obs"])
cat("All Detections total, no trim:", all.notrim, " | with trim:", all.trim,
    " | difference:", all.notrim - all.trim, " | actual NOISE rows in per-species table:", species.noise.count, "\n")
cat("noise still present in per-species breakdown even when trimmed for All Detections?",
    any(tolower(r6.trim$spp.id[r6.trim$spp.id != "All Detections"]) == "noise"), "\n\n")

cat("=== TEST 7: duplicates.remove ===\n")
dup.data <- rbind(test.data, test.data[1:5, ])
r7.dedup   <- batz.generate_plotframe.bat(dup.data, duplicates.remove = TRUE, dir.load = arulist.dir)
r7.nodedup <- batz.generate_plotframe.bat(dup.data, duplicates.remove = FALSE, dir.load = arulist.dir)
cat("obs sum with dedup:", sum(r7.dedup[r7.dedup$spp.id=="All Detections","obs"]),
    " vs without:", sum(r7.nodedup[r7.nodedup$spp.id=="All Detections","obs"]), "\n\n")

cat("=== TEST 8: spot-check against Josh's own small vetted.merged.csv test file (adapted headers) ===\n")
small <- read.csv("/mnt/user-data/uploads/4 Current  test data/vetted.merged.csv",
                   stringsAsFactors = FALSE, check.names = FALSE)
names(small)[names(small) == "monitoringnight"]       <- "date.mon"
names(small)[names(small) == "speciesmanualid"]        <- "manid"
names(small)[names(small) == "wakaleidoscopeautoid"]   <- "autoid.kp"
names(small)[names(small) == "sppaccp"]                <- "autoid.sb"
## this real static file's raw header is still literally "call.time" -
## unaffected by the code-level $call.datetime standardization, so it
## still needs adapting here to match the function's required header
names(small)[names(small) == "call.time"]              <- "call.datetime"
r8 <- suppressWarnings(batz.generate_plotframe.bat(small, spp.id = "autoid.sb", dir.load = arulist.dir))
print(r8)

cat("\n=== TEST 9: $sunregion is loaded/joined internally and carried through into plfr.batsummary ===\n")
cat("has $sunregion column?", "sunregion" %in% names(r1), "\n")
print(head(r1[, c("group", "sunregion")], 8))
## cross-check: for defaults (groupby = aru.name), every output row's
## $sunregion should match real.arulist (loaded independently in SECTION 2,
## purely for this check) - NOT test.data, which no longer carries its own
## pre-joined $sunregion column now that the function loads it internally
check.sun <- merge(r1, unique(real.arulist[, c("aru", "sunregion")]),
                    by.x = "group", by.y = "aru", suffixes = c("", ".expected"))
cat("any mismatch vs real.arulist?", any(check.sun$sunregion != check.sun$sunregion.expected), "\n")
cat("any NA sunregion in output (should be FALSE - every real test-data $aru.name is in WTG.arulist.csv)?",
    any(is.na(r1$sunregion)), "\n\n")

cat("=== TEST 10: inconsistent $sunregion within one spp.id/groupby.date/groupby group stops with a clear error ===\n")
## the real WTG.arulist.csv has NO variation in $sunregion (both its ARUs
## map to "penobscotbay"), so a genuine conflict can't be produced against
## it - built a small synthetic arulist (2 distinct sunregion values) and
## synthetic data with two rows sharing one $serial value (the
## groupby used here) but different $aru.name values that resolve to
## those two different sunregions
conflict.dir <- tempfile("arulist_conflict_")
dir.create(conflict.dir)
write.csv(data.frame(aru = c("CONFLICT_ARU_A", "CONFLICT_ARU_B"),
                      sunregion = c("north.region", "south.region"),
                      stringsAsFactors = FALSE),
          file.path(conflict.dir, "conflict.arulist.csv"), row.names = FALSE)

conflict.data <- test.data[1:2, ]
conflict.data$aru.name <- c("CONFLICT_ARU_A", "CONFLICT_ARU_B")
conflict.data$serial   <- "SHARED_SERIAL"     # forces both rows into one groupby = "serial" group
conflict.data$manid.sb <- conflict.data$manid.sb[1]   # same spp.id value for both rows
conflict.data$date.mon <- conflict.data$date.mon[1]   # same groupby.date value for both rows

tryCatch(batz.generate_plotframe.bat(conflict.data, groupby = "serial", dir.load = conflict.dir),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 11: no file matching load.pattern found in dir.load -> hard stop ===\n")
empty.dir <- tempfile("empty_arulist_dir_")
dir.create(empty.dir)
tryCatch(batz.generate_plotframe.bat(test.data, dir.load = empty.dir),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 12: arulist file found but missing $aru/$sunregion columns -> skipped (message), then hard stop (no usable file) ===\n")
badcols.dir <- tempfile("badcols_arulist_dir_")
dir.create(badcols.dir)
write.csv(data.frame(notaru = "X", notsunregion = "Y", stringsAsFactors = FALSE),
          file.path(badcols.dir, "bad.arulist.csv"), row.names = FALSE)
tryCatch(
  withCallingHandlers(
    batz.generate_plotframe.bat(test.data, dir.load = badcols.dir),
    message = function(m) {
      cat("Got expected skip-message:", substr(conditionMessage(m), 1, 90), "...\n")
      invokeRestart("muffleMessage")
    }),
  error = function(e) cat("Correctly errored (hard stop, no usable arulist file):", conditionMessage(e), "\n\n"))

cat("=== TEST 13: multiple valid arulist files matching load.pattern are merged (row-bound) together ===\n")
multi.dir <- tempfile("multi_arulist_dir_")
dir.create(multi.dir)
write.csv(data.frame(aru = "MULTI_ARU_A", sunregion = "regionA", stringsAsFactors = FALSE),
          file.path(multi.dir, "part1.arulist.csv"), row.names = FALSE)
write.csv(data.frame(aru = "MULTI_ARU_B", sunregion = "regionB", stringsAsFactors = FALSE),
          file.path(multi.dir, "part2.arulist.csv"), row.names = FALSE)
multi.data <- test.data[1:2, ]
multi.data$aru.name <- c("MULTI_ARU_A", "MULTI_ARU_B")
multi.data$manid.sb <- multi.data$manid.sb[1]
multi.data$date.mon <- multi.data$date.mon[1]
r13 <- batz.generate_plotframe.bat(multi.data, dir.load = multi.dir)
cat("both files' sunregion values present in output (regionA and regionB)?",
    all(c("regionA", "regionB") %in% r13$sunregion), "\n\n")

cat("=== TEST 14: dir.sub = TRUE (default) finds a nested arulist file; dir.sub = FALSE does not ===\n")
nested.parent <- tempfile("nested_arulist_parent_")
dir.create(nested.parent)
nested.sub <- file.path(nested.parent, "subdir")
dir.create(nested.sub)
write.csv(data.frame(aru = "NESTED_ARU", sunregion = "nested.region", stringsAsFactors = FALSE),
          file.path(nested.sub, "nested.arulist.csv"), row.names = FALSE)
nest.data <- test.data[1, , drop = FALSE]
nest.data$aru.name <- "NESTED_ARU"
r14.sub <- batz.generate_plotframe.bat(nest.data, dir.load = nested.parent, dir.sub = TRUE)
cat("dir.sub = TRUE finds the nested file, sunregion resolved?",
    "nested.region" %in% r14.sub$sunregion, "\n")
tryCatch(batz.generate_plotframe.bat(nest.data, dir.load = nested.parent, dir.sub = FALSE),
         error = function(e) cat("dir.sub = FALSE correctly can't find the nested file, errors:",
                                  conditionMessage(e), "\n\n"))

cat("=== TEST 15: a pre-existing $sunregion column in the input data is silently overwritten by the fresh join ===\n")
overwrite.data <- test.data
overwrite.data$sunregion <- "BOGUS_PRESET_VALUE"
r15 <- batz.generate_plotframe.bat(overwrite.data, dir.load = arulist.dir)
cat("bogus preset value survived (should be FALSE)?", any(r15$sunregion == "BOGUS_PRESET_VALUE"), "\n")
cat("real arulist value present instead (should be TRUE)?", any(r15$sunregion == "penobscotbay"), "\n\n")

cat("=== TEST 16: new $group/$groupedby/$groupby.date columns (2026-08-28 rename/spec addition) ===\n")
cat("has $group, $groupedby, $groupby.date columns?",
    all(c("group", "groupedby", "groupby.date") %in% names(r1)), "\n")
cat("$groupedby is constant, equal to the `groupby` param value used (\"aru.name\", the default)?",
    length(unique(r1$groupedby)) == 1 && unique(r1$groupedby) == "aru.name", "\n")
cat("$groupby.date is constant, equal to the `groupby.date` param value used (\"date.mon\", the default)?",
    length(unique(r1$groupby.date)) == 1 && unique(r1$groupby.date) == "date.mon", "\n")
cat("$group holds actual per-row grouping VALUES (detector names), not the column name \"aru.name\" repeated?",
    !any(r1$group == "aru.name") && any(nzchar(r1$group)), "\n")
print(head(r1[, c("group", "groupedby", "groupby.date")], 5))

## custom groupby - $groupedby should track whatever `groupby` was actually set to
r16 <- batz.generate_plotframe.bat(test.data, groupby = "serial", dir.load = arulist.dir)
cat("custom groupby = 'serial' -> $groupedby correctly reports 'serial' (not the default 'aru.name')?",
    length(unique(r16$groupedby)) == 1 && unique(r16$groupedby) == "serial", "\n")
cat("$group values now come from $serial, not $aru.name (should differ from TEST 1's $group values in general)?",
    !identical(sort(unique(r16$group)), sort(unique(r1$group))), "\n\n")

cat("\nAll dev-script tests completed.\n")

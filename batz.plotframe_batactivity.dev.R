# batz.plotframe_batactivity.dev.R
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
#     This function just requires the column to already be present in
#     `data` - it doesn't do any joining itself. Flagging so it's clear
#     this function expects a FULLY ASSEMBLED input (vetted data + a
#     call-datetime column + a sunregion join), not raw output from any
#     one upstream function by itself.
#
#  3. **Parameter renamed: "date" -> "date.groupby".** The spec's Optional
#     Inputs literally name this parameter "date" (default $date.mon) -
#     but the input data frame ALSO has its own real $date column (the
#     raw recording date, distinct from $date.mon). Naming the parameter
#     "date" would be genuinely confusing (and shadows R's own base
#     `date()` function) when the whole point of this parameter is "which
#     column to group by," exactly like the sibling parameter
#     "aru.groupby" already is. Renamed to "date.groupby" to match that
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
#     given by the date.groupby column (default $date.mon), and
#     mins2.noon = minutes from that noon anchor to $call.datetime (the
#     given literal spec formula, "$call.datetime - $date.mon", is read
#     using whatever column date.groupby/call.datetime actually point
#     at). Reused
#     batz.datawrangler_call.datetime()'s own date-format auto-detection
#     to parse the date.groupby column robustly (it may not be in a fixed
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
#     directly (`plfr.batsummary <- batz.plotframe_batactivity(data)`),
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
#     plfr.batsummary, placed right after $aru.groupby.** $sunregion was
#     already a REQUIRED input column (assumption #2 above) but was
#     validated and then silently dropped - never appearing in the
#     output. Collapsed the same way as every other summary column, one
#     value per spp.id/date.groupby/aru.groupby group. Since $sunregion
#     is expected to be constant for a given detector (joined in via
#     $aru.name upstream - assumption #2), each group is checked for
#     internal consistency rather than just taking the first value: a
#     group containing more than one distinct $sunregion value (e.g. if
#     aru.groupby is overridden to a column, like "serial", that doesn't
#     line up 1:1 with $sunregion the way $aru.name does) now stops with
#     a message naming the exact group and the conflicting values,
#     instead of silently picking one. See TEST 9/TEST 10 below.
# ---------------------------------------------------------------------------

## ===========================================================================
## SECTION 0: dependency (batz.datawrangler_call.datetime, reused for
## robust date-format auto-detection of the date.groupby column - see
## assumption #7 above)
## ===========================================================================
source("/home/claude/datetime_work/batz.datawrangler_call.datetime.R")

## ===========================================================================
## SECTION 1: function under test (same body as the final .R file)
## ===========================================================================
batz.plotframe_batactivity <- function(data,
                                        duplicates.remove = TRUE,
                                        spp.id = "manid.sb",
                                        date.groupby = "date.mon",
                                        aru.groupby = "aru.name",
                                        alldetections = TRUE,
                                        trim.noise = TRUE,
                                        trim.noid = TRUE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  required.headers <- c("filename", "date.mon", "manid", "autoid.kp",
                         "autoid.sb", "lat", "serial", "lon", "aru.name",
                         "date", "time", "call.datetime", "sunregion")
  missing.headers <- setdiff(required.headers, names(data))
  if (length(missing.headers) > 0) {
    stop("`data` is missing required header(s): ",
         paste(missing.headers, collapse = ", "))
  }

  for (colname in c(spp.id, date.groupby, aru.groupby)) {
    if (!(colname %in% names(data))) {
      stop("`", colname, "` (from spp.id/date.groupby/aru.groupby) is not ",
           "a column of `data`.")
    }
  }

  if (duplicates.remove) {
    data <- data[!duplicated(data), , drop = FALSE]
  }

  data$obs <- 1

  ## noon-of-monitoring-night anchor, reusing batz.datawrangler_call.datetime()'s
  ## own date-format auto-detection (see assumption #7)
  noon.anchor <- as.POSIXct(
    batz.datawrangler_call.datetime(date = as.character(data[[date.groupby]]),
                                     time = rep("120000", nrow(data))),
    tz = "UTC")

  ## $call.datetime format auto-detection (see assumption #7b) - regex-guarded
  ## candidate list, same exact-full-string-match pattern already used in
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

  ## $sunregion is expected to be a per-detector attribute (joined in via
  ## $aru.name before this function ever sees `data` - see Details/@param
  ## data), so every row within a single spp.id/date.groupby/aru.groupby
  ## group should already agree on it. Collapsed with a consistency check
  ## rather than silently taking the first value, so a real data problem
  ## (e.g. aru.groupby overridden to a column that doesn't line up 1:1
  ## with $sunregion) surfaces as a clear error instead of a silently
  ## arbitrary pick.
  safe.sunregion <- function(x, group.label) {
    present <- unique(x[!is.na(x) & nzchar(trimws(x))])
    if (length(present) == 0) return(NA_character_)
    if (length(present) > 1) {
      stop("`$sunregion` has more than one distinct value (",
           paste(present, collapse = ", "), ") within a single ",
           "spp.id/date.groupby/aru.groupby group (", group.label, ") - ",
           "sunregion is expected to be constant per aru.groupby.")
    }
    present[[1]]
  }

  build.summary <- function(df, spp.override = NULL) {
    spp.vals <- if (!is.null(spp.override)) rep(spp.override, nrow(df)) else as.character(df[[spp.id]])
    date.vals <- as.character(df[[date.groupby]])
    aru.vals  <- as.character(df[[aru.groupby]])
    sun.vals  <- as.character(df$sunregion)

    key <- paste(spp.vals, date.vals, aru.vals, sep = "\r")
    ag.obs <- tapply(df$obs, key, sum)
    ag.min <- tapply(df$.mins2.noon, key, safe.min)
    ag.max <- tapply(df$.mins2.noon, key, safe.max)

    keys  <- names(ag.obs)
    parts <- strsplit(keys, "\r", fixed = TRUE)

    ## computed per-key (not via tapply) so a stop() from safe.sunregion()
    ## names the actual offending spp.id/date/aru.groupby combination in a
    ## human-readable form (the raw key itself is \r-joined and unreadable
    ## if ever printed)
    group.labels <- vapply(parts, function(p) sprintf("spp.id=%s, date=%s, aru.groupby=%s", p[1], p[2], p[3]), character(1))
    names(group.labels) <- keys
    ag.sun <- vapply(keys, function(k) safe.sunregion(sun.vals[key == k], group.label = group.labels[[k]]),
                      character(1))

    out <- data.frame(
      spp.id         = vapply(parts, `[`, character(1), 1),
      date           = vapply(parts, `[`, character(1), 2),
      aru.groupby    = vapply(parts, `[`, character(1), 3),
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

  ## alldetections = TRUE: per-species breakdown (on all rows, pre-trim -
  ## see assumption #9) + a collapsed "All Detections" summary on the
  ## noise/NoID-trimmed data, combined into one table
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
  plfr.batsummary <- plfr.batsummary[order(plfr.batsummary$aru.groupby,
                                            plfr.batsummary$date,
                                            plfr.batsummary$spp.id), ]
  rownames(plfr.batsummary) <- NULL

  plfr.batsummary
}

## ===========================================================================
## SECTION 2: build a realistic test input by running the ACTUAL,
## already-built batz.vettedacoustics_merge.format() against a real raw
## vetted file, then bolting on the one extra column this new function
## needs beyond that function's own output ($sunregion, joined from the
## real WTG.arulist.csv per assumption #2) - no $call.datetime rename
## step needed since batz.vettedacoustics_merge.format() already outputs
## a column with that exact name (see assumption #1)
## ===========================================================================
source("/home/claude/vettedacoustics_work2/batz.batusa_recode.names.R")
source("/home/claude/vettedacoustics_work2/batz.vettedacoustics_merge.format.R")

vetted.dir <- "/mnt/user-data/uploads/4 Current  test data/05.08.2026_05.27.2026/Vetted"
res <- batz.vettedacoustics_merge.format(dir.load = vetted.dir,
                                          load.pattern = "*FinalVetted.csv",
                                          trim.noise = FALSE)   # keep NOISE rows in this test set on purpose
test.data <- res$vetted.merged

arulist <- read.csv("/mnt/user-data/uploads/4 Current  test data/WTG.arulist.csv",
                     stringsAsFactors = FALSE, check.names = FALSE)
test.data$sunregion <- arulist$sunregion[match(test.data$aru.name, arulist$aru)]

cat("Built test.data:", nrow(test.data), "rows,", ncol(test.data), "cols\n")
print(names(test.data))
cat("any NA sunregion?", any(is.na(test.data$sunregion)), "\n\n")

## ===========================================================================
## SECTION 3: tests
## ===========================================================================
cat("=== TEST 1: defaults (spp.id = manid.sb, alldetections = TRUE) ===\n")
r1 <- batz.plotframe_batactivity(test.data)
print(head(r1, 8))
cat("n rows total:", nrow(r1), "\n")
cat("has 'All Detections' rows?", any(r1$spp.id == "All Detections"), "\n")
cat("$vetting.type unique:", paste(unique(r1$vetting.type), collapse=", "), "\n")
cat("mins2.noon range:", range(c(r1$mins2.noon.min, r1$mins2.noon.max), na.rm = TRUE), "(sanity: should be within roughly 0-1440)\n\n")

cat("=== TEST 2: alldetections = FALSE (species breakdown only) ===\n")
r2 <- batz.plotframe_batactivity(test.data, alldetections = FALSE)
print(head(r2, 5))
cat("has 'All Detections' rows (should be FALSE)?", any(r2$spp.id == "All Detections"), "\n\n")

cat("=== TEST 3: custom spp.id = 'manid.kp', custom aru.groupby = 'serial' ===\n")
r3 <- batz.plotframe_batactivity(test.data, spp.id = "manid.kp", aru.groupby = "serial")
print(head(r3, 5))
cat("$vetting.type:", unique(r3$vetting.type), "\n\n")

cat("=== TEST 4: missing-header error ===\n")
bad.data <- test.data[, setdiff(names(test.data), c("sunregion", "lat"))]
tryCatch(batz.plotframe_batactivity(bad.data),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 5: invalid spp.id column ===\n")
tryCatch(batz.plotframe_batactivity(test.data, spp.id = "not.a.real.column"),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("=== TEST 6: trim.noise/trim.noid on a bigger, real dataset with actual NOISE/blank rows ===\n")
res.big <- batz.vettedacoustics_merge.format(dir.load = "/mnt/user-data/uploads/4 Current  test data",
                                              load.pattern = "*FinalVetted.csv", dir.sub = FALSE,
                                              trim.noise = FALSE, bat.names = "common")
big.data <- res.big$vetted.merged
big.data$sunregion <- "test.region"   # FinalVetted.csv's ARUs aren't in WTG.arulist.csv - stub for this test only
r6.notrim  <- batz.plotframe_batactivity(big.data, trim.noise = FALSE, trim.noid = FALSE)
r6.trim    <- batz.plotframe_batactivity(big.data, trim.noise = TRUE,  trim.noid = TRUE)
all.notrim <- sum(r6.notrim[r6.notrim$spp.id == "All Detections", "obs"])
all.trim   <- sum(r6.trim[r6.trim$spp.id == "All Detections", "obs"])
species.noise.count <- sum(r6.notrim[tolower(r6.notrim$spp.id) == "noise" & r6.notrim$spp.id != "All Detections", "obs"])
cat("All Detections total, no trim:", all.notrim, " | with trim:", all.trim,
    " | difference:", all.notrim - all.trim, " | actual NOISE rows in per-species table:", species.noise.count, "\n")
cat("noise still present in per-species breakdown even when trimmed for All Detections?",
    any(tolower(r6.trim$spp.id[r6.trim$spp.id != "All Detections"]) == "noise"), "\n\n")

cat("=== TEST 7: duplicates.remove ===\n")
dup.data <- rbind(test.data, test.data[1:5, ])
r7.dedup   <- batz.plotframe_batactivity(dup.data, duplicates.remove = TRUE)
r7.nodedup <- batz.plotframe_batactivity(dup.data, duplicates.remove = FALSE)
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
r8 <- batz.plotframe_batactivity(small, spp.id = "autoid.sb")
print(r8)

cat("\n=== TEST 9: $sunregion is carried through into plfr.batsummary ===\n")
cat("has $sunregion column?", "sunregion" %in% names(r1), "\n")
print(head(r1[, c("aru.groupby", "sunregion")], 8))
## cross-check: for defaults (aru.groupby = aru.name), every output row's
## $sunregion should match the arulist join used to build test.data
check.sun <- merge(r1, unique(test.data[, c("aru.name", "sunregion")]),
                    by.x = "aru.groupby", by.y = "aru.name", suffixes = c("", ".expected"))
cat("any mismatch vs arulist join?", any(check.sun$sunregion != check.sun$sunregion.expected), "\n")
cat("any NA sunregion in output (should be FALSE - test.data's join left none blank)?",
    any(is.na(r1$sunregion)), "\n\n")

cat("=== TEST 10: inconsistent $sunregion within one spp.id/date.groupby/aru.groupby group stops with a clear error ===\n")
conflict.data <- test.data
dup.row <- conflict.data[1, , drop = FALSE]
dup.row$sunregion <- paste0(dup.row$sunregion, "_CONFLICT")   # same spp.id/date.mon/aru.name as row 1, different sunregion
conflict.data <- rbind(conflict.data, dup.row)
tryCatch(batz.plotframe_batactivity(conflict.data),
         error = function(e) cat("Correctly errored:", conditionMessage(e), "\n\n"))

cat("\nAll dev-script tests completed.\n")

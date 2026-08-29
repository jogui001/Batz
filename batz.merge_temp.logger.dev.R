# batz.merge_temp.logger.dev.R
#
# DEV / TEST VERSION - Batz project
#
# Family:  batz.templogger_*        (functions that work on raw datalogger
#                                     "*templog.csv" exports)
# Action:  merge.format             (merge many logger files into one sheet,
#                                     standardize columns - NO RH calc)
#
# This is the procedural script used to develop and test the logic against
# real sample files before it gets wrapped into the reusable function
# batz.merge_temp.logger() (see batz.merge_temp.logger.R).
#
# UPDATE (latest, per Josh): RH is back, but now strictly gated on the
# matched templog.meta.csv $logger.type - not on header text or any other
# heuristic:
#   - logger.type == "H": $temp.wet.c keeps the real column-4 reading, and
#     $rh is calculated from dry/wet bulb.
#   - logger.type == "T": $temp.wet.c and $rh are both forced to NA, even if
#     the raw file has a second temperature column.
#   - no confident meta match (missing, ambiguous, or no meta.csv at all):
#     treated the same as "T" (both NA) - see assumption #4 below.
#
# The meta join itself uses ONLY $serial# (the file's real 8-digit serial)
# plus the file's own observed [$date.start, $date.end] to pick the right
# meta row - $serial.short and $logger.type are never used to help find or
# disambiguate the match (only to fall back to the same $serial.short logic
# used by an older version of this script when a meta file was written
# before $serial# was added -- see lookup.meta()). Once a single confident
# meta row is found, every OTHER column from that row (serial.short,
# date.deployment, date.recovery, room.number, room.name, station.code,
# logger.type) is appended to every record from that file in
# $templog.merged - not just used internally.
#
# ---------------------------------------------------------------------------
# ASSUMPTIONS MADE (spec was ambiguous on these - flag for review):
#
#  1. "$temp.dry.c" is always column 3; "$temp.wet.c" is column 4 whenever
#     $logger.type == "H" (see above).
#
#  2. "Look up earliest date...assign to date.end" in the spec is treated as
#     a copy-paste of the date.start step; date.end = MAX(date.time), i.e.
#     the latest timestamp, which is clearly the intent.
#
#  3. Blank-row cleanup ("if the lower rows in column 3 and 4 are blank,
#     remove those rows") is applied to ANY row (not just trailing rows)
#     where both temp columns are blank, since real files have this issue
#     at the start too (logger "Started" bookkeeping rows).
#
#  4. RH formula: no formula was specified, so this uses the standard Sprung
#     psychrometer equation with the Alduchov-Eskridge (1996) Magnus
#     approximation for saturation vapor pressure, at sea-level pressure
#     (1013.25 hPa) and a non-ventilated psychrometer coefficient
#     (0.000662 /degC) - see calc.rh().
#
#  5. Files with no confident meta match (no meta.csv, no matching serial#,
#     or an ambiguous match across overlapping deployment periods) are
#     treated like a "T" logger: $temp.wet.c and $rh both NA, rather than
#     guessing from the raw column 4 value or header text. This was not
#     spelled out in the instructions, only the H and T cases were.
#
#  6. Deployment/recovery window trim (per Josh): once a meta row is
#     matched, any record whose $date.time falls before
#     $date.deployment+$time.deployment or after
#     $date.recovery+$time.recovery is dropped from $templog.merged. Only
#     runs when the matched meta row has BOTH date and time columns for
#     deployment and recovery - meta files with date-only columns, or files
#     with no confident meta match at all, are left untrimmed. Trimmed
#     count per file is in $templog.notes$rows.trimmed, and (when > 0) also
#     called out in $templog.notes$notes.
#
#  7. **Standardized (2026-08-29, per Josh) - one default changed, no other
#     behavior change.** dir.save's default changed from dir.load to
#     getwd() - defaulting to dir.load silently mirrored whatever dir.load
#     was rather than defaulting to a sensible location on its own;
#     flagged as a bug and fixed (same standardization applied to
#     batz.suntimes_generate()). See the "dir.save" config note further
#     below for the original (now superseded-by-default) history.
# ---------------------------------------------------------------------------

## base R only - no package dependencies required

## ---- helper: rbind-and-fill (base R stand-in for dplyr::bind_rows) ------
rbind.fill <- function(a, b) {
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)
  all.cols <- union(names(a), names(b))
  for (col in setdiff(all.cols, names(a))) a[[col]] <- NA
  for (col in setdiff(all.cols, names(b))) b[[col]] <- NA
  rbind(a[all.cols], b[all.cols])
}

## ---- config for this test run -------------------------------------------
## NOTE (2026-08-19, per Josh): renamed from "path" to "dir.load" to match
## the parameter name used by batz.datawrangler_load.files - same concept
## (directory to search/load from), should use the same name across functions.
##
## NOTE (2026-08-20, per Josh): cross-function optional-input naming pass -
## "subdirectory" renamed to "dir.sub" (default flipped TRUE -> FALSE, to
## match the new project-wide default), and two new inputs added:
## "load.pattern" (the *templog.csv / *templog.meta.csv suffix patterns,
## previously hardwired) and "dir.save" (separate output-write location,
## defaults to dir.load). See pattern.regex() below for how load.pattern is
## turned into an actual list.files() regex (base R's utils::glob2rx()).
##
## dir.sub is set TRUE here (not the new FALSE default) because the real
## test data keeps templog.meta.csv inside a "meta files/" subfolder - with
## the new default this test run would silently find 0 meta files.
dir.load     <- "/root/batz_test"
load.pattern <- c("*templog.csv", "*templog.meta.csv")
dir.sub      <- TRUE

## NOTE (2026-08-29, per Josh - bugfix/standardization): dir.save's default
## changed from dir.load to getwd() - defaulting to dir.load silently
## mirrored whatever dir.load was rather than defaulting to a sensible
## location on its own; flagged as a bug and fixed (same standardization
## applied to batz.suntimes_generate()).
dir.save     <- getwd()

## convert a plain wildcard/glob suffix pattern (or vector of them) into one
## combined regex suitable for list.files()'s pattern= argument
pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

## ---- helper: strip UTF-8 BOM and read raw lines --------------------------
read.raw.lines <- function(file) {
  ## check the first 3 raw bytes for a UTF-8 BOM (EF BB BF) and skip them
  raw3 <- readBin(file, what = "raw", n = 3)
  has.bom <- length(raw3) == 3 &&
             identical(as.integer(raw3), c(0xEFL, 0xBBL, 0xBFL))
  con <- file(file, open = "rb")
  if (has.bom) readBin(con, what = "raw", n = 3)
  raw.all <- readBin(con, what = "raw", n = file.info(file)$size)
  close(con)
  txt <- rawToChar(raw.all, multiple = FALSE)
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  lines
}

## ---- helper: F -> C -------------------------------------------------------
f.to.c <- function(temp.f) (temp.f - 32) * 5 / 9

## ---- helper: saturation vapor pressure (hPa), Magnus-Tetens/AE1996 ------
sat.vapor.pressure <- function(temp.c) {
  6.1094 * exp((17.625 * temp.c) / (temp.c + 243.04))
}

## ---- helper: RH (%) from dry/wet bulb via Sprung psychrometer eqn -------
calc.rh <- function(temp.dry.c, temp.wet.c,
                     pressure.hpa = 1013.25,
                     psychrometer.coeff = 0.000662) {
  es.wet <- sat.vapor.pressure(temp.wet.c)
  e      <- es.wet - psychrometer.coeff * pressure.hpa * (temp.dry.c - temp.wet.c)
  es.dry <- sat.vapor.pressure(temp.dry.c)
  rh <- 100 * e / es.dry
  pmin(pmax(rh, 0), 100)
  # NOTE: kept at full precision here. Rounding (3 dp) is applied only at
  # the final CSV-write step below, never to intermediate values, so that
  # $rh stays at full precision for any further calculations.
}

## ===========================================================================
## STEP 1: load templog.meta.csv file(s), merge + de-dup
## ===========================================================================
meta.files <- list.files(dir.load, pattern = pattern.regex(load.pattern[2]),
                          recursive = dir.sub, full.names = TRUE)

if (length(meta.files) > 0) {
  meta.list <- lapply(meta.files, read.csv, stringsAsFactors = FALSE,
                       colClasses = "character", check.names = FALSE)
  templog.meta <- Reduce(rbind.fill, meta.list)
  templog.meta <- templog.meta[!duplicated(templog.meta), ]
} else {
  templog.meta <- data.frame(serial.num = character(0),
                              station.code = character(0),
                              stringsAsFactors = FALSE)
}

## if $logger.type doesn't exist, derive it from the last letter of
## $station.code (T = temp only, H = temp + humidity)
if (nrow(templog.meta) > 0 && !"logger.type" %in% names(templog.meta)) {
  templog.meta$logger.type <- toupper(substr(templog.meta$station.code,
                                               nchar(templog.meta$station.code),
                                               nchar(templog.meta$station.code)))
}

## --- meta matching setup ---------------------------------------------------
## Preferred path: templog.meta.csv has a real 8-digit serial number column
## (seen as "serial#" in practice) - match a file's serial.num to it EXACTLY.
## The same physical logger gets redeployed to different stations over time,
## so a serial# can legitimately appear more than once; disambiguate using
## the file's own observed date range against each candidate row's
## [$date.deployment, $date.recovery] window.
##
## Fallback path: older/partial meta files only have $serial.short, which is
## just the last 3-4 digits of the real serial (and may carry stray
## characters like "7273+*"). Match a file's serial.num by comparing its
## last 4 digits first, falling back to the last 3 only when that 3-digit
## value isn't also a substring of some 4-digit code in the table (a 3-digit
## reading could otherwise just be a truncated 4-digit one - unresolvable,
## so it's skipped rather than guessed at).
serial.col <- (if ("serial#" %in% names(templog.meta)) "serial#"
               else if ("serial.num" %in% names(templog.meta)) "serial.num"
               else NA_character_)

if (nrow(templog.meta) > 0 && is.na(serial.col) && "serial.short" %in% names(templog.meta)) {
  templog.meta$serial.short.clean <- gsub("[^0-9]", "", templog.meta$serial.short)
  all.4digit <- unique(templog.meta$serial.short.clean[nchar(templog.meta$serial.short.clean) == 4])
  ambiguous.3digit <- unique(templog.meta$serial.short.clean[
    nchar(templog.meta$serial.short.clean) == 3 &
    sapply(templog.meta$serial.short.clean, function(d) any(grepl(d, all.4digit, fixed = TRUE)))
  ])
} else {
  ambiguous.3digit <- character(0)
}

## if >1 meta row is a candidate (after date filtering), it's genuinely
## ambiguous - return no row (caller treats that like "no match") and
## surface all distinct candidate stations in the notes rather than
## silently picking one
resolve.match <- function(match.rows, match.desc) {
  stations <- unique(match.rows$station.code)
  if (length(stations) == 1) {
    list(meta.row = match.rows[1, , drop = FALSE],
         meta.notes = paste0("meta match ", match.desc, ": ", stations))
  } else {
    list(meta.row = NULL,
         meta.notes = paste0("meta match ambiguous ", match.desc, ": could be ",
                              paste(stations, collapse = " or "), " - check templog.meta.csv"))
  }
}

## restrict candidate meta rows to ones whose deployment window overlaps the
## file's own observed date range (when both are available); if that leaves
## nothing (e.g. dates missing/unparseable), fall back to the full candidate set
filter.by.date <- function(match.rows, date.start, date.end) {
  if (nrow(match.rows) <= 1 || is.na(date.start) || is.na(date.end) ||
      !all(c("date.deployment", "date.recovery") %in% names(match.rows))) {
    return(match.rows)
  }
  dep <- as.Date(match.rows$date.deployment, format = "%m/%d/%Y")
  rec <- as.Date(match.rows$date.recovery, format = "%m/%d/%Y")
  f.start <- as.Date(date.start)
  f.end   <- as.Date(date.end)
  keep <- !is.na(dep) & !is.na(rec) & !(f.end < dep | f.start > rec)
  if (any(keep)) match.rows[keep, , drop = FALSE] else match.rows
}

## column names to append from a matched meta row - everything except
## whatever the join actually used to find it (serial# duplicates
## $serial.num already; the internal .clean helper isn't real meta data)
meta.append.cols <- setdiff(names(templog.meta), c(serial.col, "serial.short.clean"))

## look up the single matching meta row (if any) + a diagnostic note, for
## one file's serial.num / observed date range
lookup.meta <- function(serial.num, date.start, date.end) {
  no.match <- list(meta.row = NULL, meta.notes = NA_character_)
  if (nrow(templog.meta) == 0 || is.na(serial.num)) return(no.match)

  if (!is.na(serial.col)) {
    match.rows <- templog.meta[templog.meta[[serial.col]] == serial.num, ]
    if (nrow(match.rows) == 0) {
      return(list(meta.row = NULL,
                  meta.notes = "no meta match found (serial# not in templog.meta.csv)"))
    }
    match.rows <- filter.by.date(match.rows, date.start, date.end)
    return(resolve.match(match.rows, paste0("on serial# (", serial.num, ")")))
  }

  ## fallback: serial.short suffix matching (only used when the meta file
  ## has no real serial# column at all)
  if (!"serial.short" %in% names(templog.meta)) return(no.match)
  last4 <- substr(serial.num, nchar(serial.num) - 3, nchar(serial.num))
  last3 <- substr(serial.num, nchar(serial.num) - 2, nchar(serial.num))

  match.rows <- templog.meta[templog.meta$serial.short.clean == last4, ]
  if (nrow(match.rows) > 0) {
    match.rows <- filter.by.date(match.rows, date.start, date.end)
    return(resolve.match(match.rows, paste0("on last 4 digits (", last4, ")")))
  }
  if (last3 %in% ambiguous.3digit) {
    return(list(meta.row = NULL,
                meta.notes = paste0("meta match skipped: last 3 digits (", last3,
                                     ") ambiguous with a 4-digit serial.short")))
  }
  match.rows <- templog.meta[templog.meta$serial.short.clean == last3, ]
  if (nrow(match.rows) > 0) {
    match.rows <- filter.by.date(match.rows, date.start, date.end)
    return(resolve.match(match.rows, paste0("on last 3 digits (", last3, ")")))
  }
  list(meta.row = NULL, meta.notes = "no meta match found")
}

cat("Loaded", length(meta.files), "meta file(s),",
    nrow(templog.meta), "unique station row(s) after de-dup\n\n")

## ===========================================================================
## STEP 2: set up templog.notes and the merged output frame
## ===========================================================================
templog.notes <- data.frame(
  serial.num   = character(0),
  date.start   = character(0),
  date.end     = character(0),
  temp.type    = character(0),
  rows.in      = integer(0),
  rows.out     = integer(0),
  rows.trimmed = integer(0),
  notes        = character(0),
  stringsAsFactors = FALSE
)

templog.merged <- data.frame(
  obs         = integer(0),
  date.time   = as.POSIXct(character(0)),
  temp.dry.c  = numeric(0),
  temp.wet.c  = numeric(0),
  rh          = numeric(0),
  serial.num  = character(0),
  stringsAsFactors = FALSE
)

## ===========================================================================
## STEP 3: find all *templog.csv files (excluding the meta files themselves)
## ===========================================================================
all.candidates <- list.files(dir.load, pattern = pattern.regex(load.pattern[1]),
                              recursive = dir.sub, full.names = TRUE)
templog.files <- all.candidates[!grepl(pattern.regex(load.pattern[2]), all.candidates)]

cat("Found", length(templog.files), "templog.csv file(s) to process\n\n")

## ===========================================================================
## STEP 4: process each file
## ===========================================================================
for (f in templog.files) {

  fname <- basename(f)
  cat("Processing:", fname, "\n")

  ## --- a. serial number from filename (8-digit code at the start) --------
  serial.num <- regmatches(fname, regexpr("^[0-9]{8}", fname))
  if (length(serial.num) == 0 || serial.num == "") serial.num <- NA_character_

  ## --- raw read: let read.csv's real quote-aware parser handle each line.
  ##     (some header/title rows have embedded commas inside quoted fields,
  ##     e.g. "Temp, degC (LGR S/N: X, SEN S/N: X)" - a naive comma-split
  ##     misaligns those; row lengths also vary across rows, so fill=TRUE) --
  raw.lines <- read.raw.lines(f)
  raw.lines <- raw.lines[nzchar(raw.lines)]   # drop fully empty lines
  df <- read.csv(text = raw.lines, header = FALSE, colClasses = "character",
                 fill = TRUE, quote = "\"", strip.white = TRUE,
                 stringsAsFactors = FALSE)
  names(df) <- paste0("V", seq_len(ncol(df)))

  ## --- b. keep only columns 1-4, note how many were trimmed --------------
  col.notes <- NA_character_
  if (ncol(df) > 4) {
    n.trim <- ncol(df) - 4
    df <- df[, 1:4]
    col.notes <- paste0("trimmed ", n.trim, " excess columns")
  }
  names(df) <- c("V1", "V2", "V3", "V4")

  ## --- c. rows.in ----------------------------------------------------------
  rows.in <- nrow(df)

  ## --- d. junk row cleanup -------------------------------------------------
  ## remove "Plot Title:*" row
  if (nrow(df) > 0 && grepl("^Plot Title", df$V1[1], ignore.case = TRUE)) {
    df <- df[-1, , drop = FALSE]
  }
  ## remove rows where columns 3 & 4 are BOTH blank
  is.blank <- function(x) is.na(x) | trimws(x) == ""
  df <- df[!(is.blank(df$V3) & is.blank(df$V4)), , drop = FALSE]
  row.names(df) <- NULL

  ## --- e. rows.out ----------------------------------------------------------
  rows.out <- nrow(df)

  ## --- f/g. header detection + temp unit detection --------------------------
  header.notes <- NA_character_
  has.header <- nrow(df) > 0 && grepl("date", df$V1[1], ignore.case = TRUE) ||
                (nrow(df) > 0 && any(grepl("date", as.character(df[1, ]), ignore.case = TRUE)))

  if (!has.header) {
    header.notes <- "no header present"
    temp.type <- "C.default"
  } else {
    header.row <- as.character(df[1, ])
    deg <- "°"
    if (any(grepl(paste0(deg, "f|\\bF\\b|fahrenheit"), header.row, ignore.case = TRUE))) {
      temp.type <- "F"
    } else if (any(grepl(paste0(deg, "c|\\bC\\b|celsius"), header.row, ignore.case = TRUE))) {
      temp.type <- "C"
    } else {
      temp.type <- "C.default"
    }
    df <- df[-1, , drop = FALSE]  # drop header row from the data itself
    row.names(df) <- NULL
  }

  ## --- parse date/time -------------------------------------------------------
  ## Each file uses ONE consistent timestamp format throughout, but that format
  ## varies across files (2 vs 4 digit year, 12 vs 24 hour, with/without
  ## seconds). Detect the exact format from the first valid sample rather than
  ## trial-and-error across formats - trying "%Y" (4-digit year) against a
  ## 2-digit-year string silently "succeeds" (strptime treats "23" as the
  ## literal year 23 AD instead of failing), which corrupts dates.
  build.dt.format <- function(sample) {
    has.ampm  <- grepl("(AM|PM)$", sample, ignore.case = TRUE)
    date.part <- sub(" .*$", "", sample)
    year.4digit <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", date.part)
    date.fmt  <- if (year.4digit) "%m/%d/%Y" else "%m/%d/%y"
    time.part <- sub("^[^ ]+ ", "", sample)
    time.part <- sub(" (AM|PM)$", "", time.part, ignore.case = TRUE)
    n.colons  <- lengths(regmatches(time.part, gregexpr(":", time.part)))
    if (has.ampm) {
      time.fmt <- if (n.colons >= 2) "%I:%M:%S %p" else "%I:%M %p"
    } else {
      time.fmt <- if (n.colons >= 2) "%H:%M:%S" else "%H:%M"
    }
    paste(date.fmt, time.fmt)
  }
  parse.dt <- function(x) {
    valid <- which(!is.na(x) & nzchar(trimws(x)))
    if (length(valid) == 0) return(as.POSIXct(rep(NA_character_, length(x))))
    fmt <- build.dt.format(x[valid[1]])
    as.POSIXct(x, format = fmt, tz = "America/New_York")
  }
  ## combine a templog.meta.csv $date.deployment/$date.recovery ("m/d/Y")
  ## with its companion $time.deployment/$time.recovery ("H:M:S") into one
  ## POSIXct. Returns NA if either piece is missing/blank/unparseable.
  parse.meta.datetime <- function(date.str, time.str) {
    if (is.null(date.str) || is.null(time.str) ||
        is.na(date.str) || is.na(time.str) ||
        !nzchar(trimws(date.str)) || !nzchar(trimws(time.str))) {
      return(as.POSIXct(NA))
    }
    as.POSIXct(paste(date.str, time.str), format = "%m/%d/%Y %H:%M:%S",
               tz = "America/New_York")
  }
  date.time <- parse.dt(df$V2)

  ## --- h. date.start / date.end (min / max) --------------------------------
  date.start <- if (all(is.na(date.time))) NA else format(min(date.time, na.rm = TRUE))
  date.end   <- if (all(is.na(date.time))) NA else format(max(date.time, na.rm = TRUE))

  ## --- meta lookup: matched row's columns get appended to every record for
  ##     this file below; $logger.type from it drives the RH decision -------
  meta.result <- lookup.meta(serial.num, date.start, date.end)
  meta.row    <- meta.result$meta.row
  meta.notes  <- meta.result$meta.notes

  ## --- i2. trim records outside the matched unit's deployment/recovery
  ##     window (date AND time). Only applied when the matched meta row
  ##     carries full date+time columns for both deployment and recovery -
  ##     meta files with date-only columns, or files with no confident meta
  ##     match at all, are left untrimmed. ------------------------------------
  rows.trimmed <- 0L
  if (!is.null(meta.row) &&
      all(c("date.deployment", "time.deployment", "date.recovery", "time.recovery") %in% names(meta.row))) {
    deploy.dt  <- parse.meta.datetime(meta.row$date.deployment[1], meta.row$time.deployment[1])
    recover.dt <- parse.meta.datetime(meta.row$date.recovery[1], meta.row$time.recovery[1])
    if (!is.na(deploy.dt) && !is.na(recover.dt)) {
      ## keep records with an unparseable date.time too (can't judge them
      ## against the window, so don't silently drop them here)
      keep <- is.na(date.time) | (date.time >= deploy.dt & date.time <= recover.dt)
      rows.trimmed <- sum(!keep)
      if (rows.trimmed > 0) {
        df <- df[keep, , drop = FALSE]
        date.time <- date.time[keep]
        row.names(df) <- NULL
      }
    }
  }
  window.notes <- if (rows.trimmed > 0) {
    paste0("trimmed ", rows.trimmed, " record", if (rows.trimmed == 1) "" else "s",
           " outside deployment/recovery window")
  } else NA_character_

  ## --- i. combine *.notes vars into one notes string -----------------------
  notes.parts <- c(col.notes, header.notes, meta.notes, window.notes)
  notes.parts <- notes.parts[!is.na(notes.parts)]
  notes <- if (length(notes.parts) == 0) NA_character_ else paste(notes.parts, collapse = "; ")

  ## --- j. append row to templog.notes ---------------------------------------
  templog.notes <- rbind.fill(templog.notes, data.frame(
    serial.num = serial.num, date.start = date.start, date.end = date.end,
    temp.type = temp.type, rows.in = rows.in, rows.out = rows.out,
    rows.trimmed = rows.trimmed, notes = notes, stringsAsFactors = FALSE
  ))

  ## --- k. add serial.num column; numeric temps ------------------------------
  temp.col3 <- suppressWarnings(as.numeric(df$V3))
  temp.col4 <- suppressWarnings(as.numeric(df$V4))

  ## --- l. F -> C conversion if needed ---------------------------------------
  if (identical(temp.type, "F")) {
    temp.col3 <- f.to.c(temp.col3)
    temp.col4 <- f.to.c(temp.col4)
  }

  temp.dry.c <- temp.col3

  ## --- RH gated strictly on the matched meta row's $logger.type: H keeps
  ##     the wet-bulb reading and gets RH calculated; T (or no confident
  ##     meta match at all) gets NA for both -----------------------------
  logger.type <- if (!is.null(meta.row) && "logger.type" %in% names(meta.row)) meta.row$logger.type[1] else NA_character_
  if (identical(logger.type, "H")) {
    temp.wet.c <- temp.col4
    rh <- calc.rh(temp.dry.c, temp.wet.c)
  } else {
    temp.wet.c <- NA_real_
    rh <- NA_real_
  }

  ## --- append the REMAINING meta columns (everything but the join key) to
  ##     every record from this file, defaulting to NA when there's no
  ##     confident single meta match ------------------------------------
  ## BUGFIX (2026-08-19, found by running on real hobotemp/*templog.csv test
  ## data): these were left as length-1 scalars, relying on data.frame()'s
  ## own recycling. That works fine when nrow(df) >= 1, but data.frame()
  ## cannot recycle a length-1 vector down to 0 rows - so a file whose
  ## deployment/recovery window trimmed away EVERY record crashed the whole
  ## function instead of just contributing 0 rows. Recycle explicitly here.
  meta.extra <- as.list(rep(NA_character_, length(meta.append.cols)))
  names(meta.extra) <- meta.append.cols
  if (!is.null(meta.row)) {
    for (col in meta.append.cols) meta.extra[[col]] <- meta.row[[col]][1]
  }
  meta.extra <- lapply(meta.extra, function(v) rep(v, length.out = nrow(df)))

  ## --- m. append current file to bottom of merged data frame ---------------
  ## BUGFIX (2026-08-19, found by running on real hobotemp/*templog.csv test
  ## data): serial.num and (on the "T"/no-confident-match branch)
  ## temp.wet.c/rh are constant length-1 scalars for the whole file, which
  ## data.frame() can recycle up to any nrow(df) >= 1 but NOT down to 0. A
  ## file whose deployment/recovery window trimmed away every single record
  ## (nrow(df) == 0) used to crash the whole script here. Skipping the append
  ## entirely when there's nothing left to add is a no-op for templog.merged
  ## either way (rbind.fill treats a 0-row frame as a no-op), and
  ## rows.in/rows.out/rows.trimmed/notes for the file are already recorded in
  ## templog.notes above regardless.
  if (nrow(df) > 0) {
    file.df <- do.call(data.frame, c(
      list(obs = seq_len(nrow(df)), date.time = date.time, temp.dry.c = temp.dry.c,
           temp.wet.c = temp.wet.c, rh = rh, serial.num = serial.num),
      meta.extra,
      stringsAsFactors = FALSE
    ))
    templog.merged <- rbind.fill(templog.merged, file.df)
  }

  ## --- remove this file's working variables before next iteration ----------
  ## (deploy.dt/recover.dt/keep are only created on some iterations - not
  ## included here since rm() on a name that was never assigned errors)
  rm(df, raw.lines, col.notes, rows.in, rows.out, rows.trimmed, window.notes,
     header.notes, has.header, temp.type, date.time,
     date.start, date.end, notes.parts, notes, temp.col3, temp.col4,
     meta.result, meta.row, meta.notes, logger.type, meta.extra,
     temp.dry.c, temp.wet.c, rh, file.df, serial.num)
}

cat("\n--- templog.notes ---\n")
print(templog.notes)

cat("\n--- templog.merged summary ---\n")
cat("Total rows:", nrow(templog.merged), "\n")
print(table(templog.merged$serial.num))
print(summary(templog.merged))

# Rounding is applied only here, at the final save step - never to
# intermediate values, and never to templog.merged itself, which stays at
# full precision in case it's used for any further calculations.
templog.merged.out <- templog.merged
templog.merged.out$rh <- round(templog.merged.out$rh, 3)

write.csv(templog.notes, file.path(dir.save, "templog.notes.csv"), row.names = FALSE)
write.csv(templog.merged.out, file.path(dir.save, "templog.merged.csv"), row.names = FALSE)
cat("\nWrote templog.notes.csv and templog.merged.csv to", dir.save, "\n")

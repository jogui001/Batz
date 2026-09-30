# =============================================================================
# batz.summarize_aruloc.coordinates.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.summarize_aruloc.coordinates() - added 2026-09-30, per
# Josh. Requested as "Batz.aruloc_coordinate.summery()"; renamed per Josh to
# the batz.<verb>_<subject>() convention, with "summary" spelled correctly
# (outputs coordinate.summary / coordinate.average).
#
# Standalone: standardize.headers() and canonicalize.headers() are inlined
# below. Keep this file OUTSIDE the package's R/ folder.
#
# FLAGGED:
#  1. aru.combos.csv spells the units "NWSO1/NWSO2/NWSO3" (letter O) but
#     norcross2.csv has "NWS01/NWS02/NWS03" (zero). Not auto-corrected - the
#     function warns and suggests the closest name.
#  2. $records was added to coordinate.summary (the purpose asks for the
#     number of records; the header list didn't include a column for it).
#  3. 0,0 (no GPS fix) records are dropped by default (zero.remove = TRUE).
#  4. "More than 1 day of missing records" = a gap > 24 h between
#     consecutive records at the same location (gap.days = 1).
#  5. NWS03 alternates between one stored position and a fresh nightly GPS
#     fix, so it produces ~74 location records under the exact-match rule.
# =============================================================================

## ---- inlined package helpers (batz.util_standardize.headers.R) ----------
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}
canonicalize.headers <- function(df, required) {
  std.have <- standardize.headers(names(df))
  std.want <- standardize.headers(required)
  idx <- match(std.want, std.have)
  found <- !is.na(idx)
  names(df)[idx[found]] <- required[found]
  list(df = df, missing = required[!found])
}

batz.summarize_aruloc.coordinates <- function(data,
                                              aru.combo,
                                              gap.days    = 1,
                                              zero.remove = TRUE) {

  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.data.frame(aru.combo) || ncol(aru.combo) < 1) stop("aru.combo must be a data frame with at least one column")

  ## ---- header check ------------------------------------------------------
  required <- c("aru.name", "date", "time", "longitude", "latitude")
  ch <- canonicalize.headers(data, required)
  if (length(ch$missing) > 0) {
    stop("data is missing these headers: ", paste(ch$missing, collapse = ", "))
  }
  d <- ch$df[required]
  d$aru.name  <- trimws(as.character(d$aru.name))
  d$longitude <- suppressWarnings(as.numeric(d$longitude))
  d$latitude  <- suppressWarnings(as.numeric(d$latitude))

  ## ---- clean ---------------------------------------------------------------
  bad.xy <- is.na(d$longitude) | is.na(d$latitude)
  if (any(bad.xy)) {
    cat("NOTE:", sum(bad.xy), "record(s) with a missing longitude/latitude dropped.\n")
    d <- d[!bad.xy, , drop = FALSE]
  }
  if (zero.remove) {
    zero <- d$longitude == 0 & d$latitude == 0
    if (any(zero)) {
      cat("NOTE:", sum(zero), "record(s) at 0,0 (no GPS fix) dropped.\n")
      d <- d[!zero, , drop = FALSE]
    }
  }
  d$datetime <- as.POSIXct(paste(trimws(d$date), trimws(d$time)),
                           tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  bad.dt <- is.na(d$datetime)
  if (any(bad.dt)) {
    cat("NOTE:", sum(bad.dt), "record(s) with an unreadable date/time dropped.\n")
    d <- d[!bad.dt, , drop = FALSE]
  }
  if (nrow(d) == 0) stop("no usable records left in data")

  ## ---- location records ----------------------------------------------------
  d <- d[order(d$aru.name, d$datetime), , drop = FALSE]
  n <- nrow(d)
  loc.key   <- paste(d$longitude, d$latitude)
  new.aru   <- c(TRUE, d$aru.name[-1] != d$aru.name[-n])
  new.loc   <- c(TRUE, loc.key[-1] != loc.key[-n])
  gap.secs  <- c(Inf, diff(as.numeric(d$datetime)))
  new.gap   <- gap.secs > gap.days * 86400
  d$loc.id  <- cumsum(new.aru | new.loc | new.gap)

  fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  first <- !duplicated(d$loc.id)
  last  <- !duplicated(d$loc.id, fromLast = TRUE)
  coordinate.summary <- data.frame(
    aru.name       = d$aru.name[first],
    datetime.start = fmt(d$datetime[first]),
    datetime.end   = fmt(d$datetime[last]),
    longitude      = d$longitude[first],
    latitude       = d$latitude[first],
    records        = as.integer(tabulate(d$loc.id)),
    stringsAsFactors = FALSE
  )
  rownames(coordinate.summary) <- NULL
  cat("coordinate.summary:", nrow(coordinate.summary), "location record(s) across",
      length(unique(d$aru.name)), "ARU(s).\n")

  ## ---- median location per set ----------------------------------------------
  sets <- trimws(as.character(aru.combo[[1]]))
  sets <- sets[!is.na(sets) & nzchar(sets)]
  unique.locs <- unique(d[c("aru.name", "longitude", "latitude")])
  aru.all <- unique(d$aru.name)

  med <- lapply(sets, function(s) {
    members <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
    members <- members[nzchar(members)]
    hit <- match(tolower(members), tolower(aru.all))
    if (any(is.na(hit))) {
      for (m in members[is.na(hit)]) {
        close <- aru.all[which.min(utils::adist(tolower(m), tolower(aru.all)))]
        warning("aru.combo name '", m, "' not found in data$aru.name (closest: '",
                close, "') - left out of set '", s, "'", call. = FALSE)
      }
    }
    locs <- unique.locs[unique.locs$aru.name %in% aru.all[hit[!is.na(hit)]], ]
    if (nrow(locs) == 0) return(c(NA_real_, NA_real_))
    c(stats::median(locs$longitude), stats::median(locs$latitude))
  })
  coordinate.average <- data.frame(
    aru.names = sets,
    longitude = vapply(med, `[`, numeric(1), 1),
    latitude  = vapply(med, `[`, numeric(1), 2),
    stringsAsFactors = FALSE
  )

  result <- list(coordinate.summary = coordinate.summary,
                 coordinate.average = coordinate.average)
  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)
  invisible(result)
}

# =============================================================================
# TESTS
# =============================================================================
# Real data: norcross2.csv (NWS01-03, SM5 firmware 1.5) and aru.combos.csv,
# both from Josh 2026-09-30. Point these paths at your own copies.
data.path  <- "/home/claude/aruloc/norcross2.csv"
combo.path <- "/home/claude/aruloc/aru.combos.csv"

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg); cat("[PASS]", msg, "\n") }

data      <- read.csv(data.path)
aru.combo <- read.csv(combo.path)
data.before <- data

cat("\n=== real data, aru.combos.csv as supplied ===\n")
w <- character(0)
r <- withCallingHandlers(batz.summarize_aruloc.coordinates(data, aru.combo),
                         warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
print(head(coordinate.summary, 8)); print(coordinate.average)
ok(identical(names(coordinate.summary), c("aru.name","datetime.start","datetime.end","longitude","latitude","records")), "coordinate.summary columns")
ok(identical(names(coordinate.average), c("aru.names","longitude","latitude")), "coordinate.average columns")
ok(sum(coordinate.summary$records) == nrow(data) - 6, "records add up to all rows minus the 6 at 0,0")
ok(identical(data, data.before), "caller's data not modified")
ok(nrow(coordinate.average) == 3, "one row per aru.combo set (blank row dropped)")
ok(length(w) == 6 && all(grepl("NWSO", w)) && any(grepl("closest: 'NWS01'", w)), "letter-O names (NWSO1...) warned, closest match suggested")
ok(all(is.na(coordinate.average$longitude)), "sets with no matched names -> NA")
ok(table(coordinate.summary$aru.name)[["NWS01"]] == 2 && table(coordinate.summary$aru.name)[["NWS02"]] == 2, "NWS01 and NWS02: 2 location records each")

cat("\n=== real data, names corrected to NWS01/NWS02/NWS03 ===\n")
aru.combo2 <- data.frame(Sets = gsub("NWSO", "NWS0", aru.combo$Sets))
batz.summarize_aruloc.coordinates(data, aru.combo2)
print(coordinate.average, digits = 8)
u <- unique(data[!(data$longitude == 0 & data$latitude == 0), c("aru.name","longitude","latitude")])
ok(isTRUE(all.equal(coordinate.average$longitude[3], median(u$longitude[u$aru.name == "NWS03"]))) &&
   isTRUE(all.equal(coordinate.average$latitude[3],  median(u$latitude[u$aru.name == "NWS03"]))), "NWS03 set = median of NWS03's unique locations")
ok(isTRUE(all.equal(coordinate.average$longitude[1], median(u$longitude))), "all-ARU set = median of all unique locations")

cat("\n=== synthetic: break rules ===\n")
mk <- function(aru, dt, x, y) data.frame(aru.name = aru, date = substr(dt, 1, 10), time = substr(dt, 12, 19), longitude = x, latitude = y)
syn <- rbind(
  mk("A", c("2026-06-01 20:00:00","2026-06-01 21:00:00"), -70, 44),   # loc 1
  mk("A", "2026-06-02 20:00:00", -71, 45),                            # moves -> loc 2
  mk("A", "2026-06-03 20:00:00", -70, 44),                            # back   -> loc 3
  mk("B", c("2026-06-01 20:00:00","2026-06-02 20:00:00"), -72, 43),   # exactly 1 day gap -> same record
  mk("B", "2026-06-05 20:00:00", -72, 43),                            # 3 day gap -> new record
  mk("C", "2026-06-01 20:00:00", 0, 0),                               # no GPS fix
  mk("C", "2026-06-01 21:00:00", -73, 42))
syn <- syn[sample(nrow(syn)), ]                                       # input order must not matter
batz.summarize_aruloc.coordinates(syn, data.frame(Sets = c("A; B", "a", "C", "Z")))
print(coordinate.summary); print(coordinate.average)
cs <- coordinate.summary
ok(sum(cs$aru.name == "A") == 3, "A -> B -> A gives 3 location records")
ok(sum(cs$aru.name == "B") == 2, "gap > 1 day starts a new record; gap of exactly 1 day doesn't")
ok(cs$records[cs$aru.name == "A"][1] == 2 && cs$datetime.start[1] == "2026-06-01 20:00:00" && cs$datetime.end[1] == "2026-06-01 21:00:00", "records / datetime.start / datetime.end")
ok(sum(cs$aru.name == "C") == 1 && cs$longitude[cs$aru.name == "C"] == -73, "0,0 record dropped (zero.remove = TRUE)")
ok(coordinate.average$longitude[1] == median(c(-70, -71, -72)), "set median uses unique locations (A's return to -70 counted once)")
ok(coordinate.average$longitude[2] == -70.5, "names matched ignoring case ('a' -> A)")
ok(is.na(coordinate.average$longitude[4]), "unknown name Z -> NA")
batz.summarize_aruloc.coordinates(syn, data.frame(Sets = "C"), zero.remove = FALSE)
ok(sum(coordinate.summary$aru.name == "C") == 2, "zero.remove = FALSE keeps the 0,0 record")
batz.summarize_aruloc.coordinates(syn, data.frame(Sets = "B"), gap.days = 5)
ok(sum(coordinate.summary$aru.name == "B") == 1, "gap.days = 5 keeps B as one record")

cat("\n=== synthetic: headers ===\n")
syn2 <- syn; names(syn2) <- c("ARU_Name", "Date", "Time", "Longitude", "Latitude")
batz.summarize_aruloc.coordinates(syn2, data.frame(Sets = "A"))
ok(nrow(coordinate.summary) == 6, "snake_case / capitalised headers accepted")
e <- tryCatch(batz.summarize_aruloc.coordinates(syn[c("aru.name","date","longitude")], data.frame(Sets = "A")), error = function(x) conditionMessage(x))
ok(e == "data is missing these headers: time, latitude", "missing headers named in the error")

cat("\nALL TESTS PASSED\n")

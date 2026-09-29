# =============================================================================
# batz.merge_sm5.logfile.dev.R
# -----------------------------------------------------------------------------
# Dev script for batz.merge_sm5.logfile() - added 2026-09-29, per Josh.
# Merges SM5 logs into SM5_1.5 (firmware <=1.5) and SM5_1.6 (firmware >=1.6),
# detected from each file's headers.
# Standalone: the shared engine (batz.util_sm.logfile.R) and
# standardize.headers() are inlined below. Keep this file OUTSIDE the
# package's R/ folder.
# =============================================================================

## ---- helper: standardize.headers (per Josh, 2026-09-14) - inlined, since
## this is a standalone dev script, not part of the package.
standardize.headers <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

sm.logfile.schemas <- function() {
  common <- c("date", "time", "lat", "ns", "lon", "ew", "power_v", "temp_c")
  list(
    "SM4"     = c(common, "files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c(common, "acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c(common, "acfiles", "acl", "acr")
  )
}

sm.logfile.detect.version <- function(hdrs) {
  schemas <- sm.logfile.schemas()
  full <- vapply(schemas, function(s) all(s %in% hdrs), logical(1))
  if (any(full)) {
    cand <- names(schemas)[full]
    return(cand[which.max(lengths(schemas[cand]))])
  }
  distinct <- list(
    "SM4"     = c("files", "scrubbed", "mic0_type"),
    "SM5.1.5" = c("acfiles", "fs1files", "fs2files", "zc1files", "zc2files", "scrub1", "scrub2"),
    "SM5.1.6" = c("acfiles", "acl", "acr")
  )
  ## acfiles alone can't tell SM5.1.5 from SM5.1.6 - score it for neither
  ## on its own; it only tips the balance toward "SM5" when paired with
  ## that version's other distinguishing headers.
  score <- vapply(distinct, function(d) {
    hits <- d %in% hdrs
    if (sum(hits[d != "acfiles"]) == 0) 0 else sum(hits)
  }, numeric(1))
  if (max(score) == 0 || sum(score == max(score)) > 1) return("unknown")
  names(score)[which.max(score)]
}

sm.logfile.merge <- function(dir.load, dir.sub, load.pattern, duplicates.remove,
                             versions.keep, caller.name) {

  schemas <- sm.logfile.schemas()

  pattern.regex <- function(p) paste(vapply(p, utils::glob2rx, character(1)), collapse = "|")

  month.lookup <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
                     jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")

  convert.date <- function(x) {
    m <- regmatches(x, regexpr("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x))
    out <- x
    has.match <- nzchar(m)
    if (any(has.match)) {
      parts <- regmatches(x[has.match], regexec("^([0-9]{4})-([A-Za-z]{3})-([0-9]{2})$", x[has.match]))
      converted <- vapply(parts, function(p) {
        mm <- month.lookup[tolower(p[3])]
        if (is.na(mm)) return(NA_character_)
        paste(p[2], mm, p[4], sep = "-")
      }, character(1))
      out[has.match] <- ifelse(is.na(converted), x[has.match], converted)
    }
    out
  }

  make.log.row <- function(aru.name, filename, filepath, version, load.status, reason,
                            date.start = NA_character_, date.end = NA_character_,
                            date.unique = NA_integer_, date.range = NA_integer_,
                            records = NA_integer_) {
    data.frame(aru.name = aru.name, filename = filename, version = version,
               date.start = date.start, date.end = date.end,
               date.unique = date.unique, date.range = date.range,
               records = records, load.status = load.status, reason = reason,
               filepath = filepath, stringsAsFactors = FALSE)
  }

  other.fn <- function(v) {
    if (v == "SM4") "batz.merge_sm4.logfile() or batz.merge_sm.logfiles()"
    else "batz.merge_sm5.logfile() or batz.merge_sm.logfiles()"
  }

  process.one.file <- function(f) {
    base.name <- basename(f)
    file.aru.name <- sub("_.*$", "", base.name)

    raw <- tryCatch(
      utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      return(list(data = NULL, version = "unknown",
                  log = make.log.row(file.aru.name, base.name, f, "unknown",
                                     "Failure", "could not read file")))
    }

    names(raw) <- standardize.headers(names(raw))
    version <- sm.logfile.detect.version(names(raw))

    if (version == "unknown") {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     "could not identify SM version from headers")))
    }
    if (!(version %in% versions.keep)) {
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure",
                                     paste0(version, " file - not loaded by ", caller.name,
                                            " (use ", other.fn(version), ")"))))
    }

    expected <- schemas[[version]]
    present <- expected %in% names(raw)
    headers.missing <- !all(present)
    no.data <- nrow(raw) == 0

    if (headers.missing || no.data) {
      missing.list <- paste(expected[!present], collapse = ", ")
      reason <- if (headers.missing && no.data) {
        paste0("no data and These headers are missing: ", missing.list)
      } else if (headers.missing) {
        paste0("These headers are missing: ", missing.list)
      } else {
        "no data"
      }
      return(list(data = NULL, version = version,
                  log = make.log.row(file.aru.name, base.name, f, version, "Failure", reason)))
    }

    output <- expected
    output[output == "lon"] <- "longitude"
    tmp <- raw[expected]
    names(tmp) <- output
    for (cn in names(tmp)) if (is.character(tmp[[cn]])) tmp[[cn]] <- trimws(tmp[[cn]])

    tmp$aru.name <- file.aru.name
    tmp$date <- convert.date(tmp$date)

    ns <- tolower(trimws(tmp$ns))
    ew <- tolower(trimws(tmp$ew))
    tmp$Y <- ifelse(ns == "s", -as.numeric(tmp$lat), as.numeric(tmp$lat))
    tmp$X <- ifelse(ew == "w", -as.numeric(tmp$longitude), as.numeric(tmp$longitude))

    tmp <- tmp[c("aru.name", output, "X", "Y")]

    date.vals <- tmp$date
    date.unique.n <- length(unique(date.vals))
    iso.ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date.vals) &
      !is.na(suppressWarnings(as.Date(date.vals, format = "%Y-%m-%d")))
    if (any(iso.ok)) {
      date.start.val <- min(date.vals[iso.ok])
      date.end.val   <- max(date.vals[iso.ok])
      date.range.val <- as.integer(as.Date(date.end.val) - as.Date(date.start.val)) + 1L
    } else {
      date.start.val <- NA_character_
      date.end.val   <- NA_character_
      date.range.val <- NA_integer_
    }

    list(data = tmp, version = version,
         log = make.log.row(file.aru.name, base.name, f, version, "Success",
                            "All headers present and observation in file",
                            date.start = date.start.val, date.end = date.end.val,
                            date.unique = date.unique.n, date.range = date.range.val,
                            records = nrow(tmp)))
  }

  all.files <- list.files(dir.load, pattern = pattern.regex(load.pattern),
                          recursive = dir.sub, full.names = TRUE, ignore.case = TRUE)

  cat("Scanning", dir.load, "(dir.sub =", dir.sub, ") ...\n")

  pieces   <- stats::setNames(lapply(versions.keep, function(v) list()), versions.keep)
  log.rows <- list()

  if (length(all.files) == 0) {
    cat("No files matching load.pattern found.\n")
  } else {
    for (f in all.files) {
      r <- process.one.file(f)
      log.rows[[length(log.rows) + 1]] <- r$log
      if (is.null(r$data)) {
        cat("  [skipped] ", f, " - ", r$log$reason, "\n", sep = "")
      } else {
        cat("  loaded ", f, " [", r$version, "] (", nrow(r$data), " rows)\n", sep = "")
        pieces[[r$version]][[length(pieces[[r$version]]) + 1]] <- r$data
      }
    }
  }

  merged <- lapply(versions.keep, function(v) {
    p <- pieces[[v]]
    if (length(p) == 0) return(data.frame())
    df <- do.call(rbind, p)
    if (duplicates.remove) {
      dup.mask <- duplicated(df)
      if (any(dup.mask)) {
        cat("\n", sum(dup.mask), " duplicate row(s) removed from ", v, " data.\n", sep = "")
        df <- df[!dup.mask, , drop = FALSE]
      }
    }
    rownames(df) <- NULL
    df
  })
  names(merged) <- versions.keep

  log.df <- if (length(log.rows) > 0) {
    do.call(rbind, log.rows)
  } else {
    make.log.row(character(0), character(0), character(0), character(0),
                 character(0), character(0), character(0), character(0),
                 integer(0), integer(0), integer(0))
  }

  list(data = merged, log = log.df)
}

batz.merge_sm5.logfile <- function(dir.load          = getwd(),
                                   dir.sub           = FALSE,
                                   load.pattern      = c("*_A_Summary*.txt", "*_B_Summary*.txt"),
                                   duplicates.remove = TRUE,
                                   log.file          = FALSE) {

  out <- sm.logfile.merge(dir.load, dir.sub, load.pattern, duplicates.remove,
                          versions.keep = c("SM5.1.5", "SM5.1.6"),
                          caller.name = "batz.merge_sm5.logfile()")

  result <- list(SM5_1.5 = out$data[["SM5.1.5"]],
                 SM5_1.6 = out$data[["SM5.1.6"]])
  if (log.file) result$log.file_sm5 <- out$log

  caller.env <- parent.frame()
  for (nm in names(result)) assign(nm, result[[nm]], envir = caller.env)

  invisible(result)
}

# =============================================================================
# TEST DATA: point test.dir at a folder of SM summary files. The tests
# below were run 2026-09-29 against Josh's real NWS01/NWS02/NWS03
# (SM5 firmware 1.5) files plus synthetic SM4 / SM5.1.6 / bad-header /
# empty / unrecognised files (NWS03 in a subfolder, to test dir.sub).
# =============================================================================
test.dir <- "/home/claude/sm/testdata"
## ---- tests (shared by all three dev scripts) ------------------------------
if (!exists("test.dir")) test.dir <- "/home/claude/sm/testdata"
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg); cat("[PASS]", msg, "\n") }
lg <- function(L, f) L[L$filename == f, ]

if (exists("batz.merge_sm.logfiles")) {
  cat("\n=== batz.merge_sm.logfiles(): real NWS01-03 + synthetic ===\n")
  r <- batz.merge_sm.logfiles(test.dir, dir.sub = TRUE, log.file = TRUE)
  print(log.file_sm[, c("aru.name","filename","version","date.start","date.end","date.unique","date.range","records","load.status")])
  ok(setequal(names(r), c("SM4","SM5_1.5","SM5_1.6","log.file_sm")), "returns SM4, SM5_1.5, SM5_1.6, log.file_sm")
  ok(all(c("SM4","SM5_1.5","SM5_1.6","log.file_sm") %in% ls(globalenv())), "all four auto-assigned")
  ok(nrow(log.file_sm) == 8, "one log row per matched file (8)")
  ok(all(lg(log.file_sm,"NWS01_A_Summary_1.txt")$version == "SM5.1.5",
         lg(log.file_sm,"NWS03_A_Summary_1.txt")$version == "SM5.1.5"), "real NWS files detected as SM5.1.5")
  n.real <- sum(log.file_sm$records[log.file_sm$version == "SM5.1.5"])
  ok(n.real == 27279 + 27283 + 23681, "SM5.1.5 records = all real data rows (78243)")
  ok(nrow(SM5_1.5) <= n.real && nrow(SM5_1.5) > 78000, "SM5_1.5 merged (after duplicate removal)")
  ok(identical(names(SM5_1.5), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c",
       "acfiles","fs1files","fs2files","zc1files","zc2files","scrub1","scrub2","X","Y")), "SM5_1.5 column names")
  ok(is.numeric(SM5_1.5$scrub2) && is.numeric(SM5_1.5$power_v), "trailing-space numeric columns read as numeric")
  ok(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", SM5_1.5$date)), "SM5 dates converted to YYYY-MM-DD")
  ok(all(SM5_1.5$X[SM5_1.5$longitude != 0] < 0), "W longitudes -> negative X")
  ok(setequal(unique(SM5_1.5$aru.name), c("NWS01","NWS02","NWS03")), "aru.name parsed from file names")
  n3 <- lg(log.file_sm,"NWS03_A_Summary_1.txt")
  ok(n3$date.start == "2025-05-29" && n3$date.end == "2026-07-13" && n3$date.range > n3$date.unique, "NWS03 2025 test rows show as a date gap")
  ok(lg(log.file_sm,"AYERS_A_Summary.txt")$version == "SM4" && nrow(SM4) == 2, "SM4 file -> SM4 frame")
  ok(identical(names(SM4), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c","files","scrubbed","mic0_type","X","Y")), "SM4 column names match sm4logs.merged")
  ok(lg(log.file_sm,"NEW16_A_Summary.txt")$version == "SM5.1.6" && nrow(SM5_1.6) == 2 &&
       identical(names(SM5_1.6), c("aru.name","date","time","lat","ns","longitude","ew","power_v","temp_c","acfiles","acl","acr","X","Y")), "SM5.1.6 file -> SM5_1.6 frame")
  b <- lg(log.file_sm,"BAD16_A_Summary.txt")
  ok(b$version == "SM5.1.6" && b$load.status == "Failure" && b$reason == "These headers are missing: temp_c", "partial 1.6 file -> SM5.1.6 Failure, missing temp_c")
  e <- lg(log.file_sm,"EMPTY16_B_Summary.txt")
  ok(e$version == "SM5.1.6" && e$reason == "no data", "empty 1.6 file -> no data")
  o <- lg(log.file_sm,"ODD_A_Summary.txt")
  ok(o$version == "unknown" && o$reason == "could not identify SM version from headers", "unrecognised headers -> unknown")
}

if (exists("batz.merge_sm5.logfile")) {
  cat("\n=== batz.merge_sm5.logfile() ===\n")
  rm(list = intersect(c("SM4","SM5_1.5","SM5_1.6","log.file_sm5"), ls(globalenv())), envir = globalenv())
  r <- batz.merge_sm5.logfile(test.dir, dir.sub = TRUE, log.file = TRUE)
  ok(setequal(names(r), c("SM5_1.5","SM5_1.6","log.file_sm5")) && !exists("SM4", envir = globalenv()), "returns SM5_1.5, SM5_1.6, log.file_sm5 only")
  a <- lg(log.file_sm5,"AYERS_A_Summary.txt")
  ok(a$version == "SM4" && a$load.status == "Failure" && grepl("not loaded by batz.merge_sm5.logfile", a$reason), "SM4 file skipped with version SM4")
  ok(nrow(SM5_1.6) == 2 && nrow(SM5_1.5) > 78000, "both SM5 frames filled")
  r2 <- batz.merge_sm5.logfile(test.dir, dir.sub = TRUE)
  ok(is.null(r2$log.file_sm5), "log.file = FALSE omits the log")
  if (exists("batz.merge_sm.logfiles"))
    ok(exists("log.file_sm") && nrow(log.file_sm) == 8, "log.file_sm (all-units log) not overwritten by the SM5 run")
}

if (exists("batz.merge_sm4.logfile")) {
  cat("\n=== batz.merge_sm4.logfile() ===\n")
  r <- batz.merge_sm4.logfile(test.dir, dir.sub = TRUE, log.file = TRUE)
  L <- log.file_sm4
  ok(identical(names(L), c("aru.name","filename","version","date.start","date.end","date.unique","date.range","records","load.status","reason","filepath")), "log has new $version column after $filename")
  ok(nrow(sm4logs.merged) == 2 && all(sm4logs.merged$aru.name == "AYERS"), "only the SM4 file merged")
  n1 <- lg(L,"NWS01_A_Summary_1.txt")
  ok(n1$version == "SM5.1.5" && n1$load.status == "Failure" && grepl("use batz.merge_sm5.logfile", n1$reason), "SM5 file skipped, version SM5.1.5, reason points to sm5 function")
  ## original SM4 failure-reason behaviour still intact
  d <- file.path(tempdir(), "sm4bad"); dir.create(d, showWarnings = FALSE)
  writeLines(c("DATE,TIME,LAT,NS,LON,EW,POWER(V),#FILES,#SCRUBBED,MIC0 TYPE","2026-Jun-05,20:00:00,44.5,N,70.6,W,12.1,10,0,U2"), file.path(d,"BADHEADER_A_Summary.txt"))
  writeLines("DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#FILES,#SCRUBBED,MIC0 TYPE", file.path(d,"EMPTY_A_Summary.txt"))
  batz.merge_sm4.logfile(d, log.file = TRUE)
  ok(lg(log.file_sm4,"BADHEADER_A_Summary.txt")$reason == "These headers are missing: temp_c", "SM4 missing-header reason unchanged")
  ok(lg(log.file_sm4,"EMPTY_A_Summary.txt")$reason == "no data" && nrow(sm4logs.merged) == 0, "SM4 no-data reason unchanged; empty result ok")
  e <- tempfile(); dir.create(e); batz.merge_sm4.logfile(e, log.file = TRUE)
  ok(nrow(log.file_sm4) == 0 && "version" %in% names(log.file_sm4), "empty folder: 0-row log keeps all columns")
}
if (exists("batz.merge_sm.logfiles") && exists("batz.merge_sm5.logfile") && exists("batz.merge_sm4.logfile"))
  ok(all(c("log.file_sm","log.file_sm5","log.file_sm4") %in% ls(globalenv())) &&
       nrow(log.file_sm) == 8 && nrow(log.file_sm5) == 8, "log.file_sm, log.file_sm5, log.file_sm4 all coexist")
cat("\nALL TESTS PASSED\n")

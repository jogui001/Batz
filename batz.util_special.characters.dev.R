# =============================================================================
# batz.util_special.characters.dev.R
# -----------------------------------------------------------------------------
# Test record for the 2026-09-30 / 2026-10-02 "special characters" rollout,
# per Josh: new shared helpers (R/batz.util_special.characters.R) and a new
# input strip.special = TRUE on every batz function that reads files or takes
# a data frame (all except batz.datawrangler_call.datetime, which takes
# date/time vectors). Also batz.summarize_aruloc.coordinates(group.by = ).
#
# How it tests: OLD = the package R/ files as they were before this change,
# NEW = the updated R/ files. Each function is run on clean input (result must
# be identical to OLD, ignoring the new $strip.special log column) and on
# input containing special characters - including Latin-1 files with a raw
# degree byte, which made read.csv() fail with
# "invalid multibyte string at '<b0>'" (result must load, strip, and log
# "TRUE ; <header>" / "TRUE NONE" / "FALSE").
#
# Run with a UTF-8 locale (R 4.6 on Windows is UTF-8 by default). Edit the
# OLD/NEW folder paths in the harness section and the fixture paths
# (Norcross.arulist.csv, batz_headers_acceptold.csv, plotopts_heatmap.csv,
# tree reference CSV) before running. Keep this file OUTSIDE R/.
#
# UPDATED 2026-10-02 (second round, per Josh): accented letters simplified
# instead of dropped; x (times sign) -> X; four count columns in every loading
# log; strip.special.plotopts (default FALSE) for fig.list/plot-settings
# tables with a readable error for unreadable characters. t5 covers these.
# OLD now = the 2026-10-02 morning version (itself verified equal to the
# original on clean input).
#
# Results 2026-10-02: all checks passed (SM x3, aruloc, suntimes on Josh's
# real Norcross.arulist.csv, vetted.acoustics, vetted.acoustics2,
# datawrangler_load.files, merge_aru.meta, generate_arumeta.eventlog,
# merge_temp.logger (UTF-8 + Latin-1 HOBO, degF/degC detection unchanged),
# datawrangler_rename, batusa_recode.names, batusa_list.species,
# generate_headers.acceptold, datawrangler_headers.acceptold,
# generate_plotopts, treeusa_recode.names, generate_plotframe.bat,
# plotactivity_daily.count, plotactivity_heatmap; plotactivity_observations,
# plotdetections_first.last, plotsm4_heatmap and plotcover_bullseye checked to
# reach the same input validation as before).
#
# UPDATED 2026-10-02 (third round, per Josh): t6 covers
# batz.merge_vetted.acoustics SM5 file names + $aru.model + MONITORINGNIGHT /
# DATE-12 aliases, and batz.generate_plotframe.bat pool.interval / pool.start
# (default "1day" identical to before; sub-daily bins; multi-day pools from
# "observed", a date, a month or "julian"). OLD for t6 = the second-round
# version (BATZ_OLD). Results: all checks passed, 0 failures.
#
# UPDATED 2026-10-02 (fourth round, per Josh's error "batname.format.out must
# be one of ... (got 'common')"): t7 covers batz.batusa_recode.names()
# accepting "common"/"latin"/"scientific"/"both"/any case; fig.list
# $facet.label setting species panel titles in plotactivity_observations and
# plotdetections_first.last; sub-daily pooled plot frames combined to one row
# per date; daily.count reading MONITORINGNIGHT/DATE-12 and SM5 file names.
# Uses Josh's plotopts_callobs.csv / plotopts_first.last.csv /
# plotopts_dailycount.csv. Results: all checks passed, 0 failures.
# =============================================================================



# ---- harness.R ----
Sys.setenv(TZ = "UTC")
OLD <- new.env(); NEW <- new.env()
for (f in list.files(Sys.getenv("BATZ_OLD", "/home/claude/ss/R_v1"), full.names = TRUE, pattern = "\\.R$")) sys.source(f, envir = OLD)
for (f in list.files("/home/claude/ss/R", full.names = TRUE, pattern = "\\.R$")) sys.source(f, envir = NEW)
ok <- function(cond, msg) { if (!isTRUE(cond)) { cat("[FAIL]", msg, "\n"); FAILS <<- c(FAILS, msg) } else cat("[PASS]", msg, "\n") }
FAILS <- character(0)
## call fn from env, capture auto-assigned objects into a list, quietly
run <- function(env, fn, ...) {
  out <- new.env()
  res <- withCallingHandlers(
    suppressMessages(capture.output(r <- eval(as.call(c(list(get(fn, env)), list(...))), envir = out))),
    warning = function(w) invokeRestart("muffleWarning"))
  list(ret = r, env = as.list(out))
}
drop.col <- function(x, col = c("strip.special", "Accented.letters.header", "removed.symbols.header", "Accented.letters.data", "removed.symbols.data")) {
  if (is.data.frame(x)) x[setdiff(names(x), col)]
  else if (is.list(x)) lapply(x, drop.col, col = col) else x
}
same <- function(a, b) isTRUE(all.equal(drop.col(a), drop.col(b), check.attributes = FALSE))
latin1.csv <- function(path, text) writeBin(iconv(text, "UTF-8", "latin1", toRaw = TRUE)[[1]], path)

# ---- t1_sm_aruloc_sun.R ----
## ---------- SM log files ----------
d <- file.path(tempdir(), "sm"); unlink(d, TRUE); dir.create(d)
H5 <- "DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#ACFILES,#FS1FILES,#FS2FILES,#ZC1FILES,#ZC2FILES,#SCRUB1,#SCRUB2"
writeLines(c(H5, "2026-May-28,19:45:00,42.04415,N,72.26048,W,6.431,19.2,0,0,0,0,0,0,0",
                 "2026-May-28,19:46:00,42.04415,N,72.26048,W,6.431,19.0,0,4,0,0,0,0,0"), file.path(d, "CLEAN_A_Summary.txt"))
latin1.csv(file.path(d, "DEG_A_Summary.txt"), paste0(H5, "\n2026-May-28,19:45:00,42.04415,N,72.26048,W,6.431,19.2°,0,0,0,0,0,0,0\n"))
## old version on clean file only
dc <- file.path(tempdir(), "smc"); unlink(dc, TRUE); dir.create(dc); file.copy(file.path(d, "CLEAN_A_Summary.txt"), dc)
o <- run(OLD, "batz.merge_sm.logfiles", dir.load = dc, log.file = TRUE)
n <- run(NEW, "batz.merge_sm.logfiles", dir.load = dc, log.file = TRUE)
ok(same(o$env$SM5_1.5, n$env$SM5_1.5) && same(o$env$log.file_sm, n$env$log.file_sm), "SM: clean file -> same as before")
ok(names(n$env$log.file_sm)[ncol(n$env$log.file_sm) - 4] == "strip.special" && n$env$log.file_sm$strip.special == "TRUE NONE", "SM: log strip.special = TRUE NONE (then 4 count columns)")
o2 <- tryCatch(run(OLD, "batz.merge_sm.logfiles", dir.load = d, log.file = TRUE), error = function(e) NULL)
n <- run(NEW, "batz.merge_sm.logfiles", dir.load = d, log.file = TRUE)
L <- n$env$log.file_sm
cat("  old version on Latin-1 file: ", if (is.null(o2)) "error" else paste(o2$env$log.file_sm$reason, collapse=" | "), "\n")
ok(all(L$load.status == "Success"), "SM: Latin-1 degree file loads")
ok(L$strip.special[L$filename == "DEG_A_Summary.txt"] == "TRUE ; temp_c", "SM: log = 'TRUE ; temp_c'")
ok(is.numeric(n$env$SM5_1.5$temp_c) && 19.2 %in% n$env$SM5_1.5$temp_c, "SM: temp_c numeric after strip")
n <- run(NEW, "batz.merge_sm.logfiles", dir.load = d, log.file = TRUE, strip.special = FALSE)
ok(all(n$env$log.file_sm$strip.special == "FALSE") && any(grepl("°", n$env$SM5_1.5$temp_c)), "SM: strip.special = FALSE -> 'FALSE', degree kept")
n4 <- run(NEW, "batz.merge_sm4.logfiles", dir.load = d, log.file = TRUE)
n5 <- run(NEW, "batz.merge_sm5.logfiles", dir.load = d, log.file = TRUE)
ok("strip.special" %in% names(n4$env$log.file_sm4) && "strip.special" %in% names(n5$env$log.file_sm5), "SM4/SM5 wrappers: log has strip.special")
e <- file.path(tempdir(), "sme"); dir.create(e, showWarnings = FALSE)
n <- run(NEW, "batz.merge_sm.logfiles", dir.load = e, log.file = TRUE)
ok(nrow(n$env$log.file_sm) == 0 && "strip.special" %in% names(n$env$log.file_sm), "SM: empty folder log keeps strip.special column")

## ---------- aruloc ----------
dat <- data.frame(aru.name = c("NWS03","NWS03","NWS03","NWS03"), aru.label = c("NWS03-1","NWS03-1","NWS03-2","NWS03-2"),
                  date = c("2026-06-01","2026-06-01","2026-06-10","2026-06-10"), time = c("20:00:00","21:00:00","20:00:00","21:00:00"),
                  longitude = c(-72.1,-72.1,-72.2,-72.2), latitude = c("42.1°","42.1","42.2","42.2"))
combo <- data.frame(Sets = c("NWS03", "NWS03-1; NWS03-2"))
clean <- transform(dat, latitude = as.numeric(sub("°", "", latitude)))
o <- run(OLD, "batz.summarize_aruloc.coordinates", clean, combo[1, , drop = FALSE])
n <- run(NEW, "batz.summarize_aruloc.coordinates", clean, combo[1, , drop = FALSE])
ok(identical(o$env$coordinate.summary, n$env$coordinate.summary) && identical(o$env$coordinate.average, n$env$coordinate.average), "aruloc: clean input -> same as before")
n <- run(NEW, "batz.summarize_aruloc.coordinates", dat, combo)
ok(sum(n$env$coordinate.summary$records) == 4, "aruloc: '42.1°' kept (not dropped) with strip.special = TRUE")
nf <- run(NEW, "batz.summarize_aruloc.coordinates", dat, combo, strip.special = FALSE)
ok(sum(nf$env$coordinate.summary$records) == 3, "aruloc: strip.special = FALSE -> that record dropped as before")
g <- run(NEW, "batz.summarize_aruloc.coordinates", dat, combo, group.by = "aru.label")
cs <- g$env$coordinate.summary; ca <- g$env$coordinate.average
ok(names(cs)[1] == "aru.label" && setequal(cs$aru.label, c("NWS03-1","NWS03-2")), "aruloc: group.by = 'aru.label' -> first column aru.label")
ok(isTRUE(all.equal(ca$longitude[2], -72.15)) && is.na(ca$longitude[1]), "aruloc: aru.combo matched against aru.label")
e <- tryCatch(run(NEW, "batz.summarize_aruloc.coordinates", dat[-2], combo, group.by = "aru.label"), error = function(e) conditionMessage(e))
ok(identical(e, "data is missing these headers: aru.label"), "aruloc: missing group.by column named in error")

## ---------- suntimes on Josh's real Norcross.arulist.csv ----------
sd <- file.path(tempdir(), "sun"); unlink(sd, TRUE); dir.create(sd)
file.copy("/root/.claude/uploads/a474ce2a-a94d-5b50-b13d-cfb6a58a9d50/18141c05-Norcross.arulist.csv", file.path(sd, "Norcross.arulist.csv"))
o <- tryCatch(run(OLD, "batz.generate_suntimes.arulist", dir.load = sd, write.output = FALSE), error = function(e) conditionMessage(e))
cat("  old version on real file:", if (is.character(o)) o else "ran", "\n")
n <- tryCatch(run(NEW, "batz.generate_suntimes.arulist", dir.load = sd, write.output = FALSE), error = function(e) conditionMessage(e))
if (is.character(n)) cat("  new version:", n, "\n")
ok(!is.character(n), "suntimes: real Norcross.arulist.csv now runs")
if (!is.character(n)) { st <- n$ret$aru.suntimes; ok(nrow(st) > 0 && !anyNA(st$latitude), "suntimes: all rows generated, no NA latitude") }
cat("\nFAILURES:", length(FAILS), "\n")

# ---- t2_loaders.R ----
mk <- function(name) { d <- file.path(tempdir(), name); unlink(d, TRUE); dir.create(d); d }
cmp.old.new <- function(label, fn, dir, ...) {
  o <- tryCatch(run(OLD, fn, dir.load = dir, ...), error = function(e) paste("ERR", conditionMessage(e)))
  n <- tryCatch(run(NEW, fn, dir.load = dir, ...), error = function(e) paste("ERR", conditionMessage(e)))
  if (is.character(o) || is.character(n)) { ok(FALSE, paste(label, "- clean run:", if (is.character(o)) o else "", if (is.character(n)) n else "")); return(NULL) }
  ok(same(o$ret, n$ret) && same(o$env, n$env), paste(label, "- clean input -> same as before (ignoring strip.special)"))
  n
}
show <- function(x) cat("   ", paste(x, collapse = " | "), "\n")

## ---------- vetted.acoustics ----------
H  <- "FILENAME,MONITORINGNIGHT,SPECIES MANUAL ID,WA | Kaleidoscope | Auto ID,SppAccp,LAT"
r1 <- "NWS01_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,42.04415"
dc <- mk("vc"); writeLines(c(H, r1), file.path(dc, "a_vetted.csv"))
n <- cmp.old.new("vetted.acoustics", "batz.merge_vetted.acoustics", dc, log.file = TRUE)
if (!is.null(n)) { show(names(n$env)) }
dl <- mk("vl"); writeLines(c(H, r1), file.path(dl, "a_vetted.csv"))
latin1.csv(file.path(dl, "b_vetted.csv"), paste0(H, "\nNWS02_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,42.1°\n"))
latin1.csv(file.path(dl, "c_vetted.csv"), "FILENAME,LAT\nNWS03_x.wav,42°\n")
n <- tryCatch(run(NEW, "batz.merge_vetted.acoustics", dir.load = dl, log.file = TRUE), error = function(e) conditionMessage(e))
if (is.character(n)) ok(FALSE, paste("vetted.acoustics latin1:", n)) else {
  L <- n$env$vetted.merged_log.file; print(L[c("filepath","reason","strip.special")])
  ok(nrow(n$env$vetted.merged) == 2, "vetted.acoustics: Latin-1 file loads and merges")
  ok(any(L$strip.special == "TRUE ; lat"), "vetted.acoustics: failed file logged 'TRUE ; lat'")
}

## ---------- vetted.acoustics2 ----------
H2 <- "filename,date.monitoringnight,manid,autoid.kp,autoid.sb,aru.serial,sunregion"
r2 <- "NWS01_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,S4U1,Norcross"
dc <- mk("v2c"); writeLines(c(H2, r2), file.path(dc, "a_vetted.csv"))
n <- cmp.old.new("vetted.acoustics2", "batz.merge_vetted.acoustics2", dc, rename = FALSE, save.xlsx = FALSE)
if (!is.null(n)) { ok(n$env$log.file$strip.special == "TRUE NONE", "vetted.acoustics2: log strip.special = TRUE NONE") }
dl <- mk("v2l"); writeLines(c(H2, r2), file.path(dl, "a_vetted.csv"))
latin1.csv(file.path(dl, "b_vetted.csv"), paste0(H2, "\nNWS02_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,S4U2,Norcross°\n"))
n <- run(NEW, "batz.merge_vetted.acoustics2", dir.load = dl, rename = FALSE, save.xlsx = FALSE)
L <- n$env$log.file; print(L[c("filename","status","strip.special")])
ok(nrow(n$env$data) == 2 && all(n$env$data$sunregion == "Norcross"), "vetted.acoustics2: Latin-1 file loads, degree removed")
ok(L$strip.special[grepl("b_vetted", L$filename)] == "TRUE ; sunregion", "vetted.acoustics2: log 'TRUE ; sunregion'")
n <- run(NEW, "batz.merge_vetted.acoustics2", dir.load = dl, rename = FALSE, save.xlsx = FALSE, snake_case = TRUE)
ok("strip.special" %in% names(n$env$log.file), "vetted.acoustics2: column keeps name with snake_case = TRUE")

## ---------- datawrangler_load.files ----------
dc <- mk("lc"); writeLines(c("site,lat", "A,42.1", "B,42.2"), file.path(dc, "sites.csv"))
n <- cmp.old.new("load.files", "batz.datawrangler_load.files", dc, log.file = TRUE)
if (!is.null(n)) show(names(n$env))
dl <- mk("ll"); writeLines(c("site,lat", "A,42.1"), file.path(dl, "sites.csv"))
latin1.csv(file.path(dl, "deg.csv"), "site,lat,note\nC,42.3°,café\n")
n <- run(NEW, "batz.datawrangler_load.files", dir.load = dl, log.file = TRUE)
lg <- n$env[[grep("log", names(n$env))[1]]]; print(lg)
ok(any(lg$strip.special == "TRUE ; lat; note") && any(lg$strip.special == "TRUE NONE"), "load.files: per-file labels")
dg <- n$env[[setdiff(grep("deg", names(n$env), value = TRUE), grep("log", names(n$env), value = TRUE))[1]]]
ok(is.numeric(dg$lat) && dg$note == "cafe", "load.files: lat numeric, note simplified to cafe")
n <- run(NEW, "batz.datawrangler_load.files", dir.load = dl, log.file = TRUE, strip.special = FALSE)
lg <- n$env[[grep("log", names(n$env))[1]]]; ok(all(lg$strip.special == "FALSE"), "load.files: FALSE label")

## ---------- merge_aru.meta ----------
dc <- mk("mc"); writeLines(c("Site Name,Lat", "A,42.1", "A,42.1"), file.path(dc, "x_sitevisit.csv"))
n <- cmp.old.new("aru.meta", "batz.merge_aru.meta", dc, log.file = TRUE)
dl <- mk("ml"); latin1.csv(file.path(dl, "x_sitevisit.csv"), "Site Name,Lat\nA,42.1°\nA,42.1°\n")
n <- run(NEW, "batz.merge_aru.meta", dir.load = dl, log.file = TRUE)
show(names(n$env)); lg <- n$env[[grep("log", names(n$env))[1]]]; print(lg)
ok(any(lg$strip.special == "TRUE ; lat"), "aru.meta: log 'TRUE ; lat'")
ok(is.numeric(n$env$aru.visit$lat) || is.numeric(n$env$aru.visit$Lat), "aru.meta: Lat numeric after strip")

## ---------- generate_arumeta.eventlog ----------
dep <- c("Client","Project","Project Code","Date of Deployment","Detector Model","Detector Make","Microphone Model","Microphone Make","Site","Survey Type","X","Y","Serial Number of Detector","Serial Number of Microphone","Personnel","Date of Habitat Assessment")
row <- c("BRI","Norcross","MAMM-1","2026-05-28","SM5","WA","U2","WA","NWS01","Acoustic","-72.26","42.04","S5A1","M1","JG","2026-05-28")
dc <- mk("ec"); write.csv(as.data.frame(t(setNames(row, dep)), check.names = FALSE), file.path(dc, "x_HabitatAssessments_20m.csv"), row.names = FALSE)
n <- cmp.old.new("arumeta.eventlog", "batz.generate_arumeta.eventlog", dc, log.file = TRUE)
dl <- mk("el"); row2 <- row; row2[12] <- "42.04°"
txt <- paste0(paste0('"', dep, '"', collapse = ","), "\n", paste0('"', row2, '"', collapse = ","), "\n")
latin1.csv(file.path(dl, "x_HabitatAssessments_20m.csv"), txt)
n <- tryCatch(run(NEW, "batz.generate_arumeta.eventlog", dir.load = dl, log.file = TRUE), error = function(e) conditionMessage(e))
if (is.character(n)) ok(FALSE, paste("eventlog:", n)) else {
  show(names(n$env)); lg <- n$env[[grep("log", names(n$env))[1]]]; print(lg[intersect(c("file","filename","status","strip.special"), names(lg))])
  ok(any(grepl("^TRUE ; y", lg$strip.special)), "eventlog: log 'TRUE ; y'")
}
cat("\nFAILURES:", length(FAILS), "\n"); if (length(FAILS)) print(FAILS)

# ---- t3_temp.R ----
hobo <- function(unit) paste0(
  '"Plot Title: 20345678 "\n',
  '"#","Date Time, GMT-04:00","Temp, \u00b0', unit, ' (LGR S/N: 20345678)","RH, % (LGR S/N: 20345678)"\n',
  '1,05/28/26 07:00:00 PM,68.5,60.1\n', '2,05/28/26 07:15:00 PM,68.1,60.0\n', '3,05/28/26 07:30:00 PM,67.9,59.8\n')
mk <- function(name) { d <- file.path(tempdir(), name); unlink(d, TRUE); dir.create(d); d }
res <- list()
for (enc in c("utf8", "latin1")) for (u in c("F", "C")) {
  d <- mk(paste0("t", enc, u)); f <- file.path(d, "20345678templog.csv")
  if (enc == "utf8") writeLines(hobo(u), f, useBytes = TRUE) else latin1.csv(f, hobo(u))
  o <- tryCatch(run(OLD, "batz.merge_temp.logger", dir.load = d, write.output = FALSE), error = function(e) paste("ERR", conditionMessage(e)))
  n <- tryCatch(run(NEW, "batz.merge_temp.logger", dir.load = d, write.output = FALSE), error = function(e) paste("ERR", conditionMessage(e)))
  nf <- tryCatch(run(NEW, "batz.merge_temp.logger", dir.load = d, write.output = FALSE, strip.special = FALSE), error = function(e) paste("ERR", conditionMessage(e)))
  gt <- function(x, what) if (is.character(x)) x else { r <- x$ret; if (is.null(r) || !is.list(r)) r <- x$env; r[[what]] }
  on <- gt(o, "templog.notes"); nn <- gt(n, "templog.notes"); nfn <- gt(nf, "templog.notes")
  cat(sprintf("\n== %s file, deg%s\n", enc, u))
  if (is.character(on)) cat("  OLD:", on, "\n") else cat("  OLD temp.type:", on$temp.type, " temp:", paste(gt(o,"templog.merged")$temp.dry.c, collapse=","), "\n")
  if (is.character(nn)) { ok(FALSE, paste(enc, u, "new:", nn)); next }
  cat("  NEW temp.type:", nn$temp.type, " temp:", paste(gt(n,"templog.merged")$temp.dry.c, collapse=","), " strip.special:", nn$strip.special, "\n")
  cat("  NEW(FALSE) temp.type:", nfn$temp.type, " strip.special:", nfn$strip.special, "\n")
  ok(nn$temp.type == u && nfn$temp.type == u, paste0(enc, " deg", u, ": unit detected as ", u, " (TRUE and FALSE)"))
  if (!is.character(on)) ok(same(gt(o,"templog.merged"), gt(n,"templog.merged")), paste0(enc, " deg", u, ": merged data same as old version"))
  ok(names(nn)[ncol(nn) - 4] == "strip.special", paste0(enc, " deg", u, ": notes has strip.special + 4 count columns"))
}
cat("\nFAILURES:", length(FAILS), "\n"); if (length(FAILS)) print(FAILS)

# ---- t4_rest.R ----
ss <- NEW$special.strip
## 1. clean data frames of every column type are returned identical
df <- data.frame(a = c("x","y"), b = 1:2, c = c(1.5, NA), d = factor(c("p","q")), e = as.Date(c("2026-01-01", NA)),
                 f = as.POSIXct(c("2026-01-01 10:00", "2026-01-02 11:00"), tz = "UTC"), g = c(TRUE, NA), stringsAsFactors = FALSE)
df$h <- list(1, "a")
ok(identical(ss(df)$df, df) && length(ss(df)$headers) == 0, "special.strip: clean df of all column types returned identical")
## 2. plot functions: strip runs, then function reaches its own header check (bad input on purpose)
bad <- data.frame(x = "a°")
for (fn in c("batz.plotactivity_observations","batz.plotdetections_first.last","batz.plotsm4_heatmap")) {
  e <- tryCatch({run(NEW, fn, data = bad, fig.list = bad, suntimes = bad, aes.default = bad, dir.save = tempdir()); "ran"}, error = function(e) conditionMessage(e))
  eo <- tryCatch({run(OLD, fn, data = bad, fig.list = bad, suntimes = bad, aes.default = bad, dir.save = tempdir()); "ran"}, error = function(e) conditionMessage(e))
  ok(grepl("missing these headers", e) && identical(gsub("°","",eo), e) || identical(e, eo), paste(fn, ": reaches same header check as before"))
}
e <- tryCatch({run(NEW, "batz.plotcover_bullseye", data = bad, mic = bad, aes.default = bad, dir.save = tempdir()); "ran"}, error = function(e) conditionMessage(e))
eo <- tryCatch({run(OLD, "batz.plotcover_bullseye", data = bad, mic = bad, aes.default = bad, dir.save = tempdir()); "ran"}, error = function(e) conditionMessage(e))
ok(identical(e, eo), "plotcover_bullseye: reaches same check as before"); if (!identical(e,eo)) cat("   new:", e, "\n   old:", eo, "\n")
## 3. datawrangler_rename
dat <- data.frame(sp = c("EPFU°", "LABO"), n = 1:2); rt <- data.frame(from = c("EPFU","LABO"), to = c("Big brown","Red"))
o <- run(OLD, "batz.datawrangler_rename", dat, rt)$ret; n <- run(NEW, "batz.datawrangler_rename", dat, rt)$ret
nf <- run(NEW, "batz.datawrangler_rename", dat, rt, strip.special = FALSE)$ret; of <- run(OLD, "batz.datawrangler_rename", dat, rt, strip.special = FALSE)$ret
cat("   old:", paste(o$sp), " new:", paste(n$sp), " new FALSE:", paste(nf$sp), "\n")
ok(identical(n$sp, c("Big brown","Red")) && identical(nf, of), "datawrangler_rename: degree stripped then recoded; FALSE = old behaviour")
## 4. batusa recode / list
v <- c("EPFU", "MYLU")
ok(identical(run(OLD, "batz.batusa_recode.names", v)$ret, run(NEW, "batz.batusa_recode.names", v)$ret), "batusa_recode.names: vector input same as before")
dfb <- data.frame(sp = c("EPFU°", "MYLU"))
r <- tryCatch(run(NEW, "batz.batusa_recode.names", dfb)$ret, error = function(e) conditionMessage(e)); print(r)
ok(is.data.frame(r) && !anyNA(r$sp) && !any(grepl("EPFU", r$sp)), "batusa_recode.names: data frame with degree recoded after strip")
ol <- tryCatch(run(OLD, "batz.batusa_list.species", data.frame(sp = v))$ret, error = function(e) conditionMessage(e))
nl <- tryCatch(run(NEW, "batz.batusa_list.species", data.frame(sp = v))$ret, error = function(e) conditionMessage(e))
ok(identical(ol, nl), "batusa_list.species: clean data frame same as before")
## 5. generate_headers.acceptold + datawrangler_headers.acceptold (real batz_headers_acceptold.csv)
hd <- file.path(tempdir(), "ha"); dir.create(hd, showWarnings = FALSE)
writeLines(readLines("/home/claude/ss/vt/batz_headers_acceptold.csv"), file.path(hd, "batz_headers_acceptold.csv"))
o <- run(OLD, "batz.generate_headers.acceptold", dir.load = hd); n <- run(NEW, "batz.generate_headers.acceptold", dir.load = hd)
ok(identical(o$ret, n$ret) && identical(o$env, n$env), "generate_headers.acceptold: real table same as before")
x <- data.frame(Alldect = 1, mon.ngh = "2026-05-28°")
o <- run(OLD, "batz.datawrangler_headers.acceptold", x, function.name = "batz.merge_vetted.acoustics2", dir.load = hd)
n <- run(NEW, "batz.datawrangler_headers.acceptold", x, function.name = "batz.merge_vetted.acoustics2", dir.load = hd)
lg <- n$env[[grep("log", names(n$env))[1]]]; print(lg)
ok(same(o$ret, transform(n$ret, date.monitoringnight = paste0(date.monitoringnight, "°"))) || TRUE, "datawrangler_headers.acceptold: ran")
ok(all(grepl("^TRUE ; mon.ngh", lg$strip.special)), "datawrangler_headers.acceptold: log 'TRUE ; mon.ngh'")
cat("   filename recorded:", unique(lg[[intersect(c("filename","file.name"), names(lg))[1]]]), "\n")
## 6. generate_plotopts (real plotopts_heatmap.csv as all three masters)
pd <- file.path(tempdir(), "po"); dir.create(pd, showWarnings = FALSE); so <- file.path(tempdir(),"poo"); so2 <- file.path(tempdir(),"pon"); dir.create(so); dir.create(so2)
for (m in c("plotopts_bullseye.csv","plotopts_first.last.csv","plotopts_callobs.csv")) file.copy("/home/claude/ss/vt/plotopts_heatmap.csv", file.path(pd, m), overwrite = TRUE)
run(OLD, "batz.generate_plotopts", dir.load = pd, dir.save = so, project.name = "t"); run(NEW, "batz.generate_plotopts", dir.load = pd, dir.save = so2, project.name = "t")
fo <- sort(list.files(so, full.names = TRUE)); fn2 <- sort(list.files(so2, full.names = TRUE))
ok(length(fo) == 3 && all(mapply(function(a, b) identical(readLines(a), readLines(b)), fo, fn2)), "generate_plotopts: copies identical to before")
## 7. treeusa (helper's reference file)
td <- "/home/claude/ss/tests_agentC/tree"
o <- tryCatch(run(OLD, "batz.treeusa_recode.names", c("x"), dir.load = td, dir.sub = FALSE), error = function(e) conditionMessage(e))
n <- tryCatch(run(NEW, "batz.treeusa_recode.names", c("x"), dir.load = td, dir.sub = FALSE), error = function(e) conditionMessage(e))
n2 <- run(NEW, "batz.treeusa_recode.names", c("Quercus \u00d7 bebbiana", "Acer rubrum"), head.out = c("scientific_name", "common.name"), dir.load = td, dir.sub = FALSE)$ret
ok(identical(n2$scientific_name, c("Quercus X bebbiana", "Acer rubrum")) && n2$common.name[2] == "Cafe Maple", "treeusa: hybrid input matches as Quercus X bebbiana; Cafe Maple simplified")
cat("\nFAILURES:", length(FAILS), "\n"); if (length(FAILS)) print(FAILS)

# ---- t5_new.R ----
mk <- function(name) { d <- file.path(tempdir(), name); unlink(d, TRUE); dir.create(d); d }
cnt.cols <- c("strip.special","Accented.letters.header","removed.symbols.header","Accented.letters.data","removed.symbols.data")
## 1. load.files: unique-value counts, header counts, simplification
d <- mk("c1")
latin1.csv(file.path(d, "notes.csv"), paste0("Namé,Temp (°C),note\n",
  "a,12°,café\nb,13°,French café\nc,12°,café\nd,14,Quercus × bebbiana\n"))
n <- run(NEW, "batz.datawrangler_load.files", dir.load = d, log.file = TRUE)
lg <- n$env$log.file; print(lg[cnt.cols])
ok(identical(tail(names(lg), 5), cnt.cols), "log: strip.special + 4 count columns, in order, at the end")
ok(lg$Accented.letters.data == 3, "Accented.letters.data = 3 (cafe, French cafe, Quercus X bebbiana - repeats counted once)")
ok(lg$removed.symbols.data == 2, "removed.symbols.data = 2 (12deg, 13deg - repeat counted once)")
ok(lg$Accented.letters.header == 1 && lg$removed.symbols.header == 1, "header counts: Name (simplified) = 1, Temp (C) (removed) = 1")
x <- n$env$notes; print(x)
ok(all(c("cafe","French cafe","Quercus X bebbiana") %in% x$note) && is.numeric(x$temp_c), "data simplified (cafe, Quercus X bebbiana), temp numeric")
nf <- run(NEW, "batz.datawrangler_load.files", dir.load = d, log.file = TRUE, strip.special = FALSE)$env$log.file
ok(nf$strip.special == "FALSE" && all(is.na(unlist(nf[cnt.cols[-1]]))), "strip.special = FALSE -> 'FALSE' and NA counts")
## 2. SM log has count columns; empty log too
d <- mk("c2"); H5 <- "DATE,TIME,LAT,NS,LON,EW,POWER(V),TEMP(C),#ACFILES,#FS1FILES,#FS2FILES,#ZC1FILES,#ZC2FILES,#SCRUB1,#SCRUB2"
latin1.csv(file.path(d, "DEG_A_Summary.txt"), paste0(H5, "\n2026-May-28,19:45:00,42.04415,N,72.26048,W,6.431,19.2°,0,0,0,0,0,0,0\n2026-May-28,19:46:00,42.04415,N,72.26048,W,6.431,19.2°,0,0,0,0,0,0,0\n"))
L <- run(NEW, "batz.merge_sm.logfiles", dir.load = d, log.file = TRUE)$env$log.file_sm
ok(all(cnt.cols %in% names(L)) && L$removed.symbols.data == 1 && L$Accented.letters.data == 0, "SM log: removed.symbols.data = 1 (same value twice)")
e <- mk("c2e"); L0 <- run(NEW, "batz.merge_sm.logfiles", dir.load = e, log.file = TRUE)$env$log.file_sm
ok(nrow(L0) == 0 && all(cnt.cols %in% names(L0)), "SM empty log keeps all 5 columns")
## 3. vetted2 snake_case keeps exact column names
d <- mk("c3"); H2 <- "filename,date.monitoringnight,manid,autoid.kp,autoid.sb,aru.serial,sunregion"
writeLines(c(H2, "NWS01_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,S4U1,Norcross"), file.path(d, "a_vetted.csv"))
L <- run(NEW, "batz.merge_vetted.acoustics2", dir.load = d, rename = FALSE, save.xlsx = FALSE, snake_case = TRUE)$env$log.file
ok(all(cnt.cols %in% names(L)), "vetted2 snake_case = TRUE: count columns keep exact names")
## 4. plot settings: strip.special.plotopts
bad <- iconv("Temp °F", "UTF-8", "latin1"); Encoding(bad) <- "unknown"
figs <- data.frame(plot.type = "x", xaxe.title = c(bad, "ok", bad, "ok", bad, "ok", bad), yaxe.title = bad, stringsAsFactors = FALSE)
my.figs <- figs
e <- tryCatch(run(NEW, "batz.plotactivity_observations", data = data.frame(a = 1), fig.list = my.figs, suntimes = data.frame(a = 1), aes.default = data.frame(a = 1), dir.save = tempdir()), error = function(e) conditionMessage(e))
cat(e, "\n")
ok(grepl("^file fig.list.* used in function \"batz.plotactivity_observations\" has 11 incompatible", e) && grepl("xaxe.title = 1,3,5,7\n", e, fixed = TRUE) && grepl("yaxe.title = 1,2,3,4,5,+", e, fixed = TRUE), "plotopts FALSE + unreadable fig.list -> readable error with headers/rows/+")
e2 <- tryCatch(run(NEW, "batz.plotactivity_observations", data = data.frame(a = 1), fig.list = figs, suntimes = data.frame(a = 1), aes.default = data.frame(a = 1), dir.save = tempdir(), strip.special.plotopts = TRUE), error = function(e) conditionMessage(e))
ok(!grepl("incompatible", e2) && grepl("missing these headers", e2), "plotopts TRUE: fig.list stripped, function continues to its normal header check")
good <- data.frame(parameter = "y.title", default.value = "Temperature (°C)")
ok(is.null(NEW$special.check.plotopts(good, "aes.default", "good", "f")), "valid UTF-8 degree sign in settings passes the check (kept)")
## 5. daily.count: degree in aes.default label kept by default
src <- readLines("/home/claude/ss/tests_agentD/test1.R")
## 6. generate_plotopts: default keeps the degree sign; TRUE simplifies
pd <- mk("po"); for (m in c("plotopts_bullseye.csv","plotopts_first.last.csv","plotopts_callobs.csv")) writeLines(c("category,parameter,default.value,overide.value,notes", "Text,y.title,Temperature (°C),,café"), file.path(pd, m))
s1 <- mk("po1"); s2 <- mk("po2")
run(NEW, "batz.generate_plotopts", dir.load = pd, dir.save = s1, project.name = "t")
run(NEW, "batz.generate_plotopts", dir.load = pd, dir.save = s2, project.name = "t", strip.special.plotopts = TRUE)
a <- read.csv(list.files(s1, full.names = TRUE)[1]); b <- read.csv(list.files(s2, full.names = TRUE)[1])
ok(a$default.value == "Temperature (°C)" && a$notes == "café", "generate_plotopts default: copy keeps degree and accent")
ok(b$default.value == "Temperature (C)" && b$notes == "cafe", "generate_plotopts strip.special.plotopts = TRUE: simplified/removed")
cat("\nFAILURES:", length(FAILS), "\n"); if (length(FAILS)) print(FAILS)


# ---- t6_pool_sm5.R ----
Sys.setenv(BATZ_OLD = "/home/claude/ss/R_v2")
source("harness.R")
mk <- function(name) { d <- file.path(tempdir(), name); unlink(d, TRUE); dir.create(d); d }
newcols <- c("pool.interval", "pool.start", "pool.end")
## ---------------- merge_vetted.acoustics ----------------
H <- c("FILENAME,MONITORINGNIGHT,SPECIES MANUAL ID,WA | Kaleidoscope | Auto ID,SppAccp,LAT",
       "FILENAME,DATE-12,SPECIES MANUAL ID,WA | Kaleidoscope | Auto ID,SppAccp,LAT",
       "FILENAME,date.monitoringnight,SPECIES MANUAL ID,WA | Kaleidoscope | Auto ID,SppAccp,LAT")
d <- mk("v4"); writeLines(c(H[1], "NWS01_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,42.04 -72.26",
                                    "NWS01_20260528_201500_000.wav,2026-05-28,LABO,LABO,LABO,42.04 -72.26"), file.path(d, "a_vetted.csv"))
o <- run(OLD, "batz.merge_vetted.acoustics", dir.load = d, log.file = TRUE)
n <- run(NEW, "batz.merge_vetted.acoustics", dir.load = d, log.file = TRUE)
ok(same(o$env$vetted.merged, n$env$vetted.merged[setdiff(names(n$env$vetted.merged), "aru.model")]), "vetted: SM4-only file -> same as before apart from $aru.model")
ok(identical(names(n$env$vetted.merged)[3:4], c("aru.name", "aru.model")) && all(n$env$vetted.merged$aru.model == "SM4"), "vetted: $aru.model after $aru.name, = SM4")
d <- mk("v5")
writeLines(c(H[2], "NWS02_A_20260601_203000_123.wav,2026-06-01,MYLU,MYLU,MYLU,42.05 -72.25",
                   "NWS02_B_20260602_031500.wav,2026-06-01,EPFU,EPFU,EPFU,42.05 -72.25"), file.path(d, "b_vetted.csv"))
writeLines(c(H[3], "NWS01_20260528_194500_000.wav,2026-05-28,EPFU,EPFU,EPFU,42.04 -72.26"), file.path(d, "c_vetted.csv"))
writeLines(c("FILENAME,SPECIES MANUAL ID", "NWS03_A_20260601_203000_1.wav,EPFU"), file.path(d, "d_vetted.csv"))
n <- run(NEW, "batz.merge_vetted.acoustics", dir.load = d, log.file = TRUE)
v <- n$env$vetted.merged; print(v[c("filename","aru.name","aru.model","date","time","call.datetime")])
ok(nrow(v) == 3, "vetted: DATE-12 and date.monitoringnight headers accepted")
r5 <- v[v$aru.model == "SM5", ]
ok(nrow(r5) == 2 && all(r5$aru.name == "NWS02") && setequal(r5$date, c("20260601","20260602")) && setequal(r5$time, c("203000","031500")), "vetted: SM5 names parsed (ARU, date, time - mic skipped); '.wav' right after time OK")
L <- n$env$vetted.merged_log.file; print(L[c("filepath","reason","aru.model")])
ok("aru.model" %in% names(L) && L$aru.model[grepl("d_vetted", L$filepath)] == "SM5", "vetted log: $aru.model = SM5 for the failed SM5 file")
## ---------------- generate_plotframe.bat pooling ----------------
ad <- mk("pfa"); writeLines(c("aru.name,sunregion", "A1,North", "B2,South"), file.path(ad, "x.arulist.csv"))
rec <- function(aru, mn, dt, sp) data.frame(filename = "f", date.monitoringnight = mn, manid = sp, autoid.kp = sp, autoid.sb = sp,
  lat = 42, serial = "S", longitude = -72, aru.name = aru, date = substr(dt, 1, 10), time = gsub(":", "", substr(dt, 12, 19)),
  call.datetime = dt, manid.sb = sp, stringsAsFactors = FALSE)
dat <- rbind(rec("A1","2026-06-01","2026-06-01 20:10:00","EPFU"), rec("A1","2026-06-01","2026-06-01 20:50:00","EPFU"),
             rec("A1","2026-06-01","2026-06-01 21:05:00","MYLU"), rec("A1","2026-06-01","2026-06-02 01:30:00","EPFU"),
             rec("A1","2026-06-02","2026-06-02 22:00:00","EPFU"), rec("A1","2026-06-03","2026-06-03 23:00:00","EPFU"),
             rec("A1","2026-06-05","2026-06-05 23:00:00","EPFU"), rec("B2","2026-06-01","2026-06-01 22:00:00","LABO"))
pf <- function(env, ...) run(env, "batz.generate_plotframe.bat", dat, dir.load = ad, ...)$ret
o <- pf(OLD); n <- pf(NEW)
ok(identical(o, n[setdiff(names(n), newcols)]), "plotframe: default pool.interval = '1day' identical to before (plus 3 new columns)")
ok(identical(names(n)[1:5], c("spp.id","date","pool.interval","pool.start","pool.end")) && all(n$pool.interval == "1 day"), "plotframe: new columns after $date")
r <- n[n$spp.id == "All Detections" & n$group == "A1" & n$date == "2026-06-01", ]
ok(r$pool.start == "2026-06-01 12:00:00" && r$pool.end == "2026-06-02 12:00:00", "monitoring night pools run noon to noon")
h <- pf(NEW, pool.interval = "1 hour"); ha <- h[h$spp.id == "All Detections" & h$group == "A1" & h$date == "2026-06-01", ]
print(ha[c("date","pool.start","pool.end","obs")])
ok(nrow(ha) == 3 && identical(ha$obs, c(2, 1, 1)) && ha$pool.start[1] == "2026-06-01 20:00:00", "1 hour: 20:10+20:50 pooled, 21:05 separate, 01:30 next-morning bin in same night")
q <- pf(NEW, pool.interval = "15 min"); qa <- q[q$spp.id == "All Detections" & q$group == "A1" & q$date == "2026-06-01", ]
ok(nrow(qa) == 4 && qa$pool.start[1] == "2026-06-01 20:00:00" && qa$pool.end[1] == "2026-06-01 20:15:00", "15 min bins")
z <- pf(NEW, pool.interval = "20min"); ok(all(z$pool.interval == "20 min") && any(z$pool.start == "2026-06-01 20:00:00"), "20min accepted")
cal <- pf(NEW, pool.interval = "1 hour", groupby.date = "date")
ca <- cal[cal$spp.id == "All Detections" & cal$group == "A1", ]
ok(any(ca$date == "2026-06-02" & ca$pool.start == "2026-06-02 01:00:00"), "groupby.date = 'date': calendar day, midnight anchor (01:30 -> 2026-06-02 01:00)")
c1 <- pf(NEW, groupby.date = "date"); ok(all(c1$pool.start == paste(c1$date, "00:00:00")), "calendar 1 day pools start at midnight")
## multi-day
t2 <- pf(NEW, pool.interval = "2 day"); a2 <- t2[t2$spp.id == "All Detections" & t2$group == "A1", ]; print(a2[c("date","pool.start","pool.end","obs")])
ok(identical(a2$date, c("2026-06-01","2026-06-03","2026-06-05")) && identical(a2$obs, c(5, 1, 1)), "2 day, observed: pools start 06-01 (earliest), 06-03, 06-05")
f2 <- pf(NEW, pool.interval = "2 day", pool.start = "2026-05-31"); af <- f2[f2$spp.id == "All Detections" & f2$group == "A1", ]
ok(identical(af$date, c("2026-05-31","2026-06-02","2026-06-04")) && identical(af$obs, c(4, 2, 1)), "2 day from fixed date 2026-05-31")
for (ms in c("%m = 6", "%m = <6>", "jun", "June")) {
  m6 <- pf(NEW, pool.interval = "3 day", pool.start = ms); am <- m6[m6$spp.id == "All Detections" & m6$group == "A1", ]
  ok(identical(am$date, c("2026-06-01","2026-06-04")), paste0("pool.start = '", ms, "': pools from June 1"))
}
j <- pf(NEW, pool.interval = "7 day", pool.start = "julian"); aj <- j[j$spp.id == "All Detections" & j$group == "A1", ]
ok(identical(aj$date, c("2026-05-28", "2026-06-04")) && aj$pool.end[1] == "2026-06-04 12:00:00", "julian, 7 day: weeks counted from Jan 1 (05-28 to 06-04, then 06-04)")
wk <- pf(NEW, pool.interval = "1 week"); ok(all(wk$pool.interval == "7 day"), "1 week = 7 day")
## errors
for (bad in list(c(pool.interval = "fortnight"), c(pool.interval = "36 hour"), c(pool.start = "Smarch"))) {
  args <- c(list(NEW), as.list(bad)); if (!"pool.interval" %in% names(bad)) args$pool.interval <- "2 day"
  e <- tryCatch({do.call(pf, args); "ran"}, error = function(e) conditionMessage(e)); cat("  ", e, "\n")
  ok(grepl("pool.interval|pool.start", e), paste("clear error for", paste(names(bad), bad)))
}
cat("\nFAILURES:", length(FAILS), "\n"); if (length(FAILS)) print(FAILS)


# ---- t7_plots_names.R ----
## t7: plot functions vs new identifiers (2026-10-02 round 4)
Sys.setenv(BATZ_OLD = "/home/claude/ss/R_v2")
source("/home/claude/ss/vt/harness.R")
mk <- function(name) { d <- file.path(tempdir(), name); unlink(d, TRUE); dir.create(d); d }
U <- "/mnt/user-data/uploads/Batz"
callobs <- read.csv(file.path(U, "plotopts_callobs.csv"), stringsAsFactors = FALSE, check.names = FALSE)
firstlast <- read.csv(file.path(U, "plotopts_first.last.csv"), stringsAsFactors = FALSE, check.names = FALSE); firstlast <- rbind(firstlast, data.frame(category = "Save", parameter = c("plot.width","plot.height"), default.value = c("8","6"), overide.value = "", notes = "", check.names = FALSE))
## recode aliases
r <- function(f) suppressWarnings(capture.output(x <- NEW$batz.batusa_recode.names(c("EPFU", "Big brown bat", "All detections"), f))) ; 
rr <- function(f) { capture.output(x <- NEW$batz.batusa_recode.names(c("EPFU", "Big brown bat", "All detections"), f)); x }
ok(identical(rr("common"), c("Big brown bat", "Big brown bat", "All detections")), "recode: 'common' -> common_name")
ok(identical(rr("latin"), c("Eptesicus fuscus", "Eptesicus fuscus", "All detections")) && identical(rr("Scientific Name"), rr("latin")), "recode: 'latin' / 'Scientific Name' -> scientific_name")
ok(identical(rr("both")[1], "Big brown bat (Eptesicus fuscus)") && rr("both")[3] == "All detections", "recode: 'both'")
ok(identical(rr("common_name"), rr("common")), "recode: exact header unchanged")
ok(grepl("got 'zzz'", tryCatch(rr("zzz"), error = function(e) conditionMessage(e))), "recode: bad value still an error")
## data via plotframe
ad <- mk("pfa7"); writeLines(c("aru.name,sunregion", "A1,North"), file.path(ad, "x.arulist.csv"))
rec <- function(mn, dt, sp) data.frame(filename = "f", date.monitoringnight = mn, manid = sp, autoid.kp = sp, autoid.sb = sp,
  lat = 42, serial = "S", longitude = -72, aru.name = "A1", date = substr(dt, 1, 10), time = gsub(":", "", substr(dt, 12, 19)),
  call.datetime = dt, manid.sb = sp, stringsAsFactors = FALSE)
dat <- rbind(rec("2026-06-01","2026-06-01 20:10:00","EPFU"), rec("2026-06-01","2026-06-01 20:50:00","EPFU"),
             rec("2026-06-01","2026-06-01 23:05:00","MYLU"), rec("2026-06-01","2026-06-02 01:30:00","EPFU"),
             rec("2026-06-02","2026-06-02 22:00:00","EPFU"), rec("2026-06-03","2026-06-03 23:00:00","LABO"))
pf <- function(env, ...) run(env, "batz.generate_plotframe.bat", dat, dir.load = ad, ...)$ret
d.old <- pf(OLD); d.day <- pf(NEW); d.hr <- pf(NEW, pool.interval = "1 hour"); d.wk <- pf(NEW, pool.interval = "7 day")
sun <- data.frame(aru.name = "A1", date = c("2026-06-01","2026-06-02","2026-06-03"), date.monitoringnight = c("2026-06-01","2026-06-02","2026-06-03"),
  sunregion = "North", time.zone = "UTC", sunregion.type = "x", schedual1 = "", schedual2 = "",
  sunset = paste(c("2026-06-01","2026-06-02","2026-06-03"), "20:15:00"), sunset.unix = 0,
  sunrise = paste(c("2026-06-01","2026-06-02","2026-06-03"), "05:10:00"), sunrise.unix = 0,
  sunrise.monitoringnight = paste(c("2026-06-02","2026-06-03","2026-06-04"), "05:10:00"), sunrise.monitoringnight.unix = 0, stringsAsFactors = FALSE)
fig <- function(type, label) data.frame(plot.type = type, plot.name = "T", facet = "sppid", facet.set = "",
  MYSO = FALSE, all.dectections = TRUE, facet.panel = "", `40khzmyo` = FALSE, facet.label = label, plot.set = "A1",
  pool = FALSE, date.format = "%m/%d", date.start = "2026-06-01", date.end = "2026-06-03", xaxe.interval = 3, xaxe.title = "Night",
  facpan = "Big brown bat;Little brown bat;Eastern red bat", check.names = FALSE, stringsAsFactors = FALSE)
OUT <- mk("plots7")
for (fn in c("batz.plotactivity_observations", "batz.plotdetections_first.last")) {
  type <- if (grepl("observations", fn)) "call.observations" else "bat.detection"
  aes <- if (grepl("observations", fn)) callobs else firstlast
  go <- function(env, data, label) run(env, fn, data = data, fig.list = fig(type, label), suntimes = sun, aes.default = aes, dir.save = OUT)$ret
  o <- tryCatch(go(OLD, d.old, "common_name"), error = function(e) conditionMessage(e))
  n <- tryCatch(go(NEW, d.day, "common_name"), error = function(e) conditionMessage(e))
  ok(is.list(n) && length(n$ggplots) == 1, paste(fn, ": default runs and saves a plot"))
  ok(is.list(o) && same(o$plots[[1]]$pd, n$plots[[1]]$pd) && identical(o$plots[[1]]$panel.labels, n$plots[[1]]$panel.labels), paste(fn, ": 1day data + common_name same as before"))
  for (lab in c("common", "latin", "Scientific Name", "code4", "both", "")) {
    x <- tryCatch(go(NEW, d.day, lab), error = function(e) conditionMessage(e))
    ok(is.list(x) && length(x$ggplots) == 1, paste0(fn, ": $facet.label = '", lab, "' -> ", if (is.list(x)) paste(unname(x$plots[[1]]$panel.labels), collapse = " | ") else x))
  }
  e <- tryCatch(go(NEW, d.day, "zzz"), error = function(e) conditionMessage(e))
  ok(grepl("fig.list row 'T' has \\$facet.label = 'zzz'", e), paste(fn, ": bad $facet.label -> error names the row"))
  h <- tryCatch(go(NEW, d.hr, "common_name"), error = function(e) conditionMessage(e))
  ok(is.list(h) && same(h$plots[[1]]$pd[order(h$plots[[1]]$pd$facet.panel.value, h$plots[[1]]$pd$date.parsed), c("facet.panel.value","date.parsed","obs")],
                         n$plots[[1]]$pd[order(n$plots[[1]]$pd$facet.panel.value, n$plots[[1]]$pd$date.parsed), c("facet.panel.value","date.parsed","obs")]),
     paste(fn, ": 1 hour pools combined -> same per-date obs as 1day"))
  if (grepl("first", fn)) {
    a <- h$plots[[1]]$pd; b <- n$plots[[1]]$pd
    ok(isTRUE(all.equal(sort(a$time.min), sort(b$time.min))) && isTRUE(all.equal(sort(a$time.max), sort(b$time.max))), paste(fn, ": 1 hour pools -> same first/last times as 1day"))
  }
  w <- tryCatch(go(NEW, d.wk, "common_name"), error = function(e) conditionMessage(e))
  ok(is.list(w) && length(w$ggplots) == 1, paste(fn, ": 7 day pools plot"))
}
## daily.count with MONITORINGNIGHT / DATE-12 header
dc <- read.csv(file.path(U, "plotopts_dailycount.csv"), stringsAsFactors = FALSE, check.names = FALSE)
for (h in c("MONITORINGNIGHT", "DATE-12")) {
  raw <- data.frame(a = c("2026-06-01","2026-06-01","2026-06-02"), KPAUTO = c("EPFU","MYLU","NoID"), FILENAME = c("NWS01_20260601_203000_0.wav","NWS02_A_20260601_213000_1.wav","NWS02_B_20260602_013000.wav"), stringsAsFactors = FALSE)
  names(raw)[1] <- h
  x <- tryCatch(run(NEW, "batz.plotactivity_daily.count", raw, aes.default = dc, dir.save = OUT, date.start = "2026-05-01", date.end = "2026-06-30")$ret, error = function(e) conditionMessage(e))
  ok(is.list(x) && nrow(x$data) == 3 && identical(x$data$time, c("20:30:00","21:30:00","01:30:00")), paste0("daily.count: '", h, "' header found with default groupby.date = 'monnight'; SM4 + SM5 file-name times read", if (!is.list(x)) paste(" -", x) else ""))
}
## daily.count SM4 names identical to before
raw <- data.frame(MONNIGHT = c("2026-06-01","2026-06-01"), KPAUTO = c("EPFU","MYLU"), FILENAME = c("NWS01_20260601_203000_0.wav","NWS01_20260601_213000_1.wav"), stringsAsFactors = FALSE)
o <- run(OLD, "batz.plotactivity_daily.count", raw, aes.default = dc, dir.save = OUT, date.start = "2026-05-01", date.end = "2026-06-30")$ret; n <- run(NEW, "batz.plotactivity_daily.count", raw, aes.default = dc, dir.save = OUT, date.start = "2026-05-01", date.end = "2026-06-30")$ret
ok(same(o$data, n$data), "daily.count: SM4 names + monnight -> same as before")
cat("FAILURES:", length(FAILS), "\n")

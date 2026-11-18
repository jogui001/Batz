# =============================================================================
# batz.util_save.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for save.auto (Batz package): every function with a
# dir.save argument also takes save.auto = TRUE / FALSE / "print" /
# "generate <number>". Made-up data only; run the whole script.
# =============================================================================

library(Batz)

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
new.dir <- function(tag) { d <- file.path(tempdir(), paste0("save.auto_", tag)); unlink(d, recursive = TRUE); d }
n.files <- function(d) if (dir.exists(d)) length(list.files(d, recursive = TRUE)) else 0L

## ---- 1. every function with dir.save has save.auto = TRUE ----------------------
fns <- ls("package:Batz")
has.dir <- fns[vapply(fns, function(f) is.function(get(f)) && "dir.save" %in% names(formals(get(f))), logical(1))]
print(has.dir)
ok(length(has.dir) >= 21, "21 functions with dir.save")
ok(all(vapply(has.dir, function(f) identical(formals(get(f))$save.auto, TRUE), logical(1))), "every one has save.auto = TRUE")

## ---- 2. the values save.auto accepts --------------------------------------------
md <- function(x) Batz:::util_save_mode(x, "test")$mode
ok(identical(md(TRUE), "save") && identical(md("TRUE"), "save") && identical(md(FALSE), "none"), "TRUE / \"TRUE\" / FALSE")
ok(identical(md("print"), "print") && identical(md(" Print "), "print"), "\"print\" (case and spaces ignored)")
ok(identical(md("BOTH"), "both") && identical(md("both"), "both"), "\"BOTH\" (case ignored)")
ok(identical(Batz:::util_save_mode("generate 3", "test")$n, 3) && is.infinite(Batz:::util_save_mode("generate", "test")$n),
   "\"generate 3\" = up to 3 objects; \"generate\" = all")
e <- try(Batz:::util_save_mode("generate x", "test"), silent = TRUE); ok(inherits(e, "try-error"), "\"generate x\": error")
e <- try(Batz:::util_save_mode("pdf", "test"), silent = TRUE); ok(inherits(e, "try-error"), "\"pdf\": error lists the accepted values")
ok(identical(Batz:::util_save_objname("C:/x/acme_Test AYERS_20260626to20260629_20261117104914.png"), "acme_Test.AYERS_20260626to20260629"),
   "object name: no folder, extension or time stamp; a valid R name")

## ---- 3. a CSV function: batz.generate_plotopts ---------------------------------
d <- new.dir("plotopts")
f <- batz.generate_plotopts(c("batz.plot.heatmap_aru.time", "batz.plot.bar_activity.site"), project.name = "acme", dir.save = d)
ok(n.files(d) == 3 && length(f) == 3, "TRUE: 2 CSVs + pooled workbook saved")
d <- new.dir("plotopts.false")
f <- batz.generate_plotopts(c("batz.plot.heatmap_aru.time", "batz.plot.bar_activity.site"), project.name = "acme", dir.save = d, save.auto = FALSE)
ok(!dir.exists(d) && length(f) == 0, "FALSE: nothing saved, dir.save not created")
out <- capture.output(batz.generate_plotopts("batz.plot.heatmap_aru.time", project.name = "acme", dir.save = d, save.auto = "print"))
ok(!dir.exists(d) && any(grepl("acme_pltop.heatmap_aru.time.csv", out)) && any(grepl("font.family", out)), "\"print\": tables printed, nothing saved")
suppressWarnings(rm(list = intersect(ls(globalenv()), c("acme_pltop.heatmap_aru.time", "acme_pltop.bar_activity.site", "acme_pltop.pooled")), envir = globalenv()))
batz.generate_plotopts(c("batz.plot.heatmap_aru.time", "batz.plot.bar_activity.site"), project.name = "acme", dir.save = d, save.auto = "generate 2")
ok(exists("acme_pltop.heatmap_aru.time", globalenv()) && exists("acme_pltop.bar_activity.site", globalenv()) &&
     !exists("acme_pltop.pooled", globalenv()) && !dir.exists(d), "\"generate 2\": first 2 outputs made as objects, the 3rd not")
ok(is.data.frame(acme_pltop.heatmap_aru.time), "generated object is the table")
batz.generate_plotopts(c("batz.plot.heatmap_aru.time", "batz.plot.bar_activity.site"), project.name = "acme", dir.save = d, save.auto = "generate")
ok(is.list(acme_pltop.pooled) && "pltop.bar_activity.site" %in% names(acme_pltop.pooled), "\"generate\": all, workbook = list of its tabs")

## ---- 4. a plot function: batz.plot.heatmap_aru.time ---------------------------
t <- as.POSIXct("2026-06-26 20:02:59", tz = "UTC") + c(outer(60 * 0:581, 86400 * 0:3, "+"))
sm4 <- data.frame(aru.name = "AYERS", date = format(t, "%Y-%m-%d"), time = format(t, "%H:%M:%S"),
                  mic0.type = "U2", power.v = 6.4, temperature.c = 21.5, files = 1, scrubbed = 0)
pf <- batz.generate_plotframe.sm4(sm4, dir.save = NULL)
fl <- batz.generate_figlist(functions = "batz.plot.heatmap_aru.time", dir.save = NULL)$fig.list
fl$plot <- TRUE; fl$plot.set <- "AYERS"; fl$plot.name <- "Test AYERS"
fl$date.start <- "6/26/2026"; fl$date.end <- "6/29/2026"; fl$xaxe.interval <- "4"
fl2 <- rbind(fl, transform(fl, plot.name = "Second"))
d <- new.dir("heat")
r <- batz.plot.heatmap_aru.time(pf, fl2, project.name = "acme", dir.save = d)
ok(n.files(d) == 2, "TRUE: 2 PNGs saved")
d <- new.dir("heat.false")
pdf(file.path(tempdir(), "shown.pdf")); r <- batz.plot.heatmap_aru.time(pf, fl2, project.name = "acme", dir.save = d, save.auto = FALSE); dev.off()
ok(n.files(d) == 0 && length(r$ggplots) == 2 && file.size(file.path(tempdir(), "shown.pdf")) > 5000,
   "FALSE (plot function): no PNG; each plot printed instead; plots still returned")
d2 <- new.dir("heat.both")
pdf(file.path(tempdir(), "shown2.pdf")); r <- batz.plot.heatmap_aru.time(pf, fl2, project.name = "acme", dir.save = d2, save.auto = "BOTH"); dev.off()
ok(n.files(d2) == 2 && file.size(file.path(tempdir(), "shown2.pdf")) > 5000, "\"BOTH\": 2 PNGs saved and the plots printed")
pdf(NULL); out <- capture.output(batz.plot.heatmap_aru.time(pf, fl2, project.name = "acme", dir.save = d, save.auto = "print")); dev.off()
ok(n.files(d) == 0 && sum(grepl("^---- acme_", out)) == 2, "\"print\": 2 plots printed, nothing saved")
rm(list = ls(globalenv(), pattern = "^acme_(Test|Second)"), envir = globalenv())
batz.plot.heatmap_aru.time(pf, fl2, project.name = "acme", dir.save = d, save.auto = "generate 1")
made <- ls(globalenv(), pattern = "^acme_(Test|Second)")
print(made)
ok(identical(made, "acme_Test.AYERS_20260626to20260629") && inherits(get(made), "ggplot") && n.files(d) == 0,
   "\"generate 1\": one ggplot object named after the PNG, nothing saved")

## ---- 5. a workbook: batz.generate_figlist -------------------------------------
d <- new.dir("figlist")
x <- batz.generate_figlist(dir.save = d, save.auto = "generate 1", project.name = "acme")
ok(exists("acme_fig.list_template", globalenv()) && identical(names(acme_fig.list_template), c("fig.list", "descriptions")) && !dir.exists(d),
   "figlist \"generate 1\": list(fig.list, descriptions), nothing saved")

## ---- 6. vegetation plots: plot and table ---------------------------------------
q <- data.frame(project.name = "Falmouth", aru.name = rep(c("Falmouth1", "Falmouth2"), each = 5), measurement.units = "in",
  vegetation.tree_tree1.dbh = c("13.1", "13.5", "13.6", "15.5", "17.5", "4.5", "7", "9", "12", "10;9.3"),
  vegetation.snag_snag1.dbh = c(NA, NA, NA, NA, NA, 3.4, 3.5, 4.3, 6, 9.6))
d <- new.dir("dbh")
r <- batz.plot.box_vegetation.dbh(q, project.name = "Falmouth", dir.save = d, save.auto = "generate",
                                  tree.columns = "vegetation.tree_tree1.dbh", snag.columns = "vegetation.snag_snag1.dbh")
ok(exists("Falmouth_vegetation.dbh", globalenv()) && is.data.frame(get("Falmouth_vegetation.dbh_table", globalenv())) && !dir.exists(d),
   "box_vegetation.dbh \"generate\": the plot and the table as objects")

cat("\nALL TESTS PASSED\n")

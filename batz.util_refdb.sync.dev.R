# =============================================================================
# batz.util_refdb.sync.dev.R
# -----------------------------------------------------------------------------
# Batz_reference_db_<date>_v<N>.xlsx is the one master copy of every table the
# Batz functions use. After editing a table in it (or adding one: a tab plus a
# row in its database.names tab), run this script, then commit, push and
# reinstall Batz.
# =============================================================================

library(Batz)

dir.refdb <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/8reference tables"
dir.repo  <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/GitHub/Batz"

## 1. Copy the tables into the package source (R/sysdata.rda) - newest workbook in the folder
s <- batz.util_refdb.sync(refdb = dir.refdb, dir.repo = dir.repo)
s$file                     # workbook used
s$tables                   # name, tab, rows, columns

## 2. After reinstalling: the functions read the same tables
# pak::pak("jogui001/Batz")
head(Batz:::batz.db("bat.ref.names"))
head(Batz:::batz.db("plotopts.batz.plotactivity_observations"))
stopifnot(identical(names(Batz:::.batz.db), s$tables$name))

## 3. Starting plotopts files for a project come from the same tables
batz.generate_plotopts(generate.files = "batz.plotactivity_observations", project.name = "test",
                       dir.save = file.path(tempdir(), "plotopts_test"))

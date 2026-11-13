# =============================================================================
# batz.dependencies.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for the Batz package dependencies (DESCRIPTION Imports).
# 1. every package Batz needs is installed
# 2. the replacements give the same output as the packages they replaced:
#    - colorspace palettes  -> grDevices::hcl.colors()   (batz.plot.heatmap_activity.species)
#    - png::readPNG icons   -> magick                    (batz.plot.bullseye_clutter.mic)
#    - png trim of Figure 4 -> magick                    (batz.generate_aru.appendix)
#    - writexl              -> openxlsx only             (batz.merge_sitevisit.acoustics)
# Steps 2a-2b compare against colorspace / png, so install those two only to
# run this check: install.packages(c("colorspace", "png"))
# =============================================================================

## 1. Imports installed? (pak::pak("jogui001/Batz") installs these automatically)
imports  <- c("ggplot2", "readxl", "xml2", "jsonlite", "scales", "RColorBrewer", "viridisLite",
              "officer", "magick", "openxlsx", "patchwork")
suggests <- c("scico", "doconv")            # optional: scico palettes, Word -> PDF
print(data.frame(package = c(imports, suggests),
                 needed  = rep(c("Imports", "Suggests"), c(length(imports), length(suggests))),
                 installed = vapply(c(imports, suggests), requireNamespace, logical(1), quietly = TRUE)),
      row.names = FALSE)

library(Batz)

## 2a. colorspace palettes: base R gives the same colours
if (requireNamespace("colorspace", quietly = TRUE)) {
  same <- vapply(rownames(colorspace::hcl_palettes()), function(nm) {
    type <- tolower(as.character(colorspace::hcl_palettes()[nm, "type"]))
    a <- if (grepl("qualitative", type)) colorspace::qualitative_hcl(7, palette = nm) else
         if (grepl("flexible", type)) colorspace::divergingx_hcl(7, palette = nm) else
         if (grepl("diverging", type)) colorspace::diverging_hcl(7, palette = nm) else
         colorspace::sequential_hcl(7, palette = nm)
    identical(toupper(a), toupper(grDevices::hcl.colors(7, palette = nm)))
  }, logical(1))
  cat("colorspace palettes identical in base R:", sum(same), "of", length(same), "\n")   # expect all
}
Batz:::heatmap2.resolve.colors("colorspace::YlOrRd", 5, "discrete", "test")          # no colorspace needed

## 2b. bullseye mic icons: magick reads the same pixels as png
icons <- list.files(system.file("img", package = "Batz"), pattern = "\\.png$", full.names = TRUE)
if (requireNamespace("png", quietly = TRUE)) for (ic in icons) {
  a <- grDevices::col2rgb(as.vector(as.matrix(as.raster(png::readPNG(ic)))), alpha = TRUE)
  b <- grDevices::col2rgb(as.vector(as.matrix(grDevices::as.raster(magick::image_read(ic)))), alpha = TRUE)
  vis <- a[4, ] > 0                                         # fully transparent pixels have no colour
  cat(basename(ic), " same:", identical(a[4, ], b[4, ]) && all(a[, vis] == b[, vis]), "\n")   # expect TRUE
}

#' Special-character helpers (internal)
#'
#' Shared helpers that let \code{batz} functions read files with non-UTF-8
#' bytes (e.g. a degree sign saved by Excel as a single Latin-1 byte) and,
#' when \code{strip.special = TRUE}, replace accented letters and symbols in
#' headers and data with plain text. All \code{batz} functions that read a
#' file or take a data frame use them, so they behave the same way.
#'
#' @details
#' \describe{
#'   \item{\code{special.read.csv(file, ...)}}{Replacement for
#'     \code{utils::read.csv()}. Reads the file as UTF-8 (a leading BOM is
#'     skipped); if the file is not valid UTF-8 it is re-read as Latin-1,
#'     which accepts every byte. Always used, whatever \code{strip.special} is.
#'     A non-character or multi-element \code{file} is passed straight to
#'     \code{read.csv()}.}
#'   \item{\code{special.read.lines(file, ...)}}{Same idea for
#'     \code{readLines()}: reads as UTF-8, drops a leading BOM and converts
#'     lines that are not valid UTF-8 from Latin-1.}
#'   \item{\code{special.to.utf8(v)}}{Converts a character vector to UTF-8,
#'     treating invalid elements as Latin-1.}
#'   \item{\code{special.simplify(x)}}{Cleans a character vector. Each non-ASCII
#'     character listed in \code{special.simplify.map} is replaced by its plain
#'     equivalent: accented Latin letters to the base letter (\code{é} to
#'     \code{e}, \code{ß} to \code{ss}, \code{æ} to \code{ae}), \code{×} to
#'     \code{X}, curly quotes to straight quotes, dashes and minus sign to
#'     \code{-}, wide and non-breaking spaces to a space, \code{…} to
#'     \code{...}, \code{½} to \code{1/2}, \code{±} to \code{+/-}. Any other
#'     non-ASCII character (\code{°}, \code{µ}, \code{™}, emoji, BOM and
#'     zero-width marks) is removed. The table is fixed, so results do not
#'     depend on the locale. Returns \code{list(x, simplified, removed)}; the
#'     last two are logical vectors flagging the elements that had a character
#'     simplified or removed.}
#'   \item{\code{special.strip(df, strip.special = TRUE)}}{When
#'     \code{strip.special = TRUE}, runs \code{special.simplify()} on the column
#'     names and on every character or factor column. A column that changed is
#'     re-typed with \code{utils::type.convert()}, so \code{"42.036373°"}
#'     becomes the number \code{42.036373}. Returns \code{list(df, headers,
#'     counts)}: \code{headers} names the data columns that changed (cleaned
#'     names); \code{counts} is a named integer vector with
#'     \code{accented.letters.header} and \code{removed.symbols.header} (number
#'     of unique column names with a character simplified or removed) and
#'     \code{accented.letters.data} and \code{removed.symbols.data} (number of
#'     unique data values, across all columns, with a character simplified or
#'     removed). A value with both kinds counts in both. All counts are
#'     \code{NA} when \code{strip.special = FALSE} or \code{df} is not a data
#'     frame; \code{df} is then returned unchanged.}
#'   \item{\code{special.label(strip.special, headers)}}{Text for a log's
#'     \code{$strip.special} column: \code{"FALSE"}, \code{"TRUE NONE"}, or
#'     \code{"TRUE ; <header1>; <header2>"}.}
#'   \item{\code{special.log.cols(strip.special, headers = character(0), counts = NULL)}}{One-row
#'     data frame of the five log columns \code{$strip.special},
#'     \code{$accented.letters.header}, \code{$removed.symbols.header},
#'     \code{$accented.letters.data}, \code{$removed.symbols.data}. Counts are
#'     \code{NA} when \code{counts} is \code{NULL} or \code{strip.special} is
#'     not \code{TRUE}. \code{special.log.cols.empty()} is the zero-row version.
#'     \code{special.counts.add(a, b)} adds two count vectors (for log rows that
#'     cover several files); \code{NA} plus \code{NA} stays \code{NA}.}
#'   \item{\code{special.check.plotopts(df, arg.name, obj.name, fn.name)}}{Used
#'     by the plot functions when \code{strip.special.plotopts = FALSE}. If
#'     \code{df} (a fig.list or plot-options table) has column names or text
#'     that are not valid in this R session (e.g. a Latin-1 \code{°} byte), it
#'     stops with an error naming the input, the function and the total number
#'     of incompatible characters, and listing for each affected header the
#'     first 5 rows (\code{+} if there are more). Otherwise returns
#'     \code{NULL} invisibly.}
#' }
#'
#' @keywords internal
#' @noRd
special.read.csv <- function(file, ...) {
  if (!is.character(file) || length(file) != 1) return(utils::read.csv(file, ...))
  bad.encoding <- FALSE
  x <- tryCatch(
    withCallingHandlers(
      utils::read.csv(file, fileEncoding = "UTF-8-BOM", ...),
      warning = function(w) {
        if (grepl("invalid|multibyte|input string|incomplete", conditionMessage(w), ignore.case = TRUE)) {
          bad.encoding <<- TRUE
          invokeRestart("muffleWarning")
        }
      }),
    error = function(e) {
      if (grepl("invalid|multibyte|input string", conditionMessage(e), ignore.case = TRUE)) {
        bad.encoding <<- TRUE
        return(NULL)
      }
      stop(e)
    })
  if (is.null(x) || bad.encoding) x <- utils::read.csv(file, fileEncoding = "latin1", ...)
  x
}

#' @keywords internal
#' @noRd
special.read.lines <- function(file, ...) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8", ...)
  if (length(x) > 0) {
    x[1] <- sub("^\ufeff", "", x[1])
    bad <- !validUTF8(x)
    if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  }
  x
}

#' @keywords internal
#' @noRd
special.to.utf8 <- function(v) {
  v <- enc2utf8(v)
  bad <- !is.na(v) & !validUTF8(v)
  if (any(bad)) v[bad] <- iconv(v[bad], from = "latin1", to = "UTF-8")
  v
}

#' Plain-text equivalents for accented letters and common symbols (internal).
#' Named character vector built from the Unicode character database: each
#' Latin letter with accents or marks maps to its base letter, plus ligatures
#' and common symbols. Used by \code{special.simplify()}.
#' @keywords internal
#' @noRd
special.simplify.map <- c(
    "\u00c0" = "A", "\u00c1" = "A", "\u00c2" = "A", "\u00c3" = "A", "\u00c4" = "A", "\u00c5" = "A",
    "\u00c7" = "C", "\u00c8" = "E", "\u00c9" = "E", "\u00ca" = "E", "\u00cb" = "E", "\u00cc" = "I",
    "\u00cd" = "I", "\u00ce" = "I", "\u00cf" = "I", "\u00d1" = "N", "\u00d2" = "O", "\u00d3" = "O",
    "\u00d4" = "O", "\u00d5" = "O", "\u00d6" = "O", "\u00d9" = "U", "\u00da" = "U", "\u00db" = "U",
    "\u00dc" = "U", "\u00dd" = "Y", "\u00e0" = "a", "\u00e1" = "a", "\u00e2" = "a", "\u00e3" = "a",
    "\u00e4" = "a", "\u00e5" = "a", "\u00e7" = "c", "\u00e8" = "e", "\u00e9" = "e", "\u00ea" = "e",
    "\u00eb" = "e", "\u00ec" = "i", "\u00ed" = "i", "\u00ee" = "i", "\u00ef" = "i", "\u00f1" = "n",
    "\u00f2" = "o", "\u00f3" = "o", "\u00f4" = "o", "\u00f5" = "o", "\u00f6" = "o", "\u00f9" = "u",
    "\u00fa" = "u", "\u00fb" = "u", "\u00fc" = "u", "\u00fd" = "y", "\u00ff" = "y", "\u0100" = "A",
    "\u0101" = "a", "\u0102" = "A", "\u0103" = "a", "\u0104" = "A", "\u0105" = "a", "\u0106" = "C",
    "\u0107" = "c", "\u0108" = "C", "\u0109" = "c", "\u010a" = "C", "\u010b" = "c", "\u010c" = "C",
    "\u010d" = "c", "\u010e" = "D", "\u010f" = "d", "\u0112" = "E", "\u0113" = "e", "\u0114" = "E",
    "\u0115" = "e", "\u0116" = "E", "\u0117" = "e", "\u0118" = "E", "\u0119" = "e", "\u011a" = "E",
    "\u011b" = "e", "\u011c" = "G", "\u011d" = "g", "\u011e" = "G", "\u011f" = "g", "\u0120" = "G",
    "\u0121" = "g", "\u0122" = "G", "\u0123" = "g", "\u0124" = "H", "\u0125" = "h", "\u0128" = "I",
    "\u0129" = "i", "\u012a" = "I", "\u012b" = "i", "\u012c" = "I", "\u012d" = "i", "\u012e" = "I",
    "\u012f" = "i", "\u0130" = "I", "\u0134" = "J", "\u0135" = "j", "\u0136" = "K", "\u0137" = "k",
    "\u0139" = "L", "\u013a" = "l", "\u013b" = "L", "\u013c" = "l", "\u013d" = "L", "\u013e" = "l",
    "\u0143" = "N", "\u0144" = "n", "\u0145" = "N", "\u0146" = "n", "\u0147" = "N", "\u0148" = "n",
    "\u014c" = "O", "\u014d" = "o", "\u014e" = "O", "\u014f" = "o", "\u0150" = "O", "\u0151" = "o",
    "\u0154" = "R", "\u0155" = "r", "\u0156" = "R", "\u0157" = "r", "\u0158" = "R", "\u0159" = "r",
    "\u015a" = "S", "\u015b" = "s", "\u015c" = "S", "\u015d" = "s", "\u015e" = "S", "\u015f" = "s",
    "\u0160" = "S", "\u0161" = "s", "\u0162" = "T", "\u0163" = "t", "\u0164" = "T", "\u0165" = "t",
    "\u0168" = "U", "\u0169" = "u", "\u016a" = "U", "\u016b" = "u", "\u016c" = "U", "\u016d" = "u",
    "\u016e" = "U", "\u016f" = "u", "\u0170" = "U", "\u0171" = "u", "\u0172" = "U", "\u0173" = "u",
    "\u0174" = "W", "\u0175" = "w", "\u0176" = "Y", "\u0177" = "y", "\u0178" = "Y", "\u0179" = "Z",
    "\u017a" = "z", "\u017b" = "Z", "\u017c" = "z", "\u017d" = "Z", "\u017e" = "z", "\u01a0" = "O",
    "\u01a1" = "o", "\u01af" = "U", "\u01b0" = "u", "\u01cd" = "A", "\u01ce" = "a", "\u01cf" = "I",
    "\u01d0" = "i", "\u01d1" = "O", "\u01d2" = "o", "\u01d3" = "U", "\u01d4" = "u", "\u01d5" = "U",
    "\u01d6" = "u", "\u01d7" = "U", "\u01d8" = "u", "\u01d9" = "U", "\u01da" = "u", "\u01db" = "U",
    "\u01dc" = "u", "\u01de" = "A", "\u01df" = "a", "\u01e0" = "A", "\u01e1" = "a", "\u01e6" = "G",
    "\u01e7" = "g", "\u01e8" = "K", "\u01e9" = "k", "\u01ea" = "O", "\u01eb" = "o", "\u01ec" = "O",
    "\u01ed" = "o", "\u01f0" = "j", "\u01f4" = "G", "\u01f5" = "g", "\u01f8" = "N", "\u01f9" = "n",
    "\u01fa" = "A", "\u01fb" = "a", "\u0200" = "A", "\u0201" = "a", "\u0202" = "A", "\u0203" = "a",
    "\u0204" = "E", "\u0205" = "e", "\u0206" = "E", "\u0207" = "e", "\u0208" = "I", "\u0209" = "i",
    "\u020a" = "I", "\u020b" = "i", "\u020c" = "O", "\u020d" = "o", "\u020e" = "O", "\u020f" = "o",
    "\u0210" = "R", "\u0211" = "r", "\u0212" = "R", "\u0213" = "r", "\u0214" = "U", "\u0215" = "u",
    "\u0216" = "U", "\u0217" = "u", "\u0218" = "S", "\u0219" = "s", "\u021a" = "T", "\u021b" = "t",
    "\u021e" = "H", "\u021f" = "h", "\u0226" = "A", "\u0227" = "a", "\u0228" = "E", "\u0229" = "e",
    "\u022a" = "O", "\u022b" = "o", "\u022c" = "O", "\u022d" = "o", "\u022e" = "O", "\u022f" = "o",
    "\u0230" = "O", "\u0231" = "o", "\u0232" = "Y", "\u0233" = "y", "\u1e00" = "A", "\u1e01" = "a",
    "\u1e02" = "B", "\u1e03" = "b", "\u1e04" = "B", "\u1e05" = "b", "\u1e06" = "B", "\u1e07" = "b",
    "\u1e08" = "C", "\u1e09" = "c", "\u1e0a" = "D", "\u1e0b" = "d", "\u1e0c" = "D", "\u1e0d" = "d",
    "\u1e0e" = "D", "\u1e0f" = "d", "\u1e10" = "D", "\u1e11" = "d", "\u1e12" = "D", "\u1e13" = "d",
    "\u1e14" = "E", "\u1e15" = "e", "\u1e16" = "E", "\u1e17" = "e", "\u1e18" = "E", "\u1e19" = "e",
    "\u1e1a" = "E", "\u1e1b" = "e", "\u1e1c" = "E", "\u1e1d" = "e", "\u1e1e" = "F", "\u1e1f" = "f",
    "\u1e20" = "G", "\u1e21" = "g", "\u1e22" = "H", "\u1e23" = "h", "\u1e24" = "H", "\u1e25" = "h",
    "\u1e26" = "H", "\u1e27" = "h", "\u1e28" = "H", "\u1e29" = "h", "\u1e2a" = "H", "\u1e2b" = "h",
    "\u1e2c" = "I", "\u1e2d" = "i", "\u1e2e" = "I", "\u1e2f" = "i", "\u1e30" = "K", "\u1e31" = "k",
    "\u1e32" = "K", "\u1e33" = "k", "\u1e34" = "K", "\u1e35" = "k", "\u1e36" = "L", "\u1e37" = "l",
    "\u1e38" = "L", "\u1e39" = "l", "\u1e3a" = "L", "\u1e3b" = "l", "\u1e3c" = "L", "\u1e3d" = "l",
    "\u1e3e" = "M", "\u1e3f" = "m", "\u1e40" = "M", "\u1e41" = "m", "\u1e42" = "M", "\u1e43" = "m",
    "\u1e44" = "N", "\u1e45" = "n", "\u1e46" = "N", "\u1e47" = "n", "\u1e48" = "N", "\u1e49" = "n",
    "\u1e4a" = "N", "\u1e4b" = "n", "\u1e4c" = "O", "\u1e4d" = "o", "\u1e4e" = "O", "\u1e4f" = "o",
    "\u1e50" = "O", "\u1e51" = "o", "\u1e52" = "O", "\u1e53" = "o", "\u1e54" = "P", "\u1e55" = "p",
    "\u1e56" = "P", "\u1e57" = "p", "\u1e58" = "R", "\u1e59" = "r", "\u1e5a" = "R", "\u1e5b" = "r",
    "\u1e5c" = "R", "\u1e5d" = "r", "\u1e5e" = "R", "\u1e5f" = "r", "\u1e60" = "S", "\u1e61" = "s",
    "\u1e62" = "S", "\u1e63" = "s", "\u1e64" = "S", "\u1e65" = "s", "\u1e66" = "S", "\u1e67" = "s",
    "\u1e68" = "S", "\u1e69" = "s", "\u1e6a" = "T", "\u1e6b" = "t", "\u1e6c" = "T", "\u1e6d" = "t",
    "\u1e6e" = "T", "\u1e6f" = "t", "\u1e70" = "T", "\u1e71" = "t", "\u1e72" = "U", "\u1e73" = "u",
    "\u1e74" = "U", "\u1e75" = "u", "\u1e76" = "U", "\u1e77" = "u", "\u1e78" = "U", "\u1e79" = "u",
    "\u1e7a" = "U", "\u1e7b" = "u", "\u1e7c" = "V", "\u1e7d" = "v", "\u1e7e" = "V", "\u1e7f" = "v",
    "\u1e80" = "W", "\u1e81" = "w", "\u1e82" = "W", "\u1e83" = "w", "\u1e84" = "W", "\u1e85" = "w",
    "\u1e86" = "W", "\u1e87" = "w", "\u1e88" = "W", "\u1e89" = "w", "\u1e8a" = "X", "\u1e8b" = "x",
    "\u1e8c" = "X", "\u1e8d" = "x", "\u1e8e" = "Y", "\u1e8f" = "y", "\u1e90" = "Z", "\u1e91" = "z",
    "\u1e92" = "Z", "\u1e93" = "z", "\u1e94" = "Z", "\u1e95" = "z", "\u1e96" = "h", "\u1e97" = "t",
    "\u1e98" = "w", "\u1e99" = "y", "\u1ea0" = "A", "\u1ea1" = "a", "\u1ea2" = "A", "\u1ea3" = "a",
    "\u1ea4" = "A", "\u1ea5" = "a", "\u1ea6" = "A", "\u1ea7" = "a", "\u1ea8" = "A", "\u1ea9" = "a",
    "\u1eaa" = "A", "\u1eab" = "a", "\u1eac" = "A", "\u1ead" = "a", "\u1eae" = "A", "\u1eaf" = "a",
    "\u1eb0" = "A", "\u1eb1" = "a", "\u1eb2" = "A", "\u1eb3" = "a", "\u1eb4" = "A", "\u1eb5" = "a",
    "\u1eb6" = "A", "\u1eb7" = "a", "\u1eb8" = "E", "\u1eb9" = "e", "\u1eba" = "E", "\u1ebb" = "e",
    "\u1ebc" = "E", "\u1ebd" = "e", "\u1ebe" = "E", "\u1ebf" = "e", "\u1ec0" = "E", "\u1ec1" = "e",
    "\u1ec2" = "E", "\u1ec3" = "e", "\u1ec4" = "E", "\u1ec5" = "e", "\u1ec6" = "E", "\u1ec7" = "e",
    "\u1ec8" = "I", "\u1ec9" = "i", "\u1eca" = "I", "\u1ecb" = "i", "\u1ecc" = "O", "\u1ecd" = "o",
    "\u1ece" = "O", "\u1ecf" = "o", "\u1ed0" = "O", "\u1ed1" = "o", "\u1ed2" = "O", "\u1ed3" = "o",
    "\u1ed4" = "O", "\u1ed5" = "o", "\u1ed6" = "O", "\u1ed7" = "o", "\u1ed8" = "O", "\u1ed9" = "o",
    "\u1eda" = "O", "\u1edb" = "o", "\u1edc" = "O", "\u1edd" = "o", "\u1ede" = "O", "\u1edf" = "o",
    "\u1ee0" = "O", "\u1ee1" = "o", "\u1ee2" = "O", "\u1ee3" = "o", "\u1ee4" = "U", "\u1ee5" = "u",
    "\u1ee6" = "U", "\u1ee7" = "u", "\u1ee8" = "U", "\u1ee9" = "u", "\u1eea" = "U", "\u1eeb" = "u",
    "\u1eec" = "U", "\u1eed" = "u", "\u1eee" = "U", "\u1eef" = "u", "\u1ef0" = "U", "\u1ef1" = "u",
    "\u1ef2" = "Y", "\u1ef3" = "y", "\u1ef4" = "Y", "\u1ef5" = "y", "\u1ef6" = "Y", "\u1ef7" = "y",
    "\u1ef8" = "Y", "\u1ef9" = "y", "\u00c6" = "AE", "\u00e6" = "ae", "\u0152" = "OE", "\u0153" = "oe",
    "\u00df" = "ss", "\u00d8" = "O", "\u00f8" = "o", "\u0110" = "D", "\u0111" = "d", "\u0141" = "L",
    "\u0142" = "l", "\u00de" = "Th", "\u00fe" = "th", "\u00d0" = "D", "\u00f0" = "d", "\u0131" = "i",
    "\u0126" = "H", "\u0127" = "h", "\u0132" = "IJ", "\u0133" = "ij", "\u0166" = "T", "\u0167" = "t",
    "\u00d7" = "X", "\u2018" = "'", "\u2019" = "'", "\u201a" = "'", "\u201b" = "'", "\u2032" = "'",
    "\u201c" = "\"", "\u201d" = "\"", "\u201e" = "\"", "\u2033" = "\"", "\u2013" = "-", "\u2014" = "-",
    "\u2012" = "-", "\u2010" = "-", "\u2011" = "-", "\u2212" = "-", "\u00a0" = " ", "\u2009" = " ",
    "\u202f" = " ", "\u2002" = " ", "\u2003" = " ", "\u2026" = "...", "\u00bd" = "1/2", "\u00bc" = "1/4",
    "\u00be" = "3/4", "\u00b1" = "+/-"
)

#' @keywords internal
#' @noRd
special.simplify <- function(x) {
  x <- special.to.utf8(as.character(x))
  n <- length(x)
  simplified <- removed <- logical(n)
  has <- !is.na(x) & grepl("[^\\x01-\\x7F]", x, perl = TRUE)
  if (!any(has)) return(list(x = x, simplified = simplified, removed = removed))
  u <- unique(x[has])
  res <- lapply(strsplit(u, "", fixed = TRUE), function(ch) {
    na <- !grepl("^[\\x01-\\x7F]$", ch, perl = TRUE)
    hit <- na & ch %in% names(special.simplify.map)
    ch[hit] <- special.simplify.map[ch[hit]]
    gone <- na & !hit
    ch[gone] <- ""
    list(s = paste(ch, collapse = ""), simp = any(hit), rem = any(gone))
  })
  idx <- match(x[has], u)
  x[has] <- vapply(res, `[[`, "", "s")[idx]
  simplified[has] <- vapply(res, `[[`, TRUE, "simp")[idx]
  removed[has] <- vapply(res, `[[`, TRUE, "rem")[idx]
  list(x = x, simplified = simplified, removed = removed)
}

#' @keywords internal
#' @noRd
special.strip <- function(df, strip.special = TRUE) {
  na.counts <- c(accented.letters.header = NA_integer_, removed.symbols.header = NA_integer_,
                 accented.letters.data = NA_integer_, removed.symbols.data = NA_integer_)
  if (!isTRUE(strip.special) || !is.data.frame(df)) {
    return(list(df = df, headers = character(0), counts = na.counts))
  }
  counts <- c(accented.letters.header = 0L, removed.symbols.header = 0L,
              accented.letters.data = 0L, removed.symbols.data = 0L)
  if (ncol(df) == 0) return(list(df = df, headers = character(0), counts = counts))
  ## headers
  h <- special.simplify(names(df))
  counts[["accented.letters.header"]] <- length(unique(names(df)[h$simplified]))
  counts[["removed.symbols.header"]]  <- length(unique(names(df)[h$removed]))
  if (any(h$simplified | h$removed)) names(df) <- h$x
  ## data
  hits <- character(0); simp.vals <- character(0); rem.vals <- character(0)
  for (i in seq_along(df)) {
    v <- df[[i]]
    was.factor <- is.factor(v)
    if (was.factor) v <- as.character(v)
    if (!is.character(v)) next
    s <- special.simplify(v)
    if (!any(s$simplified | s$removed)) next
    simp.vals <- c(simp.vals, unique(special.to.utf8(v)[s$simplified]))
    rem.vals  <- c(rem.vals,  unique(special.to.utf8(v)[s$removed]))
    df[[i]] <- utils::type.convert(s$x, as.is = !was.factor)
    hits <- c(hits, names(df)[i])
  }
  counts[["accented.letters.data"]] <- length(unique(simp.vals))
  counts[["removed.symbols.data"]]  <- length(unique(rem.vals))
  list(df = df, headers = unique(hits), counts = counts)
}

#' @keywords internal
#' @noRd
special.label <- function(strip.special, headers) {
  if (!isTRUE(strip.special)) return("FALSE")
  headers <- unique(headers[!is.na(headers) & nzchar(headers)])
  if (length(headers) == 0) return("TRUE NONE")
  paste0("TRUE ; ", paste(headers, collapse = "; "))
}

#' @keywords internal
#' @noRd
special.counts.add <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  out <- a
  for (k in names(a)) out[[k]] <- if (is.na(a[[k]]) && is.na(b[[k]])) NA_integer_ else
    sum(c(a[[k]], b[[k]]), na.rm = TRUE)
  out
}

#' @keywords internal
#' @noRd
special.log.cols <- function(strip.special, headers = character(0), counts = NULL) {
  if (is.null(counts) || !isTRUE(strip.special)) {
    counts <- c(accented.letters.header = NA_integer_, removed.symbols.header = NA_integer_,
                accented.letters.data = NA_integer_, removed.symbols.data = NA_integer_)
  }
  data.frame(strip.special = special.label(strip.special, headers),
             accented.letters.header = as.integer(counts[["accented.letters.header"]]),
             removed.symbols.header  = as.integer(counts[["removed.symbols.header"]]),
             accented.letters.data   = as.integer(counts[["accented.letters.data"]]),
             removed.symbols.data    = as.integer(counts[["removed.symbols.data"]]),
             stringsAsFactors = FALSE)
}

#' @keywords internal
#' @noRd
special.log.cols.empty <- function() special.log.cols(FALSE)[0, ]

#' @keywords internal
#' @noRd
special.check.plotopts <- function(df, arg.name, obj.name, fn.name) {
  if (!is.data.frame(df)) return(invisible(NULL))
  bad.bytes <- function(v) {
    v <- as.character(v)
    bad <- !is.na(v) & !validUTF8(v) & Encoding(v) != "latin1"
    list(bad = bad, n = if (any(bad)) sum(vapply(v[bad], function(s) sum(as.integer(charToRaw(s)) > 127L), 0)) else 0)
  }
  lines <- character(0); total <- 0
  hb <- bad.bytes(names(df))
  if (any(hb$bad)) {
    total <- total + hb$n
    lines <- c(lines, paste0("column names = ", paste(which(hb$bad)[seq_len(min(5, sum(hb$bad)))], collapse = ","),
                             if (sum(hb$bad) > 5) ",+" else ""))
  }
  for (i in seq_along(df)) {
    v <- df[[i]]
    if (is.factor(v)) v <- as.character(v)
    if (!is.character(v)) next
    b <- bad.bytes(v)
    if (!any(b$bad)) next
    total <- total + b$n
    rows <- which(b$bad)
    nm <- names(df)[i]
    if (!validUTF8(nm)) nm <- paste0("column ", i)
    lines <- c(lines, paste0(nm, " = ", paste(rows[seq_len(min(5, length(rows)))], collapse = ","),
                             if (length(rows) > 5) ",+" else ""))
  }
  if (total > 0) {
    show.obj <- !is.null(obj.name) && length(obj.name) == 1 && nzchar(obj.name) &&
    obj.name != arg.name && nchar(obj.name) <= 60 && !grepl("[()\n]", obj.name)
  stop("file ", arg.name, if (show.obj) paste0(" (object '", obj.name, "')"),
         " used in function \"", fn.name, "\" has ", total,
         " incompatible special character(s) in one or more headings preventing loading, ",
         "the first 5 or less instances are found in these headers and rows\n",
         paste(lines, collapse = "\n"),
         "\nFix these cells, re-save the file as CSV UTF-8, or run with strip.special.plotopts = TRUE.",
         call. = FALSE)
  }
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  # santoku >= 1.1 labels infinite breaks with the infinity symbol, which
  # ggplot2 binned legends (guide_coloursteps) can't parse
  if (is.null(getOption("santoku.infinity"))) {
    options(santoku.infinity = "Inf")
  }
}

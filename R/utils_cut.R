#' Reformat cut labels.
#'
#' @param x A string vector of labels
#'
#' @return A string vector with clean labels
#' @export
cut_relabel <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "-Inf")  ~ stringr::str_replace(x, "\\(-Inf,([^\\]]*)]", "< \\1"),
    stringr::str_detect(x, "Inf")   ~ stringr::str_replace(x, "\\(([^,]*), Inf]", "> \\1"),
    TRUE                            ~ stringr::str_replace(x, "\\(([^,]*),([^\\]]*)]", "\\1 - \\2")
  )
}

#' Reformat santoku chop dash labels.
#'
#' @param x A string vector of labels
#'
#' @return A string vector with clean labels
#' @export
chop_relabel_dash <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "-Inf")  ~ stringr::str_replace(x, "-Inf\\s-\\s(.*)", "< \\1"),
    stringr::str_detect(x, "Inf")   ~ stringr::str_replace(x, "(.*?)\\s-\\sInf", "> \\1"),
    TRUE                            ~ x
  )
}

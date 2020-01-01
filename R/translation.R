#' Translate function
#'
#' @param english English string
#' @param french French string
#'
#' @return String depending on package language option
#' @export
translate <- function(english, french) {
  language <- geo_pkg_options("language")

  dplyr::case_when(
    language == "en" ~ english,
    language == "fr" ~ french,
    TRUE ~ english
  )
}

#' Creates translation function for named string list
#'
#' @param dictionary A list of translations
#'
#' @return A translated string
#' @export
translator <- function(dictionary) {
  function(word) {
    purrr::map_chr(
      word,
      ~ ifelse(exists(.x, where = dictionary),
               return(dictionary[[.x]]),
               return(.x))
    )
  }
}

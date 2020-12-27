#' Get Swiss canton ids
#'
#' @param canton_name A vector of Swiss canton names
#'
#' @return A vector of Swiss canton FSO ids
#' @export
geo_swiss_canton_id <- function(canton_name) {
  countrycode::countrycode(
    sourcevar = canton_name,
    origin = "canton.name.regex",
    destination = "fso.number",
    custom_dict = internal$swiss_cantons,
    origin_regex = TRUE
  )
}

#' Get Swiss canton abbreviations
#'
#' @param canton_name A vector of Swiss canton names
#'
#' @return A vector of Swiss canton abbreviations
#' @export
geo_swiss_canton_code <- function(canton_name) {
  countrycode::countrycode(
    sourcevar = canton_name,
    origin = "canton.name.regex",
    destination = "abbreviation",
    custom_dict = internal$swiss_cantons,
    origin_regex = TRUE
  )
}

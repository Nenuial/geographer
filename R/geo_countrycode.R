#' Get country FIPS code
#'
#' @param country_name
#'
#' @return A FIPS country code
#' @export
get_country_fips <- function(country_name) {
  countrycode::countrycode(country_name, "country.name", "fips")
}

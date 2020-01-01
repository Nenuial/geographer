#' One year population pyramid data
#' @param country A FIPS code for the country
#' @param year An integer with the year
#'
#' @return A dataframe with age, female, and male population
#'
#' @export
get_idb_pyramid_1y_data <- function(country, year) {
  idb_api_check()

  male <- idbr::idb1(country, toString(year), sex = 'male') %>%
    dplyr::rename(MalePop = POP)

  female <- idbr::idb1(country, toString(year), sex = 'female') %>%
    dplyr::rename(FemalePop = POP)

  population <- dplyr::left_join(male, female, by = "AGE") %>%
    dplyr::select(age = AGE, male = MalePop, female = FemalePop) %>%
    dplyr::arrange(dplyr::desc(age))

  return(population)
}

#' Five year population pyramid data
#' @param country A FIPS code for the country
#' @param year An integer with the year
#'
#' @return A dataframe with age, female, and male population
#'
#' @export
get_idb_pyramid_5y_data <- function(country, year) {
  idb_api_check()

  idbr::idb5(country, toString(year),
             concept = "Female midyear population by 5-year age groups") %>%
    dplyr::select(tidyselect::contains("FPOP")) %>%
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = "age", names_pattern = "FPOP(.*)",
                        values_to = "female") -> female

  idbr::idb5(country, toString(year),
             concept = "Male midyear population by 5-year age groups") %>%
    dplyr::select(tidyselect::contains("MPOP")) %>%
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = "age", names_pattern = "MPOP(.*)",
                        values_to = "male") -> male

  population <- dplyr::left_join(male, female, by = "age") %>%
    dplyr::mutate(order = as.numeric(stringr::str_extract(age, "(^[\\d]*)")),
                  age = stringr::str_replace(age, "_", "-"),
                  age = stringr::str_replace(age, "100-", "100+")) %>%
    dplyr::arrange(dplyr::desc(order)) %>%
    dplyr::select(-order)

  return(population)
}

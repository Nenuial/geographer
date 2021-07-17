#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom ggplot2 %+%
#' @export
ggplot2::`%+%`

#' @importFrom rlang !!
#' @export
rlang::`!!`

# Function: return WB country list for Shiny choice selector
#' @export
country_list_wb <- function() {
  df <- wbstats::wbcountries()

  df %<>% dplyr::filter(region != "Aggregates")

  list <- as.list(df$iso3c)
  names(list) <- df$country

  return(list)
}

# Function: return NOAA country list for Shiny choice selector
#' @export
country_list_noaa <- function() {
  cachedir <- rappdirs::user_cache_dir("geographer")
  filename <- file.path(cachedir, "ncdc_cities.RData")
  if (!file.exists(filename)) update_city_list()

  df <- readRDS(filename)

  df %<>% select(iso, country) %>% unique() %>% arrange(country)

  list <- as.list(df$iso)
  names(list) <- df$country

  return(list)
}

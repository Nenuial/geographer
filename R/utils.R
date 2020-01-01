#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom rlang !!
#' @export
rlang::`!!`

# Function: check if the IBD API has been specified.
idb_api_check <- function(api_key = NULL) {
  if (Sys.getenv('IDB_API') != '') {
    api_key <- Sys.getenv('IDB_API')
  } else if (is.null(api_key)) {
    stop('A Census API key is required for this function to work. Supply one through idbr::idb_api_key.')
  }
}

country_list_idb <- function() {

}

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

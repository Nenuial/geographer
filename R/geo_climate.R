#' Function: retrieve climate data from the rnoaa::ncdc NOAA API and compute averages over the
#' time period 1990-2019.
#'
#' @param station_id The string for the station ID
#' @param data_type The type of data needed. Can be "temperature" or "precipitation".
#'
#' @return A vectore with climate data (precipitation or temperature)
#' @export
get_noaa_climate_data <- function(location_id, data_type = "temperature") {
  data_id <- dplyr::case_when(data_type == "temperature" ~ "MNTM",
                              data_type == "precipitation" ~ "TPCP")

  out <- rnoaa::ncdc(datasetid = "GHCNDMS", locationid = location_id, datatypeid = data_id,
              startdate = "1990-01-01", enddate = "1999-12-31", limit = 500)
  dat <- out$data
  out <- rnoaa::ncdc(datasetid = "GHCNDMS", locationid = location_id, datatypeid = data_id,
              startdate = "2000-01-01", enddate = "2009-12-31", limit = 500)
  dat <- rbind(dat, out$data)
  out <- rnoaa::ncdc(datasetid = "GHCNDMS", locationid = location_id, datatypeid = data_id,
              startdate = "2010-01-01", enddate = "2019-12-31", limit = 500)
  dat <- rbind(dat, out$data)

  dat %>%
    dplyr::mutate(month = lubridate::month(lubridate::ymd_hms(date))) %>%
    dplyr::select(month, value) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::mutate(value = round(value / 10, digits = 1)) -> clean_data

  return(clean_data)
}

#' Read NCDC city list from cachedir
#'
#' @return A dataframe of NCDC locations (cities)
#' @export
get_ncdc_city_list <- function() {
  cachedir <- rappdirs::user_cache_dir("geographer")
  filename <- file.path(cachedir, "ncdc_cities.RData")
  if (!file.exists(filename)) update_ncdc_city_list()

  fileage <- lubridate::interval(start = fs::file_info(filename)$change_time,
                                  end = lubridate::now())
  if (lubridate::day(lubridate::as.period(fileage)) > 200) update_ncdc_city_list()

  return(readRDS(filename))
}

#' Function: update the list of cities available for climate data, compute the
#' corresponding country column and store the data locally.
#'
#' @export
update_ncdc_city_list <- function() {
  cachedir <- rappdirs::user_cache_dir("geographer")
  filename <- file.path(cachedir, "ncdc_cities.RData")
  if (!file.exists(cachedir)) dir.create(cachedir, recursive = TRUE)

  out <- rnoaa::ncdc_locs(datasetid = "GHCNDMS", locationcategoryid = "CITY", limit = 1000)
  dat <- out$data
  out <- rnoaa::ncdc_locs(datasetid = "GHCNDMS", locationcategoryid = "CITY", limit = 1000, offset = 1001)
  dat <- rbind(dat, out$data)

  dat |>
    dplyr::mutate(mindate = lubridate::ymd(mindate),
                  maxdate = lubridate::ymd(maxdate),
                  iso = stringr::str_sub(name, -2),
                  city = stringr::str_extract(name, "^[^,]*"),
                  country = countrycode::countrycode(iso, "fips", "country.name",
                                                     custom_match = c("NT" = "Curaçao",
                                                                      "RB" = "Serbia"),
                                                     warn = FALSE)) |>
    dplyr::mutate(address = glue::glue("{city}, {country}")) |>
    stats::na.omit() |>
    dplyr::filter(mindate < lubridate::ymd("1990-01-01"),
                  maxdate > lubridate::ymd("2015-01-01"),
                  datacoverage >= .9) |>
    tidygeocoder::geocode(address = address, method = "osm") -> cities

  saveRDS(cities, file = filename)
}

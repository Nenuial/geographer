# Package data
undp_indicators <- jsonlite::fromJSON(here::here("inst/extdata/un_hdr_2019.json")) %>%
  dplyr::mutate_at(c("indicator_id", "year", "value"), readr::parse_number)

usethis::use_data(undp_indicators, overwrite = T)

# Internal package data
internal <- list(
  "boundbox" = sf::read_sf(here::here("inst/extdata/ne_10m_wgs84_bounding_box/ne_10m_wgs84_bounding_box.shp")),
  "swiss_cantons" = readxl::read_excel(here::here("inst/extdata/ch_cantons.xlsx"))
)
usethis::use_data(internal, overwrite = T, internal = T)

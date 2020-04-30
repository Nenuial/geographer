undp_indicators <- jsonlite::fromJSON(here::here("data-raw/un_hdr_2019.json")) %>%
  dplyr::mutate_at(c("indicator_id", "year", "value"), readr::parse_number)
usethis::use_data(undp_indicators, overwrite = T)

internal <- list(
  "boundbox" = sf::read_sf(here::here("data-raw/ne_10m_wgs84_bounding_box/ne_10m_wgs84_bounding_box.shp"))
)
usethis::use_data(internal, overwrite = T, internal = T)

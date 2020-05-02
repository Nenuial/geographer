undp_indicators <- jsonlite::fromJSON(here::here("data-raw/un_hdr_2019.json")) %>%
  dplyr::mutate_at(c("indicator_id", "year", "value"), readr::parse_number)
usethis::use_data(undp_indicators, overwrite = T)

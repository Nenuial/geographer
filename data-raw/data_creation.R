# Internal package data
internal <- list(
  "boundbox" = sf::read_sf(
    here::here(
      "inst/extdata/ne_10m_wgs84_bounding_box/ne_10m_wgs84_bounding_box.shp"
    )
  )
)

usethis::use_data(internal, overwrite = TRUE, internal = TRUE)

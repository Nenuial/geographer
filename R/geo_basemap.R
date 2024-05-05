#' Get a base map for a country
#'
#' This function renders a base map with the country
#' highlighted.
#'
#' @param country Country to highlight
#'
#' @return A leaflet map with the country borders
#' @export
#' @examples
#' # Using Leaflet
#' gph_base_map("Switzerland")
#' gph_base_map("United Kingdom")
#'
#' # Using tmap (v4)
#' gph_base_tmap("Switzerland")
#' gph_base_tmap("United Kingdom")
#'
gph_base_map <- function(country) {
  border <- rnaturalearth::ne_countries(
    scale = 10,
    country = country,
    returnclass = "sf"
  )

  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    leaflet::addPolygons(data = border, color = "red", weight = 2)
}

#' @rdname gph_base_map
#' @export
gph_base_tmap <- function(country) {
  border <- rnaturalearth::ne_countries(
    scale = 10,
    country = country,
    returnclass = "sf"
  )

  # The tmap version doesn't work in Quarto for now…
  tmap::tmap_mode("view")
  tmap::tm_basemap(leaflet::providers$OpenStreetMap) +
    tmap::tm_shape(border) +
    tmap::tm_borders(col = "red", lwd = 2, group.control = "none")
}

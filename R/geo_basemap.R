#' Get a basemap for a country
#'
#' @param country The country to get the basemap for
#'
#' @return A leaflet map with the country borders
#' @export
gph_basemap <- function(country) {
  border = rnaturalearth::ne_countries(
    scale = 10, country = country,
    returnclass = "sf")

  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$Stadia.OSMBright) |>
    leaflet::addPolygons(data = border, color = "red", weight = 2)

  # The tmap version doesn't work in Quarto for now…
  # tmap::tmap_mode("view")
  # tmap::tm_basemap(leaflet::providers$Stadia.OSMBright) +
  #   tmap::tm_shape(border) +
  #   tmap::tm_borders(col = "red", lwd = 2, group.control = "none")
}

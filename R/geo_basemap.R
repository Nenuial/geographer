#' Get a basemap for a country
#'
#' This function uses leaflet to render the basemap
#'
#' @param country The country to get the basemap for
#'
#' @return A leaflet map with the country borders
#' @export
#' @examples
#' gph_basemap("Switzerland")
#' gph_basemap("United Kingdom")
#'
gph_basemap <- function(country) {
  border = rnaturalearth::ne_countries(
    scale = 10, country = country,
    returnclass = "sf")

  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    leaflet::addPolygons(data = border, color = "red", weight = 2)
}

# #' Get a basemap for a country
# #'
# #' This function uses tmap to render the basemap
# #'
# #' @param country The country to get the basemap for
# #'
# #' @return A leaflet map with the country borders
# #' @export
# #' @examples
# #' gph_basemap("Switzerland")
# #' gph_basemap("United Kingdom")
# #'
# gph_base_tmap <- function(country) {
#   border = rnaturalearth::ne_countries(
#     scale = 10, country = country,
#     returnclass = "sf")
#
#   # The tmap version doesn't work in Quarto for now…
#   tmap::tmap_mode("view")
#   tmap::tm_basemap(leaflet::providers$OpenStreetMap) +
#     tmap::tm_shape(border) +
#     tmap::tm_borders(col = "red", lwd = 2, group.control = "none")
# }

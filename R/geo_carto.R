#' Return NaturalEarth Bound box
#'
#' @return An sf class map
#' @export
carto_ne_get_boundbox <- function() {
  return(internal$boundbox)
}

#' Project simple world map
#'
#' @param crs A valid CRS string
#'
#' @return A ggplot pbject
#' @export
geo_project_world <- function(crs) {
  ggplot2::ggplot(sf::st_as_sf(rworldmap::getMap(resolution = "low"))) +
    ggplot2::geom_sf(
      data = carto_ne_get_boundbox(),
      fill = "#56B4E950", color = "grey30", size = 0.5/ggplot2::.pt
    ) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::coord_sf(expand = FALSE, crs = crs, label_graticule = "") +
    geo_project_world_base_setup()
}

#' Projet world map with tissot matrices
#'
#' @param crs A valid CRS string
#'
#' @return A ggplot object
#' @export
geo_project_world_tissot <- function(crs) {
  ggplot2::ggplot(sf::st_as_sf(rworldmap::getMap(resolution = "low"))) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::geom_sf(
      data = sf::st_read("data/raw/tissot/TissotsIndicatrix.gdb/", layer = "TissotEllipses") |>
                       sf::st_wrap_dateline(c("WRAPDATELINE=YES", "DATELINEOFFSET=5")),
      fill = "#ff7c7c", alpha = .4, color = "black", size = 0.5/ggplot2::.pt
    ) +
    ggplot2::coord_sf(expand = FALSE, crs = crs, label_graticule = "") +
    geo_project_world_base_setup()
}

#' Internal: return ggplot2 settings for world map projections
#'
#' @return A list of ggproto elements
#' @keywords internal
geo_project_world_base_setup <- function() {
  require(ggplot2)
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = "grey30", size = 0.5),
      panel.grid.major = ggplot2::element_line(color = "gray30", size = 0.25),
      axis.ticks = ggplot2::element_line(color = "gray30", size = 0.5/ggplot2::.pt),
      plot.margin = ggplot2::margin(5, 10, 1.5, 1.5)
    )
  )
}

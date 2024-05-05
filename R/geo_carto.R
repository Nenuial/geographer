#' Return NaturalEarth Bound box
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @return A simple feature
#' @export
#' @keywords internal
carto_ne_get_boundbox <- function() {
  lifecycle::deprecate_warn(
    "1.0.0", "carto_ne_get_boundbox()",
    "gph_get_boundbox()"
  )

  gph_boundbox()
}

#' NaturalEarth Bound box for planet Earth
#'
#' @return A simple feature
#' @export
#' @examples
#' gph_boundbox()
gph_boundbox <- function() {
  return(internal$boundbox)
}


#' Project simple world map
#'
#' @param crs A valid CRS string or object
#'
#' @return A ggplot map
#' @export
#' @keywords internal
geo_project_world <- function(crs) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "geo_project_world()",
    "gph_project_world()"
  )

  gph_project_world(crs)
}

#' Project simple world map
#'
#' @param crs A valid CRS string or object
#'
#' @return A ggplot map
#' @export
#' @examples
#' gph_project_world("+proj=eqearth")
gph_project_world <- function(crs) {
  ggplot2::ggplot(rnaturalearth::ne_countries(returnclass = "sf")) +
    ggplot2::geom_sf(
      data = gph_boundbox(),
      fill = "#56B4E950", color = "grey30", size = 0.5 / ggplot2::.pt
    ) +
    ggplot2::geom_sf(
      fill = "#E69F00B0",
      color = "black",
      size = 0.5 / ggplot2::.pt
    ) +
    ggplot2::coord_sf(expand = FALSE, crs = crs, label_graticule = "") +
    gph_project_world_base_setup()
}

#' Projet world map with tissot matrices
#'
#' @param crs A valid CRS string or object
#'
#' @return A ggplot object
#' @export
#' @keywords internal
geo_project_world_tissot <- function(crs) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "geo_project_world_tissot()",
    "gph_project_world_tissot()"
  )

  gph_project_world_tissot(crs)
}

#' Projet world map with Tissot matrices
#'
#' @param crs A valid CRS string or object
#'
#' @return A ggplot object
#' @export
#' @examples
#' gph_project_world_tissot("+proj=eqearth")
gph_project_world_tissot <- function(crs) {
  ggplot2::ggplot(rnaturalearth::ne_countries(returnclass = "sf")) +
    ggplot2::geom_sf(
      fill = "#E69F00B0",
      color = "black",
      size = 0.5 / ggplot2::.pt
    ) +
    ggplot2::geom_sf(
      data = geotools::gtl_gis_tissot_indicatrix(),
      fill = "#ff7c7c", alpha = .4, color = "black", size = 0.5 / ggplot2::.pt
    ) +
    ggplot2::coord_sf(expand = FALSE, crs = crs, label_graticule = "") +
    gph_project_world_base_setup()
}

#' Return ggplot2 settings for world map projections
#'
#' @return A list of ggproto elements
#' @keywords internal
gph_project_world_base_setup <- function() {
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = "grey30",
        size = 0.5
      ),
      panel.grid.major = ggplot2::element_line(
        color = "gray30",
        size = 0.25
      ),
      axis.ticks = ggplot2::element_line(
        color = "gray30",
        size = 0.5 / ggplot2::.pt
      ),
      plot.margin = ggplot2::margin(5, 10, 1.5, 1.5)
    )
  )
}

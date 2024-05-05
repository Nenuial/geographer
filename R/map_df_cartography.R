#' Showcase different projections
#'
#' These functions showcase different map projections.
#'
#' The following projections are available:
#' * Equi-rectangular: `df_cartography_project_equirectangular()`
#' * Gall-Peters: `df_cartography_project_gallpeters()`
#' * Hobo-Dyer: `df_cartography_project_hobodyer()`
#' * Mercator: `df_cartography_project_mercator()`
#' * Wikel-Tripel: `df_cartography_project_winkeltripel()`
#' * Robinson: `df_cartography_project_robinson()`
#' * Goode Homolosine: `df_cartography_project_goodehomolosine()`
#' * Equal Earth: `df_cartography_project_equalearth()`
#'
#' @name df_cartography_projections
#' @return A ggplot2 object
#' @export
#' @examples
#' df_cartography_project_equirectangular()
#' df_cartography_project_gallpeters()
#' df_cartography_project_hobodyer()
#' df_cartography_project_mercator()
#' df_cartography_project_winkeltripel()
#' df_cartography_project_robinson()
#' df_cartography_project_goodehomolosine()
#' df_cartography_project_equalearth()
df_cartography_project_equirectangular <- function() {
  gph_project_world(geotools::gtl_crs_proj("equirec"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_gallpeters <- function() {
  gph_project_world(geotools::gtl_crs_proj("gallpeters"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_hobodyer <- function() {
  gph_project_world(geotools::gtl_crs_proj("hobodyer"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_mercator <- function() {
  gph_project_world("+proj=longlat") +
    ggplot2::coord_sf(
      expand = FALSE,
      crs = geotools::gtl_crs_proj("mercator"),
      label_graticule = "",
      xlim = c(-20000000, 20000000),
      ylim = c(-20000000, 20000000)
    )
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_winkeltripel <- function() {
  gph_project_world(geotools::gtl_crs_proj("wintri"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_robinson <- function() {
  gph_project_world(geotools::gtl_crs_proj("robinson"))
}

#' Robinson outline
#'
#' Robinson special outline shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_robinson_outline <- function() {
  list(cbind(
    c(rep(c(180, -180), each = 181), 180), # Longitudes
    c(90:-90, -90:90, 90) # Latitudes
  )) |>
    sf::st_polygon() |>
    sf::st_sfc(
      crs = geotools::gtl_crs_proj("equirec")
    ) |>
    sf::st_transform(crs = geotools::gtl_crs_proj("robinson"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_goodehomolosine <- function() {
  ggplot2::ggplot(rnaturalearth::ne_countries(returnclass = "sf")) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5 / ggplot2::.pt) +
    ggplot2::geom_sf(data = df_cartography_goodehomolosine_cutout(), fill = "white", color = "NA") +
    ggplot2::geom_sf(
      data = df_cartography_goodehomolosine_outline(),
      fill = NA, color = "gray30", size = 0.5 / ggplot2::.pt
    ) +
    ggplot2::coord_sf(
      crs = geotools::gtl_crs_proj("goode"),
      xlim = 0.95 * sf::st_bbox(df_cartography_goodehomolosine_outline())[c("xmin", "xmax")] * 1.1,
      ylim = 0.95 * sf::st_bbox(df_cartography_goodehomolosine_outline())[c("ymin", "ymax")] * 1.1,
      expand = FALSE
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#56B4E950", color = "grey30", size = 0.5),
      panel.grid.major = ggplot2::element_line(color = "gray30", size = 0.25),
      axis.ticks = ggplot2::element_line(color = "gray30", size = 0.5 / ggplot2::.pt),
      plot.margin = ggplot2::margin(5, 10, 1.5, 1.5)
    )
}

#' Goode Homolosine cutout
#'
#' Generate Goode Homolosine special cutout shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_goodehomolosine_cutout <- function() {
  # get the bounding box in transformed coordinates and expand by 10%
  xlim <- sf::st_bbox(df_cartography_goodehomolosine_outline())[c("xmin", "xmax")] * 1.1
  ylim <- sf::st_bbox(df_cartography_goodehomolosine_outline())[c("ymin", "ymax")] * 1.1

  # turn into enclosing rectangle
  goode_encl_rect <-
    list(
      cbind(
        c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
      )
    ) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = geotools::gtl_crs_proj("goode"))

  # calculate the area outside the earth outline as the difference
  # between the enclosing rectangle and the earth outline
  sf::st_difference(goode_encl_rect, df_cartography_goodehomolosine_outline())
}

#' Goode Homolosine outline
#'
#' Generate Goode Homolosine special outline shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_goodehomolosine_outline <- function() {
  # projection outline in long-lat coordinates
  lats <- c(
    90:-90, # right side down
    -90:0, 0:-90, # third cut bottom
    -90:0, 0:-90, # second cut bottom
    -90:0, 0:-90, # first cut bottom
    -90:90, # left side up
    90:0, 0:90, # cut top
    90 # close
  )
  longs <- c(
    rep(180, 181), # right side down
    rep(c(80.01, 79.99), each = 91), # third cut bottom
    rep(c(-19.99, -20.01), each = 91), # second cut bottom
    rep(c(-99.99, -100.01), each = 91), # first cut bottom
    rep(-180, 181), # left side up
    rep(c(-40.01, -39.99), each = 91), # cut top
    180 # close
  )

  goode_outline <-
    list(cbind(longs, lats)) |>
    sf::st_polygon() |>
    sf::st_sfc(
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )

  # now we need to work in transformed coordinates, not in long-lat coordinates
  sf::st_transform(goode_outline, crs = geotools::gtl_crs_proj("goode"))
}

#' @rdname df_cartography_projections
#' @export
df_cartography_project_equalearth <- function() {
  gph_project_world(geotools::gtl_crs_proj("eqearth"))
}

# nolint start: commented_code_linter
# Remove from package because require() is a warning in
# R CMD Check. Remains here for future me.
# #' DF Cartography: Patchwork of rectangular projections
# #'
# #' A patchwork composition of Equirectangular, Mercator,
# #' Gall-Peters and Hobo Dyer projections.
# #'
# #' @return A patchwork object
# #' @export
# #' @examples
# #' df_cartography_rectangular_projections()
# #'
# df_cartography_rectangular_projections <- function() {
#   require(patchwork)
#
#   df_cartography_project_equirectangular() +
#     df_cartography_project_mercator() +
#     df_cartography_project_gallpeters() +
#     df_cartography_project_hobodyer() +
#     patchwork::plot_layout(ncol = 2, widths = c(1, 1))
# }

# Remove from package because require() is a warning in
# R CMD Check. Remains here for future me.
# #' DF Cartography: Patchwork of pseudo projections
# #'
# #' A patchwork composition of Goode Homolosine, Robinson,
# #' Winkel-Tripel and Equal Earth projections.
# #'
# #' @return A patchwork object
# #' @export
# #' @examples
# #' df_cartography_nonrectangular_projections()
# #'
# df_cartography_nonrectangular_projections <- function() {
#   require(patchwork)
#
#   df_cartography_project_goodehomolosine() +
#     df_cartography_project_robinson() +
#     df_cartography_project_winkeltripel() +
#     df_cartography_project_equalearth() +
#     patchwork::plot_layout(ncol = 2, widths = c(1, 1))
# }
# nolint end

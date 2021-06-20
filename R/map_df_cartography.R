#' DF Cartography: Equirectangular projection
#'
#' ggplot2 map of the world in Equirectangular projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_equirectangular <- function() {
  geographer::geo_project_world(geotools::gtl_crs_proj("equirec"))
}

#' DF Cartography: Gall-Peters projection
#'
#' ggplot2 map of the world in Gall-Peters projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_gallpeters <- function() {
  geographer::geo_project_world(geotools::gtl_crs_proj("gallpeters"))
}

#' DF Cartography: Hobo-Dyer projection
#'
#' ggplot2 map of the world in Hobo-Dyer projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_hobodyer <- function() {
  geographer::geo_project_world(geotools::gtl_crs_proj("hobodyer"))
}

#' DF Cartography: Mercator projection
#'
#' ggplot2 map of the world in Mercator projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_mercator <- function() {
  geographer::geo_project_world("+proj=longlat") +
    ggplot2::coord_sf(
      expand = FALSE,
      crs = geotools::gtl_crs_proj("mercator"),
      label_graticule = "",
      xlim = c(-20000000, 20000000),
      ylim = c(-20000000, 20000000)
    )
}

#' DF Cartography: Winkel-Tripel projection
#'
#' ggplot2 map of the world in Winkel-Tripel projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_winkeltripel <- function() {
  require(ggplot2)

  lwgeom::st_transform_proj(sf::st_as_sf(rworldmap::getMap(resolution = "low")),
                            crs = "+proj=wintri +datum=WGS84 +no_defs +over") -> world_wintri

  sf::st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) |>
    lwgeom::st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") -> grat_wintri

  list(cbind(
    c(rep(c(180, -180), each = 181), 180), # Longitudes
    c(90:-90, -90:90, 90) # Latitudes
  )) |>
    sf::st_polygon() |>
    sf::st_sfc(
      crs = geotools::gtl_crs_proj("equirec")
    ) |>
    lwgeom::st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") -> wintri_outline

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = wintri_outline, fill = "#56B4E950", color = NA) +
    ggplot2::geom_sf(data = grat_wintri, color = "gray30", size = 0.25/ggplot2::.pt) +
    ggplot2::geom_sf(data = world_wintri, fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::geom_sf(data = wintri_outline, fill = NA, color = "grey30", size = 0.5/ggplot2::.pt) +
    ggplot2::coord_sf(datum = NA, expand = FALSE) +
    dviz.supp::theme_dviz_grid(12, rel_small = 1) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(6, 1.5, 3, 1.5)
    )
}

#' DF Cartography: Robinson projection
#'
#' ggplot2 map of the world in Robinson projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_robinson <- function() {
  ggplot2::ggplot(sf::st_as_sf(rworldmap::getMap(resolution = "low"))) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::geom_sf(data = df_cartography_robinson_cutout(), fill = "white", color = NA) +
    ggplot2::geom_sf(data = df_cartography_robinson_outline(), fill = NA, color = "grey30", size = 0.5/ggplot2::.pt) +
    geo_project_world_base_setup() +
    ggplot2::coord_sf(
      xlim = 0.95 * c(-18494733, 18613795),
      ylim = 0.95 * c(-9473396, 9188587),
      expand = FALSE,
      crs = geotools::gtl_crs_proj("robinson"),
      ndiscr = 1000
    )
}

#' DF Cartography: Robinson cutout
#'
#' Generate Robinson special cutout shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_robinson_cutout <- function() {
  # bounding box in transformed coordinates
  xlim_robin <- c(-18494733, 18613795)
  ylim_robin <- c(-9473396, 9188587)
  robin_bbox <-
    list(
      cbind(
        c(xlim_robin[1], xlim_robin[2], xlim_robin[2], xlim_robin[1], xlim_robin[1]),
        c(ylim_robin[1], ylim_robin[1], ylim_robin[2], ylim_robin[2], ylim_robin[1])
      )
    ) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = geotools::gtl_crs_proj("robinson"))

  # area outside the earth outline
  sf::st_difference(robin_bbox, df_cartography_robinson_outline())
}

#' DF Cartography: Goode Homolosine outline
#'
#' Generate Goode Homolosine special outline shape
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

#' DF Cartography: Goode Homolosine projection
#'
#' ggplot2 map of the world in Goode Homolosine projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_goodehomolosine <- function() {
  ggplot2::ggplot(sf::st_as_sf(rworldmap::getMap(resolution = "low"))) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::geom_sf(
      data = df_cartography_goodehomolosine_cutout(),
      fill = "white", color = NA
    ) +
    ggplot2::geom_sf(
      data = df_cartography_goodehomolosine_outline(),
      fill = NA, color = "grey30", size = 0.5/ggplot2::.pt
    ) +
    geo_project_world_base_setup() +
    ggplot2::coord_sf(
      xlim = 0.95 * c(-21945470, 21963330),
      ylim = 0.95 * c(-9538022, 9266738),
      expand = FALSE,
      crs = geotools::gtl_crs_proj("goode"),
      ndiscr = 1000
    )
}

#' DF Cartography: Goode Homolosine cutout
#'
#' Generate Goode Homolosine special cutout shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_goodehomolosine_cutout <- function() {
  # bounding box in transformed coordinates
  xlim_goode <- c(-21945470, 21963330)
  ylim_goode <- c(-9538022, 9266738)
  goode_bbox <-
    list(
      cbind(
        c(xlim_goode[1], xlim_goode[2], xlim_goode[2], xlim_goode[1], xlim_goode[1]),
        c(ylim_goode[1], ylim_goode[1], ylim_goode[2], ylim_goode[2], ylim_goode[1])
      )
    ) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = geotools::gtl_crs_proj("goode"))

  # area outside the earth outline
  sf::st_difference(goode_bbox, df_cartography_goodehomolosine_outline())
}

#' DF Cartography: Goode Homolosine outline
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

  list(cbind(longs, lats)) |>
    sf::st_polygon() |>
    sf::st_sfc(
      crs = geotools::gtl_crs_proj("equirec")
    ) |>
    sf::st_transform(crs = geotools::gtl_crs_proj("goode"))
}

#' DF Cartography: Equal Earth projection
#'
#' ggplot2 map of the world in Equal Earth projection
#'
#' @return A ggplot2 object
#' @export
df_cartography_project_equalearth <- function() {
  require(ggplot2)

  ggplot2::ggplot(sf::st_as_sf(rworldmap::getMap(resolution = "low"))) +
    ggplot2::geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/ggplot2::.pt) +
    ggplot2::geom_sf(data = df_cartography_equalearth_cutout(), fill = "white", color = NA) +
    ggplot2::geom_sf(data = df_cartography_equalearth_outline(), fill = NA, color = "grey30", size = 0.5/ggplot2::.pt) +
    ggplot2::scale_x_continuous(
      name = NULL,
      breaks = seq(-160, 160, by = 20)
    ) +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = seq(-80, 80, by = 20)
    ) +
    ggplot2::coord_sf(
      xlim = 0.95 * c(-18494733, 18613795),
      ylim = 0.95 * c(-9473396, 9188587),
      expand = FALSE,
      crs = geotools::gtl_crs_proj("eqearth"),
      ndiscr = 1000
    ) +
    dviz.supp::theme_dviz_grid(12, rel_small = 1) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#56B4E950", color = "white", size = 1),
      panel.grid.major = ggplot2::element_line(color = "gray30", size = 0.25),
      plot.margin = ggplot2::margin(6, 1.5, 1.5, 1.5)
    )
}

#' DF Cartography: Equal Earth cutout
#'
#' Generate Equal Earth special cutout shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_equalearth_cutout <- function() {
  # bounding box in transformed coordinates
  xlim_eqearth <- c(-18494733, 18613795)
  ylim_eqearth <- c(-9473396, 9188587)
  eqearth_bbox <-
    list(
      cbind(
        c(xlim_eqearth[1], xlim_eqearth[2], xlim_eqearth[2], xlim_eqearth[1], xlim_eqearth[1]),
        c(ylim_eqearth[1], ylim_eqearth[1], ylim_eqearth[2], ylim_eqearth[2], ylim_eqearth[1])
      )
    ) |>
    sf::st_polygon() |>
    sf::st_sfc(crs = geotools::gtl_crs_proj("eqearth"))

  # area outside the earth outline
  sf::st_difference(eqearth_bbox, df_cartography_equalearth_outline())
}

#' DF Cartography: Equal Earth outline
#'
#' Generate Equal Earth special outline shape
#'
#' @return A sf object
#' @keywords internal
df_cartography_equalearth_outline <- function() {
  list(cbind(
    c(rep(c(180, -180), each = 181), 180), # Longitudes
    c(90:-90, -90:90, 90) # Latitudes
  )) |>
    sf::st_polygon() |>
    sf::st_sfc(
      crs = geotools::gtl_crs_proj("equirec")
    ) |>
    sf::st_transform(crs = geotools::gtl_crs_proj("eqearth"))
}

#' DF Cartography: Patchwork of rectangular projections
#'
#' A patchwork composition of Equirectangular, Mercator,
#' Gall-Peters and Hobo Dyer projections.
#'
#' @return A patchwork object
#' @export
df_cartography_rectangular_projections <- function() {
  require(patchwork)

  df_cartography_project_equirectangular() +
    df_cartography_project_mercator() +
    df_cartography_project_gallpeters() +
    df_cartography_project_hobodyer() +
    patchwork::plot_layout(ncol = 2, widths = c(1, 1))
}

#' DF Cartography: Patchwork of pseudo projections
#'
#' A patchwork composition of Goode Homolosine, Robinson,
#' Winkel-Tripel and Equal Earth projections.
#'
#' @return A patchwork object
#' @export
df_cartography_nonrectangular_projections <- function() {
  require(patchwork)

  df_cartography_project_goodehomolosine() +
    df_cartography_project_robinson() +
    df_cartography_project_winkeltripel() +
    df_cartography_project_equalearth() +
    patchwork::plot_layout(ncol = 2, widths = c(1, 1))
}

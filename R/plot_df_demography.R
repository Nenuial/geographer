#' DF Demography: Historical world population plot
#'
#' World population graph from 10'000 BCE to 1200 CE
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_historical()
df_demography_graph_world_population_historical <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population),
      color = "black",
      size = .8,
      data = geodata::df_demography_population_historical
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-10000, 1200),
      expand = c(0, 0),
      breaks = seq(-10000, 1200, 1000)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(x) scales::number(x, scale = .000001)
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 300000000)) +
    ggplot2::labs(
      title = geotools::translate_enfr("World population 10'000 BCE to 1200 CE", "Population mondiale de 10'000 av. JC \u00e0 1200 ap. JC"),
      x = "",
      y = "Population (millions)"
    ) +
    theme +
    ggplot2::theme(
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    )
}

#' DF Demography: World population
#'
#' World population graph
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_current()
df_demography_graph_world_population_current <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = "historical"),
      size = .8,
      data = geodata::df_demography_population_historical |> dplyr::filter(year <= 1950)
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = "current"),
      size = .8,
      data = geodata::df_demography_population_historical |> dplyr::filter(year >= 1950)
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = estimate),
      size = .8,
      data = geodata::df_demography_population_estimates
    ) +
    ggplot2::scale_x_continuous(
      limits = c(1200, 2100),
      expand = c(0, 0),
      breaks = seq(1200, 2100, 100)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 15000000000, 5000000000),
      labels = function(x) scales::number(x, scale = .000000001, accuracy = 1)
    ) +
    ggplot2::scale_color_manual(
      breaks = c("historical", "current", "un_low", "un_medium", "un_high"),
      values = c("historical" = "black", "current" = "blue",
                 "un_low" = "green", "un_medium" = "orange", "un_high" = "red"),
      labels = c("historical" = geotools::translate_enfr("historical", "historique"),
                 "current" = geotools::translate_enfr("current", "actuel"),
                 "un_low" = geotools::translate_enfr("low estimate", "estimation basse"),
                 "un_medium" = geotools::translate_enfr("medium estimate", "estimation moyenne"),
                 "un_high" = geotools::translate_enfr("high estimate", "estimation haute"))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = NULL)
    ) +
    theme +
    ggplot2::theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::labs(
      title = geotools::translate_enfr("World population 1200 to 2100", "Population mondiale de 1200 \u00e0 2100"),
      x = "",
      y = geotools::translate_enfr("Population (billions)", "Population (milliards)")
    )
}

#' DF Demography: World population growth rate
#'
#' World population growth rate graph
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_growth()
df_demography_graph_world_population_growth <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = growth),
      color = "black",
      size = .8,
      data = geodata::df_demography_population_growth
    ) +
    ggplot2::scale_x_continuous(
      limits = c(1955, 2015),
      expand = c(0, 0),
      breaks = seq(1955, 2015, 5)
    ) +
    theme +
    ggplot2::theme(
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::labs(
      title = geotools::translate_enfr("World population growth", "Acroissement de la population mondiale"),
      x = "",
      y = geotools::translate_enfr("Growth rate (%)", "Taux d'acroissement (%)")
    )
}

# For future me. Requires patchwork.
# #' DF Demography: World population patchwork composition
# #'
# #' World population graph with historical, current and future
# #' numbers as well as growth rates.
# #'
# #' @param theme A ggplot2 theme
# #'
# #' @return A ggplot2 graph
# #' @export
# df_demography_graph_world_population <- function(theme = ggplot2::theme_minimal()) {
#   df_demography_graph_world_population_historical(theme) +
#     df_demography_graph_world_population_current(theme) +
#     df_demography_graph_world_population_growth(theme) +
#     patchwork::plot_layout(ncol = 1) +
#     patchwork::plot_annotation(tag_levels = "a", tag_suffix = ".") &
#     ggplot2::theme(
#       plot.tag = ggplot2::element_text(size = 12),
#       plot.tag.position = "topleft"
#     )
# }

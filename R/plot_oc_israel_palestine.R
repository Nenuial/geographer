#' OC Israël-Palestine: graphique des migrations
#'
#' Graphique de l'immigration en Isräel
#' de 1949 à 2017.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_israel_palestine_graph_migration_israel <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_israel_palestine_immigration_israel |>
    ggplot2::ggplot(ggplot2::aes(year, immigration)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2015, 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
    ggplot2::labs(
      title = "Immigration en Israël depuis 1948",
      x = "", y = "",
      caption = "Source: CBS, Israël"
    ) +
    theme
}

#' OC Israël-Palestine: graphique dynamique des migrations
#'
#' Graphique de l'immigration en Isräel
#' de 1949 à 2017.
#'
#' @return A highcharts graph
#' @export
oc_israel_palestine_hc_migration_israel <- function() {
  geodata::oc_israel_palestine_immigration_israel -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Immigration en Israël depuis 1948") |>
    highcharter::hc_caption(text = "Source: CBS, Israël") |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_plotOptions(
      line = list(
        marker = list(
          enabled = FALSE,
          symbol = "circle"
        )
      )
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = "Immigration",
      dashStyle = "solid",
      highcharter::hcaes(x = year, y = immigration)
    )
}

#' OC Israël-Palestine: graphique de la
#' population néé à l'étranger
#'
#' Graphique avec la population néé à l'étranger
#' par pays de naissance d'après le recensement 2018.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_israel_palestine_population_nae <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_israel_palestine_2008_population_nae |>
    dplyr::arrange(-population) |>
    dplyr::mutate(country = forcats::fct_inorder(country)) |>
    dplyr::group_by(continent) |>
    dplyr::mutate(country = forcats::fct_lump_n(country, 3, w = population,
                                                other_level = paste("Other", dplyr::first(continent)))) |>
    dplyr::ungroup() |>
    dplyr::group_by(country, continent) |>
    dplyr::summarise(population = sum(population), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::mutate(country = forcats::fct_rev(forcats::fct_reorder(country, population, max))) |>
    ggplot2::ggplot(ggplot2::aes(x = country, y = population, fill = continent)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 3)) +
    ggplot2::scale_y_continuous(
      labels = ggeo::ggeo_label_sci_10
    ) +
    ggplot2::scale_fill_manual(
      values = paletteer::paletteer_d("IslamicArt::jerusalem")[c(1,2,8,3,5)]
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Population néé à l'étranger",
      subtitle = "par pays de naissance",
      x = "", y = "Population",
      fill = "", caption = "Données: CBS Isräel (2008)"
    )
}

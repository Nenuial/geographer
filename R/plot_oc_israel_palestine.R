# Démographie ---------------------------------------------------------------------------------

#' OC Israël-Palestine: graphique des migrations
#'
#' Graphique de l'immigration en Israël
#' de 1949 à 2017.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_israel_palestine_graph_migration_israel()
oc_israel_palestine_graph_migration_israel <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_israel_palestine_immigration_israel |>
    ggplot2::ggplot(ggplot2::aes(year, immigration)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2015, 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
    ggplot2::labs(
      title = "Immigration en Isra\u00ebl depuis 1948",
      x = "", y = "",
      caption = "Source: CBS, Isra\u00ebl"
    ) +
    theme
}

#' OC Israël-Palestine: graphique dynamique des migrations
#'
#' Graphique de l'immigration en Israël
#' de 1949 à 2017.
#'
#' @return A highcharts graph
#' @export
#' @examples
#' oc_israel_palestine_hc_migration_israel()
oc_israel_palestine_hc_migration_israel <- function() {
  geodata::oc_israel_palestine_immigration_israel -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Immigration en Isra\u00ebl depuis 1948") |>
    highcharter::hc_caption(text = "Source: CBS, Isra\u00ebl") |>
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
#' @examples
#' oc_israel_palestine_graph_population_nae()
oc_israel_palestine_graph_population_nae <- function(theme = ggplot2::theme_minimal()) {
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
      title = "Population n\u00c9\u00c9 \u00e0 l'\u00c9tranger",
      subtitle = "par pays de naissance",
      x = "", y = "Population",
      fill = "", caption = "Donn\u00c9es: CBS Isra\u00ebl (2008)"
    )
}

#' OC Israël-Palestine: graphique dynamique
#' de l'immigration juive en Palestine
#' avant 1948
#'
#' @return A highcharts graph
#' @export
#' @examples
#' oc_israel_palestine_hc_immigration_sioniste()
oc_israel_palestine_hc_immigration_sioniste <- function() {
  geodata::oc_israel_palestine_immigration_juive_avant_1948 -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Immigration juive avant 1948") |>
    highcharter::hc_subtitle(text = "chiffres approximatifs") |>
    highcharter::hc_caption(text = "Source: <a href='https://www.bpb.de/themen/migration-integration/laenderprofile/english-version-country-profiles/58400/historical-development-of-jewish-immigration/'>BPD, 2008</a>") |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_xAxis(
      type= 'category'
    ) |>
    highcharter::hc_add_series(
      data = data,
      "column",
      name = "Immigration",
      color = "#475286FF",
      dashStyle = "solid",
      highcharter::hcaes(x = period, y = immigration)
    )
}

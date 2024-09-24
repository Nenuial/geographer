#' Immigration italienne en Suisse
#'
#' Un graphique avec l'évolution de l'immigration
#' italienne en Suisse de 1981 jusqu'en 2021.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#'
#' @export
#' @examples
#' oc_suisse_graph_2021_immigration_italienne()
oc_suisse_graph_2021_immigration_italienne <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_2021_immigration_italienne |>
    ggplot2::ggplot(ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = population), color = "blue") +
    ggplot2::geom_col(ggplot2::aes(y = immigration * 10), fill = "orange") +
    ggplot2::scale_y_continuous(
      name = "Population (milliers)",
      expand = ggplot2::expansion(add = c(0, 10000)),
      sec.axis = ggplot2::sec_axis(
        trans = ~ . / 10,
        name = "Immigration (milliers)",
        labels = scales::number_format(scale = .001)
      ),
      labels = scales::number_format(scale = .001)
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    theme +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(color = "blue"),
      axis.title.y.right = ggplot2::element_text(color = "orange")
    ) +
    ggplot2::labs(
      title = "Population et immigration italienne en Suisse",
      caption = "Donn\u00e9es: OFS (2021)",
      x = ""
    )
}

#' Nationalité en Suisse en 2023
#'
#' Graphique des principales nationalité en Suisse en 2023
#' hors Suisses.
#'
#' @returns A highcharts graph
#' @export
#'
#' @examples
#' oc_suisse_hc_2023_nationalite()
oc_suisse_hc_2023_nationalite <- function() {
  geodata::oc_suisse_2023_nationalite |>
    dplyr::filter(Nationalite == "Nationalit\u00e9 - total") |>
    dplyr::pull(Population) -> population_totale

  geodata::oc_suisse_2023_nationalite |>
    dplyr::filter(Nationalite != "Nationalit\u00e9 - total") |>
    dplyr::filter(Nationalite != "Suisse") |>
    dplyr::mutate(percent = round(Population / population_totale * 100, 2)) |>
    dplyr::arrange(dplyr::desc(percent)) |>
    dplyr::slice(1:10) |>
    highcharter::hchart(
      type = "column",
      highcharter::hcaes(x = Nationalite, y = percent),
      name = "Population"
    ) |>
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{text} %")) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_plotOptions(
      column = list(colorByPoint = TRUE)
    ) |>
    highcharter::hc_colors(palettetown::ichooseyou("salamence", 9)) |>
    highcharter::hc_title(text = "Ethnies principales") |>
    highcharter::hc_subtitle(text = "en Suisse en 2023 (hors Suisses)") |>
    highcharter::hc_caption(text = "Source: OFS (2023)") |>
    highcharter::hc_tooltip(valueSuffix = "%")
}

#' OC Ecocitoyenneté : consommation d'énergie par source
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_ecocitoyennete_graph_energy_per_source()
oc_ecocitoyennete_graph_energy_per_source <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_ecocitoyennete_energy_consumption_per_source |>
    dplyr::mutate(source = stringr::str_replace(source, " per capita", "")) |>
    dplyr::mutate(source = forcats::fct_inorder(source) |> forcats::fct_rev()) |>
    dplyr::filter(entity %in% c("World", "Switzerland")) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = amount, fill = source)) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(
      values = rev(c("#000304", "#ca5140", "#f2e9c0", "#d0acd0", "#8766ab", "#b8d2c5", "#ebd35e", "#d7dfa3")),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::scale_y_continuous(labels = ggeo::ggeo_label_sci_10) +
    ggplot2::facet_wrap(~ entity, ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Consommation d'\u00e9nergie par source",
      y = "\u00c9nergie par personne (kWh)",
      x = "", fill = ""
    )
}

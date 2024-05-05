#' Sex-ratio par régions en Chine
#'
#' Une carte du rapport du nombre d'hommes
#' par rapport aux femmes par régions pour 2018.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#'
#' @export
#' @examples
#' oc_chine_carte_2018_sex_ratio_par_region()
oc_chine_carte_2018_sex_ratio_par_region <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_states(country = "China", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_chine_2018_sex_ratio_par_region,
      by = "adm1_code"
    ) |>
    dplyr::mutate(
      data = santoku::chop(ratio, c(1, 1.05, 1.1, 1.15), extend = TRUE, drop = FALSE)
    ) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data),
      color = "black", size = .1
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(5),
      breaks = levels(plot_data$data),
      limits = levels(plot_data$data),
      drop = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        title.position = "top",
        even.steps = TRUE, show.limits = FALSE,
        barwidth = 30
      )
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Sex ratio de la population",
      subtitle = glue::glue("Chine 2018"),
      fill = "Rapport hommes/femmes",
      caption = "Donn\u00e9es : Bureau national des statistiques de Chine"
    )
}

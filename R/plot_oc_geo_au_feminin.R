#' OC Géographie au féminin: graphique du taux de scolarisation des filles en Afghanistan
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_scolarisation_filles_afghanistan <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c(school = "SE.PRM.ENRR.FE"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2018
  ) |>
    tidyr::drop_na(school) |>
    dplyr::mutate(school = school / 100) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = school)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_path() +
    ggplot2::annotate("rect", xmin = 1996, xmax = 2001, ymin = 0, ymax = 1,
             fill = "red", alpha = .2) +
    ggplot2::annotate("text", x = 1998.5, y = .8, label = "Talibans\n au pouvoir",
                      size = 12, family = "Fira Sans Light") +
    ggplot2::coord_cartesian(xlim = c(1970, 2018), ylim = c(0, .9)) +
    ggplot2::scale_y_continuous(labels = scales::percent,
      expand = c(0,0)
    ) +
    ggplot2::labs(
      title = "LE RETOUR DES TALIBANS…",
      subtitle = "Pourcentage de filles allant à l'école primaire",
      x = "", y = "",
      captions = "Source: John Burn-Murdoch (FT), Données: Banque Mondiale"
    ) +
    theme
}

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

#' OC Géographie au féminin: taux d'avortement en Suisse
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_suisse_avortements <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2021_ofs_avortements |>
    ggplot2::ggplot(ggplot2::aes(year, rate, color = type)) +
    ggplot2::geom_line(size = 2.5) +
    paletteer::scale_color_paletteer_d(
      "nord::lumina",
      labels = c("de 15 à 49 ans", "de 15 à 19 ans")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = seq(2004, 2020, 2)) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Interruptions de grossesse",
      subtitle = "Taux pour 1000 femmes",
      x = "", y = "", color = "",
      caption = "OFS (2021)"
    )
}

oc_geo_au_feminin_graph_rapport_hommes_femmes_2020 <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_wpp2019_sex_ratio |>
    dplyr::filter(
      un_code %in% c(900, 903, 935, 908, 905, 909, 904),
      year == 2020
    ) |>
    dplyr::mutate(age = forcats::fct_inorder(age)) |>
    dplyr::select(age, ratio, name) |>
    ggplot2::ggplot(ggplot2::aes(x = age, y = ratio, color = name, group = name)) +
    ggplot2::geom_line()
}

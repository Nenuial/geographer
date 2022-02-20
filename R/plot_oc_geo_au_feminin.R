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

#' OC Géographie au féminin: Expérience de violences sexuelles
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_auf_feminin_graph_2019_violences_sexuelles_experiences <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2019_gfs_violences_sexuelles_experiences |>
    dplyr::arrange(oui) |>
    dplyr::mutate(Expérience = forcats::fct_inorder(Expérience)) |>
    tidyr::pivot_longer(
      cols = c(-"Expérience"),
      names_to = "Réponse",
      values_to = "Pourcentage"
    ) |>
    dplyr::mutate(Label = dplyr::if_else(Pourcentage > 2, Pourcentage, NA_real_)) |>
    dplyr::mutate(Réponse = forcats::fct_inorder(Réponse)) |>
    ggplot2::ggplot(ggplot2::aes(y = Expérience, x = Pourcentage, fill = Réponse)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label),
      position = ggplot2::position_stack(vjust = .5, reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(
      values = c("#0c60ae", "#1ea1ec", "#fbbf28", "#999999")
    ) +
    ggplot2::labs(
      subtitle = "% de femmes dès 16 ans",
      x = "", y = "", fill = "",
      caption = "© gfs.bern, enquête sur les violences sexuelles, avril 2019 (N = 4’495)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

#' OC Géographie au féminin: Actes de violences sexuelles
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_auf_feminin_graph_2019_violences_sexuelles_actes <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2019_gfs_violences_sexuelles_actes |>
    dplyr::arrange(Pourcentage) |>
    dplyr::mutate(Acte = forcats::fct_inorder(Acte)) |>
    ggplot2::ggplot(ggplot2::aes(y = Acte, x = Pourcentage)) +
    ggplot2::geom_col(fill = "#ab47bc") +
    gghighlight::gghighlight(
      Acte == "A subi des actes sexuels non consentis",
      unhighlighted_params = list(fill = "#0c60ae")
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
    ggplot2::scale_y_discrete(labels = function(y) stringr::str_wrap(y, width = 50)) +
    ggplot2::labs(
      subtitle = "% de femmes dès 16 ans",
      x = "", y = "", fill = "",
      caption = "© gfs.bern, enquête sur les violences sexuelles, avril 2019 (N = 4’495)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

#' OC Géographie au féminin: Violences sexuelles
#' Raisons de ne pas contacter la police
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_auf_feminin_graph_2019_violences_sexuelles_police <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2019_gfs_violences_sexuelles_police |>
    dplyr::arrange(oui) |>
    dplyr::mutate(Raison = forcats::fct_inorder(Raison)) |>
    tidyr::pivot_longer(
      cols = c(-"Raison"),
      names_to = "Réponse",
      values_to = "Pourcentage"
    ) |>
    dplyr::mutate(Label = dplyr::if_else(Pourcentage > 2, Pourcentage, NA_real_)) |>
    dplyr::mutate(Réponse = forcats::fct_inorder(Réponse)) |>
    ggplot2::ggplot(ggplot2::aes(y = Raison, x = Pourcentage, fill = Réponse)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label),
      position = ggplot2::position_stack(vjust = .5, reverse = TRUE)
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_fill_manual(
      values = c("#0c60ae", "#999999", "#fbbf28")
    ) +
    ggplot2::labs(
      subtitle = "% de femmes dès 16 ans qui ne se sont pas adressées\nà la police après un acte sexuel non consenti",
      x = "", y = "", fill = "",
      caption = "© gfs.bern, Enquête violences sexuelles, avril 2019 (n = 1239)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

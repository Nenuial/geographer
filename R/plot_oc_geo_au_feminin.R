#' OC Géographie au féminin: graphique du taux de scolarisation des filles en Afghanistan
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_scolarisation_filles_afghanistan <- function(theme = ggplot2::theme_minimal(), line_color = "black") {
  wbstats::wb_data(
    indicator = c(school = "SE.PRM.ENRR.FE"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2022
  ) |>
    tidyr::drop_na(school) |>
    dplyr::mutate(school = school / 100) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = school)) +
    ggplot2::geom_point(size = 2, color = line_color) +
    ggplot2::geom_path(color = line_color) +
    ggplot2::annotate("rect", xmin = 1996, xmax = 2001, ymin = 0, ymax = 1,
             fill = "#bf616a", alpha = .2) +
    ggplot2::annotate("rect", xmin = 2021, xmax = 2025, ymin = 0, ymax = 1,
                      fill = "#bf616a", alpha = .2) +
    ggplot2::coord_cartesian(xlim = c(1970, 2024), ylim = c(0, .9)) +
    ggplot2::scale_y_continuous(labels = scales::percent,
      expand = c(0,0)
    ) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::labs(
      title = "Le retour des talibans…",
      subtitle = "Pourcentage de filles allant à l'école primaire",
      x = "", y = "",
      caption = "Source: John Burn-Murdoch (FT), Données: Banque Mondiale"
    ) +
    theme
}

#' OC Géographie au féminin: graphique du taux de scolarisation des filles en Afghanistan
#'
#' @return A highcharts graph
#' @export
oc_geo_au_feminin_hc_scolarisation_filles_afghanistan <- function() {
  wbstats::wb_data(
    indicator = c(school = "SE.PRM.ENRR.FE"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2022
  ) |>
    tidyr::drop_na(school) -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Le retour des talibans…") |>
    highcharter::hc_subtitle(text = "Pourcentage de filles allant à l'école primaire") |>
    highcharter::hc_caption(text = "Source: John Burn-Murdoch (FT), Données: Banque Mondiale") |>
    highcharter::hc_xAxis(
      title = list(text = ""),
      max = 2025,
      showInLegend = F,
      plotBands = list(
        list(
          from = 1996,
          to = 2001,
          color = "#bf616a",
          label = list(
            text = 'Talibans',
            style = list(
              color = "#e0d9fb"
            )
          )
        ),
        list(
          from = 2021,
          to = 2025,
          color = "#bf616a",
          label = list(
            text = '???',
            style = list(
              color = "#e0d9fb"
            )
          )
        )
      )
    ) |>
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{text}%")) |>
    highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = "Scolarisation",
      dashStyle = "solid",
      tooltip = list(valueSuffix = "%"),
      highcharter::hcaes(x = date, y = round(school, 2))
    ) |>
    ggeo::hc_purple_theme()
}

#' OC Géographie au féminin: graphique du PIB par habitant
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_situation_eco_afghanistan <- function(theme, line_color = "black") {
  wbstats::wb_data(
    indicator = c(gdp = "NY.GDP.PCAP.CD"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2022
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = gdp)) +
    ggplot2::geom_line(color = line_color) +
    ggplot2::scale_y_continuous(label = ggeo::ggeo_label_sci_10) +
    ggplot2::labs(
      title = "Une économie qui ralentit",
      y = "PIB par hab.", x = "",
      caption = "Données: Banque Mondiale"
    ) +
    theme
}

#' OC Géo au féminin: représentation politique
#' Pourcentage de femmes dans les parlements nationaux par régions
#'
#' @return A highcharts graph
#' @export
oc_geo_au_feminin_hc_situation_eco_afghanistan <- function() {
  wbstats::wb_data(
    indicator = c(gdp = "NY.GDP.PCAP.CD"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2022
  ) -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Une économie qui ralentit") |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "$US courrant")) |>
    highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = "PIB par hab.",
      dashStyle = "solid",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = round(gdp,2))
    ) |>
    ggeo::hc_purple_theme()
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

#' OC Géographie au féminin: Espérance de vie
#' Graphiques de l'expérance de vie
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_esperances_de_vie <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c(
      "lexf" = "SP.DYN.LE00.FE.IN",
      "lexm" = "SP.DYN.LE00.MA.IN"),
    start_date = 1960, end_date = 2022
  ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = lexf, color = "purple")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = lexm, color = "blue")) +
    geofacet::facet_geo(~iso3c, grid = "europe_countries_grid2")
}

#' OC Géo au féminin
#' Graphique de l'évolution de l'écart entre
#' l'espérence de vie des hommes et des femmes
#' pour le Japon, la Suisse et les USA
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_difference_esperance_de_vie <- function(theme = ggplot2::theme_minimal()) {
  list(
    "Japon" = geodata::gdt_hmd_e0_table("Japan"),
    "Suisse" = geodata::gdt_hmd_e0_table("Switzerland"),
    "USA" = geodata::gdt_hmd_e0_table("USA")
  ) |> purrr::list_rbind(names_to = "country") -> data

  data |>
    dplyr::filter(year > 1950) |>
    dplyr::mutate(diff = Female - Male) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = diff, color = country)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_y_continuous(limits = c(3, NA)) +
    theme

}

#' OC Géo au féminin: représentation politique
#' Pourcentage de femmes dans les parlements nationaux par régions
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_geo_au_feminin_graph_proportion_parlements <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c("parl" = "SG.GEN.PARL.ZS"),
    start_date = 1997,
    end_date = 2022,
    country = "regions_only"
  ) -> data

  data |>
    dplyr::rename(
      Année = date,
      Représentation = parl,
      Région = country
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = Année, y = Représentation, fill = Région, color = Région)) +
    ggplot2::geom_area(show.legend = F) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Région, nrow = 2) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    ggplot2::scale_fill_manual(values = MetBrewer::met.brewer("Nizami")[c(2:8)]) +
    ggplot2::scale_color_manual(values = MetBrewer::met.brewer("Nizami")[c(2:8)]) +
    ggplot2::labs(
      x = "", y = ""
    ) +
    theme
}

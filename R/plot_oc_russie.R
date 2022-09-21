#' OC Russie: graphique des naissances
#'
#' Graphique des naissances en Russie
#' jusqu'en 2017 (pour alignement avec avortements).
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_naissances <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_naissances_national %>%
    dplyr::filter(year <= 2017) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = births)) +
    ggplot2::geom_col(fill = "#3ea298") +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2015, 5), expand = c(0,0)) +
    ggplot2::scale_y_continuous(breaks = seq(5e5, 2e6, 5e5), labels = ggeo::ggeo_label_sci_10) +
    ggplot2::coord_cartesian(xlim = c(1998, 2018)) +
    theme +
    ggplot2::labs(
      subtitle = "Naissances (total)",
      x = "", y = "",
      caption = "Données: Rosstat (2021)"
    )
}

#' OC Russie: graphique des avortements
#'
#' Graphique de l'évolution des avortements
#' en Russie par rapport aux naissances.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_avortements <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_avortements_national %>%
    dplyr::mutate(fill = dplyr::case_when(
      abortions >= 100 ~ "neg",
      TRUE             ~ "pos"
    )) %>%
    dplyr::filter(year >= 1999) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = abortions)) +
    ggplot2::geom_segment(ggplot2::aes(y = 100, xend = year, yend = abortions)) +
    ggplot2::geom_point(ggplot2::aes(color = fill), size = 2,show.legend = FALSE) +
    ggplot2::geom_segment(ggplot2::aes(x = 1995, y = 100, xend = 2020, yend = 100), color = "black") +
    ggplot2::scale_fill_manual(values = c("#3ea298", "#d26d84")) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2015, 5), expand = c(0,0)) +
    ggplot2::scale_y_continuous(breaks = seq(50, 200, 50)) +
    ggplot2::coord_cartesian(xlim = c(1998, 2018)) +
    theme +
    ggplot2::labs(
      subtitle = "Avortements (pour 100 naissances)",
      x = "", y = "",
      caption = "Données: Rosstat (2021)"
    )
}

#' OC Russie: graphique des naissances et avortements
#'
#' Graphique des naissances en comparaison aux avortements.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_naissance_et_avortements <- function(theme = ggplot2::theme_minimal()) {
  require(patchwork)

  avortements <- oc_russie_graph_avortements()
  naissances <- oc_russie_graph_naissances() + ggplot2::labs(caption = "")
  naissances / avortements +
   patchwork::plot_annotation(title = "Naissances et avortements") & theme + ggplot2::theme(plot.title.position = "plot")
}

#' OC Russie: graphique des mariages et divorces
#'
#' Graphique des mariages en comparaison avec les divorces
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_mariages_et_divorces <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_mariages_divorces_national %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = data, color = indicator)) +
    ggplot2::geom_line(size = 1, show.legend = F) +
    ggplot2::scale_color_manual(
      values = c("#475286", "#907A58"),
      breaks = c("mariages", "divorces")
    ) +
    ggplot2::scale_y_continuous(labels = ggeo::ggeo_label_sci_10) +
    theme +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title = "<span style='color:#475286;'>Mariages</span> et <span style = 'color:#907A58;'>divorces</span>",
      subtitle = "en Russie entre 1990 et 2019",
      color = "",
      x = "", y = "",
      caption = "Données: Rosstat (2021)"
    )
}

#' OC Russie: graphique du solde migratoire
#'
#' Graphique du solde migratoire
#' en Russie par décénies.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_solde_migratoire <- function(theme = ggplot2::theme_minimal()) {
  geodata::un_wpp_2019_period_estimates %>%
    dplyr::filter(LocID == 643, MidPeriod < 2020) %>%
    dplyr::mutate(Time = forcats::as_factor(Time)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Time, y = CNMR)) +
    ggplot2::geom_segment(ggplot2::aes(xend = Time, y = 0, yend = CNMR)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
    theme +
    ggplot2::labs(
      title = "Solde migratoire net",
      subtitle = "en Russie",
      x = "", y = "",
      caption = "Données: UN WPP (2019)"
    )
}

#' OC Russie: graphique de l'espérance de vie des femmes de 65 ans
#'
#' Graphique de l'évolution de l'espérance de vie
#' de la Russie comparé à l'Italie et le Portugal
#' pour les femmes de 65 ans.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_esperance_65_femmes <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_hmd_lex(c("Russia", "Italy", "Portugal"), age = 65, type = "female") %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = lex, color = country)) +
    ggplot2::geom_line(size = 1.5) +
    paletteer::scale_color_paletteer_d(palette = "IslamicArt::samarqand") +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Espérance de vie",
      subtitle = "des femmes de 65 ans",
      x = "", y = "",
      color = "",
      caption = "Données: HMD (2021)"
    )
}

#' OC Russie: graphique de l'espérance de vie des hommes de 65 ans
#'
#' Graphique de l'évolution de l'espérance de vie
#' de la Russie comparé à l'Italie et le Portugal
#' pour les hommes de 65 ans.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_esperance_65_hommes <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_hmd_lex(c("Russia", "Italy", "Portugal"), age = 65, type = "male") %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = lex, color = country)) +
    ggplot2::geom_line(size = 1.5) +
    paletteer::scale_color_paletteer_d(palette = "IslamicArt::samarqand") +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Espérance de vie",
      subtitle = "des hommes de 65 ans",
      x = "", y = "",
      color = "",
      caption = "Données: HMD (2021)"
    )
}

#' OC Russie: graphique de l'espérance de vie
#'
#' Graphique de l'évolution de l'espérance de vie
#' de la Russie comparé au reste de l'Europe.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_esperance_europe <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_countries() %>%
    dplyr::filter(region_iso3c == "ECS") %>%
    dplyr::pull(iso3c) %>%
    wbstats::wb_data(
      indicator = c(lex = "SP.DYN.LE00.IN"),
      country = .,
      start_date = 1960,
      end_date = 2018
    ) %>%
    dplyr::mutate(country = countrycode::countrycode(iso3c, "iso3c", "un.name.fr", warn = F)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = lex, color = iso3c)) +
    ggplot2::geom_line(size = 1.5, show.legend = F) +
    ggplot2::scale_color_manual(values = c("#4C9AC4FF")) +
    gghighlight::gghighlight(iso3c == "RUS", use_direct_label = F,
                             unhighlighted_params = list(size = .5)) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    theme +
    ggplot2::theme(plot.subtitle = ggtext::element_markdown()) +
    ggplot2::labs(
      title = "Évolution de l'espérance de vie",
      subtitle = "en <span style='color:#4C9AC4FF;'><b>Russie</b></span>, comparé au reste de l'Europe",
      x = "", y = "",
      caption = "Données: Banque Mondiale (2021)"
    )
}

#' OC Russie: graphique de l'indice de fécondité
#'
#' Graphique de l'évolution de l'indice de fécondité
#' de la Russie comparé au reste de l'Europe.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_fertilite_europe <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_countries() %>%
    dplyr::filter(region_iso3c == "ECS") %>%
    dplyr::pull(iso3c) %>%
    wbstats::wb_data(
      indicator = c(lex = "SP.DYN.TFRT.IN"),
      country = .,
      start_date = 1960,
      end_date = 2018
    ) %>%
    dplyr::mutate(country = countrycode::countrycode(iso3c, "iso3c", "un.name.fr", warn = F)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = lex, color = iso3c)) +
    ggplot2::geom_line(size = 1.2, show.legend = F) +
    ggplot2::scale_color_manual(values = c("#4C9AC4FF")) +
    gghighlight::gghighlight(iso3c == "RUS", use_direct_label = F,
                             unhighlighted_params = list(size = .5)) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    theme +
    ggplot2::theme(plot.subtitle = ggtext::element_markdown()) +
    ggplot2::labs(
      title = "Évolution de l'indice de fécondité",
      subtitle = "en <span style='color:#4C9AC4FF;'><b>Russie<b></span>, comparé au reste de l'Europe",
      x = "", y = "",
      caption = "Données: Banque Mondiale (2021)"
    )
}

#' OC Russie: graphique de l'immigration
#'
#' Graphique de l'immigration en Russie avec les 5 pays
#' les plus représentés et une catégorie `autres`.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_immigration <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_2019_migration %>%
    dplyr::filter(type == "arrival") %>%
    dplyr::select(-c(country, type)) %>%
    tidyr::pivot_wider(names_from = "year", values_from = "migration") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(-adm1_code), na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = countrycode::countrycode(adm1_code, "iso3c", "un.name.fr",
                                                     custom_match = c("PSE" = "Palestine"))) %>%
    dplyr::mutate(country = forcats::fct_lump_n(
      country,
      n = 5, w = total,
      other_level = "Autres",
      ties.method = "max"
    )) %>%
    dplyr::select(-c(total, adm1_code)) %>%
    tidyr::pivot_longer(-country, names_to = "year", values_to = "migration") %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(migration = sum(migration, na.rm = T), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(as.numeric(year), migration)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = ggeo::ggeo_label_sci_10) +
    ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::facet_wrap(~ country, nrow = 2) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Immigration en Russie",
      subtitle = "par pays d'origine",
      x = "", y = "", fill = ""
    )

}

#' OC Russie: demogram pour l'exa 2022
#'
#' Graphique de l'évolution démographique de la Russie
#' entre 1980 et 2020. Graphique destiné à l'examen de
#' maturité2022.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_russie_graph_demo_exa <- function(theme = ggplot2::theme_minimal()) {
  gph_demogram(country = "Russian Federation", population_color = ochRe::ochre_palettes$healthy_reef[6]) +
    ggplot2::scale_color_manual(
      values = ochRe::ochre_palettes$healthy_reef[c(5,8)],
      breaks = c("cbr", "cdr"),
      labels = c("cbr" = geotools::translate_enfr("birth", "natalité"),
                 "cdr" = geotools::translate_enfr("death", "mortalité"))
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 10),
                       expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(1980, 2020)) +
    ggplot2::labs(
      title = "Évolution démographique de la Russie",
      subtitle = "1980 à 2020"
    ) +
    theme +
    ggplot2::theme(
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    )
}

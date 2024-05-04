#' OC Russie: graphique des naissances
#'
#' Graphique des naissances en Russie
#' jusqu'en 2017 (pour alignement avec avortements).
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_russie_graph_naissances()
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
      caption = "Donn\u00e9es: Rosstat (2021)"
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
#' @examples
#' oc_russie_graph_avortements()
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
      caption = "Donn\u00e9es: Rosstat (2021)"
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
#' @examples
#' oc_russie_graph_naissance_et_avortements()
oc_russie_graph_naissance_et_avortements <- function(theme = ggplot2::theme_minimal()) {
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
#' @examples
#' oc_russie_graph_mariages_et_divorces()
oc_russie_graph_mariages_et_divorces <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_mariages_divorces_national %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = data, color = indicator)) +
    ggplot2::geom_line(linewidth = 2, show.legend = F) +
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
      caption = "Donn\u00e9es: Rosstat (2021)"
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
#' @examples
#' oc_russie_graph_solde_migratoire()
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
      caption = "Donn\u00e9es: UN WPP (2019)"
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
#' @examplesIf interactive()
#' # Not run: needs credentials for HMD database
#' oc_russie_graph_esperance_65_femmes()
oc_russie_graph_esperance_65_femmes <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_hmd_lex(c("Russia", "Italy", "Portugal"), age = 65, type = "female") %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = lex, color = country)) +
    ggplot2::geom_line(size = .8) +
    paletteer::scale_color_paletteer_d(palette = "IslamicArt::samarqand") +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Esp\u00e9rance de vie",
      subtitle = "des femmes de 65 ans",
      x = "", y = "",
      color = "",
      caption = "Donn\u00e9es: HMD (2021)"
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
#' @examplesIf interactive()
#' # Not run: needs credentials for HMD database
#' oc_russie_graph_esperance_65_hommes()
oc_russie_graph_esperance_65_hommes <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_hmd_lex(c("Russia", "Italy", "Portugal"), age = 65, type = "male") %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = lex, color = country)) +
    ggplot2::geom_line(size = .8) +
    paletteer::scale_color_paletteer_d(palette = "IslamicArt::samarqand") +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Esp\u00e9rance de vie",
      subtitle = "des hommes de 65 ans",
      x = "", y = "",
      color = "",
      caption = "Donn\u00e9es: HMD (2021)"
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
#' @examples
#' oc_russie_graph_esperance_europe()
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
    ggplot2::geom_line(size = .8, show.legend = F) +
    ggplot2::scale_color_manual(values = c("#4C9AC4FF")) +
    gghighlight::gghighlight(iso3c == "RUS", use_direct_label = F,
                             unhighlighted_params = list(size = .5)) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    theme +
    ggplot2::theme(plot.subtitle = ggtext::element_markdown()) +
    ggplot2::labs(
      title = "\u00c9volution de l'esp\u00e9rance de vie",
      subtitle = "en <span style='color:#4C9AC4FF;'><b>Russie</b></span>, compar\u00e9 au reste de l'Europe",
      x = "", y = "",
      caption = "Donn\u00e9es: Banque Mondiale (2021)"
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
#' @examples
#' oc_russie_graph_fertilite_europe()
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
    ggplot2::geom_line(size = .8, show.legend = F) +
    ggplot2::scale_color_manual(values = c("#4C9AC4FF")) +
    gghighlight::gghighlight(iso3c == "RUS", use_direct_label = F,
                             unhighlighted_params = list(size = .5)) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    theme +
    ggplot2::theme(plot.subtitle = ggtext::element_markdown()) +
    ggplot2::labs(
      title = "\u00c9volution de l'indice de f\u00e9condit\u00e9",
      subtitle = "en <span style='color:#4C9AC4FF;'><b>Russie<b></span>, compar\u00e9 au reste de l'Europe",
      x = "", y = "",
      caption = "Donn\u00e9es: Banque Mondiale (2021)"
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
#' @examples
#' oc_russie_graph_immigration()
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
#' @examples
#' oc_russie_graph_demo_exa()
oc_russie_graph_demo_exa <- function(theme = ggplot2::theme_minimal()) {
  gph_demogram(country = "Russian Federation", population_color = ochRe::ochre_palettes$healthy_reef[6]) +
    ggplot2::scale_color_manual(
      values = ochRe::ochre_palettes$healthy_reef[c(5,8)],
      breaks = c("cbr", "cdr"),
      labels = c("cbr" = geotools::gtl_translate_enfr("birth", "natalit\u00e9"),
                 "cdr" = geotools::gtl_translate_enfr("death", "mortalit\u00e9"))
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 10),
                       expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(1980, 2020)) +
    ggplot2::labs(
      title = "\u00c9volution d\u00e9mographique de la Russie",
      subtitle = "1980 \u00e0 2020"
    ) +
    theme +
    ggplot2::theme(
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    )
}

#' OC Russie: Attitude envers les USA
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_russie_graph_attitude_us()
oc_russie_graph_attitude_us <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_levada_attitude_us |>
    dplyr::filter(attitude != "Difficult to answer") |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = percentage/100, color = attitude)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::percent) +
    ggplot2::scale_x_datetime(date_labels = "%Y",
                              guide = ggplot2::guide_axis(n.dodge = 3),
                              breaks = almanac::alma_search(
                                min(geodata::oc_russie_levada_attitude_us$date),
                                max(geodata::oc_russie_levada_attitude_us$date),
                                almanac::yearly()
                              ) |> lubridate::as_datetime()) +
    ggplot2::scale_color_manual(values = c("#e66b0f", "#0b6b8b")) +
    theme +
    ggplot2::labs(
      title = "Attitude **<span style='color:#0b6b8b'>positive</span>** et **<span style='color:#e66b0f'>n\u00e9gative</span>** vis-\u00e0-vis des USA en Russie",
      x = "", y = "",
      caption = "Source: sondages de l'institut Levada (1990-2023)",
      color = ""
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        lineheight = .5,
        margin = ggplot2::unit(.5, "cm")),
      legend.position = "none"
    )
}

#' OC Russie: Attitude envers l'UE
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_russie_graph_attitude_eu()
oc_russie_graph_attitude_eu <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_levada_attitude_eu |>
    dplyr::filter(attitude != "Difficult to answer") |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = percentage/100, color = attitude)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::percent) +
    ggplot2::scale_x_datetime(date_labels = "%Y",
                              guide = ggplot2::guide_axis(n.dodge = 3),
                              breaks = almanac::alma_search(
                                min(geodata::oc_russie_levada_attitude_eu$date),
                                max(geodata::oc_russie_levada_attitude_eu$date),
                                almanac::yearly()
                              ) |> lubridate::as_datetime()) +
    ggplot2::scale_color_manual(values = c("#e66b0f", "#0b6b8b")) +
    theme +
    ggplot2::labs(
      title = "Attitude **<span style='color:#0b6b8b'>positive</span>** et **<span style='color:#e66b0f'>n\u00e9gative</span>** vis-\u00e0-vis de l'UE en Russie",
      x = "", y = "",
      caption = "Source: sondages de l'institut Levada (2003-2023)",
      color = ""
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        lineheight = .5,
        margin = ggplot2::unit(.5, "cm")),
      legend.position = "none"
    )
}

# TODO
oc_russie_graph_depense_militaire <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_russie_depenses_militaires |>
    dplyr::filter(year == 2021) |>
    dplyr::arrange(-military_expenditure) |>
    dplyr::slice(1:10) |>
    dplyr::pull(iso3c) -> top_expenditures

  geodata::oc_russie_depenses_militaires |>
    dplyr::filter(iso3c %in% top_expenditures) |>
    dplyr::mutate(pays = countrycode::countrycode(iso3c, "iso3c", "country.name.fr")) |>
    dplyr::group_by(year) |>
    dplyr::arrange(year, military_expenditure) |>
    dplyr::mutate(rank = 1:dplyr::n()) |>
    ggplot2::ggplot() +
    ggplot2::aes(xmin = 0 ,
                 xmax = military_expenditure / 1000) +
    ggplot2::aes(ymin = rank - .45,
                 ymax = rank + .45,
                 y = rank) +
    ggplot2::facet_wrap(~ year) +
    ggplot2::geom_rect(alpha = .7) +
    ggplot2::geom_text(col = "gray13",
                       hjust = "right",
                       ggplot2::aes(label = pays),
                       x = -1) +
    ggplot2::labs(x = 'D\u00e9penses militaires (mia)') +
    ggplot2::labs(y = '') +
    ggplot2::labs(caption = 'Source: SIPRI') +
    ggeo::ggeotheme("islamic_samarquand") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) -> full_plot

  full_plot +
    ggplot2::facet_null() +
    ggplot2::scale_x_continuous(
      limits = c(-200, 900),
      breaks = c(seq(0,800,200))) +
    ggplot2::geom_text(x = 500, y = 1.5,
                       ggplot2::aes(label = as.character(year)),
                       size = 30, col = "grey18") +
    ggplot2::aes(group = country) +
    gganimate::transition_time(year) -> animated_plot

  gganimate::animate(
    animated_plot,
    fps = 5, renderer = gganimate::ffmpeg_renderer(),
    width = 1600, height = 800, res = 150
  ) -> plot_animation
}

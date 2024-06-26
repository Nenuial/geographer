# Afghanistan ---------------------------------------------------------------------------------

#' Scolarisation des filles en Afghanistan
#'
#' @param theme A ggplot2 theme
#' @param line_color Graph line color
#'
#' @name oc_geo_au_feminin_graphs_scolarisation_filles_afghanistan
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_scolarisation_filles_afghanistan()
#' oc_geo_au_feminin_hc_scolarisation_filles_afghanistan()
oc_geo_au_feminin_graph_scolarisation_filles_afghanistan <- function(theme = ggplot2::theme_minimal(),
                                                                     line_color = "black") {
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
    ggplot2::annotate("rect",
      xmin = 1996, xmax = 2001, ymin = 0, ymax = 1,
      fill = "#bf616a", alpha = .2
    ) +
    ggplot2::annotate("rect",
      xmin = 2021, xmax = 2025, ymin = 0, ymax = 1,
      fill = "#bf616a", alpha = .2
    ) +
    ggplot2::coord_cartesian(xlim = c(1970, 2024), ylim = c(0, .9)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Le retour des talibans\u2026",
      subtitle = "Pourcentage de filles allant \u00e0 l'\u00e9cole primaire",
      x = "", y = "",
      caption = "Source: John Burn-Murdoch (FT), Donn\u00e9es: Banque Mondiale"
    ) +
    theme
}

#' @rdname oc_geo_au_feminin_graphs_scolarisation_filles_afghanistan
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
    highcharter::hc_title(text = "Le retour des talibans\u2026") |>
    highcharter::hc_subtitle(text = "Pourcentage de filles allant \u00e0 l'\u00e9cole primaire") |>
    highcharter::hc_caption(text = "Source: John Burn-Murdoch (FT), Donn\u00e9es: Banque Mondiale") |>
    highcharter::hc_xAxis(
      title = list(text = ""),
      max = 2025,
      showInLegend = FALSE,
      plotBands = list(
        list(
          from = 1996,
          to = 2001,
          color = "#bf616a",
          label = list(
            text = "Talibans",
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
            text = "???",
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

#' PIB de l'Afghanistan par habitant
#'
#' Graphiques avec l'évolution du PIB par habitant en
#' Afghanistan
#'
#' @param theme A ggplot2 theme
#' @param line_color Graph line color
#'
#' @name oc_geo_au_feminin_graphs_situation_eco_afghanistan
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_situation_eco_afghanistan()
#' oc_geo_au_feminin_hc_situation_eco_afghanistan()
oc_geo_au_feminin_graph_situation_eco_afghanistan <- function(theme = ggplot2::theme_minimal(), line_color = "black") {
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
      title = "Une \u00e9conomie qui ralentit",
      y = "PIB par hab.", x = "",
      caption = "Donn\u00e9es: Banque Mondiale"
    ) +
    theme
}

#' @rdname oc_geo_au_feminin_graphs_situation_eco_afghanistan
#' @export
oc_geo_au_feminin_hc_situation_eco_afghanistan <- function() {
  wbstats::wb_data(
    indicator = c(gdp = "NY.GDP.PCAP.CD"),
    country = "Afghanistan",
    start_date = 1960,
    end_date = 2022
  ) -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Une \u00e9conomie qui ralentit") |>
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
      highcharter::hcaes(x = date, y = round(gdp, 2))
    ) |>
    ggeo::hc_purple_theme()
}


# Avortement ----------------------------------------------------------------------------------

#' Avortement en Suisse
#'
#' Graphiques sur l'évolution du taux d'avortement en Suisse
#' et l'évolution politique à ce sujet.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_graphs_suisse_avortements
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_suisse_avortements()
#' oc_geo_au_feminin_graph_suisse_avortements_par_ages()
#' oc_geo_au_feminin_hc_suisse_avortements_par_ages()
#' oc_geo_au_feminin_graph_suisse_avortements_par_regions()
#' oc_geo_au_feminin_hc_suisse_avortements_par_regions()
#' oc_geo_au_feminin_graph_suisse_votations_avortement()
#' oc_geo_au_feminin_hc_suisse_votations_avortement()
oc_geo_au_feminin_graph_suisse_avortements <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2021_ofs_avortements |>
    ggplot2::ggplot(ggplot2::aes(year, rate, color = type)) +
    ggplot2::geom_line(linewidth = 2.5) +
    paletteer::scale_color_paletteer_d(
      "nord::lumina",
      labels = c("de 15 \u00e0 49 ans", "de 15 \u00e0 19 ans")
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

#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_hc_suisse_avortements_par_ages <- function() {
  geodata::oc_geo_au_feminin_2022_ofs_avortements_par_ages |>
    dplyr::filter(unit == "%") |>
    dplyr::mutate(age_group = forcats::fct_inorder(age_group)) |>
    dplyr::rename(name = age_group, value = abortion) -> data

  data |>
    highcharter::hchart("column", highcharter::hcaes(x = name, y = value), name = "Avortements") |>
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{text} %")) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_plotOptions(
      column = list(colorByPoint = TRUE)
    ) |>
    highcharter::hc_colors(palettetown::ichooseyou("salamence", 9)) |>
    highcharter::hc_title(text = "Interruptions de grossesse par classe d'\u00e2ge") |>
    highcharter::hc_subtitle(text = "sur 11 143 en Suisse en 2022") |>
    highcharter::hc_caption(text = "Source: OFS (2022)")
}

#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_graph_suisse_avortements_par_ages <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2022_ofs_avortements_par_ages |>
    dplyr::filter(unit == "%") |>
    dplyr::mutate(age_group = forcats::fct_inorder(age_group)) |>
    ggplot2::ggplot(ggplot2::aes(x = age_group, y = abortion, fill = age_group)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    paletteer::scale_fill_paletteer_d("palettetown::salamence") +
    theme +
    ggplot2::labs(
      title = "Interruptions de grossesse par classe d'\u00e2ge",
      subtitle = "sur 11 143 en Suisse en 2022",
      x = "", y = "",
      caption = "Source: OFS (2022)"
    )
}

#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_hc_suisse_avortements_par_regions <- function() {
  geodata::oc_geo_au_feminin_ofs_avortements_par_region |>
    highcharter::hchart(
      "line",
      highcharter::hcaes(x = annee, y = avortement, group = region)
    ) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(min = 0, title = list(text = "")) |>
    highcharter::hc_plotOptions(
      line = list(
        marker = list(
          enabled = FALSE,
          symbol = "circle"
        )
      )
    ) |>
    highcharter::hc_colors(as.character(MetBrewer::met.brewer("Tiepolo", 8))) |>
    highcharter::hc_title(text = "Le taux d'avortement varie fortement selon les r\u00e9gions en Suisse") |>
    highcharter::hc_subtitle(text = "Taux d'interruptions de grossesse pour 1000 femmes de 15\u201344 ans") |>
    highcharter::hc_caption(text = "Source: OFS")
}


#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_graph_suisse_avortements_par_regions <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_ofs_avortements_par_region |>
    dplyr::filter(annee == 2022) |>
    ggplot2::ggplot(ggplot2::aes(x = annee, y = avortement, color = region, group = region)) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 10, by = 2),
      limits = c(0, 11),
      expand = c(0, NA)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(2008, 2022, by = 4),
      limits = c(NA, 2028),
      expand = c(0, NA)
    ) +
    MetBrewer::scale_color_met_d("Tiepolo") +
    # "Artificial" grid lines
    ggplot2::geom_vline(
      xintercept = seq(2008, 2022, by = 4),
      color = "grey91",
      linewidth = .6
    ) +
    ggplot2::geom_segment(
      data = tibble::tibble(y = seq(0, 10, by = 2), x1 = 2007, x2 = 2022),
      ggplot2::aes(x = x1, xend = x2, y = y, yend = y),
      inherit.aes = FALSE,
      color = "grey91",
      linewidth = .6
    ) +
    ggplot2::geom_line(
      linewidth = .8,
      data = geodata::oc_geo_au_feminin_ofs_avortements_par_region,
      show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = region),
      direction = "y",
      xlim = c(2023, NA),
      hjust = 0,
      segment.size = .7,
      segment.alpha = .5,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      show.legend = FALSE
    ) +
    theme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Le taux d'avortement varie fortement selon les r\u00e9gions en Suisse",
      subtitle = "Taux d'interruptions de grossesse pour 1000 femmes de 15\u201344 ans",
      x = "", y = "",
      caption = "Source: OFS"
    )
}

#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_hc_suisse_votations_avortement <- function() {
  geodata::oc_geo_au_feminin_votations_avortement |>
    dplyr::mutate(Votation = forcats::fct_inorder(Votation)) |>
    tidyr::pivot_longer(-Votation, names_to = "Vote", values_to = "Pourcent") |>
    dplyr::mutate(Color = dplyr::if_else(Vote == "Oui", "#457aad", "#cc504c")) -> data

  data |>
    highcharter::hchart("bar", highcharter::hcaes(x = Votation, y = Pourcent, color = Color)) |>
    highcharter::hc_chart(inverted = TRUE) |>
    highcharter::hc_plotOptions(
      bar = list(
        colorByPoint = TRUE,
        stacking = "percent",
        dataLabels = list(
          enabled = TRUE,
          format = "{point.percentage:.0f}%"
        )
      )
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.Vote + '</b> :' + this.point.Pourcent + ' %' }}"))
    ) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_title(
      text = "La population suisse a toujours refus\u00e9 les restrictions du droit \u00e0 l'avortement"
    )
}

#' @rdname oc_geo_au_feminin_graphs_suisse_avortements
#' @export
oc_geo_au_feminin_graph_suisse_votations_avortement <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_votations_avortement |>
    dplyr::mutate(Votation = forcats::fct_inorder(Votation)) |>
    tidyr::pivot_longer(-Votation, names_to = "Vote", values_to = "Pourcent") |>
    ggplot2::ggplot(ggplot2::aes(y = Votation, x = Pourcent, fill = Vote)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(40)) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = c("#cc504c", "#457aad")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(
      title = "La population suisse a toujours refus\u00e9 les restrictions du droit \u00e0 l'avortement",
      x = "", y = "", fill = ""
    )
}


# Sex-ratio -----------------------------------------------------------------------------------

# TODO
# nolint start: commented_code_linter
# oc_geo_au_feminin_graph_rapport_hommes_femmes_2020 <- function(theme = ggplot2::theme_minimal()) {
#   geodata::oc_geo_au_feminin_wpp2019_sex_ratio |>
#     dplyr::filter(
#       un_code %in% c(900, 903, 935, 908, 905, 909, 904),
#       year == 2020
#     ) |>
#     dplyr::mutate(age = forcats::fct_inorder(age)) |>
#     dplyr::select(age, ratio, name) |>
#     ggplot2::ggplot(ggplot2::aes(x = age, y = ratio, color = name, group = name)) +
#     ggplot2::geom_line()
# }
#
# TODO
# oc_geo_au_feminin_hc_sex_ratio <- function() {
#   geodata::oc_geo_au_feminin_sex_ratio |>
#     dplyr::filter(Year %in% c(1950, 2022)) |>
#     dplyr::filter(Region == "Geographic region") |>
#     dplyr::rename(value = SRB)
# }
# nolint end


# Violences -----------------------------------------------------------------------------------

#' Expérience de violences sexuelles
#'
#' Graphiques visualisant les résultats de l'enquête sur
#' les violences sexuelles en Suisse réalisée par GFS.Bern
#' sur demande d'Amnesty international en avril 2019.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_graphs_2019_violences_sexuelles
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_2019_violences_sexuelles_experiences()
#' oc_geo_au_feminin_graph_2019_violences_sexuelles_actes()
#' oc_geo_au_feminin_graph_2019_violences_sexuelles_police()
oc_geo_au_feminin_graph_2019_violences_sexuelles_experiences <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2019_gfs_violences_sexuelles_experiences |>
    dplyr::arrange(oui) |>
    dplyr::mutate(Experience = forcats::fct_inorder(Experience)) |>
    tidyr::pivot_longer(
      cols = c(-"Experience"),
      names_to = "Reponse",
      values_to = "Pourcentage"
    ) |>
    dplyr::mutate(Label = dplyr::if_else(Pourcentage > 2, Pourcentage, NA_real_)) |>
    dplyr::mutate(Reponse = forcats::fct_inorder(Reponse)) |>
    ggplot2::ggplot(ggplot2::aes(y = Experience, x = Pourcentage, fill = Reponse)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label),
      position = ggplot2::position_stack(vjust = .5, reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(
      values = c("#0c60ae", "#1ea1ec", "#fbbf28", "#999999"),
      labels = c("oui", "ambigu\u00eb", "non", "ne sait pas")
    ) +
    ggplot2::labs(
      subtitle = "% de femmes d\u00e8s 16 ans",
      x = "", y = "", fill = "",
      caption = "\u00a9 gfs.bern, enqu\u00eate sur les violences sexuelles, avril 2019 (N = 4'495)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

#' @rdname oc_geo_au_feminin_graphs_2019_violences_sexuelles
#' @export
oc_geo_au_feminin_graph_2019_violences_sexuelles_actes <- function(theme = ggplot2::theme_minimal()) {
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
      subtitle = "% de femmes d\u00e8s 16 ans",
      x = "", y = "", fill = "",
      caption = "\u00a9 gfs.bern, enqu\u00eate sur les violences sexuelles, avril 2019 (N = 4'495)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

#' @rdname oc_geo_au_feminin_graphs_2019_violences_sexuelles
#' @export
oc_geo_au_feminin_graph_2019_violences_sexuelles_police <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2019_gfs_violences_sexuelles_police |>
    dplyr::arrange(oui) |>
    dplyr::mutate(Raison = forcats::fct_inorder(Raison)) |>
    tidyr::pivot_longer(
      cols = c(-"Raison"),
      names_to = "Reponse",
      values_to = "Pourcentage"
    ) |>
    dplyr::mutate(Label = dplyr::if_else(Pourcentage > 2, Pourcentage, NA_real_)) |>
    dplyr::mutate(Reponse = forcats::fct_inorder(Reponse)) |>
    ggplot2::ggplot(ggplot2::aes(y = Raison, x = Pourcentage, fill = Reponse)) +
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
      subtitle = "% de femmes d\u00e8s 16 ans qui ne se sont pas
      adress\u00e9es\n\u00e0 la police apr\u00e8s un acte sexuel non consenti",
      x = "", y = "", fill = "",
      caption = "\u00a9 gfs.bern, enqu\u00eate violences sexuelles, avril 2019 (n = 1239)"
    ) +
    theme +
    ggplot2::theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}


# Démographie ---------------------------------------------------------------------------------

#' Espérance de vie
#'
#' Graphiques sur l'évolution de l'espérence de vie.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_graphs_esperances_de_vie
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_esperances_de_vie()
#' @examplesIf interactive()
#' # Not run: need credentials for HMD database
#' oc_geo_au_feminin_graph_difference_esperance_de_vie()
oc_geo_au_feminin_graph_esperances_de_vie <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c(
      "lexf" = "SP.DYN.LE00.FE.IN",
      "lexm" = "SP.DYN.LE00.MA.IN"
    ),
    start_date = 1960, end_date = 2022
  ) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = lexf, color = "purple")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = lexm, color = "blue")) +
    geofacet::facet_geo(~iso3c, grid = geofacet::europe_countries_grid2)
}

#' @rdname oc_geo_au_feminin_graphs_esperances_de_vie
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
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_y_continuous(limits = c(3, NA)) +
    theme
}


# Politique -----------------------------------------------------------------------------------

#' OC Géo au féminin: représentation politique
#'
#' Pourcentage de femmes dans les parlements nationaux par régions
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_proportion_parlements()
oc_geo_au_feminin_graph_proportion_parlements <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c("parl" = "SG.GEN.PARL.ZS"),
    start_date = 1997,
    end_date = 2022,
    country = "regions_only"
  ) -> data

  data |>
    dplyr::rename(
      Annee = date,
      Representation = parl,
      Region = country
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = Annee, y = Representation, fill = Region, color = Region)) +
    ggplot2::geom_area(show.legend = FALSE) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Region, nrow = 2) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    ggplot2::scale_fill_manual(values = MetBrewer::met.brewer("Nizami")[c(2:8)]) +
    ggplot2::scale_color_manual(values = MetBrewer::met.brewer("Nizami")[c(2:8)]) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      title = "Repr\u00e9sentation politique par r\u00e9gion",
      x = "", y = ""
    ) +
    theme
}


# Économie ------------------------------------------------------------------------------------

#' Différences salariales en Suisse
#'
#' Graphiques présentant les différences salariale en Suisse
#' par formation et par position.
#'
#' @name oc_geo_au_feminin_graphs_difference_salariale
#' @param theme A ggplot2 theme
#'
#' @return A graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_difference_salariale_brut_par_formation()
#' oc_geo_au_feminin_graph_difference_salariale_brut_par_position()
#' oc_geo_au_feminin_graph_difference_salariale_pourcent_par_formation()
#' oc_geo_au_feminin_graph_difference_salariale_pourcent_par_position()
oc_geo_au_feminin_graph_difference_salariale_brut_par_formation <- function(theme = ggplot2::theme_minimal()) {
  tibble::tribble(
    ~Formation, ~Homme, ~Femme,
    "Haute \u00e9cole universitaire (UNI, EPF)", 10767, 8683,
    "Haute \u00e9cole sp\u00e9cialis\u00e9e (HES),\n Haute \u00e9cole p\u00e9dagogique (HEP)", 9648, 7959,
    "Formation prof. sup\u00e9rieure, \u00e9coles sup.", 8728, 7434,
    "Brevet d'enseignement", 8985, 8479,
    "Maturit\u00e9", 7073, 6217,
    "Apprentissage complet (CFC)", 6293, 5612,
    "Formation acquise en entreprise", 5543, 4619,
    "Sans formation professionnelle compl\u00e8te", 5209, 4395
  ) |>
    tidyr::pivot_longer(-Formation, names_to = "Sexe", values_to = "Salaire") |>
    dplyr::arrange(c(Salaire)) |>
    dplyr::mutate(Formation = forcats::fct_inorder(Formation)) |>
    ggplot2::ggplot(ggplot2::aes(y = Formation, x = Salaire, fill = Sexe)) +
    ggplot2::geom_col(position = "dodge2") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      color = "",
      caption = "Source: OFS (2016)"
    ) +
    theme
}

#' @rdname oc_geo_au_feminin_graphs_difference_salariale
#' @export
oc_geo_au_feminin_graph_difference_salariale_pourcent_par_formation <- function(theme = ggplot2::theme_minimal()) {
  tibble::tribble(
    ~Formation, ~Pourcent,
    "Haute \u00e9cole universitaire (UNI, EPF)", 19.4,
    "Haute \u00e9cole sp\u00e9cialis\u00e9e (HES),\n Haute \u00e9cole p\u00e9dagogique (HEP)", 17.5,
    "Formation prof. sup\u00e9rieure, \u00e9coles sup.", 14.8,
    "Brevet d'enseignement", 5.6,
    "Maturit\u00e9", 12.1,
    "Apprentissage complet (CFC)", 10.8,
    "Formation acquise en entreprise", 16.7,
    "Sans formation professionnelle compl\u00e8te", 15.6,
  ) |>
    dplyr::arrange(c(Pourcent)) |>
    dplyr::mutate(Formation = forcats::fct_inorder(Formation)) |>
    ggplot2::ggplot(ggplot2::aes(y = Formation, x = Pourcent)) +
    ggplot2::geom_col(fill = "#2e9093") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(caption = "Source: OFS (2016)") +
    theme
}

#' @rdname oc_geo_au_feminin_graphs_difference_salariale
#' @export
oc_geo_au_feminin_graph_difference_salariale_brut_par_position <- function(theme = ggplot2::theme_minimal()) {
  tibble::tribble(
    ~Formation, ~Homme, ~Femme,
    "Cadre sup\u00e9rieur et moyen", 10878, 8861,
    "Cadre inf\u00e9rieur", 8760, 7580,
    "Responsable de\nl'ex\u00e9cution de travaux", 7238, 6481,
    "Sans fonction\nde cadre", 6121, 5607,
  ) |>
    tidyr::pivot_longer(-Formation, names_to = "Sexe", values_to = "Salaire") |>
    dplyr::arrange(c(Salaire)) |>
    dplyr::mutate(Formation = forcats::fct_inorder(Formation)) |>
    ggplot2::ggplot(ggplot2::aes(y = Formation, x = Salaire, fill = Sexe)) +
    ggplot2::geom_col(position = "dodge2") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      color = "",
      caption = "Source: OFS (2016)"
    ) +
    theme
}

#' @rdname oc_geo_au_feminin_graphs_difference_salariale
#' @export
oc_geo_au_feminin_graph_difference_salariale_pourcent_par_position <- function(theme = ggplot2::theme_minimal()) {
  tibble::tribble(
    ~Formation, ~Pourcent,
    "Cadre sup\u00e9rieur et moyen", 18.5,
    "Cadre inf\u00e9rieur", 13.5,
    "Responsable de\nl'ex\u00e9cution de travaux", 10.5,
    "Sans fonction\nde cadre", 8.4,
  ) |>
    dplyr::arrange(c(Pourcent)) |>
    dplyr::mutate(Formation = forcats::fct_inorder(Formation)) |>
    ggplot2::ggplot(ggplot2::aes(y = Formation, x = Pourcent)) +
    ggplot2::geom_col(fill = "#2e9093") +
    ggplot2::labs(caption = "Source: OFS (2016)") +
    theme
}


# Mesure d'inégalités -------------------------------------------------------------------------

#' Taux de jeunes apprenant à coder
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_graphs_programmation_16_24_ans
#' @return A ggplot2 graph
#' @export
#' @examples
#' oc_geo_au_feminin_graph_programmation_16_24_ans()
#' oc_geo_au_feminin_hc_programmation_16_24_ans()
oc_geo_au_feminin_graph_programmation_16_24_ans <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_oecd_programmation_16_24_ans |>
    dplyr::filter(Gender != "Individuals") |>
    dplyr::filter(ISO != "EU") |>
    dplyr::filter(Time == 2021) |>
    dplyr::mutate(Pays = countrycode::countrycode(ISO, origin = "iso3c", "country.name.fr")) |>
    dplyr::arrange(-Value) |>
    dplyr::mutate(Pays = forcats::fct_inorder(Pays)) |>
    ggplot2::ggplot(ggplot2::aes(y = Pays, x = Value, color = Gender)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_color_manual(
      values = c("#2a6096", "#5e813f"),
      labels = c("Femmes", "Hommes")
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Jeunes de 16 \u00e0 24 ans apprenant \u00e0 coder, 2019",
      color = "", x = "", y = "",
      caption = "Source: OCDE (2021), OECD Going Digital Toolkit"
    )
}

#' @rdname oc_geo_au_feminin_graphs_programmation_16_24_ans
#' @export
oc_geo_au_feminin_hc_programmation_16_24_ans <- function() {
  geodata::oc_geo_au_feminin_oecd_programmation_16_24_ans |>
    dplyr::filter(Gender != "Individuals") |>
    dplyr::filter(ISO != "EU") |>
    dplyr::filter(Time == 2021) |>
    dplyr::mutate(Pays = countrycode::countrycode(ISO, origin = "iso3c", "country.name.fr")) |>
    dplyr::select(Pays, Gender, Value) |>
    dplyr::mutate(Value = round(Value, 2)) |>
    tidyr::pivot_wider(names_from = Gender, values_from = Value) |>
    dplyr::arrange(Male) |>
    dplyr::rename(name = Pays, high = Male, low = Female) -> data

  highcharter::highchart() |>
    highcharter::hc_title(text = "Jeunes de 16 \u00e0 24 ans apprenant \u00e0 coder, 2019") |>
    highcharter::hc_caption(
      text = "Source: OCDE (2021), <a href='https://goingdigital.oecd.org/indicator/54'
      target='_blank'>OECD Going Digital Toolkit</a>"
    ) |>
    highcharter::hc_add_series(
      name = "",
      type = "dumbbell",
      data = data,
      connectorColor = "black",
      marker = list(
        fillColor = "#5e813f"
      ),
      lowColor = "#2a6096",
      showInLegend = FALSE
    ) |>
    highcharter::hc_chart(
      inverted = TRUE
    ) |>
    highcharter::hc_xAxis(type = "category") |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.name + '</b><br/>' +
          'Femmes: ' + this.point.low + ' \u2013 ' + 'Hommes: ' + this.point.high }}"))
    )
}

# nolint start: commented_code_linter
# oc_geo_au_feminin_carte_mesure_inegalite_2023 <- function() {
#   TODO: Create a map for the student's inequality measure
#   wbstats::wb_data("SP.POP.TOTL", start_date = 2020, end_date = 2020) -> data
#
#   recipes::recipe(~., data = data) |>
#     recipes::step_range(SP.POP.TOTL, min = 0, max = 1) |>
#     recipes::prep() |>
#     recipes::bake(new_data = NULL) -> data_clean
# }
# nolint end

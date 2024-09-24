#' Suffrage féminin
#'
#' Une carte de l'introduction du suffrage féminin
#' au niveau cantonal en Suisse.
#'
#' @param theme A ggplot2 theme
#'
#' @return ggplot2 map
#' @concept oc suisse votations suffrage femmes
#'
#' @export
#' @examples
#' oc_suisse_carte_suffrage_feminin()
oc_suisse_carte_suffrage_feminin <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_suffrage_feminin |>
    dplyr::mutate(id = geotools::gtl_swiss_canton_id(Canton)) |>
    dplyr::mutate(Annee = forcats::as_factor(Annee)) |>
    dplyr::left_join(
      themakart::thema_map("inst", "kant"),
      by = "id"
    ) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    gph_map_swiss_relief() +
    ggplot2::geom_sf(ggplot2::aes(fill = Annee, geometry = geometry),
      color = "white", size = 0.1
    ) +
    gph_map_swiss_lakes() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_d("ggthemes::Classic_Cyclic", 11),
        alpha = .8
      ),
      breaks = levels(plot_data$Annee),
      limits = levels(plot_data$Annee)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
    ) +
    ggplot2::labs(
      title = "Introduction du suffrage f\u00e9minin en Suisse",
      subtitle = "par cantons",
      x = "",
      y = "",
      fill = "",
      caption = "Fond: OFS ThemaKart (2020), Donn\u00e9es: Wikip\u00e9dia"
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom")
}

#' Suffrage féminin au valais
#'
#' Une carte du résultat de la votation sur le suffrage féminin
#' au canton du Valais le 12 avril 1970.
#'
#' @param theme A ggplot2 theme
#'
#' @return ggplot2 map
#' @concept oc suisse votations suffrage femmes valais
#'
#' @export
#' @examples
#' oc_suisse_carte_1970_suffrage_feminin_valais()
oc_suisse_carte_1970_suffrage_feminin_valais <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_1970_04_12_suffrage_feminin_vs |>
    dplyr::mutate(per_oui = vote_oui / (vote_oui + vote_non) * 100) |>
    dplyr::mutate(per_oui = santoku::chop(per_oui, c(0, 70, 72, 74, 76, 78, 100))) |>
    dplyr::left_join(
      themakart::thema_map("inst", "bezk", 2000),
      by = "id"
    ) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    gph_map_swiss_relief() +
    ggplot2::geom_sf(ggplot2::aes(fill = per_oui, geometry = geometry),
      color = "white", size = 0.1
    ) +
    gph_map_swiss_lakes() +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("viridis::viridis", direction = -1, 7)[2:7],
        alpha = .8
      ),
      breaks = levels(plot_data$per_oui),
      limits = levels(plot_data$per_oui),
      labels = scales::percent_format(scale = 1, accuracy = 1)
    ) +
    ggplot2::coord_sf(
      datum = NA,
      xlim = c(sf::st_bbox(plot_data$geometry)[["xmin"]], sf::st_bbox(plot_data$geometry)[["xmax"]]),
      ylim = c(sf::st_bbox(plot_data$geometry)[["ymin"]], sf::st_bbox(plot_data$geometry)[["ymax"]])
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barheight = 10, label.hjust = 1
      )
    ) +
    ggplot2::labs(
      title = "Votation sur le suffrage f\u00e9minin au Valais",
      subtitle = "Dimanche 12 avril 1970, par districts",
      fill = "",
      x = "",
      y = "",
      caption = "Fond: OFS ThemaKart (2020), Donn\u00e9es: Gazette de Lausanne (1970)"
    ) +
    theme
}

#' Religion par commune
#'
#' @returns A highcharts map
#' @export
#'
#' @examples
#' oc_suisse_carte_hc_2020_religion()
oc_suisse_carte_hc_2020_religion <- function() {
  geodata::oc_suisse_2000_religion |>
    dplyr::select(-Commune) |>
    dplyr::filter(Religion != "Total") |>
    dplyr::group_by(ID) |>
    dplyr::arrange(dplyr::desc(Pourcentage)) |>
    dplyr::summarise(
      Religion = dplyr::first(Religion),
      Pourcentage = dplyr::first(Pourcentage)
    ) |>
    dplyr::mutate(value = round(Pourcentage, 2)) -> data

  themakart::thema_map("inst", "polg", 2000, "gf") |>
    dplyr::mutate(mun_id = as.numeric(id)) -> swiss

  swiss |>
    dplyr::left_join(data, dplyr::join_by(mun_id == ID)) |>
    dplyr::filter(Religion == "Eglise catholique romaine") |>
    geojsonio::geojson_json() -> map_catho

  swiss |>
    dplyr::left_join(data, dplyr::join_by(mun_id == ID)) |>
    dplyr::filter(Religion == "Eglise \u00e9vang\u00e9lique r\u00e9form\u00e9e") |>
    geojsonio::geojson_json() -> map_protestant

  swiss |>
    dplyr::left_join(data, dplyr::join_by(mun_id == ID)) |>
    dplyr::filter(Religion == "Autres Eglises et communaut\u00e9s protestantes") |>
    geojsonio::geojson_json() -> map_autre

  swiss |>
    dplyr::left_join(data, dplyr::join_by(mun_id == ID)) |>
    dplyr::filter(Religion == "Aucune appartenance") |>
    geojsonio::geojson_json() -> map_athee

  palette_catho <- function(...) {
    paletteer::paletteer_c("ggthemes::Green", ...)
  }

  palette_protestant <- function(...) {
    paletteer::paletteer_c("ggthemes::Orange", ...)
  }

  palette_autre <- function(...) {
    paletteer::paletteer_c("ggthemes::Purple", ...)
  }

  palette_athee <- function(...) {
    paletteer::paletteer_c("ggthemes::Gray", ...)
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = swiss |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      borderColor = "transparent",
      borderWidth = 0.1,
      data = c()
    ) |>
    highcharter::hc_add_series(
      mapData = map_catho,
      data = data |> dplyr::filter(Religion == "Eglise catholique romaine"),
      joinBy = c("mun_id", "ID"),
      name = "Catholique",
      color = "#406975",
      borderColor = "transparent",
      borderWidth = 0.1,
      value = "value"
    ) |>
    highcharter::hc_add_series(
      mapData = map_protestant,
      data = data |> dplyr::filter(Religion == "Eglise \u00e9vang\u00e9lique r\u00e9form\u00e9e"),
      joinBy = c("mun_id", "ID"),
      name = "Protestant \u00e9vang\u00e9lique",
      color = "#fd8f24",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 1
    ) |>
    highcharter::hc_add_series(
      mapData = map_autre,
      data = data |> dplyr::filter(Religion == "Autres Eglises et communaut\u00e9s protestantes"),
      joinBy = c("mun_id", "ID"),
      name = "Autres protestants",
      color = "#475286",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 2
    ) |>
    highcharter::hc_add_series(
      mapData = map_athee,
      data = data |> dplyr::filter(Religion == "Aucune appartenance"),
      joinBy = c("mun_id", "ID"),
      name = "Ath\u00e9e",
      color = "#a59da2",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 4
    ) |>
    highcharter::hc_add_series(
      name = "Lakes",
      mapData = geodata::gdt_opendata_swiss_geodata("lake") |>
        sf::st_transform(crs = sf::st_crs(swiss)) |>
        geojsonio::geojson_json(),
      showInLegend = FALSE,
      color = "#8ec6e9",
      nullColor = "#8ec6e9",
      borderColor = "transparent",
      borderWidth = 0.1,
      data = c()
    ) |>
    highcharter::hc_colorAxis(
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(Religion == "Eglise catholique romaine") |>
              dplyr::pull(value),
            c(25, 50, 75, 100)
          ), palette_catho
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(Religion == "Eglise \u00e9vang\u00e9lique r\u00e9form\u00e9e") |>
              dplyr::pull(value),
            c(25, 50, 75, 100)
          ), palette_protestant
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(Religion == "Autres Eglises et communaut\u00e9s protestantes") |>
              dplyr::pull(value),
            c(25, 50, 75, 100)
          ), palette_autre
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(Religion == "Aucune appartenance") |>
              dplyr::pull(value),
            c(25, 50, 75, 100)
          ), palette_athee
        ),
        showInLegend = FALSE
      )
    ) |>
    highcharter::hc_caption(
      text = "Source : <a href='https://www.bfs.admin.ch/bfs/fr/home.html' target='_blank'>OFS (2000)</a>"
    )
}

# Démographie ------------------------------------------------------------

#' Indice de fécondité en Russie
#'
#' Une carte de l'indice de fécondité en 2023
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#' @param barwidth Width of legend bar
#' @param greyscale Boolean: whether to create a grey scale map
#'
#' @return A map
#' @concept oc russie fertilité
#'
#' @export
#' @examples
#' oc_russie_carte_fecondite()
#' oc_russie_carte_hc_fecondite()
oc_russie_carte_fecondite <- function(theme = ggplot2::theme_minimal(), barwidth = 40, greyscale = FALSE) {
  if (greyscale) {
    fill_scale <- ggplot2::scale_fill_stepsn(
      n.breaks = 7,
      colours = paletteer::paletteer_c("pals::kovesi.linear_grey_10_95_c0", 50, direction = -1)
    )
  } else {
    fill_scale <- ggplot2::scale_fill_stepsn(
      n.breaks = 7,
      colours = wesanderson::wes_palette("Zissou1", 50, type = "continuous")
    )
  }

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_2023_fecondite |>
        dplyr::filter(year == 2023),
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = tfr),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    fill_scale +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barwidth = barwidth
      )
    ) +
    ggplot2::labs(
      title = "Indice de f\u00e9condit\u00e9",
      subtitle = "par r\u00e9gions en 2023",
      fill = "",
      caption = "Source: Rosstat (2024)"
    )
}

#' @rdname oc_russie_carte_fecondite
#' @export
oc_russie_carte_hc_fecondite <- function() {
  geodata::oc_russie_2023_fecondite |>
    dplyr::filter(year == 2023) |>
    dplyr::rename(value = tfr) -> data

  palette_fecondite <- function(...) {
    wesanderson::wes_palette("Zissou1", ..., type = "continuous")
  }

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") -> russia

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Indice de f\u00e9condit\u00e9",
      mapData = russia |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      borderColor = "transparent",
      borderWidth = 0.1,
      joinBy = "adm1_code",
      value = "value",
      nullColor = "#838a8f",
      data = data
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "Orthographic",
        rotation = c(-90, -80)
      )
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(
          data$value,
          c(1.2, 1.5, 1.8, 2.1)
        ), palette_fecondite
      ),
      showInLegend = TRUE
    )
}

#' Taux de natalité en Russie
#'
#' Une carte du taux de natalité en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#' @param barwidth Width of legend bar
#'
#' @return A ggplot2 map
#' @concept oc russie fertilité
#'
#' @export
#' @examples
#' oc_russie_carte_natalite()
oc_russie_carte_natalite <- function(theme = ggplot2::theme_minimal(), barwidth = 40) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_2019_natalite_mortalite |>
        dplyr::filter(indicator == "cbr", type == "per1000"),
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, direction = -1) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barwidth = barwidth
      )
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Taux de natalit\u00e9",
      subtitle = "par r\u00e9gions en 2019",
      fill = "",
      caption = "Source: Rosstat (2020)"
    )
}

#' Taux de mortalité en Russie
#'
#' Une carte du taux de mortalité en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#' @param barwidth Width of legend bar
#'
#' @return A ggplot2 map
#' @concept oc russie mortalité
#'
#' @export
#' @examples
#' oc_russie_carte_mortalite()
oc_russie_carte_mortalite <- function(theme = ggplot2::theme_minimal(), barwidth = 40) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_2019_natalite_mortalite |>
        dplyr::filter(indicator == "cdr", type == "per1000"),
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, option = "inferno", direction = -1) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barwidth = barwidth
      )
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Taux de mortalit\u00e9",
      subtitle = "par r\u00e9gions en 2019",
      fill = "",
      caption = "Source: Rosstat (2020)"
    )
}

#' Accroissement naturel en Russie
#'
#' Une carte de l'accroissement naturel en 2023
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#' @param barwidth Width of legend bar
#' @param greyscale Boolean: whether to create a grey scale map
#'
#' @return A map
#' @concept oc russie mortalité
#'
#' @export
#' @examples
#' oc_russie_carte_accroissement()
#' oc_russie_carte_hc_accroissement()
oc_russie_carte_accroissement <- function(theme = ggplot2::theme_minimal(), barwidth = 40, greyscale = FALSE) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_2023_accroissement |>
        dplyr::filter(year == 2023),
      by = "adm1_code"
    ) |>
    dplyr::mutate(data_cut = santoku::chop(rni, c(-5, 0, 5, 10, 15))) -> data_plot

  if (greyscale) {
    fill_scale <- ggplot2::scale_fill_manual(
      values = paletteer::paletteer_c("pals::kovesi.linear_grey_10_95_c0", n = 6, direction = -1),
      breaks = levels(data_plot$data_cut),
      limits = levels(data_plot$data_cut),
      labels = scales::percent_format(scale = 1, accuracy = 1, suffix = "\u2030"),
      na.value = "#CCCCCC"
    )
  } else {
    fill_scale <- ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10)[4:9],
        alpha = .8
      ),
      breaks = levels(data_plot$data_cut),
      limits = levels(data_plot$data_cut),
      labels = scales::percent_format(scale = 1, accuracy = 1, suffix = "\u2030"),
      na.value = "#CCCCCC"
    )
  }

  data_plot |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data_cut),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    fill_scale +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barwidth = barwidth
      )
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Taux d'accroissement naturel",
      subtitle = "par r\u00e9gions en 2023",
      fill = "",
      caption = "Source: Rosstat (2024)"
    )
}

#' @rdname oc_russie_carte_accroissement
#' @export
oc_russie_carte_hc_accroissement <- function() {
  geodata::oc_russie_2023_accroissement |>
    dplyr::filter(year == 2023) |>
    dplyr::rename(value = rni) -> data

  palette_accroissement <- function(...) {
    paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10)[4:9]
  }

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") -> russia

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Accroissement naturel",
      mapData = russia |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      borderColor = "transparent",
      borderWidth = 0.1,
      joinBy = "adm1_code",
      value = "value",
      nullColor = "#838a8f",
      data = data
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "Orthographic",
        rotation = c(-90, -80)
      )
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(
          data$value,
          c(-5, 0, 5, 10, 15)
        ), palette_accroissement
      ),
      showInLegend = TRUE
    )
}

#' Évolution démographique en Russie
#'
#' Une carte de l'évolution démographique Russe
#' entre 1990 et 2020.
#'
#' @param theme A ggplot2 theme
#' @param barwidth Width of legend bar
#'
#' @return A ggplot2 map
#' @export
#' @examples
#' oc_russie_carte_evolution_population()
oc_russie_carte_evolution_population <- function(theme = ggplot2::theme_minimal(), barwidth = 40) {
  data <- geodata::oc_russie_2020_evolution_population |>
    dplyr::mutate(solde_cut = santoku::chop(solde, c(-250, -40, -20, -10, 0, 10, 20, 40, 80)))

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      data,
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = solde_cut),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10)[2:9],
        alpha = .8
      ),
      breaks = levels(data$solde_cut),
      limits = levels(data$solde_cut),
      labels = scales::percent_format(scale = 1, accuracy = 1),
      na.value = "#CCCCCC"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = FALSE,
        barwidth = barwidth
      )
    ) +
    ggplot2::labs(
      title = "\u00c9volution de la population russe",
      subtitle = "de 1990 \u00e0 2020, par r%u009gions",
      fill = "",
      caption = "Source: Rosstat (2020)"
    )
}

#' Taux de mariage en Russie
#'
#' Une carte des mariages pour 1000 habitants
#' par régions en 2019.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
#' @examples
#' oc_russie_carte_mariages()
oc_russie_carte_mariages <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_mariages |> dplyr::filter(year == 2019),
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = mariages),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, direction = -1) +
    ggplot2::labs(
      title = "Mariages",
      subtitle = "par r\u00e9gions en 2019 pour 1000 habitants",
      fill = "",
      caption = "Source: Rosstat (2020)"
    )
}

#' Taux de divorce en Russie
#'
#' Une carte des divorces pour 1000 habitants
#' par régions en 2019.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
#' @examples
#' oc_russie_carte_divorces()
oc_russie_carte_divorces <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") |>
    dplyr::left_join(
      geodata::oc_russie_divorces |> dplyr::filter(year == 2019),
      by = "adm1_code"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = divorces),
      color = "black", linewidth = .1
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, direction = -1, option = "inferno") +
    ggplot2::labs(
      title = "Divorces",
      subtitle = "par r\u00e9gions en 2019 pour 1000 habitants",
      fill = "",
      caption = "Source: Rosstat (2020)"
    )
}

#' Carte de la religion majoritaire en 2012
#'
#' Religion avec le plus de pourcentage en 2012
#' par région.
#'
#' @returns A higcharts map
#' @export
#'
#' @examples
#' oc_russie_carte_hc_religion()
oc_russie_carte_hc_religion <- function() {
  geodata::oc_russie_2012_religion |>
    dplyr::select(-region) |>
    dplyr::group_by(adm1_code) |>
    dplyr::arrange(-percent) |>
    dplyr::summarise(
      religion = dplyr::first(religion),
      percent = dplyr::first(percent)
    ) |>
    dplyr::mutate(value = round(percent, 2)) -> data

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") -> russia

  russia |>
    dplyr::left_join(data, by = "adm1_code") |>
    dplyr::filter(religion == "Orthodoxe") |>
    geojsonio::geojson_json() -> map_orthodoxe

  russia |>
    dplyr::left_join(data, by = "adm1_code") |>
    dplyr::filter(religion == "Islam") |>
    geojsonio::geojson_json() -> map_islam

  russia |>
    dplyr::left_join(data, by = "adm1_code") |>
    dplyr::filter(religion == "Deiste") |>
    geojsonio::geojson_json() -> map_deiste

  russia |>
    dplyr::left_join(data, by = "adm1_code") |>
    dplyr::filter(religion == "Bouddhisme") |>
    geojsonio::geojson_json() -> map_bouddhisme

  russia |>
    dplyr::left_join(data, by = "adm1_code") |>
    dplyr::filter(religion == "Athee") |>
    geojsonio::geojson_json() -> map_athee

  palette_orthodoxe <- function(...) {
    paletteer::paletteer_c("ggthemes::Blue", ...)
  }

  palette_islam <- function(...) {
    paletteer::paletteer_c("ggthemes::Green", ...)
  }

  palette_deiste <- function(...) {
    paletteer::paletteer_c("ggthemes::Purple", ...)
  }

  palette_bouddhisme <- function(...) {
    paletteer::paletteer_c("ggthemes::Orange", ...)
  }

  palette_athee <- function(...) {
    paletteer::paletteer_c("ggthemes::Gray", ...)
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = russia |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      borderColor = "transparent",
      borderWidth = 0.1,
      data = c()
    ) |>
    highcharter::hc_add_series(
      mapData = map_orthodoxe,
      data = data |> dplyr::filter(religion == "Orthodoxe"),
      joinBy = "adm1_code",
      name = "Orthodoxe",
      color = "#699cb6",
      borderColor = "transparent",
      borderWidth = 0.1,
      value = "value"
    ) |>
    highcharter::hc_add_series(
      mapData = map_islam,
      data = data |> dplyr::filter(religion == "Islam"),
      joinBy = "adm1_code",
      name = "Islam",
      color = "#729e93",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 1
    ) |>
    highcharter::hc_add_series(
      mapData = map_deiste,
      data = data |> dplyr::filter(religion == "Deiste"),
      joinBy = "adm1_code",
      name = "D\u00e9iste",
      color = "#475286",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 2
    ) |>
    highcharter::hc_add_series(
      mapData = map_bouddhisme,
      data = data |> dplyr::filter(religion == "Bouddhisme"),
      joinBy = "adm1_code",
      name = "Bouddhisme",
      color = "#fae3a0",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 3
    ) |>
    highcharter::hc_add_series(
      mapData = map_athee,
      data = data |> dplyr::filter(religion == "Athee"),
      joinBy = "adm1_code",
      name = "Ath\u00e9e",
      color = "#a59da2",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 4
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "Orthographic",
        rotation = c(-90, -80)
      )
    ) |>
    highcharter::hc_colorAxis(
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(religion == "Orthodoxe") |>
              dplyr::pull(value),
            c(25, 35, 45, 55, 65, 75, 85)
          ), palette_orthodoxe
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(religion == "Islam") |>
              dplyr::pull(value),
            c(25, 35, 45, 55, 65, 75, 85)
          ), palette_islam
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(religion == "Deiste") |>
              dplyr::pull(value),
            c(25, 35, 45, 55, 65, 75, 85)
          ), palette_deiste
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(religion == "Bouddhisme") |>
              dplyr::pull(value),
            c(25, 35, 45, 55, 65, 75, 85)
          ), palette_bouddhisme
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            data |>
              dplyr::filter(religion == "Athee") |>
              dplyr::pull(value),
            c(25, 35, 45, 55, 65, 75, 85)
          ), palette_athee
        ),
        showInLegend = FALSE
      )
    ) |>
    highcharter::hc_caption(
      text = "Source : <a href='https://sreda.org/arena' target='_blank'>SREDA (2012)</a>"
    )
}

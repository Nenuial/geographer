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


# Géopolitique -----------------------------------------------------------

#' Adhésion à l'OTAN
#'
#' Carte de l'adhésion à l'OTAN par années
#'
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map
#' @export
#'
#' @examples
#' oc_russie_carte_adhesion_otan()
#' oc_russie_carte_hc_adhesion_otan()
oc_russie_carte_adhesion_otan <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_nato_membership() |>
    dplyr::mutate(
      gwcode = countrycode::countrycode(
        iso3,
        origin = "iso3c", destination = "cown",
        custom_match = c("DDR" = 265, "DEU" = 260)
      )
    ) |>
    dplyr::mutate(
      gwcode = as.integer(gwcode)
    ) |>
    dplyr::mutate(
      pays = countrycode::countrycode(
        iso3,
        origin = "iso3c", destination = "country.name.fr",
        custom_match = c(
          "DDR" = "Allemagne de l'Est",
          "DEU" = "Allemagne"
        )
      )
    ) -> otan

  cshapes::cshp(date = lubridate::ymd("2019-12-01")) |>
    dplyr::left_join(
      otan,
      by = "gwcode"
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 3035) |>
    dplyr::mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) |>
    dplyr::filter(!(gwcode %in% c(260, 265))) -> map_data_current

  cshapes::cshp(date = lubridate::ymd("1989-12-01")) |>
    dplyr::left_join(
      otan,
      by = "gwcode"
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 3035) |>
    dplyr::mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) |>
    dplyr::filter(gwcode %in% c(260, 265)) -> map_data_germany

  map_data_current |>
    dplyr::bind_rows(map_data_germany) -> map_data

  map_data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(fill = extension),
      color = "black", linewidth = .1
    ) +
    ggplot2::geom_sf(
      data = map_data |> dplyr::filter(gwcode == 365),
      fill = "#c03728",
      color = "black",
      linewidth = .1
    ) +
    ggrepel::geom_label_repel(
      data = map_data |> dplyr::filter(gwcode == 365),
      x = 6500000,
      y = 4500000,
      label = "Russie",
      fill = "#c03728",
      color = "#FFF",
      size = 3,
      force = 2,
      min.segment.length = 0,
      point.padding = NA,
      show.legend = FALSE
    ) +
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(
        x = COORDS_X,
        y = COORDS_Y,
        label = pays,
        fill = extension,
        color = extension
      ),
      size = 3,
      force = 2,
      min.segment.length = 0,
      point.padding = NA,
      show.legend = FALSE
    ) +
    ggplot2::coord_sf(
      xlim = c(2700000, 7000000),
      ylim = c(1500000, 5500000),
      datum = NA
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_manual(
      values = paletteer::paletteer_c(
        "pals::ocean.deep", 12
      ),
      breaks = ~ .x[!is.na(.x)],
      na.value = "#CCCCCC"
    ) +
    ggplot2::scale_color_manual(
      values = c(
        rep.int("#FFF", 6),
        rep.int("#000", 6)
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrows = 4, ncol = 3,
      )
    ) +
    ggplot2::labs(
      title = "Extension de l'OTAN",
      fill = "",
      x = "", y = "",
      caption = "Source: OTAN (2024)"
    )
}

#' @rdname oc_russie_carte_adhesion_otan
#' @param layout Layout of the legend, either 'horizontal', 'vertical' or 'proximate'
#' @param zoom Map zoom (defaults to 3.5)
#' @export
oc_russie_carte_hc_adhesion_otan <- function(layout = "proximate", zoom = 3.5) {
  palette <- function(...) {
    paletteer::paletteer_c("pals::ocean.deep", 12) -> colors

    return(colors)
  }

  geodata::gdt_nato_membership() |>
    dplyr::mutate(
      gwcode = countrycode::countrycode(
        iso3,
        origin = "iso3c", destination = "cown",
        custom_match = c("DDR" = 265, "DEU" = 260)
      )
    ) |>
    dplyr::mutate(
      gwcode = as.integer(gwcode)
    ) |>
    dplyr::mutate(
      pays = countrycode::countrycode(
        iso3,
        origin = "iso3c", destination = "country.name.fr",
        custom_match = c(
          "DDR" = "Allemagne de l'Est",
          "DEU" = "Allemagne"
        )
      )
    ) -> otan

  cshapes::cshp(date = lubridate::ymd("2019-12-01")) |>
    dplyr::left_join(
      otan,
      by = "gwcode"
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) |>
    dplyr::filter(!(gwcode %in% c(260, 265))) -> map_data_current

  cshapes::cshp(date = lubridate::ymd("1989-12-01")) |>
    dplyr::left_join(
      otan,
      by = "gwcode"
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      CENTROID = purrr::map(geometry, sf::st_centroid),
      COORDS = purrr::map(CENTROID, sf::st_coordinates),
      COORDS_X = purrr::map_dbl(COORDS, 1),
      COORDS_Y = purrr::map_dbl(COORDS, 2)
    ) |>
    dplyr::filter(gwcode %in% c(260, 265)) -> map_data_germany

  map_data_current |>
    dplyr::bind_rows(map_data_germany) |>
    dplyr::mutate(
      continent = countrycode::countrycode(
        gwcode,
        origin = "cown", destination = "continent",
        custom_match = c(
          "260" = "Europe",
          "265" = "Europe",
          "340" = "Europe",
          "347" = "Europe",
          "640" = "Europe"
        )
      )
    ) |>
    dplyr::filter(continent == "Europe") -> map_data

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = map_data |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      data = c(),
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(gwcode == 365) |>
        geojsonio::geojson_json(),
      data = map_data |>
        dplyr::filter(gwcode == 365) |>
        dplyr::mutate(value = 99),
      joinBy = "gwcode",
      name = "Russie",
      color = "#c03728",
      borderColor = "transparent",
      borderWidth = 0.1,
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(gwcode != 365) |>
        dplyr::select(gwcode, geometry) |>
        geojsonio::geojson_json(),
      data = map_data |>
        dplyr::mutate(value = as.integer(extension)),
      joinBy = "gwcode",
      name = "OTAN",
      value = "value",
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_discrete_color_axis(
        map_data$extension,
        palette
      ),
      showInLegend = TRUE
    ) |>
    highcharter::hc_mapView(
      projection = list(
        name = "Orthographic",
        rotation = c(0, -20)
      ),
      zoom = zoom
    ) |>
    highcharter::hc_legend(
      layout = layout,
      align = "right"
    ) |>
    highcharter::hc_caption(
      text = "Source: OTAN (2024)"
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.pays + '</b>: ' +
          this.point.extension }}"))
    )
}


# Ukraine ----------------------------------------------------------------

#' Cartes des élections présidentielles ukrainiennes
#'
#' Cartes des élections présidentielles ukrainiennes
#' en 2010, 2014 et 2019.
#'
#' @name oc_russie_carte_hc_elections_ukraine
#'
#' @returns A higcharts map
#' @export
#'
#' @examples
#' oc_russie_carte_hc_elections_ukraine_2010()
#' oc_russie_carte_hc_elections_ukraine_2014()
#' oc_russie_carte_hc_elections_ukraine_2019()
oc_russie_carte_hc_elections_ukraine_2010 <- function() {
  geodata::oc_russie_2010_elections_ukraine |>
    dplyr::rename(
      adm1_code = CODE,
      value = PERCENT
    ) -> vote_data

  palette_timo <- function(...) {
    paletteer::paletteer_c("ggthemes::Purple", ...)
  }

  palette_yanu <- function(...) {
    paletteer::paletteer_c("ggthemes::Blue-Teal", ...)
  }

  rnaturalearth::ne_states(
    "Ukraine",
    returnclass = "sf"
  ) |>
    dplyr::left_join(vote_data, by = "adm1_code") |>
    sf::st_transform(crs = 3035) -> map_data

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = map_data |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      data = c(),
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(CANDIDATE == "Y. Tymoshenko") |>
        geojsonio::geojson_json(),
      data = vote_data |> dplyr::filter(CANDIDATE == "Y. Tymoshenko"),
      joinBy = "adm1_code",
      name = "Tymoshenko",
      color = "#ff009d",
      value = "value",
      nullColor = "transparent",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 0
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(CANDIDATE == "V. Yanukovych") |>
        geojsonio::geojson_json(),
      data = vote_data |> dplyr::filter(CANDIDATE == "V. Yanukovych"),
      joinBy = "adm1_code",
      name = "Yanukovych",
      color = "#0099ff",
      value = "value",
      nullColor = "transparent",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 1
    ) |>
    highcharter::hc_colorAxis(
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            vote_data |>
              dplyr::filter(CANDIDATE == "Y. Tymoshenko") |>
              dplyr::pull(value),
            c(50, 60, 70, 80, 90, 100)
          ), palette_timo
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            vote_data |>
              dplyr::filter(CANDIDATE == "V. Yanukovych") |>
              dplyr::pull(value),
            c(50, 60, 70, 80, 90, 100)
          ), palette_yanu
        ),
        showInLegend = FALSE
      )
    )
}

#' @rdname oc_russie_carte_hc_elections_ukraine
#' @export
oc_russie_carte_hc_elections_ukraine_2014 <- function() {
  geodata::oc_russie_2014_elections_ukraine |>
    dplyr::rename(
      adm1_code = CODE,
      value = PERCENT
    ) -> vote_data

  palette_poro <- function(...) {
    paletteer::paletteer_c("ggthemes::Red", ...)
  }

  rnaturalearth::ne_states(
    "Ukraine",
    returnclass = "sf"
  ) |>
    sf::st_transform(crs = 3035) -> map_data

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = map_data |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      data = c(),
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |> geojsonio::geojson_json(),
      data = vote_data,
      joinBy = "adm1_code",
      name = "Poroshenko",
      color = "#ff3131",
      value = "value",
      nullColor = "transparent",
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(
          vote_data |>
            dplyr::pull(value),
          c(30, 40, 50, 60, 70, 80, 90, 100)
        ), palette_poro
      ),
      showInLegend = FALSE
    )
}

#' @rdname oc_russie_carte_hc_elections_ukraine
#' @export
oc_russie_carte_hc_elections_ukraine_2019 <- function() {
  geodata::oc_russie_2019_elections_ukraine |>
    dplyr::rename(
      adm1_code = CODE,
      value = PERCENT
    ) -> vote_data


  palette_poro <- function(...) {
    paletteer::paletteer_c("ggthemes::Red", ...)
  }

  palette_zele <- function(...) {
    paletteer::paletteer_c("ggthemes::Green", ...)
  }

  rnaturalearth::ne_states(
    "Ukraine",
    returnclass = "sf"
  ) |>
    sf::st_transform(crs = 3035) |>
    dplyr::left_join(vote_data, by = "adm1_code") -> map_data

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Background",
      mapData = map_data |> geojsonio::geojson_json(),
      showInLegend = FALSE,
      data = c(),
      borderColor = "transparent",
      borderWidth = 0.1
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(CANDIDATE == "V. Zelensky") |>
        geojsonio::geojson_json(),
      data = vote_data |> dplyr::filter(CANDIDATE == "V. Zelensky"),
      joinBy = "adm1_code",
      name = "Zelensky",
      color = "#00a105",
      value = "value",
      nullColor = "transparent",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 0
    ) |>
    highcharter::hc_add_series(
      mapData = map_data |>
        dplyr::filter(CANDIDATE == "P. Poroshenko") |>
        geojsonio::geojson_json(),
      data = vote_data |> dplyr::filter(CANDIDATE == "P. Poroshenko"),
      joinBy = "adm1_code",
      name = "Poroshenko",
      color = "#ff3131",
      value = "value",
      nullColor = "transparent",
      borderColor = "transparent",
      borderWidth = 0.1,
      colorAxis = 1
    ) |>
    highcharter::hc_colorAxis(
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            vote_data |>
              dplyr::filter(CANDIDATE == "V. Zelensky") |>
              dplyr::pull(value),
            c(50, 60, 70, 80, 90, 100)
          ), palette_zele
        ),
        showInLegend = FALSE
      ),
      list(
        dataClasses = geotools::gtl_hc_color_axis(
          santoku::chop(
            vote_data |>
              dplyr::filter(CANDIDATE == "P. Poroshenko") |>
              dplyr::pull(value),
            c(50, 60, 70, 80, 90, 100)
          ), palette_poro
        ),
        showInLegend = FALSE
      )
    )
}

#' OC Russie: carte de l'indice de fécondité
#'
#' Une carte de l'indice de fécondité en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc russie fertilité
#'
#' @export
oc_russie_carte_fecondite <- function(theme = ggplot2::theme_minimal(), barwidth = 40, greyscale = F) {
  if(greyscale) {
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

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_fecondite %>%
        dplyr::filter(year == 2019),
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = tfr),
                     color = "black", size = .1) +
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
      title = "Indice de fécondité",
      subtitle = "par régions en 2019",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte du taux de natalité
#'
#' Une carte du taux de natalité en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc russie fertilité
#'
#' @export
oc_russie_carte_natalite <- function(theme = ggplot2::theme_minimal(), barwidth = 40) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_natalite_mortalite %>%
        dplyr::filter(indicator == "cbr", type == "per1000"),
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data),
                     color = "black", size = .1) +
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
      title = "Taux de natalité",
      subtitle = "par régions en 2019",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte du taux de mortalité
#'
#' Une carte du taux de mortalité en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc russie mortalité
#'
#' @export
oc_russie_carte_mortalite <- function(theme = ggplot2::theme_minimal(), barwidth = 40) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_natalite_mortalite %>%
        dplyr::filter(indicator == "cdr", type == "per1000"),
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data),
                     color = "black", size = .1) +
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
      title = "Taux de mortalité",
      subtitle = "par régions en 2019",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte de l'accroissement naturel
#'
#' Une carte de l'accroissement naturel en 2019
#' au niveau régional en Russie.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc russie mortalité
#'
#' @export
oc_russie_carte_accroissement <- function(theme = ggplot2::theme_minimal(), barwidth = 40, greyscale = F) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_natalite_mortalite %>%
        dplyr::filter(indicator == "rni", type == "per1000"),
      by = "adm1_code"
    ) %>%
    dplyr::mutate(data_cut = santoku::chop(data, c(-5, 0, 5, 10, 15))) -> data_plot

  if(greyscale) {
    fill_scale <- ggplot2::scale_fill_manual(
      values = paletteer::paletteer_c("pals::kovesi.linear_grey_10_95_c0", n = 6, direction = -1),
      breaks = levels(data_plot$data_cut),
      limits = levels(data_plot$data_cut),
      labels = scales::percent_format(scale = 1, accuracy = 1, suffix = "‰"),
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
      labels = scales::percent_format(scale = 1, accuracy = 1, suffix = "‰"),
      na.value = "#CCCCCC"
    )
  }

  data_plot %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = data_cut),
                     color = "black", size = .1) +
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
      subtitle = "par régions en 2019",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte de l'évolution démographique
#'
#' Une carte de l'évolution démographique Russe
#' entre 1990 et 2020.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_russie_carte_evolution_population <- function(theme = ggplot2::theme_minimal()) {
  data <- geodata::oc_russie_2020_evolution_population %>%
    dplyr::mutate(solde_cut = santoku::chop(solde, c(-250, -40, -20, -10, 0, 10, 20, 40, 80)))

  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      data,
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = solde_cut),
                     color = "black", size = .1) +
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
      title = "Évolution de la population russe",
      subtitle = "de 1990 à 2020, par régions",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte du taux de mariage
#'
#' Une carte des mariages pour 1000 habitants
#' par régions en 2019.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_russie_carte_mariages <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_mariages,
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = mariages),
                     color = "black", size = .1) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, direction = -1) +
    ggplot2::labs(
      title = "Mariages",
      subtitle = "par régions en 2019 pour 1000 habitants",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

#' OC Russie: carte du taux de divorce
#'
#' Une carte des divorces pour 1000 habitants
#' par régions en 2019.
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_russie_carte_mariages <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_states(country = "Russia", returnclass = "sf") %>%
    dplyr::left_join(
      geodata::oc_russie_2019_divorces,
      by = "adm1_code"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = divorces),
                     color = "black", size = .1) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_regional("Russia"), datum = NA) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_viridis_b(n.breaks = 7, direction = -1, option = "inferno") +
    ggplot2::labs(
      title = "Divorces",
      subtitle = "par régions en 2019 pour 1000 habitants",
      fill = "",
      caption = "Données: Rosstat (2020)"
    )
}

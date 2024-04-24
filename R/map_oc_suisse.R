#' OC Suisse: carte du suffrage féminin
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
oc_suisse_carte_suffrage_feminin <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_suffrage_feminin %>%
    dplyr::mutate(id = geographer::geo_swiss_canton_id(Canton)) %>%
    dplyr::mutate(Annee = forcats::as_factor(Annee)) %>%
    dplyr::left_join(
      themakart::thema_map("inst", "kant"),
      by = "id"
    ) -> plot_data

  plot_data  %>%
    ggplot2::ggplot() +
    geo_map_swiss_relief() +
    ggplot2::geom_sf(ggplot2::aes(fill = Annee, geometry = geometry),
                     color = "white", size = 0.1) +
    geo_map_swiss_lakes() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_d("ggthemes::Classic_Cyclic",  11),
        alpha = .8
      ),
      breaks = levels(plot_data$Annee),
      limits = levels(plot_data$Annee)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = T)
    ) +
    ggplot2::labs(
      title = "Introduction du suffrage féminin en Suisse",
      subtitle = "par cantons",
      x = "",
      y = "",
      fill = "",
      caption = "Fond: OFS ThemaKart (2020), Données: Wikipédia"
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom")
}

#' OC Suisse: carte du vote sur le suffrage féminin au valais
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
"oc_suisse_1970_carte_suffrage_feminin_valais"
oc_suisse_1970_carte_suffrage_feminin_valais <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_1970_04_12_suffrage_feminin_vs %>%
    dplyr::mutate(per_oui = vote_oui / (vote_oui + vote_non) * 100) %>%
    dplyr::mutate(per_oui = santoku::chop(per_oui, c(0,70,72,74,76,78,100)))%>%
    dplyr::left_join(
      themakart::thema_map("inst", "bezk", 2000),
      by = "id"
    ) -> plot_data

  plot_data  %>%
    ggplot2::ggplot() +
    geo_map_swiss_relief() +
    ggplot2::geom_sf(ggplot2::aes(fill = per_oui, geometry = geometry),
                     color = "white", size = 0.1) +
    geo_map_swiss_lakes() +
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
      title = "Votation sur le suffrage féminin au Valais",
      subtitle = "Dimanche 12 avril 1970, par districts",
      fill = "",
      x = "",
      y = "",
      caption = "Fond: OFS ThemaKart (2020), Données: Gazette de Lausanne (1970)"
    ) +
    theme
}

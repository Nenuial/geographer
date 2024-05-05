#' Plot latest swiss votation
#'
#' @param geolevel One of "canton", "district" or "municipality"
#' @param votedates The date of the vote
#' @param language One of "DE", "FR", "IT" or "RM"
#' @param id The vote id
#'
#' @return A ggplot2 map
#' @export
#' @examples
#' gph_map_swiss_votes("canton", votedates = "2024-03-03", id = 6650)
gph_map_swiss_votes <- function(geolevel = c("canton", "district", "municipality"), votedates, id, language = "FR") {
  # Check input
  geolevel <- match.arg(geolevel)

  # Retrieve geodata
  geo_data <- swissdd::get_geodata(geolevel)
  plot_data <- gph_swiss_votes_data(
    geolevel = geolevel,
    votedates = votedates,
    filter_id = id,
    language = language
  )

  geo_data |>
    dplyr::left_join(plot_data$vote_data, by = plot_data$join_id) |>
    dplyr::mutate(value = santoku::chop(jaStimmenInProzent, c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))) |>
    ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = themakart::thema_relief() |> dplyr::mutate(x = x - 2000000, y = y - 1000000),
      ggplot2::aes_string(
        x = "x", y = "y",
        fill = NULL,
        alpha = "value"
      )
    ) +
    ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none") +
    ggplot2::geom_sf(ggplot2::aes(fill = value), color = "white", linewidth = .1) +
    gph_map_swiss_lakes(source = swissdd::get_geodata("lakes")) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10),
        alpha = .8
      ),
      breaks = c(
        "[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
        "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)"
      ),
      limits = c(
        "[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
        "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)"
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, title.position = "top",
        barwidth = ggplot2::unit(150, units = "mm"),
        keyheight = ggplot2::unit(5, units = "mm")
      )
    ) +
    ggplot2::labs(
      title = plot_data$pretty_title,
      subtitle = plot_data$pretty_subtitle,
      fill = "Pourcentage de oui", x = "", y = "",
      caption = "Donn\u00e9es : OFS, Fond de carte : Themakart"
    ) +
    ggeo::ggeotheme(
      theme = geotools::gtl_options("theme"),
      mode = geotools::gtl_options("mode"),
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 0, unit = "cm")),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 0, unit = "cm")),
      plot.title.position = "plot", legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 1, r = 1.5, unit = "cm")
    )
}

#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
gph_highcharter_map_swiss_votes <- function(geolevel = c("canton", "district", "municipality"),
                                            votedates, id, language = "FR") {
  lifecycle::deprecate_warn("1.0.0", "gph_highcharter_map_swiss_votes()", "gph_hc_demogram()")
  gph_hc_map_swiss_votes(geolevel, votedates, id, language)
}

#' Plot latest swiss votation using highcharts
#'
#' @inherit gph_map_swiss_votes
#'
#' @return A highcharter map
#' @export
#' @examples
#' gph_hc_map_swiss_votes("canton", votedates = "2024-03-03", id = 6650)
gph_hc_map_swiss_votes <- function(geolevel = c("canton", "district", "municipality"), votedates, id, language = "FR") {
  # Check input
  geolevel <- match.arg(geolevel)

  geo_data <- geodata::gdt_opendata_swiss_geodata_json(geolevel)
  plot_data <- gph_swiss_votes_data(
    geolevel = geolevel,
    votedates = votedates,
    filter_id = id,
    language = language
  )

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = plot_data$pretty_title) |>
    highcharter::hc_subtitle(text = plot_data$pretty_subtitle) |>
    highcharter::hc_add_series(
      data = plot_data$vote_data,
      name = "Votation",
      value = "value",
      mapData = geo_data,
      borderWidth = .5,
      borderColor = "white",
      joinBy = plot_data$join_id,
      showInLegend = FALSE
    ) |>
    highcharter::hc_add_series(
      mapData = geodata::gdt_opendata_swiss_geodata_json("lakes"),
      borderWidth = .5,
      borderColor = "white",
      negativeColor = "lightblue",
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.{plot_data$name_field} + '</b><br/>' +
          'Oui (%): ' + this.point.value }}"))
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = list(
        list(color = "#340D35", from = 0, to = 10),
        list(color = "#7B1C5D", from = 10, to = 20),
        list(color = "#B64A60", from = 20, to = 30),
        list(color = "#DA8C78", from = 30, to = 40),
        list(color = "#EED2C4", from = 40, to = 50),
        list(color = "#D6DCCB", from = 50, to = 60),
        list(color = "#7BB291", from = 60, to = 70),
        list(color = "#1F867B", from = 70, to = 80),
        list(color = "#1B5163", from = 80, to = 90),
        list(color = "#151D44", from = 90, to = 100)
      ),
      showInLegend = FALSE
    )
}

#' Retrieve swiss vote data
#'
#' @inherit gph_map_swiss_votes
#'
#' @keywords internal
gph_swiss_votes_data <- function(geolevel = c("canton", "district", "municipality"),
                                 votedates, filter_id, language = "FR") {
  vote_data <- swissdd::get_nationalvotes(geolevel = geolevel, votedates = votedates, language = language) |>
    dplyr::filter(id == filter_id) |>
    dplyr::mutate(value = round(jaStimmenInProzent, 2))

  pretty_date <- withr::with_locale(
    new = c("LC_TIME" = "fr_CH.UTF-8"),
    format(as.Date(vote_data[[1, "votedate"]]), "%d %B %Y")
  )
  pretty_title <- glue::glue("Votation du {pretty_date}")
  pretty_subtitle <- vote_data[[1, "name"]]

  join_id <- dplyr::case_when(
    geolevel == "canton" ~ "canton_id",
    geolevel == "district" ~ "district_id",
    geolevel == "municipality" ~ "mun_id"
  )

  name_field <- dplyr::case_when(
    geolevel == "canton" ~ "canton_name",
    geolevel == "district" ~ "district_name",
    geolevel == "municipality" ~ "mun_name"
  )

  return(list(
    vote_data = vote_data,
    pretty_date = pretty_date,
    pretty_title = pretty_title,
    pretty_subtitle = pretty_subtitle,
    join_id = join_id,
    name_field = name_field
  ))
}

#' Swiss relief background for maps
#'
#' @return A list of ggplot 2 layers for Swiss relief backgroup
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   gph_map_swiss_relief() +
#'   gph_map_swiss_lakes()
gph_map_swiss_relief <- function() {
  list(
    ggplot2::geom_sf(
      data = themakart::thema_map("inst", "suis", 1848, "gf"),
      fill = "white", linewidth = .1
    ),
    ggplot2::geom_raster(
      data = themakart::thema_relief(),
      ggplot2::aes_string(
        x = "x", y = "y",
        fill = NULL,
        alpha = "value"
      )
    ),
    ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none")
  )
}

#' Swiss lakes for maps
#'
#' @param fill_color A color for the lakes (default: skyblue)
#' @param source The geometry for lakes (default: ThemaKart lakes)
#'
#' @return A ggplot2 layer for Swiss lakes
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   gph_map_swiss_relief() +
#'   gph_map_swiss_lakes()
gph_map_swiss_lakes <- function(fill_color = "skyblue", source = themakart::thema_topo("seen")) {
  ggplot2::geom_sf(data = source, fill = fill_color, color = NA)
}

#' Plot latest swiss votation
#'
#' @param geolevel One of "canton", "district" or "municipality"
#' @param votedates The date of the vote
#' @param language One of "DE", "FR", "IT" or "RM"
#' @param id The vote id
#'
#' @return A ggplot2 map
#' @export
gph_map_swiss_votes <- function(geolevel = c("canton", "district", "municipality"), votedates, id, language = "FR") {
  # Check input
  geolevel <- match.arg(geolevel)

  # Retrieve geodata
  geo_data <- swissdd::get_geodata(geolevel)
  plot_data <- gph_swiss_votes_data(geolevel, votedates, id, language)

  geo_data |>
    dplyr::left_join(plot_data$vote_data, by = plot_data$join_id) |>
    dplyr::mutate(value = santoku::chop(jaStimmenInProzent, c(0,10,20,30,40,50,60,70,80,90,100))) |>
    ggplot2::ggplot() +
    ggplot2::geom_raster(data = themakart::thema_relief() |> dplyr::mutate(x = x - 2000000, y = y - 1000000),
                         ggplot2::aes_string(x = "x", y = "y",
                                             fill = NULL,
                                             alpha = "value")) +
    ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none")+
    ggplot2::geom_sf(ggplot2::aes(fill = value), color = "white", size = .1) +
    geo_map_swiss_lakes(source = swissdd::get_geodata("lakes")) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10),
        alpha = .8
      ),
      breaks = c("[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
                 "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)"),
      limits = c("[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
                 "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)")
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
      caption = "Données : OFS, Fond de carte : Themakart"
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

#' Plot latest swiss votation using highcharts
#'
#' @inherit gph_map_swiss_votes
#'
#' @return A highcharter map
#' @export
gph_highcharter_map_swiss_votes <- function(geolevel = c("canton", "district", "municipality"), votedates, id, language = "FR") {
  # Check input
  geolevel <- match.arg(geolevel)

  geo_data <- geodata::gdt_opendata_swiss_geodata_json(geolevel)
  plot_data <- gph_swiss_votes_data(geolevel, votedates, id, language)

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
      showInLegend= F
    ) |>
    highcharter::hc_add_series(
      mapData = geodata::gdt_opendata_swiss_geodata_json("lakes"),
      borderWidth = .5,
      borderColor = "white",
      negativeColor = "lightblue",
      showInLegend = F
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
      showInLegend = F
    )
}

#' Retrieve swiss vote data
#'
#' @inherit gph_map_swiss_votes
#'
#' @keywords internal
gph_swiss_votes_data <- function(geolevel = c("canton", "district", "municipality"), votedates, id, language = "FR") {
  vote_data <- swissdd::get_nationalvotes(geolevel = geolevel, votedates = votedates, language = language) |>
    dplyr::filter(id == {{ id }}) |>
    dplyr::mutate(value = round(jaStimmenInProzent,2))

  pretty_date <- withr::with_locale(
    new = c("LC_TIME" = "fr_CH"),
    format(as.Date(vote_data[[1, "votedate"]]), "%d %B %Y")
  )
  pretty_title <- glue::glue('Votation du {pretty_date}')
  pretty_subtitle <- vote_data[[1, "name"]]

  join_id <- dplyr::case_when(
    geolevel == "canton"       ~ "canton_id",
    geolevel == "district"     ~ "district_id",
    geolevel == "municipality" ~ "mun_id"
  )

  name_field <- dplyr::case_when(
    geolevel == "canton"       ~ "canton_name",
    geolevel == "district"     ~ "district_name",
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


#' Write voting maps for communal and cantonal level to path
#'
#' @param url The url where the data comes from
#' @param path A path where to write the maps
#'
#' @export
geo_map_swiss_voting <- function(url) {
  url %>%
    jsonlite::fromJSON() -> voting_data

  voting_data %>%
    purrr::pluck("abstimmtag") %>%
    lubridate::ymd() -> voting_date

  voting_data %>%
    tibble::as_tibble() %>%
    geo_write_voting_charts()

  voting_data %>%
    purrr::pluck("schweiz", "vorlagen") %>%
    as.list() %>%
    purrr::walk(~geo_map_swiss_voting_walk(.x, date = voting_date))
}

#' Internal walk voting objects
#'
#' @param data The data of the voting object
#' @param date The date of the vote
#'
#' @keywords internal
geo_map_swiss_voting_walk <- function(data, date) {
  purrr::pluck(data, "vorlagenId") -> id
  purrr::pluck(data, "vorlagenTitel", 2, 2) -> title

  data %>%
    purrr::pluck("kantone") %>%
    tibble::as_tibble() %>%
    tidyr::unpack(resultat) -> kantone

  kantone %>%
    dplyr::pull(gemeinden) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    tidyr::unpack(resultat) %>%
    dplyr::mutate(geoLevelnummer = readr::parse_integer(geoLevelnummer)) -> gemeinden

  themakart::thema_map("inst", "voge", 2020, "vf") %>%
    geo_map_swiss_voting_write(
      data = gemeinden,
      id = id,
      title = title,
      level = "comm",
      date = date
    )

  themakart::thema_map("inst", "kant", 1997) %>%
    geo_map_swiss_voting_write(
      data = kantone,
      id = id,
      title = title,
      level = "cant",
      date = date
    )
}

#' Internal write voting maps
#'
#' @param map The map data
#' @param data The voting data
#' @param id The id of the voting object
#' @param title The title of the voting object
#' @param level The geometry level for the filename
#' @param date The date of the vote
#'
#' @keywords internal
geo_map_swiss_voting_write <- function(map, data, id, title, level, date) {
  pretty_date <- withr::with_locale(new = c("LC_TIME" = "fr_CH"), format(date, "%d %B %Y"))

  map %>%
    dplyr::left_join(data, by = c("id" = "geoLevelnummer")) %>%
    dplyr::mutate(value = santoku::chop(jaStimmenInProzent, c(0,10,20,30,40,50,60,70,80,90,100))) %>%
    ggplot2::ggplot(ggplot2::aes(fill = value)) +
    geo_map_swiss_relief() +
    ggplot2::geom_sf(color = "white", size = .1) +
    geo_map_swiss_lakes() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = prismatic::clr_alpha(
        paletteer::paletteer_c("pals::ocean.curl", direction = -1, 10),
        alpha = .8
      ),
      breaks = c("[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
                 "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)"),
      limits = c("[0, 10)", "[10, 20)", "[20, 30)", "[30, 40)", "[40, 50)",
                 "[50, 60)", "[60, 70)", "[70, 80)", "[80, 90)", "[90, 100)")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        even.steps = TRUE, show.limits = TRUE,
        barheight = 20, label.hjust = 1
      )
    ) +
    ggplot2::labs(
      title = glue::glue('Votation du {pretty_date}'),
      subtitle = title %>% stringr::str_wrap(65),
      x = "", y = "", fill = "",
      caption = "Fond: OFS ThemaKart (2020), Données: OFS (2020)"
    ) +
    ggeo::ggeotheme(
      theme = geotools::gtl_options("theme"),
      mode = geotools::gtl_options("mode"),
      plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 0, unit = "cm")),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(b = 0, unit = "cm")),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(t = 1, r = 1.5, unit = "cm")
    ) -> plot

  ggeo::ggeosave(
    glue::glue("votemap_{id}_{level}_{date}"),
    width = geotools::gtl_options("plot_full_width"),
    height = geotools::gtl_options("plot_full_height")
  )
}

#' Swiss relief background for maps
#'
#' @return A list of ggplot 2 layers for Swiss relief backgroup
#' @export
geo_map_swiss_relief <- function() {
  list(
    ggplot2::geom_sf(data = themakart::thema_map("inst", "suis", 1848, "gf"),
                     fill = "white", size = .1),
    ggplot2::geom_raster(data = themakart::thema_relief(),
                         ggplot2::aes_string(x = "x", y = "y",
                                             fill = NULL,
                                             alpha = "value")),
    ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = "none")
  )
}

#' Swiss lakes for maps
#'
#' @param fill_color A color for the lakes (default: skyblue)
#' @param source The geometry for lakes (default: ThemaKart lakes)
#'
#' @return A ggplot 2 layer for Swiss lakes
#' @export
geo_map_swiss_lakes <- function(fill_color = "skyblue", source = themakart::thema_topo("seen")) {
  ggplot2::geom_sf(data = source, fill = fill_color, color = NA)
}

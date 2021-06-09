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
    ggplot2::scale_alpha(name = "", range = c(0.6, 0), guide = F)
  )
}

#' Swiss lakes for maps
#'
#' @param fill_color A color for the lakes (default: skyblue)
#'
#' @return A ggplot 2 layer for Swiss lakes
#' @export
geo_map_swiss_lakes <- function(fill_color = "skyblue") {
  ggplot2::geom_sf(data = themakart::thema_topo("seen"), fill = fill_color, color = NA)
}


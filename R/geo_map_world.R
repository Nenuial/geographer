#' Generate world map
#'
#' @param df A dataframe with data breaks in the cut column
#' @param title The title of the map
#' @param year The year of the map
#' @param unit The units
#' @param palette The legend colors
#' @param caption The data source
#'
#' @return A ggplot
#' @export
geo_plot_world <- function(df, title, year, unit, palette, caption) {
  rnaturalearth::ne_countries(returnclass = "sf") %>%
    dplyr::filter(adm0_a3 != "ATA") %>%
    dplyr::left_join(df, by = c("iso_a3" = "iso3c")) -> df_plot

  df_plot %>%
    ggplot2::ggplot(ggplot2::aes(fill = cut)) +
    ggplot2::geom_sf(lwd = .4) +
    ggplot2::scale_fill_manual(
      values = scales::alpha(colour = palette,
                             alpha = geo_pkg_options("opacity")),
      drop = FALSE,
      na.value = scales::alpha(colour = "#4d4d4d",
                               alpha = geo_pkg_options("opacity")),
      labels = c(glue::glue("{levels(df$cut)} {unit}"),
                 translate("No data", "Pas de données")),
      guide = ggplot2::guide_legend(
        title = NULL,
        direction = "vertical",
        lwd = .4,
        keyheight = unit(6, units = "mm"),
        keywidth = unit(1.2, units = "cm"),
        label.hjust = 0,
        label.vjust = .5,
        reverse = T,
        label.position = "right"
      )) +
    ggplot2::theme(legend.position = c(.98, .5)) +
    ggeo::ggeotheme(theme = geo_pkg_options("theme"),
                    mode = geo_pkg_options("mode")) +
    ggplot2::coord_sf(crs = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", datum = NA) +
    ggplot2::labs(title = stringr::str_to_upper(title),
                  subtitle = year,
                  fill = "",
                  caption = glue::glue("{translate('Data:', 'Données :')} {caption}"))
}

#' Create world maps for an indicator
#'
#' @param indicator A list of parameters
#'
#' @return Nothing
#' @export
geo_wb_world_map <- function(indicator) {
  df <- geo_wb_data(indicator)

  indicator$years %>%
    purrr::map(~geo_plot_world(df %>% dplyr::filter(date == .x),
                               title = indicator$title,
                               year = .x,
                               unit = indicator$unit,
                               palette = ggeo::return_palette(df %>%
                                                                pull(cut) %>%
                                                                levels() %>%
                                                                length(),
                                                              indicator$center,
                                                              indicator$palette),
                               caption = translate("World Bank", "Banque Mondiale"))) %>%
    patchwork::align_plots() %>%
    purrr::walk(~ggeo::ggeosave(glue::glue("{indicator$file}_{.x$labels$subtitle}"),
                                plot = .x, height = 31, width = 64))
}

#' Fetch World Bank data
#'
#' @param indicator A list of parameters
#'
#' @return A dataframe
#' @export
geo_wb_data <- function(indicator) {
  wbstats::wb_data(indicator = indicator$code,
                   start_date = min(as.numeric(indicator$years)),
                   end_date = max(as.numeric(indicator$years))) %>%
    dplyr::mutate(data = !!indicator$operation) %>%
    dplyr::mutate(data = !!indicator$operation,
                  cut = santoku::chop(data, breaks = indicator$breaks,
                                      labels = santoku::lbl_dash(),
                                      extend = T, drop = F) %>%
                    forcats::fct_relabel(chop_relabel_dash))
}


#' Return Historical World Map
#'
#' @param date A date object
#'
#' @return An sf object
#' @export
geo_historical_world_map <- function(date) {
  cshapes::cshp(date, dependencies = T) |>
    sf::st_as_sf()
}


# TODO

geo_globe <- function(lat, lon) {
  rnaturalearth::ne_countries(returnclass = "sf") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(
      crs = "+proj=ortho +lat_0=45 +lon_0=70 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs",
      datum = NA
    ) +
    ggplot2::theme_minimal()
}

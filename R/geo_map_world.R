#' Create world maps for an indicator
#'
#' @param indicator A list of lists.
#'   Each list should contain:
#'   \describe{
#'     \item{code}{A named vector with the name of the indicator and it's World Bank code}
#'     \item{operation}{An expression that can make operations based on the indicator names}
#'     \item{title}{Title of the map}
#'     \item{file}{Name of the file}
#'     \item{palette}{List with parameters *palette* for the name of the palette,
#'     *type* for the type of palette and *dir* for the direction}
#'     \item{breaks}{Breaks for cutting the data}
#'     \item{center}{Where is the center of the data (for diverging palettes), -1 otherwise}
#'     \item{unit}{The unit of the indicator}
#'     \item{year}{A vector with each year wanted}
#'   }
#'
#' @return NULL
#' @export
#' @examplesIf interactive()
#' # Not run: saves the plots on file
#' indicators <- list(
#'   list(
#'     code = c("population" = "EN.POP.DNST"),
#'     operation = rlang::expr(population),
#'     title = geotools::gtl_translate_enfr(
#'       "Population density", "Densité de population"
#'     ),
#'     file = "density",
#'     palette = list(
#'       palette = "viridis::viridis",
#'       type = "cont", dir = -1
#'     ),
#'     breaks = c(10, 25, 50, 75, 100, 150, 300, 1000),
#'     center = -1,
#'     unit = "pp/km²",
#'     years = c(2019)
#'   )
#' )
#'
#' gph_wb_world_map(indicators)
gph_wb_world_map <- function(indicator) {
  df <- gph_wb_data(indicator)

  df |>
    dplyr::pull(cut) |>
    levels() |>
    length() -> palette_length # nolint: object_usage_linter

  indicator |>
    dplyr::pull(year) |>
    purrr::map(~ gph_map_world(df |> dplyr::filter(date == .x),
      title = indicator$title,
      year = .x,
      unit = indicator$unit,
      palette = ggeo::ggeopal_center(
        palette_length,
        indicator$center,
        indicator$palette
      ),
      caption = geotools::gtl_translate_enfr("World Bank", "Banque Mondiale")
    )) |>
    patchwork::align_patches() |>
    purrr::walk(
      ~ ggeo::ggeosave(
        glue::glue("{indicator$file}_{.x$labels$subtitle}"),
        plot = .x, height = 31, width = 64
      )
    )
}

#' @export
#' @rdname gph_wb_world_map
gph_wb_world_hc <- function(indicator) {
  df <- gph_wb_data(indicator)

  palette <- function(..., pal_center = indicator$center, pal_palette = indicator$palette) {
    ggeo::ggeopal_center(
      ...,
      pal_center,
      pal_palette
    )
  }

  df |>
    dplyr::pull(cut) |>
    levels() |>
    length() -> palette_length # nolint: object_usage_linter

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = indicator$title) |>
    #highcharter::hc_subtitle(text = indicator$subtitle) |>
    highcharter::hc_caption(
      text = indicator$caption
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = indicator$title,
      mapData = map,
      data = df |> dplyr::select(country, iso3c, value = data),
      value = "value",
      joinBy = c("iso.a3", "iso3c"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(df$data, breaks = indicator$breaks), palette
      ),
      showInLegend = TRUE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          this.point.value + ' {indicator$unit}' }}"))
    )
}

#' Generate world map
#'
#' @param df A dataframe with data breaks in the cut column
#' @param title The title of the map
#' @param year The year of the map
#' @param unit The units
#' @param palette The legend colors
#' @param caption The data source
#'
#' @return A ggplot2
#' @keywords internal
gph_map_world <- function(df, title, year, unit, palette, caption) {
  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(adm0_a3 != "ATA") |>
    dplyr::left_join(df, by = c("iso_a3" = "iso3c")) -> df_plot

  df_plot |>
    ggplot2::ggplot(ggplot2::aes(fill = cut)) +
    ggplot2::geom_sf(lwd = .4) +
    ggplot2::scale_fill_manual(
      values = scales::alpha(
        colour = palette,
        alpha = geo_pkg_options("opacity")
      ),
      drop = FALSE,
      na.value = scales::alpha(
        colour = "#4d4d4d",
        alpha = geo_pkg_options("opacity")
      ),
      labels = c(
        glue::glue("{levels(df$cut)} {unit}"),
        geotools::gtl_translate_enfr("No data", "Pas de donn\u00e9es")
      ),
      guide = ggplot2::guide_legend(
        title = NULL,
        direction = "vertical",
        lwd = .4,
        keyheight = unit(6, units = "mm"),
        keywidth = unit(1.2, units = "cm"),
        label.hjust = 0,
        label.vjust = .5,
        reverse = TRUE,
        label.position = "right"
      )
    ) +
    ggplot2::theme(legend.position = c(.98, .5)) +
    ggeo::ggeotheme(
      theme = geo_pkg_options("theme"),
      mode = geo_pkg_options("mode")
    ) +
    ggplot2::coord_sf(crs = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", datum = NA) +
    ggplot2::labs(
      title = stringr::str_to_upper(title),
      subtitle = year,
      fill = "",
      caption = glue::glue("{geotools::gtl_translate_enfr('Data:', 'Donn\u00e9es :')} {caption}")
    )
}

#' Fetch World Bank data
#'
#' @param indicator A list of parameters
#'
#' @return A dataframe
#' @keywords internal
gph_wb_data <- function(indicator) {
  wbstats::wb_data(
    indicator = indicator$code,
    start_date = min(as.numeric(indicator$years)),
    end_date = max(as.numeric(indicator$years))
  ) |>
    dplyr::mutate(data = !!indicator$operation) |>
    dplyr::mutate(
      data = !!indicator$operation,
      cut = santoku::chop(data,
        breaks = indicator$breaks,
        labels = santoku::lbl_dash(),
        extend = TRUE, drop = FALSE
      ) |>
        forcats::fct_relabel(geotools::gtl_relabel_dash)
    )
}

#' Return Historical World Map
#'
#' @param date A date object
#'
#' @return An sf object
#' @export
#' @examples
#' lubridate::ymd("1945-06-01") |>
#'   gph_historical_world_map() |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_sf()
#'
gph_historical_world_map <- function(date) {
  cshapes::cshp(date, dependencies = TRUE) |>
    sf::st_as_sf()
}

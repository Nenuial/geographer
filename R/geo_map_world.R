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
    purrr::map(
      ~ gph_map_world(
        df |> dplyr::filter(date == .x),
        title = indicator$title,
        year = .x,
        unit = indicator$unit,
        palette = ggeo::ggeopal_center(
          palette_length,
          indicator$center,
          indicator$palette
        ),
        caption = geotools::gtl_translate_enfr("World Bank", "Banque Mondiale")
      )
    ) |>
    patchwork::align_patches() |>
    purrr::walk(
      ~ ggeo::ggeosave(
        glue::glue("{indicator$file}_{.x$labels$subtitle}"),
        plot = .x,
        height = 31,
        width = 64
      )
    )
}

#' @export
#' @rdname gph_wb_world_map
gph_wb_world_hc <- function(indicator) {
  df <- gph_wb_data(indicator)

  palette <- function(
    ...,
    pal_center = indicator$center,
    pal_palette = indicator$palette
  ) {
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

  map <- geodata::gdt_world_topojson()

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = indicator$title) |>
    highcharter::hc_caption(
      text = indicator$caption
    ) |>
    geotools::gtl_hc_map_view(
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
        santoku::chop(df$data, breaks = indicator$breaks, drop = FALSE),
        palette
      ),
      showInLegend = TRUE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue(
        "function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          this.point.value + ' {indicator$unit}' }}"
      ))
    )
}

#' World map of an indicator
#'
#' A world map of one indicator for one year, in classes, with a light-to-dark ramp of one hue that
#' also reads in black and white. The legend, under the map, follows the current language
#' (see [geotools::gtl_opt_set_i18n()]). The map has no title; the caption gives the source.
#'
#' @param indicator A World Bank indicator code (`"SP.DYN.TFRT.IN"`) or a column of the UN World
#'   Population Prospects 2024 indicators (`"TFR"`, `"PopDensity"`, see [geodata::gdt_wpp_indicators()]).
#'   World Bank codes are told apart by their dots.
#' @param year The year
#' @param breaks The limits between the classes: five classes of about the same size when `NULL`
#' @param unit The unit, written above the legend
#' @param palette A sequential ColorBrewer palette: "Blues", "Greens", "Greys", "Oranges", "Purples",
#'   "Reds"...
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examplesIf interactive()
#' gph_map_world_indicator(
#'   "TFR", 2023,
#'   breaks = c(1.5, 2.1, 3, 4.5),
#'   unit = geotools::gtl_translate_enfr("children per woman", "enfants par femme", "Kinder pro Frau")
#' )
#' gph_map_world_indicator("SP.DYN.IMRT.IN", 2022, unit = "‰", palette = "Reds")
gph_map_world_indicator <- function(indicator, year, breaks = NULL, unit = NULL, palette = "Blues",
                                    theme = ggplot2::theme_void()) {
  if (grepl(".", indicator, fixed = TRUE)) {
    data <- wbstats::wb_data(indicator = c(value = indicator), start_date = year, end_date = year) |>
      dplyr::select(iso3c, value)
    source <- geotools::gtl_translate_enfr(
      "Data: World Bank", "Donn\u00e9es\u00a0: Banque Mondiale", "Daten: Weltbank"
    )
  } else {
    data <- geodata::gdt_wpp_indicators(years = year)
    if (!indicator %in% names(data)) {
      rlang::abort(glue::glue("{indicator} is neither a World Bank code nor a WPP 2024 indicator"))
    }
    data <- data |>
      dplyr::filter(!is.na(ISO3_code)) |> # nolint: object_usage_linter
      dplyr::select(iso3c = ISO3_code, value = dplyr::all_of(indicator))
    source <- geotools::gtl_translate_enfr(
      "Data: UN, World Population Prospects 2024",
      "Donn\u00e9es\u00a0: ONU, World Population Prospects 2024",
      "Daten: UNO, World Population Prospects 2024"
    )
  }
  data <- dplyr::filter(data, !is.na(value))
  if (nrow(data) == 0) {
    rlang::abort(glue::glue("No data for {indicator} in {year}"))
  }

  if (is.null(breaks)) {
    breaks <- unique(signif(stats::quantile(data$value, probs = seq(.2, .8, .2), names = FALSE), 2))
  }
  data$bin <- santoku::chop(
    data$value, breaks,
    labels = santoku::lbl_dash(
      symbol = "\u2013", fmt = geotools::gtl_label_number(drop0trailing = TRUE), first = "< {r}", last = "\u2265 {l}"
    ),
    extend = TRUE, drop = FALSE
  )
  classes <- levels(data$bin)
  no_data <- geotools::gtl_translate_enfr("No data", "Pas de donn\u00e9es", "Keine Daten")

  # Natural Earth has -99 for the ISO code of France and Norway, and KOS for Kosovo
  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(adm0_a3 != "ATA") |>
    dplyr::mutate(iso3c = dplyr::case_when(
      adm0_a3 == "KOS" ~ "XKX",
      iso_a3_eh != "-99" ~ iso_a3_eh,
      TRUE ~ adm0_a3
    )) |>
    dplyr::left_join(data, by = "iso3c") |>
    ggplot2::ggplot(ggplot2::aes(fill = bin)) + # nolint: object_usage_linter
    ggplot2::geom_sf(color = "grey40", linewidth = .1) +
    ggplot2::scale_fill_manual(
      values = scales::gradient_n_pal(scales::brewer_pal(palette = palette)(9)[3:9])(
        seq(0, 1, length.out = length(classes))
      ),
      breaks = c(classes, NA),
      labels = c(classes, no_data),
      na.value = "white",
      drop = FALSE
    ) +
    ggplot2::coord_sf(
      crs = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
      datum = NA
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.text.position = "bottom",
      legend.key.width = ggplot2::unit(1.6, "cm"),
      legend.key.height = ggplot2::unit(.4, "cm"),
      legend.key.spacing.x = ggplot2::unit(0, "cm")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(fill = unit, caption = source)
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
    ggplot2::coord_sf(
      crs = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
      datum = NA
    ) +
    ggplot2::labs(
      title = stringr::str_to_upper(title),
      subtitle = year,
      fill = "",
      caption = glue::glue(
        "{geotools::gtl_translate_enfr('Data:', 'Donn\u00e9es\u00a0:')} {caption}"
      )
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
      cut = santoku::chop(
        data,
        breaks = indicator$breaks,
        labels = santoku::lbl_dash(),
        extend = TRUE,
        drop = FALSE
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

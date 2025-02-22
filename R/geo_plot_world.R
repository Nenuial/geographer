#' Crete a treemap plot of the world countries by area
#'
#' @returns A highcharter plot
#' @export
#'
#' @examples gph_country_area_tree()
gph_country_area_tree <- function() {
  palette <- function(...) {
    c(
      "#00496f",
      "#dd4123",
      "#ecc132",
      "#adc05f",
      "#0b6b8b"
    )
  }

  data_surface <- wbstats::wb_data(c("data" = "AG.SRF.TOTL.K2"), start_date = 2022)

  data_surface |>
    dplyr::mutate(
      continent = countrycode::countrycode(
        iso2c,
        origin = "iso2c",
        destination = "continent"
      )
    ) |>
    dplyr::filter(!is.na(continent)) |>
    dplyr::mutate(
      continent = forcats::as_factor(continent)
    ) -> data_surface

  highcharter::highchart() |>
    highcharter::hc_add_series(
      type = "treemap",
      data = data_surface,
      highcharter::hcaes(name = country, value = data, color = as.integer(continent))
    ) |>
    highcharter::hc_caption(
      text = "Source: Banque Mondiale (2022)"
    ) |>
    highcharter::hc_tooltip(
      valueSuffix = " km\u00b2"
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_discrete_color_axis(
        data_surface$continent,
        palette
      ),
      showInLegend = FALSE
    )
}

#' Crete a treemap plot of the world countries by population
#'
#' @returns A highcharter plot
#' @export
#'
#' @examples gph_country_population_tree()
gph_country_population_tree <- function() {
  palette <- function(...) {
    c(
      "#00496f",
      "#dd4123",
      "#ecc132",
      "#adc05f",
      "#0b6b8b"
    )
  }

  data_pop <- wbstats::wb_data(c("data" = "SP.POP.TOTL"), start_date = 2022)

  data_pop |>
    dplyr::mutate(
      continent = countrycode::countrycode(
        iso2c,
        origin = "iso2c",
        destination = "continent"
      )
    ) |>
    dplyr::filter(!is.na(continent)) |>
    dplyr::mutate(
      continent = forcats::as_factor(continent)
    ) -> data_pop

  highcharter::highchart() |>
    highcharter::hc_add_series(
      type = "treemap",
      data = data_pop,
      highcharter::hcaes(name = country, value = data, color = as.integer(continent))
    ) |>
    highcharter::hc_caption(
      text = "Source: Banque Mondiale (2022)"
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_discrete_color_axis(
        data_pop$continent,
        palette
      ),
      showInLegend = FALSE
    )
}

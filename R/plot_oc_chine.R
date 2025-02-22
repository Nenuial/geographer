# Démographie ------------------------------------------------------------

#' Sex-ratio à la naissance
#'
#' @returns A highcharter plot
#' @export
#'
#' @examples
#' oc_chine_hc_sex_ratio()
oc_chine_hc_sex_ratio <- function() {
  wbstats::wb_data(
    indicator = c(sex_ratio = "SP.POP.BRTH.MF"),
    start_date = 1960,
    end_date = 2022
  ) |>
    dplyr::filter(iso3c != "LIE") |>
    dplyr::mutate(sex_ratio = round(sex_ratio, 2)) |>
    dplyr::mutate(country = countrycode::countrycode(iso3c, "iso3c", "un.name.fr", warn = FALSE)) |>
    tidyr::drop_na(country, sex_ratio) -> sex_ratio_data

  sex_ratio_data |>
    dplyr::filter(max(sex_ratio) > 1.1, .by = iso3c) |>
    dplyr::pull(iso3c) |>
    unique() -> high_ratio_countries

  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = sex_ratio_data |>
        dplyr::filter(!(iso3c %in% high_ratio_countries)),
      type = "line",
      name = "Sex-ratio",
      color = "#d7d7d7",
      # smaller line width
      lineWidth = .4,
      enableMouseTracking = FALSE,
      showInLegend = FALSE,
      highcharter::hcaes(x = date, y = sex_ratio, group = iso3c)
    ) |>
    highcharter::hc_add_series(
      data = sex_ratio_data |>
        dplyr::filter(iso3c %in% high_ratio_countries),
      type = "line",
      lineWidth = 2.5,
      name = sex_ratio_data |>
        dplyr::filter(iso3c %in% high_ratio_countries) |>
        dplyr::pull(country) |>
        unique(),
      highcharter::hcaes(x = date, y = sex_ratio, group = iso3c)
    ) |>
    highcharter::hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE)
      )
    ) |>
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.country} :</b> {point.y}"
    ) |>
    highcharter::hc_title(text = "Sex-ratio \u00e0 la naissance") |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_caption(text = "Source: World Bank (2024)")
}

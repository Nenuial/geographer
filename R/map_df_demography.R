#' Humand Development Index (HDI) map
#'
#' @param year The year
#'
#' @returns A highcharter map
#' @export
#'
#' @examples
#' df_demography_map_hc_world_hdi(2020)
df_demography_map_hc_world_hdi <- function(year) {
  geodata::gdt_un_hdr_composite |>
    dplyr::filter(indicator == "hdi") |>
    dplyr::filter(!stringr::str_detect(iso3, "^ZZ")) |>
    dplyr::filter(year == {{ year }}) -> hdi_data

  palette <- function(...) {
    viridis::viridis(direction = -1, alpha = .8, ...)
  }

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Human Development Index") |>
    highcharter::hc_caption(
      text = "Source: United Nations Development Programme"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "Parlament 2022",
      mapData = map,
      data = hdi_data,
      value = "value",
      joinBy = c("iso.a3", "iso3"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(hdi_data$value, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1), drop = FALSE), palette
      ),
      showInLegend = TRUE
    )
}

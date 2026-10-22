#' Historical world population plot
#'
#' World population graph from 10'000 BCE to 1200 CE
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_historical()
df_demography_graph_world_population_historical <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population),
      color = "black",
      size = .8,
      data = geodata::df_demography_population_historical
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-10000, 1200),
      expand = c(0, 0),
      breaks = seq(-10000, 1200, 1000)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = function(x) scales::number(x, scale = .000001)
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 300000000)) +
    ggplot2::labs(
      title = geotools::gtl_translate_enfr(
        "World population 10'000 BCE to 1200 CE",
        "Population mondiale de 10'000 av. JC \u00e0 1200 ap. JC"
      ),
      x = "",
      y = "Population (millions)"
    ) +
    theme +
    ggplot2::theme(
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    )
}

#' World population
#'
#' World population graph
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_current()
df_demography_graph_world_population_current <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = "historical"),
      size = .8,
      data = geodata::df_demography_population_historical |> dplyr::filter(year <= 1950)
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = "current"),
      size = .8,
      data = geodata::df_demography_population_historical |> dplyr::filter(year >= 1950)
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = population, color = estimate),
      size = .8,
      data = geodata::df_demography_population_estimates
    ) +
    ggplot2::scale_x_continuous(
      limits = c(1200, 2100),
      expand = c(0, 0),
      breaks = seq(1200, 2100, 100)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(0, 15000000000, 5000000000),
      labels = function(x) scales::number(x, scale = .000000001, accuracy = 1)
    ) +
    ggplot2::scale_color_manual(
      breaks = c("historical", "current", "un_low", "un_medium", "un_high"),
      values = c(
        "historical" = "black", "current" = "blue",
        "un_low" = "green", "un_medium" = "orange", "un_high" = "red"
      ),
      labels = c(
        "historical" = geotools::gtl_translate_enfr("historical", "historique"),
        "current" = geotools::gtl_translate_enfr("current", "actuel"),
        "un_low" = geotools::gtl_translate_enfr("low estimate", "estimation basse"),
        "un_medium" = geotools::gtl_translate_enfr("medium estimate", "estimation moyenne"),
        "un_high" = geotools::gtl_translate_enfr("high estimate", "estimation haute")
      )
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = NULL)
    ) +
    theme +
    ggplot2::theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::labs(
      title = geotools::gtl_translate_enfr("World population 1200 to 2100", "Population mondiale de 1200 \u00e0 2100"),
      x = "",
      y = geotools::gtl_translate_enfr("Population (billions)", "Population (milliards)")
    )
}

#' World population growth rate
#'
#' World population growth rate graph
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' df_demography_graph_world_population_growth()
df_demography_graph_world_population_growth <- function(theme = ggplot2::theme_minimal()) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = year, y = growth),
      color = "black",
      size = .8,
      data = geodata::df_demography_population_growth
    ) +
    ggplot2::scale_x_continuous(
      limits = c(1955, 2015),
      expand = c(0, 0),
      breaks = seq(1955, 2015, 5)
    ) +
    theme +
    ggplot2::theme(
      plot.margin = ggplot2::margin(.2, .6, .2, .6, unit = "cm"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::labs(
      title = geotools::gtl_translate_enfr("World population growth", "Acroissement de la population mondiale"),
      x = "",
      y = geotools::gtl_translate_enfr("Growth rate (%)", "Taux d'acroissement (%)")
    )
}

#' Graph of the Human Development Index
#'
#' Every country of the world with the Human Development Index
#' from 1990 to 2021.
#'
#' @returns A hicharter graph
#' @export
#'
#' @examples
#' df_demography_hc_world_hdi()
df_demography_hc_world_hdi <- function() {
  geodata::gdt_un_hdr_composite |>
    dplyr::filter(!stringr::str_detect(iso3, "^ZZ")) |>
    dplyr::filter(indicator == "hdi") -> hdi_data

  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = hdi_data,
      "line",
      name = "HDI",
      showInLegend = FALSE,
      dashStyle = "solid",
      color = "grey",
      lineWidth = .6,
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = year, y = value, group = iso3)
    ) |>
    ## Remove the symbols from the lines
    highcharter::hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE)
      )
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
            return '<b>' + this.series.name + '</b>: ' + this.point.value + '<br/>' +
            '<b>Country</b>: ' + this.point.country + '<br/>' +
            '<b>Year</b>: ' + this.point.year;
          }}"))
    )
}

# nolint start: commented_code_linter
# For future me. Requires patchwork.
# #' World population patchwork composition
# #'
# #' World population graph with historical, current and future
# #' numbers as well as growth rates.
# #'
# #' @param theme A ggplot2 theme
# #'
# #' @return A ggplot2 graph
# #' @export
# df_demography_graph_world_population <- function(theme = ggplot2::theme_minimal()) {
#   df_demography_graph_world_population_historical(theme) +
#     df_demography_graph_world_population_current(theme) +
#     df_demography_graph_world_population_growth(theme) +
#     patchwork::plot_layout(ncol = 1) +
#     patchwork::plot_annotation(tag_levels = "a", tag_suffix = ".") &
#     ggplot2::theme(
#       plot.tag = ggplot2::element_text(size = 12),
#       plot.tag.position = "topleft"
#     )
# }
# nolint end

#' World population since year 0
#'
#' World population since year 0 (historical estimates, then the World Bank since 1960)
#' with the dates of the billions (United Nations) and the Black Death.
#'
#' @param from The first year
#'
#' @return A highcharter graph
#' @export
#' @examplesIf interactive()
#' df_demography_hc_world_population()
df_demography_hc_world_population <- function(from = 0) {
  data <- geodata::gdt_world_population_history(from) |>
    dplyr::mutate(billions = round(population / 1e9, 3))
  billion <- geotools::gtl_translate_enfr("bn", "Mrd")
  milestones <- tibble::tibble(
    year = c(1804, 1927, 1960, 1974, 1987, 1999, 2011, 2022),
    billions = 1:8,
    label = paste(1:8, billion)
  )

  highcharter::highchart() |>
    highcharter::hc_add_series(data, "area", highcharter::hcaes(x = year, y = billions),
      name = geotools::gtl_translate_enfr("World population", "Population mondiale"),
      color = "#919c4c", fillOpacity = .3, marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_add_series(milestones, "scatter", highcharter::hcaes(x = year, y = billions),
      name = geotools::gtl_translate_enfr("Milestone", "Cap"),
      color = "#c03728", marker = list(radius = 5, symbol = "circle"),
      dataLabels = list(
        enabled = TRUE, format = "{point.label} ({point.x})",
        align = "right", x = -8, y = 2, style = list(fontSize = "14px")
      )
    ) |>
    highcharter::hc_xAxis(
      min = from, max = as.numeric(format(Sys.Date(), "%Y")) + 5, title = list(text = NULL),
      plotLines = list(hc_event_line(1347, geotools::gtl_translate_enfr("Black Death", "Peste noire"), 200))
    ) |>
    highcharter::hc_yAxis(
      title = list(text = geotools::gtl_translate_enfr("billions of people", "milliards de personnes")),
      min = 0, max = 9
    ) |>
    highcharter::hc_tooltip(
      valueDecimals = 2,
      valueSuffix = geotools::gtl_translate_enfr(" billion", " milliards"),
      headerFormat = "<b>{point.x}</b><br>"
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_caption(text = geotools::gtl_translate_enfr(
      paste0(
        "Data: historical estimates (US Census Bureau), ",
        "<a target='_blank' href='https://data.worldbank.org/indicator/SP.POP.TOTL'>World Bank</a> ",
        "since 1960; milestones: United Nations"
      ),
      paste0(
        "Donn\u00e9es\u00a0: estimations historiques (US Census Bureau), ",
        "<a target='_blank' href='https://data.worldbank.org/indicator/SP.POP.TOTL'>Banque mondiale</a> ",
        "depuis 1960\u00a0; caps\u00a0: Nations unies"
      )
    ))
}

#' A real demographic transition
#'
#' Crude birth and death rates of a country of the Human Mortality Database
#' (`geodata::gdt_hmd_demography`), the natural increase shaded between them.
#'
#' @param code The HMD country code, e.g. `"SWE"` or `"CHE"`
#' @param events A named numeric vector of years to mark with a dashed line,
#'   e.g. `c("Spanish flu" = 1918)`
#'
#' @return A highcharter graph
#' @export
#' @examples
#' df_demography_hc_transition("SWE", c(famine = 1773, "Spanish flu" = 1918))
df_demography_hc_transition <- function(code, events = NULL) {
  data <- geodata::gdt_hmd_demography |>
    dplyr::filter(Code == code) |>
    dplyr::mutate(low = pmin(CBR, CDR), high = pmax(CBR, CDR))

  highcharter::highchart() |>
    highcharter::hc_add_series(data, "arearange", highcharter::hcaes(x = Year, low = low, high = high),
      name = geotools::gtl_translate_enfr("Natural increase", "Accroissement naturel"),
      color = "#849042", fillOpacity = .3, lineWidth = 0, enableMouseTracking = FALSE
    ) |>
    highcharter::hc_add_series(data, "line", highcharter::hcaes(x = Year, y = CBR),
      name = geotools::gtl_translate_enfr("Crude birth rate", "Taux de natalit\u00e9"), color = "#fc8320"
    ) |>
    highcharter::hc_add_series(data, "line", highcharter::hcaes(x = Year, y = CDR),
      name = geotools::gtl_translate_enfr("Crude death rate", "Taux de mortalit\u00e9"), color = "#b73023"
    ) |>
    highcharter::hc_xAxis(plotLines = hc_event_lines(events), title = list(text = NULL)) |>
    highcharter::hc_yAxis(
      title = list(text = geotools::gtl_translate_enfr("per 1,000 people (\u2030)", "pour 1000 habitants (\u2030)")),
      min = 0
    ) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1, valueSuffix = " \u2030") |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE), lineWidth = 2.5)) |>
    highcharter::hc_caption(text = geotools::gtl_translate_enfr(
      "Data: <a target='_blank' href='https://www.mortality.org'>Human Mortality Database</a>",
      "Donn\u00e9es\u00a0: <a target='_blank' href='https://www.mortality.org'>Human Mortality Database</a>"
    ))
}

#' Malthus's arithmetic
#'
#' Malthus's own example (1798): the population doubles every 25 years
#' (1, 2, 4, 8... 256) while food grows by the same amount (1, 2, 3... 9).
#'
#' @return A highcharter graph
#' @export
#' @examples
#' df_demography_hc_malthus()
df_demography_hc_malthus <- function() {
  data <- tibble::tibble(year = seq(1800, 2000, 25), population = 2^(0:8), food = 1:9)

  highcharter::highchart() |>
    highcharter::hc_add_series(data, "line", highcharter::hcaes(x = year, y = population),
      name = geotools::gtl_translate_enfr("Population: doubles", "Population\u00a0: double"),
      color = "#c03728", lineWidth = 4
    ) |>
    highcharter::hc_add_series(data, "line", highcharter::hcaes(x = year, y = food),
      name = geotools::gtl_translate_enfr("Food: + 1", "Nourriture\u00a0: + 1"),
      color = "#919c4c", lineWidth = 4
    ) |>
    highcharter::hc_xAxis(title = list(text = NULL), tickInterval = 25) |>
    highcharter::hc_yAxis(title = list(text = "1800 = 1")) |>
    highcharter::hc_tooltip(shared = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = TRUE, radius = 5))) |>
    highcharter::hc_caption(text = geotools::gtl_translate_enfr(
      "Malthus's own example, one step every 25 years: population 1, 2, 4, 8\u2026 256; food 1, 2, 3\u2026 9",
      paste0(
        "L'exemple de Malthus, un pas tous les 25 ans\u00a0: ",
        "population 1, 2, 4, 8\u2026 256\u00a0; nourriture 1, 2, 3\u2026 9"
      )
    ))
}

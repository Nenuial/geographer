#' Create demogram for country
#'
#' Provides basic ggplot2 graph with the following data on it :
#' \describe{
#'   \item{population}{the population curve (blue)}
#'   \item{birth_rate}{the birth rate curve (grey)}
#'   \item{death_rate}{the death_rate curve (black)}
#' }
#'
#' @param country A string with the country name
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
gph_demogram <- function(country, theme = ggplot2::theme_minimal()) {
  data <- geodata::gdt_wb_demo(country)
  coef <- max(data$population, na.rm = T) / max(data$birth_rate, data$death_rate, na.rm = T)

  data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(date, birth_rate, color = "cbr"), size = .8) +
    ggplot2::geom_line(ggplot2::aes(date, death_rate, color = "cdr"), size = .8) +
    ggplot2::geom_line(ggplot2::aes(date, population / coef), color = "blue", size = .8) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      sec.axis = ggplot2::sec_axis(
        trans = ~ . * coef,
        name = "Population",
        labels = ggeo::ggeo_label_sci_10
      )
    ) +
    ggplot2::scale_color_manual(
      values = c("grey", "black"),
      breaks = c("cbr", "cdr"),
      labels = c("cbr" = geotools::translate_enfr("birth", "natalité"),
                 "cdr" = geotools::translate_enfr("death", "mortalité"))
    ) +
    theme +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(color = "blue"),
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    ) +
    ggplot2::labs(
      x = "",
      y = geotools::translate_enfr("Rate (‰)", "Taux (‰)"),
      color = "",
      caption = geotools::translate_enfr("Data: World Bank", "Données: Banque Mondiale")
    )
}

#' Create demogram for country
#'
#' Provides basic ggplot2 graph with the following data on it :
#' \describe{
#'   \item{population}{the population curve (blue)}
#'   \item{birth_rate}{the birth rate curve (grey)}
#'   \item{death_rate}{the death_rate curve (black)}
#' }
#'
#' @param country A string with the country name
#'
#' @return A highcharter graph
#' @export
gph_highcharter_demogram <-  function(country) {
  data <- geodata::gdt_wb_demo(country)

  highcharter::highchart() |>
    highcharter::hc_title(text = glue::glue("Demograph for {country}")) |>
    highcharter::hc_xAxis(title = list(text = "Year")) -> hc

  list(
    list(min = 0, title = list(text = "Rates")),
    list(min= 0, title = list(text = "Population", style = list(color = "blue")), opposite = TRUE)
  ) -> hc$x$hc_opts$yAxis

  hc |>
    highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 1,
      name = "population",
      color = "blue",
      dashStyle = "solid",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = population)
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 0,
      name = "crude birth rate",
      color = "grey",
      dashStyle = "solid",
      tooltip = list(valueSuffix = " ‰"),
      highcharter::hcaes(x = date, y = birth_rate)
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 0,
      name = "crude death rate",
      color = "black",
      dashStyle = "solid",
      tooltip = list(valueSuffix = " ‰"),
      highcharter::hcaes(x = date, y = death_rate)
    )
}

#' Create lexgram for country
#'
#' Provides basic ggplot2 graph with the following data on it :
#' \describe{
#'   \item{lex}{the total life expectancy (black dotted)}
#'   \item{lex_male}{the male life expectancy (blue)}
#'   \item{ley_female}{the male life expectancy (red)}
#' }
#'
#' @param country A string with the country name
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
gph_lexgram <- function(country, theme = ggplot2::theme_minimal()) {
  geodata::gdt_wb_lex(country) |>
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = lex), linetype = "dotted", size = .8) +
    ggplot2::geom_line(ggplot2::aes(y = lex_male), color = "blue", size = .8) +
    ggplot2::geom_line(ggplot2::aes(y = lex_female), color = "red", size = .8) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      x = "",
      y = geotools::translate_enfr("Life expectancy", "Espérance de vie"),
      caption = geotools::translate_enfr("Data: World Bank", "Données: Banque Mondiale")
    ) +
    theme
}

#' Create population pyramid
#'
#' @param country A string with the country name
#' @param year An integer for the year
#'
#' @return A ggplot graph
#' @export
gph_pyramid <- function(country, year, theme = ggplot2::theme_minimal()) {
  country_name <- country
  pyramid_data <- geodata::gdt_idb_pyramid(country, year)

  if(geotools::gtl_opt_short_language() == "fr") countrycode::countrycode(
    country, "country.name", "un.name.fr",
    custom_match = c(
      "Kosovo" = "Kosovo",
      "Gaza" = "Gaza",
      "West Bank" = "Cisjordanie"
    )
  ) -> country_name

  pop_max <- max(abs(c(max(pyramid_data$population), min(pyramid_data$population))))

  pyramid_data |>
    ggplot2::ggplot(ggplot2::aes(x = age, y = population, fill = gender)) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0),
                                sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_y_continuous(
      limits = c(-pop_max, pop_max),
      breaks = ggeo::ggeo_remove_breaks(scales::breaks_pretty(6), list(0)),
      labels = ggeo::ggeo_label_pyramid
    ) +
    ggplot2::scale_fill_manual(values = c("male" = "#7294d4", "female" = "#e69fc4")) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = .5),
      plot.subtitle = ggplot2::element_text(hjust = .5)
    ) +
    ggplot2::labs(
      title = country_name,
      subtitle = year,
      x = "", y = "",
      caption = geotools::translate_enfr("Data: US Census Bureau", "Données: US Census Bureau")
    )
}

#' Create relative population pyramid (5 year cohorts)
#'
#' @param country A string with the country name
#' @param year An integer for the year
#'
#' @return A ggplot graph
#' @export
gph_pyramid_relative <- function(country, year, theme = ggplot2::theme_minimal()) {
  geodata::gdt_idb_pyramid_5y(country, year) |>
    dplyr::mutate(population = ifelse(gender == "male", population * -1, population)) |>
    dplyr::mutate(cohort = forcats::fct_inorder(cohort)) -> data

  total_population <- sum(abs(data$population))

  data |>
    dplyr::mutate(percent = population / total_population) |>
    dplyr::mutate(align = ifelse(sign(population) == 1, -0.2, 1.2)) -> data_plot

  data_plot %>%
    ggplot2::ggplot(ggplot2::aes(x = cohort, y = percent, fill = gender)) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(abs(percent), accuracy = 0.1),
                   hjust = align),
      family = "Fira Sans Light", size = 3) +
    ggplot2::annotate(
      "text",
      label = paste0("Population:\n", scales::number(total_population, big.mark = "'")),
      family = "Fira Sans Light",
      hjust = 0, vjust = .8,
      x = "100+", y = min(data_plot$percent)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = ggeo::ggeo_label_abs_percent,
      expand = c(.01,.01)
    ) +
    ggplot2::scale_fill_manual(values = c("male" = "#7294d4", "female" = "#e69fc4")) +
    theme +
    ggplot2::labs(
      x = "", y = "",
      caption = geotools::translate_enfr("Data: US Census Bureau", "Données: US Census Bureau")
    )
}

#' Create highcharter population pyramid
#'
#' @param country A string with the country name
#' @param year An integer for the year
#'
#' @return A highcharter graph
#' @export
gph_highcharter_pyramid <- function(country, year) {
  data <- geodata::gdt_idb_pyramid(country, year) |>
    tidyr::pivot_wider(names_from = "gender", values_from = "population") |>
    dplyr::arrange(dplyr::desc(age)) |>
    dplyr::mutate(age = as.character(age))

  highcharter::highchart() |>
    highcharter::hc_add_series(
      data = data,
      type = "bar",
      name = geotools::translate_enfr("Male", "Hommes"),
      highcharter::hcaes(age, male)
    ) |>
    highcharter::hc_add_series(
      data = data,
      type = "bar",
      name = geotools::translate_enfr("Female", "Femmes"),
      colorIndex = 5,
      highcharter::hcaes(age, female)
    ) |>
    highcharter::hc_xAxis(
      title = list(text = geotools::translate_enfr("Year", "Année")),
      categories = data |> dplyr::pull(age),
      labels = list(
        step = 5,
        formatter = shinyjqui::JS(glue::glue("function () {{ return ({year} - this.value) }}"))
      )
    ) |>
    highcharter::hc_yAxis(
      labels = list(
        formatter = shinyjqui::JS("function () { this.value = Math.abs(this.value)
                                                   return this.axis.defaultLabelFormatter.call(this) }")
      )
    ) |>
    highcharter::hc_plotOptions(
      series = list(stacking = "normal",
                    pointPadding = .1,
                    groupPadding = 0)
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.series.name + ', age ' + this.point.category + ', ' +
          ({year} - this.point.category) + '</b><br/>' +
          'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0); }}"))
    ) |>
    highcharter::hc_title(text = glue::glue("{countrycode::countrycode(country, 'country.name', geotools::translate_enfr('un.name.en', 'un.name.fr'))}")) |>
    highcharter::hc_subtitle(text = glue::glue("{year}"))
}

gph_ploty_pyramid <- function(country, year) {
  gph_pyramid(country, year) |>
    plotly::ggplotly() |>
    plotly::config(displayModeBar = FALSE)
}

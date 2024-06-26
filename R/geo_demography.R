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
#' @param population_color A color value for the population curve
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' gph_demogram("Switzerland")
gph_demogram <- function(country, theme = ggplot2::theme_minimal(), population_color = "blue") {
  data <- geodata::gdt_wb_demo(country)
  coef <- max(data$population, na.rm = TRUE) / max(data$birth_rate, data$death_rate, na.rm = TRUE)

  data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(date, birth_rate, color = "cbr"), linewidth = .8) +
    ggplot2::geom_line(ggplot2::aes(date, death_rate, color = "cdr"), linewidth = .8) +
    ggplot2::geom_line(ggplot2::aes(date, population / coef), color = population_color, linewidth = .8) +
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
      labels = c(
        "cbr" = geotools::gtl_translate_enfr("birth", "natalit\u00e9"),
        "cdr" = geotools::gtl_translate_enfr("death", "mortalit\u00e9")
      )
    ) +
    theme +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(color = population_color),
      legend.position = c(.05, .05),
      legend.justification = c("left", "bottom"),
      legend.box.just = "right",
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    ) +
    ggplot2::labs(
      x = "",
      y = geotools::gtl_translate_enfr("Rate (\u2030)", "Taux (\u2030)"),
      color = "",
      caption = geotools::gtl_translate_enfr("Data: World Bank", "Donn\u00e9es: Banque Mondiale")
    )
}

#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
gph_highcharter_demogram <- function(country) {
  lifecycle::deprecate_warn("1.0.0", "gph_highcharter_demogram()", "gph_hc_demogram()")
  gph_hc_demogram(country)
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
#' @examples
#' gph_hc_demogram("Switzerland")
gph_hc_demogram <- function(country) {
  data <- geodata::gdt_wb_demo(country)

  highcharter::highchart() |>
    highcharter::hc_title(text = glue::glue(geotools::gtl_translate_enfr(
      "Demograph for {country}",
      "D\u00e9mographe pour {countrycode::countryname(country, destination = 'cldr.name.fr')}"
    ))) |>
    highcharter::hc_xAxis(title = list(text = geotools::gtl_translate_enfr("Year", "Ann\u00e9e"))) -> hc

  list(
    list(min = 0, title = list(text = geotools::gtl_translate_enfr("Rate (\u2030)", "Taux (\u2030)"))),
    list(min = 0, title = list(text = "Population", style = list(color = "blue")), opposite = TRUE)
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
      name = geotools::gtl_translate_enfr("crude birth rate", "taux de natalit\u00e9"),
      color = "grey",
      dashStyle = "solid",
      tooltip = list(valueSuffix = " \u2030"),
      highcharter::hcaes(x = date, y = birth_rate)
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 0,
      name = geotools::gtl_translate_enfr("crude death rate", "taux de mortalit\u00e9"),
      color = "black",
      dashStyle = "solid",
      tooltip = list(valueSuffix = " \u2030"),
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
#' @param men Color for men line
#' @param women Color for women line
#' @param all Color for average line
#'
#' @return A ggplot2 graph
#' @export
#' @examples
#' gph_lexgram("Switzerland")
gph_lexgram <- function(country, theme = ggplot2::theme_minimal(), men = "blue", women = "red", all = "black") {
  geodata::gdt_wb_lex(country) |>
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = lex), linetype = "dotted", color = all, linewidth = .8) +
    ggplot2::geom_line(ggplot2::aes(y = lex_male), color = men, linewidth = .8) +
    ggplot2::geom_line(ggplot2::aes(y = lex_female), color = women, linewidth = .8) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      x = "",
      y = geotools::gtl_translate_enfr("Life expectancy", "Esp\u00e9rance de vie"),
      caption = geotools::gtl_translate_enfr("Data: World Bank", "Donn\u00e9es: Banque Mondiale")
    ) +
    theme
}

#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
gph_highcharter_lexgram <- function(country, men = "blue", women = "red", all = "black") {
  lifecycle::deprecate_warn("1.0.0", "gph_highcharter_lexgram()", "gph_hc_lexgram()")
  gph_hc_lexgram(country, men, women, all)
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
#' @param men Color for men line
#' @param women Color for women line
#' @param all Color for average line
#'
#' @return A highcharts graph
#' @export
#' @examples
#' gph_hc_lexgram("Switzerland")
gph_hc_lexgram <- function(country, men = "blue", women = "red", all = "black") {
  geodata::gdt_wb_lex(country) -> data

  highcharter::highchart() |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = geotools::gtl_translate_enfr(
      "Life expectancy",
      "Esp\u00e9rance de vie"
    ))) |>
    highcharter::hc_caption(text = geotools::gtl_translate_enfr(
      "Data: World Bank",
      "Donn\u00e9es: Banque Mondiale"
    )) |>
    highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = geotools::gtl_translate_enfr("Women", "Femmes"),
      color = women,
      showInLegend = FALSE,
      dashStyle = "solid",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = round(lex_female, 2))
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = geotools::gtl_translate_enfr("Everyone", "Tous"),
      color = all,
      showInLegend = FALSE,
      dashStyle = "shortdot",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = round(lex, 2))
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      name = geotools::gtl_translate_enfr("Men", "Hommes"),
      color = men,
      showInLegend = FALSE,
      dashStyle = "solid",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = round(lex_male, 2))
    )
}

#' Create population pyramid
#'
#' @param country A string with the country name
#' @param year An integer for the year
#' @param theme A ggplot2 theme
#'
#' @return A ggplot graph
#' @export
#' @examplesIf interactive()
#' # Not run: needs a valid IDB API key
#' gph_pyramid("Switzerland", 2020)
gph_pyramid <- function(country, year, theme = ggplot2::theme_minimal()) {
  country_name <- country
  pyramid_data <- geodata::gdt_idb_pyramid(country, year)

  if (geotools::gtl_opt_short_language() == "fr") {
    countrycode::countrycode(
      country, "country.name", "un.name.fr",
      custom_match = c(
        "Kosovo" = "Kosovo",
        "Gaza" = "Gaza",
        "West Bank" = "Cisjordanie"
      )
    ) -> country_name
  }

  pop_max <- max(abs(c(max(pyramid_data$population), min(pyramid_data$population))))

  pyramid_data |>
    ggplot2::ggplot(ggplot2::aes(x = age, y = population, fill = gender)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, 5), expand = c(0, 0),
      sec.axis = ggplot2::dup_axis()
    ) +
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
      caption = geotools::gtl_translate_enfr("Data: US Census Bureau", "Donn\u00e9es: US Census Bureau")
    )
}

#' Create relative population pyramid (5 year cohorts)
#'
#' @param country A string with the country name
#' @param year An integer for the year
#' @param theme A ggplot2 theme
#'
#' @return A ggplot graph
#' @export
#' @examplesIf interactive()
#' # Not run: needs a valid IDB API key
#' gph_pyramid_relative("Switzerland", 2020)
gph_pyramid_relative <- function(country, year, theme = ggplot2::theme_minimal()) {
  geodata::gdt_idb_pyramid_5y(country, year) |>
    dplyr::mutate(population = ifelse(gender == "male", population * -1, population)) |>
    dplyr::mutate(cohort = forcats::fct_inorder(cohort)) -> data

  total_population <- sum(abs(data$population))

  data |>
    dplyr::mutate(percent = population / total_population) |>
    dplyr::mutate(align = ifelse(sign(population) == 1, -0.2, 1.2)) -> data_plot

  data_plot |>
    ggplot2::ggplot(ggplot2::aes(x = cohort, y = percent, fill = gender)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(abs(percent),
          accuracy = 0.1
        ),
        hjust = align
      ),
      family = "Fira Sans Light", size = 3
    ) +
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
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_fill_manual(values = c("male" = "#7294d4", "female" = "#e69fc4")) +
    theme +
    ggplot2::labs(
      x = "", y = "",
      caption = geotools::gtl_translate_enfr("Data: US Census Bureau", "Donn\u00e9es: US Census Bureau")
    )
}

#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
gph_highcharter_pyramid <- function(country, year) {
  lifecycle::deprecate_warn("1.0.0", "gph_highcharter_pyramid()", "gph_hc_pyramid()")
  gph_hc_pyramid(country, year)
}

#' Create highcharter population pyramid
#'
#' @param country A string with the country name
#' @param year An integer for the year
#'
#' @return A highcharter graph
#' @export
#' @examplesIf interactive()
#' # Not run: need a valid IDB API key
#' gph_hc_pyramid("Switzerland", 2020)
gph_hc_pyramid <- function(country, year) {
  country -> country_name
  geodata::gdt_idb_pyramid(country, year) |>
    tidyr::pivot_wider(names_from = "gender", values_from = "population") |>
    dplyr::arrange(dplyr::desc(age)) |>
    dplyr::mutate(age = as.character(age)) -> data

  ceiling(
    max(c(max(abs(data$male)), max(data$female))) / 10000
  ) * 10000 -> pop_max

  if (geotools::gtl_opt_short_language() == "fr") {
    countrycode::countrycode(
      country, "country.name", "un.name.fr",
      custom_match = c(
        "Kosovo" = "Kosovo",
        "Gaza" = "Gaza",
        "West Bank" = "Cisjordanie"
      )
    ) -> country_name
  }

  highcharter::highchart() |>
    highcharter::hc_title(text = glue::glue("{country_name}")) |>
    highcharter::hc_subtitle(text = glue::glue("{year}")) |>
    highcharter::hc_plotOptions(
      series = list(
        stacking = "normal",
        pointPadding = .1,
        groupPadding = 0
      )
    ) -> hc

  list(
    list(
      title = list(text = geotools::gtl_translate_enfr("Year", "Ann\u00e9e")),
      categories = data |> dplyr::pull(age),
      labels = list(
        step = 5,
        formatter = shinyjqui::JS(glue::glue("function () {{ return ({year} - this.value) }}"))
      )
    ),
    list(
      opposite = TRUE,
      categories = data |> dplyr::pull(age),
      labels = list(
        step = 5,
        formatter = shinyjqui::JS(glue::glue("function () {{ return ({year} - this.value) }}"))
      ),
      linkedTo = 0
    )
  ) -> hc$x$hc_opts$xAxis

  hc |>
    highcharter::hc_add_series(
      data = data,
      type = "bar",
      name = geotools::gtl_translate_enfr("Male", "Hommes"),
      color = "#4f93b8",
      borderWidth = 0,
      highcharter::hcaes(age, male)
    ) |>
    highcharter::hc_add_series(
      data = data,
      type = "bar",
      name = geotools::gtl_translate_enfr("Female", "Femmes"),
      color = "#ad8cae",
      borderWidth = 0,
      highcharter::hcaes(age, female)
    ) |>
    highcharter::hc_yAxis(
      max = pop_max,
      min = pop_max * -1,
      labels = list(
        formatter = shinyjqui::JS("function () { this.value = Math.abs(this.value)
                                                   return this.axis.defaultLabelFormatter.call(this) }")
      )
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.series.name + ', age ' + this.point.category + ', ' +
          ({year} - this.point.category) + '</b><br/>' +
          'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0); }}"))
    )
}

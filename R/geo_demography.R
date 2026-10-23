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
      caption = geotools::gtl_translate_enfr("Data: World Bank", "Donn\u00e9es\u00a0: Banque Mondiale")
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
#' @param population_color Color of the population curve
#' @param birth_color Color of the birth rate curve
#' @param death_color Color of the death rate curve
#'
#' @return A highcharter graph
#' @export
#' @examples
#' gph_hc_demogram("Switzerland")
#' # for a dark background
#' gph_hc_demogram("Switzerland", "#6ea8fe", "#8f99a8", "#dee2e6")
gph_hc_demogram <- function(country, population_color = "blue", birth_color = "grey", death_color = "black") {
  data <- geodata::gdt_wb_demo(country)

  highcharter::highchart() |>
    highcharter::hc_title(text = glue::glue(geotools::gtl_translate_enfr(
      "Demograph for {country}",
      "D\u00e9mographe pour {countrycode::countryname(country, destination = 'cldr.name.fr')}"
    ))) |>
    highcharter::hc_xAxis(title = list(text = geotools::gtl_translate_enfr("Year", "Ann\u00e9e"))) -> hc

  list(
    list(min = 0, title = list(text = geotools::gtl_translate_enfr("Rate (\u2030)", "Taux (\u2030)"))),
    list(min = 0, title = list(text = "Population", style = list(color = population_color)), opposite = TRUE)
  ) -> hc$x$hc_opts$yAxis

  hc |>
    highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 1,
      name = "population",
      color = population_color,
      dashStyle = "solid",
      tooltip = list(valueSuffix = ""),
      highcharter::hcaes(x = date, y = population)
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 0,
      name = geotools::gtl_translate_enfr("crude birth rate", "taux de natalit\u00e9"),
      color = birth_color,
      dashStyle = "solid",
      tooltip = list(valueSuffix = " \u2030"),
      highcharter::hcaes(x = date, y = birth_rate)
    ) |>
    highcharter::hc_add_series(
      data = data,
      "line",
      yAxis = 0,
      name = geotools::gtl_translate_enfr("crude death rate", "taux de mortalit\u00e9"),
      color = death_color,
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
      caption = geotools::gtl_translate_enfr("Data: World Bank", "Donn\u00e9es\u00a0: Banque Mondiale")
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
      "Donn\u00e9es\u00a0: Banque Mondiale"
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
      caption = geotools::gtl_translate_enfr("Data: US Census Bureau", "Donn\u00e9es\u00a0: US Census Bureau")
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
      caption = geotools::gtl_translate_enfr("Data: US Census Bureau", "Donn\u00e9es\u00a0: US Census Bureau")
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
#' @examplesIf FALSE
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

#' Create highcharter population pyramid with relative values
#'
#' @param country A string with the country name
#' @param year An integer for the year
#'
#' @return A highcharter graph
#' @export
#' @examplesIf FALSE
#' # Not run: need a valid IDB API key
#' gph_hc_pyramid_relative("Switzerland", 2020)
gph_hc_pyramid_relative <- function(country, year) {
  country -> country_name
  geodata::gdt_idb_pyramid_5y(country, year) |>
    dplyr::mutate(population = ifelse(gender == "male", population * -1, population)) |>
    dplyr::mutate(cohort = forcats::fct_inorder(cohort)) -> data

  total_population <- sum(abs(data$population))

  data |>
    tidyr::pivot_wider(names_from = "gender", values_from = "population") |>
    dplyr::mutate(
      male_percent = male / total_population * 100,
      female_percent = female / total_population * 100
    ) |>
    dplyr::arrange(dplyr::desc(sort)) -> plot_data

  ceiling(
    max(c(max(abs(plot_data$male_percent)), max(plot_data$female_percent))) * 2
  ) / 2 -> pop_max

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
      categories = data |> dplyr::pull(cohort) |> rev()
    ),
    list(
      opposite = TRUE,
      categories = data |> dplyr::pull(cohort) |> rev(),
      linkedTo = 0
    )
  ) -> hc$x$hc_opts$xAxis

  hc |>
    highcharter::hc_add_series(
      data = plot_data,
      type = "bar",
      name = geotools::gtl_translate_enfr("Male", "Hommes"),
      color = "#4f93b8",
      borderWidth = 0,
      highcharter::hcaes(cohort, male_percent)
    ) |>
    highcharter::hc_add_series(
      data = plot_data,
      type = "bar",
      name = geotools::gtl_translate_enfr("Female", "Femmes"),
      color = "#ad8cae",
      borderWidth = 0,
      highcharter::hcaes(cohort, female_percent)
    ) |>
    highcharter::hc_yAxis(
      max = pop_max,
      min = pop_max * -1,
      labels = list(
        formatter = shinyjqui::JS("function () { this.value = Math.abs(this.value)
                                                   return this.axis.defaultLabelFormatter.call(this) + '\u00a0%' }")
      )
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
          'Population: ' + Highcharts.numberFormat(Math.abs(
            this.point[this.series.name.toLowerCase()]
          ), 0) + '<br/>' +
          'Pop relative: ' + Highcharts.numberFormat(Math.abs(this.point.y), 2) + '\u00a0%'; }}"))
    )
}

#' A dashed vertical line with a label, for the events on a time axis
#'
#' @param year The year of the event
#' @param label The label of the event
#' @param y The vertical offset of the label (px)
#'
#' @return A list for `highcharter::hc_xAxis(plotLines = ...)`
#' @keywords internal
#' @noRd
hc_event_line <- function(year, label, y = 20) {
  list(
    value = year, color = "#3a3e3f88", dashStyle = "Dash", width = 1, zIndex = 3,
    label = list(text = label, rotation = 0, y = y, style = list(fontSize = "15px", color = "#3a3e3f"))
  )
}

#' Event lines from a named vector of years
#'
#' @param events A named numeric vector: names are the labels, values the years
#'
#' @return A list of plotLines, the labels staggered in height
#' @keywords internal
#' @noRd
hc_event_lines <- function(events) {
  if (is.null(events) || length(events) == 0) {
    return(list())
  }
  purrr::imap(unname(as.list(events)), \(year, i) {
    hc_event_line(year, names(events)[i], y = if (i %% 2 == 1) 40 else 70)
  })
}

#' Total fertility rate chart
#'
#' The total fertility rate (children per woman) of one or more countries since 1960,
#' with the replacement level (2.1). One country is drawn as an area, several as lines.
#'
#' @param countries A character vector of country names or iso codes
#' @param events A named numeric vector of years to mark with a dashed line,
#'   e.g. `c("one child" = 1980)`
#' @param periods A named list of year ranges to shade, e.g. `list(famine = c(1959, 1961))`
#' @param highlight Countries (as in `countries`) drawn with a thicker line
#' @param colors Colours of the countries, in the order of `countries`
#'
#' @return A highcharter graph
#' @export
#' @examplesIf interactive()
#' gph_hc_tfr("China", events = c("one child" = 1980), periods = list(famine = c(1959, 1961)))
#' gph_hc_tfr(c("France", "Germany", "Italy"), highlight = "France")
gph_hc_tfr <- function(countries, events = NULL, periods = NULL, highlight = NULL,
                       colors = c("#c03728", "#8a93a3", "#d9a441", "#919c4c", "#5b7fa6", "#6b452b", "#2b323f")) {
  to_iso3 <- function(x) {
    codes <- countrycode::countrycode(x, "country.name", "iso3c", warn = FALSE)
    ifelse(is.na(codes), toupper(x), codes)
  }
  codes <- to_iso3(countries)
  highlight <- if (is.null(highlight)) character(0) else to_iso3(highlight)
  data <- geodata::gdt_wb_tfr(codes) |>
    dplyr::filter(!is.na(tfr)) |>
    dplyr::mutate(tfr = round(tfr, 2))
  lang <- geotools::gtl_opt_short_language(valid = c("en", "fr"))
  names <- countrycode::countrycode(codes, "iso3c", paste0("cldr.name.", lang))

  bands <- purrr::imap(unname(periods), \(range, i) {
    list(
      from = range[1], to = range[2], color = "#c0372822",
      label = list(text = names(periods)[i], style = list(fontSize = "14px"))
    )
  })

  hc <- highcharter::highchart()
  for (i in seq_along(codes)) {
    country_data <- dplyr::filter(data, iso3c == codes[i])
    hc <- if (length(codes) == 1) {
      highcharter::hc_add_series(hc, country_data, "area", highcharter::hcaes(x = date, y = tfr),
        name = geotools::gtl_translate_enfr("Children per woman (TFR)", "Enfants par femme (ISF)"),
        color = colors[i], fillOpacity = .25, lineWidth = 3
      )
    } else {
      highcharter::hc_add_series(hc, country_data, "line", highcharter::hcaes(x = date, y = tfr),
        name = names[i], color = colors[i], lineWidth = if (codes[i] %in% highlight) 5 else 2.5
      )
    }
  }

  hc |>
    highcharter::hc_xAxis(title = list(text = NULL), plotBands = bands, plotLines = hc_event_lines(events)) |>
    highcharter::hc_yAxis(
      title = list(text = geotools::gtl_translate_enfr("children per woman", "enfants par femme")),
      min = 0,
      plotLines = list(list(
        value = 2.1, color = "#c03728", dashStyle = "ShortDot", width = 2, zIndex = 3,
        label = list(
          text = geotools::gtl_translate_enfr("replacement level: 2.1", "seuil de remplacement\u00a0: 2,1"),
          align = "right", x = -10, style = list(fontSize = "14px", color = "#c03728")
        )
      ))
    ) |>
    highcharter::hc_tooltip(shared = TRUE) |>
    highcharter::hc_legend(enabled = length(codes) > 1) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_caption(text = paste0(
      "Data: <a target='_blank' href='https://data.worldbank.org/indicator/SP.DYN.TFRT.IN'>",
      "World Bank</a>"
    ))
}

#' Working-age population chart
#'
#' The population aged 15 to 64 of a country since 1990, in millions, with its peak.
#'
#' @param country A string with the country name or iso code
#' @param start The first year
#' @param color Colour of the curve
#' @param peak_color Colour of the peak and its label
#'
#' @return A highcharter graph
#' @export
#' @examplesIf interactive()
#' gph_hc_working_age("China")
gph_hc_working_age <- function(country, start = 1990, color = "#5b7fa6", peak_color = "#c03728") {
  code <- countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE)
  data <- geodata::gdt_wb_working_age(if (is.na(code)) country else code, start = start) |>
    dplyr::filter(!is.na(working_age)) |>
    dplyr::mutate(millions = round(working_age / 1e6))
  peak <- data[which.max(data$working_age), ]
  lang <- geotools::gtl_opt_short_language(valid = c("en", "fr"))
  name <- countrycode::countrycode(peak$iso3c, "iso3c", paste0("cldr.name.", lang))

  highcharter::highchart() |>
    highcharter::hc_add_series(data, "area", highcharter::hcaes(x = date, y = millions),
      name = geotools::gtl_translate_enfr("People aged 15\u201364", "Personnes de 15 \u00e0 64 ans"),
      color = color, fillOpacity = .25, lineWidth = 3, marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_add_series(peak, "scatter", highcharter::hcaes(x = date, y = millions),
      name = "peak", color = peak_color, marker = list(radius = 6, symbol = "circle"),
      enableMouseTracking = FALSE,
      dataLabels = list(
        enabled = TRUE,
        format = geotools::gtl_translate_enfr(
          "peak {point.x}: {point.y} million", "pic {point.x}\u00a0: {point.y} millions"
        ),
        style = list(fontSize = "14px", color = peak_color), y = -8
      )
    ) |>
    highcharter::hc_title(text = glue::glue(geotools::gtl_translate_enfr(
      "People aged 15\u201364 in {name}", "Personnes de 15 \u00e0 64 ans\u00a0: {name}"
    ), name = name), style = list(fontSize = "16px")) |>
    highcharter::hc_xAxis(title = list(text = NULL)) |>
    highcharter::hc_yAxis(title = list(text = "millions"), min = floor(min(data$millions) * .8 / 100) * 100) |>
    highcharter::hc_tooltip(
      valueSuffix = geotools::gtl_translate_enfr(" million", " millions"),
      headerFormat = "<b>{point.x}</b><br>"
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_credits(enabled = TRUE, text = "Data: World Bank")
}

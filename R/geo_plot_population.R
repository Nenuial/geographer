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

  data %>%
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
  geodata::gdt_wb_lex(country) %>%
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
  geodata::gdt_idb_pyramid(country, year) %>%
    ggplot2::ggplot(ggplot2::aes(x = age, y = population, fill = gender)) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 5), expand = c(0, 0),
                                sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_y_continuous(
      breaks = ggeo::ggeo_remove_breaks(scales::breaks_pretty(6), list(0)),
      labels = ggeo::ggeo_label_pyramid
    ) +
    ggplot2::scale_fill_manual(values = c("male" = "#7294d4", "female" = "#e69fc4")) +
    theme +
    ggplot2::labs(
      x = "", y = "",
      caption = geotools::translate_enfr("Data: US Census Bureau", "Données: US Census Bureau")
    )
}

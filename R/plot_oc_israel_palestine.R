#' OC Russie: graphique des naissances
#'
#' Graphique des naissances en Russie
#' jusqu'en 2017 (pour alignement avec avortements).
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 graph
#' @export
oc_israel_palestine_migration_israel <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_israel_palestine_immigration_israel |>
    ggplot2::ggplot(ggplot2::aes(year, immigration)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2015, 5)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
    ggplot2::labs(
      title = "Immigration en Israël depuis 1948",
      x = "", y = ""
    ) +
    theme
}

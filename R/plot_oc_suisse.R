#' OC Suisse: graphique de l'immigration italienne
#'
#' Une carte de l'introduction
#'
#' @param theme A ggplot2 theme
#'
#' @return ggplot2 graph
#' @concept oc suisse population immigration
#'
#' @export
#' @examples
#' oc_suisse_graph_2021_immigration_italienne()
oc_suisse_graph_2021_immigration_italienne <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_suisse_2021_immigration_italienne %>%
    ggplot2::ggplot(ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = population), color = "blue") +
    ggplot2::geom_col(ggplot2::aes(y = immigration * 10), fill = "orange") +
    ggplot2::scale_y_continuous(
      name = "Population (milliers)",
      expand = ggplot2::expansion(add = c(0, 10000)),
      sec.axis = ggplot2::sec_axis(
        trans = ~ . / 10,
        name = "Immigration (milliers)",
        labels = scales::number_format(scale = .001)
      ),
      labels = scales::number_format(scale = .001)
    ) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    theme +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(color = "blue"),
      axis.title.y.right = ggplot2::element_text(color = "orange")
    ) +
    ggplot2::labs(
      title = "Population et immigration italienne en Suisse",
      caption = "Donn\u00e9es: OFS (2021)",
      x = ""
    )
}

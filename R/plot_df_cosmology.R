# Hubble -------------------------------------------------------------------------------------

#' Hubble's detective work
#'
#' Velocity against distance of the 24 nebulae of Hubble (1929), printed for
#' a figure 17 × 11 cm. The student version is an empty grid to plot the
#' points by hand (distance 0 to 2.2 million parsecs, velocity -400 to
#' 1200 km/s, a fine grid every 0.1 and 100). The key version adds the points,
#' the least-squares line through the origin (K = Σrv / Σr² ≈ 424 km/s per
#' million parsecs) and Hubble's rounded K ≈ 500 (dashed).
#'
#' @param key `TRUE` for the answer key: points and lines
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 graph
#' @export
#'
#' @examples
#' df_cosmology_graph_hubble()
#' df_cosmology_graph_hubble(key = TRUE)
df_cosmology_graph_hubble <- function(key = FALSE, theme = ggplot2::theme_minimal()) {
  ink <- "#333333"
  data <- geodata::df_cosmology_hubble1929

  p <- ggplot2::ggplot(data, ggplot2::aes(r_mpc, v_kms)) +
    ggplot2::geom_hline(yintercept = 0, color = "#8c8c8c", linewidth = 0.4)

  if (key) {
    k_fit <- sum(data$r_mpc * data$v_kms) / sum(data$r_mpc^2)
    # labels as plotmath: the approximately-equal sign then comes from the
    # symbol font, which every pdf device can draw
    lines <- tibble::tibble(
      k = c(k_fit, 500),
      label = c(paste("K %~~%", round(k_fit)), "Hubble*':'~K %~~% 500"),
      color = c("#1f3a5f", "#c0392b"),
      linetype = c("solid", "22"),
      # in the open area between the points: the fit below its line,
      # Hubble's value above its line
      x = c(1.75, 1.55),
      vjust = c(1.6, -0.6)
    )
    p <- p +
      ggplot2::geom_abline(
        data = lines,
        ggplot2::aes(slope = k, intercept = 0, color = I(color), linetype = I(linetype)),
        linewidth = 0.7
      ) +
      # white background: the lines pass behind the labels
      ggplot2::geom_label(
        data = lines,
        ggplot2::aes(x = x, y = k * x, label = label, color = I(color), vjust = vjust),
        parse = TRUE, hjust = 0.5, size = 3.2, fill = "white", label.size = 0
      ) +
      ggplot2::geom_point(shape = 21, size = 2, fill = ink, color = "white", stroke = 0.4)
  }

  p +
    ggplot2::scale_x_continuous(
      name = "Distance (million parsecs)",
      limits = c(0, 2.2),
      breaks = seq(0, 2.2, by = 0.2),
      minor_breaks = seq(0, 2.2, by = 0.1),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      name = "Velocity (km/s)",
      limits = c(-400, 1200),
      breaks = seq(-400, 1200, by = 200),
      minor_breaks = seq(-400, 1200, by = 100),
      expand = c(0, 0)
    ) +
    ggplot2::labs(caption = "Data: Hubble (1929), Table 1") +
    theme +
    # the grid is the exercise: drawn whatever the theme
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "#bdbdbd", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "#e4e4e4", linewidth = 0.2),
      panel.border = ggplot2::element_rect(fill = NA, color = ink, linewidth = 0.4)
    )
}

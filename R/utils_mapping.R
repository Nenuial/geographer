#' Generate world map
#'
#' @param df A dataframe with data breaks in the cut column
#' @param title The title of the map
#' @param year The year of the map
#' @param unit The units
#' @param palette The legend colors
#' @param caption The data source
#'
#' @return A ggplot
#' @export
geo_plot_world <- function(df, title, year, unit, palette, caption) {
  rnaturalearth::ne_countries(returnclass = "sf") %>%
    dplyr::filter(adm0_a3 != "ATA") %>%
    dplyr::left_join(df, by = c("iso_a3" = "iso3c")) -> df_plot

  df_plot %>%
    ggplot2::ggplot(ggplot2::aes(fill = cut)) +
    ggplot2::geom_sf(lwd = .4) +
    ggplot2::scale_fill_manual(
      values = scales::alpha(colour = palette,
                             alpha = geo_pkg_options("opacity")),
      drop = FALSE,
      na.value = scales::alpha(colour = "#4d4d4d",
                               alpha = geo_pkg_options("opacity")),
      labels = c(glue::glue("{levels(df$cut)} {unit}"),
                 translate("No data", "Pas de données")),
      guide = ggplot2::guide_legend(
        title = NULL,
        direction = "vertical",
        lwd = .4,
        keyheight = unit(6, units = "mm"),
        keywidth = unit(1.2, units = "cm"),
        label.hjust = 0,
        label.vjust = .5,
        reverse = T,
        label.position = "right"
      )) +
    ggplot2::theme(legend.position = c(.98, .5)) +
    ggeo::ggeotheme(theme = geo_pkg_options("theme"),
                    mode = geo_pkg_options("mode")) +
    ggplot2::coord_sf(crs = "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", datum = NA) +
    ggplot2::labs(title = stringr::str_to_upper(title),
                  subtitle = year,
                  fill = "",
                  caption = glue::glue("{translate('Data:', 'Données :')} {caption}"))
}

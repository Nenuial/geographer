# Avortement ----------------------------------------------------------------------------------

#' Avortement
#'
#' Cartes des droits en matière d'avortement ainsi que
#' des taux d'avortement.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_avortements
#' @return A map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_hc_abortion_rights()
#' oc_geo_au_feminin_carte_abortion_rights()
#' oc_geo_au_feminin_carte_hc_taux_avortement()
#' oc_geo_au_feminin_carte_taux_avortement()
oc_geo_au_feminin_carte_hc_abortion_rights <- function() {
  geodata::gdt_crr_get_data() |>
    tidyr::unnest_auto(category_id) |>
    dplyr::mutate(name = as.character(name)) |>
    dplyr::mutate(iso3c = countrycode::countrycode(name, "country.name", "iso3c")) |>
    dplyr::select(name, iso3c, gest = category_id) |>
    dplyr::mutate(value = 1) -> data

  # 1348 ~ "Totalement interdit",
  # 1349 ~ "Pour sauver la vie de la femme",
  # 1350 ~ "Pour raisons de santé",
  # 1351 ~ "Pour motifs socio-économiques",
  # 1352 ~ "Sur demande (avancement de la grossesse variable)"


  map <- geojsonio::geojson_read(
    "https://code.highcharts.com/mapdata/custom/world-highres2.topo.json"
  )

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Les l\u00e9gislations sur l'avortement dans le monde") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://reproductiverights.org/maps/worlds-abortion-laws/'
      target='_blank'>Center for reproduction rights (2023)</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_plotOptions(map = list(
      allAreas = FALSE,
      joinBy = c("iso.a3", "iso3c"),
      mapData = map,
      showInLegend = FALSE
    )) |>
    highcharter::hc_add_series(
      name = "Totalement interdit",
      data = data |> dplyr::filter(gest == 1348),
      color = "#950023",
      value = "value",
      borderColor = "transparent"
    ) |>
    highcharter::hc_add_series(
      name = "Sauver la vie de la femme",
      data = data |> dplyr::filter(gest == 1349),
      color = "#da7c59",
      value = "value",
      borderColor = "transparent"
    ) |>
    highcharter::hc_add_series(
      name = "Raisons de sant\u00e9",
      data = data |> dplyr::filter(gest == 1350),
      color = "#f1c936",
      value = "value",
      borderColor = "transparent"
    ) |>
    highcharter::hc_add_series(
      name = "Motifs socio-\u00e9conomiques",
      data = data |> dplyr::filter(gest == 1351),
      color = "#bbdb8e",
      value = "value",
      borderColor = "transparent"
    ) |>
    highcharter::hc_add_series(
      name = "Sur demande",
      data = data |> dplyr::filter(gest == 1352),
      color = "#5c9099",
      value = "value",
      borderColor = "transparent"
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.name + '</b><br/>'
          + this.point.series.name }}"))
    )
}

#' @rdname oc_geo_au_feminin_cartes_avortements
#' @export
oc_geo_au_feminin_carte_abortion_rights <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_crr_get_data() |>
    tidyr::unnest_auto(category_id) |>
    dplyr::mutate(name = as.character(name)) |>
    dplyr::mutate(iso3c = countrycode::countrycode(name, "country.name", "iso3c")) |>
    dplyr::select(name, iso3c, gest = category_id) |>
    dplyr::mutate(
      gest_lab = dplyr::case_match(
        gest,
        1348 ~ "Totalement interdit",
        1349 ~ "Sauver la vie de la femme",
        1350 ~ "Raisons de sant\u00e9",
        1351 ~ "Motifs socio-\u00e9conomiques",
        1352 ~ "Sur demande"
      )
    ) |>
    dplyr::mutate(gest_lab = forcats::as_factor(gest_lab)) |>
    dplyr::mutate(gest_lab = forcats::fct_reorder(
      gest_lab,
      gest
    )) -> data

  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica") |>
    dplyr::left_join(data,
      by = c("adm0_a3" = "iso3c")
    ) -> plot_data


  plot_data |>
    ggplot2::ggplot(ggplot2::aes(fill = forcats::fct_rev(gest_lab))) +
    ggplot2::geom_sf(size = .05) +
    ggplot2::coord_sf(crs = "+proj=eqearth", datum = NA) +
    ggplot2::labs(fill = "") +
    ggplot2::scale_fill_manual(
      values = c(
        "#5c9099",
        "#bbdb8e",
        "#f1c936",
        "#da7c59",
        "#950023"
      ),
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "Pas de donn\u00e9es"
        breaks
      },
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Les l\u00e9gislations sur l'avortement dans le monde",
      caption = "Source : Center for reproduction rights (2023)"
    )
}

#' @rdname oc_geo_au_feminin_cartes_avortements
#' @export
oc_geo_au_feminin_carte_hc_taux_avortement <- function() {
  geodata::gdt_gutt_get_abortion_rates() |>
    dplyr::rename(value = abortion_rate) |>
    dplyr::mutate(
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name.fr")
    ) -> data

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  palette <- function(...) {
    viridis::magma(direction = -1, alpha = .8, ...)
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Taux d'avortement") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://www.guttmacher.org' target='_blank'>Guttmacher Institute (2015-2019)</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "Avortement 2015-2019",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso3"),
      nullColor = "#CCCCCC",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country_name + '</b><br/>' +
          'Taux: ' + this.point.value }}"))
    )
}

#' @rdname oc_geo_au_feminin_cartes_avortements
#' @export
oc_geo_au_feminin_carte_taux_avortement <- function(theme = ggplot2::theme_minimal()) {
  geodata::gdt_gutt_get_abortion_rates() |>
    dplyr::mutate(
      country_name = countrycode::countrycode(iso3, "iso3c", "country.name.fr")
    ) |>
    dplyr::mutate(value = santoku::chop_equally(abortion_rate, 6)) -> data

  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica") |>
    dplyr::left_join(data,
      by = c("adm0_a3" = "iso3")
    ) -> plot_data


  plot_data |>
    ggplot2::ggplot(ggplot2::aes(fill = value)) +
    ggplot2::geom_sf(size = .05) +
    ggplot2::coord_sf(crs = "+proj=eqearth", datum = NA) +
    ggplot2::scale_fill_manual(
      values = viridis::magma(6, direction = -1),
      na.value = "#CCCCCC"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorsteps()
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Taux d'avortement",
      fill = "",
      caption = "Source : Guttmacher Institute (2015-2019)"
    )
}


# Contraception -------------------------------------------------------------------------------

#' Contraception
#'
#' Carte mondiale intéractive de la prévalence de la
#' contraception.
#'
#' @return A highcharts map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_hc_contraception()
oc_geo_au_feminin_carte_hc_contraception <- function() {
  palette <- function(...) {
    viridis::viridis(direction = -1, ...)
  }

  data <- wbstats::wb_data(
    indicator = c("value" = "SP.DYN.CONU.ZS"),
    start_date = 1974,
    end_date = 2021
  ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::filter(date == max(date), .by = "iso3c") -> data
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "L'usage de la contraception") |>
    highcharter::hc_subtitle(text = "% des femmes \u00e2g\u00e9es de 15 \u00e0 49 ans") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://donnees.banquemondiale.org/indicateur/SP.DYN.CONU.ZS'
      target='_blank'>Banque Mondiale</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "Contraception 2022",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso3c"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          'Pr\u00e9valence ('+ this.point.date + '): ' + this.point.value }}"))
    )
}


# Espérance de vie femmes/hommes --------------------------------------------------------------

#' Filter WPP Lex data for graph
#'
#' @param data WPP Lex data to be filtered
#' @param filter Period filter
#'
#' @return A tibble
#' @keywords internal
oc_geo_au_feminin_carte_base_donnees_wpp <- function(data, filter) {
  data |>
    dplyr::filter(period == filter) |>
    dplyr::mutate(data = santoku::chop(lexdelta, c(-0.2, 2, 4, 6, 8),
      extend = TRUE, drop = FALSE
    )) |>
    dplyr::mutate(adm0_a3 = countrycode::countrycode(un_code, "un", "iso3c", warn = FALSE))
}

#' Base plot for WPP map
#'
#' @param data WPP Lex data
#'
#' @return A ggplot2 map
#' @keywords internal
oc_geo_au_feminin_carte_base_carte_wpp <- function(data) {
  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica") |>
    dplyr::left_join(data,
      by = "adm0_a3"
    ) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    ggfx::with_shadow(
      ggplot2::geom_sf(ggplot2::aes(fill = data),
        color = "#fffeea", size = .1
      ),
      colour = "#c6c6c5",
      x_offset = 5,
      y_offset = 5,
      sigma = 4
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_proj("eqearth"), datum = NA) +
    ggplot2::scale_fill_manual(
      values = c(
        "#8e2d41", "#ce5f29", "#ed9a2c",
        "#f4da66", "#94bbb5", "#4b9593"
      ),
      breaks = levels(plot_data$data),
      limits = levels(plot_data$data)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        title.position = "top",
        even.steps = TRUE, show.limits = FALSE,
        barwidth = 30
      )
    )
}

#' Différence d'espérance de vie
#'
#' Cartes pour mettre en évidence les différences
#' d'espérence de vie entre hommes et femmes.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_esperance_de_vie
#' @return A map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_esperance_de_vie_wpp2012_2005_a_2010()
#' oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2005_a_2010()
#' oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2015_a_2020()
oc_geo_au_feminin_carte_esperance_de_vie_wpp2012_2005_a_2010 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2012_lex,
    "2005-2010"
  ) |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Diff\u00e9rence d'esp\u00e9rance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2005-2010)",
      caption = "Donn\u00e9es : WPP2012",
      fill = "Diff\u00e9rence (ann\u00e9es)"
    )
}

#' @rdname oc_geo_au_feminin_cartes_esperance_de_vie
#' @export
oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2005_a_2010 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2019_lex,
    "2005-2010"
  ) |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Diff\u00e9rence d'esp\u00e9rance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2005-2010)",
      caption = "Donn\u00e9es : WPP2019",
      fill = "Diff\u00e9rence (ann\u00e9es)"
    )
}

#' @rdname oc_geo_au_feminin_cartes_esperance_de_vie
#' @export
oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2015_a_2020 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2019_lex,
    "2015-2020"
  ) |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(7)[6:1]
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Diff\u00e9rence d'esp\u00e9rance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2015-2020)",
      caption = "Donn\u00e9es : WPP2019",
      fill = "Diff\u00e9rence (ann\u00e9es)"
    )
}


# Géopolitique --------------------------------------------------------------------------------

#' Géopolitique de l'Afghanistan
#'
#' Cartes qui permettent de retracer l'évolution géopolitique
#' de l'Afghanistan depuis 1912.
#'
#' Carte disponibles:
#' * 1912: `oc_geo_au_feminin_carte_afghanistan_independance()`
#' * 1950: `oc_geo_au_feminin_carte_afghanistan()`
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_afghanistan
#' @return A ggplot2 map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_afghanistan_independance()
#' oc_geo_au_feminin_carte_afghanistan()
oc_geo_au_feminin_carte_afghanistan_independance <- function(theme = ggplot2::theme_minimal()) {
  crs <- "+proj=laea +lat_0=31 +lon_0=69 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

  # Matrix as LonMin, LonMax, LatMin, LatMax
  box <- sf::sf_project(
    from = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", to = crs,
    pts = matrix(c(50.75, 103.62, 17.18, 44.21), ncol = 2)
  )

  gph_historical_world_map(lubridate::ymd("1919-12-31")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = rnaturalearth::ne_download(
        scale = 50, category = "physical", type = "land",
        returnclass = "sf"
      ),
      fill = "#3a3a3f", color = "#fffeea", size = .1
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = country_name),
      color = "#fffeea", size = .1,
      show.legend = FALSE
    ) +
    gghighlight::gghighlight(
      gwcode %in% c( # Start selection
        365, # URSS
        700, # Afghanistan
        750 # British India
      ), # End selection
      unhighlighted_params = list(fill = "#3a3a3f")
    ) +
    ggplot2::scale_fill_manual(
      values = c("#d6eefa", "#8ed6f3", "#e73041"),
    ) +
    ggplot2::coord_sf(
      crs = crs,
      datum = NA,
      xlim = box[, 1],
      ylim = box[, 2]
    ) +
    theme +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#1786ab"))
}

#' @rdname oc_geo_au_feminin_cartes_afghanistan
#' @export
oc_geo_au_feminin_carte_afghanistan <- function(theme = ggplot2::theme_minimal()) {
  crs <- "+proj=laea +lat_0=31 +lon_0=69 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

  # Matrix as LonMin, LonMax, LatMin, LatMax
  box <- sf::sf_project(
    from = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", to = crs,
    pts = matrix(c(50.75, 103.62, 17.18, 44.21), ncol = 2)
  )

  gph_historical_world_map(lubridate::ymd("1950-01-01")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = rnaturalearth::ne_download(
        scale = 50, category = "physical", type = "land",
        returnclass = "sf"
      ),
      fill = "#3a3a3f", color = "#fffeea", size = .1
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = country_name),
      color = "#fffeea", size = .1,
      show.legend = FALSE
    ) +
    gghighlight::gghighlight(
      gwcode %in% c( # Start selection
        365, # URSS
        700, # Afghanistan
        710, # China
        711, # Tibet (China)
        750, # British India
        770 # Pakistan
      ), # End selection
      unhighlighted_params = list(fill = "#3a3a3f")
    ) +
    ggplot2::scale_fill_manual(
      values = c("#e2e2e2", "#ec775d", "#d0c978", "#f5eb73", "#e82f42", "#ec775d"),
    ) +
    ggplot2::coord_sf(
      crs = crs,
      datum = NA,
      xlim = box[, 1],
      ylim = box[, 2]
    ) +
    theme +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#1786ab"))
}

# Mesure d'inégalité --------------------------------------------------------------------------

#' Mesure d'inégalité
#'
#' Ces différentes cartes présente plusieurs indicateurs de
#' l'inégalité entre hommes et femmes.
#'
#' Indicateurs à disposition:
#' * SIGI 2023: `oc_geo_au_feminin_carte_sigi_2023()` et `oc_geo_au_feminin_carte_hc_sigi_2023()`
#' * GII 2021: `oc_geo_au_feminin_carte_hc_gii_2021()`
#' * WB WBL 2022: `oc_geo_au_feminin_carte_hc_wbl()`
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_inegalites
#' @return A map
#' @export
#' @examples
#' # Sigi 2023
#' oc_geo_au_feminin_carte_sigi_2023()
#' oc_geo_au_feminin_carte_hc_sigi_2023()
#'
#' # GII 2021
#' oc_geo_au_feminin_carte_hc_gii_2021()
#'
#' # WB WBL 2022
#' oc_geo_au_feminin_carte_hc_wbl()
oc_geo_au_feminin_carte_sigi_2023 <- function(theme = ggplot2::theme_minimal()) {
  geodata::oc_geo_au_feminin_2023_oecd_sigi |>
    dplyr::filter(VAR == "SIGI") -> map_data

  rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    dplyr::left_join(map_data, dplyr::join_by(adm0_a3 == LOCATION)) |>
    ggplot2::ggplot(ggplot2::aes(fill = Value)) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = "+proj=eqearth") +
    ggplot2::scale_fill_viridis_b() +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "L'indice Institutions sociales et \u00e9galit\u00e9 des genres",
      subtitle = "0 = pas de discrimination ; 100 = discrimination absolue, 2023",
      fill = "",
      caption = "Source OCDE (2023), SIGI"
    )
}

#' @rdname oc_geo_au_feminin_cartes_inegalites
#' @export
oc_geo_au_feminin_carte_hc_sigi_2023 <- function() {
  data <- geodata::oc_geo_au_feminin_2023_oecd_sigi |>
    dplyr::filter(VAR == "SIGI") |>
    dplyr::rename(value = Value)
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "L'indice Institutions sociales et \u00e9galit\u00e9 des genres") |>
    highcharter::hc_subtitle(text = "0 = pas de discrimination ; 100 = discrimination absolue, 2023") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://www.oecd.org/stories/gender/social-norms-and-gender-discrimination/sigi'
      target='_blank'>OCDE (2023)</a>,
      <a href='https://www.oecd.org/stories/gender/social-norms-and-gender-discrimination/sigi'
      target='_blank'>SIGI</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "SIGI 2023",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "LOCATION"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), viridis::viridis
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.Country + '</b><br/>' +
          'SIGI: ' + this.point.value }}"))
    )
}

#' @rdname oc_geo_au_feminin_cartes_inegalites
#' @export
oc_geo_au_feminin_carte_hc_gii_2021 <- function() {
  data <- geodata::oc_geo_au_feminin_2021_unhdr_gii |>
    dplyr::rename(value = gii)
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Indice d'in\u00e9galit\u00e9 de genre") |>
    highcharter::hc_caption(
      text = "Source :
      <a href='https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII'
      target='_blank'>UN HDR (2021)</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "GII 2021",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), viridis::viridis
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          'GII: ' + this.point.value }}"))
    )
}

#' @rdname oc_geo_au_feminin_cartes_inegalites
#' @export
oc_geo_au_feminin_carte_hc_wbl <- function() {
  palette <- function(...) {
    viridis::viridis(direction = -1, ...)
  }

  data <- wbstats::wb_data(
    indicator = c("value" = "SG.LAW.INDX"),
    start_date = 2022,
    end_date = 2022
  )
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Women, Business and the Law") |>
    highcharter::hc_caption(text = "Source : <a href='https://donnees.banquemondiale.org/indicateur/SG.LAW.INDX'
                            target='_blank'>Banque Mondiale (2022)</a>") |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "WBL 2022",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso3c"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          'WBL: ' + this.point.value }}"))
    )
}


# Politique -----------------------------------------------------------------------------------

#' Représentation politique
#'
#' Carte interactive du nombre de femmes au parlement.
#'
#' @return A highcharts map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_hc_femmes_parlement()
oc_geo_au_feminin_carte_hc_femmes_parlement <- function() {
  wbstats::wb_data(
    indicator = c("value" = "SG.GEN.PARL.ZS"),
    start_date = 2022,
    end_date = 2022
  ) |>
    dplyr::mutate(value = round(value, 2)) -> data

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  palette <- function(...) {
    viridis::turbo(direction = -1, alpha = .8, ...)
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Taux de femmes dans les parlements") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://donnees.banquemondiale.org/indicateur/SG.GEN.PARL.ZS'
      target='_blank'>Banque Mondiale (2022)</a>"
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
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso3c"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 6), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.country + '</b><br/>' +
          'Taux: ' + this.point.value }}"))
    )
}

# Sex-ratio -----------------------------------------------------------------------------------

#' Sex ratio à la naissance
#'
#' Cartes du sex ratio à la naissance.
#'
#' @param year The year
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_sex_ratio
#' @return A ggplot2 map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_sex_ratio(2015)
#' oc_geo_au_feminin_carte_hc_sex_ratio()
oc_geo_au_feminin_carte_sex_ratio <- function(year, theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(
    indicator = c("ratio" = "SP.POP.BRTH.MF"),
    start_date = year,
    end_date = year
  ) |>
    dplyr::mutate(data = santoku::chop(ratio, c(1.05, 1.10, 1.15), extend = TRUE, drop = FALSE)) -> data

  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica") |>
    dplyr::left_join(data,
      by = c("adm0_a3" = "iso3c")
    ) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    ggfx::with_shadow(
      ggplot2::geom_sf(ggplot2::aes(fill = data),
        color = "#fffeea", size = .1
      ),
      colour = "#c6c6c5",
      x_offset = 5,
      y_offset = 5,
      sigma = 4
    ) +
    ggplot2::coord_sf(crs = geotools::gtl_crs_proj("eqearth"), datum = NA) +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(4),
      breaks = levels(plot_data$data),
      limits = levels(plot_data$data),
      drop = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_coloursteps(
        title.position = "top",
        even.steps = TRUE, show.limits = FALSE,
        barwidth = 30
      )
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Sex ratio \u00e0 la naissance",
      subtitle = glue::glue("Ann\u00e9e {year}"),
      fill = "Naissances gar\u00e7ons/filles",
      caption = "Donn\u00e9es : Banque Mondiale"
    )
}

#' @rdname oc_geo_au_feminin_cartes_sex_ratio
#' @export
oc_geo_au_feminin_carte_hc_sex_ratio <- function() {
  geodata::oc_geo_au_feminin_sex_ratio |>
    dplyr::filter(Year == 2022) |>
    dplyr::rename(value = SRB) -> data

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  palette <- function(...) {
    paletteer::paletteer_c("grDevices::RdBu", n = 12) |>
      rev() -> colors

    return(colors[c(6:12)])
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Sex-ratio \u00e0 la naissance") |>
    highcharter::hc_caption(
      text = "Source : <a href='https://population.un.org/wpp/' target='_blank'>UN WPP (2022)</a>"
    ) |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "Sex-ratio 2022",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "iso"),
      nullColor = "#838a8f",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop(data$value, c(98, 100, 102, 104, 106, 108, 110), drop = FALSE), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.Location + '</b><br/>' +
          'Sex-ratio: ' + this.point.value }}"))
    )
}

# Suffrage féminin ----------------------------------------------------------------------------

#' Suffrage féminin mondial
#'
#' Carte interactive de l'accès au suffrage féminin par pays
#'
#' @return A highcharts map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_hc_mondiale_suffrage_feminin()
oc_geo_au_feminin_carte_hc_mondiale_suffrage_feminin <- function() {
  data <- geodata::oc_geo_au_feminin_owid_suffrage_feminin |>
    dplyr::rename(value = Year) |>
    dplyr::mutate(breaks = santoku::chop_pretty(value, 10))
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world-highres2.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "EqualEarth"
      )
    ) |>
    highcharter::hc_add_series(
      name = "Ann\u00e9e",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "Code"),
      borderColor = "transparent",
      nullColor = "#838a8f",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_pretty(data$value), viridis::viridis
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.Entity + '</b><br/>' +
          'Ann\u00e9e: ' + this.point.value }}"))
    )
}

#' Suffrage féminin suisse
#'
#' Carte interactive de l'accès au suffrage féminin par canton
#'
#' @return A highcharts map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_hc_suisse_suffrage_feminin()
oc_geo_au_feminin_carte_hc_suisse_suffrage_feminin <- function() {
  data <- geodata::oc_suisse_suffrage_feminin |>
    dplyr::mutate(canton_id = geotools::gtl_swiss_canton_id(Canton)) |>
    dplyr::rename(value = Annee)
  map <- geodata::gdt_opendata_swiss_geodata_json("canton")

  highcharter::highchart(type = "map") |>
    highcharter::hc_add_series(
      name = "Ann\u00e9e",
      mapData = map,
      data = data,
      value = "value",
      joinBy = "canton_id",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_add_series(
      mapData = geodata::gdt_opendata_swiss_geodata_json("lakes"),
      borderWidth = .5,
      borderColor = "transparent",
      negativeColor = "lightblue",
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_equally(data$value, 15), viridis::viridis
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.Canton + '</b><br/>' +
          'Ann\u00e9e: ' + this.point.value }}"))
    )
}


# Violences -----------------------------------------------------------------------------------

# FIXME The bounding box in mapsf seems broken
# TODO Change the map to the new tmap ?
oc_geo_au_feminin_carte_excision_europe <- function(theme = ggplot2::theme_minimal()) {
  sf::sf_use_s2(FALSE)

  rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    # dplyr::mutate(centroid = sf::st_centroid(geometry, of_largest_polygon = T)) |>
    dplyr::left_join(geodata::oc_geo_au_feminin_2020_fgm_indirect_data, by = c("adm0_a3" = "iso")) |>
    dplyr::filter(region_un == "Europe", adm0_a3 != "RUS") -> map

  mapsf::mf_map(map)
  mapsf::mf_map(map, var = "Girls_undergone_FGM", type = "prop")

  # nolint start: commented_code_linter
  # ggplot2::ggplot(ggplot2::aes(fill = Legal_status)) +
  # ggplot2::geom_sf(color = "#fffeea", size = .1) +
  # ggplot2::geom_sf(mapping = ggplot2::aes(geometry = centroid, size = Girls_undergone_FGM)) +
  # ggplot2::coord_sf(crs = geotools::gtl_crs_proj("eqearth"), datum = NA,
  #                   xlim = c(-1450000, 3223000),
  #                   ylim = c(4220000, 8120000)) +
  # ggplot2::scale_size_continuous(breaks = c(1000, 10000, 100000))
  # nolint end
}

#' Viols
#'
#' Cartes du taux de viols déclarés en Europe.
#'
#' @param theme A ggplot2 theme
#'
#' @name oc_geo_au_feminin_cartes_viol_europe
#' @return A map
#' @export
#' @examples
#' oc_geo_au_feminin_carte_viol_europe()
#' oc_geo_au_feminin_carte_hc_viol_europe()
oc_geo_au_feminin_carte_viol_europe <- function(theme = ggplot2::theme_minimal()) {
  eurostat::get_eurostat("crim_off_cat") |>
    dplyr::filter(iccs == "ICCS03011", unit == "P_HTHAB") |>
    dplyr::mutate(time = lubridate::ymd(TIME_PERIOD)) |>
    dplyr::filter(lubridate::year(time) == 2021) -> data

  eurostat::get_eurostat_geospatial(nuts_level = 0, make_valid = TRUE) |>
    dplyr::left_join(data, by = dplyr::join_by(CNTR_CODE == geo)) |>
    ggplot2::ggplot(ggplot2::aes(fill = values)) +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_viridis_b(option = "G", direction = -1) +
    ggplot2::coord_sf(crs = 3034, xlim = c(2.4e6, 5.5e6), ylim = c(1e6, NA), datum = NA) +
    theme +
    ggplot2::labs(
      title = "Nombre de plainte enregistr\u00e9es pour viol",
      subtitle = "Pour 100'000 habitants",
      fill = "",
      caption = "Eurostat (2021)"
    )
}

#' @rdname oc_geo_au_feminin_cartes_viol_europe
#' @export
oc_geo_au_feminin_carte_hc_viol_europe <- function() {
  eurostat::get_eurostat("crim_off_cat") |>
    dplyr::filter(iccs == "ICCS03011", unit == "P_HTHAB") |>
    dplyr::mutate(time = lubridate::ymd(TIME_PERIOD)) |>
    dplyr::filter(lubridate::year(time) == 2021) |>
    dplyr::rename(value = values) |>
    dplyr::mutate(pays = countrycode::countrycode(geo, "eurostat", "country.name.fr")) |>
    dplyr::mutate(iso = countrycode::countrycode(geo, "eurostat", "iso2c")) -> data

  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/europe.topo.json")

  palette <- function(...) {
    viridis::mako(direction = -1, ...)
  }

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Nombre de plainte enregistr\u00e9es pour viol") |>
    highcharter::hc_subtitle(text = "Pour 100'000 habitants") |>
    highcharter::hc_caption(text = "Source : <a href='' target='_blank'>Eurostat (2021)</a>") |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name = "WebMercator"
      ),
      center = c(10, 58)
    ) |>
    highcharter::hc_add_series(
      name = "Avortement 2015-2019",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("hc.a2", "iso"),
      nullColor = "#CCCCCC",
      borderColor = "transparent",
      borderWidth = 0.1,
      showInLegend = FALSE
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_pretty(data$value, 6), palette
      ),
      showInLegend = FALSE
    ) |>
    highcharter::hc_tooltip(
      formatter = shinyjqui::JS(glue::glue("function () {{
          return '<b>' + this.point.pays + '</b><br/>' +
          'Taux: ' + this.point.value }}"))
    )
}

#' OC Géo au féminin: carte de l'Afhanistan 1919
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc géo au féminin Afghanistan
#'
#' @export
oc_geo_au_feminin_carte_afghanistan_independance <- function(theme = ggplot2::theme_minimal()) {
  crs <- "+proj=laea +lat_0=31 +lon_0=69 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

  # Matrix as LonMin, LonMax, LatMin, LatMax
  box <- sf::sf_project(from = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", to = crs,
                        pts = matrix(c(50.75, 103.62, 17.18, 44.21), ncol = 2))

  geo_historical_world_map(lubridate::ymd("1919-12-31")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = rnaturalearth::ne_download(scale = 50, category = "physical", type = "land",
                                                       returnclass = "sf"),
                     fill = "#3a3a3f", color = "#fffeea", size = .1) +
    ggplot2::geom_sf(ggplot2::aes(fill = country_name), color = "#fffeea", size = .1,
                     show.legend = F) +
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
      xlim = box[,1],
      ylim = box[,2]
    ) +
    theme +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#1786ab"))
}

#' OC Géo au féminin: carte de l'Afhanistan 1950
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @concept oc géo au féminin Afghanistan
#'
#' @export
oc_geo_au_feminin_carte_afghanistan <- function(theme = ggplot2::theme_minimal()) {
  crs <- "+proj=laea +lat_0=31 +lon_0=69 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

  # Matrix as LonMin, LonMax, LatMin, LatMax
  box <- sf::sf_project(from = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", to = crs,
                        pts = matrix(c(50.75, 103.62, 17.18, 44.21), ncol = 2))

  geo_historical_world_map(lubridate::ymd("1950-01-01")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = rnaturalearth::ne_download(scale = 50, category = "physical", type = "land",
                                                       returnclass = "sf"),
                     fill = "#3a3a3f", color = "#fffeea", size = .1) +
    ggplot2::geom_sf(ggplot2::aes(fill = country_name), color = "#fffeea", size = .1,
                     show.legend = F) +
    gghighlight::gghighlight(
      gwcode %in% c( # Start selection
        365, # URSS
        700, # Afghanistan
        710, # China
        711, # Tibet (China)
        750, # British India
        770  # Pakistan
      ), # End selection
      unhighlighted_params = list(fill = "#3a3a3f")
    ) +
    ggplot2::scale_fill_manual(
      values = c("#e2e2e2", "#ec775d", "#d0c978", "#f5eb73", "#e82f42", "#ec775d"),
    ) +
    ggplot2::coord_sf(
      crs = crs,
      datum = NA,
      xlim = box[,1],
      ylim = box[,2]
    ) +
    theme +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#1786ab"))
}

# Espérance de vie femmes/hommes ------------------------------------------


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
    dplyr::mutate(data = santoku::chop(lexdelta, c(-0.2,2,4,6,8),
                                       extend = T, drop = F)) |>
    dplyr::mutate(adm0_a3 = countrycode::countrycode(un_code, "un", "iso3c", warn = F))
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
                     by = "adm0_a3") -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    ggfx::with_shadow(ggplot2::geom_sf(ggplot2::aes(fill = data),
                                       color = "#fffeea", size = .1),
                      colour = "#c6c6c5",
                      x_offset = 5,
                      y_offset = 5,
                      sigma = 4) +
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

#' OC Géo au féminin: carte de la différence d'espérance de vie
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_geo_au_feminin_carte_esperance_de_vie_wpp2012_2005_a_2010 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2012_lex,
    "2005-2010") |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Différence d'espérance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2005-2010)",
      caption = "Données : WPP2012",
      fill = "Différence (années)"
    )
}

#' OC Géo au féminin: carte de la différence d'espérance de vie
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2005_a_2010 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2019_lex,
    "2005-2010") |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Différence d'espérance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2005-2010)",
      caption = "Données : WPP2019",
      fill = "Différence (années)"
    )
}

#' OC Géo au féminin: carte de la différence d'espérance de vie
#'
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_geo_au_feminin_carte_esperance_de_vie_wpp2019_2015_a_2020 <- function(theme = ggplot2::theme_minimal()) {
  oc_geo_au_feminin_carte_base_donnees_wpp(
    geodata::oc_geo_au_feminin_wpp2019_lex,
    "2015-2020") |>
    oc_geo_au_feminin_carte_base_carte_wpp() +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(7)[6:1]
    ) +
    theme +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Différence d'espérance de vie",
      subtitle = "Entre les femmes et les hommes (estimations 2015-2020)",
      caption = "Données : WPP2019",
      fill = "Différence (années)"
    )
}

#' OC Géo au féminin: carte du sex ratio à la naissance
#'
#' @param year The year
#' @param theme A ggplot2 theme
#'
#' @return A ggplot2 map
#' @export
oc_geo_au_feminin_carte_sex_ratio <- function(year, theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(indicator = c("ratio" = "SP.POP.BRTH.MF"),
                   start_date = year,
                   end_date = year) |>
    dplyr::mutate(data = santoku::chop(ratio, c(1.05,1.1,1.15), extend = T, drop = F)) -> data

  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica") |>
    dplyr::left_join(data,
                     by = c("adm0_a3" = "iso3c")) -> plot_data

  plot_data |>
    ggplot2::ggplot() +
    ggfx::with_shadow(ggplot2::geom_sf(ggplot2::aes(fill = data),
                                       color = "#fffeea", size = .1),
                      colour = "#c6c6c5",
                      x_offset = 5,
                      y_offset = 5,
                      sigma = 4) +
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
      title = "Sex ratio à la naissance",
      subtitle = glue::glue("Année {year}"),
      fill = "Naissances garçons/filles",
      caption = "Données : Banque Mondiale"
    )
}

#' OC Géo au féminin: carte mondiale du suffrage féminin
#'
#' Carte interactive de l'accès au suffrage féminin par pays
#'
#' @param theme A ggplot 2 theme
#'
#' @return A ggiraph map
#' @export
oc_geo_au_feminin_carte_mondiale_suffrage_feminin <- function(theme = ggplot2::theme_minimal()) {
  rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
  dplyr::left_join(
    geodata::oc_geo_au_feminin_owid_suffrage_feminin,
    dplyr::join_by(adm0_a3 == Code)
  ) |>
    ggplot2::ggplot(ggplot2::aes(fill = Year, tooltip = Year)) +
    ggiraph::geom_sf_interactive(linewidth = .05) +
    ggplot2::scale_fill_viridis_c(option = "G") +
    ggplot2::coord_sf(crs = "+proj=eqearth") +
    ggplot2::labs(
      title = "Le suffrage féminin",
      fill = "Année"
    ) +
    theme -> plot

  ggiraph::girafe(ggobj = plot)
}

oc_geo_au_feminin_carte_hc_mondiale_suffrage_feminin <- function() {
  data <- geodata::oc_geo_au_feminin_owid_suffrage_feminin |>
    dplyr::rename(value = Year) |>
    dplyr::mutate(breaks = santoku::chop_pretty(value, 10))
  map <- geojsonio::geojson_read("https://code.highcharts.com/mapdata/custom/world.topo.json")

  highcharter::highchart(type = "map") |>
    highcharter::hc_title(text = "Le suffrage féminin") |>
    highcharter::hc_mapView(
      maxZoom = 30,
      projection = list(
        name= 'EqualEarth'
      )
    ) |>
    # highcharter::hc_tooltip(
    #   format = "<b>{point.name}</b>:<br>",
    #   formatter = highcharter::JS("function(){ return this.point.value; }")
    # ) |>
    highcharter::hc_add_series(
      name = "Année",
      mapData = map,
      data = data,
      value = "value",
      joinBy = c("iso.a3", "Code"),
      borderColor = "#FAFAFA",
      borderWidth = 0.1,
      showInLegend = F
    ) |>
    highcharter::hc_colorAxis(
      dataClasses = geotools::gtl_hc_color_axis(
        santoku::chop_pretty(data$value), viridis::cividis
      ),
      showInLegend = F
    )
}

oc_geo_au_feminin_carte_excision_europe <- function(theme = ggplot2::theme_minimal()) {
  sf::sf_use_s2(FALSE)

  rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    #dplyr::mutate(centroid = sf::st_centroid(geometry, of_largest_polygon = T)) |>
    dplyr::left_join(geodata::oc_geo_au_feminin_2020_fgm_indirect_data, by = c("adm0_a3" = "iso")) |>
    dplyr::filter(region_un == "Europe", adm0_a3 != "RUS") -> map

  mapsf::mf_map(map)
  mapsf::mf_map(map, var = "Girls_undergone_FGM", type = "prop")

    # ggplot2::ggplot(ggplot2::aes(fill = Legal_status)) +
    # ggplot2::geom_sf(color = "#fffeea", size = .1) +
    # ggplot2::geom_sf(mapping = ggplot2::aes(geometry = centroid, size = Girls_undergone_FGM)) +
    # ggplot2::coord_sf(crs = geotools::gtl_crs_proj("eqearth"), datum = NA,
    #                   xlim = c(-1450000, 3223000),
    #                   ylim = c(4220000, 8120000)) +
    # ggplot2::scale_size_continuous(breaks = c(1000, 10000, 100000))
}

oc_geo_au_feminin_carte_femmes_parlement <- function(theme = ggplot2::theme_minimal()) {
  wbstats::wb_data(indicator = c("parl" = "SG.GEN.PARL.ZS"),
                   start_date = 2022,
                   end_date = 2022) |>
    dplyr::mutate(parl_cut = santoku::chop_equally(parl, 6)) -> data

  rnaturalearth::ne_countries(returnclass = "sf") |>
    dplyr::left_join(
      data,
      dplyr::join_by(adm0_a3 == iso3c)
    ) |>
    ggplot2::ggplot(ggplot2::aes(fill = parl_cut)) +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = "+proj=eqearth", datum = NA) +
    ggplot2::scale_fill_manual(
      values = viridis::turbo(6, direction = -1, alpha = .8)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorsteps()
    )
}

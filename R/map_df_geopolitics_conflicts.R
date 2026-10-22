# Read a conflict ----------------------------------------------------------------------------

geopolitics_conflict_sites <- function(map) {
  geodata::df_geopolitics_conflict_sites |>
    dplyr::filter(map == !!map)
}

# Natural Earth countries (1:10m) cropped to a window in degrees
geopolitics_countries <- function(window, codes = NULL) {
  withr::local_options(sf_use_s2 = FALSE)
  # st_as_sf() loads sf first, so that subsetting keeps the geometry
  countries <- sf::st_as_sf(rnaturalearthhires::countries10)
  if (!is.null(codes)) {
    countries <- countries[countries$ADM0_A3 %in% codes, ]
  }
  countries |>
    sf::st_make_valid() |>
    sf::st_crop(window) |>
    suppressMessages() |>
    suppressWarnings()
}

#' Read a conflict: maps
#'
#' Static maps for the exercise "Read a conflict".
#'
#' `df_geopolitics_map_preah_vihear()`: the Thailand–Cambodia border along the
#' Dângrêk mountains, with the relief (the 1904 treaty put the border on the
#' watershed, along the escarpment), the provinces and the disputed temples of
#' Preah Vihear, Ta Muen Thom and Ta Krabey.
#'
#' `df_geopolitics_map_iran_war()`: the region of the war around Iran, from
#' Israel to Pakistan, with the capitals, the nuclear sites of Fordow, Natanz
#' and Isfahan, the Al Udeid air base and the Strait of Hormuz.
#'
#' Coordinates of the sites from Wikidata, borders from Natural Earth, relief
#' from the Copernicus DEM. Both maps are drawn for a figure about 17 cm wide.
#' Names, legends and sources are in English or French, following
#' [geotools::gtl_opt_set_i18n()].
#'
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map
#' @export
#'
#' @examples
#' df_geopolitics_map_preah_vihear()
#' df_geopolitics_map_iran_war()
df_geopolitics_map_preah_vihear <- function(theme = ggplot2::theme_minimal()) {
  window <- c(xmin = 102.5, xmax = 105.5, ymin = 13.5, ymax = 15.5)
  ink <- "#333333"
  relief <- geodata::df_geopolitics_preah_vihear_relief
  countries <- geopolitics_countries(window + c(-1, 1, -1, 1), codes = c("THA", "KHM", "LAO"))
  sites <- geopolitics_conflict_sites("preah_vihear")

  provinces <- sf::st_as_sf(rnaturalearthhires::states10) |>
    dplyr::filter(adm0_a3 %in% c("THA", "KHM"))
  province_names <- c(
    "Si Sa Ket" = "Sisaket", "Surin" = "Surin", "Buri Ram" = "Buriram",
    "Preah Vihear" = "Preah Vihear", "Oddar Meanchey" = "Oddar Meanchey"
  )
  province_labels <- provinces |>
    dplyr::mutate(label = province_names[ifelse(adm0_a3 == "KHM", name_en, name)]) |>
    dplyr::filter(!is.na(label)) |>
    (\(p) {
      withr::local_options(sf_use_s2 = FALSE)
      suppressWarnings(suppressMessages(sf::st_point_on_surface(sf::st_crop(sf::st_make_valid(p), window))))
    })() |>
    geology_xy()

  country_labels <- tibble::tribble(
    ~x,    ~y,    ~label,
    103.2, 15.3,  tr("THAILAND", "THA\u00cfLANDE"),
    103.9, 13.65, tr("CAMBODIA", "CAMBODGE"),
    105.3, 15.3,  "LAOS"
  )

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = relief,
      ggplot2::aes(lon, lat, fill = elevation),
      interpolate = TRUE
    ) +
    ggplot2::geom_raster(
      data = relief,
      ggplot2::aes(lon, lat, alpha = I(pmax(0, 0.72 - hillshade) * 1.6)),
      fill = "black",
      interpolate = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#eef3e2", "#dfe8c6", "#e9d9a8", "#d7b27e", "#b98a5e"),
      limits = c(0, 750),
      breaks = c(0, 250, 500, 750),
      name = tr("Elevation (m)", "Altitude (m)")
    ) +
    ggplot2::geom_sf(data = provinces, fill = NA, color = "#7a7a7a", linewidth = 0.25, linetype = "22") +
    ggplot2::geom_sf(data = countries, fill = NA, color = ink, linewidth = 0.9) +
    ggplot2::geom_text(
      data = province_labels,
      ggplot2::aes(x, y, label = label),
      size = 2.6, fontface = "italic", color = "#555555"
    ) +
    ggplot2::geom_text(
      data = country_labels,
      ggplot2::aes(x, y, label = label),
      size = 3.8, fontface = "bold", color = ink
    ) +
    ggplot2::geom_point(data = sites, ggplot2::aes(lon, lat), shape = 17, size = 2.8, color = "#b2182b") +
    ggrepel::geom_label_repel(
      data = sites,
      ggplot2::aes(lon, lat, label = name),
      size = 3, fontface = "bold", color = "#b2182b", fill = "white", alpha = 0.9,
      label.size = 0, min.segment.length = 0, box.padding = 0.5, seed = 1904
    ) +
    geology_scale_bar(
      x = window[["xmin"]] + 0.08, y = window[["ymin"]] + 0.08,
      length_km = 50, segments = 5, height = 0.025, size = 2.5,
      # km per degree of longitude in the middle of the map
      unit_km = 111.32 * cos(mean(window[c("ymin", "ymax")]) * pi / 180)
    ) +
    ggplot2::coord_sf(
      xlim = window[c("xmin", "xmax")],
      ylim = window[c("ymin", "ymax")],
      expand = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      caption = tr(
        "Borders: Natural Earth. Relief: Copernicus DEM GLO-90 (ESA). Sites: Wikidata",
        "Fronti\u00e8res\u00a0: Natural Earth. Relief\u00a0: Copernicus DEM GLO-90 (ESA). Sites\u00a0: Wikidata"
      )
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.height = grid::unit(0.25, "cm"),
      legend.key.width = grid::unit(1.2, "cm"),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7),
      panel.border = ggplot2::element_rect(fill = NA, color = ink, linewidth = 0.4)
    )
}

#' @rdname df_geopolitics_map_preah_vihear
#' @export
df_geopolitics_map_iran_war <- function(theme = ggplot2::theme_minimal()) {
  ink <- "#333333"
  crs <- "+proj=lcc +lat_1=20 +lat_2=38 +lat_0=27 +lon_0=54 +datum=WGS84 +units=m +no_defs"

  # the window in degrees, outlined with many points so that its projected
  # extent is right (parallels are curved in this projection)
  lons <- seq(33, 75, by = 0.5)
  lats <- seq(12, 42, by = 0.5)
  window <- tibble::tibble(
    lon = c(lons, rep(75, length(lats)), rev(lons), rep(33, length(lats))),
    lat = c(rep(12, length(lons)), lats, rep(42, length(lons)), rev(lats))
  ) |>
    geology_points(crs) |>
    sf::st_bbox()

  countries <- geopolitics_countries(c(xmin = 20, xmax = 90, ymin = 0, ymax = 55)) |>
    sf::st_transform(crs)
  sites <- geopolitics_conflict_sites("iran_war") |>
    geology_points(crs) |>
    geology_xy()

  # names of the states of the region at their Natural Earth label point (a
  # fixed list: Natural Earth also has tiny units like sovereign base areas)
  labelled <- c(
    "IRN", "ISR", "PSX", "LBN", "SYR", "IRQ", "JOR", "SAU", "KWT", "BHR", "QAT", "ARE",
    "OMN", "YEM", "TUR", "AFG", "PAK", "TKM", "AZE", "EGY"
  )
  country_labels <- countries |>
    sf::st_drop_geometry() |>
    dplyr::filter(ADM0_A3 %in% labelled) |>
    dplyr::transmute(
      code = ADM0_A3, name = tr(NAME_EN, NAME_FR), lon = LABEL_X, lat = LABEL_Y, rank = LABELRANK
    ) |>
    geology_points(crs) |>
    geology_xy() |>
    dplyr::filter(
      dplyr::between(x, window[["xmin"]], window[["xmax"]]),
      dplyr::between(y, window[["ymin"]], window[["ymax"]])
    ) |>
    dplyr::transmute(
      x, y,
      label = toupper(name),
      face = "bold",
      size = ifelse(code == "IRN", 4, ifelse(rank <= 4, 2.9, 2.3)),
      color = ink
    )

  site_names <- c(
    "Tehran" = "T\u00e9h\u00e9ran", "Fordow" = "Fordo", "Natanz" = "Natanz", "Isfahan" = "Ispahan",
    "Al Udeid air base" = "Base a\u00e9rienne d'Al-Udeid", "Doha" = "Doha", "Islamabad" = "Islamabad",
    "Muscat" = "Mascate"
  )
  site_labels <- sites |>
    dplyr::filter(type != "strait") |>
    dplyr::transmute(x, y, label = tr(name, site_names[name]), face = "plain", size = 2.7, color = "#7a0b0b")

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = countries,
      ggplot2::aes(fill = I(ifelse(ADM0_A3 == "IRN", "#f3dcb4", "#efefe9"))),
      color = "#8c8c8c",
      linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(sites, type != "strait"),
      ggplot2::aes(x, y, shape = type, fill = type),
      size = 2.6, color = ink, stroke = 0.5
    ) +
    ggplot2::scale_shape_manual(
      values = c(capital = 21, "nuclear site" = 24, "air base" = 22),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(capital = "white", "nuclear site" = "#ffd84a", "air base" = "#4f7cac"),
      labels = c(
        capital = tr("capital", "capitale"),
        "nuclear site" = tr("nuclear site", "site nucl\u00e9aire"),
        "air base" = tr("air base", "base a\u00e9rienne")
      ),
      name = NULL
    ) +
    # one legend: the fill legend with the shapes of the symbols
    ggplot2::guides(
      shape = "none",
      fill = ggplot2::guide_legend(override.aes = list(shape = c(22, 21, 24), size = 3))
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(sites, type == "strait"),
      ggplot2::aes(x, y, label = tr("Strait of\nHormuz", "D\u00e9troit\nd'Ormuz")),
      size = 2.6, fontface = "italic", color = "#2a5d8f", lineheight = 0.85, nudge_y = -120000
    ) +
    # one layer for all names, so that they avoid each other
    ggrepel::geom_text_repel(
      data = dplyr::bind_rows(country_labels, site_labels),
      ggplot2::aes(x, y, label = label, fontface = face, size = I(size), color = I(color)),
      segment.color = "#8c8c8c", segment.size = 0.3, min.segment.length = 0.3,
      max.overlaps = Inf, seed = 2025
    ) +
    geology_scale_bar(
      x = window[["xmin"]] + 150000, y = window[["ymin"]] + 150000,
      length_km = 1000, segments = 4, height = 35000, size = 2.5
    ) +
    ggplot2::coord_sf(
      crs = crs,
      xlim = window[c("xmin", "xmax")],
      ylim = window[c("ymin", "ymax")],
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      caption = tr(
        "Borders: Natural Earth. Sites: Wikidata",
        "Fronti\u00e8res\u00a0: Natural Earth. Sites\u00a0: Wikidata"
      )
    ) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      panel.background = ggplot2::element_rect(fill = "#dbe7f3", color = NA),
      panel.border = ggplot2::element_rect(fill = NA, color = ink, linewidth = 0.4)
    )
}

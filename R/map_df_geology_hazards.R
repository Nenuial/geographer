# Helpers ------------------------------------------------------------------------------------

geology_points <- function(data, crs) {
  data |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(crs)
}

geology_xy <- function(data) {
  coordinates <- sf::st_coordinates(data)
  data |>
    sf::st_drop_geometry() |>
    dplyr::mutate(x = coordinates[, 1], y = coordinates[, 2])
}

# Scale bar in map units (metres by default, or unit_km km per map unit):
# alternating black and white segments
geology_scale_bar <- function(x, y, length_km, segments, height, size = 2.8, unit_km = 0.001) {
  step <- length_km / unit_km / segments
  bars <- tibble::tibble(
    xmin = x + (seq_len(segments) - 1) * step,
    xmax = x + seq_len(segments) * step,
    fill = rep(c("black", "white"), length.out = segments)
  )
  ticks <- tibble::tibble(x = x + (0:segments) * step, label = (0:segments) * length_km / segments)

  list(
    ggplot2::geom_rect(
      data = bars,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y, ymax = y + height, fill = I(fill)),
      color = "black",
      linewidth = 0.3,
      inherit.aes = FALSE
    ),
    ggplot2::geom_text(
      data = ticks,
      ggplot2::aes(x = x, y = y + height * 1.6, label = label),
      size = size,
      vjust = 0,
      inherit.aes = FALSE
    ),
    ggplot2::annotate(
      "text",
      x = x + length_km / unit_km + step * 0.4, y = y + height / 2,
      label = "km", hjust = 0, size = size
    )
  )
}

# Find the earthquake ------------------------------------------------------------------------

# Switzerland and the full circles around the three stations, LV95 (metres)
geology_swiss_extent <- c(xmin = 2480000, xmax = 2840000, ymin = 1000000, ymax = 1300000)

#' Find the earthquake: map of the stations
#'
#' Map of Switzerland with the three seismic stations of the epicentre exercise
#' (Sion, Bern and Lugano), for drawing circles with a compass. The map is in
#' the Swiss projection LV95, so distances are true, and its panel has a fixed
#' size: printed at 100 %, the map has the scale `1:scale`. The key version adds
#' the circles (radius: distance to the epicentre) and the epicentre of the
#' Visp earthquake of 1855.
#'
#' The panel of `df_geology_map_epicentre()` measures 360 km × 300 km at the
#' chosen scale (14.4 cm × 12 cm at 1:2 500 000, 18 cm × 15 cm at 1:2 000 000):
#' give the figure at least about 2 cm more in each direction and do not let
#' the document rescale it.
#'
#' @param key `TRUE` for the answer key (circles and epicentre)
#' @param scale Denominator of the printed scale
#' @param title `TRUE` for a default title, `FALSE` for none, or a string
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map (`df_geology_map_epicentre()`) or a leaflet map
#'   (`df_geology_map_leaflet_epicentre()`)
#' @export
#'
#' @examples
#' df_geology_map_epicentre()
#' df_geology_map_epicentre(key = TRUE)
#' df_geology_map_leaflet_epicentre(key = TRUE)
df_geology_map_epicentre <- function(
  key = FALSE,
  scale = 2500000,
  title = TRUE,
  theme = ggplot2::theme_minimal()
) {
  extent <- geology_swiss_extent
  basemap <- geodata::df_geology_swiss_basemap
  stations <- geology_points(geodata::df_geology_seismic_stations, 2056)

  if (isTRUE(title)) {
    title <- if (key) "Epicentre of the earthquake" else "Seismic stations"
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = dplyr::filter(basemap, layer == "country"),
      ggplot2::aes(fill = I(ifelse(name == "Switzerland", "#ffffff", "#e6e6e6"))),
      color = "#4d4d4d",
      linewidth = 0.4
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(basemap, layer == "canton"),
      fill = NA,
      color = "#a6a6a6",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(basemap, layer == "lake"),
      fill = "#c6dbef",
      color = "#6b8fb3",
      linewidth = 0.2
    )

  if (key) {
    epicentre <- geology_points(geodata::df_geology_epicentre, 2056)
    p <- p +
      ggplot2::geom_sf(
        data = sf::st_buffer(stations, stations$distance * 1000),
        fill = NA,
        color = "#d7191c",
        linewidth = 0.5,
        linetype = "dashed"
      ) +
      ggplot2::geom_sf(data = epicentre, shape = 8, size = 3.5, stroke = 1, color = "#d7191c") +
      ggplot2::geom_text(
        data = geology_xy(epicentre),
        ggplot2::aes(x, y, label = paste(name, year)),
        nudge_y = 9000,
        color = "#d7191c",
        fontface = "bold",
        size = 3.2
      )
  }

  p +
    ggplot2::geom_sf(data = stations, shape = 17, size = 3, color = "black") +
    ggplot2::geom_text(
      data = geology_xy(stations),
      ggplot2::aes(x, y, label = station),
      nudge_x = 6000,
      hjust = 0,
      fontface = "bold",
      size = 3.4
    ) +
    geology_scale_bar(
      x = extent[["xmin"]] + 15000, y = extent[["ymin"]] + 12000,
      length_km = 50, segments = 5, height = 3500
    ) +
    ggplot2::coord_sf(
      crs = 2056,
      xlim = extent[c("xmin", "xmax")],
      ylim = extent[c("ymin", "ymax")],
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = if (isFALSE(title)) NULL else title,
      x = NULL,
      y = NULL,
      caption = glue::glue(
        "Scale 1:{format(scale, big.mark = ' ', scientific = FALSE)} when printed at 100 %. ",
        "Swiss projection LV95. Data: Natural Earth"
      )
    ) +
    theme +
    ggplot2::theme(
      panel.widths = grid::unit((extent[["xmax"]] - extent[["xmin"]]) / scale * 100, "cm"),
      panel.heights = grid::unit((extent[["ymax"]] - extent[["ymin"]]) / scale * 100, "cm"),
      panel.border = ggplot2::element_rect(fill = NA, color = "#4d4d4d", linewidth = 0.4)
    )
}

#' @rdname df_geology_map_epicentre
#' @export
df_geology_map_leaflet_epicentre <- function(key = FALSE) {
  stations <- geodata::df_geology_seismic_stations
  label_options <- leaflet::labelOptions(noHide = TRUE, direction = "right", textsize = "14px")

  map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::setView(lng = 8.2, lat = 46.6, zoom = 8) |>
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))

  if (key) {
    epicentre <- geodata::df_geology_epicentre
    map <- map |>
      leaflet::addCircles(
        data = stations, lng = ~lon, lat = ~lat, radius = ~ distance * 1000,
        color = "#d7191c", weight = 3, opacity = 1, fill = FALSE, dashArray = "8 6"
      ) |>
      leaflet::addCircleMarkers(
        data = epicentre, lng = ~lon, lat = ~lat, radius = 7,
        color = "#d7191c", fillColor = "#d7191c", fillOpacity = 1, weight = 1.5, opacity = 1,
        label = ~ paste(name, year), labelOptions = label_options
      )
  }

  map |>
    leaflet::addCircleMarkers(
      data = stations, lng = ~lon, lat = ~lat, radius = 6,
      color = "#000000", fillColor = "#000000", fillOpacity = 1, weight = 1.5, opacity = 1,
      label = ~station, labelOptions = label_options
    )
}

#' Find the earthquake: seismograms
#'
#' Synthetic seismograms of the same earthquake at the three stations of the
#' epicentre exercise, stacked on the same time axis (seconds after the
#' earthquake, a tick every second): first the small P wave, then the larger
#' S wave. The key version marks the arrivals of the P and S waves.
#'
#' @param key `TRUE` to mark the arrivals of the P and S waves
#' @param title `TRUE` for a default title, `FALSE` for none, or a string
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 graph (`df_geology_graph_seismograms()`) or a
#'   highcharts chart (`df_geology_hc_seismograms()`)
#' @export
#'
#' @examples
#' df_geology_graph_seismograms()
#' df_geology_hc_seismograms(key = TRUE)
df_geology_graph_seismograms <- function(key = FALSE, title = TRUE, theme = ggplot2::theme_minimal()) {
  stations <- geodata::df_geology_seismic_stations
  data <- geodata::df_geology_seismograms |>
    dplyr::mutate(station = factor(station, levels = stations$station))

  p <- ggplot2::ggplot(data, ggplot2::aes(time, amplitude)) +
    ggplot2::geom_line(linewidth = 0.25, color = "#1a1a1a")

  if (key) {
    arrivals <- stations |>
      dplyr::select(station, P = p_arrival, S = s_arrival) |>
      tidyr::pivot_longer(c(P, S), names_to = "wave", values_to = "time") |>
      dplyr::mutate(station = factor(station, levels = stations$station))
    p <- p +
      ggplot2::geom_vline(data = arrivals, ggplot2::aes(xintercept = time), color = "#d7191c", linewidth = 0.5) +
      ggplot2::geom_text(
        data = arrivals,
        ggplot2::aes(x = time, y = Inf, label = wave),
        color = "#d7191c", fontface = "bold", hjust = -0.3, vjust = 1.3, size = 3.5
      )
  }

  p +
    ggplot2::facet_wrap(ggplot2::vars(station), ncol = 1, strip.position = "left") +
    ggplot2::scale_x_continuous(
      name = "Time after the earthquake (s)",
      breaks = 0:40,
      labels = \(x) ifelse(x %% 5 == 0, x, ""),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(name = NULL, breaks = NULL) +
    ggplot2::labs(title = if (isTRUE(title)) "Seismograms" else if (isFALSE(title)) NULL else title) +
    theme +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#d9d9d9", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(color = "#4d4d4d", linewidth = 0.25),
      axis.ticks.length.x = grid::unit(2, "pt"),
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold"),
      strip.placement = "outside"
    )
}

#' @rdname df_geology_graph_seismograms
#' @export
df_geology_hc_seismograms <- function(key = FALSE, title = TRUE) {
  stations <- geodata::df_geology_seismic_stations
  n <- nrow(stations)

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(animation = FALSE) |>
    highcharter::hc_title(text = if (isTRUE(title)) "Seismograms" else if (isFALSE(title)) NULL else title) |>
    highcharter::hc_xAxis(
      min = 0, max = 40, tickInterval = 1,
      gridLineWidth = 1, gridLineColor = "#e6e6e6",
      labels = list(step = 5),
      title = list(text = "Time after the earthquake (s)")
    ) |>
    highcharter::hc_legend(enabled = FALSE) |>
    highcharter::hc_tooltip(enabled = FALSE) |>
    highcharter::hc_plotOptions(
      series = list(animation = FALSE, turboThreshold = 0, marker = list(enabled = FALSE))
    )

  # one pane per station, stacked
  panes <- purrr::map(seq_len(n), \(i) {
    list(
      title = list(text = stations$station[i], rotation = 0, margin = 30, style = list(fontWeight = "bold")),
      top = paste0((i - 1) * 100 / n, "%"),
      height = paste0(100 / n - 4, "%"),
      offset = 0,
      labels = list(enabled = FALSE),
      gridLineWidth = 0
    )
  })
  hc <- do.call(highcharter::hc_yAxis_multiples, c(list(hc), panes))

  for (i in seq_len(n)) {
    trace <- geodata::df_geology_seismograms |>
      dplyr::filter(station == stations$station[i]) |>
      dplyr::select(time, amplitude)
    hc <- hc |>
      highcharter::hc_add_series(
        type = "line",
        name = stations$station[i],
        data = highcharter::list_parse2(trace),
        yAxis = i - 1,
        color = "#1a1a1a",
        lineWidth = 1
      )
    if (key) {
      hc <- hc |>
        highcharter::hc_add_series(
          type = "scatter",
          name = paste(stations$station[i], "arrivals"),
          data = list(
            list(x = stations$p_arrival[i], y = 3, name = "P"),
            list(x = stations$s_arrival[i], y = 3, name = "S")
          ),
          yAxis = i - 1,
          color = "#d7191c",
          marker = list(enabled = TRUE, symbol = "triangle-down", radius = 6),
          dataLabels = list(enabled = TRUE, format = "{point.name}", style = list(color = "#d7191c"))
        )
    }
  }
  hc
}

# Vesuvius -----------------------------------------------------------------------------------

geology_vesuvius_crs <- 32633

geology_vesuvius_window <- function(half_width) {
  crater <- geodata::df_geology_vesuvius_sites |>
    dplyr::filter(type == "crater") |>
    geology_points(geology_vesuvius_crs) |>
    sf::st_coordinates() |>
    unname()
  c(
    xmin = crater[1, 1] - half_width, xmax = crater[1, 1] + half_width,
    ymin = crater[1, 2] - half_width, ymax = crater[1, 2] + half_width
  )
}

geology_red_zone_colors <- c(
  entirely = "#de2d26",
  partly = "#fc9272",
  no = "#f2f2f2"
)

#' Vesuvius: where to draw the red line
#'
#' Map of the municipalities around Vesuvius with their names and population,
#' the crater, the ancient sites of Pompeii and Herculaneum and rings showing
#' the approximate reach of the pyroclastic flows of past eruptions. The key
#' version colours the red zone of the national emergency plan (municipalities
#' entirely or partly in it).
#'
#' @param key `TRUE` to colour the official red zone
#' @param rings Named vector of ring radii in km around the crater; the names
#'   label the rings (approximate reach of pyroclastic flows)
#' @param half_width Half width of the map around the crater in km
#'   (ggplot version)
#' @param title `TRUE` for a default title, `FALSE` for none, or a string
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map (`df_geology_map_vesuvius()`) or a leaflet map
#'   (`df_geology_map_leaflet_vesuvius()`)
#' @export
#'
#' @examples
#' df_geology_map_vesuvius()
#' df_geology_map_vesuvius(key = TRUE)
#' df_geology_map_leaflet_vesuvius(key = TRUE)
df_geology_map_vesuvius <- function(
  key = FALSE,
  rings = c("1631" = 10, "79" = 15),
  half_width = 25,
  title = TRUE,
  theme = ggplot2::theme_minimal()
) {
  window <- geology_vesuvius_window(half_width * 1000)
  municipalities <- geodata::df_geology_vesuvius_municipalities
  sites <- geology_points(geodata::df_geology_vesuvius_sites, geology_vesuvius_crs)
  crater <- dplyr::filter(sites, type == "crater")

  if (isTRUE(title)) {
    title <- if (key) "Vesuvius: the red zone" else "Vesuvius: where to draw the red line?"
  }

  labels <- municipalities |>
    sf::st_point_on_surface() |>
    suppressWarnings() |>
    geology_xy() |>
    dplyr::filter(
      dplyr::between(x, window[["xmin"]], window[["xmax"]]),
      dplyr::between(y, window[["ymin"]], window[["ymax"]])
    ) |>
    dplyr::mutate(
      label = paste0(name, "\n", format(population, big.mark = " ")),
      face = ifelse(population >= 40000, "bold", "plain"),
      size = ifelse(population >= 40000, 2.4, 1.8)
    ) |>
    dplyr::select(x, y, label, face, size) |>
    # site names in the same layer, so that all labels avoid each other
    dplyr::bind_rows(
      geology_xy(sites) |>
        dplyr::transmute(x, y, label = name, face = "bold.italic", size = 3)
    )

  ring_lines <- sf::st_buffer(crater[rep(1, length(rings)), ], rings * 1000) |>
    dplyr::mutate(label = paste0("~", rings, " km (", names(rings), ")"))
  # ring labels south-west of the crater, over the gulf
  ring_labels <- tibble::tibble(
    x = sf::st_coordinates(crater)[1, 1] - rings * 1000 * cos(pi / 4),
    y = sf::st_coordinates(crater)[1, 2] - rings * 1000 * sin(pi / 4),
    label = ring_lines$label
  )

  fill <- if (key) geology_red_zone_colors[municipalities$red_zone] else "#f2f2f2"

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = municipalities,
      ggplot2::aes(fill = I(fill)),
      color = "#8c8c8c",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(data = ring_lines, fill = NA, color = "#4d4d4d", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_label(
      data = ring_labels,
      ggplot2::aes(x, y, label = label),
      size = 2.6, label.size = 0, fill = "white", alpha = 0.8, vjust = 0.5
    ) +
    ggrepel::geom_text_repel(
      data = labels,
      ggplot2::aes(x, y, label = label, fontface = face, size = I(size)),
      lineheight = 0.85,
      segment.color = NA,
      max.overlaps = Inf,
      seed = 79
    ) +
    ggplot2::geom_sf(data = crater, shape = 17, size = 3.5, color = "black") +
    ggplot2::geom_sf(
      data = dplyr::filter(sites, type == "ancient site"),
      shape = 22, size = 3, fill = "#ffd84a", color = "black", stroke = 0.5
    ) +
    geology_scale_bar(
      x = window[["xmin"]] + 2000, y = window[["ymin"]] + 2000,
      length_km = 10, segments = 5, height = 400, size = 2.5
    ) +
    ggplot2::coord_sf(
      crs = geology_vesuvius_crs,
      xlim = window[c("xmin", "xmax")],
      ylim = window[c("ymin", "ymax")],
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = if (isFALSE(title)) NULL else title,
      subtitle = if (key) {
        "Red: entirely in the red zone; pink: partly (Napoli, Nola, Pomigliano d'Arco)"
      },
      x = NULL,
      y = NULL,
      caption = paste(
        c(
          "Population: ISTAT, 1 January 2026 (estimate).",
          "Rings: approximate reach of pyroclastic flows.",
          if (key) "Red zone: national emergency plan (2014)."
        ),
        collapse = "\n"
      )
    ) +
    theme +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#dbe7f3", color = NA),
      panel.border = ggplot2::element_rect(fill = NA, color = "#4d4d4d", linewidth = 0.4)
    )
}

#' @rdname df_geology_map_vesuvius
#' @export
df_geology_map_leaflet_vesuvius <- function(key = FALSE, rings = c("1631" = 10, "79" = 15)) {
  municipalities <- geodata::df_geology_vesuvius_municipalities |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      fill = if (key) unname(geology_red_zone_colors[red_zone]) else "#f2f2f2",
      label = paste0(name, ": ", format(population, big.mark = "\u2009"), " inhabitants")
    )
  sites <- geodata::df_geology_vesuvius_sites
  crater <- dplyr::filter(sites, type == "crater")
  label_options <- leaflet::labelOptions(noHide = TRUE, direction = "top", textsize = "13px")

  map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::setView(lng = crater$lon, lat = crater$lat, zoom = 11) |>
    leaflet::addPolygons(
      data = municipalities,
      fillColor = ~fill, fillOpacity = if (key) 0.6 else 0.15,
      color = "#636363", weight = 1,
      label = ~label,
      highlightOptions = leaflet::highlightOptions(weight = 3, bringToFront = TRUE)
    )

  for (i in seq_along(rings)) {
    map <- map |>
      leaflet::addCircles(
        lng = crater$lon, lat = crater$lat, radius = rings[[i]] * 1000,
        color = "#252525", weight = 2.5, opacity = 1, fill = FALSE, dashArray = "8 6",
        label = paste0("\u2248 ", rings[[i]], " km (", names(rings)[i], ")")
      )
  }

  map |>
    leaflet::addCircleMarkers(
      data = sites, lng = ~lon, lat = ~lat, radius = 6,
      color = "#000000", fillColor = ~ ifelse(type == "crater", "#000000", "#ffd84a"),
      fillOpacity = 1, weight = 1.5,
      label = ~name, labelOptions = label_options
    ) |>
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))
}

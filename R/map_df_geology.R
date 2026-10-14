# Plate detective ----------------------------------------------------------------------------

# Depth classes shared by every plate detective graphic
# (the GeoKey deck uses the same classes and colours)
geology_depth_labels <- c("0\u201370 km", "70\u2013300 km", "300\u2013700 km")
geology_depth_colors <- c("#ffd84a", "#ff8a3c", "#ff4f9a")
# circle, square and diamond keep the classes apart in greyscale
geology_depth_shapes <- c(21, 22, 23)
geology_volcano_color <- "#d7191c"
geology_depth_fills <- c(geology_depth_colors)
names(geology_depth_fills) <- geology_depth_labels
geology_depth_point_shapes <- c(geology_depth_shapes)
names(geology_depth_point_shapes) <- geology_depth_labels

geology_depth_class <- function(depth) {
  cut(depth, breaks = c(-Inf, 70, 300, Inf), labels = geology_depth_labels)
}

geology_bbox <- function(region) {
  geodata::df_geology_regions[geodata::df_geology_regions$region == region, ]
}

geology_title <- function(title, bbox) {
  if (isTRUE(title)) {
    return(glue::glue("{bbox$region} \u2013 {bbox$name}"))
  }
  if (isFALSE(title)) {
    return(NULL)
  }
  title
}

geology_quakes <- function(region) {
  geodata::df_geology_earthquakes |>
    dplyr::filter(region == !!region) |>
    dplyr::mutate(depth_class = geology_depth_class(depth)) |>
    dplyr::arrange(depth)
}

geology_volcanoes <- function(region) {
  geodata::df_geology_volcanoes |>
    dplyr::filter(region == !!region)
}

# Land of the region as one dissolved (multi)polygon, without country borders
geology_land <- function(bbox, margin = 1) {
  withr::local_options(sf_use_s2 = FALSE)
  rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    sf::st_make_valid() |>
    sf::st_crop(c(
      xmin = bbox$lon_min - margin, xmax = bbox$lon_max + margin,
      ymin = bbox$lat_min - margin, ymax = bbox$lat_max + margin
    )) |>
    sf::st_union() |>
    suppressMessages() |>
    suppressWarnings()
}

#' Plate detective maps
#'
#' Earthquakes (magnitude 5 or more, 1975–2025) and Holocene volcanoes of one
#' of the five regions of the plate detective exercise, without plate
#' boundaries: finding them is the exercise. Epicentres are coloured (and
#' shaped, for greyscale printing) by depth class: 0–70 km, 70–300 km and
#' 300–700 km. The size of the points shows the magnitude.
#'
#' @param region Region code: `"A"` Japan, `"B"` Central Andes, `"C"` Iceland /
#'   North Atlantic, `"D"` Himalaya / Tibet or `"E"` California (see
#'   [geodata::df_geology_regions])
#' @param title `TRUE` for the region code and name, `FALSE` for no title, or
#'   a string
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map (`df_geology_map_plate_detective()`) or a highcharts
#'   chart (`df_geology_map_hc_plate_detective()`)
#' @export
#'
#' @examples
#' df_geology_map_plate_detective("A")
#' df_geology_map_hc_plate_detective("B")
df_geology_map_plate_detective <- function(
  region = c("A", "B", "C", "D", "E"),
  title = TRUE,
  theme = ggplot2::theme_minimal()
) {
  region <- match.arg(region)
  bbox <- geology_bbox(region)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = geology_land(bbox),
      fill = "#d2d2d2",
      color = "#5f5f5f",
      linewidth = 0.25
    ) +
    ggplot2::geom_point(
      data = geology_quakes(region),
      ggplot2::aes(lon, lat, fill = depth_class, shape = depth_class, size = mag),
      color = "#333333",
      stroke = 0.2,
      alpha = 0.85,
      show.legend = TRUE
    ) +
    ggplot2::geom_point(
      data = geology_volcanoes(region),
      ggplot2::aes(lon, lat, color = "Volcano"),
      shape = 17,
      size = 2.4
    ) +
    ggplot2::scale_fill_manual(
      values = geology_depth_fills,
      drop = FALSE,
      name = "Depth"
    ) +
    ggplot2::scale_shape_manual(
      values = geology_depth_point_shapes,
      drop = FALSE,
      name = "Depth"
    ) +
    ggplot2::scale_size_continuous(
      range = c(0.5, 3.5),
      limits = c(5, 9.5),
      breaks = c(5, 6, 7, 8, 9),
      name = "Magnitude"
    ) +
    ggplot2::scale_color_manual(values = c(Volcano = geology_volcano_color), name = NULL) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1,
        override.aes = list(size = 3, shape = geology_depth_shapes, fill = geology_depth_colors)
      ),
      shape = ggplot2::guide_legend(order = 1),
      size = ggplot2::guide_legend(order = 2),
      color = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox$lon_min, bbox$lon_max),
      ylim = c(bbox$lat_min, bbox$lat_max),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = geology_title(title, bbox),
      x = NULL,
      y = NULL,
      caption = "Data: USGS (M 5+, 1975\u20132025), Smithsonian Global Volcanism Program"
    ) +
    theme +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#eef4fa", color = NA),
      panel.grid = ggplot2::element_line(color = "#b8c8d8", linewidth = 0.3),
      panel.ontop = FALSE
    )
}

#' @rdname df_geology_map_plate_detective
#' @export
df_geology_map_hc_plate_detective <- function(region = c("A", "B", "C", "D", "E"), title = TRUE) {
  region <- match.arg(region)
  bbox <- geology_bbox(region)

  # equal scale on both axes (plate carree at the middle latitude, as in the
  # ggplot version): the chart fills its container and the longitude or
  # latitude range is widened around the region to keep the shapes
  k <- cos(mean(c(bbox$lat_min, bbox$lat_max)) * pi / 180)
  step <- if (max(bbox$lon_max - bbox$lon_min, bbox$lat_max - bbox$lat_min) > 25) 10 else 5
  fit <- highcharter::JS(glue::glue(
    "function () {{
      var chart = this;
      var fit = function () {{
        var w = chart.plotWidth, h = chart.plotHeight, size = w + 'x' + h;
        if (chart.fittedSize === size) return;
        chart.fittedSize = size;
        var s = Math.max(({bbox$lon_max} - ({bbox$lon_min})) * {k} / w, ({bbox$lat_max} - ({bbox$lat_min})) / h);
        var cx = ({bbox$lon_min} + {bbox$lon_max}) / 2, cy = ({bbox$lat_min} + {bbox$lat_max}) / 2;
        chart.xAxis[0].setExtremes(cx - s * w / {k} / 2, cx + s * w / {k} / 2, false);
        chart.yAxis[0].setExtremes(cy - s * h / 2, cy + s * h / 2, false);
        chart.redraw(false);
      }};
      fit();
      Highcharts.addEvent(chart, 'redraw', fit);
    }}"
  ))

  degrees <- function(east, west) {
    highcharter::JS(glue::glue(
      "function () {{ var v = this.value; ",
      "return Math.abs(v) + '\u00b0' + (v > 0 ? '{east}' : (v < 0 ? '{west}' : '')); }}"
    ))
  }

  # veil over the area outside the region (no data there)
  outside <- function(min, max) {
    list(
      list(from = -1000, to = min, color = "rgba(255, 255, 255, 0.65)", zIndex = 5),
      list(from = max, to = 1000, color = "rgba(255, 255, 255, 0.65)", zIndex = 5)
    )
  }

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(
      plotBackgroundColor = "#eef4fa",
      animation = FALSE,
      events = list(load = fit)
    ) |>
    highcharter::hc_title(text = geology_title(title, bbox)) |>
    highcharter::hc_caption(
      text = "Data: USGS (M \u2265 5, 1975\u20132025), Smithsonian Global Volcanism Program"
    ) |>
    highcharter::hc_xAxis(
      min = bbox$lon_min, max = bbox$lon_max, tickInterval = step,
      startOnTick = FALSE, endOnTick = FALSE,
      gridLineWidth = 1, gridLineColor = "#b8c8d8", lineWidth = 0,
      title = list(text = ""), labels = list(formatter = degrees("E", "W")),
      plotBands = outside(bbox$lon_min, bbox$lon_max)
    ) |>
    highcharter::hc_yAxis(
      min = bbox$lat_min, max = bbox$lat_max, tickInterval = step,
      startOnTick = FALSE, endOnTick = FALSE,
      gridLineWidth = 1, gridLineColor = "#b8c8d8",
      title = list(text = ""), labels = list(formatter = degrees("N", "S")),
      plotBands = outside(bbox$lat_min, bbox$lat_max)
    ) |>
    highcharter::hc_plotOptions(series = list(animation = FALSE, turboThreshold = 0)) |>
    highcharter::hc_legend(title = list(text = "Depth"))

  # land: one polygon series per ring, out of the legend and the tooltip
  # wide margin: the visible range can grow beyond the region
  rings <- geology_land(bbox, margin = 15) |>
    sf::st_cast("POLYGON") |>
    suppressWarnings() |>
    purrr::map(\(polygon) unname(polygon[[1]]))
  for (ring in rings) {
    hc <- hc |>
      highcharter::hc_add_series(
        type = "polygon",
        data = highcharter::list_parse2(as.data.frame(ring)),
        color = "#d2d2d2",
        lineColor = "#5f5f5f",
        lineWidth = 0.6,
        showInLegend = FALSE,
        enableMouseTracking = FALSE
      )
  }

  quakes <- geology_quakes(region)
  symbols <- c("circle", "square", "diamond")
  for (i in seq_along(geology_depth_labels)) {
    points <- quakes |>
      dplyr::filter(depth_class == geology_depth_labels[i]) |>
      purrr::pmap(\(lon, lat, depth, mag, place, year, ...) {
        list(
          x = lon, y = lat, depth = depth, mag = mag, place = place, year = year,
          marker = list(radius = round(1.5 + (mag - 5) * 1.6, 1))
        )
      })
    hc <- hc |>
      highcharter::hc_add_series(
        type = "scatter",
        name = geology_depth_labels[i],
        data = points,
        color = geology_depth_colors[i],
        marker = list(symbol = symbols[i], lineColor = "#333333", lineWidth = 0.4)
      )
  }

  volcanoes <- geology_volcanoes(region) |>
    purrr::pmap(\(lon, lat, name, type, last_eruption, ...) {
      list(x = lon, y = lat, name = name, type = type, last_eruption = last_eruption)
    })

  hc |>
    highcharter::hc_add_series(
      type = "scatter",
      name = "Volcano",
      data = volcanoes,
      color = geology_volcano_color,
      marker = list(symbol = "triangle", radius = 5, lineColor = "#7a0000", lineWidth = 0.5)
    ) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      formatter = highcharter::JS(
        "function () {
          var p = this.point;
          if (this.series.name === 'Volcano') {
            return '<b>' + p.name + '</b><br/>' + p.type +
              (p.last_eruption !== null ? '<br/>Last eruption: ' + p.last_eruption : '');
          }
          return '<b>M ' + p.mag + '</b>, depth ' + Math.round(p.depth) + ' km<br/>' +
            (p.place || '') + '<br/>' + p.year;
        }"
      )
    )
}

# Cross-sections ----------------------------------------------------------

geology_sections <- tibble::tribble(
  ~region, ~lat, ~from, ~to,
  "A",       38,   145, 128,
  "B",      -22,   -72, -62
)

geology_cross_section_data <- function(region, lat, from, to, half_width) {
  km_per_degree <- 111.32 * cos(lat * pi / 180)
  direction <- sign(to - from)

  along <- function(data) {
    data |>
      dplyr::filter(
        abs(lat - !!lat) <= half_width,
        dplyr::between(lon, min(from, to), max(from, to))
      ) |>
      dplyr::mutate(distance = (lon - from) * direction * km_per_degree)
  }

  list(
    quakes = geology_quakes(region) |> along(),
    volcanoes = geology_volcanoes(region) |> along()
  )
}

geology_section_line <- function(region, lat, from, to) {
  section <- geology_sections[geology_sections$region == region, ]
  list(
    lat = if (is.null(lat)) section$lat else lat,
    from = if (is.null(from)) section$from else from,
    to = if (is.null(to)) section$to else to
  )
}

geology_degrees <- function(value, positive, negative) {
  paste0(abs(value), "\u00b0", ifelse(value >= 0, positive, negative))
}

#' Plate detective cross-sections
#'
#' Depth of the earthquakes against their distance from the trench, along a
#' line across a subduction zone (answer key of the plate detective exercise).
#' Earthquakes within `half_width` degrees of latitude of the line are kept;
#' volcanoes near the line are shown at the surface.
#'
#' @param region `"A"` (Japan, along 38° N from 145° E to 128° E) or `"B"`
#'   (Central Andes, along 22° S from 72° W to 62° W)
#' @param half_width Half width of the band around the line, in degrees of
#'   latitude
#' @param lat,from,to Latitude of the line and longitudes of its start (the
#'   trench side) and end, in degrees (west and south negative). `NULL` uses
#'   the default line of the region.
#' @param title `TRUE` for a title with the region and the line, `FALSE` for
#'   no title, or a string
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 graph (`df_geology_graph_cross_section()`) or a
#'   highcharts chart (`df_geology_hc_cross_section()`)
#' @export
#'
#' @examples
#' df_geology_graph_cross_section("A")
#' df_geology_hc_cross_section("B")
df_geology_graph_cross_section <- function(
  region = c("A", "B"),
  half_width = 2,
  lat = NULL,
  from = NULL,
  to = NULL,
  title = TRUE,
  theme = ggplot2::theme_minimal()
) {
  region <- match.arg(region)
  line <- geology_section_line(region, lat, from, to)
  data <- geology_cross_section_data(region, line$lat, line$from, line$to, half_width)
  bbox <- geology_bbox(region)

  if (isTRUE(title)) {
    title <- glue::glue(
      "{bbox$region} \u2013 {bbox$name}: cross-section along ",
      "{geology_degrees(line$lat, 'N', 'S')}"
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "#6b6b6b", linewidth = 0.3) +
    ggplot2::geom_point(
      data = data$quakes,
      ggplot2::aes(distance, depth, fill = depth_class, shape = depth_class, size = mag),
      color = "#333333",
      stroke = 0.2,
      alpha = 0.85,
      show.legend = TRUE
    ) +
    ggplot2::geom_point(
      data = data$volcanoes,
      ggplot2::aes(distance, -15, color = "Volcano"),
      shape = 17,
      size = 2.4
    ) +
    ggplot2::scale_y_reverse(name = "Depth (km)") +
    ggplot2::scale_x_continuous(name = "Distance from the trench (km)") +
    ggplot2::scale_fill_manual(
      values = geology_depth_fills,
      drop = FALSE,
      name = "Depth"
    ) +
    ggplot2::scale_shape_manual(
      values = geology_depth_point_shapes,
      drop = FALSE,
      name = "Depth"
    ) +
    ggplot2::scale_size_continuous(
      range = c(0.5, 3.5),
      limits = c(5, 9.5),
      breaks = c(5, 6, 7, 8, 9),
      name = "Magnitude"
    ) +
    ggplot2::scale_color_manual(values = c(Volcano = geology_volcano_color), name = NULL) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1,
        override.aes = list(size = 3, shape = geology_depth_shapes, fill = geology_depth_colors)
      ),
      shape = ggplot2::guide_legend(order = 1),
      size = ggplot2::guide_legend(order = 2),
      color = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(
      title = if (isFALSE(title)) NULL else title,
      subtitle = glue::glue(
        "Earthquakes within {half_width}\u00b0 of the line, from ",
        "{geology_degrees(line$from, 'E', 'W')} to {geology_degrees(line$to, 'E', 'W')}"
      ),
      caption = "Data: USGS (M 5+, 1975\u20132025), Smithsonian Global Volcanism Program"
    ) +
    theme
}

#' @rdname df_geology_graph_cross_section
#' @export
df_geology_hc_cross_section <- function(
  region = c("A", "B"),
  half_width = 2,
  lat = NULL,
  from = NULL,
  to = NULL,
  title = TRUE
) {
  region <- match.arg(region)
  line <- geology_section_line(region, lat, from, to)
  data <- geology_cross_section_data(region, line$lat, line$from, line$to, half_width)
  bbox <- geology_bbox(region)

  if (isTRUE(title)) {
    title <- glue::glue(
      "{bbox$region} \u2013 {bbox$name}: cross-section along ",
      "{geology_degrees(line$lat, 'N', 'S')}"
    )
  }

  hc <- highcharter::highchart() |>
    highcharter::hc_chart(animation = FALSE) |>
    highcharter::hc_title(text = if (isFALSE(title)) NULL else title) |>
    highcharter::hc_caption(
      text = "Data: USGS (M \u2265 5, 1975\u20132025), Smithsonian Global Volcanism Program"
    ) |>
    highcharter::hc_xAxis(title = list(text = "Distance from the trench (km)"), min = 0) |>
    highcharter::hc_yAxis(
      title = list(text = "Depth (km)"),
      reversed = TRUE, min = -40, max = 700, startOnTick = FALSE,
      labels = list(formatter = highcharter::JS("function () { return this.value >= 0 ? this.value : ''; }"))
    ) |>
    highcharter::hc_plotOptions(series = list(animation = FALSE, turboThreshold = 0))

  symbols <- c("circle", "square", "diamond")
  for (i in seq_along(geology_depth_labels)) {
    points <- data$quakes |>
      dplyr::filter(depth_class == geology_depth_labels[i]) |>
      purrr::pmap(\(distance, depth, mag, place, year, ...) {
        list(
          x = round(distance), y = depth, mag = mag, place = place, year = year,
          marker = list(radius = round(1.5 + (mag - 5) * 1.6, 1))
        )
      })
    hc <- hc |>
      highcharter::hc_add_series(
        type = "scatter",
        name = geology_depth_labels[i],
        data = points,
        color = geology_depth_colors[i],
        marker = list(symbol = symbols[i], lineColor = "#333333", lineWidth = 0.4)
      )
  }

  volcanoes <- data$volcanoes |>
    purrr::pmap(\(distance, name, ...) list(x = round(distance), y = -15, name = name))

  hc |>
    highcharter::hc_add_series(
      type = "scatter",
      name = "Volcano",
      data = volcanoes,
      color = geology_volcano_color,
      marker = list(symbol = "triangle", radius = 5, lineColor = "#7a0000", lineWidth = 0.5)
    ) |>
    highcharter::hc_tooltip(
      formatter = highcharter::JS(
        "function () {
          var p = this.point;
          if (this.series.name === 'Volcano') {
            return '<b>' + p.name + '</b><br/>' + p.x + ' km from the trench';
          }
          return '<b>M ' + p.mag + '</b>, depth ' + Math.round(p.y) + ' km<br/>' +
            p.x + ' km from the trench<br/>' + (p.place || '') + '<br/>' + p.year;
        }"
      )
    )
}

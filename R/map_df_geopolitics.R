# Sea zones ----------------------------------------------------------------------------------

# An imaginary coast to draw the UNCLOS sea zones. Units are nautical miles
# (nm), plain cartesian coordinates (no CRS). Printed 17 cm wide, the 340 nm
# wide map is at 1 cm = 20 nm (1:3 704 000).

geopolitics_sea_zones_window <- c(xmin = 0, xmax = 340, ymin = 0, ymax = 380)

geopolitics_sea_zones_poly <- function(m) {
  sf::st_polygon(list(rbind(m, m[1, ])))
}

geopolitics_sea_zones_island <- function(x, y, r, n = 9, seed = 1) {
  a <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  # no global seed: same shapes, the caller's random state is left alone
  rr <- r * withr::with_seed(seed, stats::runif(n, 0.75, 1.25))
  geopolitics_sea_zones_poly(cbind(x + rr * cos(a), y + rr * sin(a)))
}

# Coast of Aldoria, north to south, and of Borania, west to east
geopolitics_sea_zones_coast_a <- rbind(
  c(60, 420), c(56, 365), c(61, 345), c(59, 322), c(64, 300), c(61, 275),
  c(66, 252), c(73, 233), c(70, 212), c(64, 192),
  c(68, 172),                                     # north headland of the bay
  c(50, 176), c(37, 172), c(31, 164), c(34, 155), c(44, 150), c(55, 147),
  c(66, 150),                                     # south headland of the bay
  c(61, 125), c(64, 102), c(58, 80), c(56, 55), c(50, 30), c(46, 0), c(42, -40)
)
geopolitics_sea_zones_coast_b <- rbind(
  c(215, 420), c(222, 380), c(240, 368), c(262, 352), c(275, 349), c(292, 336),
  c(310, 326), c(326, 318), c(345, 310), c(420, 300)
)

geopolitics_sea_zones_geometry <- function() {
  land_a <- geopolitics_sea_zones_poly(rbind(geopolitics_sea_zones_coast_a, c(-60, -40), c(-60, 420)))
  land_b <- geopolitics_sea_zones_poly(rbind(geopolitics_sea_zones_coast_b, c(420, 420)))
  islands <- list(
    geopolitics_sea_zones_island(80, 262, 3.2, seed = 2),
    geopolitics_sea_zones_island(85, 287, 2.6, seed = 3),
    geopolitics_sea_zones_island(80, 314, 3.0, seed = 4)
  )
  # Kalos: an island of Aldoria about 17 nm off the coast of Borania
  kalos <- geopolitics_sea_zones_island(264.8, 332.1, 1.6, seed = 5)

  # Internal waters: the closed bay (mouth 22 nm) and the waters inside the
  # straight baselines along the Merin Islands
  bay <- geopolitics_sea_zones_poly(rbind(
    c(68, 172), c(50, 176), c(37, 172), c(31, 164), c(34, 155), c(44, 150),
    c(55, 147), c(66, 150)
  ))
  fringe <- geopolitics_sea_zones_poly(rbind(
    c(73, 233), c(82.5, 259.5), c(87.5, 286), c(83, 313), c(61, 345),
    c(59, 322), c(64, 300), c(61, 275), c(66, 252)
  ))

  list(
    land_a = sf::st_sfc(land_a),
    land_b = sf::st_sfc(land_b),
    islands = sf::st_sfc(islands),
    kalos = sf::st_sfc(kalos),
    # the zones of Aldoria are measured from its baselines
    base_a = sf::st_union(sf::st_sfc(c(list(land_a, bay, fringe), islands))),
    base_b = sf::st_sfc(land_b),
    baseline_lines = sf::st_sfc(
      sf::st_linestring(rbind(c(66, 150), c(68, 172))),
      sf::st_linestring(rbind(c(73, 233), c(82.5, 259.5), c(87.5, 286), c(83, 313), c(61, 345)))
    ),
    fields = tibble::tibble(
      label = c("Gas\nfield 1", "Gas\nfield 2", "Nodules"),
      fill = c("#f6c04d", "#f6c04d", "#9e9e9e"),
      geometry = sf::st_sfc(
        geopolitics_sea_zones_island(150, 352, 9, n = 14, seed = 6),
        geopolitics_sea_zones_island(224, 300, 8, n = 14, seed = 7),
        geopolitics_sea_zones_island(305, 42, 11, n = 14, seed = 8)
      )
    ) |> sf::st_as_sf()
  )
}

geopolitics_sea_zones_box <- function(w = geopolitics_sea_zones_window, pad = 0) {
  sf::st_as_sfc(sf::st_bbox(c(
    xmin = w[["xmin"]] - pad, ymin = w[["ymin"]] - pad,
    xmax = w[["xmax"]] + pad, ymax = w[["ymax"]] + pad
  )))
}

geopolitics_sea_zones_clip <- function(x, pad = 0) {
  suppressWarnings(sf::st_intersection(sf::st_sfc(x), geopolitics_sea_zones_box(pad = pad)))
}

# Zero line of a distance difference computed on a 0.5 nm grid
geopolitics_sea_zones_isoline <- function(xs, ys, z) {
  iso <- isoband::isolines(xs, ys, t(matrix(z, nrow = length(xs))), levels = 0)
  sf::st_sfc(isoband::iso_to_sfg(iso)[[1]])
}

geopolitics_sea_zones_isoband <- function(xs, ys, z) {
  iso <- isoband::isobands(xs, ys, t(matrix(z, nrow = length(xs))), levels_low = -1e6, levels_high = 0)
  sf::st_make_valid(sf::st_sfc(isoband::iso_to_sfg(iso)[[1]]))
}

# Boundaries between Aldoria and Borania. With Kalos an island, it counts like
# any coast; with Kalos a rock (UNCLOS art. 121.3) it keeps its territorial
# sea only, an enclave in the waters of Borania.
geopolitics_sea_zones_boundaries <- function(geo, kalos = c("island", "rock")) {
  kalos <- match.arg(kalos)
  xs <- seq(-20, 360, by = 0.5)
  ys <- seq(-20, 400, by = 0.5)
  pts <- sf::st_as_sf(expand.grid(x = xs, y = ys), coords = c("x", "y"))
  d_a <- as.numeric(sf::st_distance(pts, geo$base_a))
  d_b <- as.numeric(sf::st_distance(pts, geo$base_b))
  d_k <- as.numeric(sf::st_distance(pts, geo$kalos))

  if (kalos == "island") {
    base_a <- sf::st_union(c(geo$base_a, geo$kalos))
    z <- pmin(d_a, d_k) - d_b
    side_a <- geopolitics_sea_zones_isoband(xs, ys, z)
    median <- geopolitics_sea_zones_isoline(xs, ys, z)
  } else {
    base_a <- geo$base_a
    z <- d_a - d_b
    enclave <- geopolitics_sea_zones_isoband(xs, ys, ifelse(d_k <= 12, d_k - d_b, 1))
    side_a <- sf::st_union(c(geopolitics_sea_zones_isoband(xs, ys, z), enclave))
    # waters closer to Kalos than to Borania, to cut its territorial sea
    side_k <- geopolitics_sea_zones_isoband(xs, ys, d_k - d_b)
    median <- c(
      geopolitics_sea_zones_isoline(xs, ys, z),
      # between the territorial seas of Kalos and Borania
      suppressWarnings(sf::st_intersection(
        geopolitics_sea_zones_isoline(xs, ys, d_k - d_b),
        sf::st_buffer(geo$kalos, 12)
      ))
    )
  }
  # a median line exists only where both states reach 200 nm
  both <- sf::st_intersection(
    sf::st_buffer(base_a, 200, nQuadSegs = 60),
    sf::st_buffer(geo$base_b, 200, nQuadSegs = 60)
  )
  median <- suppressWarnings(sf::st_intersection(sf::st_union(median), both))
  side_b <- sf::st_difference(geopolitics_sea_zones_box(pad = 100), side_a)
  list(
    base_a = base_a, side_a = side_a, side_b = side_b, median = median,
    side_k = if (kalos == "rock") side_k else NULL
  )
}

# Outer limits of the zones of one state, kept on its side and off land
geopolitics_sea_zones_limits <- function(base, side, land, dists = c(12, 24, 200)) {
  out <- lapply(dists, function(d) {
    ring <- sf::st_boundary(sf::st_buffer(base, d, nQuadSegs = 60))
    l <- suppressWarnings(sf::st_difference(sf::st_intersection(ring, side), land))
    g <- geopolitics_sea_zones_clip(sf::st_union(l))
    if (length(g) == 0) return(NULL)
    sf::st_sf(limit = d, geometry = g)
  })
  do.call(rbind, Filter(Negate(is.null), out))
}

#' Draw the sea zones
#'
#' An imaginary coast with two states, Aldoria and Borania, for drawing the
#' maritime zones of the United Nations Convention on the Law of the Sea
#' (UNCLOS) with a compass: internal waters, territorial sea (12 nm),
#' contiguous zone (24 nm), exclusive economic zone (200 nm), median line
#' between the two states and the high seas. The key versions draw the zones,
#' with the island of Kalos counting either as an island (it has its own
#' zones) or as a rock (UNCLOS art. 121.3: territorial sea only, an enclave).
#'
#' Units are nautical miles. The map is 340 nm × 380 nm with no side margins:
#' printed 17 cm wide (figure 17 cm × 19.9 cm), the scale is 1 cm = 20 nm.
#' Pass a theme without axis text, titles or side margins, otherwise the panel
#' shrinks and the scale is lost. A key version takes a few seconds (distances
#' computed on a 0.5 nm grid for the median line).
#'
#' @param key `"none"` for the student map, `"island"` or `"rock"` for the
#'   answer key with Kalos counting as an island or as a rock
#' @param theme A ggplot2 theme
#'
#' @returns A ggplot2 map
#' @export
#'
#' @examples
#' df_geopolitics_map_sea_zones()
#' df_geopolitics_map_sea_zones("rock")
df_geopolitics_map_sea_zones <- function(
  key = c("none", "island", "rock"),
  theme = ggplot2::theme_void()
) {
  key <- match.arg(key)
  geo <- geopolitics_sea_zones_geometry()
  w <- geopolitics_sea_zones_window
  land <- sf::st_union(c(geo$land_a, geo$land_b, geo$islands, geo$kalos))

  ink <- "#333333"
  land_a_fill <- "#f1e6cf"
  land_b_fill <- "#e3ead3"
  zone_col <- c("12" = "#1f5fa8", "24" = "#2a9d8f", "200" = "#c0392b")
  median_col <- "#6a1b9a"

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = geopolitics_sea_zones_box(), fill = "#e8f1f8", color = NA) +
    ggplot2::geom_sf(data = geopolitics_sea_zones_clip(geo$land_a), fill = land_a_fill, color = ink, linewidth = 0.35) +
    ggplot2::geom_sf(data = geopolitics_sea_zones_clip(geo$land_b), fill = land_b_fill, color = ink, linewidth = 0.35) +
    ggplot2::geom_sf(data = c(geo$islands, geo$kalos), fill = land_a_fill, color = ink, linewidth = 0.35) +
    ggplot2::geom_sf(data = geo$fields, ggplot2::aes(fill = I(fill)), color = "#666666", linewidth = 0.3, alpha = 0.85)

  if (key != "none") {
    bd <- geopolitics_sea_zones_boundaries(geo, key)
    lims <- rbind(
      geopolitics_sea_zones_limits(bd$base_a, bd$side_a, land),
      geopolitics_sea_zones_limits(geo$base_b, bd$side_b, land)
    )
    if (key == "rock") {
      lims <- rbind(lims, geopolitics_sea_zones_limits(geo$kalos, bd$side_k, land, dists = 12))
    }
    lims$limit <- factor(lims$limit, levels = c(12, 24, 200))
    p <- p +
      ggplot2::geom_sf(data = geo$baseline_lines, color = ink, linewidth = 0.6, linetype = "22") +
      ggplot2::geom_sf(data = lims, ggplot2::aes(color = limit), linewidth = 0.55, show.legend = FALSE) +
      ggplot2::geom_sf(data = geopolitics_sea_zones_clip(bd$median), color = median_col, linewidth = 1) +
      ggplot2::scale_color_manual(values = zone_col, guide = "none") +
      ggplot2::annotate("text", x = 300, y = 80, label = "HIGH SEAS", size = 2.6, fontface = "bold", color = ink)
  }

  labels <- tibble::tribble(
    ~x, ~y, ~label, ~size, ~face, ~angle,
    25, 240, "ALDORIA", 4.2, "bold", 90,
    305, 365, "BORANIA", 4.2, "bold", 0,
    46, 163, "Bay of\nSaro", 2.3, "italic", 0,
    103, 288, "Merin\nIslands", 2.5, "italic", 0,
    256, 323, "Kalos\n(Aldoria)", 2.4, "italic", 0,
    150, 352, "Gas\nfield 1", 2.3, "plain", 0,
    224, 300, "Gas\nfield 2", 2.3, "plain", 0,
    305, 42, "Nodules", 2.3, "plain", 0,
    140, 230, "Mare Medium", 3.2, "italic", 0,
    200, 20, "Open Ocean", 3.2, "italic", 0
  )
  labels$y[labels$label == "Kalos\n(Aldoria)"] <- 320
  p <- p + ggplot2::geom_text(
    data = labels,
    ggplot2::aes(x, y, label = label, size = I(size), fontface = face, angle = angle),
    color = ink, lineheight = 0.85
  )

  # Scale bar under the map: 0-100 nm, ticks every 10 nm, 12 and 24 marked
  sb_y <- -9
  p <- p +
    ggplot2::annotate("segment", x = 10, xend = 110, y = sb_y, yend = sb_y, color = ink, linewidth = 0.5) +
    ggplot2::annotate(
      "segment",
      x = seq(10, 110, 10),
      xend = seq(10, 110, 10),
      y = sb_y,
      yend = sb_y + 2.5,
      color = ink,
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "segment",
      x = c(22, 34),
      xend = c(22, 34),
      y = sb_y,
      yend = sb_y - 2.5,
      color = ink,
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = c(10, 60, 110),
      y = sb_y + 6,
      label = c("0", "50", "100 nm"),
      size = 2.5,
      color = ink
    ) +
    ggplot2::annotate("text", x = c(22, 34), y = sb_y - 6, label = c("12", "24"), size = 2.2, color = ink) +
    ggplot2::annotate(
      "text",
      x = 122,
      y = sb_y + 3,
      label = "1 cm = 20 nautical miles",
      hjust = 0,
      size = 2.6,
      color = ink
    ) +
    ggplot2::annotate("text", x = 122, y = sb_y - 4, label = "1 nm = 1.852 km", hjust = 0, size = 2.3, color = ink)

  if (key != "none") {
    leg <- tibble::tibble(
      x = c(205, 205, 270, 270, 270),
      y = c(-5, -13, -3, -9, -15),
      col = c(ink, median_col, zone_col),
      lt = c("22", "solid", "solid", "solid", "solid"),
      lw = c(0.6, 1, 0.55, 0.55, 0.55),
      label = c("straight baseline", "median line", "12 nm", "24 nm", "200 nm")
    )
    p <- p +
      ggplot2::geom_segment(
        data = leg,
        ggplot2::aes(x = x, xend = x + 10, y = y, yend = y, color = I(col), linetype = I(lt), linewidth = I(lw))
      ) +
      ggplot2::geom_text(data = leg, ggplot2::aes(x = x + 13, y = y, label = label), hjust = 0, size = 2.2, color = ink)
  }

  p +
    ggplot2::coord_sf(
      xlim = c(w[["xmin"]], w[["xmax"]]), ylim = c(w[["ymin"]], w[["ymax"]]),
      expand = FALSE, clip = "off", datum = NA
    ) +
    theme +
    ggplot2::theme(
      # nothing beside the panel (legend, axis titles or text, as some themes
      # add them), otherwise it shrinks and the scale is lost; the map draws
      # its own legend under the panel
      legend.position = "none",
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 0.9, 0, "cm"),
      panel.border = ggplot2::element_rect(fill = NA, color = ink, linewidth = 0.4)
    )
}

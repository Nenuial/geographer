#' Create a globe with the countries in the list highlighted
#'
#' @param countries A vector of country names
#'
#' @return A ggplot map
#' @export
#' @examples
#' gph_mini_globe("Switzerland")
gph_mini_globe <- function(countries) {
  rnaturalearth::ne_countries(
    scale = 50, country = countries,
    returnclass = "sf"
  ) |>
    sf::st_union() |>
    sf::st_centroid() |>
    purrr::pluck(1) |>
    as.numeric() -> coords

  countries |>
    countrycode::countrycode(origin = "country.name", destination ="iso3c") -> country_codes

  withr::with_options(
    list(sf_use_s2 = FALSE),
    gph_mini_globe_render(country_codes, coords)
  )
}

#' Internal: render mini globe
#'
#' @param country_codes The list of countries to highlight
#' @param coords The coordinates
#'
#' @return A ggplot map
#' @keywords internal
gph_mini_globe_render <- function(country_codes, coords) {
  mini_world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

  # Define the orthographic projection
  # Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
  lon <- coords[[1]]
  lat <- coords[[2]]
  ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
  # Define the polygon that will help you finding the "blade"
  # to split what lies within and without your projection
  circle <- sf::st_point(x = c(0,0)) |>
    sf::st_buffer(dist = 6371000) |>
    sf::st_sfc(crs = ortho)
  # Project this polygon in lat-lon
  circle_longlat <- circle |> sf::st_transform(crs = 4326)
  # circle_longlat cannot be used as it is
  # You must decompose it into a string with ordered longitudes
  # Then complete the polygon definition to cover the hemisphere
  if(lat != 0) {
    circle_longlat <- sf::st_boundary(circle_longlat)
    circle_coords <- sf::st_coordinates(circle_longlat)[, c(1,2)]
    circle_coords <- circle_coords[order(circle_coords[, 1]),]
    circle_coords <- circle_coords[!duplicated(circle_coords),]
    # Rebuild line
    circle_longlat <- sf::st_linestring(circle_coords) |>
      sf::st_sfc(crs = 4326)
    if(lat > 0) {
      rectangle <- list(rbind(circle_coords,
                              c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                              c(X = 180, Y = 90),
                              c(X = -180, Y = 90),
                              c(X = -180, circle_coords[1, 'Y']),
                              circle_coords[1, c('X','Y')])) |>
        sf::st_polygon() |> sf::st_sfc(crs = 4326)
    } else {
      rectangle <- list(rbind(circle_coords,
                              c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                              c(X = 180, Y = -90),
                              c(X = -180, Y = -90),
                              c(X = -180, circle_coords[1, 'Y']),
                              circle_coords[1, c('X','Y')])) |>
        sf::st_polygon() |> sf::st_sfc(crs = 4326)
    }
    circle_longlat <- sf::st_union(sf::st_make_valid(circle_longlat), sf::st_make_valid(rectangle))
  }
  # This visualization shows the visible emisphere in red
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = mini_world) +
  #   ggplot2::geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3)
  # A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
  # I should not change the shapes too much
  visible <- sf::st_intersection(sf::st_make_valid(mini_world), sf::st_buffer(circle_longlat, -0.09)) |>
    sf::st_transform(crs = ortho)
  # DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach
  # Resulting polygons are often broken and they need to be fixed
  # Get reason why they're broken
  broken_reason <- sf::st_is_valid(visible, reason = TRUE)
  # First fix NA's by decomposing them
  # Remove them from visible for now
  na_visible <- visible[is.na(broken_reason),]
  visible <- visible[!is.na(broken_reason),]
  # Open and close polygons
  na_visible <- sf::st_cast(na_visible, 'MULTILINESTRING') |>
    sf::st_cast('LINESTRING', do_split=TRUE)
  na_visible <- na_visible |>
    dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE))
  # Exclude polygons with less than 4 points
  na_visible <- na_visible |>
    dplyr::filter(npts >=4) |>
    dplyr::select(-npts) |>
    sf::st_cast('POLYGON')
  # Fix other broken polygons
  broken <- which(!sf::st_is_valid(visible))
  for(land in broken) {
    result = tryCatch({
      # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
      visible[land,] <- sf::st_make_valid(visible[land,]) |>
        sf::st_collection_extract()
    }, error = function(e) {
      visible[land,] <<- sf::st_buffer(visible[land,], 0)
    })
  }
  # Bind together the two tables
  visible <- rbind(visible, na_visible)
  # Final plot
  ggplot2::ggplot(data = sf::st_collection_extract(visible)) +
    ggplot2::geom_sf(data = circle,
                     fill = 'aliceblue') + # if you like the color
    ggplot2::geom_sf(fill = "#2b592a", size = .1) +
    ggplot2::coord_sf(crs = ortho) +
    gghighlight::gghighlight(adm0_a3 %in% country_codes) +
    ggplot2::theme_void()
}

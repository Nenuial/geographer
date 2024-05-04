# This is just some stuff that need finishing…
#
# geodata::oc_russie_2020_population_municipale %>%
#   dplyr::mutate(change = (`2020`-`2016`) / `2020` * 100) %>%
#   dplyr::mutate(change_cat = santoku::chop(change,
#                                            c(-100000,-5,-2.5,0,2.5,5,100000))) -> pop_data
#
# geodata::oc_russie_adm6_gis %>%
#   dplyr::mutate(oktmo = as.character(oktmo)) %>%
#   sf::st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
#   dplyr::left_join(pop_data, by = "oktmo") %>%
#   ggplot2::ggplot(ggplot2::aes(fill = change_cat, geometry = geometry)) +
#   ggplot2::geom_sf(size = 0.1) +
#   ggplot2::scale_fill_manual(
#     values = prismatic::clr_alpha(
#       paletteer::paletteer_c("pals::ocean.curl", direction = -1, 8)[-3],
#       alpha = .8
#     ),
#     breaks = levels(pop_data$change_cat),
#     limits = levels(pop_data$change_cat)
#   ) +
#   ggplot2::guides(
#     fill = ggplot2::guide_coloursteps(
#       even.steps = TRUE, show.limits = FALSE,
#       barheight = 10, label.hjust = 1
#     )
#   ) +
#   ggplot2::coord_sf(crs = geotools::gtl_crs_proj("Russia"), datum = NA)

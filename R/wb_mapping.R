#' Create world maps for an indicator
#'
#' @param indicator A list of parameters
#'
#' @return Nothing
#' @export
geo_wb_world_map <- function(indicator) {
  df <- geo_wb_data(indicator)

  indicator$years %>%
    purrr::map(~geo_plot_world(df %>% dplyr::filter(date == .x),
                               title = indicator$title,
                               year = .x,
                               unit = indicator$unit,
                               palette = ggeo::return_palette(df %>%
                                                                pull(cut) %>%
                                                                levels() %>%
                                                                length(),
                                                              indicator$center,
                                                              indicator$palette),
                               caption = translate("World Bank", "Banque Mondiale"))) %>%
    patchwork::align_plots() %>%
    purrr::walk(~ggeo::ggeosave(glue::glue("{indicator$file}_{.x$labels$subtitle}"),
                                plot = .x, height = 31, width = 64))
}

#' Fetch World Bank data
#'
#' @param indicator A list of parameters
#'
#' @return A dataframe
#' @export
geo_wb_data <- function(indicator) {
  wbgdata::wbgdata(indicator = indicator$code,
                   years = as.numeric(indicator$years),
                   removeNA = T, cache = wbgdata::get_wbcache(),
                   rename.indicators = T) %>%
    dplyr::mutate(data = !!indicator$operation) %>%
    dplyr::mutate(data = !!indicator$operation,
                  cut = santoku::chop(data, breaks = indicator$breaks,
                                      labels = santoku::lbl_dash(),
                                      extend = T, drop = F) %>%
                    forcats::fct_relabel(chop_relabel_dash))
}

# Chokepoints --------------------------------------------------------------------------------

# Texts follow the language set with geotools::gtl_opt_set_i18n() (English
# by default): functions, so that the language is read when a chart is drawn
tr <- function(english, french) geotools::gtl_translate_enfr(english, french)

geopolitics_chokepoint_credit <- function() {
  tr(
    "Sources: UN Global Platform; IMF PortWatch (portwatch.imf.org)",
    "Sources : UN Global Platform ; IMF PortWatch (portwatch.imf.org)"
  )
}

geopolitics_chokepoint_series <- function() {
  c(
    daily = tr("Ships per day", "Navires par jour"),
    total = tr("7-day mean, all ships", "Moyenne sur 7 jours, tous les navires"),
    tanker = tr("7-day mean, tankers", "Moyenne sur 7 jours, p\u00e9troliers")
  )
}

geopolitics_chokepoint_axis <- function() tr("Ships per day", "Navires par jour")

geopolitics_chokepoint_names <- c(
  "Suez Canal" = "Canal de Suez",
  "Panama Canal" = "Canal de Panama",
  "Bab el-Mandeb Strait" = "D\u00e9troit de Bab el-Mandeb",
  "Malacca Strait" = "D\u00e9troit de Malacca",
  "Strait of Hormuz" = "D\u00e9troit d'Ormuz",
  "Cape of Good Hope" = "Cap de Bonne-Esp\u00e9rance"
)

geopolitics_chokepoint_colors <- c(daily = "#cfd8e3", total = "#1f3a5f", tanker = "#d95f02")

# Daily transits of one chokepoint with 7-day centred means, computed on the
# whole series so that the first and last days of a period have full means
geopolitics_chokepoint_data <- function(chokepoint, from, to) {
  data <- geodata::df_geopolitics_chokepoints_daily |>
    dplyr::filter(chokepoint == !!chokepoint) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      mean_total = as.numeric(stats::filter(n_total, rep(1 / 7, 7), sides = 2)),
      mean_tanker = as.numeric(stats::filter(n_tanker, rep(1 / 7, 7), sides = 2))
    )
  if (nrow(data) == 0) {
    stop("Unknown chokepoint: ", chokepoint, call. = FALSE)
  }

  from <- if (is.null(from)) min(data$date) else as.Date(from)
  to <- if (is.null(to)) max(data$date) else as.Date(to)
  dplyr::filter(data, dplyr::between(date, from, to))
}

# Month names in the language of the package options, whatever the locale
geopolitics_month_abb <- function() {
  tr(
    month.abb,
    c("janv.", "f\u00e9vr.", "mars", "avr.", "mai", "juin", "juil.", "ao\u00fbt", "sept.", "oct.", "nov.", "d\u00e9c.")
  )
}

geopolitics_month_label <- function(x) {
  months <- geopolitics_month_abb()
  ifelse(is.na(x), NA_character_, paste(months[as.integer(format(x, "%m"))], format(x, "%Y")))
}

geopolitics_date_label <- function(x) {
  paste(as.integer(format(x, "%d")), geopolitics_month_abb()[as.integer(format(x, "%m"))], format(x, "%Y"))
}

geopolitics_chokepoint_title <- function(data, key, label) {
  if (!key) {
    return(label)
  }
  paste0(
    tr(data$name[1], geopolitics_chokepoint_names[[data$name[1]]]), ", ",
    geopolitics_date_label(min(data$date)), " \u2013 ", geopolitics_date_label(max(data$date))
  )
}

geopolitics_chokepoint_events <- function(events, data) {
  if (is.null(events)) {
    return(NULL)
  }
  events |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(dplyr::between(date, min(data$date), max(data$date)))
}

#' A chokepoint in crisis
#'
#' Daily number of ships passing a maritime chokepoint (IMF PortWatch, AIS
#' estimates) over a period: daily total, 7-day centred mean of all ships and
#' 7-day centred mean of tankers. The student version shows only `label` as
#' title; the key version (`key = TRUE`) shows the name of the chokepoint and
#' the period, and marks the `events`. Texts are in English or French,
#' following [geotools::gtl_opt_set_i18n()] (`label` and `events` are passed
#' as they are).
#'
#' @param chokepoint PortWatch identifier, e.g. `"chokepoint1"` (Suez Canal),
#'   see [geodata::df_geopolitics_chokepoints_daily]
#' @param from,to First and last day (dates or `"YYYY-MM-DD"`); `NULL` for the
#'   first or the latest day available
#' @param key `TRUE` for the answer key: real name, period and events
#' @param label Title of the student version (e.g. `"Chokepoint A"`), `NULL`
#'   for none
#' @param events A data frame with columns `date` and `label` (short),
#'   shown as vertical lines in the key version
#' @param theme A ggplot2 theme
#' @param daily_color,total_color,tanker_color Colours of the daily bars and
#'   of the 7-day means of all ships and of tankers (highcharts version, e.g.
#'   for a dark theme)
#'
#' @returns A ggplot2 graph (`df_geopolitics_graph_chokepoint()`) or a
#'   highcharts chart (`df_geopolitics_hc_chokepoint()`, to be themed with
#'   e.g. [ggeo::hc_web_theme()])
#' @export
#'
#' @examples
#' df_geopolitics_graph_chokepoint(
#'   "chokepoint1", "2021-02-15", "2021-05-15",
#'   key = TRUE,
#'   events = tibble::tibble(
#'     date = c("2021-03-23", "2021-03-29"),
#'     label = c("Ever Given runs aground", "freed")
#'   )
#' )
#' df_geopolitics_hc_chokepoint("chokepoint2", "2023-01-01", "2024-12-31", label = "Chokepoint A")
df_geopolitics_graph_chokepoint <- function(
  chokepoint,
  from = NULL,
  to = NULL,
  key = FALSE,
  label = NULL,
  events = NULL,
  theme = ggplot2::theme_minimal()
) {
  data <- geopolitics_chokepoint_data(chokepoint, from, to)
  events <- if (key) geopolitics_chokepoint_events(events, data)
  colors <- geopolitics_chokepoint_colors
  series <- geopolitics_chokepoint_series()

  # month ticks, labelled every 1, 2 or 3 months depending on the period
  months <- as.numeric(diff(range(data$date))) / 30.4
  step <- if (months <= 8) "1 month" else if (months <= 14) "2 months" else "3 months"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = date)) +
    ggplot2::geom_col(ggplot2::aes(y = n_total, fill = series[["daily"]]), width = 1) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_total, color = series[["total"]], linetype = series[["total"]]),
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_tanker, color = series[["tanker"]], linetype = series[["tanker"]]),
      linewidth = 0.6,
      na.rm = TRUE
    )

  if (!is.null(events) && nrow(events) > 0) {
    p <- p +
      ggplot2::geom_vline(data = events, ggplot2::aes(xintercept = date), color = "#c0392b", linewidth = 0.4) +
      # horizontal labels at the top, alternately left and right of their
      # line, so that two close events do not overlap
      ggplot2::geom_text(
        data = dplyr::mutate(
          dplyr::arrange(events, date),
          left = seq_along(date) %% 2 == 1,
          x = date + ifelse(left, -1, 1) * as.numeric(diff(range(data$date))) * 0.008,
          hjust = ifelse(left, 1, 0)
        ),
        ggplot2::aes(x = x, y = Inf, label = label, hjust = hjust),
        vjust = 1.5, size = 2.4, color = "#c0392b"
      )
  }

  p +
    ggplot2::scale_fill_manual(values = stats::setNames(colors[["daily"]], series[["daily"]]), name = NULL) +
    # explicit order: all ships first, whatever the alphabetical order of the
    # names in the chosen language
    ggplot2::scale_color_manual(
      values = stats::setNames(colors[c("total", "tanker")], series[c("total", "tanker")]),
      breaks = unname(series[c("total", "tanker")]),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "22"), series[c("total", "tanker")]),
      breaks = unname(series[c("total", "tanker")]),
      name = NULL
    ) +
    ggplot2::scale_x_date(
      date_breaks = step,
      date_minor_breaks = "1 month",
      labels = geopolitics_month_label,
      expand = ggplot2::expansion(mult = 0.01)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2),
      linetype = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::labs(
      title = geopolitics_chokepoint_title(data, key, label),
      x = NULL,
      y = geopolitics_chokepoint_axis(),
      caption = geopolitics_chokepoint_credit()
    ) +
    theme +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "left",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(0.3, "cm"),
      legend.key.width = grid::unit(0.7, "cm")
    )
}

#' @rdname df_geopolitics_graph_chokepoint
#' @export
df_geopolitics_hc_chokepoint <- function(
  chokepoint,
  from = NULL,
  to = NULL,
  key = FALSE,
  label = NULL,
  events = NULL,
  daily_color = "#cfd8e3",
  total_color = "#1f3a5f",
  tanker_color = "#d95f02"
) {
  data <- geopolitics_chokepoint_data(chokepoint, from, to) |>
    dplyr::mutate(x = highcharter::datetime_to_timestamp(date))
  events <- if (key) geopolitics_chokepoint_events(events, data)
  colors <- c(daily = daily_color, total = total_color, tanker = tanker_color)
  # dates and decimals in the language of the package options
  months <- paste0("['", paste(geopolitics_month_abb(), collapse = "','"), "']")
  decimal <- tr(".", ",")
  series <- geopolitics_chokepoint_series()

  plot_lines <- if (!is.null(events) && nrow(events) > 0) {
    purrr::map2(events$date, events$label, \(date, label) {
      list(
        value = highcharter::datetime_to_timestamp(date),
        color = "#c0392b",
        width = 1,
        zIndex = 3,
        label = list(text = label, rotation = 90, style = list(color = "#c0392b", fontSize = "11px"))
      )
    })
  }

  points <- function(column, digits) {
    purrr::map2(data$x, round(data[[column]], digits), \(x, y) list(x, if (is.na(y)) NULL else y))
  }

  highcharter::highchart() |>
    highcharter::hc_chart(animation = FALSE) |>
    highcharter::hc_title(text = geopolitics_chokepoint_title(data, key, label)) |>
    highcharter::hc_caption(text = geopolitics_chokepoint_credit()) |>
    highcharter::hc_xAxis(
      type = "datetime",
      plotLines = plot_lines,
      labels = list(formatter = highcharter::JS(glue::glue(
        "function () {{
          var d = new Date(this.value), months = {months};
          var info = this.axis.tickPositions.info, unit = info ? info.unitName : 'month';
          if (unit === 'day' || unit === 'week') return d.getUTCDate() + ' ' + months[d.getUTCMonth()];
          if (unit === 'year') return d.getUTCFullYear();
          return months[d.getUTCMonth()] + ' ' + d.getUTCFullYear();
        }}"
      )))
    ) |>
    highcharter::hc_yAxis(min = 0, title = list(text = geopolitics_chokepoint_axis())) |>
    highcharter::hc_tooltip(
      shared = TRUE,
      formatter = highcharter::JS(glue::glue(
        "function () {{
          var d = new Date(this.x), months = {months};
          var s = '<span style=\"font-size: 0.8em\">' + d.getUTCDate() + ' ' + months[d.getUTCMonth()] +
            ' ' + d.getUTCFullYear() + '</span>';
          this.points.forEach(function (p) {{
            s += '<br/><span style=\"color:' + p.color + '\">\u25CF</span> ' + p.series.name + ': <b>' +
              String(p.y).replace('.', '{decimal}') + '</b>';
          }});
          return s;
        }}"
      ))
    ) |>
    highcharter::hc_plotOptions(
      series = list(animation = FALSE, turboThreshold = 0, marker = list(enabled = FALSE)),
      column = list(pointPadding = 0, groupPadding = 0, borderWidth = 0)
    ) |>
    highcharter::hc_add_series(
      type = "column", name = series[["daily"]], data = points("n_total", 0),
      color = colors[["daily"]]
    ) |>
    highcharter::hc_add_series(
      type = "line", name = series[["total"]], data = points("mean_total", 1),
      color = colors[["total"]], lineWidth = 2
    ) |>
    highcharter::hc_add_series(
      type = "line", name = series[["tanker"]], data = points("mean_tanker", 1),
      color = colors[["tanker"]], lineWidth = 2, dashStyle = "ShortDash"
    )
}

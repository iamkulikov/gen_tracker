trackerPlotPalette <- function(n = 4) {
  colors <- c("#1f4e79", "#c55a11", "#2e7d32", "#7b1fa2")
  if (n <= length(colors)) {
    return(colors[seq_len(n)])
  }
  grDevices::colorRampPalette(colors)(n)
}

trackerPlotTheme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 3,
        color = "#1a202c",
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1,
        color = "#4a5568",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 2,
        color = "#718096",
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      panel.grid.major = ggplot2::element_line(color = "#e2e8f0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "#cbd5e0", fill = NA, linewidth = 0.5),
      axis.title = ggplot2::element_text(color = "#2d3748", face = "bold"),
      axis.text = ggplot2::element_text(color = "#4a5568"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold", color = "#2d3748"),
      legend.text = ggplot2::element_text(color = "#4a5568"),
      legend.key.width = grid::unit(1.4, "cm"),
      legend.margin = ggplot2::margin(t = 4)
    )
}

formatDataTypeLabel <- function(data_type) {
  dplyr::case_when(
    data_type == "estimate" ~ "Observed",
    data_type == "projection" ~ "Projection",
    TRUE ~ data_type
  )
}

shareAxisDecimals <- function(values) {
  pct <- (values * 100)[is.finite(values)]
  if (length(pct) == 0) {
    return(1L)
  }
  span <- diff(range(pct))
  if (!is.finite(span) || span <= 0) {
    return(1L)
  }
  if (span >= 5) {
    return(0L)
  }
  if (span >= 0.5) {
    return(1L)
  }
  if (span >= 0.05) {
    return(2L)
  }
  3L
}

countAxisDecimals <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(0L)
  }
  if (all(abs(values - round(values)) < 1e-9)) {
    return(0L)
  }
  span <- diff(range(values))
  if (!is.finite(span) || span <= 0) {
    return(0L)
  }
  if (span >= 10) {
    return(0L)
  }
  if (span >= 1) {
    return(1L)
  }
  2L
}

shareAxisLabelFormatter <- function(values) {
  decimals <- shareAxisDecimals(values)
  function(x) {
    paste0(
      format(round(x * 100, decimals), nsmall = decimals, trim = TRUE),
      "%"
    )
  }
}

countAxisLabelFormatter <- function(values) {
  decimals <- countAxisDecimals(values)
  function(x) {
    format(
      round(x, decimals),
      nsmall = decimals,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
}

buildPlotEventMarkers <- function(plot_data, events) {
  empty <- tibble::tibble(
    query_id = character(),
    event_id = character(),
    event_name = character(),
    start_year = integer(),
    end_year = integer(),
    is_single_year = logical(),
    line_color = character(),
    label_x = numeric(),
    label_vjust = numeric()
  )
  if (nrow(plot_data) == 0 || is.null(events) || nrow(events) == 0) {
    return(empty)
  }

  query_ids <- unique(plot_data$query_id)
  palette <- trackerPlotPalette(length(query_ids))

  markers <- plot_data |>
    dplyr::distinct(.data$query_id, .data$event_id, .data$event_name) |>
    dplyr::left_join(
      events |> dplyr::select("event_id", "start_year", "end_year"),
      by = "event_id"
    ) |>
    dplyr::filter(!is.na(.data$start_year)) |>
    dplyr::mutate(
      end_year = dplyr::coalesce(.data$end_year, .data$start_year),
      is_single_year = .data$start_year == .data$end_year,
      line_color = palette[match(.data$query_id, query_ids)],
      label_x = dplyr::if_else(
        .data$is_single_year,
        as.numeric(.data$start_year),
        (as.numeric(.data$start_year) + as.numeric(.data$end_year)) / 2
      ),
      label_vjust = 1.15 + (match(.data$query_id, query_ids) - 1L) * 0.55
    )

  markers |>
    dplyr::select(
      "query_id", "event_id", "event_name", "start_year", "end_year",
      "is_single_year", "line_color", "label_x", "label_vjust"
    )
}

addPlotEventMarkerLayers <- function(p, markers) {
  if (nrow(markers) == 0) {
    return(p)
  }

  range_markers <- markers |> dplyr::filter(!.data$is_single_year)
  single_markers <- markers |> dplyr::filter(.data$is_single_year)

  if (nrow(range_markers) > 0) {
    p <- p + ggplot2::geom_rect(
      data = range_markers,
      mapping = ggplot2::aes(
        xmin = .data$start_year,
        xmax = .data$end_year + 1,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = range_markers$line_color,
      alpha = 0.14,
      inherit.aes = FALSE,
      color = NA
    )
  }

  if (nrow(single_markers) > 0) {
    p <- p + ggplot2::geom_vline(
      data = single_markers,
      ggplot2::aes(xintercept = .data$start_year),
      color = single_markers$line_color,
      linewidth = 0.75,
      inherit.aes = FALSE
    )
  }

  p + ggplot2::geom_text(
    data = markers,
    ggplot2::aes(
      x = .data$label_x,
      y = Inf,
      label = .data$event_name
    ),
    color = markers$line_color,
    inherit.aes = FALSE,
    vjust = markers$label_vjust,
    hjust = 0.5,
    size = 3.1,
    lineheight = 0.95,
    show.legend = FALSE
  )
}

buildTrackerPlot <- function(plot_data, metric, events = NULL, title = NULL, subtitle = NULL) {
  if (nrow(plot_data) == 0) {
    stop("Cannot build plot from empty data.")
  }

  query_ids <- unique(plot_data$query_id)
  palette <- trackerPlotPalette(length(query_ids))
  markers <- buildPlotEventMarkers(plot_data, events)
  has_markers <- nrow(markers) > 0

  legend_labels <- plot_data |>
    dplyr::distinct(.data$query_id, .data$legend_label) |>
    dplyr::arrange(.data$query_id)
  color_breaks <- legend_labels$query_id
  color_labels <- legend_labels$legend_label
  n_queries <- length(color_breaks)

  metric_label <- formatMetricLabel(metric)
  plot_title <- if (!is.null(title) && length(title) == 1L && nzchar(title)) {
    title
  } else {
    NULL
  }
  plot_subtitle <- if (!is.null(subtitle) && length(subtitle) == 1L && nzchar(subtitle)) {
    subtitle
  } else {
    NULL
  }

  y_expand <- if (has_markers) {
    ggplot2::expansion(mult = c(0.02, 0.2))
  } else {
    ggplot2::expansion(mult = c(0.02, 0.06))
  }

  y_scale <- if (metric == "share_total_population") {
    ggplot2::scale_y_continuous(
      labels = shareAxisLabelFormatter(plot_data$value),
      expand = y_expand
    )
  } else {
    ggplot2::scale_y_continuous(
      labels = countAxisLabelFormatter(plot_data$value),
      expand = y_expand
    )
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$year,
    y = .data$value,
    color = .data$query_id,
    linetype = .data$data_type
  ))

  p <- addPlotEventMarkerLayers(p, markers)

  p +
    ggplot2::geom_line(linewidth = 1.05, lineend = "round") +
    ggplot2::scale_color_manual(
      values = stats::setNames(palette, color_breaks),
      breaks = color_breaks,
      labels = color_labels,
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("estimate" = "solid", "projection" = "22"),
      labels = c("estimate" = "Observed", "projection" = "Projected"),
      name = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = if (n_queries <= 2) 1 else 2,
        byrow = TRUE,
        override.aes = list(linewidth = 1.1, linetype = "solid")
      ),
      linetype = "none"
    ) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = metric_label,
      caption = NULL
    ) +
    trackerPlotTheme()
}

buildQueryDetailsRows <- function(plot_data) {
  plot_data |>
    dplyr::distinct(
      .data$query_id,
      .data$line_label,
      .data$query_description,
      .data$recipe_code
    ) |>
    dplyr::arrange(.data$query_id)
}

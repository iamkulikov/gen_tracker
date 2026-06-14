trackerPlotPalette <- function(n = 4) {
  # Blue, vermillion, green, gold — distinguishable for common color-vision deficiencies.
  colors <- c("#1f4e79", "#c55a11", "#2e7d32", "#9a6700")
  if (n <= length(colors)) {
    return(colors[seq_len(n)])
  }
  grDevices::colorRampPalette(colors)(n)
}

querySlotIndex <- function(query_id) {
  slot <- suppressWarnings(as.integer(sub("^q", "", query_id)))
  slot[!is.finite(slot) | slot < 1L] <- 1L
  slot
}

queryLineColors <- function(query_ids, max_slots = 4L) {
  palette <- trackerPlotPalette(max_slots)
  slots <- vapply(query_ids, querySlotIndex, integer(1))
  slots <- pmin(slots, max_slots)
  palette[slots]
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

shareScalePinsToUnity <- function(values) {
  vals <- values[is.finite(values)]
  if (length(vals) == 0L) {
    return(FALSE)
  }
  max(vals, na.rm = TRUE) >= 0.98
}

shareYScaleExpansion <- function(has_markers, pin_top) {
  if (pin_top) {
    return(ggplot2::expansion(mult = c(0.02, 0)))
  }
  if (has_markers) {
    return(ggplot2::expansion(mult = c(0.02, 0.2)))
  }
  ggplot2::expansion(mult = c(0.02, 0.06))
}

buildShareYScale <- function(values, has_markers = FALSE) {
  pin_top <- shareScalePinsToUnity(values)
  expand <- shareYScaleExpansion(has_markers, pin_top)
  if (pin_top) {
    return(ggplot2::scale_y_continuous(
      labels = shareAxisLabelFormatter(values),
      limits = c(0, 1),
      breaks = scales::breaks_extended(5),
      expand = expand
    ))
  }
  ggplot2::scale_y_continuous(
    labels = shareAxisLabelFormatter(values),
    expand = expand
  )
}

buildCountYScale <- function(values, has_markers = FALSE) {
  expand <- shareYScaleExpansion(has_markers, pin_top = FALSE)
  ggplot2::scale_y_continuous(
    labels = countAxisLabelFormatter(values),
    expand = expand
  )
}

markerZoneBounds <- function(start_year, end_year, is_single_year) {
  start_val <- as.numeric(start_year)
  if (isTRUE(is_single_year)) {
    return(c(start = start_val, end = start_val + 1))
  }
  c(start = start_val, end = as.numeric(end_year) + 1)
}

markerZoneCenter <- function(start_year, end_year, is_single_year) {
  bounds <- markerZoneBounds(start_year, end_year, is_single_year)
  (bounds[["start"]] + bounds[["end"]]) / 2
}

markerZonesOverlap <- function(start_a, end_a, start_b, end_b) {
  start_a < end_b && start_b < end_a
}

markerLabelHalfWidthYears <- function(
  label,
  year_span,
  zone_start,
  zone_end,
  chars_across_plot = 52
) {
  span <- max(as.numeric(year_span), 1)
  nchar_label <- max(nchar(as.character(label)), 1L)
  text_half <- (nchar_label / 2) * (span / chars_across_plot)
  zone_half <- max(zone_end - zone_start, 1) / 2
  pmax(text_half, zone_half)
}

assignMarkerLabelLanes <- function(
  markers,
  year_span = NULL,
  base_vjust = 1.15,
  lane_step = 1.35
) {
  if (nrow(markers) == 0L) {
    return(markers)
  }
  if (nrow(markers) == 1L) {
    markers$label_vjust <- base_vjust
    return(markers)
  }

  n <- nrow(markers)
  zone_start <- numeric(n)
  zone_end <- numeric(n)
  label_half_width <- numeric(n)
  for (i in seq_len(n)) {
    bounds <- markerZoneBounds(
      markers$start_year[[i]],
      markers$end_year[[i]],
      markers$is_single_year[[i]]
    )
    zone_start[[i]] <- bounds[["start"]]
    zone_end[[i]] <- bounds[["end"]]
    span <- if (!is.null(year_span) && is.finite(year_span) && year_span > 0) {
      year_span
    } else {
      max(zone_end[[i]] - zone_start[[i]], 1)
    }
    label_text <- if ("event_name" %in% names(markers)) {
      markers$event_name[[i]]
    } else {
      ""
    }
    label_half_width[[i]] <- markerLabelHalfWidthYears(
      label = label_text,
      year_span = span,
      zone_start = zone_start[[i]],
      zone_end = zone_end[[i]]
    )
  }

  label_start <- markers$label_x - label_half_width
  label_end <- markers$label_x + label_half_width

  order <- order(markers$label_x, label_start, label_end)
  lanes <- rep(NA_integer_, n)

  for (idx in order) {
    used <- integer(0)
    for (j in order) {
      if (j == idx || is.na(lanes[[j]])) {
        next
      }
      if (markerZonesOverlap(
        label_start[[idx]],
        label_end[[idx]],
        label_start[[j]],
        label_end[[j]]
      )) {
        used <- c(used, lanes[[j]])
      }
    }
    lane <- 0L
    while (lane %in% used) {
      lane <- lane + 1L
    }
    lanes[[idx]] <- lane
  }

  markers$label_vjust <- base_vjust + lanes * lane_step
  markers
}

buildPlotEventMarkers <- function(plot_data, events, composite_members = NULL) {
  empty <- tibble::tibble(
    query_id = character(),
    event_id = character(),
    event_name = character(),
    episode_index = integer(),
    start_year = integer(),
    end_year = integer(),
    is_single_year = logical(),
    line_color = character(),
    label_x = numeric(),
    label_vjust = numeric(),
    show_label = logical()
  )
  if (nrow(plot_data) == 0 || is.null(events) || nrow(events) == 0) {
    return(empty)
  }

  query_ids <- unique(plot_data$query_id)
  line_colors <- queryLineColors(query_ids)

  query_meta <- plot_data |>
    dplyr::distinct(.data$query_id, .data$event_id, .data$event_name, .data$event_mode)

  marker_rows <- list()
  for (i in seq_len(nrow(query_meta))) {
    row <- query_meta[i, ]
    event <- events |>
      dplyr::filter(.data$event_id == row$event_id) |>
      dplyr::slice(1)
    if (nrow(event) == 0) {
      next
    }
    event_mode <- row$event_mode
    if (is.na(event_mode) || !nzchar(event_mode)) {
      event_mode <- "start"
    }
    episodes <- resolveEventEpisodes(
      event,
      event_mode,
      composite_members = composite_members,
      events = events
    )
    if (nrow(episodes) == 0) {
      next
    }
    line_color <- line_colors[match(row$query_id, query_ids)]
    for (j in seq_len(nrow(episodes))) {
      ep <- episodes[j, ]
      start_year <- as.integer(ep$start_year)
      end_year <- as.integer(ep$end_year)
      is_single_year <- start_year == end_year
      marker_rows[[length(marker_rows) + 1L]] <- tibble::tibble(
        query_id = row$query_id,
        event_id = row$event_id,
        event_name = row$event_name,
        episode_index = as.integer(ep$episode_index),
        start_year = start_year,
        end_year = end_year,
        is_single_year = is_single_year,
        line_color = line_color,
        label_x = markerZoneCenter(start_year, end_year, is_single_year),
        label_vjust = 1.15,
        show_label = j == 1L
      )
    }
  }

  if (length(marker_rows) == 0) {
    return(empty)
  }

  result <- dplyr::bind_rows(marker_rows)
  label_idx <- which(result$show_label)
  if (length(label_idx) > 0L) {
    year_span <- if ("year" %in% names(plot_data)) {
      diff(range(plot_data$year, na.rm = TRUE))
    } else {
      NA_real_
    }
    if (!is.finite(year_span) || year_span <= 0) {
      year_span <- NULL
    }
    positioned <- assignMarkerLabelLanes(
      result[label_idx, , drop = FALSE],
      year_span = year_span
    )
    result$label_vjust[label_idx] <- positioned$label_vjust
  }
  result
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
    p <- p + ggplot2::geom_rect(
      data = single_markers,
      mapping = ggplot2::aes(
        xmin = .data$start_year,
        xmax = .data$start_year + 1,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = single_markers$line_color,
      alpha = 0.14,
      inherit.aes = FALSE,
      color = NA
    )
  }

  label_markers <- markers |> dplyr::filter(.data$show_label)
  if (nrow(label_markers) > 0) {
    p <- p + ggplot2::geom_text(
      data = label_markers,
      ggplot2::aes(
        x = .data$label_x,
        y = Inf,
        label = .data$event_name
      ),
      color = label_markers$line_color,
      inherit.aes = FALSE,
      vjust = label_markers$label_vjust,
      hjust = 0.5,
      size = 3.1,
      lineheight = 0.95,
      show.legend = FALSE
    )
  }
  p
}

buildTrackerPlot <- function(
  plot_data,
  metric,
  events = NULL,
  composite_members = NULL,
  title = NULL,
  subtitle = NULL
) {
  if (nrow(plot_data) == 0) {
    stop("Cannot build plot from empty data.")
  }

  markers <- buildPlotEventMarkers(plot_data, events, composite_members = composite_members)
  has_markers <- nrow(markers) > 0

  legend_labels <- plot_data |>
    dplyr::distinct(.data$query_id, .data$legend_label) |>
    dplyr::arrange(.data$query_id)
  color_breaks <- legend_labels$query_id
  color_labels <- legend_labels$legend_label
  color_values <- queryLineColors(color_breaks)
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

  y_scale <- if (isShareMetric(metric)) {
    buildShareYScale(plot_data$value, has_markers = has_markers)
  } else {
    buildCountYScale(plot_data$value, has_markers = has_markers)
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
      values = stats::setNames(color_values, color_breaks),
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
  detail_cols <- c(
    "query_id",
    "line_label",
    "chart_narrative",
    "query_description",
    "recipe_code",
    "event_short_description",
    "event_source_url",
    "event_family",
    "reliability_score",
    "migration_exposure",
    "reliability_warning"
  )
  available_cols <- intersect(detail_cols, names(plot_data))

  plot_data |>
    dplyr::group_by(.data$query_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(available_cols)) |>
    dplyr::arrange(.data$query_id)
}

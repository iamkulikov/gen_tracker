buildPlotViewSubtitle <- function(metric, year_range, show_projection = TRUE) {
  NULL
}

applyPlotViewFilters <- function(plot_data, year_range, show_projection = TRUE) {
  if (nrow(plot_data) == 0) {
    return(plot_data)
  }
  out <- plot_data |>
    dplyr::filter(
      .data$year >= year_range[[1]],
      .data$year <= year_range[[2]]
    )
  if (!isTRUE(show_projection)) {
    out <- out |> dplyr::filter(!.data$is_projection)
  }
  out
}

buildExportQueryDescriptions <- function(
  plot_data,
  recipes,
  events,
  countries,
  age_groups,
  population = NULL,
  event_countries = NULL,
  composite_members = NULL,
  migration = NULL
) {
  recipes_tbl <- tibble::as_tibble(recipes)
  if (nrow(recipes_tbl) == 0) {
    return(tibble::tibble())
  }

  detail_rows <- buildQueryDetailsRows(plot_data)
  if (nrow(detail_rows) == 0) {
    detail_rows <- tibble::tibble(
      query_id = recipes_tbl$query_id,
      line_label = NA_character_,
      chart_narrative = NA_character_,
      query_description = NA_character_,
      recipe_code = NA_character_
    )
  }

  purrr::map_dfr(seq_len(nrow(recipes_tbl)), function(i) {
    recipe <- as.list(recipes_tbl[i, ])
    event <- events |>
      dplyr::filter(.data$event_id == recipe$event_id) |>
      dplyr::slice(1)
    country_row <- countries |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::slice(1)

    detail <- detail_rows |>
      dplyr::filter(.data$query_id == recipe$query_id) |>
      dplyr::slice(1)

    series_rows <- plot_data |>
      dplyr::filter(.data$query_id == recipe$query_id)

    warnings <- collectQueryWarnings(
      recipe = recipe,
      event = event,
      country_row = country_row,
      event_countries = event_countries,
      stratum_series = series_rows,
      composite_members = composite_members,
      events = events,
      migration = migration
    )

    reliability_score <- if ("reliability_score" %in% names(series_rows)) {
      series_rows$reliability_score[[1]]
    } else {
      NA_real_
    }
    migration_exposure <- if ("migration_exposure" %in% names(series_rows)) {
      series_rows$migration_exposure[[1]]
    } else {
      NA_real_
    }
    reliability_summary <- formatReliabilitySummary(list(
      reliability_score = reliability_score,
      migration_exposure = migration_exposure,
      reliability_warning = NA_character_
    ))

    tibble::tibble(
      query_id = recipe$query_id,
      line_label = detail$line_label %||% NA_character_,
      query_description = detail$query_description %||% NA_character_,
      recipe_code = detail$recipe_code %||% encodeRecipe(recipe),
      metric = recipe$metric,
      reliability_score = reliability_score,
      migration_exposure = migration_exposure,
      reliability_summary = reliability_summary,
      warnings = paste(warnings, collapse = " | ")
    )
  })
}

buildExportMetadataTables <- function(
  plot_data,
  recipes,
  events,
  countries,
  age_groups,
  view_state,
  population = NULL,
  population_paths = NULL,
  events_path = NULL,
  countries_path = NULL,
  event_countries_path = NULL,
  event_countries = NULL,
  event_countries_loaded = FALSE,
  event_countries_rows = 0L,
  composite_members = NULL,
  migration = NULL
) {
  pop_source <- summarizePopulationSource(
    population = population,
    population_paths = population_paths
  )

  used_event_ids <- unique(tibble::as_tibble(recipes)$event_id)
  used_events <- events |>
    dplyr::filter(.data$event_id %in% used_event_ids) |>
    dplyr::mutate(
      event_years = purrr::map2_chr(.data$start_year, .data$end_year, formatEventYearRange)
    )

  recipe_pack_code <- tryCatch(
    encodeRecipePack(recipes, view_state),
    error = function(e) ""
  )

  query_descriptions <- buildExportQueryDescriptions(
    plot_data = plot_data,
    recipes = recipes,
    events = events,
    countries = countries,
    age_groups = age_groups,
    population = population,
    event_countries = event_countries,
    composite_members = composite_members,
    migration = migration
  )

  metadata <- tibble::tribble(
    ~field, ~value,
    "app_version", genTrackerAppVersion(),
    "exported_at", as.character(Sys.time()),
    "metric", formatMetricLabel(view_state$metric),
    "metric_code", view_state$metric,
    "year_min", as.character(view_state$year_range[[1]]),
    "year_max", as.character(view_state$year_range[[2]]),
    "show_projection", as.character(isTRUE(view_state$show_projection)),
    "recipe_pack", recipe_pack_code,
    "query_count", as.character(nrow(tibble::as_tibble(recipes))),
    "data_rows_exported", as.character(nrow(plot_data)),
    "methodology_notes", buildExportMethodologyNotes(pop_source)
  )

  queries <- buildExportQueriesSheet(
    recipes = recipes,
    query_descriptions = query_descriptions,
    events = used_events
  )

  list(
    metadata = metadata,
    query_descriptions = query_descriptions,
    queries = queries,
    used_events = used_events
  )
}

buildExportMethodologyNotes <- function(pop_source) {
  paste(
    c(
      "Demographic calculations use current-country population tables by birth year and age at event reference.",
      "Migration and historical border changes are not formally corrected in this MVP.",
      "Projection segments (dashed lines) follow UN WPP medium scenario where available.",
      "Reliability scoring is not yet implemented.",
      sprintf(
        "Demographic source: %s (version %s).",
        pop_source$demographic_source,
        pop_source$source_version
      )
    ),
    collapse = "\n"
  )
}

buildExportQueriesSheet <- function(
  recipes,
  query_descriptions,
  events,
  include_reliability = TRUE
) {
  recipes_tbl <- tibble::as_tibble(recipes)
  if (nrow(recipes_tbl) == 0L) {
    return(tibble::tibble())
  }

  qd_cols <- c("query_id", "line_label", "recipe_code", "metric")
  if (isTRUE(include_reliability)) {
    qd_cols <- c(qd_cols, "reliability_summary")
  }
  qd <- tibble::as_tibble(query_descriptions) |>
    dplyr::select(dplyr::any_of(qd_cols))

  event_cols <- normalizeEventCurationFields(events) |>
    dplyr::filter(.data$event_id %in% recipes_tbl$event_id) |>
    dplyr::mutate(
      event_years = purrr::map2_chr(.data$start_year, .data$end_year, formatEventYearRange),
      curation_scores = formatCurationScoreSummary(pick(dplyr::all_of(eventCurationScoreFields())))
    ) |>
    dplyr::select(
      "event_id",
      "event_name",
      "event_type",
      "event_scope",
      "event_years",
      "peak_year",
      "event_family",
      short_description = "short_description",
      "source_url",
      "selection_channel",
      "include_in_core_catalogue",
      "curation_scores"
    )

  out_cols <- c(
    "query_id",
    "line_label",
    "recipe_code",
    "metric",
    "event_name",
    "event_type",
    "event_scope",
    "event_years",
    "peak_year",
    "event_family",
    "short_description",
    "source_url",
    "selection_channel",
    "include_in_core_catalogue",
    "curation_scores"
  )
  if (isTRUE(include_reliability)) {
    out_cols <- c(out_cols, "reliability_summary")
  }

  recipes_tbl |>
    dplyr::select("query_id", "event_id") |>
    dplyr::left_join(qd, by = "query_id") |>
    dplyr::left_join(event_cols, by = "event_id") |>
    dplyr::select(dplyr::any_of(out_cols)) |>
    dplyr::arrange("query_id")
}

exportDataSeriesColumn <- function(query_id, line_label) {
  dplyr::if_else(
    is.na(line_label) | !nzchar(line_label),
    query_id,
    paste0(query_id, " - ", line_label)
  )
}

buildExportDataWide <- function(plot_data) {
  if (nrow(plot_data) == 0L) {
    return(tibble::tibble(year = integer()))
  }

  plot_data |>
    dplyr::distinct(.data$year, .data$query_id, .keep_all = TRUE) |>
    dplyr::mutate(
      series_column = exportDataSeriesColumn(.data$query_id, .data$line_label)
    ) |>
    dplyr::select("year", "series_column", "value") |>
    tidyr::pivot_wider(
      id_cols = "year",
      names_from = "series_column",
      values_from = "value"
    ) |>
    dplyr::arrange(.data$year)
}

buildMacroExportDataProjectionCells <- function(plot_data, sheet_df) {
  if (nrow(plot_data) == 0L || nrow(sheet_df) == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  proj_lookup <- plot_data |>
    dplyr::filter(.data$is_projection) |>
    dplyr::mutate(
      series_column = exportDataSeriesColumn(.data$query_id, .data$line_label)
    ) |>
    dplyr::distinct(.data$country_id, .data$year, .data$series_column, .keep_all = TRUE)

  if (nrow(proj_lookup) == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  col_names <- names(sheet_df)
  cells <- vector("list", nrow(proj_lookup))
  n_cells <- 0L
  for (i in seq_len(nrow(proj_lookup))) {
    excel_row <- which(
      sheet_df$country_id == proj_lookup$country_id[[i]] &
        sheet_df$year == proj_lookup$year[[i]]
    )
    excel_col <- match(proj_lookup$series_column[[i]], col_names)
    if (length(excel_row) != 1L || is.na(excel_col)) {
      next
    }
    n_cells <- n_cells + 1L
    cells[[n_cells]] <- c(excel_row + 1L, excel_col)
  }

  if (n_cells == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  mat <- do.call(rbind, cells[seq_len(n_cells)])
  data.frame(row = mat[, 1], col = mat[, 2])
}

buildExportDataProjectionCells <- function(plot_data, wide_df) {
  if (nrow(plot_data) == 0L || nrow(wide_df) == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  proj_lookup <- plot_data |>
    dplyr::distinct(.data$year, .data$query_id, .data$line_label, .data$is_projection) |>
    dplyr::filter(.data$is_projection) |>
    dplyr::mutate(
      series_column = exportDataSeriesColumn(.data$query_id, .data$line_label)
    )

  if (nrow(proj_lookup) == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  col_names <- names(wide_df)
  cells <- vector("list", nrow(proj_lookup))
  n_cells <- 0L
  for (i in seq_len(nrow(proj_lookup))) {
    excel_row <- match(proj_lookup$year[i], wide_df$year)
    excel_col <- match(proj_lookup$series_column[i], col_names)
    if (is.na(excel_row) || is.na(excel_col)) {
      next
    }
    n_cells <- n_cells + 1L
    cells[[n_cells]] <- c(excel_row + 1L, excel_col)
  }

  if (n_cells == 0L) {
    return(data.frame(row = integer(), col = integer()))
  }

  mat <- do.call(rbind, cells[seq_len(n_cells)])
  data.frame(row = mat[, 1], col = mat[, 2])
}

transposeExportQueriesSheet <- function(queries) {
  queries_tbl <- tibble::as_tibble(queries)
  if (nrow(queries_tbl) == 0L) {
    return(tibble::tibble(field = character()))
  }

  field_order <- setdiff(names(queries_tbl), "query_id")

  queries_tbl |>
    dplyr::mutate(dplyr::across(dplyr::all_of(field_order), as.character)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(field_order),
      names_to = "field",
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      id_cols = "field",
      names_from = "query_id",
      values_from = "value"
    ) |>
    dplyr::mutate(
      field = factor(.data$field, levels = field_order)
    ) |>
    dplyr::arrange(.data$field) |>
    dplyr::mutate(field = as.character(.data$field))
}

buildPlotViewSubtitle <- function(metric, year_range, show_projection = TRUE) {
  projection_note <- if (isTRUE(show_projection)) {
    "includes projection"
  } else {
    "estimates only"
  }
  sprintf("%s · solid = observed · dashed = projected", projection_note)
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
  event_countries = NULL
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

    warnings <- collectQueryWarnings(
      recipe = recipe,
      event = event,
      country_row = country_row,
      event_countries = event_countries
    )

    tibble::tibble(
      query_id = recipe$query_id,
      line_label = detail$line_label %||% NA_character_,
      query_description = detail$query_description %||% NA_character_,
      recipe_code = detail$recipe_code %||% encodeRecipe(recipe),
      metric = recipe$metric,
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
  event_countries_rows = 0L
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

  metadata <- tibble::tribble(
    ~field, ~value,
    "app_version", genTrackerAppVersion(),
    "exported_at", as.character(Sys.time()),
    "metric", formatMetricLabel(view_state$metric),
    "metric_code", view_state$metric,
    "year_min", as.character(view_state$year_range[[1]]),
    "year_max", as.character(view_state$year_range[[2]]),
    "show_projection", as.character(isTRUE(view_state$show_projection)),
    "query_count", as.character(nrow(tibble::as_tibble(recipes))),
    "data_rows_exported", as.character(nrow(plot_data)),
    "population_file", pop_source$population_file %||% "",
    "demographic_source", pop_source$demographic_source,
    "demographic_source_version", pop_source$source_version,
    "population_built_at", pop_source$built_at %||% "",
    "events_file", if (!is.null(events_path)) basename(events_path) else "",
    "countries_file", if (!is.null(countries_path)) basename(countries_path) else "",
    "event_countries_file", if (!is.null(event_countries_path)) basename(event_countries_path) else "",
    "event_countries_loaded", as.character(isTRUE(event_countries_loaded)),
    "event_countries_rows", as.character(event_countries_rows)
  )

  notes <- tibble::tribble(
    ~note,
    "Demographic calculations use current-country population tables by birth year and age at event reference.",
    "Migration and historical border changes are not formally corrected in this MVP.",
    "Projection segments (dashed lines) follow UN WPP medium scenario where available.",
    "Reliability scoring is not yet implemented.",
    sprintf(
      "Export reflects chart view: years %s–%s, metric %s, projection %s.",
      view_state$year_range[[1]],
      view_state$year_range[[2]],
      formatMetricLabel(view_state$metric),
      if (isTRUE(view_state$show_projection)) "shown" else "hidden"
    )
  )

  event_disclaimers <- used_events |>
    dplyr::transmute(
      event_id = .data$event_id,
      event_name = .data$event_name,
      event_scope = .data$event_scope,
      event_years = .data$event_years,
      disclaimer = dplyr::case_when(
        .data$event_scope == "global" ~ "Global event: applicable to any country in the catalogue.",
        .data$event_scope == "national" ~ "National event: interpret only for linked countries unless cross-country comparison is explicitly allowed.",
        .data$event_scope == "multi_country" ~ "Multi-country event: see event–country links for affected territories.",
        TRUE ~ "See event metadata for scope and timing."
      )
    )

  query_descriptions <- buildExportQueryDescriptions(
    plot_data = plot_data,
    recipes = recipes,
    events = events,
    countries = countries,
    age_groups = age_groups,
    population = population,
    event_countries = event_countries
  )

  list(
    metadata = metadata,
    notes = notes,
    event_disclaimers = event_disclaimers,
    query_descriptions = query_descriptions,
    used_events = used_events
  )
}

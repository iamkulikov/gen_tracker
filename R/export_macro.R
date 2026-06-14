buildMacroQuerySentencePreview <- function(recipe, events, age_groups) {
  sex_label <- queryBuilderChoiceLabel(queryBuilderSexChoices(), recipe$sex)
  event_name <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$event_name)
  age_label <- formatAgeStatusLabel(recipe$age_status_id, age_groups)
  if (identical(recipe$age_status_id, "custom")) {
    age_label <- sprintf("ages %s-%s", recipe$custom_age_min, recipe$custom_age_max)
  }

  age_modifier <- recipeAgeModifier(recipe)
  modifier_label <- queryBuilderChoiceLabel(queryBuilderComplementChoices(), age_modifier)
  modifier_text <- if (identical(age_modifier, "none")) "" else paste0(" ", modifier_label)
  mode_label <- queryBuilderChoiceLabel(queryBuilderEventModeChoices(), recipe$event_mode)

  sprintf(
    "%s who were%s %s %s %s.",
    sex_label,
    modifier_text,
    age_label,
    mode_label,
    event_name
  )
}

buildMacroPlotData <- function(
  template_recipe,
  countries,
  population,
  events,
  age_groups,
  plot_context = NULL,
  composite_members = NULL,
  year_range = NULL,
  show_projection = TRUE
) {
  template_recipe <- as.list(template_recipe)
  if (is.null(plot_context)) {
    plot_context <- buildPlotCalculationContext(population)
  }

  line_label <- buildMacroQuerySentencePreview(template_recipe, events, age_groups)
  country_lookup <- countries |>
    dplyr::select("country_id", "iso2")

  chunks <- purrr::map(countries$country_id, function(country_id) {
    recipe <- template_recipe
    recipe$country_id <- country_id

    series <- calculateStratumSeries(
      recipe = recipe,
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      plot_context = plot_context,
      composite_members = composite_members,
      skip_event_country_check = TRUE
    )

    if (nrow(series) == 0) {
      return(NULL)
    }
    if (!isTRUE(show_projection)) {
      series <- series |> dplyr::filter(!.data$is_projection)
    }

    series |>
      dplyr::left_join(country_lookup, by = "country_id", relationship = "many-to-one") |>
      dplyr::transmute(
        country_id = .data$iso2,
        year = as.integer(.data$year),
        value = .data$value,
        is_projection = .data$is_projection,
        query_id = template_recipe$query_id,
        line_label = line_label
      )
  })

  out <- dplyr::bind_rows(chunks)
  if (nrow(out) == 0) {
    return(
      tibble::tibble(
        country_id = character(),
        year = integer(),
        value = numeric(),
        is_projection = logical(),
        query_id = character(),
        line_label = character()
      )
    )
  }

  if (!is.null(year_range) && length(year_range) == 2L) {
    out <- out |>
      dplyr::filter(
        .data$year >= as.integer(year_range[[1]]),
        .data$year <= as.integer(year_range[[2]])
      )
  }

  out |>
    dplyr::arrange(.data$country_id, .data$year)
}

buildMacroPlotDataFromRecipes <- function(
  template_recipes,
  countries,
  population,
  events,
  age_groups,
  plot_context = NULL,
  composite_members = NULL,
  year_range = NULL,
  show_projection = TRUE
) {
  recipes_tbl <- tibble::as_tibble(template_recipes)
  if (nrow(recipes_tbl) == 0L) {
    return(
      tibble::tibble(
        country_id = character(),
        year = integer(),
        value = numeric(),
        is_projection = logical(),
        query_id = character(),
        line_label = character()
      )
    )
  }

  if (is.null(plot_context)) {
    plot_context <- buildPlotCalculationContext(population)
  }

  purrr::map_dfr(seq_len(nrow(recipes_tbl)), function(i) {
    buildMacroPlotData(
      template_recipe = as.list(recipes_tbl[i, ]),
      countries = countries,
      population = population,
      events = events,
      age_groups = age_groups,
      plot_context = plot_context,
      composite_members = composite_members,
      year_range = year_range,
      show_projection = show_projection
    )
  })
}

buildMacroExportDataSheet <- function(macro_plot_data) {
  if (nrow(macro_plot_data) == 0L) {
    return(tibble::tibble(country_id = character(), year = integer()))
  }

  macro_plot_data |>
    dplyr::mutate(
      series_column = exportDataSeriesColumn(.data$query_id, .data$line_label)
    ) |>
    dplyr::distinct(.data$country_id, .data$year, .data$query_id, .keep_all = TRUE) |>
    dplyr::select("country_id", "year", "series_column", "value") |>
    tidyr::pivot_wider(
      id_cols = c("country_id", "year"),
      names_from = "series_column",
      values_from = "value"
    ) |>
    dplyr::arrange(.data$country_id, .data$year)
}

buildMacroExportMethodologyNotes <- function(pop_source, country_count) {
  paste(
    c(
      "All-countries export: one row per country and year for every country in the dictionary.",
      "Country-event catalogue links are not applied; the selected event's fixed year anchors are used for each country.",
      sprintf("Countries included: %d.", country_count),
      "Demographic calculations use current-country population tables by birth year and age at event reference.",
      "Migration and historical border changes are not formally corrected in this MVP.",
      "Projection segments (dashed lines) follow UN WPP medium scenario where available.",
      sprintf(
        "Demographic source: %s (version %s).",
        pop_source$demographic_source,
        pop_source$source_version
      )
    ),
    collapse = "\n"
  )
}

buildMacroExportQueryDescription <- function(
  template_recipe,
  events,
  countries,
  age_groups,
  population = NULL,
  event_countries = NULL,
  composite_members = NULL,
  migration = NULL
) {
  recipe <- as.list(template_recipe)
  event <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1)
  country_row <- countries |>
    dplyr::filter(.data$country_id == recipe$country_id) |>
    dplyr::slice(1)

  line_label <- buildMacroQuerySentencePreview(recipe, events, age_groups)
  age_label <- resolveAgeRange(recipe, age_groups)$age_label
  country_name <- country_row$country_name %||% recipe$country_id

  query_description <- buildRecipeDescriptionTechnical(
    recipe,
    event,
    country_name,
    age_label,
    countries = countries,
    events = events,
    age_groups = age_groups,
    population = population
  )

  warnings <- unique(c(
    collectQueryWarnings(
      recipe = recipe,
      event = event,
      country_row = country_row,
      event_countries = event_countries,
      stratum_series = NULL,
      composite_members = composite_members,
      events = events,
      migration = migration
    ),
    "Country-event catalogue links are not applied in the all-countries export."
  ))

  tibble::tibble(
    query_id = recipe$query_id,
    line_label = line_label,
    query_description = query_description,
    recipe_code = encodeRecipe(recipe),
    metric = recipe$metric,
    warnings = paste(warnings, collapse = " | ")
  )
}

buildMacroExportQueryDescriptions <- function(
  template_recipes,
  events,
  countries,
  age_groups,
  population = NULL,
  event_countries = NULL,
  composite_members = NULL,
  migration = NULL
) {
  recipes_tbl <- tibble::as_tibble(template_recipes)
  if (nrow(recipes_tbl) == 0L) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(recipes_tbl)), function(i) {
    buildMacroExportQueryDescription(
      template_recipe = as.list(recipes_tbl[i, ]),
      events = events,
      countries = countries,
      age_groups = age_groups,
      population = population,
      event_countries = event_countries,
      composite_members = composite_members,
      migration = migration
    )
  })
}

buildMacroExportMetadataTables <- function(
  macro_plot_data,
  template_recipes,
  countries,
  events,
  age_groups,
  view_state,
  population = NULL,
  population_paths = NULL,
  event_countries = NULL,
  composite_members = NULL,
  migration = NULL
) {
  recipes_tbl <- tibble::as_tibble(template_recipes)
  pop_source <- summarizePopulationSource(
    population = population,
    population_paths = population_paths
  )

  used_event_ids <- unique(recipes_tbl$event_id)
  used_events <- events |>
    dplyr::filter(.data$event_id %in% used_event_ids) |>
    dplyr::mutate(
      event_years = purrr::map2_chr(.data$start_year, .data$end_year, formatEventYearRange)
    )

  recipe_pack_code <- tryCatch(
    encodeRecipePack(recipes_tbl, view_state),
    error = function(e) ""
  )

  query_descriptions <- buildMacroExportQueryDescriptions(
    template_recipes = recipes_tbl,
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
    "export_scope", "all_countries",
    "metric", formatMetricLabel(view_state$metric),
    "metric_code", view_state$metric,
    "year_min", as.character(view_state$year_range[[1]]),
    "year_max", as.character(view_state$year_range[[2]]),
    "show_projection", as.character(isTRUE(view_state$show_projection)),
    "recipe_pack", recipe_pack_code,
    "query_count", as.character(nrow(recipes_tbl)),
    "country_count", as.character(nrow(countries)),
    "data_rows_exported", as.character(nrow(macro_plot_data)),
    "methodology_notes", buildMacroExportMethodologyNotes(pop_source, nrow(countries))
  )

  queries <- buildExportQueriesSheet(
    recipes = recipes_tbl,
    query_descriptions = query_descriptions,
    events = used_events,
    include_reliability = FALSE
  )

  list(
    metadata = metadata,
    query_descriptions = query_descriptions,
    queries = queries,
    used_events = used_events
  )
}

buildMacroLongTable <- buildMacroPlotDataFromRecipes

exportMacroLongTable <- function(
  file_path,
  template_recipes,
  countries,
  population,
  events,
  age_groups,
  plot_context = NULL,
  composite_members = NULL,
  event_countries = NULL,
  migration = NULL,
  year_range = NULL,
  show_projection = TRUE,
  population_paths = NULL,
  template_recipe = NULL
) {
  if (!is.null(template_recipe)) {
    template_recipes <- tibble::as_tibble(template_recipe)
  }
  recipes_tbl <- tibble::as_tibble(template_recipes)
  metric <- recipes_tbl$metric[[1]] %||% "count"

  macro_plot_data <- buildMacroPlotDataFromRecipes(
    template_recipes = recipes_tbl,
    countries = countries,
    population = population,
    events = events,
    age_groups = age_groups,
    plot_context = plot_context,
    composite_members = composite_members,
    year_range = year_range,
    show_projection = show_projection
  )

  view_state <- list(
    metric = metric,
    year_range = year_range %||% if (nrow(macro_plot_data) > 0) {
      range(macro_plot_data$year, na.rm = TRUE)
    } else {
      c(NA_integer_, NA_integer_)
    },
    show_projection = show_projection
  )

  export_tables <- buildMacroExportMetadataTables(
    macro_plot_data = macro_plot_data,
    template_recipes = recipes_tbl,
    countries = countries,
    events = events,
    age_groups = age_groups,
    view_state = view_state,
    population = population,
    population_paths = population_paths,
    event_countries = event_countries,
    composite_members = composite_members,
    migration = migration
  )

  data_sheet <- buildMacroExportDataSheet(macro_plot_data)

  writeExportXlsxWorkbook(
    sheets = list(
      data = data_sheet,
      metadata = export_tables$metadata,
      queries = transposeExportQueriesSheet(export_tables$queries)
    ),
    file_path = file_path,
    metric = metric,
    plot_data = macro_plot_data,
    data_wide = data_sheet
  )

  invisible(file_path)
}

buildPlotData <- function(
  recipes,
  population,
  events,
  age_groups,
  countries,
  event_countries = NULL,
  plot_context = NULL,
  composite_members = NULL,
  migration = NULL
) {
  recipes_tbl <- tibble::as_tibble(recipes)
  if (nrow(recipes_tbl) == 0) {
    return(tibble::tibble())
  }

  if (nrow(recipes_tbl) > 4) {
    stop("Maximum 4 queries supported.")
  }

  if (is.null(plot_context)) {
    plot_context <- buildPlotCalculationContext(population)
  }

  chunks <- purrr::map(seq_len(nrow(recipes_tbl)), function(i) {
    recipe <- as.list(recipes_tbl[i, ])
    recipe$query_count <- nrow(recipes_tbl)
    series <- calculateStratumSeries(
      recipe,
      population,
      events,
      age_groups,
      countries,
      event_countries = event_countries,
      plot_context = plot_context,
      composite_members = composite_members
    )

    event <- events |> dplyr::filter(.data$event_id == recipe$event_id) |> dplyr::slice(1)
    country_row <- countries |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::slice(1)
    country_name <- country_row |> dplyr::pull(.data$country_name)
    age_label <- resolveAgeRange(recipe, age_groups)$age_label

    narrative <- buildChartNarrative(
      recipe = recipe,
      event = event,
      countries = countries,
      events = events,
      age_groups = age_groups,
      population = population
    )
    description <- buildRecipeDescriptionTechnical(
      recipe,
      event,
      country_name,
      age_label,
      countries = countries,
      events = events,
      age_groups = age_groups,
      population = population
    )
    recipe_code <- encodeRecipe(recipe)

    query_num <- sub("^q", "", recipe$query_id)
    legend_text <- buildPlotLegendLabel(recipe, country_name, event$event_name)
    line_text <- buildQuerySentencePreview(
      recipe = recipe,
      countries = countries |>
        dplyr::filter(.data$country_id == recipe$country_id) |>
        dplyr::slice(1),
      events = event,
      age_groups = age_groups
    )

    growth_warning <- stratumGrowthWarning(
      series,
      recipe,
      event,
      composite_members = composite_members,
      events = events
    )
    line_warning <- if (length(growth_warning) == 1L && nzchar(growth_warning)) {
      growth_warning
    } else {
      NA_character_
    }

    reliability <- computeStratumReliability(
      series = series,
      recipe = recipe,
      country_row = country_row,
      event = event,
      event_countries = event_countries,
      countries = countries,
      migration = migration
    )

    series |>
      dplyr::mutate(
        legend_label = legend_text,
        line_label = line_text,
        country_name = country_name,
        sex = recipe$sex,
        event_name = event$event_name,
        event_mode = recipe$event_mode,
        recipe_code = recipe_code,
        chart_narrative = narrative,
        query_description = description,
        event_short_description = event[["short_description"]][1] %||% NA_character_,
        event_source_url = event[["source_url"]][1] %||% NA_character_,
        event_family = event[["event_family"]][1] %||% NA_character_,
        reliability_score = reliability$reliability_score,
        migration_exposure = reliability$migration_exposure,
        reliability_warning = reliability$reliability_warning,
        warning = line_warning
      )
  })

  dplyr::bind_rows(chunks)
}

build_plot_data <- buildPlotData

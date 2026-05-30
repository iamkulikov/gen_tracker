buildPlotData <- function(
  recipes,
  population,
  events,
  age_groups,
  countries,
  event_countries = NULL,
  plot_context = NULL
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
      plot_context = plot_context
    )

    event <- events |> dplyr::filter(.data$event_id == recipe$event_id) |> dplyr::slice(1)
    country_name <- countries |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$country_name)
    age_label <- resolveAgeRange(recipe, age_groups)$age_label

    description <- buildRecipeDescription(
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

    series |>
      dplyr::mutate(
        legend_label = legend_text,
        line_label = line_text,
        country_name = country_name,
        sex = recipe$sex,
        event_name = event$event_name,
        recipe_code = recipe_code,
        query_description = description,
        reliability_score = NA_real_,
        warning = NA_character_
      )
  })

  dplyr::bind_rows(chunks)
}

build_plot_data <- buildPlotData

buildPlotCalculationContext <- function(population) {
  empty_totals <- tibble::tibble(
    country_id = character(),
    year = integer(),
    total_country_population = numeric()
  )
  empty_working_age_totals <- tibble::tibble(
    country_id = character(),
    year = integer(),
    total_working_age_population = numeric()
  )
  if (nrow(population) == 0) {
    return(list(
      population_by_country = list(),
      country_totals = empty_totals,
      working_age_totals = empty_working_age_totals
    ))
  }

  country_totals <- population |>
    dplyr::summarise(
      total_country_population = sum(.data$population, na.rm = TRUE),
      .by = c("country_id", "year")
    )

  working_age_bounds <- workingAgeBounds()
  working_age_totals <- population |>
    dplyr::filter(
      .data$age >= working_age_bounds$age_min,
      .data$age <= working_age_bounds$age_max
    ) |>
    dplyr::summarise(
      total_working_age_population = sum(.data$population, na.rm = TRUE),
      .by = c("country_id", "year")
    )

  pop_with_birth <- population |>
    dplyr::mutate(birth_year = .data$year - .data$age)

  population_by_country <- split(
    pop_with_birth,
    pop_with_birth$country_id,
    drop = TRUE
  )

  list(
    population_by_country = population_by_country,
    country_totals = country_totals,
    working_age_totals = working_age_totals
  )
}

countryPopulationFromPlotContext <- function(plot_context, target_country_id, population) {
  country_population <- plot_context$population_by_country[[target_country_id]]
  if (is.null(country_population)) {
    country_population <- population |>
      dplyr::filter(.data$country_id == .env$target_country_id) |>
      dplyr::mutate(birth_year = .data$year - .data$age)
  }
  country_population
}

countryTotalsFromPlotContext <- function(plot_context, target_country_id, population) {
  totals <- plot_context$country_totals |>
    dplyr::filter(.data$country_id == .env$target_country_id) |>
    dplyr::select("year", "total_country_population")
  if (nrow(totals) == 0) {
    totals <- population |>
      dplyr::filter(.data$country_id == .env$target_country_id) |>
      dplyr::summarise(
        total_country_population = sum(.data$population, na.rm = TRUE),
        .by = "year"
      )
  }
  totals
}

countryWorkingAgeTotalsFromPlotContext <- function(
  plot_context,
  target_country_id,
  population,
  age_groups
) {
  totals <- plot_context$working_age_totals |>
    dplyr::filter(.data$country_id == .env$target_country_id) |>
    dplyr::select("year", "total_working_age_population")
  if (nrow(totals) == 0) {
    bounds <- workingAgeBounds(age_groups)
    totals <- population |>
      dplyr::filter(
        .data$country_id == .env$target_country_id,
        .data$age >= bounds$age_min,
        .data$age <= bounds$age_max
      ) |>
      dplyr::summarise(
        total_working_age_population = sum(.data$population, na.rm = TRUE),
        .by = "year"
      )
  }
  totals
}

metricDenominatorTotals <- function(metric, country_totals, working_age_totals) {
  if (identical(metric, "share_working_age_population")) {
    return(
      working_age_totals |>
        dplyr::rename(total_country_population = "total_working_age_population")
    )
  }
  country_totals
}

filterStratumRowsForMetric <- function(rows, metric, working_age_bounds) {
  if (!identical(metric, "share_working_age_population")) {
    return(rows)
  }
  rows |>
    dplyr::filter(
      .data$age >= working_age_bounds$age_min,
      .data$age <= working_age_bounds$age_max
    )
}

computeStratumMetricValue <- function(metric, stratum_population, denominator) {
  if (identical(metric, "count")) {
    return(stratum_population)
  }
  stratum_population / denominator
}

calculateStratumSeriesComposite <- function(
  recipe,
  country_population,
  totals,
  event,
  age_range,
  population_years,
  population_ages,
  composite_members,
  events,
  working_age_bounds
) {
  birth_years <- resolveBirthYears(
    recipe = recipe,
    event = event,
    age_range = age_range,
    population_years = population_years,
    population_ages = population_ages,
    composite_members = composite_members,
    events = events
  )
  thresholds <- resolveBirthYearThresholds(
    recipe = recipe,
    event = event,
    age_range = age_range,
    population_years = population_years,
    population_ages = population_ages,
    composite_members = composite_members,
    events = events
  )

  if (length(birth_years) == 0 || nrow(thresholds) == 0) {
    years <- sort(unique(country_population$year))
    return(tibble::tibble(
      query_id = recipe$query_id,
      country_id = recipe$country_id,
      event_id = recipe$event_id,
      year = years,
      value = 0,
      metric = recipe$metric,
      data_type = "estimate",
      is_projection = FALSE,
      stratum_population = 0,
      total_country_population = NA_real_
    ))
  }

  granular <- country_population |>
    dplyr::filter(.data$birth_year %in% birth_years) |>
    dplyr::left_join(thresholds, by = "birth_year", relationship = "many-to-one") |>
    dplyr::filter(.data$year >= .data$threshold) |>
    filterStratumRowsForMetric(recipe$metric, working_age_bounds) |>
    dplyr::summarise(
      stratum_population = sum(.data$population, na.rm = TRUE),
      data_type = dplyr::first(.data$data_type),
      .by = c("country_id", "year")
    )

  granular |>
    dplyr::left_join(totals, by = "year", relationship = "many-to-one") |>
    dplyr::mutate(
      metric = recipe$metric,
      value = computeStratumMetricValue(
        recipe$metric,
        .data$stratum_population,
        .data$total_country_population
      ),
      query_id = recipe$query_id,
      event_id = recipe$event_id,
      is_projection = .data$data_type == "projection"
    ) |>
    dplyr::select(
      "query_id", "country_id", "event_id", "year", "value",
      "metric", "data_type", "is_projection", "stratum_population",
      "total_country_population"
    ) |>
    dplyr::arrange(.data$year)
}

calculateStratumSeries <- function(
  recipe,
  population,
  events,
  age_groups,
  countries,
  event_countries = NULL,
  plot_context = NULL,
  composite_members = NULL,
  skip_event_country_check = FALSE
) {
  validateRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries,
    composite_members = composite_members,
    skip_event_country_check = skip_event_country_check
  )

  event <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1)

  age_range <- resolveAgeRange(recipe, age_groups)
  working_age_bounds <- workingAgeBounds(age_groups)
  event_threshold <- resolveEventThreshold(event, recipe$event_mode)
  birth_years <- resolveBirthYears(
    recipe = recipe,
    event = event,
    age_range = age_range,
    population_years = population$year,
    population_ages = population$age,
    composite_members = composite_members,
    events = events
  )

  if (is.null(plot_context)) {
    country_population <- population |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::mutate(birth_year = .data$year - .data$age)
    country_totals <- population |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::summarise(
        total_country_population = sum(.data$population, na.rm = TRUE),
        .by = "year"
      )
    working_age_totals <- population |>
      dplyr::filter(
        .data$country_id == recipe$country_id,
        .data$age >= working_age_bounds$age_min,
        .data$age <= working_age_bounds$age_max
      ) |>
      dplyr::summarise(
        total_working_age_population = sum(.data$population, na.rm = TRUE),
        .by = "year"
      )
  } else {
    country_population <- countryPopulationFromPlotContext(
      plot_context,
      recipe$country_id,
      population
    )
    country_totals <- countryTotalsFromPlotContext(
      plot_context,
      recipe$country_id,
      population
    )
    working_age_totals <- countryWorkingAgeTotalsFromPlotContext(
      plot_context,
      recipe$country_id,
      population,
      age_groups
    )
  }

  totals <- metricDenominatorTotals(
    recipe$metric,
    country_totals,
    working_age_totals
  )

  if (recipe$sex != "all") {
    country_population <- country_population |>
      dplyr::filter(.data$sex == recipe$sex)
  }

  if (usesGranularThresholdPath(recipe, event)) {
    out <- calculateStratumSeriesComposite(
      recipe = recipe,
      country_population = country_population,
      totals = totals,
      event = event,
      age_range = age_range,
      population_years = population$year,
      population_ages = population$age,
      composite_members = composite_members,
      events = events,
      working_age_bounds = working_age_bounds
    )
    return(out)
  }

  stratum <- country_population |>
    dplyr::filter(.data$birth_year %in% birth_years) |>
    filterStratumRowsForMetric(recipe$metric, working_age_bounds) |>
    dplyr::summarise(
      stratum_population = sum(.data$population, na.rm = TRUE),
      data_type = dplyr::first(.data$data_type),
      .by = c("country_id", "year")
    )

  out <- stratum |>
    dplyr::left_join(totals, by = "year", relationship = "many-to-one") |>
    dplyr::mutate(
      metric = recipe$metric,
      value = computeStratumMetricValue(
        recipe$metric,
        .data$stratum_population,
        .data$total_country_population
      ),
      query_id = recipe$query_id,
      event_id = recipe$event_id,
      is_projection = .data$data_type == "projection"
    ) |>
    dplyr::select(
      "query_id", "country_id", "event_id", "year", "value",
      "metric", "data_type", "is_projection", "stratum_population",
      "total_country_population"
    ) |>
    dplyr::arrange(.data$year)

  should_zero_before_threshold <- !identical(recipeAgeModifier(recipe), "not") &&
    !identical(recipe$age_status_id, "not_born_yet")
  if (should_zero_before_threshold) {
    out <- out |>
      dplyr::mutate(
        stratum_population = dplyr::if_else(
          .data$year < .env$event_threshold,
          0,
          .data$stratum_population
        ),
        value = dplyr::if_else(.data$year < .env$event_threshold, 0, .data$value)
      )
  }

  out
}

build_plot_calculation_context <- buildPlotCalculationContext
calculate_stratum_series <- calculateStratumSeries

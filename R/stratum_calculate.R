buildPlotCalculationContext <- function(population) {
  empty_totals <- tibble::tibble(
    country_id = character(),
    year = integer(),
    total_country_population = numeric()
  )
  if (nrow(population) == 0) {
    return(list(
      population_by_country = list(),
      country_totals = empty_totals
    ))
  }

  country_totals <- population |>
    dplyr::summarise(
      total_country_population = sum(.data$population, na.rm = TRUE),
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
    country_totals = country_totals
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

calculateStratumSeries <- function(
  recipe,
  population,
  events,
  age_groups,
  countries,
  event_countries = NULL,
  plot_context = NULL
) {
  validateRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )

  event <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1)

  age_range <- resolveAgeRange(recipe, age_groups)
  birth_years <- resolveBirthYears(
    recipe = recipe,
    event = event,
    age_range = age_range,
    population_years = population$year,
    population_ages = population$age
  )

  if (is.null(plot_context)) {
    country_population <- population |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::mutate(birth_year = .data$year - .data$age)
    totals <- population |>
      dplyr::filter(.data$country_id == recipe$country_id) |>
      dplyr::summarise(
        total_country_population = sum(.data$population, na.rm = TRUE),
        .by = "year"
      )
  } else {
    country_population <- countryPopulationFromPlotContext(
      plot_context,
      recipe$country_id,
      population
    )
    totals <- countryTotalsFromPlotContext(
      plot_context,
      recipe$country_id,
      population
    )
  }

  if (recipe$sex != "all") {
    country_population <- country_population |>
      dplyr::filter(.data$sex == recipe$sex)
  }

  stratum <- country_population |>
    dplyr::filter(.data$birth_year %in% birth_years) |>
    dplyr::summarise(
      stratum_population = sum(.data$population, na.rm = TRUE),
      data_type = dplyr::first(.data$data_type),
      .by = c("country_id", "year")
    )

  out <- stratum |>
    dplyr::left_join(totals, by = "year", relationship = "many-to-one") |>
    dplyr::mutate(
      metric = recipe$metric,
      value = dplyr::if_else(
        .data$metric == "count",
        .data$stratum_population,
        .data$stratum_population / .data$total_country_population
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

  out
}

build_plot_calculation_context <- buildPlotCalculationContext
calculate_stratum_series <- calculateStratumSeries

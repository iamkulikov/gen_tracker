test_that("share metric equals count divided by total", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- list(
    query_id = "q2",
    country_id = "RUS",
    sex = "all",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "share_total_population"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expected <- out$stratum_population / out$total_country_population
  expect_equal(out$value, expected)
  expect_true(any(out$is_projection))
})

test_that("workingAgeBounds matches working_age group in default catalog", {
  bounds <- workingAgeBounds(defaultAgeGroups())
  expect_equal(bounds$age_min, 15L)
  expect_equal(bounds$age_max, 64L)
})

test_that("share_working_age_population uses working-age denominator and current-age filter", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- list(
    query_id = "q_wa",
    country_id = "RUS",
    sex = "male",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "share_working_age_population"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_equal(out$value, out$stratum_population / out$total_country_population)
  expect_true(all(out$value >= 0))
  expect_true(all(out$value <= 1))

  plot_context <- buildPlotCalculationContext(population)
  expected_denominator <- plot_context$working_age_totals |>
    dplyr::filter(.data$country_id == "RUS", .data$year == 1980L) |>
    dplyr::pull("total_working_age_population")
  expect_equal(
    out$total_country_population[out$year == 1980L][[1]],
    expected_denominator[[1]]
  )
  expect_gt(expected_denominator[[1]], 0)

  recipe_all <- recipe
  recipe_all$sex <- "all"
  out_all <- calculateStratumSeries(
    recipe_all,
    population,
    events,
    age_groups,
    countries
  )
  expect_true(any(out$stratum_population < out_all$stratum_population))
})

test_that("share_working_age_population numerator is below total-population share", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe_base <- list(
    query_id = "q_wa",
    country_id = "RUS",
    sex = "all",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "share_total_population"
  )
  recipe_working <- recipe_base
  recipe_working$metric <- "share_working_age_population"

  out_total <- calculateStratumSeries(
    recipe_base,
    population,
    events,
    age_groups,
    countries
  )
  out_working <- calculateStratumSeries(
    recipe_working,
    population,
    events,
    age_groups,
    countries
  )

  expect_true(all(out_working$stratum_population <= out_total$stratum_population))
  expect_true(any(out_working$stratum_population < out_total$stratum_population))
})

test_that("plot_context working-age totals match uncached denominator", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- list(
    query_id = "q_wa",
    country_id = "RUS",
    sex = "all",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "share_working_age_population"
  )

  uncached <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    plot_context = NULL
  )
  plot_context <- buildPlotCalculationContext(population)
  cached <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    plot_context = plot_context
  )

  expect_equal(cached$value, uncached$value)
  expect_equal(cached$total_country_population, uncached$total_country_population)
})

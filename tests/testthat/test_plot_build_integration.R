test_that("buildPlotData returns enriched columns for a single query", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  out <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  expect_true(nrow(out) > 0)
  expect_true(all(c(
    "legend_label", "line_label", "chart_narrative", "query_description",
    "recipe_code", "is_projection"
  ) %in% names(out)))
  expect_match(out$legend_label[[1]], "Russia")
  expect_no_match(out$legend_label[[1]], "Q1")
  expect_match(out$line_label[[1]], "Men in Russia")
  expect_match(out$line_label[[1]], "War in Afghanistan")
  expect_match(out$chart_narrative[[1]], "Men in Russia")
  expect_match(out$chart_narrative[[1]], "War in Afghanistan")
  expect_match(out$chart_narrative[[1]], "roughly 1962")
  expect_false(grepl("age_status_id", out$chart_narrative[[1]], fixed = TRUE))
  expect_match(out$query_description[[1]], "Birth years included:")
  expect_match(out$query_description[[1]], "1962")
  expect_true(startsWith(out$recipe_code[[1]], "GEN2:"))
})

test_that("buildPlotData supports up to four queries with distinct query ids", {
  population <- buildTestPopulation()
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "adults", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q3", "RUS", "all", "alive", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "period", "share_total_population",
    "q4", "RUS", "all", "youth", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "share_total_population"
  )

  out <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries
  )

  expect_equal(sort(unique(out$query_id)), c("q1", "q2", "q3", "q4"))
  expect_equal(length(unique(out$legend_label)), 4)
})

test_that("buildPlotData rejects more than four queries", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tibble(
    query_id = paste0("q", 1:5),
    country_id = "RUS",
    sex = "all",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "count"
  )

  expect_error(
    buildPlotData(
      recipes = recipes,
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      event_countries = buildTestEventCountries()
    ),
    "Maximum 4 queries"
  )
})

test_that("buildPlotData blocks incompatible country-event pairs", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(country_id = "USA")

  expect_error(
    buildPlotData(
      recipes = tibble::as_tibble(recipe),
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      event_countries = buildTestEventCountries()
    ),
    "not linked"
  )
})

test_that("buildPlotData multi-query respects per-country event links", {
  population <- dplyr::bind_rows(
    buildTestPopulation(),
    buildTestPopulation() |>
      dplyr::mutate(country_id = "USA", country_name = "United States")
  )
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "USA", "all", "adults", FALSE, NA_integer_, NA_integer_, "USA_NATIONAL_EVENT", "start", "count"
  )

  out <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries
  )

  expect_equal(sort(unique(out$query_id)), c("q1", "q2"))
  expect_match(out$legend_label[out$query_id == "q1"][[1]], "Russia")
  expect_match(out$legend_label[out$query_id == "q2"][[1]], "United States")
})

test_that("buildTrackerPlot renders for multi-query plot data", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )

  plot_data <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(plot_data, events = events, metric = "count")
  expect_s3_class(plot_obj, "ggplot")
})

test_that("buildQueryDetailsRows returns one row per query", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "adults", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )

  plot_data <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries(),
    migration = buildTestMigrationPrepared(rus_rate = 0)
  )

  details <- buildQueryDetailsRows(plot_data)
  expect_equal(nrow(details), 2)
  expect_true(all(c(
    "line_label", "chart_narrative", "query_description", "recipe_code",
    "reliability_score", "migration_exposure"
  ) %in% names(details)))
  expect_true(is.finite(details$reliability_score[[1]]))
  expect_gte(details$reliability_score[[1]], 70)
  expect_false(grepl("age_status_id", details$chart_narrative[[1]], fixed = TRUE))
  expect_match(details$query_description[[1]], "Birth years included:")
})

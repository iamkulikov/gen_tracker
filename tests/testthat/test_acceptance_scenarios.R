test_that("acceptance: school-aged Russians when Afghanistan war began", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(
    sex = "male",
    age_status_id = "school_age",
    event_id = "AFG_WAR",
    event_mode = "start",
    is_complement = FALSE,
    metric = "count"
  )

  assessment <- assessRecipe(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )
  expect_true(assessment$valid)

  series <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries
  )
  expect_true(nrow(series) > 0)
  expect_true(all(series$value >= 0))
  expect_true(any(series$year == 1979L))
})

test_that("acceptance: share metric stays within zero-one range", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(
    sex = "all",
    age_status_id = "school_age",
    metric = "share_total_population"
  )

  series <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries
  )

  expect_true(all(series$value >= 0 & series$value <= 1))
})

test_that("acceptance: projection years are flagged separately from estimates", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(metric = "count")
  series <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries
  )

  expect_true(any(series$is_projection))
  expect_true(any(!series$is_projection))
  expect_equal(
    series$is_projection,
    series$data_type == "projection"
  )
})

test_that("acceptance: filtering projection mirrors app year-range behaviour", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(metric = "count")
  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries
  )

  observed_only <- plot_data |>
    dplyr::filter(!.data$is_projection)

  expect_false(any(observed_only$is_projection))
  expect_true(all(observed_only$data_type == "estimate"))
  expect_lt(nrow(observed_only), nrow(plot_data))
})

test_that("acceptance: custom age range is validated and calculated", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(
    age_status_id = "custom",
    custom_age_min = 10L,
    custom_age_max = 14L
  )

  assessment <- assessRecipe(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )
  expect_true(assessment$valid)

  series <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries
  )
  expect_true(nrow(series) > 0)
})

test_that("acceptance: invalid custom age range is blocked before calculation", {
  countries <- buildTestCountries()
  events <- buildTestEvents()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  recipe <- buildTestRecipe(
    age_status_id = "custom",
    custom_age_min = 20L,
    custom_age_max = 10L
  )

  assessment <- assessRecipe(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )

  expect_false(assessment$valid)
  expect_match(assessment$errors[[1]], "Minimum age")
})

test_that("acceptance: compare male and female school-age cohorts on one chart", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

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
    event_countries = event_countries
  )

  expect_equal(length(unique(plot_data$query_id)), 2)
  expect_true(all(plot_data$value >= 0))
})

test_that("acceptance: recipe round-trip preserves calculation inputs", {
  recipe <- buildTestRecipe(
    sex = "female",
    age_status_id = "alive",
    event_mode = "period",
    metric = "share_total_population"
  )
  recipe$query_id <- "q3"

  code <- encodeRecipe(recipe)
  decoded <- decodeRecipe(code)

  expect_equal(decoded$country_id, recipe$country_id)
  expect_equal(decoded$sex, recipe$sex)
  expect_equal(decoded$age_status_id, recipe$age_status_id)
  expect_equal(decoded$is_complement, recipe$is_complement)
  expect_equal(decoded$event_id, recipe$event_id)
  expect_equal(decoded$event_mode, recipe$event_mode)
  expect_equal(decoded$metric, recipe$metric)
})

test_that("acceptance: global event works for any country in plot pipeline", {
  population <- buildTestPopulation()
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    country_id = "RUS",
    event_id = "GLOBAL_CRISIS",
    event_mode = "peak",
    age_status_id = "not_born_yet",
    metric = "share_total_population"
  )

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  expect_true(nrow(plot_data) > 0)
  expect_match(plot_data$line_label[[1]], "Russia")
})

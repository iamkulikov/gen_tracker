test_that("compositeEventsAsCatalog carries description through", {
  catalog <- buildCompositeTestEvents()
  expect_true("description" %in% names(catalog))
  expect_equal(catalog$description[[1]], "test composite")
})

test_that("collectQueryWarnings adds indicator coverage disclaimer for computed events", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "CMP_FX_RUS_1998", "FX 1998", "economy", "national", 1998L, 1998L, 1998L, "computed", FALSE
  )
  recipe <- buildTestRecipe(event_id = "CMP_FX_RUS_1998")
  countries <- buildTestCountries()
  country_row <- countries |> dplyr::filter(.data$country_id == "RUS") |> dplyr::slice(1)

  warnings <- collectQueryWarnings(
    recipe = recipe,
    event = events,
    country_row = country_row
  )

  expect_true(any(grepl("Indicator coverage limits computed and composite events", warnings)))
  expect_true(any(grepl("Inflation data starts in 1960", warnings)))
})

test_that("collectQueryWarnings adds disclaimer and composite description together", {
  events <- buildCompositeTestEvents()
  recipe <- buildTestRecipe(
    country_id = "RUS",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "experienced_any",
    event_mode = "start",
    metric = "count"
  )
  countries <- buildTestCountries()
  country_row <- countries |> dplyr::filter(.data$country_id == "RUS") |> dplyr::slice(1)

  warnings <- collectQueryWarnings(
    recipe = recipe,
    event = events,
    country_row = country_row
  )

  expect_true(any(grepl("Indicator coverage limits computed and composite events", warnings)))
  expect_true(any(grepl("test composite", warnings)))
})

test_that("collectQueryWarnings omits indicator disclaimer for manual events", {
  events <- buildTestEvents()
  recipe <- buildTestRecipe()
  countries <- buildTestCountries()
  country_row <- countries |> dplyr::filter(.data$country_id == "RUS") |> dplyr::slice(1)

  warnings <- collectQueryWarnings(
    recipe = recipe,
    event = events |> dplyr::filter(.data$event_id == recipe$event_id),
    country_row = country_row
  )

  expect_false(any(grepl("Indicator coverage limits computed and composite events", warnings)))
})

test_that("buildExportQueryDescriptions carries indicator disclaimer to export warnings", {
  population <- buildTestPopulation()
  events <- buildCompositeTestEventsUniverse()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(
    country_id = "RUS",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "experienced_any",
    event_mode = "start",
    metric = "count"
  )

  plot_data <- tibble::tibble(
    query_id = "q1",
    year = 1998L,
    value = 100,
    metric = "count",
    is_projection = FALSE,
    stratum_population = 100,
    total_country_population = 1000
  )

  descriptions <- buildExportQueryDescriptions(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups,
    population = population
  )

  expect_true(any(grepl("Indicator coverage limits computed and composite events", descriptions$warnings)))
  expect_true(any(grepl("test composite", descriptions$warnings)))
})

test_that("assessRecipe blocks national event for unlinked country", {
  events <- buildTestEvents()
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "USA", "United States", "USA", ""
    ))
  event_countries <- buildTestEventCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "USA",
    sex = "male",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "count"
  )

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = event_countries
  )

  expect_false(assessment$valid)
  expect_match(assessment$errors[[1]], "not linked")
})

test_that("assessRecipe accepts linked national event country", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- buildTestEventCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "RUS",
    sex = "male",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "count"
  )

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = event_countries
  )

  expect_true(assessment$valid)
})

test_that("assessRecipe allows global events for any country", {
  events <- buildTestEvents() |>
    dplyr::mutate(event_scope = "global", cross_country_allowed = TRUE)
  countries <- buildTestCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "RUS",
    sex = "all",
    age_status_id = "alive",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "period",
    metric = "share_total_population"
  )

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = NULL
  )

  expect_true(assessment$valid)
})

test_that("share_working_age_population is an allowed metric", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "RUS",
    sex = "all",
    age_status_id = "alive",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "period",
    metric = "share_working_age_population"
  )

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = NULL
  )

  expect_true(assessment$valid)
})

test_that("cross-country allowed national event warns but stays valid", {
  events <- buildTestEvents() |>
    dplyr::mutate(cross_country_allowed = TRUE)
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "USA", "United States", "USA", ""
    ))
  event_countries <- buildTestEventCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "USA",
    sex = "female",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "count"
  )

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = event_countries
  )

  expect_true(assessment$valid)
  expect_length(assessment$warnings, 1)
  expect_match(assessment$warnings[[1]], "not directly linked")
})

test_that("validateRecipe stops on incompatible country-event pair", {
  events <- buildTestEvents()
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "USA", "United States", "USA", ""
    ))
  event_countries <- buildTestEventCountries()
  recipe <- list(
    query_id = "q1",
    country_id = "USA",
    sex = "male",
    age_status_id = "school_age",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "count"
  )

  expect_error(
    validateRecipe(
      recipe,
      countries = countries,
      events = events,
      age_groups = defaultAgeGroups(),
      event_countries = event_countries
    ),
    "not linked"
  )
})

test_that("filterCompatibleEvents keeps global and linked events", {
  events <- buildTestEvents() |>
    dplyr::bind_rows(tibble::tribble(
      ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year,
      ~peak_year, ~event_origin, ~cross_country_allowed,
      "GLOBAL_CRISIS", "Global crisis", "economy", "global", 2000L, 2001L, 2000L, "manual", TRUE
    ))
  event_countries <- buildTestEventCountries()

  filtered <- filterCompatibleEvents(
    events = events,
    country_id = "USA",
    event_countries = event_countries
  )

  expect_equal(
    sort(filtered$event_id),
    c("GLOBAL_CRISIS", "MULTI_BORDER_WAR", "USA_NATIONAL_EVENT")
  )
})

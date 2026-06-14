test_that("filterCompatibleEvents keeps global, linked, and cross-country events", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  event_countries <- buildTestEventCountries()

  usa_events <- filterCompatibleEvents(
    events = events,
    country_id = "USA",
    event_countries = event_countries
  )
  expect_equal(
    sort(usa_events$event_id),
    c("GLOBAL_CRISIS", "MULTI_BORDER_WAR", "USA_NATIONAL_EVENT")
  )

  rus_events <- filterCompatibleEvents(
    events = events,
    country_id = "RUS",
    event_countries = event_countries
  )
  expect_equal(
    sort(rus_events$event_id),
    c("AFG_WAR", "GLOBAL_CRISIS")
  )
})

test_that("filterCompatibleCountries respects event scope and links", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  event_countries <- buildTestEventCountries()

  multi_countries <- filterCompatibleCountries(
    countries = countries,
    event_id = "MULTI_BORDER_WAR",
    events = events,
    event_countries = event_countries
  )
  expect_equal(sort(multi_countries$country_id), c("MEX", "USA"))

  national_countries <- filterCompatibleCountries(
    countries = countries,
    event_id = "USA_NATIONAL_EVENT",
    events = events,
    event_countries = event_countries
  )
  expect_equal(national_countries$country_id, "USA")

  global_countries <- filterCompatibleCountries(
    countries = countries,
    event_id = "GLOBAL_CRISIS",
    events = events,
    event_countries = event_countries
  )
  expect_equal(nrow(global_countries), nrow(countries))
})

test_that("assessRecipe blocks unlinked national event for wrong country", {
  events <- buildTestEvents()
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "USA", "United States", "USA", ""
    ))
  event_countries <- buildTestEventCountries()
  recipe <- buildTestRecipe(country_id = "USA", event_id = "AFG_WAR")

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

test_that("assessRecipe accepts multi-country event for linked counterpart", {
  events <- buildTestEvents()
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  event_countries <- buildTestEventCountries()
  recipe <- buildTestRecipe(country_id = "MEX", event_id = "MULTI_BORDER_WAR")

  assessment <- assessRecipe(
    recipe,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = event_countries
  )

  expect_true(assessment$valid)
})

test_that("filterCompatibleEvents returns empty when country_id is missing", {
  events <- buildTestEvents()
  filtered <- filterCompatibleEvents(
    events = events,
    country_id = NULL,
    event_countries = buildTestEventCountries()
  )
  expect_equal(nrow(filtered), 0L)
})

test_that("sortEventsForCountry lists linked events before global events", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  event_countries <- buildTestEventCountries()

  sorted <- filterCompatibleEvents(
    events = events,
    country_id = "RUS",
    event_countries = event_countries
  )
  expect_equal(sorted$event_id[[1]], "AFG_WAR")
  expect_equal(sorted$event_id[[2]], "GLOBAL_CRISIS")
})

test_that("filterCompatibleEvents places global events before other non-primary events", {
  events <- buildTestEvents() |>
    dplyr::bind_rows(buildTestGlobalEvent()) |>
    dplyr::bind_rows(tibble::tribble(
      ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
      "OTHER_ALLOWED", "Other allowed war", "war", "multi_country", 2015L, 2016L, 2015L, "manual", TRUE
    ))
  event_countries <- buildTestEventCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~event_id, ~country_id, ~country_role,
      "OTHER_ALLOWED", "MEX", "affected"
    ))

  sorted <- filterCompatibleEvents(
    events = events,
    country_id = "USA",
    event_countries = event_countries
  )
  expect_equal(
    sorted$event_id,
    c("USA_NATIONAL_EVENT", "MULTI_BORDER_WAR", "GLOBAL_CRISIS", "OTHER_ALLOWED")
  )
})

test_that("eventListTier orders primary, global, then other", {
  expect_equal(eventListTier("national", TRUE), 1L)
  expect_equal(eventListTier("global", FALSE), 2L)
  expect_equal(eventListTier("multi_country", FALSE), 3L)
})

test_that("sortEventsForCountry orders by start_year within relevance tier", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "RUS_OLD", "Older Russian event", "politics", "national", 1990L, 1991L, 1990L, "manual", FALSE,
    "RUS_NEW", "Recent Russian event", "politics", "national", 2014L, 2015L, 2014L, "manual", FALSE
  )
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "RUS_OLD", "RUS", "affected",
    "RUS_NEW", "RUS", "affected"
  )

  sorted <- sortEventsForCountry(
    events = events,
    country_id = "RUS",
    event_countries = event_countries
  )
  expect_equal(sorted$event_id, c("RUS_NEW", "RUS_OLD"))
})

test_that("formatEventChoiceLabel appends year or year range", {
  expect_equal(formatEventYearRange(1979L, 1989L), "1979–1989")
  expect_equal(formatEventYearRange(2001L, 2001L), "2001")
  expect_equal(
    formatEventChoiceLabel("War in Afghanistan", 1979L, 1989L),
    "War in Afghanistan (1979–1989)"
  )
})

test_that("isEventPrimaryForCountry detects links and national prefix", {
  event_countries <- buildTestEventCountries()
  expect_true(isEventPrimaryForCountry(
    "AFG_WAR", "national", "RUS", event_countries
  ))
  expect_false(isEventPrimaryForCountry(
    "GLOBAL_CRISIS", "global", "RUS", event_countries
  ))
})

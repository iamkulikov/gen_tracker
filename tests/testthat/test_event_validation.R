test_that("event validation checks key constraints", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  event_countries <- buildTestEventCountries()

  expect_no_error(validateEvents(events, countries, event_countries))
})

test_that("event validation fails for inverted years", {
  bad_events <- buildTestEvents() |>
    dplyr::mutate(start_year = 1990L, end_year = 1980L)

  expect_error(validateEvents(bad_events), "start_year > end_year")
})

test_that("event validation fails for unsupported event_type", {
  bad_events <- buildTestEvents() |>
    dplyr::mutate(event_type = "unknown_type")

  expect_error(validateEvents(bad_events), "event_type must be one of")
})

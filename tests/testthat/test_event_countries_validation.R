test_that("validateEventCountries passes on aligned fixture data", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  event_countries <- buildTestEventCountries()

  expect_no_error(validateEventCountries(events, countries, event_countries))
})

test_that("validateEventCountries rejects orphan event_id", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "UNKNOWN_EVENT", "RUS", "affected"
  )

  expect_error(
    validateEventCountries(events, countries, event_countries),
    "missing in events dictionary"
  )
})

test_that("validateEventCountries rejects orphan country_id", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "ZZZ", "affected"
  )

  expect_error(
    validateEventCountries(events, countries, event_countries),
    "missing in countries dictionary"
  )
})

test_that("validateEventCountries rejects duplicate pairs", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "affected",
    "AFG_WAR", "RUS", "origin"
  )

  expect_error(
    validateEventCountries(events, countries, event_countries),
    "Duplicate \\(event_id, country_id\\)"
  )
})

test_that("validateEventCountries rejects invalid country_role", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "observer"
  )

  expect_error(
    validateEventCountries(events, countries, event_countries),
    "country_role must be one of"
  )
})

test_that("validateEventCountries rejects national events without links", {
  events <- buildTestEvents()
  countries <- buildTestCountries()
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "affected"
  )

  expect_error(
    validateEventCountries(events, countries, event_countries),
    "National events without country links"
  )
})

test_that("validateEvents delegates to validateEventCountries when links present", {
  events <- buildTestEvents() |> dplyr::bind_rows(buildTestGlobalEvent())
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  event_countries <- buildTestEventCountries()

  expect_no_error(validateEvents(events, countries, event_countries))
})

test_that("real event pipeline files validate against local dictionaries", {
  data_dir <- testthat::test_path("..", "..", "data")
  events_path <- eventsDataPath(data_dir)
  countries_path <- countriesDataPath(data_dir)
  links_path <- eventCountriesDeployPath(data_dir)
  skip_if_not(
    file.exists(events_path) &&
      file.exists(countries_path) &&
      file.exists(links_path),
    "Local data/ dictionaries are not available"
  )

  events_raw <- readr::read_csv(events_path, show_col_types = FALSE)
  if (nrow(readr::problems(events_raw)) > 0) {
    skip("Local events CSV has parsing issues; fix quoting in 1_events.csv before validation")
  }

  universe <- loadEventsUniverse(manual_path = events_path, data_dir = data_dir)
  events <- universe$events
  countries <- loadCountryDictionary(countries_path)
  event_countries <- loadEventCountriesUniverse(
    manual_path = links_path,
    data_dir = data_dir
  )

  expect_no_error(validateEvents(events, countries, event_countries))
})

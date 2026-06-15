test_that("formatComputedEventName substitutes country and year placeholders", {
  expect_equal(
    formatComputedEventName(
      "Currency crisis in {country_id} ({year})",
      "RUS",
      1998L
    ),
    "Currency crisis in RUS (1998)"
  )
})

test_that("eventsForEventPicker hides computed events unless show_in_picker is TRUE", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed, ~show_in_picker,
    "AFG_WAR", "War", "war", "national", 1979L, 1989L, 1984L, "manual", FALSE, TRUE,
    "CMP_FX_RUS_1998", "FX 1998", "economy", "national", 1998L, 1998L, 1998L, "computed", FALSE, FALSE,
    "MERGE_FX_RUS", "FX composite", "economy", "national", 1998L, 2014L, 2014L, "composite", FALSE, TRUE
  )
  picker <- eventsForEventPicker(normalizeEventsCatalog(events))
  expect_equal(sort(picker$event_id), c("AFG_WAR", "MERGE_FX_RUS"))
})

test_that("buildComputedEventsFromCriteria produces deterministic CMP ids", {
  indicators <- buildFixtureIndicators()
  criteria <- buildFixtureComputedCriteria()
  countries <- tibble::tribble(
    ~country_id, ~country_name, ~iso3, ~boundary_warning,
    "RUS", "Russia", "RUS", "",
    "USA", "United States", "USA", "",
    "ARG", "Argentina", "ARG", "",
    "GRC", "Greece", "GRC", ""
  )

  built <- buildComputedEventsFromCriteria(indicators, criteria, countries = countries)
  events <- built$events

  expect_gt(nrow(events), 0)
  expect_true(all(grepl("^CMP_", events$event_id)))
  expect_true(all(events$event_origin == "computed"))

  rus_fx <- events |>
    dplyr::filter(grepl("^CMP_FX_DEPRECIATION_50_RUS_", .data$event_id))
  expect_equal(sort(rus_fx$start_year), c(1998L, 2008L, 2014L))

  rebuilt <- buildComputedEventsFromCriteria(indicators, criteria, countries = countries)
  expect_equal(rebuilt$events$event_id, built$events$event_id)
})

test_that("assertNoEventIdCollisions rejects manual CMP prefix", {
  manual <- tibble::tibble(event_id = "CMP_BAD")
  computed <- tibble::tibble(event_id = "CMP_OK_RUS_2000")
  expect_error(assertNoEventIdCollisions(manual, computed), "CMP_ prefix")
})

test_that("loadEventsUniverse merges manual and computed without overlap", {
  tmp <- tempfile("gt_events_")
  dir.create(tmp)
  manual_path <- file.path(tmp, DATA_FILE_EVENTS)
  computed_path <- file.path(tmp, DATA_FILE_EVENTS_COMPUTED)

  manual <- buildTestEvents()
  readr::write_csv(manual, manual_path)
  countries <- tibble::tribble(
    ~country_id, ~country_name, ~iso3, ~boundary_warning,
    "RUS", "Russia", "RUS", "",
    "USA", "United States", "USA", "",
    "ARG", "Argentina", "ARG", "",
    "GRC", "Greece", "GRC", ""
  )
  built <- buildComputedEventsFromCriteria(
    buildFixtureIndicators(),
    buildFixtureComputedCriteria(),
    countries = countries
  )
  readr::write_csv(
    built$events |> dplyr::select(
      "event_id", "event_name", "event_type", "event_scope",
      "start_year", "end_year", "peak_year", "event_origin", "cross_country_allowed"
    ),
    computed_path
  )

  universe <- loadEventsUniverse(manual_path = manual_path, computed_path = computed_path, data_dir = tmp)
  expect_true("AFG_WAR" %in% universe$events$event_id)
  expect_true(any(grepl("^CMP_", universe$events$event_id)))
  expect_equal(
    length(intersect(manual$event_id, built$events$event_id)),
    0
  )
})

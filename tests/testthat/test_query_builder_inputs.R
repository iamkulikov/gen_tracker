test_that("queryBuilderInputsReady is false until module inputs are bound", {
  empty_input <- list(
    country_id = NULL,
    sex = NULL,
    age_status_id = NULL,
    age_modifier = NULL,
    event_id = NULL,
    event_mode = NULL,
    custom_age_min = NULL,
    custom_age_max = NULL
  )
  expect_false(queryBuilderInputsReady(empty_input))

  ready_input <- list(
    country_id = "RUS",
    sex = "all",
    age_status_id = "school_age",
    age_modifier = "none",
    event_id = "AFG_WAR",
    event_mode = "start",
    custom_age_min = 0L,
    custom_age_max = 100L
  )
  expect_true(queryBuilderInputsReady(ready_input))
})

test_that("query builder selectInput choices submit internal codes", {
  sex <- queryBuilderSexChoices()
  expect_equal(unname(sex), c("all", "male", "female"))
  expect_equal(names(sex), c("People", "Men", "Women"))

  modes <- queryBuilderEventModeChoices()
  expect_equal(unname(modes), c("start", "end", "period", "peak"))

  modifiers <- queryBuilderComplementChoices()
  expect_equal(unname(modifiers), c("none", "not", "younger_than", "older_than"))

  age_statuses <- queryBuilderAgeStatusChoices(defaultAgeGroups())
  expect_equal(
    tail(unname(age_statuses), 2),
    c("not_born_yet", "custom")
  )
  expect_true("Conscious (7+)" %in% names(age_statuses))
  expect_equal(tail(unname(age_statuses), 1), "custom")
})

test_that("queryBuilderInputsReady accepts numeric zero for custom age min", {
  ready_input <- list(
    country_id = "RUS",
    sex = "male",
    age_status_id = "custom",
    age_modifier = "none",
    event_id = "AFG_WAR",
    event_mode = "period",
    custom_age_min = 0L,
    custom_age_max = 12L
  )
  expect_true(queryBuilderInputsReady(ready_input))
})

test_that("queryBuilderInputsReady does not require custom ages for preset groups", {
  ready_input <- list(
    country_id = "RUS",
    sex = "all",
    age_status_id = "adults",
    age_modifier = "none",
    event_id = "AFG_WAR",
    event_mode = "start"
  )
  expect_true(queryBuilderInputsReady(ready_input))
})

test_that("resolveQueryBuilderCountryId falls back when country input is empty", {
  countries <- buildTestCountries()
  expect_equal(resolveQueryBuilderCountryId(NULL, countries), "RUS")
  expect_equal(resolveQueryBuilderCountryId(character(0), countries), "RUS")
  expect_equal(resolveQueryBuilderCountryId("USA", countries), "USA")
})

test_that("lookupQueryBuilderEvent returns the matching catalog row", {
  events <- buildTestEvents() |>
    dplyr::mutate(
      short_description = dplyr::case_when(
        .data$event_id == "AFG_WAR" ~ "Soviet intervention in Afghanistan.",
        .default = NA_character_
      ),
      source_url = dplyr::case_when(
        .data$event_id == "AFG_WAR" ~ "https://en.wikipedia.org/wiki/Soviet%E2%80%93Afghan_War",
        .default = NA_character_
      )
    )

  row <- lookupQueryBuilderEvent(events, "AFG_WAR")
  expect_equal(row$event_id[[1]], "AFG_WAR")
  expect_equal(row$short_description[[1]], "Soviet intervention in Afghanistan.")
  expect_null(lookupQueryBuilderEvent(events, "MISSING"))
  expect_null(lookupQueryBuilderEvent(events, NULL))
})

test_that("queryBuilderEventInfoUi renders icon tooltip with description and source", {
  event_row <- buildTestEvents() |>
    dplyr::filter(.data$event_id == "AFG_WAR") |>
    dplyr::mutate(
      short_description = "A short event summary.",
      source_url = "https://example.org/source"
    )
  ui <- queryBuilderEventInfoUi(event_row)
  html <- as.character(ui)
  expect_match(html, "query-event-info-trigger")
  expect_match(html, "query-event-info-popover")
  expect_match(html, "A short event summary\\.")
  expect_match(html, "query-event-info-link")
  expect_match(html, "https://example.org/source")
})

test_that("queryBuilderEventInfoUi hides when metadata is missing", {
  event_row <- buildTestEvents() |> dplyr::slice(1)
  expect_null(queryBuilderEventInfoUi(event_row))
  expect_null(queryBuilderEventInfoUi(NULL))
})

test_that("queryBuilderEventInfoUi can render source-only metadata", {
  event_row <- buildTestEvents() |>
    dplyr::filter(.data$event_id == "AFG_WAR") |>
    dplyr::mutate(
      short_description = NA_character_,
      source_url = "https://example.org/source-only"
    )
  ui <- queryBuilderEventInfoUi(event_row)
  html <- as.character(ui)
  expect_match(html, "query-event-info-link")
  expect_false(grepl("query-event-info-desc", html))
})

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

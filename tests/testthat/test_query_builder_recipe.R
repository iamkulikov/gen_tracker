test_that("assembleQueryBuilderRecipe maps preset age group without custom bounds", {
  input <- list(
    country_id = "RUS",
    sex = "male",
    age_status_id = "school_age",
    age_modifier = "none",
    custom_age_min = 99L,
    custom_age_max = 100L,
    event_id = "AFG_WAR",
    event_mode = "start"
  )

  recipe <- assembleQueryBuilderRecipe(input, query_id = "q1", metric = "count")

  expect_equal(recipe$age_status_id, "school_age")
  expect_equal(recipe$age_modifier, "none")
  expect_false(recipe$is_complement)
  expect_true(is.na(recipe$custom_age_min))
  expect_true(is.na(recipe$custom_age_max))
  expect_equal(recipe$metric, "count")
})

test_that("assembleQueryBuilderRecipe keeps custom age bounds for custom group", {
  input <- list(
    country_id = "RUS",
    sex = "all",
    age_status_id = "custom",
    age_modifier = "not",
    custom_age_min = 10L,
    custom_age_max = 14L,
    event_id = "AFG_WAR",
    event_mode = "period"
  )

  recipe <- assembleQueryBuilderRecipe(
    input,
    query_id = "q2",
    metric = "share_total_population"
  )

  expect_equal(recipe$age_status_id, "custom")
  expect_equal(recipe$age_modifier, "not")
  expect_true(recipe$is_complement)
  expect_equal(recipe$custom_age_min, 10L)
  expect_equal(recipe$custom_age_max, 14L)
  expect_equal(recipe$metric, "share_total_population")
})

test_that("assembled custom-age recipe validates before calculation", {
  countries <- buildTestCountries()
  events <- buildTestEvents()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()

  valid_input <- list(
    country_id = "RUS",
    sex = "male",
    age_status_id = "custom",
    age_modifier = "none",
    custom_age_min = 10L,
    custom_age_max = 14L,
    event_id = "AFG_WAR",
    event_mode = "start"
  )
  valid_recipe <- assembleQueryBuilderRecipe(valid_input, "q1", "count")
  valid_assessment <- assessRecipe(
    recipe = valid_recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )
  expect_true(valid_assessment$valid)

  invalid_input <- valid_input
  invalid_input$custom_age_min <- 20L
  invalid_input$custom_age_max <- 10L
  invalid_recipe <- assembleQueryBuilderRecipe(invalid_input, "q1", "count")
  invalid_assessment <- assessRecipe(
    recipe = invalid_recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries
  )
  expect_false(invalid_assessment$valid)
})

test_that("assembleQueryBuilderRecipe respects metric reactive value", {
  input <- list(
    country_id = "RUS",
    sex = "female",
    age_status_id = "adults",
    age_modifier = "younger_than",
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "peak"
  )

  recipe <- assembleQueryBuilderRecipe(input, "q3", metric = "share_total_population")
  expect_equal(recipe$query_id, "q3")
  expect_equal(recipe$sex, "female")
  expect_equal(recipe$event_mode, "peak")
  expect_equal(recipe$age_modifier, "younger_than")
  expect_false(recipe$is_complement)
})

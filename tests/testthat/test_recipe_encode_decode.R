test_that("recipe encode/decode is stable for GEN2", {
  recipe <- list(
    query_id = "q1",
    country_id = "RUS",
    sex = "male",
    age_status_id = "school_age",
    age_modifier = "not",
    is_complement = TRUE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "end",
    metric = "share_total_population"
  )

  code <- encodeRecipe(recipe)
  decoded <- decodeRecipe(code)

  expect_equal(decoded$country_id, recipe$country_id)
  expect_equal(decoded$sex, recipe$sex)
  expect_equal(decoded$age_status_id, recipe$age_status_id)
  expect_equal(decoded$age_modifier, recipe$age_modifier)
  expect_equal(decoded$is_complement, recipe$is_complement)
  expect_equal(decoded$event_id, recipe$event_id)
  expect_equal(decoded$event_mode, recipe$event_mode)
  expect_equal(decoded$metric, recipe$metric)
})

test_that("recipe encode/decode round-trips share_working_age_population", {
  recipe <- list(
    query_id = "q1",
    country_id = "RUS",
    sex = "male",
    age_status_id = "school_age",
    age_modifier = "none",
    is_complement = FALSE,
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_,
    event_id = "AFG_WAR",
    event_mode = "start",
    metric = "share_working_age_population"
  )

  code <- encodeRecipe(recipe)
  decoded <- decodeRecipe(code)
  expect_equal(decoded$metric, "share_working_age_population")
})

test_that("unknown recipe version fails", {
  expect_error(decodeRecipe("GEN1:country=RUS"), "Unsupported recipe code version")
})

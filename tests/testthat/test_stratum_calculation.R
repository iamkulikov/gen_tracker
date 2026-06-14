test_that("stratum calculation returns count metric", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

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

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(nrow(out) > 0)
  expect_true(all(out$value >= 0))
})

test_that("experienced start is zero before event year (count)", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "school_age",
    event_mode = "start",
    is_complement = FALSE,
    metric = "count"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year < 1979L] == 0))
  expect_gt(sum(out$value[out$year >= 1979L]), 0)
})

test_that("experienced start is zero before event year (share)", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "school_age",
    event_mode = "start",
    is_complement = FALSE,
    metric = "share_total_population"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year < 1979L] == 0))
  expect_gt(sum(out$value[out$year >= 1979L]), 0)
})

test_that("alive during period is zero before period start", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1981L, peak_year = 1980L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "alive",
    event_mode = "period",
    is_complement = FALSE,
    metric = "count"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year < 1979L] == 0))
  expect_gt(sum(out$value[out$year >= 1979L]), 0)
})

test_that("born after event stays zero up to event end year", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1981L, peak_year = 1980L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "not_born_yet",
    event_mode = "period",
    is_complement = FALSE,
    metric = "count"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year <= 1981L] == 0))
  expect_gt(sum(out$value[out$year > 1981L]), 0)
})

test_that("complemented age status is non-zero before event year", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "school_age",
    event_mode = "start",
    is_complement = TRUE,
    metric = "count"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value >= 0))
  expect_gt(sum(out$value[out$year < 1979L]), 0)
})

test_that("positive age status at event end is zero before end year", {
  population <- buildSemanticsPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1981L, peak_year = 1980L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "school_age",
    event_mode = "end",
    is_complement = FALSE,
    metric = "count"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year < 1981L] == 0))
  expect_gt(sum(out$value[out$year >= 1981L]), 0)
})

test_that("resolveEventEpisodes expands multi-year period into per-year episodes", {
  event <- buildSemanticsEvent(start_year = 1980L, end_year = 1983L, peak_year = 1982L)
  episodes <- resolveEventEpisodes(event, "period")
  expect_equal(nrow(episodes), 4)
  expect_equal(episodes$threshold, 1980:1983)
})

test_that("long period adults accumulates cohorts by year without jump at start", {
  population <- buildSemanticsPopulation(years = 1975:1990, ages = 0:100)
  events <- buildSemanticsEvent(start_year = 1980L, end_year = 1983L, peak_year = 1982L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipe <- buildTestRecipe(
    age_status_id = "custom",
    custom_age_min = 18L,
    custom_age_max = 25L,
    event_mode = "period",
    is_complement = FALSE,
    metric = "count",
    sex = "all"
  )

  out <- calculateStratumSeries(recipe, population, events, age_groups, countries)
  expect_true(all(out$value[out$year < 1980L] == 0))
  expect_gt(out$value[out$year == 1980L], 0)
  expect_gt(out$value[out$year == 1983L], out$value[out$year == 1980L])
  expect_gt(out$value[out$year == 1981L], out$value[out$year == 1980L])
})

test_that("long period alive assigns per-year witness thresholds", {
  events <- buildSemanticsEvent(start_year = 1980L, end_year = 1983L, peak_year = 1982L)
  recipe <- buildTestRecipe(
    age_status_id = "alive",
    event_mode = "period",
    is_complement = FALSE,
    metric = "count",
    sex = "all"
  )
  age_range <- resolveAgeRange(recipe, defaultAgeGroups())
  thresholds <- resolveBirthYearThresholds(
    recipe = recipe,
    event = events,
    age_range = age_range,
    population_years = 1975:1990,
    population_ages = 0:100,
    events = events
  )
  expect_equal(thresholds$threshold[thresholds$birth_year == 1980L], 1980L)
  expect_equal(thresholds$threshold[thresholds$birth_year == 1983L], 1983L)
})

test_that("shouldAssessStratumGrowth excludes complement and not_born_yet", {
  base <- buildTestRecipe()
  expect_true(shouldAssessStratumGrowth(base))
  expect_false(shouldAssessStratumGrowth(buildTestRecipe(age_status_id = "not_born_yet")))
  expect_false(shouldAssessStratumGrowth(buildTestRecipe(is_complement = TRUE)))
  expect_false(shouldAssessStratumGrowth(buildTestRecipe(age_modifier = "not", is_complement = TRUE)))
})

test_that("stratumGrowthWarning flags material post-threshold increases", {
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1979:1983,
    value = c(100, 105, 110, 108, 112),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(100, 105, 110, 108, 112),
    total_country_population = 1000
  )

  msg <- stratumGrowthWarning(series, recipe, events)
  expect_length(msg, 1)
  expect_match(msg, "1979 and 1980")
  expect_match(msg, "migration")
})

test_that("stratumGrowthWarning ignores flat or declining series", {
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1979:1983,
    value = c(100, 99, 98, 97, 96),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(100, 99, 98, 97, 96),
    total_country_population = 1000
  )

  expect_length(stratumGrowthWarning(series, recipe, events), 0)
})

test_that("collectQueryWarnings includes growth diagnostic when series supplied", {
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")
  countries <- buildTestCountries()
  country_row <- countries |> dplyr::filter(.data$country_id == "RUS") |> dplyr::slice(1)
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1979:1982,
    value = c(100, 120, 130, 140),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(100, 120, 130, 140),
    total_country_population = 1000
  )

  warnings <- collectQueryWarnings(
    recipe = recipe,
    event = events,
    country_row = country_row,
    stratum_series = series
  )
  expect_true(any(grepl("Stratum population increases", warnings)))
})

test_that("buildPlotData attaches growth warning for suspicious series", {
  population <- buildGrowthSuspicionPopulation()
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1985L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries
  )

  expect_true(any(!is.na(plot_data$warning)))
  expect_match(plot_data$warning[[1]], "Stratum population increases")
})

test_that("stratumGrowthWarning ignores composite episode threshold steps", {
  events <- buildCompositeTestEventsUniverse()
  composite <- events |> dplyr::filter(.data$event_id == "MERGE_MAJOR_FX_DEPRECIATION_RUS")
  members <- buildCompositeTestMembers()
  recipe <- buildTestRecipe(
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "adults",
    event_mode = "start",
    metric = "count"
  )
  years <- 1997:2015
  stratum_population <- dplyr::case_when(
    years < 1998L ~ 0,
    years < 2008L ~ 100,
    years < 2014L ~ 200,
    TRUE ~ 300
  )
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    year = years,
    value = stratum_population,
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = stratum_population,
    total_country_population = 1000
  )

  msg <- stratumGrowthWarning(
    series,
    recipe,
    composite,
    composite_members = members,
    events = events
  )
  expect_length(msg, 0)
})

test_that("stratumGrowthWarning flags growth between composite episode thresholds", {
  events <- buildCompositeTestEventsUniverse()
  composite <- events |> dplyr::filter(.data$event_id == "MERGE_MAJOR_FX_DEPRECIATION_RUS")
  members <- buildCompositeTestMembers()
  recipe <- buildTestRecipe(
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "adults",
    event_mode = "start",
    metric = "count"
  )
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    year = 1998:2002,
    value = c(100, 130, 140, 150, 160),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(100, 130, 140, 150, 160),
    total_country_population = 1000
  )

  msg <- stratumGrowthWarning(
    series,
    recipe,
    composite,
    composite_members = members,
    events = events
  )
  expect_length(msg, 1)
  expect_match(msg, "1998 and 1999")
})

test_that("stratumGrowthWarning ignores long period threshold steps", {
  events <- buildSemanticsEvent(start_year = 1980L, end_year = 1983L, peak_year = 1982L)
  recipe <- buildTestRecipe(
    age_status_id = "adults",
    event_mode = "period",
    metric = "count"
  )
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1979:1984,
    value = c(0, 100, 150, 200, 250, 240),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(0, 100, 150, 200, 250, 240),
    total_country_population = 1000
  )

  msg <- stratumGrowthWarning(series, recipe, events, events = events)
  expect_length(msg, 0)
})

test_that("stratumGrowthWarning flags growth between long period threshold years", {
  events <- buildSemanticsEvent(start_year = 1980L, end_year = 1983L, peak_year = 1982L)
  recipe <- buildTestRecipe(
    age_status_id = "adults",
    event_mode = "period",
    metric = "count"
  )
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1983:1985,
    value = c(200, 230, 260),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(200, 230, 260),
    total_country_population = 1000
  )

  msg <- stratumGrowthWarning(series, recipe, events, events = events)
  expect_length(msg, 1)
  expect_match(msg, "1983 and 1984")
})

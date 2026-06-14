makeReliabilitySeries <- function(
  years = 1979:1985,
  stratum = 100,
  is_projection = FALSE
) {
  tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = as.integer(years),
    value = stratum,
    metric = "count",
    data_type = if (isTRUE(is_projection)) "projection" else "estimate",
    is_projection = is_projection,
    stratum_population = stratum,
    total_country_population = 1000
  )
}

test_that("computeStratumReliability returns NA when migration is unavailable", {
  series <- makeReliabilitySeries()
  recipe <- buildTestRecipe()
  result <- computeStratumReliability(
    series = series,
    recipe = recipe,
    country_row = buildTestCountries() |> dplyr::filter(country_id == "RUS"),
    migration = NULL
  )
  expect_true(is.na(result$reliability_score))
  expect_true(is.na(result$migration_exposure))
  expect_true(is.na(result$reliability_warning))
})

test_that("clean estimate with no migration scores 100", {
  series <- makeReliabilitySeries()
  recipe <- buildTestRecipe()
  migration <- buildTestMigrationPrepared(rus_rate = 0)
  result <- computeStratumReliability(
    series = series,
    recipe = recipe,
    country_row = buildTestCountries() |> dplyr::filter(country_id == "RUS"),
    event = buildSemanticsEvent(),
    event_countries = buildTestEventCountries(),
    migration = migration
  )
  expect_equal(result$reliability_score, 100)
  expect_equal(result$migration_exposure, 0)
  expect_true(is.na(result$reliability_warning))
})

test_that("high migration lowers the reliability score and emits a warning", {
  series <- makeReliabilitySeries()
  recipe <- buildTestRecipe()
  migration <- buildTestMigrationPrepared(rus_rate = 12)
  result <- computeStratumReliability(
    series = series,
    recipe = recipe,
    country_row = buildTestCountries() |> dplyr::filter(country_id == "RUS"),
    event = buildSemanticsEvent(),
    event_countries = buildTestEventCountries(),
    migration = migration
  )
  # exposure 12 -> penalty capped at 40
  expect_equal(result$reliability_score, 60)
  expect_equal(result$migration_exposure, 12)
  expect_match(result$reliability_warning, "Reliability 60/100")
  expect_match(result$reliability_warning, "net migration")
})

test_that("boundary warning applies a penalty", {
  series <- makeReliabilitySeries()
  recipe <- buildTestRecipe()
  migration <- buildTestMigrationPrepared(rus_rate = 0)
  country_row <- tibble::tibble(
    country_id = "RUS",
    country_name = "Russia",
    iso3 = "RUS",
    boundary_warning = "Borders changed since the USSR."
  )
  result <- computeStratumReliability(
    series = series,
    recipe = recipe,
    country_row = country_row,
    event = buildSemanticsEvent(),
    event_countries = buildTestEventCountries(),
    migration = migration
  )
  expect_equal(result$reliability_score, 85)
})

test_that("projection-heavy series loses projection points", {
  series <- makeReliabilitySeries(is_projection = TRUE)
  recipe <- buildTestRecipe()
  migration <- buildTestMigrationPrepared(rus_rate = 0)
  result <- computeStratumReliability(
    series = series,
    recipe = recipe,
    country_row = buildTestCountries() |> dplyr::filter(country_id == "RUS"),
    event = buildSemanticsEvent(),
    event_countries = buildTestEventCountries(),
    migration = migration
  )
  expect_equal(result$reliability_score, 80)
})

test_that("buildPlotData fills numeric reliability when migration supplied", {
  population <- buildSemanticsPopulation(years = 1979:1985)
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1985L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")
  migration <- buildTestMigrationPrepared(years = 1979:1985, rus_rate = 6)

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries(),
    migration = migration
  )

  expect_true(all(is.finite(plot_data$reliability_score)))
  expect_true("migration_exposure" %in% names(plot_data))
  expect_equal(unique(plot_data$reliability_score), 100 - 4 * 6)
})

test_that("buildPlotData keeps reliability NA without migration", {
  population <- buildSemanticsPopulation(years = 1979:1985)
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1985L, peak_year = 1984L)
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  expect_true(all(is.na(plot_data$reliability_score)))
})

test_that("formatReliabilitySummary shows score even when high", {
  summary <- formatReliabilitySummary(list(
    reliability_score = 94,
    migration_exposure = 1.5,
    reliability_warning = NA_character_
  ))
  expect_match(summary, "Reliability: 94/100")
  expect_true(is.na(buildReliabilityWarning(94, character(0))))
})

test_that("formatReliabilityScoreLine is emitted for high scores", {
  line <- formatReliabilityScoreLine(list(reliability_score = 100, migration_exposure = 0))
  expect_equal(line, "Reliability score: 100/100")
})

test_that("collectQueryWarnings includes reliability score line when migration loaded", {
  events <- buildSemanticsEvent(start_year = 1979L, end_year = 1985L, peak_year = 1984L)
  recipe <- buildTestRecipe(age_status_id = "school_age", event_mode = "start", metric = "count")
  countries <- buildTestCountries()
  country_row <- countries |> dplyr::filter(.data$country_id == "RUS") |> dplyr::slice(1)
  series <- tibble::tibble(
    query_id = "q1",
    country_id = "RUS",
    event_id = "AFG_WAR",
    year = 1979:1982,
    value = c(100, 99, 98, 97),
    metric = "count",
    data_type = "estimate",
    is_projection = FALSE,
    stratum_population = c(100, 99, 98, 97),
    total_country_population = 1000
  )

  warnings <- collectQueryWarnings(
    recipe = recipe,
    event = events,
    country_row = country_row,
    stratum_series = series,
    migration = buildTestMigrationPrepared(rus_rate = 0)
  )
  expect_true(any(grepl("Reliability score: 100/100", warnings)))
  expect_false(any(grepl("Reliability score is not yet implemented", warnings)))
})

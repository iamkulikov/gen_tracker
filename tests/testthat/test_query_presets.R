test_that("resolvePresetEventId prefers explicit candidates", {
  events <- buildTestEvents()
  expect_equal(
    resolvePresetEventId(c("AFG_WAR", "RUS_AFGHAN_WAR"), "RUS", events),
    "AFG_WAR"
  )
})

test_that("resolvePresetEventId falls back to country prefix", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "RUS_AFGHAN_WAR", "War in Afghanistan", "war", "national", 1979L, 1989L, 1984L, "manual", FALSE
  )
  expect_equal(
    resolvePresetEventId(c("MISSING_EVENT"), "RUS", events),
    "RUS_AFGHAN_WAR"
  )
})

test_that("demo presets define acceptance-style scenarios", {
  presets <- demoQueryPresets()
  expect_true("afghan_school_boys" %in% names(presets))
  expect_equal(presets$compare_sex$query_count, 2L)
  expect_equal(presets$afghan_share$metric, "share_total_population")
})

test_that("populationYearBounds separates estimate ceiling from projection max", {
  population <- buildTestPopulation()
  bounds <- populationYearBounds(population)
  expect_equal(bounds$min, 1979L)
  expect_equal(bounds$max, 1985L)
  expect_equal(bounds$estimate_max, 1982L)
  expect_equal(bounds$default_range, c(1979L, 1985L))
})

test_that("seedSessionDefaultQueries and resetDemoQueries share the default recipe", {
  recipe <- defaultQueryRecipe(
    query_id = "q1",
    countries = buildTestCountries(),
    events = buildTestEvents()
  )
  expect_equal(recipe$country_id, "RUS")
  expect_equal(recipe$event_id, "AFG_WAR")
  expect_type(seedSessionDefaultQueries, "closure")
  expect_type(resetDemoQueries, "closure")
})

test_that("defaultQueryRecipe resolves against loaded dictionaries", {
  recipe <- defaultQueryRecipe(
    query_id = "q1",
    countries = buildTestCountries(),
    events = buildTestEvents()
  )
  expect_equal(recipe$country_id, "RUS")
  expect_equal(recipe$event_id, "AFG_WAR")
  expect_equal(recipe$age_status_id, "adults")
  expect_false(recipe$is_complement)
})

test_that("formatLinkedCountryLabels uses country names when available", {
  countries <- buildTestCountries() |>
    dplyr::bind_rows(tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "MEX", "Mexico", "MEX", ""
    ))
  labels <- formatLinkedCountryLabels(c("RUS", "MEX"), countries = countries)
  expect_equal(labels, c("Russia", "Mexico"))
})

test_that("eventWindowDefaultStart rounds down to the decade before earliest event", {
  expect_equal(eventWindowDefaultStart(c(1979L, 1991L), 1950L, 2100L), 1970L)
  expect_equal(eventWindowDefaultStart(2007L, 1950L, 2100L), 2000L)
})

test_that("eventWindowDefaultStart steps back a decade on exact decade boundaries", {
  expect_equal(eventWindowDefaultStart(2010L, 1950L, 2100L), 2000L)
  expect_equal(eventWindowDefaultStart(1980L, 1950L, 2100L), 1970L)
})

test_that("eventWindowDefaultStart clamps to the available population range", {
  expect_equal(eventWindowDefaultStart(1946L, 1950L, 2100L), 1950L)
  expect_equal(eventWindowDefaultStart(1950L, 1950L, 2100L), 1950L)
  expect_equal(eventWindowDefaultStart(2099L, 1950L, 2100L), 2090L)
})

test_that("eventWindowDefaultStart falls back to year_min without valid events", {
  expect_equal(eventWindowDefaultStart(integer(), 1950L, 2100L), 1950L)
  expect_equal(eventWindowDefaultStart(c(NA_integer_, NA_integer_), 1950L, 2100L), 1950L)
})

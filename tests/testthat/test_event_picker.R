test_that("formatCompositeEpisodeCount uses English pluralization", {
  expect_equal(formatCompositeEpisodeCount(1), "1 episode")
  expect_equal(formatCompositeEpisodeCount(3), "3 episodes")
  expect_equal(formatCompositeEpisodeCount(0), "0 episodes")
})

test_that("buildCompositeEventsFromTags uses English episode count for custom composite_name", {
  events <- buildCompositeTestElementaryEvents()
  tags <- tibble::tribble(
    ~event_id, ~tag,
    "CMP_FX_RUS_1998", "major_fx_depreciation",
    "CMP_FX_RUS_2008", "major_fx_depreciation",
    "CMP_FX_RUS_2014", "major_fx_depreciation"
  )

  built <- buildCompositeEventsFromTags(
    events = events,
    event_tags = tags,
    classifier_spec = 'tag == "major_fx_depreciation"',
    countries = buildTestCountries(),
    composite_name = "Дефолты по гособлигациям"
  )

  expect_match(built$composite_events$event_name[[1]], "Дефолты по гособлигациям")
  expect_match(built$composite_events$event_name[[1]], "3 episodes")
  expect_false(grepl("эпизод", built$composite_events$event_name[[1]]))
})

test_that("buildYearMarkerEvents creates stable global year events", {
  markers <- buildYearMarkerEvents(2009L, 2011L)
  expect_equal(markers$event_id, c("YEAR_2009", "YEAR_2010", "YEAR_2011"))
  expect_equal(markers$event_name, c("2009", "2010", "2011"))
  expect_true(all(markers$event_origin == "year_marker"))
  expect_true(all(markers$event_scope == "global"))
})

test_that("eventsForEventPicker includes year marker events", {
  events <- buildTestEvents() |>
    dplyr::bind_rows(buildYearMarkerEvents(2009L, 2010L))
  picker <- eventsForEventPicker(normalizeEventsCatalog(events))
  expect_true("YEAR_2010" %in% picker$event_id)
})

test_that("countryCodeFromEventId extracts ISO3 prefixes and skips system codes", {
  expect_equal(countryCodeFromEventId("KOR_OLYMPICS_1988"), "KOR")
  expect_equal(countryCodeFromEventId("GLB_COVID_19_PANDEMIC"), NA_character_)
  expect_equal(countryCodeFromEventId("YEAR_2010"), NA_character_)
})

test_that("isCountryScopedGlobalEvent detects country-prefixed global rows", {
  expect_true(isCountryScopedGlobalEvent("GBR_COVID_LOCKDOWNS_2020", "global"))
  expect_false(isCountryScopedGlobalEvent("GLB_COVID_19_PANDEMIC", "global"))
  expect_false(isCountryScopedGlobalEvent("GBR_COVID_LOCKDOWNS_2020", "national"))
})

test_that("formatEventPickerOptionLabel uses bare year name for years group", {
  expect_equal(
    formatEventPickerOptionLabel(
      event_name = "2010",
      start_year = 2010L,
      end_year = 2010L,
      event_id = "YEAR_2010",
      group = "years"
    ),
    "2010"
  )
})

test_that("partitionEventPickerIndices orders years oldest to newest", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "YEAR_2012", "2012", "year_marker", "global", 2012L, 2012L, 2012L, "year_marker", TRUE,
    "YEAR_2010", "2010", "year_marker", "global", 2010L, 2010L, 2010L, "year_marker", TRUE,
    "YEAR_2011", "2011", "year_marker", "global", 2011L, 2011L, 2011L, "year_marker", TRUE
  )

  groups <- partitionEventPickerIndices(events, "RUS", NULL)
  expect_equal(
    events$event_id[groups$years],
    c("YEAR_2010", "YEAR_2011", "YEAR_2012")
  )
})

test_that("formatEventPickerOptionLabel prefixes country code only for other events", {
  expect_equal(
    formatEventPickerOptionLabel(
      event_name = "Seoul Olympics",
      start_year = 1988L,
      end_year = 1988L,
      event_id = "KOR_OLYMPICS_1988",
      group = "other"
    ),
    "KOR: Seoul Olympics (1988)"
  )
  expect_equal(
    formatEventPickerOptionLabel(
      event_name = "Seoul Olympics",
      start_year = 1988L,
      end_year = 1988L,
      event_id = "KOR_OLYMPICS_1988",
      group = "primary"
    ),
    "Seoul Olympics (1988)"
  )
})

test_that("partitionEventPickerIndices orders groups and reclassifies country-scoped globals", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "RUS_NEW", "Recent Russian event", "politics", "national", 2014L, 2015L, 2014L, "manual", FALSE,
    "MERGE_RUS_FX", "FX episodes in Russia", "economy", "national", 1998L, 2014L, 2014L, "composite", FALSE,
    "RUS_OLD", "Older Russian event", "politics", "national", 1990L, 1991L, 1990L, "manual", FALSE,
    "GLB_CRISIS", "Global crisis", "economy", "global", 2008L, 2009L, 2008L, "manual", TRUE,
    "GBR_COVID_LOCKDOWNS_2020", "COVID-19 lockdowns", "health", "global", 2020L, 2020L, 2020L, "manual", TRUE,
    "IDN_COVID_2020_2022", "COVID-19 pandemic", "health", "national", 2020L, 2022L, 2021L, "manual", TRUE,
    "YEAR_2010", "2010", "year_marker", "global", 2010L, 2010L, 2010L, "year_marker", TRUE
  )
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "RUS_NEW", "RUS", "affected",
    "RUS_OLD", "RUS", "affected",
    "MERGE_RUS_FX", "RUS", "affected"
  )

  events <- sortEventsForCountry(events, "RUS", event_countries)
  groups <- partitionEventPickerIndices(events, "RUS", event_countries)
  expect_equal(
    events$event_id[groups$primary],
    c("MERGE_RUS_FX", "RUS_NEW", "RUS_OLD")
  )
  expect_equal(events$event_id[groups$global], "GLB_CRISIS")
  expect_equal(
    events$event_id[groups$other],
    c("IDN_COVID_2020_2022", "GBR_COVID_LOCKDOWNS_2020")
  )
  expect_equal(events$event_id[groups$years], "YEAR_2010")
})

test_that("sortEventsForCountry lists composite events before other primary events", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "RUS_OLD", "Older Russian event", "politics", "national", 1990L, 1991L, 1990L, "manual", FALSE,
    "MERGE_RUS_FX", "FX episodes in Russia", "economy", "national", 1998L, 2014L, 2014L, "composite", FALSE,
    "RUS_NEW", "Recent Russian event", "politics", "national", 2014L, 2015L, 2014L, "manual", FALSE
  )
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "RUS_OLD", "RUS", "affected",
    "MERGE_RUS_FX", "RUS", "affected",
    "RUS_NEW", "RUS", "affected"
  )

  sorted <- sortEventsForCountry(events, "RUS", event_countries)
  expect_equal(sorted$event_id, c("MERGE_RUS_FX", "RUS_NEW", "RUS_OLD"))
})

test_that("assertNoDuplicateEventPickerOptions passes for disambiguated COVID-like labels", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "USA_COVID_PANDEMIC", "COVID-19 pandemic", "health", "national", 2020L, 2021L, 2020L, "manual", FALSE,
    "IDN_COVID_2020_2022", "COVID-19 pandemic", "health", "national", 2020L, 2022L, 2021L, "manual", TRUE,
    "PAK_COVID_2020_2021", "COVID-19 pandemic", "health", "national", 2020L, 2021L, 2020L, "manual", TRUE,
    "GLB_COVID_19_PANDEMIC", "COVID-19 pandemic", "health", "global", 2020L, 2022L, 2021L, "manual", TRUE,
    "GBR_COVID_LOCKDOWNS_2020", "COVID-19 lockdowns", "health", "global", 2020L, 2020L, 2020L, "manual", TRUE,
    "FRA_COVID_LOCKDOWNS_2020", "COVID-19 lockdowns", "health", "global", 2020L, 2020L, 2020L, "manual", TRUE
  )
  event_countries <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "USA_COVID_PANDEMIC", "USA", "affected"
  )

  expect_true(assertNoDuplicateEventPickerOptions(events, "USA", event_countries))
})

test_that("loaded picker catalog has no duplicate labels within dropdown groups", {
  skip_if_not(file.exists(eventsDataPath()))

  data_dir <- dirname(eventsDataPath())
  universe <- loadEventsUniverse(manual_path = eventsDataPath(), data_dir = data_dir)
  population_paths <- resolvePopulationPaths()
  skip_if(length(population_paths) == 0, "population data not available")

  population <- loadPopulationData(population_paths)
  year_bounds <- populationYearBounds(population)
  events <- appendYearMarkerEvents(universe$events, year_bounds)
  picker <- eventsForEventPicker(events)
  ec_path <- eventCountriesDeployPath(data_dir)
  skip_if_not(file.exists(ec_path), "event_countries deploy file not available")
  event_countries <- loadEventCountriesUniverse(manual_path = ec_path, data_dir = data_dir)

  for (country_id in c("USA", "RUS", "ESP")) {
    compatible <- filterCompatibleEvents(picker, country_id, event_countries)
    expect_true(assertNoDuplicateEventPickerOptions(compatible, country_id, event_countries))
  }
})

test_that("assessRecipe accepts year marker events for any country", {
  events <- buildTestEvents() |>
    dplyr::bind_rows(buildYearMarkerEvents(2010L, 2010L))
  events <- normalizeEventsCatalog(events)
  recipe <- buildTestRecipe(country_id = "RUS", event_id = "YEAR_2010")

  assessment <- assessRecipe(
    recipe,
    countries = buildTestCountries(),
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = buildTestEventCountries()
  )
  expect_true(assessment$valid)
})

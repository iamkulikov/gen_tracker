test_that("buildCompositeEventsFromTags groups per country with at least two members", {
  events <- buildCompositeTestElementaryEvents()
  tags <- tibble::tribble(
    ~event_id, ~tag,
    "CMP_FX_RUS_1998", "major_fx_depreciation",
    "CMP_FX_RUS_2008", "major_fx_depreciation",
    "CMP_FX_RUS_2014", "major_fx_depreciation"
  )
  links <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "CMP_FX_RUS_1998", "RUS", "affected",
    "CMP_FX_RUS_2008", "RUS", "affected",
    "CMP_FX_RUS_2014", "RUS", "affected"
  )

  built <- buildCompositeEventsFromTags(
    events = events,
    event_tags = tags,
    classifier_spec = 'tag == "major_fx_depreciation"',
    event_countries = links,
    countries = buildTestCountries()
  )

  expect_equal(nrow(built$composite_events), 1)
  expect_equal(built$composite_events$composite_event_id, "MERGE_MAJOR_FX_DEPRECIATION_RUS")
  expect_equal(nrow(built$composite_members), 3)
  expect_equal(
    sort(built$composite_members$member_event_id),
    sort(events$event_id)
  )
})

test_that("buildCompositeEventsFromTags uses criterion label and country name", {
  events <- buildCompositeTestElementaryEvents()
  tags <- tibble::tribble(
    ~event_id, ~tag,
    "CMP_FX_RUS_1998", "major_fx_depreciation",
    "CMP_FX_RUS_2008", "major_fx_depreciation",
    "CMP_FX_RUS_2014", "major_fx_depreciation"
  )
  criteria <- tibble::tribble(
    ~criterion_id, ~default_tags, ~name_template,
    "FX_CRISIS_70", "major_fx_depreciation",
    "Currency crisis (FX weakening >70%) in {country_id} ({year})"
  )

  built <- buildCompositeEventsFromTags(
    events = events,
    event_tags = tags,
    classifier_spec = 'tag == "major_fx_depreciation"',
    countries = buildTestCountries(),
    criteria = criteria
  )

  expect_equal(
    built$composite_events$event_name[[1]],
    "Currency crisis (FX weakening >70%) in Russia (3 episodes)"
  )
})

test_that("resolveEventEpisodes expands composite into member episodes", {
  events <- buildCompositeTestEventsUniverse()
  composite <- events |> dplyr::filter(.data$event_id == "MERGE_MAJOR_FX_DEPRECIATION_RUS")
  members <- buildCompositeTestMembers()

  episodes <- resolveEventEpisodes(
    composite,
    "start",
    composite_members = members,
    events = events
  )

  expect_equal(nrow(episodes), 3)
  expect_equal(sort(episodes$threshold), c(1998L, 2008L, 2014L))
})

test_that("calculateStratumSeries composite experienced_any is zero before first episode", {
  population <- buildSemanticsPopulation(years = 1990:2020)
  events <- buildCompositeTestEventsUniverse()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  members <- buildCompositeTestMembers()

  recipe <- buildTestRecipe(
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "adults",
    event_mode = "start",
    metric = "count"
  )

  out <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = buildCompositeTestEventCountries(),
    composite_members = members
  )

  expect_true(all(out$value[out$year < 1998L] == 0))
  expect_gt(out$value[out$year == 1998L], 0)
  expect_gt(out$value[out$year == 2008L], out$value[out$year == 2007L])
})

test_that("resolveBirthYearThresholds uses earliest witness year per birth year", {
  events <- buildCompositeTestEventsUniverse()
  composite <- events |> dplyr::filter(.data$event_id == "MERGE_MAJOR_FX_DEPRECIATION_RUS")
  members <- buildCompositeTestMembers()
  recipe <- buildTestRecipe(
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    age_status_id = "custom",
    custom_age_min = 18L,
    custom_age_max = 60L,
    event_mode = "start"
  )
  age_range <- resolveAgeRange(recipe, defaultAgeGroups())
  thresholds <- resolveBirthYearThresholds(
    recipe = recipe,
    event = composite,
    age_range = age_range,
    population_years = 1990:2020,
    population_ages = 0:100,
    composite_members = members,
    events = events
  )
  birth_year <- 1980L
  expect_true(birth_year %in% thresholds$birth_year)
  expect_equal(thresholds$threshold[thresholds$birth_year == birth_year], 1998L)
})

test_that("assessRecipe rejects composite country mismatch", {
  events <- buildCompositeTestEventsUniverse()
  recipe <- buildTestRecipe(
    country_id = "USA",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS"
  )
  assessment <- assessRecipe(
    recipe,
    countries = buildTestCountries(),
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = buildCompositeTestEventCountries()
  )
  expect_false(assessment$valid)
  expect_match(paste(assessment$errors, collapse = " "), "Composite event")
})

test_that("buildPlotEventMarkers emits one row per composite episode", {
  events <- buildCompositeTestEventsUniverse()
  members <- buildCompositeTestMembers()
  plot_data <- tibble::tibble(
    query_id = "q1",
    event_id = "MERGE_MAJOR_FX_DEPRECIATION_RUS",
    event_name = "Major FX episodes in RUS",
    event_mode = "start"
  )

  markers <- buildPlotEventMarkers(plot_data, events, composite_members = members)
  expect_equal(nrow(markers), 3)
  expect_equal(sum(markers$show_label), 1L)
  expect_equal(sort(markers$start_year), c(1998L, 2008L, 2014L))
})

test_that("single elementary event resolveEventEpisodes returns one row", {
  events <- buildTestEvents()
  event <- events |> dplyr::slice(1)
  episodes <- resolveEventEpisodes(event, "start", composite_members = NULL, events = events)
  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$threshold, 1979L)
})

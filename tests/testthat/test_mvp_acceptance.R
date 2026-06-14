mvpSmokeScenario <- function(
  label,
  recipes,
  population = buildTestPopulation(),
  events = dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent()),
  countries = buildTestCountries(),
  age_groups = defaultAgeGroups(),
  event_countries = buildTestEventCountries(),
  year_range = NULL,
  show_projection = TRUE,
  metric = NULL
) {
  recipes_tbl <- tibble::as_tibble(recipes)
  if (is.null(metric)) {
    metric <- recipes_tbl$metric[[1]]
  }
  if (is.null(year_range)) {
    year_range <- range(population$year)
  }

  for (i in seq_len(nrow(recipes_tbl))) {
    recipe <- as.list(recipes_tbl[i, ])
    assessment <- assessRecipe(
      recipe = recipe,
      countries = countries,
      events = events,
      age_groups = age_groups,
      event_countries = event_countries
    )
    expect_true(assessment$valid, label = sprintf(
      "%s: recipe %s invalid: %s",
      label,
      recipe$query_id,
      paste(assessment$errors, collapse = "; ")
    ))
  }

  plot_data <- buildPlotData(
    recipes = recipes_tbl,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries
  )
  plot_data <- applyPlotViewFilters(
    plot_data = plot_data,
    year_range = year_range,
    show_projection = show_projection
  )

  expect_gt(nrow(plot_data), 0)
  expect_true(all(plot_data$value >= 0))
  if (metric == "share_total_population") {
    expect_true(all(plot_data$value <= 1))
  }

  subtitle <- buildPlotViewSubtitle(metric, year_range, show_projection)
  plot_obj <- buildTrackerPlot(plot_data, events = events, metric = metric, subtitle = subtitle)
  expect_s3_class(plot_obj, "ggplot")

  export_tables <- buildExportMetadataTables(
    plot_data = plot_data,
    recipes = recipes_tbl,
    events = events,
    countries = countries,
    age_groups = age_groups,
    view_state = list(metric = metric, year_range = year_range, show_projection = show_projection),
    population = population,
    event_countries = event_countries
  )
  expect_equal(nrow(export_tables$queries), nrow(recipes_tbl))

  invisible(plot_data)
}

test_that("MVP smoke: single national query with export metadata", {
  recipes <- tibble::as_tibble(buildTestRecipe())
  mvpSmokeScenario("single national", recipes, show_projection = FALSE, year_range = c(1979L, 1982L))
})

test_that("MVP smoke: compare male and female cohorts", {
  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )
  mvpSmokeScenario("male vs female", recipes)
})

test_that("MVP smoke: global event on linked country", {
  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "all", "youth", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "share_total_population"
  )
  mvpSmokeScenario("global event", recipes, metric = "share_total_population")
})

test_that("MVP smoke: four-line chart within product limit", {
  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q3", "RUS", "all", "alive", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "period", "count",
    "q4", "RUS", "all", "youth", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "share_total_population"
  )
  plot_data <- mvpSmokeScenario("four lines", recipes)
  expect_equal(length(unique(plot_data$query_id)), 4)
})

test_that("MVP smoke: incompatible country-event pair fails before plot build", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(country_id = "USA")

  expect_error(
    buildPlotData(
      recipes = tibble::as_tibble(recipe),
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      event_countries = buildTestEventCountries()
    ),
    "not linked"
  )
})

test_that("MVP definition checklist: reproducible recipe encoding", {
  recipe <- buildTestRecipe(
    sex = "female",
    age_status_id = "alive",
    event_mode = "period",
    metric = "share_total_population"
  )
  code <- encodeRecipe(recipe)
  decoded <- decodeRecipe(code)
  expect_equal(decoded$country_id, recipe$country_id)
  expect_equal(decoded$metric, recipe$metric)
  expect_match(code, "^GEN2:")
})

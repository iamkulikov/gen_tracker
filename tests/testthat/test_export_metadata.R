test_that("buildChartNarrative stays human-readable without internal ids", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  event <- events |> dplyr::filter(.data$event_id == recipe$event_id) |> dplyr::slice(1)

  narrative <- buildChartNarrative(
    recipe = recipe,
    event = event,
    countries = countries,
    events = events,
    age_groups = age_groups,
    population = population
  )

  expect_match(narrative, "Men in Russia")
  expect_match(narrative, "War in Afghanistan")
  expect_match(narrative, "roughly 1962")
  expect_false(grepl("age_status_id", narrative, fixed = TRUE))
  expect_false(grepl("is_complement", narrative, fixed = TRUE))
  expect_false(grepl("Birth years included:", narrative, fixed = TRUE))
})

test_that("buildRecipeDescription includes birth years and methodology", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  event <- events |> dplyr::filter(.data$event_id == recipe$event_id) |> dplyr::slice(1)
  country_name <- "Russia"
  age_label <- "School age"

  description <- buildRecipeDescription(
    recipe = recipe,
    event = event,
    country_name = country_name,
    age_label = age_label,
    countries = countries,
    events = events,
    age_groups = age_groups,
    population = population
  )

  expect_match(description, "Men in Russia")
  expect_match(description, "Birth years included:")
  expect_match(description, "1962")
  expect_match(description, "1973")
  expect_match(description, "at the beginning of")
  expect_match(description, "People, thousands")
})

test_that("applyPlotViewFilters mirrors app year range and projection toggle", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  raw <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  filtered <- applyPlotViewFilters(
    plot_data = raw,
    year_range = c(1980L, 1984L),
    show_projection = FALSE
  )

  expect_true(all(filtered$year >= 1980L & filtered$year <= 1984L))
  expect_false(any(filtered$is_projection))
  expect_lt(nrow(filtered), nrow(raw))
})

test_that("buildPlotViewSubtitle reflects projection toggle", {
  subtitle_on <- buildPlotViewSubtitle(
    metric = "share_total_population",
    year_range = c(1979L, 1985L),
    show_projection = TRUE
  )
  subtitle_off <- buildPlotViewSubtitle(
    metric = "count",
    year_range = c(1979L, 1985L),
    show_projection = FALSE
  )

  expect_null(subtitle_on)
  expect_null(subtitle_off)
})

test_that("buildExportMetadataTables captures view state and source metadata", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )
  plot_data <- applyPlotViewFilters(
    plot_data = plot_data,
    year_range = c(1979L, 1983L),
    show_projection = FALSE
  )

  tables <- buildExportMetadataTables(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups,
    view_state = list(
      metric = "count",
      year_range = c(1979L, 1983L),
      show_projection = FALSE
    ),
    population = population,
    population_paths = "data/population.rds",
    events_path = "data/events.csv",
    countries_path = "data/countries.csv",
    event_countries_path = "data/event_countries.csv",
    event_countries = buildTestEventCountries(),
    event_countries_loaded = TRUE,
    event_countries_rows = 4L
  )

  expect_true("app_version" %in% tables$metadata$field)
  expect_equal(
    tables$metadata$value[tables$metadata$field == "year_min"],
    "1979"
  )
  expect_equal(
    tables$metadata$value[tables$metadata$field == "show_projection"],
    "FALSE"
  )
  expect_false("population_file" %in% tables$metadata$field)
  expect_true("methodology_notes" %in% tables$metadata$field)
  methodology <- tables$metadata$value[tables$metadata$field == "methodology_notes"]
  expect_match(methodology, "Demographic calculations use current-country")
  expect_match(methodology, "Reliability scoring is not yet implemented.")
  expect_match(methodology, "Demographic source:")
  expect_false(grepl("population.rds", methodology, fixed = TRUE))
  expect_equal(nrow(tables$query_descriptions), 1)
  expect_match(tables$query_descriptions$query_description[[1]], "Birth years included:")
  expect_equal(nrow(tables$queries), 1)
  expect_equal(
    names(tables$queries),
    c(
      "query_id", "line_label", "recipe_code", "metric",
      "event_name", "event_type", "event_scope", "event_years", "peak_year",
      "event_family", "short_description", "source_url", "selection_channel",
      "include_in_core_catalogue", "curation_scores",
      "reliability_summary"
    )
  )
  expect_equal(tables$queries$event_name[[1]], "War in Afghanistan")
})

test_that("buildExportQueriesSheet joins query fields with event metadata", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )
  qd <- buildExportQueryDescriptions(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups
  )
  events_used <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::mutate(
      event_years = purrr::map2_chr(.data$start_year, .data$end_year, formatEventYearRange)
    )

  queries <- buildExportQueriesSheet(
    recipes = tibble::as_tibble(recipe),
    query_descriptions = qd,
    events = events_used
  )

  expect_equal(queries$metric[[1]], "count")
  expect_equal(queries$event_scope[[1]], "national")
  expect_true(nzchar(queries$event_years[[1]]))
  expect_match(queries$reliability_summary[[1]], "Reliability:")
})

test_that("buildExportQueriesSheet puts reliability summary last for transpose", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries(),
    migration = buildTestMigrationPrepared(rus_rate = 0)
  )
  qd <- buildExportQueryDescriptions(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups,
    migration = buildTestMigrationPrepared(rus_rate = 0)
  )
  events_used <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::mutate(
      event_years = purrr::map2_chr(.data$start_year, .data$end_year, formatEventYearRange)
    )

  queries <- buildExportQueriesSheet(
    recipes = tibble::as_tibble(recipe),
    query_descriptions = qd,
    events = events_used
  )
  wide <- transposeExportQueriesSheet(queries)

  expect_equal(tail(names(queries), 1), "reliability_summary")
  expect_equal(wide$field[[nrow(wide)]], "reliability_summary")
  expect_match(wide$q1[wide$field == "reliability_summary"], "Reliability:")
  expect_match(wide$q1[wide$field == "reliability_summary"], "Higher scores mean")
})

test_that("transposeExportQueriesSheet puts attributes in rows and queries in columns", {
  queries <- tibble::tribble(
    ~query_id, ~line_label, ~recipe_code, ~metric,
    ~event_name, ~event_type, ~event_scope, ~event_years, ~peak_year, ~reliability_summary,
    "q1", "Line one", "GEN2:abc", "count",
    "War", "war", "national", "1979-1989", 1984L, "Reliability: 100/100.",
    "q2", "Line two", "GEN2:def", "count",
    "War", "war", "national", "1979-1989", 1984L, "Reliability: 94/100."
  )

  wide <- transposeExportQueriesSheet(queries)
  expect_equal(wide$field[[1]], "line_label")
  expect_equal(wide$field, setdiff(names(queries), "query_id"))
  expect_equal(wide$q1[wide$field == "line_label"], "Line one")
  expect_equal(wide$q2[wide$field == "recipe_code"], "GEN2:def")
})

test_that("buildExportDataProjectionCells marks projection year and series pairs", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )
  wide <- buildExportDataWide(plot_data)
  cells <- buildExportDataProjectionCells(plot_data, wide)

  expect_gt(nrow(cells), 0)
  proj_years <- plot_data$year[plot_data$is_projection]
  expect_true(all(wide$year[cells$row - 1L] %in% proj_years))
})

test_that("buildExportDataWide pivots one column per query by year", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )

  plot_data <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  wide <- buildExportDataWide(plot_data)
  expect_equal(nrow(wide), dplyr::n_distinct(plot_data$year))
  expect_true("year" %in% names(wide))
  expect_equal(ncol(wide), 1L + dplyr::n_distinct(plot_data$query_id))
  expect_true(all(grepl("^q[12]", setdiff(names(wide), "year"))))
})

test_that("exportXlsx writes three sheets: data, metadata, queries", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  view_state <- list(
    metric = "count",
    year_range = range(plot_data$year),
    show_projection = TRUE
  )
  export_tables <- buildExportMetadataTables(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups,
    view_state = view_state,
    population = population
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  exportXlsx(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    file_path = tmp,
    export_tables = export_tables
  )

  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
  sheets <- readxl::excel_sheets(tmp)
  expect_equal(sheets, c("data", "metadata", "queries"))

  data_tbl <- readxl::read_excel(tmp, sheet = "data")
  expect_equal(nrow(data_tbl), dplyr::n_distinct(plot_data$year))
  expect_true(any(grepl("^q1", names(data_tbl))))

  meta_tbl <- readxl::read_excel(tmp, sheet = "metadata")
  expect_true("methodology_notes" %in% meta_tbl$field)
  methodology <- meta_tbl$value[meta_tbl$field == "methodology_notes"][[1]]
  expect_match(methodology, "Migration and historical border changes")

  queries_tbl <- readxl::read_excel(tmp, sheet = "queries")
  expect_equal(names(queries_tbl)[1], "field")
  expect_true("q1" %in% names(queries_tbl))
  expect_equal(queries_tbl$q1[queries_tbl$field == "event_name"][[1]], "War in Afghanistan")
  expect_equal(queries_tbl$q1[queries_tbl$field == "metric"][[1]], "count")
})

test_that("exported plot subtitle matches view state used for chart data", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )

  raw <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )
  view_state <- list(
    metric = "count",
    year_range = c(1980L, 1984L),
    show_projection = FALSE
  )
  plot_data <- applyPlotViewFilters(
    plot_data = raw,
    year_range = view_state$year_range,
    show_projection = view_state$show_projection
  )
  subtitle <- buildPlotViewSubtitle(
    metric = view_state$metric,
    year_range = view_state$year_range,
    show_projection = view_state$show_projection
  )
  plot_obj <- buildTrackerPlot(
    plot_data = plot_data,
    events = events,
    metric = view_state$metric,
    subtitle = subtitle
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_null(subtitle)
  expect_null(plot_obj$labels$subtitle)
  expect_false(any(plot_data$year < 1980L | plot_data$year > 1984L))
})

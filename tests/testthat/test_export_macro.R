test_that("loadCountryDictionary reads iso2 from countries.csv", {
  data_dir <- testthat::test_path("..", "..", "data")
  countries_path <- countriesDataPath(data_dir = data_dir)
  skip_if_not(file.exists(countries_path), "local 0_countries.csv not available")

  countries <- loadCountryDictionary(countries_path)
  expect_true("iso2" %in% names(countries))
  expect_true(all(!is.na(countries$iso2)))
  expect_equal(countries$iso2[countries$country_id == "RUS"], "RU")
  expect_equal(countries$iso2[countries$country_id == "USA"], "US")
})

test_that("buildMacroPlotData uses iso2 in country_id for all countries", {
  population <- tidyr::crossing(
    country_id = c("RUS", "USA"),
    country_name = c("Russia", "United States"),
    year = 1979:1982,
    age = 0:30,
    sex = c("male", "female")
  ) |>
    dplyr::mutate(
      population = 1000,
      data_type = "estimate",
      scenario = "baseline",
      source = "test",
      source_version = "v1"
    )

  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(country_id = "RUS", metric = "count")
  plot_context <- buildPlotCalculationContext(population)

  macro_data <- buildMacroPlotData(
    template_recipe = recipe,
    countries = countries,
    population = population,
    events = events,
    age_groups = age_groups,
    plot_context = plot_context,
    year_range = c(1979L, 1982L),
    show_projection = TRUE
  )

  expect_equal(sort(unique(macro_data$country_id)), c("RU", "US"))
  expect_true(all(c("country_id", "year", "value", "line_label", "query_id") %in% names(macro_data)))
  expect_false(grepl("Russia", macro_data$line_label[[1]], fixed = TRUE))
  expect_match(macro_data$line_label[[1]], "Men who were")
  expect_true(all(macro_data$value >= 0))
  expect_equal(nrow(macro_data), length(unique(macro_data$country_id)) * 4L)
})

test_that("buildMacroExportDataSheet matches single-export column naming without country", {
  macro_data <- tibble::tibble(
    country_id = c("RU", "US"),
    year = c(1979L, 1979L),
    value = c(100, 200),
    is_projection = FALSE,
    query_id = "q1",
    line_label = "Men who were School age at the beginning of War in Afghanistan."
  )

  sheet <- buildMacroExportDataSheet(macro_data)
  expect_equal(names(sheet), c(
    "country_id",
    "year",
    "q1 - Men who were School age at the beginning of War in Afghanistan."
  ))
  expect_equal(sheet$country_id, c("RU", "US"))
})

test_that("buildMacroPlotDataFromRecipes exports all valid chart queries", {
  population <- tidyr::crossing(
    country_id = c("RUS", "USA"),
    country_name = c("Russia", "United States"),
    year = 1979:1980,
    age = 0:30,
    sex = c("male", "female")
  ) |>
    dplyr::mutate(
      population = 1000,
      data_type = "estimate",
      scenario = "baseline",
      source = "test",
      source_version = "v1"
    )

  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  plot_context <- buildPlotCalculationContext(population)

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~age_modifier, ~is_complement,
    ~custom_age_min, ~custom_age_max, ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", "none", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "RUS", "female", "school_age", "none", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count"
  )

  macro_data <- buildMacroPlotDataFromRecipes(
    template_recipes = recipes,
    countries = countries,
    population = population,
    events = events,
    age_groups = age_groups,
    plot_context = plot_context,
    year_range = c(1979L, 1980L),
    show_projection = TRUE
  )

  expect_equal(sort(unique(macro_data$query_id)), c("q1", "q2"))
  sheet <- buildMacroExportDataSheet(macro_data)
  expect_equal(ncol(sheet), 4L)
  expect_true(any(grepl("^q1 - ", names(sheet))))
  expect_true(any(grepl("^q2 - ", names(sheet))))
})

test_that("exportMacroLongTable writes data metadata queries sheets like single export", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()
  plot_context <- buildPlotCalculationContext(population)

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  exportMacroLongTable(
    file_path = tmp,
    template_recipes = tibble::as_tibble(recipe),
    countries = countries,
    population = population,
    events = events,
    age_groups = age_groups,
    plot_context = plot_context,
    year_range = c(1979L, 1982L),
    show_projection = TRUE,
    population_paths = "data/population.rds"
  )

  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  sheets <- openxlsx::getSheetNames(tmp)
  expect_equal(sheets, c("data", "metadata", "queries"))

  data <- readxl::read_excel(tmp, sheet = "data")
  expect_true("country_id" %in% names(data))
  expect_true("year" %in% names(data))
  expect_equal(data$country_id[[1]], "RU")
  expect_true(any(grepl("^q1 - Men who were", names(data))))

  metadata <- readxl::read_excel(tmp, sheet = "metadata")
  expect_equal(metadata$field[metadata$field == "export_scope"], "export_scope")
  expect_equal(metadata$value[metadata$field == "export_scope"], "all_countries")
  methodology <- metadata$value[metadata$field == "methodology_notes"]
  expect_match(methodology, "All-countries export")
  expect_match(methodology, "Country-event catalogue links are not applied")

  queries <- readxl::read_excel(tmp, sheet = "queries")
  expect_false("reliability_summary" %in% queries$field)
  line_label <- queries$q1[queries$field == "line_label"][[1]]
  expect_match(line_label, "Men who were")
  expect_match(line_label, "War in Afghanistan")
  expect_false(grepl("Russia", line_label, fixed = TRUE))
})

if (!exists("defaultAgeGroups", mode = "function")) {
  root_candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  project_root_path <- file.path(root_candidate, "R", "project_root.R")
  while (!file.exists(project_root_path) && dirname(root_candidate) != root_candidate) {
    root_candidate <- dirname(root_candidate)
    project_root_path <- file.path(root_candidate, "R", "project_root.R")
  }
  source(project_root_path)
  loadProjectSources(root_candidate)
}

buildTestPopulation <- function() {
  tidyr::crossing(
    country_id = c("RUS"),
    country_name = c("Russia"),
    year = 1979:1985,
    age = 0:100,
    sex = c("male", "female")
  ) |>
    dplyr::mutate(
      population = 1000,
      data_type = dplyr::if_else(year <= 1982, "estimate", "projection"),
      scenario = "baseline",
      source = "test",
      source_version = "v1"
    )
}

buildTestEvents <- function() {
  tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "AFG_WAR", "War in Afghanistan", "war", "national", 1979L, 1989L, 1984L, "manual", FALSE,
    "USA_NATIONAL_EVENT", "USA national event", "politics", "national", 2001L, 2001L, 2001L, "manual", FALSE,
    "MULTI_BORDER_WAR", "Multi-country border war", "war", "multi_country", 1990L, 1991L, 1990L, "manual", FALSE
  )
}

buildTestGlobalEvent <- function() {
  tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "GLOBAL_CRISIS", "Global crisis", "economy", "global", 2000L, 2001L, 2000L, "manual", TRUE
  )
}

buildTestCountries <- function() {
  tibble::tribble(
    ~country_id, ~country_name, ~iso3, ~iso2, ~boundary_warning,
    "RUS", "Russia", "RUS", "RU", "",
    "USA", "United States", "USA", "US", ""
  )
}

buildTestEventCountries <- function() {
  tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "affected",
    "USA_NATIONAL_EVENT", "USA", "affected",
    "MULTI_BORDER_WAR", "USA", "affected",
    "MULTI_BORDER_WAR", "MEX", "affected"
  )
}

buildTestEventCountriesFixturePath <- function() {
  testthat::test_path("data", "event_countries_fixture.csv")
}

buildTestRecipe <- function(
  query_id = "q1",
  country_id = "RUS",
  sex = "male",
  age_status_id = "school_age",
  age_modifier = "none",
  is_complement = FALSE,
  custom_age_min = NA_integer_,
  custom_age_max = NA_integer_,
  event_id = "AFG_WAR",
  event_mode = "start",
  metric = "count"
) {
  list(
    query_id = query_id,
    country_id = country_id,
    sex = sex,
    age_status_id = age_status_id,
    age_modifier = age_modifier,
    is_complement = is_complement,
    custom_age_min = custom_age_min,
    custom_age_max = custom_age_max,
    event_id = event_id,
    event_mode = event_mode,
    metric = metric
  )
}

buildBenchmarkPopulation <- function(
  country_ids = paste0("C", sprintf("%02d", 1:20)),
  years = 1970:2025,
  ages = 0:100
) {
  tidyr::crossing(
    country_id = country_ids,
    year = years,
    age = ages,
    sex = c("male", "female")
  ) |>
    dplyr::mutate(
      country_name = .data$country_id,
      population = 1000L,
      data_type = dplyr::if_else(.data$year <= 2020, "estimate", "projection"),
      scenario = "baseline",
      source = "benchmark",
      source_version = "v1"
    )
}

buildBenchmarkCountries <- function(country_ids = paste0("C", sprintf("%02d", 1:20))) {
  tibble::tibble(
    country_id = country_ids,
    country_name = country_ids,
    iso3 = country_ids,
    boundary_warning = ""
  )
}

buildBenchmarkPlotRecipes <- function(country_ids = paste0("C", sprintf("%02d", 1:4))) {
  tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~age_modifier, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", country_ids[[1]], "male", "school_age", "none", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "count",
    "q2", country_ids[[2]], "female", "alive", "none", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "count",
    "q3", country_ids[[3]], "all", "not_born_yet", "none", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "period", "share_total_population",
    "q4", country_ids[[4]], "all", "youth", "none", FALSE, NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "share_total_population"
  )
}

buildGrowthSuspicionPopulation <- function(years = 1979:1985, ages = 0:100) {
  base <- buildSemanticsPopulation(years = years, ages = ages)
  base |>
    dplyr::mutate(
      population = .data$population * (1 + 0.05 * (.data$year - 1979L))
    )
}

buildSemanticsPopulation <- function(years = 1970:1990, ages = 0:100) {
  tidyr::crossing(
    country_id = "RUS",
    country_name = "Russia",
    year = years,
    age = ages,
    sex = c("male", "female")
  ) |>
    dplyr::mutate(
      population = 1,
      data_type = "estimate",
      scenario = "baseline",
      source = "semantics_fixture",
      source_version = "v1"
    )
}

buildSemanticsEvent <- function(start_year = 1979L, end_year = 1989L, peak_year = 1984L) {
  tibble::tibble(
    event_id = "AFG_WAR",
    event_name = "War in Afghanistan",
    event_type = "war",
    event_scope = "national",
    start_year = start_year,
    end_year = end_year,
    peak_year = peak_year,
    event_origin = "manual",
    cross_country_allowed = FALSE
  )
}

buildFixtureIndicators <- function() {
  dir <- testthat::test_path("data", "indicators")
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  stats::setNames(
    lapply(files, loadIndicatorFile),
    tools::file_path_sans_ext(basename(files))
  )
}

buildFixtureComputedCriteria <- function() {
  loadEventCriteria(testthat::test_path("data", "event_criteria_fixture.csv"))
}

buildCompositeTestElementaryEvents <- function() {
  tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "CMP_FX_RUS_1998", "FX 1998", "economy", "national", 1998L, 1998L, 1998L, "computed", FALSE,
    "CMP_FX_RUS_2008", "FX 2008", "economy", "national", 2008L, 2008L, 2008L, "computed", FALSE,
    "CMP_FX_RUS_2014", "FX 2014", "economy", "national", 2014L, 2014L, 2014L, "computed", FALSE
  )
}

buildCompositeTestMembers <- function() {
  tibble::tribble(
    ~composite_event_id, ~member_event_id,
    "MERGE_MAJOR_FX_DEPRECIATION_RUS", "CMP_FX_RUS_1998",
    "MERGE_MAJOR_FX_DEPRECIATION_RUS", "CMP_FX_RUS_2008",
    "MERGE_MAJOR_FX_DEPRECIATION_RUS", "CMP_FX_RUS_2014"
  )
}

buildCompositeTestEvents <- function() {
  compositeEventsAsCatalog(tibble::tribble(
    ~composite_event_id, ~event_name, ~event_type, ~event_scope, ~country_id,
    ~default_event_mode, ~classifier_spec, ~description, ~event_origin,
    ~start_year, ~end_year, ~peak_year,
    "MERGE_MAJOR_FX_DEPRECIATION_RUS", "Major FX episodes in RUS", "economy", "national", "RUS",
    "start", 'tag == "major_fx_depreciation"', "test composite", "composite",
    1998L, 2014L, 2014L
  ))
}

buildCompositeTestEventCountries <- function() {
  dplyr::bind_rows(
    buildTestEventCountries(),
    tibble::tribble(
      ~event_id, ~country_id, ~country_role,
      "CMP_FX_RUS_1998", "RUS", "affected",
      "CMP_FX_RUS_2008", "RUS", "affected",
      "CMP_FX_RUS_2014", "RUS", "affected",
      "MERGE_MAJOR_FX_DEPRECIATION_RUS", "RUS", "affected"
    )
  )
}

buildCompositeTestEventsUniverse <- function() {
  dplyr::bind_rows(buildCompositeTestElementaryEvents(), buildCompositeTestEvents())
}

buildTestMigration <- function(
  years = 1979:1985,
  rus_rate = 0,
  usa_rate = 0
) {
  dplyr::bind_rows(
    tibble::tibble(
      country_id = "RUS",
      year = as.integer(years),
      net_migration_rate = rus_rate,
      net_migration = rus_rate * 100,
      data_type = "estimate",
      source = "UN WPP",
      source_version = "2024"
    ),
    tibble::tibble(
      country_id = "USA",
      year = as.integer(years),
      net_migration_rate = usa_rate,
      net_migration = usa_rate * 100,
      data_type = "estimate",
      source = "UN WPP",
      source_version = "2024"
    )
  )
}

buildTestMigrationPrepared <- function(
  years = 1979:1985,
  rus_rate = 0,
  usa_rate = 0
) {
  migration <- buildTestMigration(years = years, rus_rate = rus_rate, usa_rate = usa_rate)
  list(
    schema_version = "1",
    migration = migration,
    country_features = computeMigrationCountryFeatures(migration)
  )
}

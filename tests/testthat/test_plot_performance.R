plotPerformanceSecondsLimit <- function() {
  env_limit <- suppressWarnings(as.numeric(Sys.getenv("GEN_TRACKER_PLOT_PERF_SECONDS", unset = "")))
  if (!is.na(env_limit) && env_limit > 0) {
    return(env_limit)
  }
  2
}

test_that("plot_context matches uncached stratum calculation", {
  population <- buildBenchmarkPopulation(country_ids = c("RUS", "USA"))
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()
  recipe <- buildTestRecipe(
    country_id = "RUS",
    event_id = "GLOBAL_CRISIS",
    metric = "share_total_population"
  )

  direct <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries,
    plot_context = NULL
  )
  plot_context <- buildPlotCalculationContext(population)
  cached <- calculateStratumSeries(
    recipe,
    population,
    events,
    age_groups,
    countries,
    event_countries = event_countries,
    plot_context = plot_context
  )

  expect_equal(direct, cached)
})

test_that("buildPlotData with shared plot_context matches default path", {
  population <- buildBenchmarkPopulation(country_ids = c("RUS", "USA", "C01", "C02"))
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  countries <- dplyr::bind_rows(
    buildTestCountries(),
    buildBenchmarkCountries(country_ids = c("C01", "C02"))
  )
  age_groups <- defaultAgeGroups()
  event_countries <- buildTestEventCountries()
  recipes <- buildBenchmarkPlotRecipes(country_ids = c("RUS", "USA", "C01", "C02"))

  default_out <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries
  )
  plot_context <- buildPlotCalculationContext(population)
  cached_out <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = event_countries,
    plot_context = plot_context
  )

  expect_equal(default_out, cached_out)
})

test_that("buildPlotData completes four-query benchmark within time limit", {
  population <- buildBenchmarkPopulation()
  events <- buildTestGlobalEvent()
  countries <- buildBenchmarkCountries()
  age_groups <- defaultAgeGroups()
  recipes <- buildBenchmarkPlotRecipes()
  plot_context <- buildPlotCalculationContext(population)
  seconds_limit <- plotPerformanceSecondsLimit()

  elapsed <- system.time({
    out <- buildPlotData(
      recipes = recipes,
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      event_countries = tibble::tibble(
        event_id = character(),
        country_id = character(),
        country_role = character()
      ),
      plot_context = plot_context
    )
  })["elapsed"]

  expect_true(nrow(out) > 0)
  expect_equal(sort(unique(out$query_id)), c("q1", "q2", "q3", "q4"))
  expect_true(
    elapsed < seconds_limit,
    info = sprintf(
      paste(
        "buildPlotData(4 queries) took %.3f s (limit %.3f s).",
        "Set GEN_TRACKER_PLOT_PERF_SECONDS to override on slow CI."
      ),
      elapsed,
      seconds_limit
    )
  )
})

test_that("real population buildPlotData stays within limit when RDS is available", {
  rds_path <- Sys.getenv("GEN_TRACKER_PREPARED_POPULATION_PATH", unset = "")
  if (!nzchar(rds_path)) {
    rds_path <- file.path("data", "population.rds")
  }
  if (!file.exists(rds_path)) {
    skip("Prepared population RDS not available for runtime benchmark.")
  }

  population <- readRDS(rds_path)
  country_ids <- unique(population$country_id)
  if (length(country_ids) < 4L) {
    skip("Prepared population has fewer than four countries.")
  }

  events <- buildTestGlobalEvent()
  countries <- tibble::tibble(
    country_id = country_ids,
    country_name = country_ids,
    iso3 = country_ids,
    boundary_warning = ""
  )
  age_groups <- defaultAgeGroups()
  recipes <- buildBenchmarkPlotRecipes(country_ids = country_ids[1:4])
  plot_context <- buildPlotCalculationContext(population)
  seconds_limit <- max(plotPerformanceSecondsLimit(), 5)

  elapsed <- system.time({
    out <- buildPlotData(
      recipes = recipes,
      population = population,
      events = events,
      age_groups = age_groups,
      countries = countries,
      event_countries = tibble::tibble(
        event_id = character(),
        country_id = character(),
        country_role = character()
      ),
      plot_context = plot_context
    )
  })["elapsed"]

  expect_true(nrow(out) > 0)
  expect_true(
    elapsed < seconds_limit,
    info = sprintf(
      "buildPlotData on prepared RDS took %.3f s (limit %.3f s).",
      elapsed,
      seconds_limit
    )
  )
})

# Profile Generation Tracker plot build (buildPlotData / calculateStratumSeries).
#
# Measures single-query and four-query buildPlotData() on benchmark or prepared data.
# Use after plot-context optimizations (plan stage F5).
#
# From project root:
#   source("scripts/profile_plot_build.R")
#   profilePlotBuild()
#
# Non-interactive:
#   Rscript scripts/profile_plot_build.R
#
# Optional:
#   GEN_TRACKER_PROFILE_PROFVIS=TRUE  — open profvis::profvis() for buildPlotData
#   GEN_TRACKER_PROFILE_OUTPUT=path — append markdown baseline row to a file
#   GEN_TRACKER_PREPARED_POPULATION_PATH=path — use real prepared RDS instead of benchmark

locateGenTrackerRoot <- function() {
  env_root <- Sys.getenv("GEN_TRACKER_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/"))
  }

  if (file.exists(file.path("R", "project_root.R")) && file.exists("app.R")) {
    return(normalizePath(getwd(), winslash = "/"))
  }

  stop(
    "Cannot find gen_tracker project root. ",
    "setwd() to the project folder or set GEN_TRACKER_PROJECT_ROOT.",
    call. = FALSE
  )
}

formatElapsed <- function(seconds) {
  sprintf("%.3f s", as.numeric(seconds))
}

sourceProjectR <- function(root) {
  r_files <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(r_files, source, local = globalenv()))
}

buildBenchmarkFixtures <- function() {
  country_ids <- paste0("C", sprintf("%02d", 1:20))
  population <- tidyr::crossing(
    country_id = country_ids,
    year = 1970:2025,
    age = 0:100,
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

  countries <- tibble::tibble(
    country_id = country_ids,
    country_name = country_ids,
    iso3 = country_ids,
    boundary_warning = ""
  )

  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year,
    ~peak_year, ~event_origin, ~cross_country_allowed,
    "GLOBAL_CRISIS", "Global crisis", "economy", "global", 2000L, 2001L, 2000L, "manual", TRUE
  )

  recipes_one <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_group_id, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~operator, ~metric,
    "q1", "C01", "male", "school_age", NA_integer_, NA_integer_,
    "GLOBAL_CRISIS", "start", "experienced", "count"
  )

  recipes_four <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_group_id, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~operator, ~metric,
    "q1", "C01", "male", "school_age", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "experienced", "count",
    "q2", "C02", "female", "adults", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "alive_during_event", "count",
    "q3", "C03", "all", "teenagers", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "period", "born_after_event", "share_total_population",
    "q4", "C04", "all", "youth", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "experienced", "share_total_population"
  )

  list(
    population = population,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = tibble::tibble(
      event_id = character(),
      country_id = character(),
      country_role = character()
    ),
    recipes_one = recipes_one,
    recipes_four = recipes_four,
    source_label = sprintf(
      "benchmark (%s rows, %s countries)",
      format(nrow(population), big.mark = ","),
      length(unique(population$country_id))
    )
  )
}

loadPreparedFixtures <- function(rds_path) {
  if (!file.exists(rds_path)) {
    return(NULL)
  }

  population <- readRDS(rds_path)
  country_ids <- unique(population$country_id)
  if (length(country_ids) < 4L) {
    return(NULL)
  }

  countries <- tibble::tibble(
    country_id = country_ids,
    country_name = country_ids,
    iso3 = country_ids,
    boundary_warning = ""
  )
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year,
    ~peak_year, ~event_origin, ~cross_country_allowed,
    "GLOBAL_CRISIS", "Global crisis", "economy", "global", 2000L, 2001L, 2000L, "manual", TRUE
  )

  recipes_one <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_group_id, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~operator, ~metric,
    "q1", country_ids[[1]], "male", "school_age", NA_integer_, NA_integer_,
    "GLOBAL_CRISIS", "start", "experienced", "count"
  )

  recipes_four <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_group_id, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~operator, ~metric,
    "q1", country_ids[[1]], "male", "school_age", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "experienced", "count",
    "q2", country_ids[[2]], "female", "adults", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "peak", "alive_during_event", "count",
    "q3", country_ids[[3]], "all", "teenagers", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "period", "born_after_event", "share_total_population",
    "q4", country_ids[[4]], "all", "youth", NA_integer_, NA_integer_, "GLOBAL_CRISIS", "start", "experienced", "share_total_population"
  )

  list(
    population = population,
    countries = countries,
    events = events,
    age_groups = defaultAgeGroups(),
    event_countries = tibble::tibble(
      event_id = character(),
      country_id = character(),
      country_role = character()
    ),
    recipes_one = recipes_one,
    recipes_four = recipes_four,
    source_label = sprintf(
      "prepared RDS (%s rows, %s countries) — %s",
      format(nrow(population), big.mark = ","),
      length(country_ids),
      basename(rds_path)
    )
  )
}

timeStep <- function(label, expr) {
  gc(verbose = FALSE)
  elapsed <- system.time(force(expr))["elapsed"]
  list(label = label, seconds = as.numeric(elapsed))
}

profilePlotBuild <- function(root = locateGenTrackerRoot()) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(tidyr)
  })
  sourceProjectR(root)

  rds_path <- Sys.getenv("GEN_TRACKER_PREPARED_POPULATION_PATH", unset = "")
  if (!nzchar(rds_path)) {
    rds_path <- file.path(root, "data", "population.rds")
  }

  fixtures <- loadPreparedFixtures(rds_path)
  if (is.null(fixtures)) {
    fixtures <- buildBenchmarkFixtures()
  }

  population <- fixtures$population
  message("Generation Tracker plot profile: ", fixtures$source_label)

  plot_context <- NULL
  context_step <- timeStep(
    "buildPlotCalculationContext",
    plot_context <<- buildPlotCalculationContext(population)
  )

  steps <- c(
    list(context_step),
    list(
      timeStep(
        "calculateStratumSeries (1 query, cached context)",
        calculateStratumSeries(
          as.list(fixtures$recipes_one[1, ]),
          population,
          fixtures$events,
          fixtures$age_groups,
          fixtures$countries,
          event_countries = fixtures$event_countries,
          plot_context = plot_context
        )
      ),
      timeStep(
        "buildPlotData (1 query, cached context)",
        buildPlotData(
          recipes = fixtures$recipes_one,
          population = population,
          events = fixtures$events,
          age_groups = fixtures$age_groups,
          countries = fixtures$countries,
          event_countries = fixtures$event_countries,
          plot_context = plot_context
        )
      ),
      timeStep(
        "buildPlotData (4 queries, cached context)",
        buildPlotData(
          recipes = fixtures$recipes_four,
          population = population,
          events = fixtures$events,
          age_groups = fixtures$age_groups,
          countries = fixtures$countries,
          event_countries = fixtures$event_countries,
          plot_context = plot_context
        )
      ),
      timeStep(
        "buildPlotData (4 queries, no context — legacy path)",
        buildPlotData(
          recipes = fixtures$recipes_four,
          population = population,
          events = fixtures$events,
          age_groups = fixtures$age_groups,
          countries = fixtures$countries,
          event_countries = fixtures$event_countries,
          plot_context = NULL
        )
      )
    )
  )

  cat("\nPlot build profile\n")
  cat("==================\n")
  cat("Source:", fixtures$source_label, "\n\n")
  for (step in steps) {
    cat(sprintf("  %-45s %s\n", step$label, formatElapsed(step$seconds)))
  }
  cat("\n")

  output_path <- Sys.getenv("GEN_TRACKER_PROFILE_OUTPUT", unset = "")
  if (nzchar(output_path)) {
    row <- sprintf(
      "| %s | %s | context %.3f | 1q %.3f | 4q cached %.3f | 4q legacy %.3f |",
      format(Sys.Date()),
      fixtures$source_label,
      steps[[1]]$seconds,
      steps[[3]]$seconds,
      steps[[4]]$seconds,
      steps[[5]]$seconds
    )
    write(row, file = output_path, append = file.exists(output_path))
    message("Appended baseline row to ", output_path)
  }

  if (isTRUE(as.logical(Sys.getenv("GEN_TRACKER_PROFILE_PROFVIS", unset = "FALSE")))) {
    if (!requireNamespace("profvis", quietly = TRUE)) {
      warning("Install profvis to use GEN_TRACKER_PROFILE_PROFVIS=TRUE.", call. = FALSE)
    } else {
      profvis::profvis({
        buildPlotData(
          recipes = fixtures$recipes_four,
          population = population,
          events = fixtures$events,
          age_groups = fixtures$age_groups,
          countries = fixtures$countries,
          event_countries = fixtures$event_countries,
          plot_context = plot_context
        )
      })
    }
  }

  invisible(steps)
}

if (identical(Sys.getenv("GEN_TRACKER_PROFILE_PLOT_BUILD_AUTO", unset = "TRUE"), "TRUE")) {
  is_rscript <- !interactive() &&
    length(grep("^--file=", commandArgs(trailingOnly = FALSE))) > 0
  if (is_rscript) {
    profilePlotBuild()
  }
}

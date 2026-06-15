# Profile Generation Tracker startup (bootstrap data loading).
#
# Measures the same steps as bootstrapAppData() in app.R, with per-step timings.
# Use this to establish a startup baseline before performance work (plan stage F1).
#
# From project root:
#   source("scripts/profile_startup.R")
#   profileStartup()
#
# Non-interactive:
#   Rscript scripts/profile_startup.R
#
# Optional:
#   GEN_TRACKER_PROFILE_PROFVIS=TRUE  — open profvis::profvis() for full bootstrap
#   GEN_TRACKER_PROFILE_OUTPUT=path   — append markdown baseline row to a file

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

  if (file.exists(file.path("..", "R", "project_root.R")) && file.exists(file.path("..", "app.R"))) {
    return(normalizePath("..", winslash = "/"))
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

fileSizeMb <- function(path) {
  if (length(path) != 1L || !file.exists(path)) {
    return(NA_real_)
  }
  as.numeric(file.info(path)$size) / (1024^2)
}

resolveOptionalPath <- function(env_name, fallback_path) {
  env_value <- Sys.getenv(env_name, unset = "")
  if (nzchar(env_value)) {
    return(env_value)
  }
  fallback_path
}

describePopulationSource <- function(paths, data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  if (length(paths) == 0) {
    return(list(
      mode = "missing",
      label = "no sources (build data/population.rds or enable Excel)",
      paths = character(),
      prepared_path = preparedPopulationPath(data_dir),
      cache_path = populationCachePath(data_dir),
      allow_excel = isTRUE(as.logical(Sys.getenv("GEN_TRACKER_ALLOW_EXCEL_SOURCES", unset = "FALSE")))
    ))
  }

  if (length(paths) == 1L && tolower(tools::file_ext(paths)) == "rds") {
    prepared <- preparedPopulationPath(data_dir)
    mode <- if (normalizePath(paths, winslash = "/") == normalizePath(prepared, winslash = "/")) {
      "prepared_rds"
    } else if (normalizePath(paths, winslash = "/") == normalizePath(populationCachePath(data_dir), winslash = "/")) {
      "population_cache_rds"
    } else {
      "rds"
    }
    return(list(
      mode = mode,
      label = mode,
      paths = paths,
      prepared_path = prepared,
      cache_path = populationCachePath(data_dir),
      allow_excel = isTRUE(as.logical(Sys.getenv("GEN_TRACKER_ALLOW_EXCEL_SOURCES", unset = "FALSE")))
    ))
  }

  exts <- tolower(tools::file_ext(paths))
  mode <- if (all(exts %in% c("xlsx", "xls"))) {
    "excel_wpp"
  } else if (all(exts == "csv")) {
    "csv"
  } else {
    "mixed"
  }

  list(
    mode = mode,
    label = mode,
    paths = paths,
    prepared_path = preparedPopulationPath(data_dir),
    cache_path = populationCachePath(data_dir),
    allow_excel = isTRUE(as.logical(Sys.getenv("GEN_TRACKER_ALLOW_EXCEL_SOURCES", unset = "FALSE")))
  )
}

timeStep <- function(label, expr, env = parent.frame()) {
  started <- Sys.time()
  value <- eval(substitute(expr), envir = env)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  list(label = label, seconds = elapsed, value = value)
}

bootstrapAppDataTimed <- function() {
  population_paths <- resolvePopulationPaths()
  events_path <- resolveOptionalPath("GEN_TRACKER_EVENTS_PATH", eventsDataPath())
  countries_path <- resolveOptionalPath("GEN_TRACKER_COUNTRIES_PATH", countriesDataPath())
  event_countries_path <- resolveOptionalPath(
    "GEN_TRACKER_EVENT_COUNTRIES_PATH",
    eventCountriesDeployPath()
  )

  missing <- c()
  if (length(population_paths) == 0) {
    missing <- c(
      missing,
      paste(
        "data/population.rds (run source('scripts/build_prepared_population.R') after WPP Excel import)",
        "or set GEN_TRACKER_POPULATION_PATHS / GEN_TRACKER_ALLOW_EXCEL_SOURCES=TRUE",
        sep = "; "
      )
    )
  }
  if (!file.exists(events_path)) {
    missing <- c(missing, sprintf("events file at %s", events_path))
  }
  if (!file.exists(countries_path)) {
    missing <- c(missing, sprintf("countries file at %s", countries_path))
  }
  if (length(missing) > 0) {
    stop(sprintf("Failed to bootstrap data sources: missing %s.", paste(missing, collapse = "; ")))
  }

  steps <- list()
  source_info <- describePopulationSource(population_paths)

  if (length(population_paths) == 1L && tolower(tools::file_ext(population_paths)) == "rds") {
    rds_path <- population_paths[[1]]
    steps$readRDS <- timeStep("readRDS", readRDS(rds_path))
    pop_for_validate <- steps$readRDS$value
    use_strict <- populationValidationStrictForPath(rds_path)
    steps$validatePopulation <- timeStep(
      "validatePopulation",
      validatePopulation(pop_for_validate, strict = use_strict)
    )
    if (!use_strict) {
      cat("  (manifest-backed RDS: duplicate-key scan skipped, same as app startup)\n")
    }
    rm(pop_for_validate)
  }

  steps$loadPopulationData <- timeStep(
    "loadPopulationData",
    suppressMessages(loadPopulationData(population_paths, strict = NULL))
  )
  population <- steps$loadPopulationData$value

  steps$loadEvents <- timeStep("loadEvents", loadEvents(events_path))
  events <- steps$loadEvents$value

  steps$loadCountryDictionary <- timeStep(
    "loadCountryDictionary",
    loadCountryDictionary(countries_path)
  )
  countries <- steps$loadCountryDictionary$value

  steps$loadEventCountries <- timeStep(
    "loadEventCountries",
    if (file.exists(event_countries_path)) {
      loadEventCountries(event_countries_path)
    } else {
      tibble::tibble(
        event_id = character(),
        country_id = character(),
        country_role = character()
      )
    }
  )
  event_countries <- steps$loadEventCountries$value

  steps$validateEvents <- timeStep(
    "validateEvents",
    validateEvents(events, countries = countries, event_countries = event_countries)
  )

  list(
    population = population,
    events = events,
    countries = countries,
    event_countries = event_countries,
    steps = steps,
    paths = list(
      population = population_paths,
      events = events_path,
      countries = countries_path,
      event_countries = event_countries_path
    ),
    source = source_info
  )
}

printStartupProfile <- function(profile) {
  cat("\n=== Generation Tracker startup profile ===\n\n")

  cat("Session\n")
  cat("  date:       ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
  cat("  platform:   ", R.version$platform, "\n", sep = "")
  cat("  R version:  ", R.version$version.string, "\n", sep = "")
  cat("  working dir:", getwd(), "\n\n", sep = "")

  cat("Environment (relevant)\n")
  env_keys <- c(
    "GEN_TRACKER_DATA_DIR",
    "GEN_TRACKER_POPULATION_PATHS",
    "GEN_TRACKER_PREPARED_POPULATION_PATH",
    "GEN_TRACKER_POPULATION_CACHE_PATH",
    "GEN_TRACKER_ALLOW_EXCEL_SOURCES",
    "GEN_TRACKER_EVENTS_PATH",
    "GEN_TRACKER_COUNTRIES_PATH",
    "GEN_TRACKER_EVENT_COUNTRIES_PATH"
  )
  for (key in env_keys) {
    val <- Sys.getenv(key, unset = "")
    if (nzchar(val)) {
      cat("  ", key, "=", val, "\n", sep = "")
    }
  }
  cat("\n")

  src <- profile$source
  cat("Population source\n")
  cat("  mode:          ", src$mode, " (", src$label, ")\n", sep = "")
  cat("  allow_excel:   ", src$allow_excel, "\n", sep = "")
  cat("  prepared_path: ", src$prepared_path, "\n", sep = "")
  cat("  cache_path:    ", src$cache_path, "\n", sep = "")
  if (length(src$paths) > 0) {
    for (p in src$paths) {
      cat("  path:          ", p, "\n", sep = "")
      cat("    file_size:   ", sprintf("%.2f MiB", fileSizeMb(p)), "\n", sep = "")
      cat("    cache_fresh: ", populationSourcesAreFresh(src$paths, src$cache_path), "\n", sep = "")
    }
  }
  pop <- profile$population
  cat("  nrow:          ", nrow(pop), "\n", sep = "")
  cat("  ncol:          ", ncol(pop), "\n", sep = "")
  cat("  object.size:   ", format(object.size(pop), units = "auto"), "\n\n", sep = "")

  cat("Timings (bootstrapAppData steps)\n")
  step_names <- names(profile$steps)
  for (nm in step_names) {
    step <- profile$steps[[nm]]
    cat(sprintf("  %-22s %s\n", step$label, formatElapsed(step$seconds)))
  }

  boot_elapsed <- sum(vapply(profile$steps, function(s) s$seconds, numeric(1)))
  cat(sprintf("  %-22s %s (sum of steps above)\n", "bootstrap (partial)", formatElapsed(boot_elapsed)))

  non_overlap <- c(
    "loadPopulationData",
    "loadEvents",
    "loadCountryDictionary",
    "loadEventCountries",
    "validateEvents"
  )
  overlap_present <- !is.null(profile$steps$readRDS)
  if (overlap_present) {
    cat(
      "\n  Note: for prepared RDS, readRDS and validatePopulation are sub-steps ",
      "also included inside loadPopulationData; do not add all three for a realistic total.\n",
      sep = ""
    )
  }
  non_overlap_seconds <- sum(
    vapply(
      intersect(non_overlap, names(profile$steps)),
      function(nm) profile$steps[[nm]]$seconds,
      numeric(1)
    )
  )
  cat(sprintf(
    "  %-22s %s (loadPopulation + dictionaries; use for SLO)\n",
    "bootstrap (effective)",
    formatElapsed(non_overlap_seconds)
  ))

  if (!is.null(profile$bootstrap_total_seconds)) {
    cat(sprintf("  %-22s %s (single timed run)\n", "bootstrapAppDataTimed", formatElapsed(profile$bootstrap_total_seconds)))
  }

  if (!is.null(profile$source_r_seconds)) {
    cat(sprintf("\n  source(R/*.R):        %s\n", formatElapsed(profile$source_r_seconds)))
  }

  cat("\nMarkdown row (paste into docs/dev_workflow.md baseline table):\n\n")
  cat(profileMarkdownRow(profile), "\n")
}

profileMarkdownRow <- function(profile) {
  src <- profile$source
  path_label <- if (length(src$paths) == 0) {
    "—"
  } else {
    paste(basename(src$paths), collapse = "; ")
  }
  size_mb <- if (length(src$paths) == 1L) {
    sprintf("%.2f", fileSizeMb(src$paths[[1]]))
  } else {
    "—"
  }

  step_seconds <- function(name) {
    step <- profile$steps[[name]]
    if (is.null(step)) {
      return("—")
    }
    sprintf("%.3f", step$seconds)
  }

  non_overlap_names <- c(
    "loadPopulationData",
    "loadEvents",
    "loadCountryDictionary",
    "loadEventCountries",
    "validateEvents"
  )
  effective_seconds <- sum(
    vapply(
      intersect(non_overlap_names, names(profile$steps)),
      function(nm) profile$steps[[nm]]$seconds,
      numeric(1)
    )
  )

  paste(
    "|",
    format(Sys.Date(), "%Y-%m-%d"),
    "|",
    src$mode,
    "|",
    path_label,
    "|",
    size_mb,
    "|",
    nrow(profile$population),
    "|",
    step_seconds("readRDS"),
    "|",
    step_seconds("loadPopulationData"),
    "|",
    step_seconds("validatePopulation"),
    "|",
    step_seconds("loadEvents"),
    "|",
    step_seconds("loadCountryDictionary"),
    "|",
    step_seconds("validateEvents"),
    "|",
    sprintf("%.3f", profile$bootstrap_total_seconds %||% sum(vapply(profile$steps, function(s) s$seconds, numeric(1)))),
    "|",
    sprintf("%.3f", effective_seconds),
    "|",
    sprintf("%s R %s.%s", R.version$platform, R.version$major, R.version$minor),
    "|"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    y
  } else {
    x
  }
}

profileStartup <- function(run_profvis = NULL) {
  root_dir <- locateGenTrackerRoot()
  source(file.path(root_dir, "R", "project_root.R"), local = FALSE)

  source_started <- Sys.time()
  loadProjectSources(root_dir)
  source_r_seconds <- as.numeric(difftime(Sys.time(), source_started, units = "secs"))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root_dir)

  if (is.null(run_profvis)) {
    run_profvis <- isTRUE(as.logical(Sys.getenv("GEN_TRACKER_PROFILE_PROFVIS", unset = "FALSE")))
  }

  if (run_profvis) {
    if (!requireNamespace("profvis", quietly = TRUE)) {
      stop(
        "profvis is not installed. Install with install.packages('profvis') ",
        "or set GEN_TRACKER_PROFILE_PROFVIS=FALSE.",
        call. = FALSE
      )
    }
    message("Opening profvis for bootstrapAppDataTimed() ...")
    return(profvis::profvis({
      bootstrapAppDataTimed()
    }))
  }

  boot_started <- Sys.time()
  boot <- bootstrapAppDataTimed()
  boot$bootstrap_total_seconds <- as.numeric(difftime(Sys.time(), boot_started, units = "secs"))
  boot$source_r_seconds <- source_r_seconds

  profile <- boot
  printStartupProfile(profile)

  out_path <- Sys.getenv("GEN_TRACKER_PROFILE_OUTPUT", unset = "")
  if (nzchar(out_path)) {
    header <- paste(
      "| Date | Mode | Path | File MiB | nrow | readRDS | loadPopulation | validatePopulation |",
      "loadEvents | loadCountries | validateEvents | bootstrap total | bootstrap effective | Platform |",
      "\n|------|------|------|----------|------|---------|------------------|----------------------|",
      "------------|---------------|----------------|-----------------|---------------------|---------|\n",
      sep = ""
    )
    row <- profileMarkdownRow(profile)
    if (!file.exists(out_path)) {
      writeLines(c(header, row), out_path, useBytes = TRUE)
    } else {
      write(row, file = out_path, append = TRUE)
    }
    message("Appended baseline row to: ", out_path)
  }

  invisible(profile)
}

if (length(grep("^source\\(", sys.calls())) == 0L) {
  profileStartup()
}

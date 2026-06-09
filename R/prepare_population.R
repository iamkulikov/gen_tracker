PREPARED_POPULATION_SCHEMA_VERSION <- "1"

preparedPopulationPath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  env_path <- Sys.getenv("GEN_TRACKER_PREPARED_POPULATION_PATH", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  file.path(data_dir, "population.rds")
}

preparedPopulationManifestPath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  file.path(data_dir, "population_build_manifest.json")
}

manifestJsonField <- function(text, key, quoted = TRUE) {
  pattern <- if (quoted) {
    sprintf('"%s"\\s*:\\s*"([^"]*)"', key)
  } else {
    sprintf('"%s"\\s*:\\s*([0-9.]+)', key)
  }
  match <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(match) < 2) {
    return(NA)
  }
  if (quoted) {
    return(match[[2]])
  }
  as.numeric(match[[2]])
}

readPreparedPopulationManifest <- function(path = preparedPopulationManifestPath()) {
  if (!file.exists(path)) {
    return(NULL)
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "")
  list(
    schema_version = manifestJsonField(text, "schema_version"),
    built_at = manifestJsonField(text, "built_at"),
    output_path = manifestJsonField(text, "output_path"),
    row_count = manifestJsonField(text, "row_count", quoted = FALSE),
    file_size_bytes = manifestJsonField(text, "file_size_bytes", quoted = FALSE),
    compress = manifestJsonField(text, "compress")
  )
}

pathsReferToSameFile <- function(path_a, path_b) {
  if (is.na(path_a) || is.na(path_b) || !nzchar(path_a) || !nzchar(path_b)) {
    return(FALSE)
  }
  norm_a <- tryCatch(
    normalizePath(path_a, winslash = "/", mustWork = TRUE),
    error = function(e) normalizePath(path_a, winslash = "/", mustWork = FALSE)
  )
  norm_b <- tryCatch(
    normalizePath(path_b, winslash = "/", mustWork = TRUE),
    error = function(e) normalizePath(path_b, winslash = "/", mustWork = FALSE)
  )
  identical(norm_a, norm_b)
}

preparedPopulationManifestCoversFile <- function(
  rds_path,
  manifest_path = preparedPopulationManifestPath()
) {
  if (length(rds_path) != 1L || !file.exists(rds_path)) {
    return(FALSE)
  }

  manifest <- readPreparedPopulationManifest(manifest_path)
  if (is.null(manifest)) {
    return(FALSE)
  }
  if (is.na(manifest$schema_version) ||
      manifest$schema_version != PREPARED_POPULATION_SCHEMA_VERSION) {
    return(FALSE)
  }
  if (!pathsReferToSameFile(manifest$output_path, rds_path)) {
    return(FALSE)
  }

  if (!is.na(manifest$row_count) && manifest$row_count <= 0) {
    return(FALSE)
  }

  TRUE
}

populationValidationStrictForPath <- function(path, strict = NULL) {
  if (!is.null(strict)) {
    return(isTRUE(strict))
  }
  if (length(path) != 1L) {
    return(TRUE)
  }
  if (tolower(tools::file_ext(path)) != "rds") {
    return(TRUE)
  }
  !preparedPopulationManifestCoversFile(path)
}

missingPreparedPopulationHint <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  prepared <- preparedPopulationPath(data_dir)
  paste(
    sprintf("prepared population at %s", prepared),
    "(build offline: Rscript scripts/build_prepared_population.R;",
    "do not use data/population_cache.rds for the app;",
    "set GEN_TRACKER_ALLOW_EXCEL_SOURCES=TRUE only for one-off Excel import at startup)",
    sep = " "
  )
}

resolveWppSourcePaths <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  search_dir <- resolveWppSourcesSearchDir(data_dir)
  paths <- sort(list.files(
    search_dir,
    pattern = "^WPP2024_.*SINGLE_AGE.*\\.(xlsx|xls)$",
    full.names = TRUE
  ))
  if (length(paths) == 0) {
    stop(sprintf("No WPP2024 single-age Excel files found in %s.", search_dir))
  }

  female <- paths[grepl("Female", paths, ignore.case = TRUE)]
  male <- paths[grepl("Male", paths, ignore.case = TRUE)]
  male <- setdiff(male, female)
  if (length(male) == 0 || length(female) == 0) {
    stop(
      "Expected one Male and one Female WPP2024 single-age file. ",
      "Found: ", paste(basename(paths), collapse = ", ")
    )
  }

  c(male[1], female[1])
}

filterPopulationToCountries <- function(population, countries) {
  country_ids <- unique(countries$country_id)
  if (length(country_ids) == 0) {
    stop("countries table has no country_id values.")
  }

  missing_in_pop <- setdiff(country_ids, unique(population$country_id))
  if (length(missing_in_pop) > 0) {
    warning(
      sprintf(
        "These country_id values are absent in population source and will have no rows: %s",
        paste(missing_in_pop, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  name_lookup <- countries |>
    dplyr::distinct(.data$country_id, .data$country_name)

  population |>
    dplyr::filter(.data$country_id %in% country_ids) |>
    dplyr::select(-dplyr::any_of("country_name")) |>
    dplyr::left_join(name_lookup, by = "country_id", relationship = "many-to-one") |>
    dplyr::relocate(.data$country_name, .after = .data$country_id)
}

escapeJsonString <- function(x) {
  gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', as.character(x), fixed = TRUE), fixed = TRUE)
}

writePreparedPopulationManifest <- function(manifest, path = preparedPopulationManifestPath()) {
  lines <- c(
    "{",
    sprintf('  "schema_version": "%s",', escapeJsonString(manifest$schema_version)),
    sprintf('  "built_at": "%s",', escapeJsonString(manifest$built_at)),
    sprintf('  "output_path": "%s",', escapeJsonString(manifest$output_path)),
    sprintf(
      '  "source_files": [%s],',
      paste(sprintf('"%s"', escapeJsonString(manifest$source_files)), collapse = ", ")
    ),
    sprintf('  "countries_file": "%s",', escapeJsonString(manifest$countries_file)),
    sprintf('  "country_count": %s,', manifest$country_count),
    sprintf('  "row_count": %s,', manifest$row_count),
    sprintf('  "year_min": %s,', manifest$year_min),
    sprintf('  "year_max": %s,', manifest$year_max),
    sprintf(
      '  "sexes": [%s],',
      paste(sprintf('"%s"', escapeJsonString(manifest$sexes)), collapse = ", ")
    ),
    sprintf(
      '  "data_types": [%s],',
      paste(sprintf('"%s"', escapeJsonString(manifest$data_types)), collapse = ", ")
    ),
    sprintf('  "file_size_bytes": %s,', manifest$file_size_bytes),
    sprintf('  "compress": "%s",', escapeJsonString(manifest$compress)),
    sprintf('  "build_seconds": %s', manifest$build_seconds),
    "}"
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

buildPreparedPopulation <- function(
  data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data"),
  countries_path = file.path(data_dir, "countries.csv"),
  output_path = preparedPopulationPath(data_dir),
  compress = "gzip"
) {
  if (!file.exists(countries_path)) {
    stop(sprintf("countries file not found: %s", countries_path))
  }

  countries <- loadCountryDictionary(countries_path)
  source_paths <- resolveWppSourcePaths(data_dir)

  message(sprintf("Loading %s ...", basename(source_paths[1])))
  message(sprintf("Loading %s ...", basename(source_paths[2])))
  started <- Sys.time()
  population <- loadPopulationData(source_paths, write_cache = FALSE)
  message(sprintf(
    "WPP import finished (%s rows). Filtering to countries.csv ...",
    format(nrow(population), big.mark = ",")
  ))

  population <- filterPopulationToCountries(population, countries)
  if (exists("validatePopulation", mode = "function")) {
    validatePopulation(population, strict = TRUE)
  }

  output_dir <- dirname(output_path)
  if (nzchar(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  saveRDS(population, output_path, compress = compress)
  elapsed <- difftime(Sys.time(), started, units = "secs")

  manifest <- list(
    schema_version = PREPARED_POPULATION_SCHEMA_VERSION,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    output_path = normalizePath(output_path, winslash = "/", mustWork = FALSE),
    source_files = basename(source_paths),
    countries_file = basename(countries_path),
    country_count = length(unique(population$country_id)),
    row_count = nrow(population),
    year_min = min(population$year, na.rm = TRUE),
    year_max = max(population$year, na.rm = TRUE),
    sexes = sort(unique(population$sex)),
    data_types = sort(unique(population$data_type)),
    file_size_bytes = file.info(output_path)$size,
    compress = compress,
    build_seconds = as.numeric(elapsed)
  )
  writePreparedPopulationManifest(manifest)

  message(sprintf(
    "Saved %s (%s rows, %.1f MB, %s s). Manifest: %s",
    output_path,
    format(manifest$row_count, big.mark = ","),
    manifest$file_size_bytes / 1024^2,
    format(manifest$build_seconds, digits = 1),
    preparedPopulationManifestPath(data_dir)
  ))

  invisible(population)
}

build_prepared_population <- buildPreparedPopulation
prepared_population_path <- preparedPopulationPath
read_prepared_population_manifest <- readPreparedPopulationManifest
prepared_population_manifest_covers_file <- preparedPopulationManifestCoversFile
population_validation_strict_for_path <- populationValidationStrictForPath
missing_prepared_population_hint <- missingPreparedPopulationHint

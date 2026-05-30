GEN_TRACKER_APP_VERSION <- "0.1.0-mvp"

genTrackerAppVersion <- function() {
  GEN_TRACKER_APP_VERSION
}

summarizePopulationSource <- function(
  population,
  population_paths = NULL,
  manifest_path = preparedPopulationManifestPath()
) {
  source_versions <- character(0)
  sources <- character(0)
  if (!is.null(population) && nrow(population) > 0) {
    if ("source" %in% names(population)) {
      sources <- sort(unique(as.character(population$source)))
    }
    if ("source_version" %in% names(population)) {
      source_versions <- sort(unique(as.character(population$source_version)))
    }
  }

  manifest <- readPreparedPopulationManifest(manifest_path)
  built_at <- if (!is.null(manifest) && !is.na(manifest$built_at)) {
    manifest$built_at
  } else {
    NA_character_
  }

  population_file <- if (length(population_paths) == 1L) {
    basename(population_paths[[1]])
  } else if (length(population_paths) > 1L) {
    paste(length(population_paths), "files")
  } else {
    NA_character_
  }

  list(
    population_file = population_file,
    population_paths = population_paths,
    demographic_source = if (length(sources) > 0) paste(sources, collapse = ", ") else "UN WPP",
    source_version = if (length(source_versions) > 0) {
      paste(source_versions, collapse = ", ")
    } else {
      "unknown"
    },
    built_at = built_at,
    manifest_path = manifest_path
  )
}

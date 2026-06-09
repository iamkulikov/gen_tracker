resolveDataDir <- function() {
  Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")
}

dataPath <- function(..., data_dir = NULL) {
  file.path(data_dir %||% resolveDataDir(), ...)
}

firstExistingPath <- function(candidates) {
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }
  candidates[[1]]
}

dataSourcesDir <- function(data_dir = NULL) {
  dataPath("sources", data_dir = data_dir)
}

dataGeneratedDir <- function(data_dir = NULL) {
  dataPath("generated", data_dir = data_dir)
}

dataConfigDir <- function(data_dir = NULL) {
  dataPath("config", data_dir = data_dir)
}

generatedEventsDir <- function(data_dir = NULL) {
  dataPath("generated", "events", data_dir = data_dir)
}

generatedIndicatorsDir <- function(data_dir = NULL) {
  dataPath("generated", "indicators", data_dir = data_dir)
}

generatedCacheDir <- function(data_dir = NULL) {
  dataPath("generated", "cache", data_dir = data_dir)
}

wppSourcesDir <- function(data_dir = NULL) {
  dataPath("sources", "wpp", data_dir = data_dir)
}

indicatorSourcesDir <- function(data_dir = NULL) {
  dataPath("sources", "indicators", data_dir = data_dir)
}

resolveWppSourcesSearchDir <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  firstExistingPath(c(
    wppSourcesDir(base),
    dataPath(base)
  ))
}

resolveIndicatorExcelPath <- function(file_name, data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  firstExistingPath(c(
    file.path(indicatorSourcesDir(base), file_name),
    file.path(base, "indicators", file_name),
    dataPath(file_name, data_dir = base)
  ))
}

resolveIndicatorsCsvDir <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  firstExistingPath(c(
    generatedIndicatorsDir(base),
    dataPath("indicators", data_dir = base)
  ))
}

resolveConfigPath <- function(file_name, data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  firstExistingPath(c(
    file.path(dataConfigDir(base), file_name),
    dataPath(file_name, data_dir = base)
  ))
}

resolveEventDataPath <- function(file_name, data_dir = NULL) {
  dataPath(file_name, data_dir = data_dir)
}

resolveGeneratedEventPath <- function(file_name, data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  firstExistingPath(c(
    file.path(generatedEventsDir(base), file_name),
    dataPath(file_name, data_dir = base)
  ))
}

resolvePopulationCachePath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  env_path <- Sys.getenv("GEN_TRACKER_POPULATION_CACHE_PATH", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  preferred <- file.path(generatedCacheDir(base), "population_cache.rds")
  legacy <- dataPath("population_cache.rds", data_dir = base)
  if (file.exists(preferred)) {
    return(preferred)
  }
  legacy
}

eventCountriesManualLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("event_countries_manual.csv", data_dir = data_dir)
}

eventCountriesComputedLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("event_countries_computed.csv", data_dir = data_dir)
}

eventCountriesCompositeLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("event_countries_composite.csv", data_dir = data_dir)
}

eventTagsManualLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("event_tags_manual.csv", data_dir = data_dir)
}

eventTagsComputedLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("event_tags_computed.csv", data_dir = data_dir)
}

eventsComputedManifestPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("events_computed_manifest.json", data_dir = data_dir)
}

compositeEventsManifestPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath("composite_events_manifest.json", data_dir = data_dir)
}

resolveMigrationSourceSearchDir <- function(data_dir = NULL) {
  resolveWppSourcesSearchDir(data_dir = data_dir)
}

resolve_data_dir <- resolveDataDir
data_path <- dataPath
first_existing_path <- firstExistingPath

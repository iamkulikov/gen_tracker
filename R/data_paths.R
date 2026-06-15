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

# Pipeline-stage filenames (0–10). See docs/data_maintenance.md §3.
DATA_FILE_COUNTRIES <- "0_countries.csv"
DATA_FILE_EVENTS <- "1_events.csv"
DATA_FILE_EVENT_COUNTRIES_CURATED <- "2_event_countries_curated.csv"
DATA_FILE_EVENT_COUNTRIES_MANUAL_LAYER <- "3_event_countries_manual_layer.csv"
DATA_FILE_EVENT_TAGS_MANUAL_LAYER <- "3_event_tags_manual.csv"
DATA_FILE_EVENT_CRITERIA <- "4_event_criteria.csv"
DATA_FILE_INDICATOR_PREFIX <- "5_"
DATA_FILE_EVENTS_COMPUTED <- "6_events_computed.csv"
DATA_FILE_EVENT_COUNTRIES_COMPUTED <- "7_event_countries_computed.csv"
DATA_FILE_EVENT_TAGS_COMPUTED <- "7_event_tags_computed.csv"
DATA_FILE_EVENTS_COMPUTED_MANIFEST <- "7_events_computed_manifest.json"
DATA_FILE_COMPOSITE_CLASSIFIERS <- "8_composite_classifiers.csv"
DATA_FILE_COMPOSITE_EVENTS <- "8_composite_events.csv"
DATA_FILE_COMPOSITE_MEMBERS <- "8_composite_members.csv"
DATA_FILE_EVENT_COUNTRIES_DEPLOY <- "9_event_countries.csv"
DATA_FILE_EVENT_TAGS_DEPLOY <- "9_event_tags.csv"
DATA_FILE_EVENT_COUNTRIES_COMPOSITE <- "10_event_countries_composite.csv"
DATA_FILE_COMPOSITE_EVENTS_MANIFEST <- "10_composite_events_manifest.json"
TEMPLATE_EVENT_CRITERIA <- "template_event_criteria.csv"
TEMPLATE_COMPOSITE_CLASSIFIERS <- "template_composite_classifiers.csv"

resolvePipelineFile <- function(new_name, legacy_names = character(), candidates = NULL) {
  if (is.null(candidates)) {
    candidates <- c(new_name, legacy_names)
  }
  firstExistingPath(candidates)
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

indicatorCsvFilename <- function(indicator_stem) {
  paste0(DATA_FILE_INDICATOR_PREFIX, indicator_stem, ".csv")
}

indicatorNameFromCsvBasename <- function(file_name) {
  stem <- tools::file_path_sans_ext(basename(file_name))
  sub(paste0("^", DATA_FILE_INDICATOR_PREFIX), "", stem)
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

resolveGeneratedEventPath <- function(file_name, data_dir = NULL, legacy_names = character()) {
  base <- data_dir %||% resolveDataDir()
  candidates <- c(
    file.path(generatedEventsDir(base), file_name),
    unlist(lapply(
      c(file_name, legacy_names),
      function(name) c(
        file.path(generatedEventsDir(base), name),
        dataPath(name, data_dir = base)
      )
    ))
  )
  resolvePipelineFile(file_name, legacy_names = legacy_names, candidates = unique(candidates))
}

countriesDataPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_COUNTRIES,
    legacy_names = "countries.csv",
    candidates = c(
      dataPath(DATA_FILE_COUNTRIES, data_dir = base),
      dataPath("countries.csv", data_dir = base)
    )
  )
}

eventsDataPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENTS,
    legacy_names = "events.csv",
    candidates = c(
      dataPath(DATA_FILE_EVENTS, data_dir = base),
      dataPath("events.csv", data_dir = base)
    )
  )
}

eventCountriesCuratedPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENT_COUNTRIES_CURATED,
    legacy_names = "event_countries_manual.csv",
    candidates = c(
      dataPath(DATA_FILE_EVENT_COUNTRIES_CURATED, data_dir = base),
      dataPath("event_countries_manual.csv", data_dir = base)
    )
  )
}

eventCountriesDeployPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENT_COUNTRIES_DEPLOY,
    legacy_names = "event_countries.csv",
    candidates = c(
      dataPath(DATA_FILE_EVENT_COUNTRIES_DEPLOY, data_dir = base),
      dataPath("event_countries.csv", data_dir = base)
    )
  )
}

eventTagsDeployPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENT_TAGS_DEPLOY,
    legacy_names = "event_tags.csv",
    candidates = c(
      dataPath(DATA_FILE_EVENT_TAGS_DEPLOY, data_dir = base),
      dataPath("event_tags.csv", data_dir = base)
    )
  )
}

eventsComputedDataPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENTS_COMPUTED,
    legacy_names = "events_computed.csv",
    candidates = c(
      dataPath(DATA_FILE_EVENTS_COMPUTED, data_dir = base),
      dataPath("events_computed.csv", data_dir = base)
    )
  )
}

compositeEventsDataPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_COMPOSITE_EVENTS,
    legacy_names = "composite_events.csv",
    candidates = c(
      dataPath(DATA_FILE_COMPOSITE_EVENTS, data_dir = base),
      dataPath("composite_events.csv", data_dir = base)
    )
  )
}

compositeMembersDataPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_COMPOSITE_MEMBERS,
    legacy_names = "composite_members.csv",
    candidates = c(
      dataPath(DATA_FILE_COMPOSITE_MEMBERS, data_dir = base),
      dataPath("composite_members.csv", data_dir = base)
    )
  )
}

eventCriteriaPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_EVENT_CRITERIA,
    legacy_names = "event_criteria.csv",
    candidates = c(
      file.path(dataConfigDir(base), DATA_FILE_EVENT_CRITERIA),
      file.path(dataConfigDir(base), "event_criteria.csv"),
      dataPath(DATA_FILE_EVENT_CRITERIA, data_dir = base),
      dataPath("event_criteria.csv", data_dir = base)
    )
  )
}

eventCriteriaTemplatePath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    TEMPLATE_EVENT_CRITERIA,
    legacy_names = "event_criteria.template.csv",
    candidates = c(
      dataPath(TEMPLATE_EVENT_CRITERIA, data_dir = base),
      dataPath("event_criteria.template.csv", data_dir = base),
      file.path(dataConfigDir(base), TEMPLATE_EVENT_CRITERIA),
      file.path(dataConfigDir(base), "event_criteria.template.csv")
    )
  )
}

compositeClassifiersPath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    DATA_FILE_COMPOSITE_CLASSIFIERS,
    legacy_names = "composite_classifiers.csv",
    candidates = c(
      file.path(dataConfigDir(base), DATA_FILE_COMPOSITE_CLASSIFIERS),
      file.path(dataConfigDir(base), "composite_classifiers.csv"),
      dataPath(DATA_FILE_COMPOSITE_CLASSIFIERS, data_dir = base),
      dataPath("composite_classifiers.csv", data_dir = base)
    )
  )
}

compositeClassifiersTemplatePath <- function(data_dir = NULL) {
  base <- data_dir %||% resolveDataDir()
  resolvePipelineFile(
    TEMPLATE_COMPOSITE_CLASSIFIERS,
    legacy_names = "composite_classifiers.template.csv",
    candidates = c(
      dataPath(TEMPLATE_COMPOSITE_CLASSIFIERS, data_dir = base),
      dataPath("composite_classifiers.template.csv", data_dir = base),
      file.path(dataConfigDir(base), TEMPLATE_COMPOSITE_CLASSIFIERS),
      file.path(dataConfigDir(base), "composite_classifiers.template.csv")
    )
  )
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
  resolveGeneratedEventPath(
    DATA_FILE_EVENT_COUNTRIES_MANUAL_LAYER,
    data_dir = data_dir,
    legacy_names = "event_countries_manual.csv"
  )
}

eventCountriesComputedLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_EVENT_COUNTRIES_COMPUTED,
    data_dir = data_dir,
    legacy_names = "event_countries_computed.csv"
  )
}

eventCountriesCompositeLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_EVENT_COUNTRIES_COMPOSITE,
    data_dir = data_dir,
    legacy_names = "event_countries_composite.csv"
  )
}

eventTagsManualLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_EVENT_TAGS_MANUAL_LAYER,
    data_dir = data_dir,
    legacy_names = "event_tags_manual.csv"
  )
}

eventTagsComputedLayerPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_EVENT_TAGS_COMPUTED,
    data_dir = data_dir,
    legacy_names = "event_tags_computed.csv"
  )
}

eventsComputedManifestPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_EVENTS_COMPUTED_MANIFEST,
    data_dir = data_dir,
    legacy_names = "events_computed_manifest.json"
  )
}

compositeEventsManifestPath <- function(data_dir = NULL) {
  resolveGeneratedEventPath(
    DATA_FILE_COMPOSITE_EVENTS_MANIFEST,
    data_dir = data_dir,
    legacy_names = "composite_events_manifest.json"
  )
}

eventCountriesManualLayerWritePath <- function(data_dir = NULL) {
  file.path(generatedEventsDir(data_dir), DATA_FILE_EVENT_COUNTRIES_MANUAL_LAYER)
}

generatedEventFileWritePath <- function(file_name, data_dir = NULL) {
  file.path(generatedEventsDir(data_dir), file_name)
}

eventCountriesDeployWritePath <- function(data_dir = NULL) {
  resolveEventDataPath(DATA_FILE_EVENT_COUNTRIES_DEPLOY, data_dir = data_dir)
}

eventTagsDeployWritePath <- function(data_dir = NULL) {
  resolveEventDataPath(DATA_FILE_EVENT_TAGS_DEPLOY, data_dir = data_dir)
}

eventsComputedWritePath <- function(data_dir = NULL) {
  resolveEventDataPath(DATA_FILE_EVENTS_COMPUTED, data_dir = data_dir)
}

compositeEventsWritePath <- function(data_dir = NULL) {
  resolveEventDataPath(DATA_FILE_COMPOSITE_EVENTS, data_dir = data_dir)
}

compositeMembersWritePath <- function(data_dir = NULL) {
  resolveEventDataPath(DATA_FILE_COMPOSITE_MEMBERS, data_dir = data_dir)
}

resolveMigrationSourceSearchDir <- function(data_dir = NULL) {
  resolveWppSourcesSearchDir(data_dir = data_dir)
}

resolve_data_dir <- resolveDataDir
data_path <- dataPath
first_existing_path <- firstExistingPath

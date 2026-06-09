#!/usr/bin/env Rscript
# Generate events_computed.csv from indicators + criteria (idempotent; manual events untouched).

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")
indicators_dir <- resolveIndicatorsCsvDir(data_dir)
criteria_path <- resolveConfigPath("event_criteria.csv", data_dir)
if (!file.exists(criteria_path)) {
  template_path <- resolveEventDataPath("event_criteria.template.csv", data_dir)
  if (file.exists(template_path)) {
    dir.create(dataConfigDir(data_dir), recursive = TRUE, showWarnings = FALSE)
    config_copy <- file.path(dataConfigDir(data_dir), "event_criteria.csv")
    file.copy(template_path, config_copy)
    criteria_path <- config_copy
    message("Created ", criteria_path, " from template.")
  }
}
if (!file.exists(criteria_path)) {
  criteria_path <- resolveEventDataPath("event_criteria.template.csv", data_dir)
}

if (file.exists(resolveIndicatorExcelPath("CPI.xlsx", data_dir)) ||
    file.exists(resolveIndicatorExcelPath("ER.xlsx", data_dir)) ||
    file.exists(resolveIndicatorExcelPath("Defaults_DB.xlsx", data_dir))) {
  prep <- prepareIndicatorsFromExcel(data_dir = data_dir)
  if (length(prep$written) > 0) {
    message("Prepared indicators: ", paste(basename(prep$written), collapse = ", "))
  }
}
countries_path <- resolveEventDataPath("countries.csv", data_dir)
output_events <- resolveEventDataPath("events_computed.csv", data_dir)
output_links <- eventCountriesComputedLayerPath(data_dir)
output_tags <- eventTagsComputedLayerPath(data_dir)
output_manifest <- eventsComputedManifestPath(data_dir)

if (!file.exists(criteria_path)) {
  stop("Missing event_criteria.csv (or template) in data/config/ or data/.")
}
if (!dir.exists(indicators_dir)) {
  stop("Missing generated indicators directory with indicator CSV files.")
}
if (!file.exists(countries_path)) {
  stop("Missing data/countries.csv.")
}

criteria <- loadEventCriteria(criteria_path)
indicators_by_name <- loadIndicatorsDirectory(indicators_dir)
countries <- loadCountryDictionary(countries_path)

built <- buildComputedEventsFromCriteria(indicators_by_name, criteria, countries = countries)
events_out <- built$events |>
  dplyr::select(
    "event_id", "event_name", "event_type", "event_scope",
    "start_year", "end_year", "peak_year", "event_origin", "cross_country_allowed",
    "show_in_picker"
  )

dir.create(generatedEventsDir(data_dir), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(events_out, output_events)
writeEventCountryLayer(built$event_countries, output_links)
writeEventTagLayer(built$event_tags, output_tags)
writeComputedEventsManifest(output_manifest, criteria, indicators_by_name, events_out)

links_merge <- mergeDeployEventLinks(data_dir = data_dir)
tags_merge <- mergeDeployEventTags(data_dir = data_dir)

message(sprintf(
  paste(
    "Wrote %d computed events to %s (%d country links, %d tags).",
    "Deploy merge: %d links, %d tags."
  ),
  nrow(events_out),
  output_events,
  nrow(built$event_countries),
  nrow(built$event_tags),
  nrow(links_merge$links),
  nrow(tags_merge$tags)
))

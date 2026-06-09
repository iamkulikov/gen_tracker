#!/usr/bin/env Rscript
# Materialize per-country composite events from event tags (full overwrite of composite artifacts).

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")
classifiers_path <- resolveConfigPath("composite_classifiers.csv", data_dir)
countries_path <- resolveEventDataPath("countries.csv", data_dir)
manual_events_path <- resolveEventDataPath("events.csv", data_dir)
manual_links_path <- eventCountriesManualLayerPath(data_dir)

output_composites <- resolveEventDataPath("composite_events.csv", data_dir)
output_members <- resolveEventDataPath("composite_members.csv", data_dir)
output_links <- eventCountriesCompositeLayerPath(data_dir)
output_manifest <- compositeEventsManifestPath(data_dir)

if (!file.exists(classifiers_path)) {
  template_path <- resolveEventDataPath("composite_classifiers.template.csv", data_dir)
  if (file.exists(template_path)) {
    dir.create(dataConfigDir(data_dir), recursive = TRUE, showWarnings = FALSE)
    config_copy <- file.path(dataConfigDir(data_dir), "composite_classifiers.csv")
    file.copy(template_path, config_copy)
    classifiers_path <- config_copy
    message("Created ", classifiers_path, " from template.")
  } else {
    stop(
      "Missing ",
      classifiers_path,
      ". Copy data/composite_classifiers.template.csv or add classifier_spec rows.",
      call. = FALSE
    )
  }
}
if (!file.exists(manual_events_path)) {
  stop("Missing data/events.csv.")
}

classifiers <- readr::read_csv(classifiers_path, show_col_types = FALSE)
criteria <- loadEventCriteria(resolveConfigPath("event_criteria.csv", data_dir))
if (nrow(criteria) == 0L) {
  criteria <- loadEventCriteria(resolveEventDataPath("event_criteria.template.csv", data_dir))
}
if (!"classifier_spec" %in% names(classifiers)) {
  stop("composite_classifiers.csv must contain classifier_spec.")
}

universe <- loadEventsUniverse(manual_path = manual_events_path, data_dir = data_dir)
events <- universe$elementary_events
countries <- if (file.exists(countries_path)) loadCountryDictionary(countries_path) else NULL

event_countries <- loadEventCountriesUniverse(
  manual_path = resolveEventDataPath("event_countries.csv", data_dir),
  data_dir = data_dir
)

event_tags <- loadEventTagsUniverse(data_dir = data_dir)

all_composites <- emptyCompositeEventsFrame()
all_members <- emptyCompositeMembersFrame()
all_links <- emptyComputedEventCountriesFrame()

for (i in seq_len(nrow(classifiers))) {
  spec <- classifiers$classifier_spec[[i]]
  composite_name <- if ("composite_name" %in% names(classifiers)) {
    classifiers$composite_name[[i]]
  } else {
    NULL
  }
  built <- buildCompositeEventsFromTags(
    events = events,
    event_tags = event_tags,
    classifier_spec = spec,
    event_countries = event_countries,
    countries = countries,
    criteria = criteria,
    composite_name = composite_name
  )
  all_composites <- dplyr::bind_rows(all_composites, built$composite_events)
  all_members <- dplyr::bind_rows(all_members, built$composite_members)
  all_links <- dplyr::bind_rows(all_links, built$event_countries)
}

dir.create(generatedEventsDir(data_dir), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(all_composites, output_composites)
readr::write_csv(all_members, output_members)
writeEventCountryLayer(all_links, output_links)

manifest_lines <- c(
  "{",
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf('  "composite_event_rows": %d,', nrow(all_composites)),
  sprintf('  "composite_member_rows": %d', nrow(all_members)),
  "}"
)
writeLines(manifest_lines, output_manifest, useBytes = TRUE)

links_merge <- mergeDeployEventLinks(data_dir = data_dir)
tags_merge <- mergeDeployEventTags(data_dir = data_dir)

message(sprintf(
  "Wrote %d composite events (%d members) to %s. Deploy links: %d rows; tags: %d rows.",
  nrow(all_composites),
  nrow(all_members),
  output_composites,
  nrow(links_merge$links),
  nrow(tags_merge$tags)
))

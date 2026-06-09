#!/usr/bin/env Rscript
# One-time / idempotent migration to the Stage 9 data/ layout (variant B + C).
#
# Moves sources and generated artefacts into subfolders, seeds layer files,
# writes merged deploy link/tag CSVs, and removes superseded split files at root.

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")

move_if_exists <- function(from, to) {
  if (!file.exists(from)) {
    return(invisible(FALSE))
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (normalizePath(from, winslash = "/") == normalizePath(to, winslash = "/")) {
    return(invisible(TRUE))
  }
  if (file.exists(to)) {
    message("Skip (target exists): ", to)
    return(invisible(FALSE))
  }
  ok <- file.rename(from, to)
  if (!ok) {
    file.copy(from, to, overwrite = FALSE)
    unlink(from)
  }
  message("Moved ", basename(from), " -> ", to)
  invisible(TRUE)
}

remove_if_exists <- function(path) {
  if (file.exists(path)) {
    unlink(path)
    message("Removed superseded ", path)
  }
}

message("Migrating data layout under ", normalizePath(data_dir, winslash = "/"))

dirs <- c(
  wppSourcesDir(data_dir),
  indicatorSourcesDir(data_dir),
  generatedIndicatorsDir(data_dir),
  generatedEventsDir(data_dir),
  generatedCacheDir(data_dir),
  dataConfigDir(data_dir)
)
invisible(lapply(dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE)))

wpp_pattern <- "^WPP2024_.*\\.(xlsx|xls)$"
for (path in list.files(data_dir, pattern = wpp_pattern, full.names = TRUE)) {
  move_if_exists(path, file.path(wppSourcesDir(data_dir), basename(path)))
}

for (name in c("CPI.xlsx", "ER.xlsx")) {
  move_if_exists(dataPath(name, data_dir = data_dir), file.path(indicatorSourcesDir(data_dir), name))
}
move_if_exists(
  dataPath("indicators", "Defaults_DB.xlsx", data_dir = data_dir),
  file.path(indicatorSourcesDir(data_dir), "Defaults_DB.xlsx")
)

for (name in c("cpi_inflation.csv", "exchange_rate.csv")) {
  move_if_exists(
    dataPath("indicators", name, data_dir = data_dir),
    file.path(generatedIndicatorsDir(data_dir), name)
  )
}

move_if_exists(
  dataPath("population_cache.rds", data_dir = data_dir),
  file.path(generatedCacheDir(data_dir), "population_cache.rds")
)

for (name in c("event_criteria.csv", "composite_classifiers.csv")) {
  move_if_exists(dataPath(name, data_dir = data_dir), file.path(dataConfigDir(data_dir), name))
}

for (name in c("events_computed_manifest.json", "composite_events_manifest.json")) {
  move_if_exists(dataPath(name, data_dir = data_dir), file.path(generatedEventsDir(data_dir), name))
}

deploy_links <- resolveEventDataPath("event_countries.csv", data_dir)
if (file.exists(deploy_links)) {
  existing <- loadEventCountries(deploy_links)
  if (!"origin" %in% names(existing)) {
    writeEventCountryLayer(existing, eventCountriesManualLayerPath(data_dir))
  }
}

for (pair in list(
  c("event_countries_computed.csv", "event_countries_computed.csv"),
  c("event_countries_composite.csv", "event_countries_composite.csv"),
  c("event_tags_computed.csv", "event_tags_computed.csv")
)) {
  move_if_exists(
    dataPath(pair[[1]], data_dir = data_dir),
    file.path(generatedEventsDir(data_dir), pair[[2]])
  )
}

legacy_manual_tags <- dataPath("event_tags.csv", data_dir = data_dir)
manual_tags_layer <- eventTagsManualLayerPath(data_dir)
if (file.exists(legacy_manual_tags)) {
  tags <- loadEventTagLayer(legacy_manual_tags)
  if ("origin" %in% names(tags)) {
    writeEventTagLayer(stripEventTagsOrigin(tags), manual_tags_layer)
    remove_if_exists(legacy_manual_tags)
  } else if (!file.exists(manual_tags_layer)) {
    writeEventTagLayer(tags, manual_tags_layer)
  }
}

mergeDeployEventLinks(data_dir = data_dir)
mergeDeployEventTags(data_dir = data_dir)

remove_if_exists(dataPath("event_countries_computed.csv", data_dir = data_dir))
remove_if_exists(dataPath("event_countries_composite.csv", data_dir = data_dir))
remove_if_exists(dataPath("event_tags_computed.csv", data_dir = data_dir))

legacy_indicators_dir <- dataPath("indicators", data_dir = data_dir)
if (dir.exists(legacy_indicators_dir)) {
  remaining <- list.files(legacy_indicators_dir, full.names = TRUE)
  remaining <- remaining[!grepl("README\\.md$", remaining, ignore.case = TRUE)]
  if (length(remaining) == 0) {
    message("Legacy data/indicators/ is empty (README may remain).")
  }
}

message("Migration complete. Deploy root should contain merged event_countries.csv with origin column.")

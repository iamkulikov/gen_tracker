#!/usr/bin/env Rscript
# One-time / idempotent rename of event-pipeline files to numbered stage names (0–10).
# Run after pulling the pipeline-filename change; safe to re-run (skips missing sources).

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")

rename_if_exists <- function(from, to) {
  if (!file.exists(from)) {
    return(invisible(FALSE))
  }
  if (file.exists(to)) {
    message("Skip (target exists): ", to)
    return(invisible(FALSE))
  }
  from_norm <- normalizePath(from, winslash = "/", mustWork = FALSE)
  to_norm <- normalizePath(to, winslash = "/", mustWork = FALSE)
  if (nzchar(from_norm) && nzchar(to_norm) && from_norm == to_norm) {
    return(invisible(TRUE))
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.rename(from, to)
  if (!ok) {
    file.copy(from, to, overwrite = FALSE)
    unlink(from)
  }
  message("Renamed ", basename(from), " -> ", basename(to))
  invisible(TRUE)
}

message("Renaming event pipeline files under ", normalizePath(data_dir, winslash = "/"))

root <- function(name) dataPath(name, data_dir = data_dir)
cfg <- function(name) file.path(dataConfigDir(data_dir), name)
gen_events <- function(name) file.path(generatedEventsDir(data_dir), name)
gen_ind <- function(name) file.path(generatedIndicatorsDir(data_dir), name)

# 0 — countries
rename_if_exists(root("countries.csv"), root(DATA_FILE_COUNTRIES))

# 1 — manual events
rename_if_exists(root("events.csv"), root(DATA_FILE_EVENTS))

# 2 — curated cross-country links (hand-edited)
rename_if_exists(root("event_countries_manual.csv"), root(DATA_FILE_EVENT_COUNTRIES_CURATED))

# 3 — manual link/tag layers (generated)
rename_if_exists(gen_events("event_countries_manual.csv"), gen_events(DATA_FILE_EVENT_COUNTRIES_MANUAL_LAYER))
rename_if_exists(gen_events("event_tags_manual.csv"), gen_events(DATA_FILE_EVENT_TAGS_MANUAL_LAYER))

# 4 — computed criteria config
rename_if_exists(cfg("event_criteria.csv"), cfg(DATA_FILE_EVENT_CRITERIA))
rename_if_exists(root("event_criteria.csv"), cfg(DATA_FILE_EVENT_CRITERIA))

# 5 — prepared indicators
for (stem in c("cpi_inflation", "exchange_rate", "sovereign_defaults")) {
  rename_if_exists(gen_ind(paste0(stem, ".csv")), gen_ind(indicatorCsvFilename(stem)))
  rename_if_exists(dataPath("indicators", paste0(stem, ".csv"), data_dir = data_dir), gen_ind(indicatorCsvFilename(stem)))
}

# 6 — computed events catalogue
rename_if_exists(root("events_computed.csv"), root(DATA_FILE_EVENTS_COMPUTED))

# 7 — computed layers
rename_if_exists(gen_events("event_countries_computed.csv"), gen_events(DATA_FILE_EVENT_COUNTRIES_COMPUTED))
rename_if_exists(gen_events("event_tags_computed.csv"), gen_events(DATA_FILE_EVENT_TAGS_COMPUTED))
rename_if_exists(gen_events("events_computed_manifest.json"), gen_events(DATA_FILE_EVENTS_COMPUTED_MANIFEST))
rename_if_exists(root("event_countries_computed.csv"), gen_events(DATA_FILE_EVENT_COUNTRIES_COMPUTED))
rename_if_exists(root("event_tags_computed.csv"), gen_events(DATA_FILE_EVENT_TAGS_COMPUTED))

# 8 — composite config + catalogues
rename_if_exists(cfg("composite_classifiers.csv"), cfg(DATA_FILE_COMPOSITE_CLASSIFIERS))
rename_if_exists(root("composite_classifiers.csv"), cfg(DATA_FILE_COMPOSITE_CLASSIFIERS))
rename_if_exists(root("composite_events.csv"), root(DATA_FILE_COMPOSITE_EVENTS))
rename_if_exists(root("composite_members.csv"), root(DATA_FILE_COMPOSITE_MEMBERS))

# 9 — deploy merge (links/tags)
rename_if_exists(root("event_countries.csv"), root(DATA_FILE_EVENT_COUNTRIES_DEPLOY))
rename_if_exists(root("event_tags.csv"), root(DATA_FILE_EVENT_TAGS_DEPLOY))

# 10 — composite layers
rename_if_exists(gen_events("event_countries_composite.csv"), gen_events(DATA_FILE_EVENT_COUNTRIES_COMPOSITE))
rename_if_exists(gen_events("composite_events_manifest.json"), gen_events(DATA_FILE_COMPOSITE_EVENTS_MANIFEST))
rename_if_exists(root("event_countries_composite.csv"), gen_events(DATA_FILE_EVENT_COUNTRIES_COMPOSITE))

# Templates (no stage index)
rename_if_exists(root("event_criteria.template.csv"), root(TEMPLATE_EVENT_CRITERIA))
rename_if_exists(cfg("event_criteria.template.csv"), root(TEMPLATE_EVENT_CRITERIA))
rename_if_exists(root("composite_classifiers.template.csv"), root(TEMPLATE_COMPOSITE_CLASSIFIERS))
rename_if_exists(cfg("composite_classifiers.template.csv"), root(TEMPLATE_COMPOSITE_CLASSIFIERS))

message("Pipeline filename migration complete.")

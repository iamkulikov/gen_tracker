# Schema constants and helpers for the v2 event-curation framework.
# See docs/event_curation_methodology.md for the conceptual background.

eventCurationFamilies <- function() {
  c(
    "statehood_borders",
    "regime_institutions",
    "organized_violence",
    "macro_financial_crisis",
    "development_model_policy",
    "mass_health_demography",
    "disaster_environment",
    "social_movement_rights",
    "technology_media_infrastructure",
    "cultural_symbolic"
  )
}

eventCurationSelectionChannels <- function() {
  c(
    "manual_curated",
    "dataset_threshold",
    "composite_indicator",
    "literature_based",
    "memory_or_oral_history",
    "expert_review"
  )
}

eventCurationScoreFields <- function() {
  c(
    "population_reach_score",
    "intensity_score",
    "institutional_discontinuity_score",
    "memory_salience_score",
    "cohort_relevance_score",
    "source_confidence_score"
  )
}

# event_origin value that marks a row as curated to the v2 gold standard and
# therefore subject to strict validation.
eventCurationV2Origin <- function() {
  "manual_curated_v2"
}

isManualCuratedV2Event <- function(events) {
  if (!"event_origin" %in% names(events)) {
    return(rep(FALSE, nrow(events)))
  }
  !is.na(events$event_origin) & events$event_origin == eventCurationV2Origin()
}

# Compact human-readable summary of the six 0-3 curation scores for a data frame
# of score columns (one row per event). Returns "" when all scores are missing.
formatCurationScoreSummary <- function(scores) {
  scores <- tibble::as_tibble(scores)
  labels <- c(
    population_reach_score = "reach",
    intensity_score = "intensity",
    institutional_discontinuity_score = "institutional",
    memory_salience_score = "memory",
    cohort_relevance_score = "cohort",
    source_confidence_score = "source"
  )
  present <- intersect(names(labels), names(scores))
  if (length(present) == 0L || nrow(scores) == 0L) {
    return(rep("", nrow(scores)))
  }
  vapply(seq_len(nrow(scores)), function(i) {
    parts <- vapply(present, function(col) {
      value <- scores[[col]][[i]]
      if (is.na(value)) return(NA_character_)
      sprintf("%s %d", labels[[col]], as.integer(value))
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0L) "" else paste(parts, collapse = " / ")
  }, character(1))
}

# Ensure the v2 curation columns exist with the expected types. Missing columns
# are added as typed NA so downstream code (loaders, validators, exporters) can
# rely on their presence even for legacy or computed/composite catalogs.
normalizeEventCurationFields <- function(events) {
  if (!"short_description" %in% names(events)) events$short_description <- NA_character_
  if (!"source_url" %in% names(events)) events$source_url <- NA_character_
  if (!"event_family" %in% names(events)) events$event_family <- NA_character_
  if (!"selection_channel" %in% names(events)) events$selection_channel <- NA_character_
  for (score in eventCurationScoreFields()) {
    if (!score %in% names(events)) events[[score]] <- NA_integer_
  }
  if (!"include_in_core_catalogue" %in% names(events)) {
    events$include_in_core_catalogue <- NA
  }

  events |>
    dplyr::mutate(
      short_description = as.character(.data$short_description),
      source_url = as.character(.data$source_url),
      event_family = as.character(.data$event_family),
      selection_channel = as.character(.data$selection_channel),
      dplyr::across(dplyr::all_of(eventCurationScoreFields()), as.integer),
      include_in_core_catalogue = as.logical(.data$include_in_core_catalogue)
    )
}

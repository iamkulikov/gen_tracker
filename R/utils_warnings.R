indicatorCoverageDisclaimer <- function() {
  paste(
    "Indicator coverage limits computed and composite events.",
    "Inflation data starts in 1960, exchange rates in 1980,",
    "and WPP single-age population in 1950 in the current setup;",
    "episodes before those windows are not included unless added manually to the curated catalogue."
  )
}

isIndicatorDerivedEvent <- function(event) {
  if (is.null(event) || nrow(event) == 0) {
    return(FALSE)
  }
  origin <- event$event_origin[[1]] %||% "manual"
  origin %in% c("computed", "composite") || isCompositeEvent(event)
}

eventCatalogDescription <- function(event) {
  if (is.null(event) || nrow(event) == 0) {
    return(character(0))
  }
  if ("description" %in% names(event)) {
    text <- trimws(as.character(event$description[[1]]))
    if (length(text) == 1L && nzchar(text)) {
      return(text)
    }
  }
  character(0)
}

collectQueryWarnings <- function(
  recipe,
  event,
  country_row,
  event_countries = NULL,
  stratum_series = NULL,
  composite_members = NULL,
  events = NULL,
  migration = NULL
) {
  warnings <- character(0)

  boundary_warning <- dplyr::coalesce(country_row$boundary_warning, "")
  if (nzchar(boundary_warning)) {
    warnings <- c(warnings, country_row$boundary_warning)
  }

  compat <- checkEventCountryCompatibility(
    country_id = recipe$country_id,
    event = event,
    event_countries = event_countries,
    country_name = country_row$country_name,
    event_name = event$event_name
  )
  if (compat$level == "warning" && nzchar(compat$message)) {
    warnings <- c(warnings, compat$message)
  }

  if (!is.null(stratum_series)) {
    growth_warning <- stratumGrowthWarning(
      stratum_series,
      recipe,
      event,
      composite_members = composite_members,
      events = events
    )
    if (length(growth_warning) == 1L && nzchar(growth_warning)) {
      warnings <- c(warnings, growth_warning)
    }
  }

  reliability_implemented <- !is.null(migration)
  if (reliability_implemented && !is.null(stratum_series)) {
    reliability <- computeStratumReliability(
      series = stratum_series,
      recipe = recipe,
      country_row = country_row,
      event = event,
      event_countries = event_countries,
      migration = migration
    )
    score_line <- formatReliabilityScoreLine(reliability)
    if (length(score_line) == 1L && nzchar(score_line)) {
      warnings <- c(warnings, score_line)
    }
    if (!is.na(reliability$reliability_warning)) {
      warnings <- c(warnings, reliability$reliability_warning)
    }
  }

  warnings <- c(
    warnings,
    "Migration is not formally adjusted in MVP."
  )
  if (!reliability_implemented) {
    warnings <- c(warnings, "Reliability score is not yet implemented.")
  }

  if (isIndicatorDerivedEvent(event)) {
    warnings <- c(warnings, indicatorCoverageDisclaimer())
    event_description <- eventCatalogDescription(event)
    if (length(event_description) == 1L) {
      warnings <- c(warnings, event_description)
    }
  }

  unique(warnings)
}

# Reliability scoring (v1). See docs/reliability_methodology.md.
#
# The score is an interpretation-reliability indicator in [0, 100] where 100 is a
# clean historical estimate with little migration turnover and no projection or
# border caveats. It is NOT a population correction: country-level net migration
# cannot adjust a specific age/sex cohort, so the score only flags how cautiously
# a line should be read.

RELIABILITY_VERSION <- "reliability-v1"

RELIABILITY_MIGRATION_PENALTY_PER_RATE <- 4 # penalty points per 1 unit of mean |rate| per 1,000/yr
RELIABILITY_MIGRATION_PENALTY_MAX <- 40
RELIABILITY_PROJECTION_PENALTY_MAX <- 20
RELIABILITY_BOUNDARY_PENALTY <- 15
RELIABILITY_LINK_PENALTY <- c(ok = 0, warning = 10, error = 25)
RELIABILITY_WARNING_THRESHOLD <- 70

reliabilityObservedYears <- function(series) {
  if (is.null(series) || nrow(series) == 0) {
    return(integer(0))
  }
  observed <- series$year[is.finite(series$stratum_population) & series$stratum_population > 0]
  if (length(observed) == 0) {
    observed <- series$year
  }
  sort(unique(as.integer(observed)))
}

migrationExposureForSeries <- function(
  series,
  country_id,
  migration = NULL,
  country_features = NULL
) {
  observed_years <- reliabilityObservedYears(series)
  if (length(observed_years) > 0 && !is.null(migration) && nrow(migration) > 0) {
    country_rate <- migration |>
      dplyr::filter(
        .data$country_id == .env$country_id,
        .data$year %in% .env$observed_years,
        is.finite(.data$net_migration_rate)
      )
    if (nrow(country_rate) > 0) {
      return(mean(abs(country_rate$net_migration_rate)))
    }
  }

  if (!is.null(country_features) && nrow(country_features) > 0) {
    row <- country_features |>
      dplyr::filter(.data$country_id == .env$country_id) |>
      dplyr::slice(1)
    if (nrow(row) == 1 && is.finite(row$abs_rate_mean)) {
      return(as.numeric(row$abs_rate_mean))
    }
  }

  NA_real_
}

reliabilityProjectionShare <- function(series) {
  if (is.null(series) || nrow(series) == 0 || !"is_projection" %in% names(series)) {
    return(0)
  }
  observed_years <- reliabilityObservedYears(series)
  rows <- series[series$year %in% observed_years, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(0)
  }
  mean(rows$is_projection %in% TRUE)
}

reliabilityEventLinkLevel <- function(recipe, event, event_countries, countries = NULL) {
  if (is.null(event) || nrow(event) == 0) {
    return("ok")
  }
  compat <- tryCatch(
    checkEventCountryCompatibility(
      country_id = recipe$country_id,
      event = event,
      event_countries = event_countries,
      countries = countries
    ),
    error = function(e) list(level = "ok")
  )
  level <- compat$level %||% "ok"
  if (!level %in% names(RELIABILITY_LINK_PENALTY)) {
    level <- "ok"
  }
  level
}

buildReliabilityWarning <- function(score, factors) {
  if (!is.finite(score) || score >= RELIABILITY_WARNING_THRESHOLD) {
    return(NA_character_)
  }
  factors <- factors[nzchar(factors)]
  if (length(factors) == 0) {
    return(NA_character_)
  }
  sprintf(
    "Reliability %d/100: %s. Interpret this line cautiously.",
    as.integer(round(score)),
    paste(factors, collapse = "; ")
  )
}

formatReliabilitySummary <- function(reliability) {
  score <- reliability$reliability_score
  if (!is.finite(score)) {
    return("Reliability: not scored (migration data not loaded).")
  }
  exposure <- reliability$migration_exposure
  exposure_part <- if (is.finite(exposure) && exposure > 0) {
    sprintf(
      " Mean |net migration| over the shown years is about %.1f per 1,000 population per year.",
      exposure
    )
  } else if (is.finite(exposure)) {
    " Migration turnover over the shown years is very low."
  } else {
    ""
  }
  sprintf(
    "Reliability: %d/100.%s Higher scores mean the line is easier to interpret; migration is not formally corrected.",
    as.integer(round(score)),
    exposure_part
  )
}

formatReliabilityScoreLine <- function(reliability) {
  score <- reliability$reliability_score
  if (!is.finite(score)) {
    return(character(0))
  }
  sprintf("Reliability score: %d/100", as.integer(round(score)))
}

computeStratumReliability <- function(
  series,
  recipe,
  country_row = NULL,
  event = NULL,
  event_countries = NULL,
  countries = NULL,
  migration = NULL
) {
  na_result <- list(
    reliability_score = NA_real_,
    migration_exposure = NA_real_,
    reliability_warning = NA_character_
  )
  if (is.null(migration)) {
    return(na_result)
  }

  migration_table <- migration$migration %||% migration
  country_features <- migration$country_features %||% NULL

  exposure <- migrationExposureForSeries(
    series = series,
    country_id = recipe$country_id,
    migration = migration_table,
    country_features = country_features
  )
  projection_share <- reliabilityProjectionShare(series)
  boundary_warning <- ""
  if (!is.null(country_row) && "boundary_warning" %in% names(country_row)) {
    boundary_warning <- dplyr::coalesce(country_row$boundary_warning[[1]], "")
  }
  link_level <- reliabilityEventLinkLevel(recipe, event, event_countries, countries)

  mig_penalty <- if (is.finite(exposure)) {
    min(RELIABILITY_MIGRATION_PENALTY_MAX, RELIABILITY_MIGRATION_PENALTY_PER_RATE * exposure)
  } else {
    0
  }
  proj_penalty <- RELIABILITY_PROJECTION_PENALTY_MAX * projection_share
  boundary_penalty <- if (nzchar(boundary_warning)) RELIABILITY_BOUNDARY_PENALTY else 0
  link_penalty <- unname(RELIABILITY_LINK_PENALTY[[link_level]])

  score <- max(0, min(100, 100 - mig_penalty - proj_penalty - boundary_penalty - link_penalty))

  factors <- c(
    if (mig_penalty > 0) {
      sprintf("net migration over the observed years (mean |rate| ~ %.1f per 1,000/yr)", exposure)
    } else "",
    if (proj_penalty > 0) {
      sprintf("%d%% of the shown years are projections", as.integer(round(100 * projection_share)))
    } else "",
    if (boundary_penalty > 0) "historical borders may differ from the modern country" else "",
    if (link_penalty > 0) "the event is not directly linked to this country" else ""
  )

  list(
    reliability_score = score,
    migration_exposure = if (is.finite(exposure)) exposure else NA_real_,
    reliability_warning = buildReliabilityWarning(score, factors)
  )
}

compute_stratum_reliability <- computeStratumReliability
migration_exposure_for_series <- migrationExposureForSeries
format_reliability_summary <- formatReliabilitySummary
format_reliability_score_line <- formatReliabilityScoreLine

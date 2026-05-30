collectQueryWarnings <- function(
  recipe,
  event,
  country_row,
  event_countries = NULL
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

  warnings <- c(
    warnings,
    "Migration is not formally adjusted in MVP.",
    "Reliability score is not yet implemented."
  )

  unique(warnings)
}

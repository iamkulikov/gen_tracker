loadEvents <- function(path) {
  events <- readr::read_csv(path, show_col_types = FALSE)
  if (!"event_origin" %in% names(events)) events$event_origin <- "manual"
  if (!"cross_country_allowed" %in% names(events)) events$cross_country_allowed <- FALSE

  events |>
    dplyr::mutate(
      event_id = as.character(event_id),
      event_name = as.character(event_name),
      event_type = as.character(event_type),
      event_scope = as.character(event_scope),
      start_year = as.integer(start_year),
      end_year = as.integer(end_year),
      peak_year = as.integer(peak_year),
      event_origin = dplyr::coalesce(.data$event_origin, "manual"),
      cross_country_allowed = dplyr::coalesce(as.logical(.data$cross_country_allowed), FALSE)
    )
}

loadEventCountries <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      event_id = as.character(event_id),
      country_id = as.character(country_id),
      country_role = dplyr::coalesce(as.character(country_role), "affected")
    )
}

loadCountryDictionary <- function(path) {
  countries <- readr::read_csv(path, show_col_types = FALSE)
  if (!"boundary_warning" %in% names(countries)) countries$boundary_warning <- ""

  countries |>
    dplyr::mutate(
      country_id = as.character(country_id),
      country_name = as.character(country_name),
      boundary_warning = dplyr::coalesce(as.character(boundary_warning), "")
    )
}

load_events <- loadEvents
load_country_dictionary <- loadCountryDictionary

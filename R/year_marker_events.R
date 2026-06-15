YEAR_MARKER_EVENT_ORIGIN <- "year_marker"

buildYearMarkerEvents <- function(year_min, year_max) {
  year_min <- as.integer(year_min)
  year_max <- as.integer(year_max)
  if (!is.finite(year_min) || !is.finite(year_max) || year_min > year_max) {
    return(tibble::tibble(
      event_id = character(),
      event_name = character(),
      event_type = character(),
      event_scope = character(),
      start_year = integer(),
      end_year = integer(),
      peak_year = integer(),
      event_origin = character(),
      cross_country_allowed = logical(),
      show_in_picker = logical(),
      country_id = character()
    ))
  }

  years <- seq.int(year_min, year_max)
  tibble::tibble(
    event_id = sprintf("YEAR_%d", years),
    event_name = as.character(years),
    event_type = "year_marker",
    event_scope = "global",
    start_year = years,
    end_year = years,
    peak_year = years,
    event_origin = YEAR_MARKER_EVENT_ORIGIN,
    cross_country_allowed = TRUE,
    show_in_picker = TRUE,
    country_id = NA_character_
  )
}

appendYearMarkerEvents <- function(events, year_bounds) {
  if (is.null(year_bounds) || length(year_bounds) < 2L) {
    return(events)
  }
  year_min <- year_bounds[["min"]] %||% year_bounds[[1]]
  year_max <- year_bounds[["max"]] %||% year_bounds[[2]]
  markers <- buildYearMarkerEvents(year_min, year_max)
  if (nrow(markers) == 0) {
    return(events)
  }
  dplyr::bind_rows(events, markers) |>
    dplyr::distinct(.data$event_id, .keep_all = TRUE)
}

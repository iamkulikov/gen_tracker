validateEventCountries <- function(events, countries, event_countries) {
  if (is.null(event_countries) || nrow(event_countries) == 0) {
    return(invisible(list(warnings = character(0))))
  }

  allowed_roles <- c("affected", "origin", "culturally_relevant")
  if (!all(event_countries$country_role %in% allowed_roles)) {
    stop(
      "country_role must be one of: affected, origin, culturally_relevant.",
      call. = FALSE
    )
  }

  dup <- event_countries |>
    dplyr::count(.data$event_id, .data$country_id, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop("Duplicate (event_id, country_id) pairs in event_countries.", call. = FALSE)
  }

  orphan_events <- setdiff(event_countries$event_id, events$event_id)
  if (length(orphan_events) > 0) {
    stop("Some event_countries.event_id are missing in events dictionary.", call. = FALSE)
  }

  if (!is.null(countries)) {
    orphan_countries <- setdiff(event_countries$country_id, countries$country_id)
    if (length(orphan_countries) > 0) {
      stop(
        "Some event_countries.country_id are missing in countries dictionary.",
        call. = FALSE
      )
    }
  }

  national_events <- events |>
    dplyr::filter(.data$event_scope == "national")
  linked_events <- unique(event_countries$event_id)
  missing_links <- national_events$event_id[
    !national_events$event_id %in% linked_events
  ]
  if (length(missing_links) > 0) {
    stop(
      paste(
        "National events without country links:",
        paste(utils::head(missing_links, 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(list(warnings = character(0)))
}

validateEvents <- function(events, countries = NULL, event_countries = NULL) {
  if (anyNA(events$event_id) || any(events$event_id == "")) {
    stop("event_id must be non-empty.")
  }
  if (anyDuplicated(events$event_id) > 0) {
    stop("event_id must be unique.")
  }
  if (any(events$start_year > events$end_year, na.rm = TRUE)) {
    stop("Found events with start_year > end_year.")
  }
  if (any(events$peak_year < events$start_year | events$peak_year > events$end_year, na.rm = TRUE)) {
    stop("Found events with peak_year outside [start_year, end_year].")
  }

  allowed_event_type <- c(
    "war", "economy", "politics", "policy", "society",
    "health", "disaster", "technology", "energy", "culture", "sport"
  )
  if (!all(events$event_type %in% allowed_event_type)) {
    stop(
      "event_type must be one of: war, economy, politics, policy, society, ",
      "health, disaster, technology, energy, culture, sport."
    )
  }

  allowed_scope <- c("national", "multi_country", "global")
  if (!all(events$event_scope %in% allowed_scope)) {
    stop("event_scope must be one of: national, multi_country, global.")
  }

  if (!is.null(event_countries) && !is.null(countries) && nrow(event_countries) > 0) {
    validateEventCountries(events, countries, event_countries)
  }

  invisible(TRUE)
}

validatePopulation <- function(population, strict = TRUE) {
  required_cols <- c(
    "country_id", "country_name", "year", "sex", "age", "population",
    "data_type", "scenario", "source", "source_version"
  )
  missing_cols <- setdiff(required_cols, names(population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Population misses columns: %s", paste(missing_cols, collapse = ", ")))
  }

  if (anyNA(population$country_id) || any(population$country_id == "")) {
    stop("country_id must be non-empty for all rows.")
  }
  if (anyNA(population$year) || anyNA(population$age) || anyNA(population$sex)) {
    stop("year/age/sex cannot contain NA.")
  }
  if (any(!population$sex %in% c("all", "male", "female"))) {
    stop("sex must be one of all/male/female.")
  }
  if (any(population$age < 0 | population$age > 120, na.rm = TRUE)) {
    stop("age must be in [0, 120].")
  }
  if (any(population$population < 0, na.rm = TRUE)) {
    stop("Population values must be >= 0.")
  }

  if (any(!population$data_type %in% c("estimate", "projection"))) {
    stop("data_type must be estimate or projection.")
  }

  if (!isTRUE(strict)) {
    return(invisible(TRUE))
  }

  dup <- population |>
    dplyr::count(
      .data$country_id, .data$year, .data$sex, .data$age, .data$scenario,
      .data$data_type, name = "n"
    ) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop("Population has duplicate keys by country_id/year/sex/age/scenario/data_type.")
  }

  invisible(TRUE)
}

validate_events <- validateEvents
validate_event_countries <- validateEventCountries
validate_population <- validatePopulation

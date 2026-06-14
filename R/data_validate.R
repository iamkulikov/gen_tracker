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

isUrlShaped <- function(x) {
  !is.na(x) & grepl("^https?://[^[:space:]]+$", x)
}

# Validate the v2 event-curation metadata fields. Rows curated to the v2 gold
# standard (event_origin == "manual_curated_v2") must satisfy the strict
# requirements; legacy/dataset-derived rows only generate warnings so existing
# development is not blocked. Returns a character vector of warnings.
validateEventCurationFields <- function(events) {
  events <- normalizeEventCurationFields(events)
  v2 <- isManualCuratedV2Event(events)
  warnings <- character(0)

  allowed_family <- eventCurationFamilies()
  allowed_channel <- eventCurationSelectionChannels()
  score_fields <- eventCurationScoreFields()

  # short_description: required and non-empty for v2 rows.
  missing_desc <- v2 & (is.na(events$short_description) | !nzchar(trimws(events$short_description)))
  if (any(missing_desc)) {
    stop(
      "manual_curated_v2 events require a non-empty short_description: ",
      paste(utils::head(events$event_id[missing_desc], 5), collapse = ", "),
      call. = FALSE
    )
  }

  # source_url: required and URL-shaped for v2 rows.
  bad_url <- v2 & !isUrlShaped(events$source_url)
  if (any(bad_url)) {
    stop(
      "manual_curated_v2 events require a URL-shaped source_url (http(s)://...): ",
      paste(utils::head(events$event_id[bad_url], 5), collapse = ", "),
      call. = FALSE
    )
  }

  # event_family: required and valid for v2 rows; warn for invalid legacy values.
  fam <- events$event_family
  bad_family_v2 <- v2 & (is.na(fam) | !(fam %in% allowed_family))
  if (any(bad_family_v2)) {
    stop(
      "manual_curated_v2 events require a valid event_family: ",
      paste(utils::head(events$event_id[bad_family_v2], 5), collapse = ", "),
      call. = FALSE
    )
  }
  bad_family_legacy <- !v2 & !is.na(fam) & nzchar(fam) & !(fam %in% allowed_family)
  if (any(bad_family_legacy)) {
    warnings <- c(warnings, sprintf(
      "Legacy events have unrecognized event_family values: %s.",
      paste(utils::head(events$event_id[bad_family_legacy], 5), collapse = ", ")
    ))
  }

  # selection_channel: required and valid for v2 rows; warn for invalid legacy values.
  chan <- events$selection_channel
  bad_chan_v2 <- v2 & (is.na(chan) | !(chan %in% allowed_channel))
  if (any(bad_chan_v2)) {
    stop(
      "manual_curated_v2 events require a valid selection_channel: ",
      paste(utils::head(events$event_id[bad_chan_v2], 5), collapse = ", "),
      call. = FALSE
    )
  }
  bad_chan_legacy <- !v2 & !is.na(chan) & nzchar(chan) & !(chan %in% allowed_channel)
  if (any(bad_chan_legacy)) {
    warnings <- c(warnings, sprintf(
      "Legacy events have unrecognized selection_channel values: %s.",
      paste(utils::head(events$event_id[bad_chan_legacy], 5), collapse = ", ")
    ))
  }

  # Score fields: integer-like within [0, 3]. Hard fail for v2, warn for legacy.
  for (score in score_fields) {
    values <- events[[score]]
    out_of_range <- !is.na(values) & (values < 0L | values > 3L)
    missing_v2 <- v2 & is.na(values)
    if (any(v2 & out_of_range) || any(missing_v2)) {
      stop(
        sprintf(
          "manual_curated_v2 events require %s as an integer in [0, 3]: %s",
          score,
          paste(utils::head(events$event_id[(v2 & out_of_range) | missing_v2], 5), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (any(!v2 & out_of_range)) {
      warnings <- c(warnings, sprintf(
        "Legacy events have %s outside [0, 3]: %s.",
        score,
        paste(utils::head(events$event_id[!v2 & out_of_range], 5), collapse = ", ")
      ))
    }
  }

  # include_in_core_catalogue: boolean-like (NA allowed for legacy, required for v2).
  if (any(v2 & is.na(events$include_in_core_catalogue))) {
    stop(
      "manual_curated_v2 events require a boolean include_in_core_catalogue: ",
      paste(
        utils::head(events$event_id[v2 & is.na(events$include_in_core_catalogue)], 5),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  warnings
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

  curation_warnings <- validateEventCurationFields(events)
  for (msg in curation_warnings) {
    warning(msg, call. = FALSE)
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

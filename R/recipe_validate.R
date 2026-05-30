formatLinkedCountryLabels <- function(linked_ids, countries = NULL) {
  if (length(linked_ids) == 0) {
    return(character(0))
  }
  if (is.null(countries) || nrow(countries) == 0) {
    return(linked_ids)
  }
  labels <- countries |>
    dplyr::filter(.data$country_id %in% .env$linked_ids) |>
    dplyr::pull(.data$country_name)
  missing <- setdiff(linked_ids, countries$country_id)
  c(labels, missing)
}

checkEventCountryCompatibility <- function(
  country_id,
  event,
  event_countries = NULL,
  countries = NULL,
  country_name = NULL,
  event_name = NULL
) {
  country_label <- country_name %||% country_id
  target_event_id <- event$event_id[[1]]
  target_event_name <- event_name %||% event$event_name[[1]]
  target_event_scope <- event$event_scope[[1]]
  target_cross_country_allowed <- isTRUE(event$cross_country_allowed[[1]])
  event_label <- target_event_name

  if (identical(target_event_scope, "global")) {
    return(list(compatible = TRUE, level = "ok", message = character(0)))
  }

  linked <- character(0)
  if (!is.null(event_countries) && nrow(event_countries) > 0) {
    linked <- event_countries |>
      dplyr::filter(.data$event_id == .env$target_event_id) |>
      dplyr::pull(.data$country_id)
  }

  if (country_id %in% linked) {
    return(list(compatible = TRUE, level = "ok", message = character(0)))
  }

  if (isTRUE(target_cross_country_allowed)) {
    return(list(
      compatible = TRUE,
      level = "warning",
      message = sprintf(
        paste(
          "\"%s\" is not directly linked to %s.",
          "Cross-country comparison is allowed, but interpret the result carefully."
        ),
        event_label,
        country_label
      )
    ))
  }

  has_mapping_table <- !is.null(event_countries) && nrow(event_countries) > 0

  if (length(linked) > 0) {
    linked_labels <- formatLinkedCountryLabels(linked, countries = countries)
    linked_names <- paste(linked_labels, collapse = ", ")
    return(list(
      compatible = FALSE,
      level = "error",
      message = sprintf(
        paste(
          "\"%s\" is not linked to %s.",
          "Choose one of: %s, or pick a global event."
        ),
        event_label,
        country_label,
        linked_names
      )
    ))
  }

  if (has_mapping_table && identical(target_event_scope, "national")) {
    return(list(
      compatible = FALSE,
      level = "error",
      message = sprintf(
        "\"%s\" is a national event with no configured country links, so %s cannot be used.",
        event_label,
        country_label
      )
    ))
  }

  if (!has_mapping_table) {
    return(list(
      compatible = TRUE,
      level = "warning",
      message = sprintf(
        paste(
          "Country links are not loaded, so compatibility of \"%s\" with %s",
          "was not verified."
        ),
        event_label,
        country_label
      )
    ))
  }

  list(
    compatible = FALSE,
    level = "error",
    message = sprintf(
      "\"%s\" is not linked to %s. Pick a linked country or a global event.",
      event_label,
      country_label
    )
  )
}

isEventPrimaryForCountry <- function(
  event_id,
  event_scope,
  country_id,
  event_countries = NULL
) {
  if (is.null(country_id) || !nzchar(country_id)) {
    return(FALSE)
  }
  if (!is.null(event_countries) && nrow(event_countries) > 0) {
    linked <- event_countries |>
      dplyr::filter(
        .data$event_id == .env$event_id,
        .data$country_id == .env$country_id
      )
    if (nrow(linked) > 0) {
      return(TRUE)
    }
  }
  if (identical(event_scope, "national")) {
    prefix <- sub("_.*", "", event_id)
    if (identical(prefix, country_id)) {
      return(TRUE)
    }
  }
  FALSE
}

eventListTier <- function(event_scope, is_primary) {
  if (isTRUE(is_primary)) {
    return(1L)
  }
  if (identical(event_scope, "global")) {
    return(2L)
  }
  3L
}

sortEventsForCountry <- function(events, country_id, event_countries = NULL) {
  if (is.null(country_id) || !nzchar(country_id) || nrow(events) == 0) {
    return(events[0, , drop = FALSE])
  }

  primary <- vapply(seq_len(nrow(events)), function(i) {
    isEventPrimaryForCountry(
      event_id = events$event_id[[i]],
      event_scope = events$event_scope[[i]],
      country_id = country_id,
      event_countries = event_countries
    )
  }, logical(1))

  tier <- vapply(seq_len(nrow(events)), function(i) {
    eventListTier(events$event_scope[[i]], primary[[i]])
  }, integer(1))

  events |>
    dplyr::mutate(.tier = tier) |>
    dplyr::arrange(
      .data$.tier,
      dplyr::desc(.data$start_year),
      .data$event_name
    ) |>
    dplyr::select(-".tier")
}

filterCompatibleEvents <- function(events, country_id, event_countries = NULL) {
  if (is.null(country_id) || !nzchar(country_id) || nrow(events) == 0) {
    return(events[0, , drop = FALSE])
  }

  keep <- vapply(seq_len(nrow(events)), function(i) {
    checkEventCountryCompatibility(
      country_id = country_id,
      event = events[i, , drop = FALSE],
      event_countries = event_countries,
      countries = NULL
    )$compatible
  }, logical(1))

  sortEventsForCountry(
    events = events[keep, , drop = FALSE],
    country_id = country_id,
    event_countries = event_countries
  )
}

filterCompatibleCountries <- function(countries, event_id, events, event_countries = NULL) {
  if (is.null(event_id) || !nzchar(event_id) || nrow(countries) == 0) {
    return(countries[0, , drop = FALSE])
  }

  selected_event_id <- event_id
  event <- events |>
    dplyr::filter(.data$event_id == .env$selected_event_id) |>
    dplyr::slice(1)
  if (nrow(event) == 0) {
    return(countries[0, , drop = FALSE])
  }

  keep <- vapply(seq_len(nrow(countries)), function(i) {
    checkEventCountryCompatibility(
      country_id = countries$country_id[[i]],
      event = event,
      event_countries = event_countries,
      countries = countries,
      country_name = countries$country_name[[i]]
    )$compatible
  }, logical(1))

  countries[keep, , drop = FALSE]
}

assessRecipe <- function(
  recipe,
  countries,
  events,
  age_groups,
  event_countries = NULL,
  max_queries = 4L
) {
  errors <- character(0)
  warnings <- character(0)

  required_fields <- c(
    "query_id", "country_id", "sex", "age_group_id", "event_id",
    "event_mode", "operator", "metric"
  )
  missing_fields <- setdiff(required_fields, names(recipe))
  if (length(missing_fields) > 0) {
    errors <- c(
      errors,
      sprintf("Query is incomplete: missing %s.", paste(missing_fields, collapse = ", "))
    )
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  if (!recipe$country_id %in% countries$country_id) {
    errors <- c(errors, "Choose a country from the list.")
  }
  if (!recipe$event_id %in% events$event_id) {
    errors <- c(errors, "Choose an event from the list.")
  }
  if (!recipe$sex %in% c("all", "male", "female")) {
    errors <- c(errors, "Sex must be all, male, or female.")
  }
  if (!recipe$age_group_id %in% age_groups$age_group_id) {
    errors <- c(errors, "Choose a valid age group.")
  }
  if (identical(recipe$age_group_id, "custom")) {
    if (is.null(recipe$custom_age_min) || is.null(recipe$custom_age_max)) {
      errors <- c(errors, "Enter both minimum and maximum age for a custom range.")
    } else if (recipe$custom_age_min > recipe$custom_age_max) {
      errors <- c(errors, "Minimum age must not exceed maximum age.")
    }
  }
  if (!recipe$event_mode %in% c("start", "period", "peak")) {
    errors <- c(errors, "Event timing must be start, period, or peak.")
  }
  if (!recipe$operator %in% c(
    "experienced", "not_experienced", "alive_during_event", "born_after_event"
  )) {
    errors <- c(errors, "Choose how the cohort relates to the event.")
  }
  if (!recipe$metric %in% c("count", "share_total_population")) {
    errors <- c(errors, "Metric must be count or share of total population.")
  }
  if (!is.null(recipe$query_count) && recipe$query_count > max_queries) {
    errors <- c(errors, "You can compare at most 4 lines at once.")
  }

  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = unique(errors), warnings = unique(warnings)))
  }

  event <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1)
  country_row <- countries |>
    dplyr::filter(.data$country_id == recipe$country_id) |>
    dplyr::slice(1)

  compat <- checkEventCountryCompatibility(
    country_id = recipe$country_id,
    event = event,
    event_countries = event_countries,
    countries = countries,
    country_name = country_row$country_name,
    event_name = event$event_name
  )
  if (!compat$compatible) {
    errors <- c(errors, compat$message)
  } else if (compat$level == "warning" && nzchar(compat$message)) {
    warnings <- c(warnings, compat$message)
  }

  if (is.na(event$peak_year) || !event$peak_year %in% event$start_year:event$end_year) {
    warnings <- c(
      warnings,
      sprintf(
        "Peak year for \"%s\" is missing or outside the event interval; start year is used where needed.",
        event$event_name
      )
    )
  }

  list(
    valid = length(errors) == 0,
    errors = unique(errors),
    warnings = unique(warnings)
  )
}

validateRecipe <- function(
  recipe,
  countries,
  events,
  age_groups,
  event_countries = NULL,
  max_queries = 4L
) {
  assessment <- assessRecipe(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups,
    event_countries = event_countries,
    max_queries = max_queries
  )
  if (!assessment$valid) {
    stop(paste(assessment$errors, collapse = " "))
  }
  invisible(TRUE)
}

queryBuilderSexChoices <- function() {
  c("People" = "all", "Men" = "male", "Women" = "female")
}

queryBuilderOperatorChoices <- function() {
  c(
    "experienced" = "experienced",
    "did not experience" = "not_experienced",
    "were alive during" = "alive_during_event",
    "were born after the end of" = "born_after_event"
  )
}

queryBuilderEventModeChoices <- function() {
  c(
    "when it began" = "start",
    "over the full event period" = "period",
    "at its peak year" = "peak"
  )
}

queryBuilderChoiceLabel <- function(choices, value) {
  idx <- match(value, choices, nomatch = 0L)
  if (idx < 1L) {
    return(as.character(value))
  }
  names(choices)[idx]
}

buildQuerySentencePreview <- function(recipe, countries, events, age_groups) {
  sex_label <- queryBuilderChoiceLabel(queryBuilderSexChoices(), recipe$sex)
  country_name <- countries |>
    dplyr::filter(.data$country_id == recipe$country_id) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$country_name)
  event_name <- events |>
    dplyr::filter(.data$event_id == recipe$event_id) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$event_name)
  age_label <- age_groups |>
    dplyr::filter(.data$age_group_id == recipe$age_group_id) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$age_label)
  if (identical(recipe$age_group_id, "custom")) {
    age_label <- sprintf("ages %s–%s", recipe$custom_age_min, recipe$custom_age_max)
  }

  operator_label <- queryBuilderChoiceLabel(queryBuilderOperatorChoices(), recipe$operator)
  mode_label <- queryBuilderChoiceLabel(queryBuilderEventModeChoices(), recipe$event_mode)

  sprintf(
    "%s in %s who were %s and %s %s (%s).",
    sex_label,
    country_name,
    age_label,
    operator_label,
    event_name,
    mode_label
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

validate_recipe <- validateRecipe
assess_recipe <- assessRecipe

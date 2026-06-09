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
  flags <- eventPrimaryFlagsForCountry(
    events = tibble::tibble(
      event_id = event_id,
      event_scope = event_scope
    ),
    country_id = country_id,
    event_countries = event_countries
  )
  isTRUE(flags[[1]])
}

eventPrimaryFlagsForCountry <- function(events, country_id, event_countries = NULL) {
  n <- nrow(events)
  if (n == 0L || is.null(country_id) || length(country_id) != 1L || !nzchar(country_id)) {
    return(logical(0))
  }

  flags <- rep(FALSE, n)
  if (!is.null(event_countries) && nrow(event_countries) > 0L) {
    linked_ids <- event_countries$event_id[event_countries$country_id == country_id]
    if (length(linked_ids) > 0L) {
      flags <- events$event_id %in% linked_ids
    }
  }

  national <- events$event_scope == "national"
  prefix <- paste0(country_id, "_")
  flags | (national & startsWith(events$event_id, prefix))
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

  primary <- eventPrimaryFlagsForCountry(events, country_id, event_countries)
  tier <- ifelse(primary, 1L, ifelse(events$event_scope == "global", 2L, 3L))

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
  if (is.null(country_id) || length(country_id) == 0L || !nzchar(country_id) || nrow(events) == 0) {
    return(events[0, , drop = FALSE])
  }

  has_mapping_table <- !is.null(event_countries) && nrow(event_countries) > 0L

  if (!has_mapping_table) {
    matched <- events
  } else {
    linked_event_ids <- unique(
      event_countries$event_id[event_countries$country_id == country_id]
    )
    events_with_links <- unique(event_countries$event_id)

    is_global <- events$event_scope == "global"
    is_linked <- events$event_id %in% linked_event_ids
    is_cross <- !is.na(events$cross_country_allowed) & events$cross_country_allowed == TRUE
    is_national <- events$event_scope == "national"
    has_links <- events$event_id %in% events_with_links

    keep <- is_global | is_linked | is_cross
    keep <- keep & !(is_national & !has_links)
    blocked <- has_links & !is_linked & !is_global & !is_cross
    keep <- keep & !blocked

    matched <- events[keep, , drop = FALSE]
  }

  sortEventsForCountry(
    events = matched,
    country_id = country_id,
    event_countries = event_countries
  )
}

filterCompatibleCountries <- function(countries, event_id, events, event_countries = NULL) {
  if (is.null(event_id) || length(event_id) == 0L || !nzchar(event_id) || nrow(countries) == 0) {
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
    "query_id", "country_id", "sex", "age_status_id", "is_complement",
    "event_id", "event_mode", "metric"
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
  age_status_choices <- queryBuilderAgeStatusValues(age_groups)
  if (!recipe$age_status_id %in% age_status_choices) {
    errors <- c(errors, "Choose a valid age status.")
  }
  age_modifier <- recipeAgeModifier(recipe)
  if (!age_modifier %in% queryBuilderAgeModifierValues()) {
    errors <- c(errors, "Age modifier must be none, not, younger_than, or older_than.")
  }
  if (!is.logical(recipe$is_complement) || length(recipe$is_complement) != 1L || is.na(recipe$is_complement)) {
    errors <- c(errors, "Complement flag must be TRUE or FALSE.")
  }
  if (recipe$age_status_id %in% c("alive", "not_born_yet") && age_modifier %in% c("younger_than", "older_than")) {
    errors <- c(errors, "Younger than and older than are only available for age ranges.")
  }
  if (identical(recipe$age_status_id, "custom")) {
    if (is.null(recipe$custom_age_min) || is.null(recipe$custom_age_max)) {
      errors <- c(errors, "Enter both minimum and maximum age for a custom range.")
    } else if (recipe$custom_age_min > recipe$custom_age_max) {
      errors <- c(errors, "Minimum age must not exceed maximum age.")
    }
  }
  if (!recipe$event_mode %in% c("start", "end", "period", "peak")) {
    errors <- c(errors, "Event timing must be start, end, period, or peak.")
  }
  if (!recipe$metric %in% c("count", "share_total_population", "share_working_age_population")) {
    errors <- c(
      errors,
      "Metric must be count, share of total population, or share of working-age population."
    )
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

  if (isCompositeEvent(event) && "country_id" %in% names(event) && !is.na(event$country_id) && nzchar(event$country_id)) {
    if (!identical(recipe$country_id, event$country_id)) {
      errors <- c(
        errors,
        sprintf(
          "Composite event \"%s\" is defined for %s; choose that country in the recipe.",
          event$event_name,
          event$country_id
        )
      )
    }
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
  composite_members = NULL,
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

queryBuilderAgeModifierValues <- function() {
  c("none", "not", "younger_than", "older_than")
}

recipeAgeModifier <- function(recipe) {
  if (!is.null(recipe$age_modifier) && length(recipe$age_modifier) == 1L && !is.na(recipe$age_modifier)) {
    age_modifier <- as.character(recipe$age_modifier)
    if (!identical(age_modifier, "none") || !isTRUE(recipe$is_complement)) {
      return(age_modifier)
    }
  }
  if (isTRUE(recipe$is_complement)) {
    return("not")
  }
  "none"
}

queryBuilderComplementChoices <- function() {
  c(" " = "none", "not" = "not", "younger than" = "younger_than", "older than" = "older_than")
}

queryBuilderAgeStatusValues <- function(age_groups) {
  c(age_groups$age_group_id, "alive", "not_born_yet")
}

formatAgeStatusLabel <- function(age_status_id, age_groups) {
  if (identical(age_status_id, "alive")) {
    return("Alive")
  }
  if (identical(age_status_id, "not_born_yet")) {
    return("Not Born Yet")
  }

  row <- age_groups |>
    dplyr::filter(.data$age_group_id == .env$age_status_id) |>
    dplyr::slice(1)
  if (nrow(row) == 0) {
    return(age_status_id)
  }
  if (identical(age_status_id, "custom")) {
    return("Custom Age")
  }
  age_min <- as.integer(row$age_min)
  age_max <- as.integer(row$age_max)
  if (is.na(age_max)) {
    return(sprintf("%s (%s+)", row$age_label, age_min))
  }
  sprintf("%s (%s-%s)", row$age_label, age_min, age_max)
}

queryBuilderAgeStatusChoices <- function(age_groups) {
  ordered_age_groups <- dplyr::bind_rows(
    age_groups |> dplyr::filter(.data$age_group_id != "custom"),
    age_groups |> dplyr::filter(.data$age_group_id == "custom")
  )
  labels <- vapply(
    ordered_age_groups$age_group_id,
    formatAgeStatusLabel,
    character(1),
    age_groups = ordered_age_groups
  )
  c(
    stats::setNames(
      ordered_age_groups$age_group_id[ordered_age_groups$age_group_id != "custom"],
      labels[ordered_age_groups$age_group_id != "custom"]
    ),
    "Alive" = "alive",
    "Not Born Yet" = "not_born_yet",
    stats::setNames(
      ordered_age_groups$age_group_id[ordered_age_groups$age_group_id == "custom"],
      labels[ordered_age_groups$age_group_id == "custom"]
    )
  )
}

queryBuilderEventModeChoices <- function() {
  c(
    "at the beginning of" = "start",
    "at the end of" = "end",
    "during" = "period",
    "at the peak of" = "peak"
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
  age_label <- formatAgeStatusLabel(recipe$age_status_id, age_groups)
  if (identical(recipe$age_status_id, "custom")) {
    age_label <- sprintf("ages %s-%s", recipe$custom_age_min, recipe$custom_age_max)
  }

  age_modifier <- recipeAgeModifier(recipe)
  modifier_label <- queryBuilderChoiceLabel(queryBuilderComplementChoices(), age_modifier)
  modifier_text <- if (identical(age_modifier, "none")) "" else paste0(" ", modifier_label)
  mode_label <- queryBuilderChoiceLabel(queryBuilderEventModeChoices(), recipe$event_mode)

  sprintf(
    "%s in %s who were%s %s %s %s.",
    sex_label,
    country_name,
    modifier_text,
    age_label,
    mode_label,
    event_name
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

validate_recipe <- validateRecipe
assess_recipe <- assessRecipe

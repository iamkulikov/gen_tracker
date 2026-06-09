encodeRecipe <- function(recipe) {
  parts <- c(
    sprintf("country=%s", recipe$country_id),
    sprintf("sex=%s", recipe$sex),
    sprintf("age_status=%s", recipe$age_status_id),
    sprintf("age_modifier=%s", recipeAgeModifier(recipe)),
    sprintf("complement=%s", isTRUE(recipe$is_complement)),
    sprintf("age_min=%s", dplyr::coalesce(as.character(recipe$custom_age_min), "")),
    sprintf("age_max=%s", dplyr::coalesce(as.character(recipe$custom_age_max), "")),
    sprintf("event=%s", recipe$event_id),
    sprintf("mode=%s", recipe$event_mode),
    sprintf("metric=%s", recipe$metric)
  )
  paste0("GEN2:", paste(parts, collapse = ";"))
}

decodeRecipe <- function(recipe_code) {
  if (!startsWith(recipe_code, "GEN2:")) {
    stop("Unsupported recipe code version. Expected GEN2.")
  }

  payload <- sub("^GEN2:", "", recipe_code)
  kv <- strsplit(payload, ";", fixed = TRUE)[[1]]
  parsed <- stats::setNames(
    object = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[2], character(1)),
    nm = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[1], character(1))
  )

  age_modifier <- parsed[["age_modifier"]]
  if (is.null(age_modifier) || is.na(age_modifier) || !nzchar(age_modifier)) {
    age_modifier <- if (identical(parsed[["complement"]], "TRUE")) {
      "not"
    } else {
      "none"
    }
  }

  list(
    country_id = parsed[["country"]],
    sex = parsed[["sex"]],
    age_status_id = parsed[["age_status"]],
    age_modifier = age_modifier,
    is_complement = identical(age_modifier, "not"),
    custom_age_min = suppressWarnings(as.integer(parsed[["age_min"]])),
    custom_age_max = suppressWarnings(as.integer(parsed[["age_max"]])),
    event_id = parsed[["event"]],
    event_mode = parsed[["mode"]],
    metric = parsed[["metric"]]
  )
}

chartNarrativeMetricPhrase <- function(metric) {
  if (metric == "share_total_population") {
    return("Each point shows what share of the country's total population the line represents.")
  }
  if (metric == "share_working_age_population") {
    return(
      paste(
        "Each point shows what share of the country's working-age population (ages 15–64)",
        "the line represents among people currently in that age range."
      )
    )
  }
  if (metric == "count" && isTRUE(populationCountStoredInThousands())) {
    return("Each point counts people in thousands.")
  }
  "Each point shows the population count for the selected group."
}

chartNarrativeLineMeaning <- function(recipe) {
  if (isTRUE(recipe$is_complement)) {
    return(
      paste(
        "The line tracks people who did not match the age condition at the event reference year",
        "— the complement of the highlighted cohort."
      )
    )
  }
  if (identical(recipe$age_status_id, "not_born_yet")) {
    return(
      paste(
        "The line follows people born after the event reference.",
        "It starts near zero and rises as those birth cohorts enter the population."
      )
    )
  }
  if (identical(recipe$age_status_id, "alive")) {
    return(
      paste(
        "The line shows people who were already alive at the event reference.",
        "It stays at zero until that year, then follows that living cohort over time."
      )
    )
  }
  paste(
    "The line shows people who were in the selected age band when the event is measured.",
    "It stays at zero until the event year, then traces that experience cohort over time."
  )
}

chartNarrativeBirthYearsPhrase <- function(recipe, event, age_groups, population) {
  if (is.null(population) || nrow(population) == 0) {
    return(NULL)
  }

  age_range <- resolveAgeRange(recipe, age_groups)
  birth_years <- resolveBirthYears(
    recipe = recipe,
    event = event,
    age_range = age_range,
    population_years = population$year,
    population_ages = population$age
  )
  birth_years_text <- formatBirthYearRange(birth_years)
  if (!nzchar(birth_years_text) || identical(birth_years_text, "none")) {
    return(NULL)
  }

  sprintf("Included birth years are roughly %s.", birth_years_text)
}

buildChartNarrative <- function(
  recipe,
  event,
  countries,
  events,
  age_groups,
  population = NULL
) {
  sentence <- buildQuerySentencePreview(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups
  )

  parts <- c(
    sentence,
    chartNarrativeLineMeaning(recipe),
    chartNarrativeMetricPhrase(recipe$metric),
    chartNarrativeBirthYearsPhrase(recipe, event, age_groups, population)
  )
  parts <- parts[!vapply(parts, function(x) is.null(x) || !nzchar(x), logical(1))]
  paste(parts, collapse = "\n\n")
}

buildRecipeDescriptionTechnical <- function(
  recipe,
  event,
  country_name,
  age_label,
  countries,
  events,
  age_groups,
  population = NULL
) {
  sentence <- buildQuerySentencePreview(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups
  )

  age_range <- resolveAgeRange(recipe, age_groups)
  age_detail <- formatAgeRangeDetail(age_range)
  mode_label <- queryBuilderChoiceLabel(queryBuilderEventModeChoices(), recipe$event_mode)
  event_years <- formatEventYearRange(event$start_year, event$end_year)
  metric_label <- formatMetricLabel(recipe$metric)
  relation_label <- if (isTRUE(recipe$is_complement)) {
    sprintf("complement of %s", recipe$age_status_id)
  } else if (recipeAgeModifier(recipe) %in% c("younger_than", "older_than")) {
    sprintf(
      "%s %s",
      queryBuilderChoiceLabel(queryBuilderComplementChoices(), recipeAgeModifier(recipe)),
      recipe$age_status_id
    )
  } else {
    recipe$age_status_id
  }

  birth_years_text <- "not computed"
  if (!is.null(population) && nrow(population) > 0) {
    birth_years <- resolveBirthYears(
      recipe = recipe,
      event = event,
      age_range = age_range,
      population_years = population$year,
      population_ages = population$age
    )
    birth_years_text <- formatBirthYearRange(birth_years)
  }

  paste(
    c(
      sentence,
      sprintf("Age band at reference: %s.", age_detail),
      sprintf(
        "Birth years included: %s (%s, %s).",
        birth_years_text,
        relation_label,
        mode_label
      ),
      sprintf("Event: %s (%s).", event$event_name, event_years),
      sprintf(
        paste(
          "Methodology: the recipe uses age_status_id=%s, age_modifier=%s,",
          "and is_complement=%s; positive age and alive cohorts are zero before",
          "the event threshold, while not_born_yet cohorts emerge naturally after",
          "the reference year."
        ),
        recipe$age_status_id,
        recipeAgeModifier(recipe),
        isTRUE(recipe$is_complement)
      ),
      sprintf("Metric: %s.", metric_label)
    ),
    collapse = " · "
  )
}

buildRecipeDescription <- function(
  recipe,
  event,
  country_name,
  age_label,
  countries,
  events,
  age_groups,
  population = NULL
) {
  buildRecipeDescriptionTechnical(
    recipe = recipe,
    event = event,
    country_name = country_name,
    age_label = age_label,
    countries = countries,
    events = events,
    age_groups = age_groups,
    population = population
  )
}

buildPlotLegendLabel <- function(recipe, country_name, event_name) {
  sex_label <- queryBuilderChoiceLabel(queryBuilderSexChoices(), recipe$sex)
  sex_short <- if (identical(recipe$sex, "all")) {
    "All"
  } else {
    substr(sex_label, 1, 1)
  }
  sprintf("%s · %s · %s", sex_short, country_name, event_name)
}

encode_recipe <- encodeRecipe
decode_recipe <- decodeRecipe

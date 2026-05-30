encodeRecipe <- function(recipe) {
  parts <- c(
    sprintf("country=%s", recipe$country_id),
    sprintf("sex=%s", recipe$sex),
    sprintf("age_group=%s", recipe$age_group_id),
    sprintf("age_min=%s", dplyr::coalesce(as.character(recipe$custom_age_min), "")),
    sprintf("age_max=%s", dplyr::coalesce(as.character(recipe$custom_age_max), "")),
    sprintf("event=%s", recipe$event_id),
    sprintf("mode=%s", recipe$event_mode),
    sprintf("operator=%s", recipe$operator),
    sprintf("metric=%s", recipe$metric)
  )
  paste0("GEN1:", paste(parts, collapse = ";"))
}

decodeRecipe <- function(recipe_code) {
  if (!startsWith(recipe_code, "GEN1:")) {
    stop("Unsupported recipe code version. Expected GEN1.")
  }

  payload <- sub("^GEN1:", "", recipe_code)
  kv <- strsplit(payload, ";", fixed = TRUE)[[1]]
  parsed <- stats::setNames(
    object = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[2], character(1)),
    nm = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[1], character(1))
  )

  list(
    country_id = parsed[["country"]],
    sex = parsed[["sex"]],
    age_group_id = parsed[["age_group"]],
    custom_age_min = suppressWarnings(as.integer(parsed[["age_min"]])),
    custom_age_max = suppressWarnings(as.integer(parsed[["age_max"]])),
    event_id = parsed[["event"]],
    event_mode = parsed[["mode"]],
    operator = parsed[["operator"]],
    metric = parsed[["metric"]]
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
  sentence <- buildQuerySentencePreview(
    recipe = recipe,
    countries = countries,
    events = events,
    age_groups = age_groups
  )

  age_range <- resolveAgeRange(recipe, age_groups)
  age_detail <- formatAgeRangeDetail(age_range)
  mode_label <- queryBuilderChoiceLabel(queryBuilderEventModeChoices(), recipe$event_mode)
  operator_label <- queryBuilderChoiceLabel(queryBuilderOperatorChoices(), recipe$operator)
  event_years <- formatEventYearRange(event$start_year, event$end_year)
  metric_label <- formatMetricLabel(recipe$metric)

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
        operator_label,
        mode_label
      ),
      sprintf("Event: %s (%s).", event$event_name, event_years),
      sprintf("Metric: %s.", metric_label)
    ),
    collapse = " · "
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

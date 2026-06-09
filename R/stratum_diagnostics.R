shouldAssessStratumGrowth <- function(recipe) {
  if (identical(recipe$age_status_id, "not_born_yet")) {
    return(FALSE)
  }
  if (identical(recipeAgeModifier(recipe), "not")) {
    return(FALSE)
  }
  if (isTRUE(recipe$is_complement)) {
    return(FALSE)
  }
  TRUE
}

stratumGrowthWarning <- function(
  series,
  recipe,
  event,
  composite_members = NULL,
  events = NULL
) {
  if (!shouldAssessStratumGrowth(recipe)) {
    return(character(0))
  }
  if (is.null(series) || nrow(series) == 0) {
    return(character(0))
  }

  threshold <- resolveEventThreshold(event, recipe$event_mode)
  legitimate_years <- resolveLegitimateGrowthYears(
    recipe = recipe,
    event = event,
    composite_members = composite_members,
    events = events
  )
  observed <- series |>
    dplyr::filter(
      .data$year >= .env$threshold,
      is.finite(.data$stratum_population),
      .data$stratum_population > 0
    ) |>
    dplyr::arrange(.data$year)

  if (nrow(observed) < 2L) {
    return(character(0))
  }

  prev <- observed$stratum_population[-nrow(observed)]
  next_val <- observed$stratum_population[-1L]
  increases <- next_val > prev
  if (!any(increases)) {
    return(character(0))
  }

  rel <- (next_val - prev) / pmax(prev, 1e-9)
  abs_inc <- next_val - prev
  scale_floor <- max(1, 0.01 * max(observed$stratum_population, na.rm = TRUE))
  material <- increases & (rel >= 0.02 | abs_inc >= scale_floor)
  if (!any(material)) {
    return(character(0))
  }

  material_idx <- which(material)
  step_years <- observed$year[material_idx + 1L]
  suspicious_idx <- material_idx[!step_years %in% legitimate_years]
  if (length(suspicious_idx) == 0L) {
    return(character(0))
  }

  first_hit <- suspicious_idx[1L]
  year_from <- observed$year[first_hit]
  year_to <- observed$year[first_hit + 1L]

  paste0(
    "Stratum population increases between ",
    year_from,
    " and ",
    year_to,
    " after the event threshold. This may reflect migration, data revisions, or methodology rather than cohort growth."
  )
}

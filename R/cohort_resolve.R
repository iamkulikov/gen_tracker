resolveAgeRange <- function(recipe, age_groups) {
  if (recipe$age_group_id == "custom") {
    age_min <- as.integer(recipe$custom_age_min)
    age_max <- as.integer(recipe$custom_age_max)
    age_label <- sprintf("%s-%s", age_min, age_max)
  } else {
    row <- age_groups |> dplyr::filter(.data$age_group_id == recipe$age_group_id) |> dplyr::slice(1)
    age_min <- as.integer(row$age_min)
    age_max <- as.integer(row$age_max)
    if (is.na(age_max)) {
      age_max <- 120L
    }
    age_label <- row$age_label
  }

  list(age_min = age_min, age_max = age_max, age_label = age_label)
}

resolveEventYears <- function(event, event_mode) {
  if (event_mode == "start") {
    return(as.integer(event$start_year))
  }
  if (event_mode == "period") {
    return(seq.int(as.integer(event$start_year), as.integer(event$end_year)))
  }
  if (event_mode == "peak") {
    return(as.integer(dplyr::coalesce(event$peak_year, event$start_year)))
  }

  stop("Unsupported event_mode.")
}

resolveBirthYears <- function(recipe, event, age_range, population_years = NULL, population_ages = NULL) {
  event_years <- resolveEventYears(event, recipe$event_mode)
  allowed_ages <- seq.int(age_range$age_min, age_range$age_max)
  matched_birth_years <- unique(as.integer(outer(event_years, allowed_ages, FUN = "-")))

  if (recipe$operator == "experienced") {
    return(sort(matched_birth_years))
  }

  if (recipe$operator == "born_after_event") {
    return(seq.int(as.integer(event$end_year) + 1L, max(population_years, na.rm = TRUE)))
  }

  if (recipe$operator == "alive_during_event") {
    max_birth_year <- as.integer(event$end_year)
    return(seq.int(min(population_years, na.rm = TRUE) - max(population_ages, na.rm = TRUE), max_birth_year))
  }

  if (recipe$operator == "not_experienced") {
    all_birth_years <- seq.int(
      min(population_years, na.rm = TRUE) - max(population_ages, na.rm = TRUE),
      max(population_years, na.rm = TRUE)
    )
    return(sort(setdiff(all_birth_years, matched_birth_years)))
  }

  stop("Unsupported operator.")
}

resolve_age_range <- resolveAgeRange
resolve_event_years <- resolveEventYears
resolve_birth_years <- resolveBirthYears

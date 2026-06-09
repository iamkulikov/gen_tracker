resolveAgeRange <- function(recipe, age_groups) {
  if (recipe$age_status_id %in% c("alive", "not_born_yet")) {
    return(list(age_min = NA_integer_, age_max = NA_integer_, age_label = recipe$age_status_id))
  }

  if (recipe$age_status_id == "custom") {
    age_min <- as.integer(recipe$custom_age_min)
    age_max <- as.integer(recipe$custom_age_max)
    age_label <- sprintf("%s-%s", age_min, age_max)
  } else {
    row <- age_groups |> dplyr::filter(.data$age_group_id == recipe$age_status_id) |> dplyr::slice(1)
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
  if (event_mode == "end") {
    return(as.integer(event$end_year))
  }
  if (event_mode == "period") {
    return(seq.int(as.integer(event$start_year), as.integer(event$end_year)))
  }
  if (event_mode == "peak") {
    return(as.integer(dplyr::coalesce(event$peak_year, event$start_year)))
  }

  stop("Unsupported event_mode.")
}

resolveEventThreshold <- function(event, event_mode) {
  if (event_mode == "period") {
    return(as.integer(event$start_year))
  }
  as.integer(resolveEventReferenceYear(event, event_mode))
}

resolveEventReferenceYear <- function(event, event_mode) {
  if (event_mode == "period") {
    return(as.integer(event$end_year))
  }
  event_years <- resolveEventYears(event, event_mode)
  as.integer(max(event_years, na.rm = TRUE))
}

resolvePopulationBirthYearUniverse <- function(population_years, population_ages) {
  seq.int(
    min(population_years, na.rm = TRUE) - max(population_ages, na.rm = TRUE),
    max(population_years, na.rm = TRUE)
  )
}

resolveAgesForModifier <- function(age_range, population_ages, age_modifier) {
  max_age <- max(population_ages, na.rm = TRUE)
  if (age_modifier == "younger_than") {
    if (age_range$age_min <= 0L) {
      return(integer(0))
    }
    return(seq.int(0L, age_range$age_min - 1L))
  }
  if (age_modifier == "older_than") {
    if (age_range$age_max >= max_age) {
      return(integer(0))
    }
    return(seq.int(age_range$age_max + 1L, max_age))
  }
  seq.int(age_range$age_min, age_range$age_max)
}

isCompositeEvent <- function(event) {
  if (nrow(event) == 0) {
    return(FALSE)
  }
  if ("event_origin" %in% names(event)) {
    origin <- event$event_origin[[1]] %||% "manual"
    if (identical(origin, "composite")) {
      return(TRUE)
    }
  }
  if ("event_id" %in% names(event)) {
    event_id <- event$event_id[[1]] %||% ""
    return(isTRUE(grepl("^MERGE_", event_id)))
  }
  FALSE
}

syntheticEventAtYear <- function(event, year) {
  year <- as.integer(year)
  out <- event[1, , drop = FALSE]
  out$start_year <- year
  out$end_year <- year
  if ("peak_year" %in% names(out)) {
    out$peak_year <- year
  }
  out
}

recipeForEpisodeResolution <- function(recipe) {
  if (identical(recipe$event_mode, "period")) {
    episode_recipe <- recipe
    episode_recipe$event_mode <- "start"
    return(episode_recipe)
  }
  recipe
}

isMultiYearPeriod <- function(event, event_mode = "period") {
  if (!identical(event_mode, "period") || nrow(event) == 0) {
    return(FALSE)
  }
  start_y <- as.integer(event$start_year[[1]])
  end_y <- as.integer(dplyr::coalesce(event$end_year[[1]], start_y))
  start_y < end_y
}

resolveEventEpisodes <- function(event, event_mode, composite_members = NULL, events = NULL) {
  empty <- tibble::tibble(
    episode_index = integer(),
    start_year = integer(),
    end_year = integer(),
    peak_year = integer(),
    reference_year = integer(),
    threshold = integer(),
    member_event_id = character()
  )

  if (!isCompositeEvent(event)) {
    if (isMultiYearPeriod(event, event_mode)) {
      start_y <- as.integer(event$start_year[[1]])
      end_y <- as.integer(dplyr::coalesce(event$end_year[[1]], start_y))
      years <- seq.int(start_y, end_y)
      episode_rows <- vector("list", length(years))
      for (i in seq_along(years)) {
        year <- years[[i]]
        episode_rows[[i]] <- tibble::tibble(
          episode_index = as.integer(i),
          start_year = year,
          end_year = year,
          peak_year = year,
          reference_year = year,
          threshold = year,
          member_event_id = as.character(event$event_id[[1]])
        )
      }
      return(dplyr::bind_rows(episode_rows))
    }

    ref <- resolveEventReferenceYear(event, event_mode)
    thr <- resolveEventThreshold(event, event_mode)
    return(tibble::tibble(
      episode_index = 1L,
      start_year = as.integer(event$start_year),
      end_year = as.integer(dplyr::coalesce(event$end_year, event$start_year)),
      peak_year = as.integer(dplyr::coalesce(event$peak_year, event$start_year)),
      reference_year = ref,
      threshold = thr,
      member_event_id = as.character(event$event_id)
    ))
  }

  if (is.null(composite_members) || is.null(events) || nrow(composite_members) == 0) {
    return(empty)
  }

  member_ids <- composite_members |>
    dplyr::filter(.data$composite_event_id == event$event_id[[1]]) |>
    dplyr::pull(.data$member_event_id)

  member_events <- events |>
    dplyr::filter(.data$event_id %in% member_ids) |>
    dplyr::arrange(.data$start_year, .data$event_id)

  if (nrow(member_events) == 0) {
    return(empty)
  }

  episode_rows <- vector("list", nrow(member_events))
  for (i in seq_len(nrow(member_events))) {
    member_row <- member_events[i, , drop = FALSE]
    episode_rows[[i]] <- tibble::tibble(
      episode_index = as.integer(i),
      start_year = as.integer(member_row$start_year[[1]]),
      end_year = as.integer(dplyr::coalesce(member_row$end_year[[1]], member_row$start_year[[1]])),
      peak_year = as.integer(dplyr::coalesce(member_row$peak_year[[1]], member_row$start_year[[1]])),
      reference_year = resolveEventReferenceYear(member_row, event_mode),
      threshold = resolveEventThreshold(member_row, event_mode),
      member_event_id = as.character(member_row$event_id[[1]])
    )
  }
  dplyr::bind_rows(episode_rows)
}

resolveBirthYearsForEvent <- function(
  recipe,
  event,
  age_range,
  population_years = NULL,
  population_ages = NULL
) {
  all_birth_years <- resolvePopulationBirthYearUniverse(population_years, population_ages)
  reference_year <- resolveEventReferenceYear(event, recipe$event_mode)
  age_modifier <- recipeAgeModifier(recipe)

  if (recipe$age_status_id == "not_born_yet") {
    matched_birth_years <- all_birth_years[all_birth_years > reference_year]
    if (identical(age_modifier, "not")) {
      matched_birth_years <- setdiff(all_birth_years, matched_birth_years)
    }
    return(sort(matched_birth_years))
  }

  if (recipe$age_status_id == "alive") {
    matched_birth_years <- all_birth_years[all_birth_years <= reference_year]
    if (identical(age_modifier, "not")) {
      matched_birth_years <- setdiff(all_birth_years, matched_birth_years)
    }
    return(sort(matched_birth_years))
  }

  event_years <- resolveEventYears(event, recipe$event_mode)
  allowed_ages <- resolveAgesForModifier(age_range, population_ages, age_modifier)
  if (length(allowed_ages) == 0L) {
    return(integer(0))
  }
  matched_birth_years <- unique(as.integer(outer(event_years, allowed_ages, FUN = "-")))

  if (identical(age_modifier, "not")) {
    return(sort(setdiff(all_birth_years, matched_birth_years)))
  }

  sort(matched_birth_years)
}

resolveBirthYears <- function(
  recipe,
  event,
  age_range,
  population_years = NULL,
  population_ages = NULL,
  composite_members = NULL,
  events = NULL
) {
  if (!isCompositeEvent(event)) {
    return(resolveBirthYearsForEvent(
      recipe = recipe,
      event = event,
      age_range = age_range,
      population_years = population_years,
      population_ages = population_ages
    ))
  }

  episodes <- resolveEventEpisodes(event, recipe$event_mode, composite_members, events)
  if (nrow(episodes) == 0 || is.null(events)) {
    return(integer(0))
  }

  union_birth_years <- integer(0)
  for (i in seq_len(nrow(episodes))) {
    member_event <- events |>
      dplyr::filter(.data$event_id == episodes$member_event_id[[i]]) |>
      dplyr::slice(1)
    if (nrow(member_event) == 0) {
      next
    }
    union_birth_years <- union(
      union_birth_years,
      resolveBirthYearsForEvent(
        recipe = recipe,
        event = member_event,
        age_range = age_range,
        population_years = population_years,
        population_ages = population_ages
      )
    )
  }

  sort(unique(union_birth_years))
}

resolveBirthYearThresholds <- function(
  recipe,
  event,
  age_range,
  population_years = NULL,
  population_ages = NULL,
  composite_members = NULL,
  events = NULL
) {
  episodes <- resolveEventEpisodes(event, recipe$event_mode, composite_members, events)
  if (nrow(episodes) == 0) {
    return(tibble::tibble(birth_year = integer(), threshold = integer()))
  }

  threshold_rows <- list()
  for (i in seq_len(nrow(episodes))) {
    if (isCompositeEvent(event)) {
      if (is.null(events)) {
        next
      }
      member_event <- events |>
        dplyr::filter(.data$event_id == episodes$member_event_id[[i]]) |>
        dplyr::slice(1)
      episode_recipe <- recipe
    } else if (isMultiYearPeriod(event, recipe$event_mode)) {
      member_event <- syntheticEventAtYear(event, episodes$threshold[[i]])
      episode_recipe <- recipeForEpisodeResolution(recipe)
    } else {
      member_event <- event
      episode_recipe <- recipe
    }
    if (nrow(member_event) == 0) {
      next
    }
    episode_birth_years <- resolveBirthYearsForEvent(
      recipe = episode_recipe,
      event = member_event,
      age_range = age_range,
      population_years = population_years,
      population_ages = population_ages
    )
    if (length(episode_birth_years) == 0) {
      next
    }
    threshold_rows[[length(threshold_rows) + 1L]] <- tibble::tibble(
      birth_year = episode_birth_years,
      threshold = episodes$threshold[[i]]
    )
  }

  if (length(threshold_rows) == 0) {
    return(tibble::tibble(birth_year = integer(), threshold = integer()))
  }

  dplyr::bind_rows(threshold_rows) |>
    dplyr::group_by(.data$birth_year) |>
    dplyr::summarise(threshold = min(.data$threshold, na.rm = TRUE), .groups = "drop")
}

usesGranularThresholdPath <- function(recipe, event) {
  if (isTRUE(recipe$is_complement)) {
    return(FALSE)
  }
  if (identical(recipe$age_status_id, "not_born_yet")) {
    return(FALSE)
  }
  if (identical(recipeAgeModifier(recipe), "not")) {
    return(FALSE)
  }
  isCompositeEvent(event) || isMultiYearPeriod(event, recipe$event_mode)
}

usesCompositeGranularPath <- usesGranularThresholdPath

resolveLegitimateGrowthYears <- function(recipe, event, composite_members = NULL, events = NULL) {
  episodes <- resolveEventEpisodes(event, recipe$event_mode, composite_members, events)
  if (nrow(episodes) == 0) {
    return(as.integer(resolveEventThreshold(event, recipe$event_mode)))
  }
  sort(unique(episodes$threshold))
}

resolve_age_range <- resolveAgeRange
resolve_event_years <- resolveEventYears
resolve_event_threshold <- resolveEventThreshold
resolve_event_reference_year <- resolveEventReferenceYear
resolve_ages_for_modifier <- resolveAgesForModifier
resolve_birth_years <- resolveBirthYears

resolvePresetEventId <- function(candidates, country_id, events) {
  if (is.null(candidates) || length(candidates) == 0) {
    return(NULL)
  }
  available <- events$event_id
  matched <- candidates[candidates %in% available]
  if (length(matched) > 0) {
    return(matched[[1]])
  }

  prefix <- paste0(country_id, "_")
  prefixed <- available[startsWith(available, prefix)]
  if (length(prefixed) > 0) {
    return(prefixed[[1]])
  }

  national <- events |>
    dplyr::filter(.data$event_scope == "national")
  if (nrow(national) > 0) {
    return(national$event_id[[1]])
  }

  if (length(available) == 0) {
    return(NULL)
  }

  available[[1]]
}

resolvePresetCountryId <- function(country_id, countries, preferred = "RUS") {
  if (!is.null(country_id) && country_id %in% countries$country_id) {
    return(country_id)
  }
  queryBuilderDefaultCountryId(countries, preferred = preferred)
}

demoQueryPresets <- function() {
  list(
    blank = list(
      label = "— Custom —",
      query_count = 1L,
      metric = "count",
      queries = list()
    ),
    afghan_school_boys = list(
      label = "School-aged men · Afghan war (Russia)",
      query_count = 1L,
      metric = "count",
      queries = list(
        list(
          query_id = "q1",
          country_id = "RUS",
          sex = "male",
          age_status_id = "school_age",
          age_modifier = "none",
          is_complement = FALSE,
          event_candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
          event_mode = "start",
          custom_age_min = 0L,
          custom_age_max = 100L
        )
      )
    ),
    afghan_share = list(
      label = "Population share · school-aged · Afghan war",
      query_count = 1L,
      metric = "share_total_population",
      queries = list(
        list(
          query_id = "q1",
          country_id = "RUS",
          sex = "all",
          age_status_id = "school_age",
          age_modifier = "none",
          is_complement = FALSE,
          event_candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
          event_mode = "start",
          custom_age_min = 0L,
          custom_age_max = 100L
        )
      )
    ),
    compare_sex = list(
      label = "Compare men vs women · Afghan war (Russia)",
      query_count = 2L,
      metric = "count",
      queries = list(
        list(
          query_id = "q1",
          country_id = "RUS",
          sex = "male",
          age_status_id = "school_age",
          age_modifier = "none",
          is_complement = FALSE,
          event_candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
          event_mode = "start",
          custom_age_min = 0L,
          custom_age_max = 100L
        ),
        list(
          query_id = "q2",
          country_id = "RUS",
          sex = "female",
          age_status_id = "school_age",
          age_modifier = "none",
          is_complement = FALSE,
          event_candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
          event_mode = "start",
          custom_age_min = 0L,
          custom_age_max = 100L
        )
      )
    )
  )
}

demoQueryPresetChoices <- function() {
  presets <- demoQueryPresets()
  stats::setNames(names(presets), vapply(presets, function(p) p$label, character(1)))
}

defaultQueryRecipe <- function(
  query_id = "q1",
  countries = NULL,
  events = NULL
) {
  country_id <- if (!is.null(countries) && nrow(countries) > 0) {
    queryBuilderDefaultCountryId(countries, "RUS")
  } else {
    "RUS"
  }
  event_id <- resolvePresetEventId(
    candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
    country_id = country_id,
    events = if (is.null(events)) {
      tibble::tibble(event_id = character(), event_scope = character())
    } else {
      events
    }
  )

  list(
    query_id = query_id,
    country_id = country_id,
    sex = "all",
    age_status_id = "adults",
    age_modifier = "none",
    is_complement = FALSE,
    event_id = event_id,
    event_mode = "start",
    custom_age_min = NA_integer_,
    custom_age_max = NA_integer_
  )
}

applyQueryBuilderInputs <- function(session, module_prefix, recipe, events, countries) {
  event_id <- if (!is.null(recipe$event_candidates)) {
    resolvePresetEventId(recipe$event_candidates, recipe$country_id, events)
  } else {
    recipe$event_id
  }
  country_id <- resolvePresetCountryId(recipe$country_id, countries)

  shiny::updateSelectInput(session, paste0(module_prefix, "-sex"), selected = recipe$sex)
  shiny::updateSelectInput(session, paste0(module_prefix, "-country_id"), selected = country_id)
  shiny::updateSelectInput(
    session,
    paste0(module_prefix, "-age_status_id"),
    selected = recipe$age_status_id
  )
  shiny::updateSelectInput(
    session,
    paste0(module_prefix, "-age_modifier"),
    selected = recipeAgeModifier(recipe)
  )
  shiny::updateSelectInput(session, paste0(module_prefix, "-event_id"), selected = event_id)
  shiny::updateSelectInput(session, paste0(module_prefix, "-event_mode"), selected = recipe$event_mode)
  shiny::updateNumericInput(
    session,
    paste0(module_prefix, "-custom_age_min"),
    value = recipe$custom_age_min
  )
  shiny::updateNumericInput(
    session,
    paste0(module_prefix, "-custom_age_max"),
    value = recipe$custom_age_max
  )
}

applyDemoQueryPreset <- function(session, preset_id, events, countries) {
  presets <- demoQueryPresets()
  preset <- presets[[preset_id]]
  if (is.null(preset) || preset_id == "blank") {
    return(invisible(FALSE))
  }

  shiny::updateSliderInput(session, "query_count", value = preset$query_count)
  shiny::updateSelectInput(session, "metric", selected = preset$metric)

  purrr::walk(seq_along(preset$queries), function(i) {
    applyQueryBuilderInputs(
      session = session,
      module_prefix = paste0("qb", i),
      recipe = preset$queries[[i]],
      events = events,
      countries = countries
    )
  })

  invisible(TRUE)
}

seedSessionDefaultQueries <- function(session, events, countries) {
  shiny::updateSelectInput(session, "metric", selected = "count")
  applyQueryBuilderInputs(
    session = session,
    module_prefix = "qb1",
    recipe = defaultQueryRecipe("q1", countries = countries, events = events),
    events = events,
    countries = countries
  )

  invisible(TRUE)
}

resetDemoQueries <- function(session, events, countries) {
  seedSessionDefaultQueries(session, events, countries)
}

populationYearBounds <- function(population) {
  yr_min <- min(population$year, na.rm = TRUE)
  yr_max <- max(population$year, na.rm = TRUE)
  estimate_max <- max(population$year[population$data_type == "estimate"], na.rm = TRUE)
  if (!is.finite(estimate_max)) {
    estimate_max <- yr_max
  }
  list(
    min = yr_min,
    max = yr_max,
    estimate_max = estimate_max,
    default_range = c(yr_min, yr_max)
  )
}

# Left edge of the default view window: the decade (year ending in 0) before the
# earliest event among the supplied start years, clamped to the available
# population year range. When the earliest event lands exactly on a decade
# boundary we step back a further decade so the marker keeps some lead-in buffer.
# Falls back to year_min when no events apply.
eventWindowDefaultStart <- function(event_start_years, year_min, year_max) {
  starts <- suppressWarnings(as.integer(event_start_years))
  starts <- starts[is.finite(starts)]
  if (length(starts) == 0) {
    return(as.integer(year_min))
  }
  earliest <- min(starts)
  decade <- as.integer(floor(earliest / 10) * 10)
  if (earliest == decade) {
    decade <- decade - 10L
  }
  as.integer(max(as.integer(year_min), min(decade, as.integer(year_max))))
}

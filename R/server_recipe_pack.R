recipePackModuleNs <- function(module_id, input_id) {
  paste0(module_id, "-", input_id)
}

applyRecipeToQueryModule <- function(session, module_id, recipe) {
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "country_id"),
    selected = recipe$country_id
  )
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "sex"),
    selected = recipe$sex
  )
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "age_modifier"),
    selected = recipeAgeModifier(recipe)
  )
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "age_status_id"),
    selected = recipe$age_status_id
  )
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "event_mode"),
    selected = recipe$event_mode
  )
  shiny::updateSelectInput(
    session,
    recipePackModuleNs(module_id, "event_id"),
    selected = recipe$event_id
  )
}

recipePackCustomAgeSeeds <- function(recipes) {
  seeds <- list()
  for (i in seq_along(recipes)) {
    recipe <- recipes[[i]]
    if (identical(recipe$age_status_id, "custom")) {
      seeds[[as.character(i)]] <- c(recipe$custom_age_min, recipe$custom_age_max)
    }
  }
  seeds
}

applyRecipePackViewState <- function(session, view_state, year_bounds = NULL) {
  normalized <- normalizeRecipePackViewState(
    list(
      metric = view_state$metric,
      year_range = view_state$year_range,
      show_projection = view_state$show_projection
    ),
    year_bounds = year_bounds
  )

  shiny::updateSelectInput(session, "metric", selected = normalized$metric)
  shiny::updateSliderInput(
    session,
    "year_range",
    value = normalized$year_range
  )
  shiny::updateCheckboxInput(
    session,
    "show_projection",
    value = normalized$show_projection
  )

  normalized
}

applyRecipePackToSession <- function(
  session,
  pack,
  set_active_slots,
  year_bounds = NULL
) {
  recipes <- pack$recipes
  n <- length(recipes)
  if (n < 1L || n > 4L) {
    stop("Recipe pack must contain between 1 and 4 queries.")
  }

  set_active_slots(seq_len(n))

  view_state <- list(
    metric = pack$metric,
    year_range = pack$year_range,
    show_projection = pack$show_projection
  )
  applyRecipePackViewState(session, view_state, year_bounds = year_bounds)

  for (i in seq_len(n)) {
    applyRecipeToQueryModule(
      session = session,
      module_id = sprintf("qb%d", i),
      recipe = recipes[[i]]
    )
  }

  invisible(pack)
}

buildRecipePackCodeFromStates <- function(states, view_state) {
  ready <- purrr::keep(states, function(state) {
    !isTRUE(state$pending) && !is.null(state$recipe$event_id)
  })
  if (length(ready) == 0L) {
    stop("Add at least one complete query before exporting a recipe pack.")
  }
  recipes_tbl <- purrr::map_dfr(ready, function(state) {
    tibble::as_tibble(state$recipe)
  })
  encodeRecipePack(recipes_tbl, view_state)
}

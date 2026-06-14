test_that("encodeRecipePack accepts a list of recipe lists from query states", {
  states <- list(
    list(recipe = buildTestRecipe(query_id = "q1", country_id = "RUS")),
    list(recipe = buildTestRecipe(query_id = "q2", country_id = "USA", event_id = "USA_NATIONAL_EVENT"))
  )
  recipes <- purrr::map(states, function(state) state$recipe)
  view_state <- list(
    metric = "count",
    year_range = c(1979L, 1985L),
    show_projection = TRUE
  )

  code <- encodeRecipePack(recipes, view_state)
  expect_true(isRecipePackCode(code))
  expect_length(decodeRecipePack(code)$recipes, 2)
})

test_that("buildRecipePackCodeFromStates encodes query builder states", {
  states <- list(
    list(
      recipe = buildTestRecipe(query_id = "q1"),
      pending = FALSE
    )
  )
  view_state <- list(
    metric = "count",
    year_range = c(1979L, 1985L),
    show_projection = FALSE
  )

  code <- buildRecipePackCodeFromStates(states, view_state)
  expect_true(isRecipePackCode(code))
})

test_that("encodeRecipePack round-trips with view state and multiple queries", {
  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~age_modifier, ~is_complement,
    ~custom_age_min, ~custom_age_max, ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", "none", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "USA", "all", "adults", "none", FALSE, NA_integer_, NA_integer_, "USA_NATIONAL_EVENT", "start", "count"
  )

  view_state <- list(
    metric = "count",
    year_range = c(1979L, 1985L),
    show_projection = TRUE
  )

  code <- encodeRecipePack(recipes, view_state)
  expect_true(isRecipePackCode(code))
  decoded <- decodeRecipePack(code)

  expect_equal(decoded$metric, "count")
  expect_equal(decoded$year_range, c(1979L, 1985L))
  expect_true(decoded$show_projection)
  expect_length(decoded$recipes, 2)
  expect_equal(decoded$recipes[[1]]$country_id, "RUS")
  expect_equal(decoded$recipes[[2]]$country_id, "USA")
  expect_equal(decoded$recipes[[1]]$query_id, "q1")
  expect_equal(decoded$recipes[[2]]$query_id, "q2")
})

test_that("encodeRecipePack round-trips share_working_age_population metric", {
  recipes <- tibble::as_tibble(buildTestRecipe(metric = "share_working_age_population"))
  view_state <- list(
    metric = "share_working_age_population",
    year_range = c(1979L, 1985L),
    show_projection = TRUE
  )

  code <- encodeRecipePack(recipes, view_state)
  decoded <- decodeRecipePack(code)

  expect_equal(decoded$metric, "share_working_age_population")
  expect_equal(decoded$recipes[[1]]$metric, "share_working_age_population")
})

test_that("validateRecipePack accepts a valid pack", {
  recipes <- tibble::as_tibble(buildTestRecipe())
  view_state <- list(
    metric = "count",
    year_range = c(1979L, 1985L),
    show_projection = FALSE
  )
  code <- encodeRecipePack(recipes, view_state)

  result <- validateRecipePack(
    code = code,
    countries = buildTestCountries(),
    events = buildTestEvents(),
    age_groups = defaultAgeGroups(),
    event_countries = buildTestEventCountries(),
    year_bounds = list(min = 1970L, max = 2100L)
  )

  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_equal(result$pack$recipes[[1]]$event_id, "AFG_WAR")
})

test_that("validateRecipePack rejects unknown version", {
  result <- validateRecipePack(
    code = "GEN2:country=RUS",
    countries = buildTestCountries(),
    events = buildTestEvents(),
    age_groups = defaultAgeGroups()
  )
  expect_false(result$valid)
  expect_match(result$errors[[1]], "GENPACK1")
})

test_that("buildExportMetadataTables stores recipe_pack in metadata", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  view_state <- list(
    metric = "count",
    year_range = range(plot_data$year),
    show_projection = TRUE
  )

  tables <- buildExportMetadataTables(
    plot_data = plot_data,
    recipes = tibble::as_tibble(recipe),
    events = events,
    countries = countries,
    age_groups = age_groups,
    view_state = view_state,
    population = population
  )

  pack_value <- tables$metadata$value[tables$metadata$field == "recipe_pack"]
  expect_true(isRecipePackCode(pack_value))
  expect_equal(
    decodeRecipePack(pack_value)$recipes[[1]]$country_id,
    "RUS"
  )
  expect_true(nzchar(pack_value))
})

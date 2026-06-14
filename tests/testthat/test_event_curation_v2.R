buildCuratedV2Event <- function(...) {
  defaults <- list(
    event_id = "USA_TEST_EVENT",
    event_name = "Test curated event",
    event_type = "politics",
    event_scope = "national",
    start_year = 1990L,
    end_year = 1991L,
    peak_year = 1990L,
    event_origin = "manual_curated_v2",
    cross_country_allowed = FALSE,
    short_description = "A curated event with a plausible mass-exposure mechanism.",
    source_url = "https://en.wikipedia.org/wiki/Example",
    event_family = "regime_institutions",
    selection_channel = "manual_curated",
    population_reach_score = 3L,
    intensity_score = 2L,
    institutional_discontinuity_score = 3L,
    memory_salience_score = 2L,
    cohort_relevance_score = 2L,
    source_confidence_score = 3L,
    include_in_core_catalogue = TRUE
  )
  overrides <- list(...)
  tibble::as_tibble(utils::modifyList(defaults, overrides))
}

test_that("loadEvents adds and types the v2 curation columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(
    tibble::tibble(
      event_id = "USA_LEGACY",
      event_name = "Legacy",
      event_type = "politics",
      event_scope = "national",
      start_year = 2000L,
      end_year = 2000L,
      peak_year = 2000L,
      event_origin = "llm_seed_v1",
      cross_country_allowed = FALSE
    ),
    tmp
  )

  events <- loadEvents(tmp)
  expect_true(all(c(
    "short_description", "source_url", "event_family", "selection_channel",
    eventCurationScoreFields(), "include_in_core_catalogue"
  ) %in% names(events)))
  expect_true(is.character(events$event_family))
  expect_true(is.integer(events$population_reach_score))
  expect_true(is.logical(events$include_in_core_catalogue))
})

test_that("validateEvents passes a well-formed v2 row", {
  events <- buildCuratedV2Event()
  expect_no_error(validateEvents(events))
})

test_that("validateEvents fails v2 row missing short_description", {
  events <- buildCuratedV2Event(short_description = NA_character_)
  expect_error(validateEvents(events), "short_description")
})

test_that("validateEvents fails v2 row with non-URL source", {
  events <- buildCuratedV2Event(source_url = "not-a-url")
  expect_error(validateEvents(events), "source_url")
})

test_that("validateEvents fails v2 row with invalid event_family", {
  events <- buildCuratedV2Event(event_family = "not_a_family")
  expect_error(validateEvents(events), "event_family")
})

test_that("validateEvents fails v2 row with invalid selection_channel", {
  events <- buildCuratedV2Event(selection_channel = "guesswork")
  expect_error(validateEvents(events), "selection_channel")
})

test_that("validateEvents fails v2 row with out-of-range score", {
  events <- buildCuratedV2Event(intensity_score = 5L)
  expect_error(validateEvents(events), "intensity_score")
})

test_that("validateEvents fails v2 row missing include_in_core_catalogue", {
  events <- buildCuratedV2Event(include_in_core_catalogue = NA)
  expect_error(validateEvents(events), "include_in_core_catalogue")
})

test_that("legacy rows with bad curation values warn instead of failing", {
  events <- tibble::tibble(
    event_id = "USA_LEGACY",
    event_name = "Legacy",
    event_type = "politics",
    event_scope = "national",
    start_year = 2000L,
    end_year = 2000L,
    peak_year = 2000L,
    event_origin = "llm_seed_v1",
    cross_country_allowed = FALSE,
    event_family = "not_a_family"
  )
  expect_warning(validateEvents(events), "event_family")
})

test_that("formatCurationScoreSummary renders compact scores and blanks for NA", {
  scores <- tibble::tibble(
    population_reach_score = c(3L, NA_integer_),
    intensity_score = c(2L, NA_integer_),
    institutional_discontinuity_score = c(1L, NA_integer_),
    memory_salience_score = c(0L, NA_integer_),
    cohort_relevance_score = c(2L, NA_integer_),
    source_confidence_score = c(3L, NA_integer_)
  )
  summary <- formatCurationScoreSummary(scores)
  expect_match(summary[[1]], "reach 3")
  expect_match(summary[[1]], "source 3")
  expect_identical(summary[[2]], "")
})

test_that("buildPlotData carries event description, source and family", {
  population <- buildTestPopulation()
  events <- buildTestEvents() |>
    dplyr::mutate(
      short_description = dplyr::if_else(
        event_id == "AFG_WAR", "Soviet war in Afghanistan.", NA_character_
      ),
      source_url = dplyr::if_else(
        event_id == "AFG_WAR", "https://en.wikipedia.org/wiki/Soviet_war_in_Afghanistan",
        NA_character_
      ),
      event_family = dplyr::if_else(
        event_id == "AFG_WAR", "organized_violence", NA_character_
      )
    )
  countries <- buildTestCountries()
  recipe <- buildTestRecipe()

  out <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = defaultAgeGroups(),
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  expect_equal(out$event_short_description[[1]], "Soviet war in Afghanistan.")
  expect_match(out$event_source_url[[1]], "^https://")
  expect_equal(out$event_family[[1]], "organized_violence")

  details <- buildQueryDetailsRows(out)
  expect_true(all(c(
    "event_short_description", "event_source_url", "event_family"
  ) %in% names(details)))
})

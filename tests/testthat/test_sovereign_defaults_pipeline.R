test_that("importDefaultsExplicitFromExcel maps ISO2 codes and flag defaults", {
  data_dir <- testthat::test_path("..", "..", "data")
  path <- resolveIndicatorExcelPath("Defaults_DB.xlsx", data_dir = data_dir)
  if (!file.exists(path)) {
    skip("Defaults_DB.xlsx not available under data/")
  }
  countries <- readr::read_csv(
    resolveEventDataPath("countries.csv", data_dir = data_dir),
    show_col_types = FALSE
  )

  defaults <- importDefaultsExplicitFromExcel(path, countries = countries)
  expect_gt(nrow(defaults), 0)
  expect_true(all(c("country_id", "year", "flag", "source") %in% names(defaults)))
  expect_true(all(nchar(defaults$country_id) == 3))
  expect_true(all(defaults$flag %in% c(0L, 1L)))

  arg <- defaults |>
    dplyr::filter(.data$country_id == "ARG", .data$flag == 1L) |>
    dplyr::arrange(.data$year)
  expect_gte(nrow(arg), 2)
})

test_that("SOVEREIGN_DEFAULT criterion produces deterministic CMP events", {
  indicators <- list(
    sovereign_defaults = tibble::tribble(
      ~country_id, ~year, ~flag, ~source, ~source_version,
      "ARG", 1982L, 1L, "test", "v1",
      "ARG", 1989L, 1L, "test", "v1",
      "ARG", 2001L, 1L, "test", "v1",
      "GRC", 2012L, 1L, "test", "v1"
    )
  )
  criteria <- tibble::tribble(
    ~criterion_id, ~indicator, ~operator, ~threshold_value, ~event_type,
    ~default_event_mode, ~name_template, ~default_tags, ~show_in_picker,
    "SOVEREIGN_DEFAULT", "sovereign_defaults", "flag ==", 1, "economy",
    "start", "Государственный дефолт в {country_id} ({year})", "sovereign_default", FALSE
  )
  countries <- tibble::tribble(
    ~country_id, ~country_name, ~iso3, ~boundary_warning,
    "ARG", "Argentina", "ARG", "",
    "GRC", "Greece", "GRC", ""
  )

  built <- buildComputedEventsFromCriteria(indicators, criteria, countries = countries)
  arg_events <- built$events |>
    dplyr::filter(grepl("^CMP_SOVEREIGN_DEFAULT_ARG_", .data$event_id))
  expect_equal(nrow(arg_events), 3)
  expect_equal(sort(arg_events$start_year), c(1982L, 1989L, 2001L))
  expect_true(all(grepl("^CMP_SOVEREIGN_DEFAULT_", built$events$event_id)))
})

test_that("sovereign_default composite groups per country with localized name", {
  events <- tibble::tribble(
    ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed,
    "CMP_SOVEREIGN_DEFAULT_ARG_1982", "Default 1982", "economy", "national", 1982L, 1982L, 1982L, "computed", FALSE,
    "CMP_SOVEREIGN_DEFAULT_ARG_1989", "Default 1989", "economy", "national", 1989L, 1989L, 1989L, "computed", FALSE,
    "CMP_SOVEREIGN_DEFAULT_ARG_2001", "Default 2001", "economy", "national", 2001L, 2001L, 2001L, "computed", FALSE
  )
  tags <- tibble::tribble(
    ~event_id, ~tag,
    "CMP_SOVEREIGN_DEFAULT_ARG_1982", "sovereign_default",
    "CMP_SOVEREIGN_DEFAULT_ARG_1989", "sovereign_default",
    "CMP_SOVEREIGN_DEFAULT_ARG_2001", "sovereign_default"
  )
  links <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "CMP_SOVEREIGN_DEFAULT_ARG_1982", "ARG", "affected",
    "CMP_SOVEREIGN_DEFAULT_ARG_1989", "ARG", "affected",
    "CMP_SOVEREIGN_DEFAULT_ARG_2001", "ARG", "affected"
  )
  criteria <- tibble::tribble(
    ~criterion_id, ~default_tags, ~name_template,
    "SOVEREIGN_DEFAULT", "sovereign_default", "Государственный дефолт в {country_id} ({year})"
  )

  built <- buildCompositeEventsFromTags(
    events = events,
    event_tags = tags,
    classifier_spec = 'tag == "sovereign_default"',
    event_countries = links,
    countries = tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "ARG", "Argentina", "ARG", ""
    ),
    criteria = criteria,
    composite_name = "Дефолты по гособлигациям"
  )

  expect_equal(nrow(built$composite_events), 1)
  expect_equal(built$composite_events$composite_event_id, "MERGE_SOVEREIGN_DEFAULT_ARG")
  expect_equal(nrow(built$composite_members), 3)
  expect_match(built$composite_events$event_name, "Дефолты по гособлигациям")
  expect_match(built$composite_events$event_name, "Argentina")
})

test_that("sovereign default composite experienced_any forms a staircase", {
  population <- buildSemanticsPopulation(years = 1975:2010)
  events <- dplyr::bind_rows(
    tibble::tribble(
      ~event_id, ~event_name, ~event_type, ~event_scope, ~start_year, ~end_year, ~peak_year, ~event_origin, ~cross_country_allowed, ~country_id,
      "CMP_SOVEREIGN_DEFAULT_ARG_1982", "D 1982", "economy", "national", 1982L, 1982L, 1982L, "computed", FALSE, "ARG",
      "CMP_SOVEREIGN_DEFAULT_ARG_1989", "D 1989", "economy", "national", 1989L, 1989L, 1989L, "computed", FALSE, "ARG",
      "CMP_SOVEREIGN_DEFAULT_ARG_2001", "D 2001", "economy", "national", 2001L, 2001L, 2001L, "computed", FALSE, "ARG",
      "MERGE_SOVEREIGN_DEFAULT_ARG", "Defaults ARG", "economy", "national", 1982L, 2001L, 2001L, "composite", FALSE, "ARG"
    )
  )
  members <- tibble::tribble(
    ~composite_event_id, ~member_event_id,
    "MERGE_SOVEREIGN_DEFAULT_ARG", "CMP_SOVEREIGN_DEFAULT_ARG_1982",
    "MERGE_SOVEREIGN_DEFAULT_ARG", "CMP_SOVEREIGN_DEFAULT_ARG_1989",
    "MERGE_SOVEREIGN_DEFAULT_ARG", "CMP_SOVEREIGN_DEFAULT_ARG_2001"
  )
  recipe <- buildTestRecipe(
    country_id = "ARG",
    event_id = "MERGE_SOVEREIGN_DEFAULT_ARG",
    age_status_id = "adults",
    event_mode = "start",
    metric = "count"
  )

  out <- calculateStratumSeries(
    recipe,
    population |> dplyr::mutate(country_id = "ARG", country_name = "Argentina"),
    events,
    defaultAgeGroups(),
    tibble::tribble(
      ~country_id, ~country_name, ~iso3, ~boundary_warning,
      "ARG", "Argentina", "ARG", ""
    ),
    event_countries = tibble::tribble(
      ~event_id, ~country_id, ~country_role,
      "MERGE_SOVEREIGN_DEFAULT_ARG", "ARG", "affected"
    ),
    composite_members = members
  )

  expect_true(all(out$value[out$year < 1982L] == 0))
  expect_gt(out$value[out$year == 1982L], 0)
  expect_gt(out$value[out$year == 1989L], out$value[out$year == 1988L])
  expect_gt(out$value[out$year == 2001L], out$value[out$year == 2000L])
})

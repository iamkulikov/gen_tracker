test_that("birth years resolved for event start and school age", {
  recipe <- list(event_mode = "start", age_status_id = "school_age", is_complement = FALSE)
  event <- tibble::tibble(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  age_range <- list(age_min = 6L, age_max = 17L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2100, population_ages = 0:100)
  expect_equal(min(by), 1962L)
  expect_equal(max(by), 1973L)
})

test_that("born after event uses end year boundary", {
  recipe <- list(event_mode = "period", age_status_id = "not_born_yet", is_complement = FALSE)
  event <- tibble::tibble(start_year = 1961L, end_year = 1963L, peak_year = 1962L)
  age_range <- list(age_min = 6L, age_max = 17L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2000, population_ages = 0:100)
  expect_true(all(by > 1963L))
})

test_that("event end mode resolves age range against event end year", {
  recipe <- list(event_mode = "end", age_status_id = "adults", is_complement = FALSE)
  event <- tibble::tibble(start_year = 1989L, end_year = 1991L, peak_year = 1990L)
  age_range <- list(age_min = 18L, age_max = 25L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2100, population_ages = 0:100)
  expect_equal(min(by), 1966L)
  expect_equal(max(by), 1973L)
})

test_that("complement returns birth years outside matched age range", {
  recipe <- list(event_mode = "start", age_status_id = "school_age", is_complement = TRUE)
  event <- tibble::tibble(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  age_range <- list(age_min = 6L, age_max = 17L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2000, population_ages = 0:100)
  expect_false(any(1962L:1973L %in% by))
})

test_that("younger than uses ages below selected age range", {
  recipe <- list(
    event_mode = "start",
    age_status_id = "school_age",
    age_modifier = "younger_than",
    is_complement = FALSE
  )
  event <- tibble::tibble(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  age_range <- list(age_min = 6L, age_max = 17L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2000, population_ages = 0:100)
  expect_equal(min(by), 1974L)
  expect_equal(max(by), 1979L)
})

test_that("older than uses ages above selected age range", {
  recipe <- list(
    event_mode = "start",
    age_status_id = "school_age",
    age_modifier = "older_than",
    is_complement = FALSE
  )
  event <- tibble::tibble(start_year = 1979L, end_year = 1989L, peak_year = 1984L)
  age_range <- list(age_min = 6L, age_max = 17L)

  by <- resolveBirthYears(recipe, event, age_range, population_years = 1950:2000, population_ages = 0:100)
  expect_equal(max(by), 1961L)
})

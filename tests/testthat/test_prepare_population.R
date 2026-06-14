test_that("filterPopulationToCountries keeps only listed countries", {
  population <- tibble::tribble(
    ~country_id, ~country_name, ~year, ~sex, ~age, ~population,
    ~data_type, ~scenario, ~source, ~source_version,
    "RUS", "Russia WPP", 2000L, "male", 10L, 100,
    "estimate", "wpp_estimates", "UN WPP", "2024",
    "USA", "United States WPP", 2000L, "female", 10L, 200,
    "estimate", "wpp_estimates", "UN WPP", "2024"
  )
  countries <- tibble::tribble(
    ~country_id, ~country_name, ~iso3, ~boundary_warning,
    "RUS", "Russia", "RUS", ""
  )

  out <- filterPopulationToCountries(population, countries)

  expect_equal(nrow(out), 1)
  expect_equal(out$country_id, "RUS")
  expect_equal(out$country_name, "Russia")
})

test_that("WPP sex detection does not treat Female files as Male", {
  expect_equal(
    dplyr::case_when(
      grepl("Female", "WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx", ignore.case = TRUE) ~ "female",
      grepl("Male", "WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx", ignore.case = TRUE) ~ "male",
      TRUE ~ "all"
    ),
    "female"
  )
})

test_that("identifySingleAgeColumns finds digit and ellipsis headers", {
  cols <- c("Year", "Type", as.character(0:100))
  expect_equal(length(identifySingleAgeColumns(cols)), 101)

  cols_ellipsis <- c("Year", "Type", paste0("...", 1:100))
  expect_equal(length(identifySingleAgeColumns(cols_ellipsis)), 100)
})

test_that("coerceSingleAgeFromLabel maps digit and positional labels", {
  expect_equal(coerceSingleAgeFromLabel(c("0", "1", "2"), c("0", "1", "2")), c(0L, 1L, 2L))
  expect_equal(
    coerceSingleAgeFromLabel(c("...1", "...2"), c("...1", "...2", "...3")),
    c(0L, 1L)
  )
})

test_that("preparedPopulationPath points to data/population.rds", {
  path <- preparedPopulationPath("data")
  expect_equal(basename(path), "population.rds")
  expect_equal(dirname(path), "data")
})

test_that("WPP excel loader parses expected schema", {
  data_dir <- testthat::test_path("..", "..", "data")
  search_dir <- resolveWppSourcesSearchDir(data_dir)
  paths <- list.files(search_dir, pattern = "SINGLE_AGE.*MALE", full.names = TRUE, ignore.case = TRUE)
  skip_if_not(length(paths) > 0 && file.exists(paths[[1]]), "WPP source file is not available.")
  path <- paths[[1]]

  pop <- loadPopulationData(path)

  expect_true(nrow(pop) > 0)
  expect_true(all(c("country_id", "country_name", "year", "sex", "age", "population") %in% names(pop)))
  expect_true(all(pop$sex == "male"))
  expect_true(any(pop$data_type == "estimate"))
  expect_true(any(pop$data_type == "projection"))
})

test_that("PPP excel loader parses expected schema", {
  data_dir <- testthat::test_path("..", "..", "data")
  search_dir <- resolveWppSourcesSearchDir(data_dir)
  paths <- list.files(search_dir, pattern = "PopulationBySingleAge.*Female", full.names = TRUE, ignore.case = TRUE)
  skip_if_not(length(paths) > 0 && file.exists(paths[[1]]), "PPP source file is not available.")
  path <- paths[[1]]

  pop <- loadPopulationData(path)

  expect_true(nrow(pop) > 0)
  expect_true(all(pop$sex == "female"))
  expect_true(all(pop$data_type == "projection"))
  expect_true(all(pop$scenario == "ppp_median"))
})

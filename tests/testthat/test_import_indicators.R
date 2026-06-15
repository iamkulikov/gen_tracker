test_that("importCpiFromWdiExcel reads WDI layout when CPI.xlsx present", {
  data_dir <- testthat::test_path("..", "..", "data")
  path <- resolveIndicatorExcelPath("CPI.xlsx", data_dir = data_dir)
  if (!file.exists(path)) {
    skip("CPI.xlsx not available under data/")
  }
  countries <- buildTestCountries()
  cpi <- importCpiFromWdiExcel(path, countries = countries)
  expect_gt(nrow(cpi), 0)
  expect_true(all(c("country_id", "year", "value", "source") %in% names(cpi)))
  rus <- cpi |> dplyr::filter(.data$country_id == "RUS", .data$year == 1998L)
  expect_equal(nrow(rus), 1)
  expect_gt(rus$value, 20)
})

test_that("importErFromExcel maps Russia and supports YoY crisis detection", {
  data_dir <- testthat::test_path("..", "..", "data")
  path <- resolveIndicatorExcelPath("ER.xlsx", data_dir = data_dir)
  if (!file.exists(path)) {
    skip("ER.xlsx not available under data/")
  }
  countries <- buildTestCountries()
  er <- importErFromExcel(path, countries = countries)
  rus <- er |> dplyr::filter(.data$country_id == "RUS") |> dplyr::arrange(.data$year)
  expect_gt(nrow(rus), 0)
  parsed <- parseCriterionOperator("relative_increase >=", 0.7)
  hits <- applyCriterionToIndicator(rus, parsed)
  expect_true(any(hits$year == 1999L))
})

test_that("validateIndicators allows negative CPI inflation (deflation)", {
  ind <- tibble::tibble(
    country_id = "JPN",
    year = 2009L,
    value = -1.4,
    source = "test",
    source_version = "v1"
  )
  expect_no_error(validateIndicators(ind, indicator_name = "cpi_inflation"))
  expect_error(validateIndicators(ind, indicator_name = "exchange_rate"), "negative")
})

test_that("importDefaultsExplicitFromExcel reads explicit sheet when Defaults_DB present", {
  data_dir <- testthat::test_path("..", "..", "data")
  path <- resolveIndicatorExcelPath("Defaults_DB.xlsx", data_dir = data_dir)
  if (!file.exists(path)) {
    skip("Defaults_DB.xlsx not available under data/")
  }
  countries <- readr::read_csv(
    countriesDataPath(data_dir = data_dir),
    show_col_types = FALSE
  )
  defaults <- importDefaultsExplicitFromExcel(path, countries = countries)
  expect_gt(sum(defaults$flag == 1L), 0)
  expect_no_error(validateIndicators(defaults, indicator_name = "sovereign_defaults"))
})

test_that("loadIndicatorsDirectory strips 5_ prefix from indicator keys", {
  tmp <- tempfile("gt_ind_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  readr::write_csv(
    tibble::tibble(
      country_id = "RUS",
      year = 2000L,
      value = 1,
      source = "test",
      source_version = "v1"
    ),
    file.path(tmp, indicatorCsvFilename("cpi_inflation"))
  )
  loaded <- loadIndicatorsDirectory(tmp)
  expect_true("cpi_inflation" %in% names(loaded))
  expect_false("5_cpi_inflation" %in% names(loaded))
})

test_that("relative_increase operator detects FX weakening", {
  ind <- tibble::tribble(
    ~country_id, ~year, ~value, ~source, ~source_version,
    "RUS", 1998L, 10, "test", "v1",
    "RUS", 1999L, 20, "test", "v1"
  )
  parsed <- parseCriterionOperator("relative_increase >=", 0.7)
  hits <- applyCriterionToIndicator(ind, parsed)
  expect_equal(hits$year, 1999L)
})

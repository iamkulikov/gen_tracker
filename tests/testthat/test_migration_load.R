test_that("validateMigration accepts well-formed table", {
  migration <- buildTestMigration()
  expect_true(validateMigration(migration))
})

test_that("validateMigration rejects duplicate country x year x data_type", {
  migration <- dplyr::bind_rows(
    buildTestMigration(years = 1980L),
    buildTestMigration(years = 1980L)
  )
  expect_error(validateMigration(migration), "duplicate")
})

test_that("validateMigration rejects unknown data_type", {
  migration <- buildTestMigration(years = 1980L)
  migration$data_type <- "guess"
  expect_error(validateMigration(migration), "data_type")
})

test_that("computeMigrationCountryFeatures aggregates absolute rate per country", {
  migration <- tibble::tibble(
    country_id = c("RUS", "RUS", "RUS"),
    year = c(2000L, 2001L, 2002L),
    net_migration_rate = c(-4, 2, -6),
    net_migration = c(-400, 200, -600),
    data_type = "estimate",
    source = "UN WPP",
    source_version = "2024"
  )
  features <- computeMigrationCountryFeatures(migration)
  expect_equal(features$country_id, "RUS")
  expect_equal(features$abs_rate_mean, mean(c(4, 2, 6)))
  expect_equal(features$abs_rate_max, 6)
  expect_equal(features$years_estimate, 3L)
  expect_equal(features$years_projection, 0L)
})

test_that("loadPreparedMigration round-trips an rds artifact", {
  prepared <- buildTestMigrationPrepared()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(prepared, tmp)

  loaded <- loadPreparedMigration(tmp)
  expect_equal(nrow(loaded$migration), nrow(prepared$migration))
  expect_true(all(c("country_id", "year", "net_migration_rate") %in% names(loaded$migration)))
  expect_true(nrow(loaded$country_features) > 0)
})

test_that("loadPreparedMigration returns NULL for missing optional file", {
  missing <- tempfile(fileext = ".rds")
  expect_null(loadPreparedMigration(missing, required = FALSE))
  expect_error(loadPreparedMigration(missing, required = TRUE), "not found")
})

restoreEnvVar <- function(name, old_value) {
  if (is.na(old_value)) {
    Sys.unsetenv(name)
  } else {
    Sys.setenv(setNames(old_value, name))
  }
}

test_that("validatePopulation strict=FALSE skips duplicate scan", {
  population <- buildTestPopulation()
  population_dup <- dplyr::bind_rows(population, population[1, , drop = FALSE])

  expect_error(validatePopulation(population_dup, strict = TRUE), "duplicate")
  expect_true(validatePopulation(population_dup, strict = FALSE))
})

test_that("preparedPopulationManifestCoversFile matches manifest output_path", {
  tmp <- tempfile("gt_manifest_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  rds_path <- file.path(tmp, "population.rds")
  saveRDS(buildTestPopulation(), rds_path)

  manifest_path <- file.path(tmp, "population_build_manifest.json")
  writePreparedPopulationManifest(
    list(
      schema_version = PREPARED_POPULATION_SCHEMA_VERSION,
      built_at = "2026-01-01T00:00:00Z",
      output_path = normalizePath(rds_path, winslash = "/"),
      source_files = c("male.xlsx", "female.xlsx"),
      countries_file = "countries.csv",
      country_count = 1,
      row_count = 100,
      year_min = 1979,
      year_max = 1985,
      sexes = c("male", "female"),
      data_types = c("estimate", "projection"),
      file_size_bytes = file.info(rds_path)$size,
      compress = "gzip",
      build_seconds = 1
    ),
    path = manifest_path
  )

  old_data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = NA_character_)
  old_prepared <- Sys.getenv("GEN_TRACKER_PREPARED_POPULATION_PATH", unset = NA_character_)
  on.exit({
    restoreEnvVar("GEN_TRACKER_DATA_DIR", old_data_dir)
    restoreEnvVar("GEN_TRACKER_PREPARED_POPULATION_PATH", old_prepared)
  }, add = TRUE)
  Sys.setenv(GEN_TRACKER_DATA_DIR = tmp, GEN_TRACKER_PREPARED_POPULATION_PATH = rds_path)

  expect_true(preparedPopulationManifestCoversFile(rds_path, manifest_path = manifest_path))
  expect_false(populationValidationStrictForPath(rds_path))
})

test_that("populationValidationStrictForPath is TRUE without manifest", {
  tmp <- tempfile("gt_no_manifest_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  rds_path <- file.path(tmp, "population.rds")
  saveRDS(buildTestPopulation(), rds_path)
  expect_true(populationValidationStrictForPath(rds_path))
})

test_that("missingPreparedPopulationHint mentions build script", {
  hint <- missingPreparedPopulationHint()
  expect_match(hint, "build_prepared_population")
  expect_match(hint, "population_cache")
})

test_that("resolvePopulationPaths returns empty without prepared rds or excel flag", {
  tmp <- tempfile("gt_resolve_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  old_data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = NA_character_)
  old_excel <- Sys.getenv("GEN_TRACKER_ALLOW_EXCEL_SOURCES", unset = NA_character_)
  old_paths <- Sys.getenv("GEN_TRACKER_POPULATION_PATHS", unset = NA_character_)
  on.exit({
    restoreEnvVar("GEN_TRACKER_DATA_DIR", old_data_dir)
    restoreEnvVar("GEN_TRACKER_ALLOW_EXCEL_SOURCES", old_excel)
    restoreEnvVar("GEN_TRACKER_POPULATION_PATHS", old_paths)
  }, add = TRUE)
  Sys.setenv(
    GEN_TRACKER_DATA_DIR = tmp,
    GEN_TRACKER_ALLOW_EXCEL_SOURCES = "FALSE",
    GEN_TRACKER_POPULATION_PATHS = ""
  )

  expect_equal(resolvePopulationPaths(tmp), character())
})

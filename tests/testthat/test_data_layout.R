test_that("mergeEventCountriesWithOrigin tags layers and deduplicates", {
  manual <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "affected"
  )
  computed <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "CMP_FX_RUS_1998", "RUS", "affected",
    "AFG_WAR", "RUS", "affected"
  )
  composite <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "MERGE_FX_RUS", "RUS", "affected"
  )

  merged <- mergeEventCountriesWithOrigin(manual, computed, composite)
  expect_named(merged, c("event_id", "country_id", "country_role", "origin"))
  expect_equal(nrow(merged), 3)
  expect_equal(
    merged |> dplyr::filter(.data$event_id == "AFG_WAR") |> dplyr::pull(.data$origin),
    "manual"
  )
})

test_that("mergeDeployEventLinks writes deploy file with origin column", {
  tmp <- tempfile("gt_layout_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  manual <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "AFG_WAR", "RUS", "affected"
  )
  computed <- tibble::tribble(
    ~event_id, ~country_id, ~country_role,
    "CMP_FX_RUS_1998", "RUS", "affected"
  )

  writeEventCountryLayer(manual, eventCountriesManualLayerWritePath(tmp))
  writeEventCountryLayer(computed, generatedEventFileWritePath(DATA_FILE_EVENT_COUNTRIES_COMPUTED, tmp))

  result <- mergeDeployEventLinks(data_dir = tmp)
  expect_true(file.exists(result$deploy_path))
  on_disk <- readr::read_csv(result$deploy_path, show_col_types = FALSE)
  expect_true("origin" %in% names(on_disk))
  expect_equal(nrow(on_disk), 2)

  loaded <- loadEventCountriesUniverse(
    manual_path = eventCountriesDeployPath(tmp),
    data_dir = tmp
  )
  expect_false("origin" %in% names(loaded))
  expect_equal(nrow(loaded), 2)
})

test_that("loadEventTagsUniverse reads merged deploy tags", {
  tmp <- tempfile("gt_tags_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  manual <- tibble::tribble(~event_id, ~tag, "AFG_WAR", "war")
  computed <- tibble::tribble(~event_id, ~tag, "CMP_FX_RUS_1998", "fx_crisis")

  writeEventTagLayer(manual, generatedEventFileWritePath(DATA_FILE_EVENT_TAGS_MANUAL_LAYER, tmp))
  writeEventTagLayer(computed, generatedEventFileWritePath(DATA_FILE_EVENT_TAGS_COMPUTED, tmp))
  mergeDeployEventTags(data_dir = tmp)

  tags <- loadEventTagsUniverse(data_dir = tmp)
  expect_equal(nrow(tags), 2)
  expect_false("origin" %in% names(tags))
})

test_that("resolveIndicatorExcelPath prefers sources directory", {
  tmp <- tempfile("gt_paths_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  legacy <- file.path(tmp, "CPI.xlsx")
  preferred <- file.path(indicatorSourcesDir(tmp), "CPI.xlsx")
  dir.create(indicatorSourcesDir(tmp), recursive = TRUE)
  writeLines("legacy", legacy)
  writeLines("preferred", preferred)

  expect_equal(resolveIndicatorExcelPath("CPI.xlsx", tmp), preferred)
})

test_that("resolveWppSourcePaths finds WPP files under sources/wpp", {
  tmp <- tempfile("gt_wpp_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  wpp_dir <- wppSourcesDir(tmp)
  dir.create(wpp_dir, recursive = TRUE)
  male <- file.path(wpp_dir, "WPP2024_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx")
  female <- file.path(wpp_dir, "WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx")
  writeLines("x", male)
  writeLines("x", female)

  paths <- resolveWppSourcePaths(tmp)
  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))
})

test_that("buildDataSourcesUi avoids file paths and dev commands", {
  ui <- buildDataSourcesUi(
    year_bounds = list(min = 1950L, max = 2100L, estimate_max = 2023L),
    n_countries = 12L,
    n_events = 340L
  )

  ui_text <- as.character(ui)
  expect_false(grepl("population\\.rds", ui_text, fixed = TRUE))
  expect_false(grepl("events\\.csv", ui_text, fixed = TRUE))
  expect_false(grepl("build_event_countries", ui_text, fixed = TRUE))
  expect_false(grepl("GEN_TRACKER_", ui_text, fixed = TRUE))
  expect_false(grepl("Event–country links", ui_text, fixed = TRUE))
  expect_match(ui_text, "UN World Population Prospects")
  expect_match(ui_text, GEN_TRACKER_WPP_URL)
  expect_match(ui_text, "Population source")
  expect_match(ui_text, "Event catalogue")
  expect_match(ui_text, "gt-data-grid-value")
  expect_match(ui_text, "12")
  expect_match(ui_text, "340")
  expect_match(ui_text, "1950")
  expect_match(ui_text, "2100")
  expect_match(ui_text, "estimates through 2023")
})

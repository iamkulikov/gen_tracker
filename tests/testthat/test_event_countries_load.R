test_that("loadEventCountries reads fixture schema and defaults country_role", {
  path <- buildTestEventCountriesFixturePath()
  event_countries <- loadEventCountries(path)

  expect_s3_class(event_countries, "data.frame")
  expect_named(
    event_countries,
    c("event_id", "country_id", "country_role")
  )
  expect_type(event_countries$event_id, "character")
  expect_type(event_countries$country_id, "character")
  expect_type(event_countries$country_role, "character")
  expect_equal(
    event_countries |>
      dplyr::filter(.data$event_id == "AFG_WAR") |>
      dplyr::pull(.data$country_role),
    "affected"
  )
})

test_that("loadEventCountries coalesces missing country_role to affected", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(
    tibble::tribble(
      ~event_id, ~country_id, ~country_role,
      "AFG_WAR", "RUS", NA_character_
    ),
    tmp
  )

  loaded <- loadEventCountries(tmp)
  expect_equal(loaded$country_role[[1]], "affected")
})

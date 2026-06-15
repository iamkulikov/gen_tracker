# Add or refresh iso2 on data/countries.csv from data/iso3166_alpha_crosswalk.csv.
# Run once after editing countries.csv or when the crosswalk is updated:
#   Rscript scripts/build_country_iso2.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

locateGenTrackerRoot <- function() {
  env_root <- Sys.getenv("GEN_TRACKER_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/"))
  }
  normalizePath(".", winslash = "/")
}

root <- locateGenTrackerRoot()
countries_path <- countriesDataPath(root)
crosswalk_path <- file.path(root, "data", "iso3166_alpha_crosswalk.csv")

if (!file.exists(countries_path)) {
  stop("Missing ", countries_path, call. = FALSE)
}
if (!file.exists(crosswalk_path)) {
  stop(
    "Missing ", crosswalk_path,
    ". Generate it from countries.csv (e.g. ISO 3166 alpha-3 to alpha-2 mapping).",
    call. = FALSE
  )
}

country_col_types <- cols(
  country_id = col_character(),
  country_name = col_character(),
  boundary_warning = col_character(),
  iso2 = col_character()
)

countries <- read_csv(
  countries_path,
  show_col_types = FALSE,
  na = character(),
  col_types = country_col_types
)
crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE, na = character()) |>
  transmute(
    country_id = as.character(iso3),
    iso2 = as.character(iso2)
  )

if (!"boundary_warning" %in% names(countries)) {
  countries$boundary_warning <- ""
}

out <- countries |>
  select(-any_of("iso2")) |>
  left_join(crosswalk, by = "country_id", relationship = "many-to-one")

unmapped <- out |>
  filter(is.na(.data$iso2) | !nzchar(.data$iso2)) |>
  pull(.data$country_id)
if (length(unmapped) > 0) {
  stop(
    "No ISO2 mapping for: ",
    paste(unmapped, collapse = ", "),
    call. = FALSE
  )
}

out <- out |>
  mutate(
    boundary_warning = coalesce(as.character(.data$boundary_warning), "")
  ) |>
  select(country_id, country_name, boundary_warning, iso2)

write_csv(out, countries_path)
message("Updated ", countries_path, " (", nrow(out), " rows, iso2 column).")

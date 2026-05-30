defaultAgeGroups <- function() {
  tibble::tibble(
    age_group_id = c(
      "infant", "children", "school_age", "teenagers", "youth",
      "adults", "working_age", "elderly", "conscious_memory", "custom"
    ),
    age_label = c(
      "Infant", "Children", "School age", "Teenagers", "Youth",
      "Adults", "Working age", "Elderly", "Conscious memory", "Custom"
    ),
    age_min = c(0L, 0L, 6L, 13L, 18L, 18L, 15L, 65L, 7L, NA_integer_),
    age_max = c(2L, 12L, 17L, 19L, 25L, NA_integer_, 64L, NA_integer_, NA_integer_, NA_integer_)
  )
}

load_age_groups <- function(path = NULL) {
  if (is.null(path)) {
    return(defaultAgeGroups())
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      age_min = as.integer(age_min),
      age_max = as.integer(age_max)
    )
}

loadAgeGroups <- load_age_groups

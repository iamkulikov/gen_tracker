requiredIndicatorColumns <- function() {
  c("country_id", "year", "source", "source_version")
}

loadIndicatorFile <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Indicator file not found: %s", path), call. = FALSE)
  }
  raw <- readr::read_csv(path, show_col_types = FALSE)
  required <- requiredIndicatorColumns()
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(
      sprintf("Indicator file %s misses columns: %s", path, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!any(c("value", "flag") %in% names(raw))) {
    stop(sprintf("Indicator file %s must contain value and/or flag.", path), call. = FALSE)
  }

  raw |>
    dplyr::mutate(
      country_id = as.character(.data$country_id),
      year = as.integer(.data$year),
      source = as.character(.data$source),
      source_version = as.character(.data$source_version),
      value = if ("value" %in% names(raw)) as.numeric(.data$value) else NA_real_,
      flag = if ("flag" %in% names(raw)) as.integer(.data$flag) else NA_integer_
    )
}

indicatorAllowsNegativeValues <- function(indicator_name) {
  grepl("inflation|cpi", indicator_name, ignore.case = TRUE)
}

validateIndicators <- function(
  indicators,
  indicator_name = "indicator",
  allow_negative_values = NULL
) {
  if (is.null(allow_negative_values)) {
    allow_negative_values <- indicatorAllowsNegativeValues(indicator_name)
  }
  if (nrow(indicators) == 0) {
    return(invisible(TRUE))
  }
  if (anyNA(indicators$country_id) || any(indicators$country_id == "")) {
    stop(sprintf("%s: country_id must be non-empty.", indicator_name), call. = FALSE)
  }
  if (anyNA(indicators$year)) {
    stop(sprintf("%s: year cannot be NA.", indicator_name), call. = FALSE)
  }

  dup <- indicators |>
    dplyr::count(.data$country_id, .data$year, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(sprintf("%s: duplicate country_id × year keys.", indicator_name), call. = FALSE)
  }

  if (
    !isTRUE(allow_negative_values) &&
    "value" %in% names(indicators) &&
    any(!is.na(indicators$value) & indicators$value < 0)
  ) {
    stop(sprintf("%s: negative values are not allowed.", indicator_name), call. = FALSE)
  }
  if ("flag" %in% names(indicators) && any(!is.na(indicators$flag) & !indicators$flag %in% c(0L, 1L))) {
    stop(sprintf("%s: flag must be 0 or 1.", indicator_name), call. = FALSE)
  }

  invisible(TRUE)
}

loadIndicatorsDirectory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    return(list())
  }
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    return(list())
  }
  stats::setNames(
    lapply(files, function(path) {
      ind <- loadIndicatorFile(path)
      indicator_name <- indicatorNameFromCsvBasename(path)
      validateIndicators(ind, indicator_name = indicator_name)
      ind
    }),
    vapply(files, indicatorNameFromCsvBasename, character(1))
  )
}

load_indicator_file <- loadIndicatorFile
validate_indicators <- validateIndicators
load_indicators_directory <- loadIndicatorsDirectory

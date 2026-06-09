requiredMigrationColumns <- function() {
  c("country_id", "year", "net_migration_rate", "data_type", "source", "source_version")
}

validateMigration <- function(migration) {
  required <- requiredMigrationColumns()
  missing <- setdiff(required, names(migration))
  if (length(missing) > 0) {
    stop(
      sprintf("Migration table misses columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (nrow(migration) == 0) {
    return(invisible(TRUE))
  }

  if (anyNA(migration$country_id) || any(migration$country_id == "")) {
    stop("Migration: country_id must be non-empty.", call. = FALSE)
  }
  if (anyNA(migration$year)) {
    stop("Migration: year cannot be NA.", call. = FALSE)
  }
  allowed_types <- c("estimate", "projection")
  if (!all(migration$data_type %in% allowed_types)) {
    stop("Migration: data_type must be 'estimate' or 'projection'.", call. = FALSE)
  }

  dup <- migration |>
    dplyr::count(.data$country_id, .data$year, .data$data_type, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop("Migration: duplicate country_id x year x data_type keys.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_migration <- validateMigration
required_migration_columns <- requiredMigrationColumns

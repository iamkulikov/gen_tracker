PREPARED_MIGRATION_SCHEMA_VERSION <- "1"

MIGRATION_RECENT_WINDOW_YEARS <- 20L

preparedMigrationPath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  env_path <- Sys.getenv("GEN_TRACKER_MIGRATION_PATH", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  file.path(data_dir, "migration.rds")
}

preparedMigrationManifestPath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  file.path(data_dir, "migration_build_manifest.json")
}

resolveMigrationSourcePath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  env_path <- Sys.getenv("GEN_TRACKER_MIGRATION_SOURCE_PATH", unset = "")
  if (nzchar(env_path)) {
    if (!file.exists(env_path)) {
      stop(sprintf("Migration source file not found: %s", env_path), call. = FALSE)
    }
    return(env_path)
  }
  search_dir <- resolveMigrationSourceSearchDir(data_dir)
  paths <- sort(list.files(
    search_dir,
    pattern = "^WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS.*\\.(xlsx|xls)$",
    full.names = TRUE
  ))
  if (length(paths) == 0) {
    stop(
      sprintf("No WPP2024 demographic indicators (F01) Excel file found in %s.", search_dir),
      call. = FALSE
    )
  }
  paths[[1]]
}

# The F01 indicators sheet uses the same banner layout as the single-age files:
# a header row that simultaneously contains "Index", "Year" and the
# "Region, subregion, country or area" label.
detectMigrationHeaderRow <- function(raw_df) {
  candidates <- which(
    vapply(
      seq_len(nrow(raw_df)),
      function(i) {
        row_vals <- trimws(as.character(unlist(raw_df[i, ], use.names = FALSE)))
        any(row_vals == "Index") &&
          any(row_vals == "Year") &&
          any(grepl("Region, subregion, country or area", row_vals, fixed = TRUE))
      },
      logical(1)
    )
  )
  if (length(candidates) == 0) {
    stop("Could not detect header row in migration indicators sheet.", call. = FALSE)
  }
  candidates[[1]]
}

readMigrationIndicatorSheet <- function(
  path,
  sheet,
  data_type,
  source = "UN WPP",
  source_version = "2024"
) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  raw_df <- as.data.frame(raw, stringsAsFactors = FALSE)

  header_row <- detectMigrationHeaderRow(raw_df)
  header_vals <- trimws(as.character(unlist(raw_df[header_row, ], use.names = FALSE)))
  header_vals <- fillEmptyHeaderCells(header_vals)

  data_df <- raw_df[(header_row + 1L):nrow(raw_df), , drop = FALSE]
  names(data_df) <- header_vals

  required_cols <- c("Region, subregion, country or area *", "Location code", "Year", "Type")
  missing_cols <- setdiff(required_cols, names(data_df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Migration sheet %s misses required columns: %s",
        sheet, paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  rate_col <- grep("Net Migration Rate", names(data_df), value = TRUE)[1]
  count_col <- grep("Net Number of Migrants", names(data_df), value = TRUE)[1]
  if (is.na(rate_col)) {
    stop(
      sprintf("Migration sheet %s has no 'Net Migration Rate' column.", sheet),
      call. = FALSE
    )
  }

  iso_col <- if ("ISO3 Alpha-code" %in% names(data_df)) "ISO3 Alpha-code" else "Location code"

  out <- data_df |>
    dplyr::filter(
      !is.na(.data[["Location code"]]),
      !is.na(.data[["Year"]]),
      .data[["Type"]] == "Country/Area"
    ) |>
    dplyr::transmute(
      country_id = as.character(.data[[iso_col]]),
      year = as.integer(.data[["Year"]]),
      net_migration_rate = suppressWarnings(as.numeric(.data[[rate_col]])),
      net_migration = if (!is.na(count_col)) {
        suppressWarnings(as.numeric(.data[[count_col]]))
      } else {
        NA_real_
      },
      data_type = data_type,
      source = source,
      source_version = source_version
    ) |>
    dplyr::filter(!is.na(.data$country_id), .data$country_id != "", !is.na(.data$year))

  out
}

loadMigrationFromExcel <- function(path) {
  estimates <- readMigrationIndicatorSheet(
    path = path,
    sheet = "Estimates",
    data_type = "estimate"
  )
  projection <- readMigrationIndicatorSheet(
    path = path,
    sheet = "Medium variant",
    data_type = "projection"
  )

  dplyr::bind_rows(estimates, projection) |>
    dplyr::arrange(.data$country_id, .data$year)
}

# Query-independent per-country aggregates so runtime reliability scoring stays
# O(1) per country. Built from observed (estimate) years to describe a country's
# structural migration intensity.
computeMigrationCountryFeatures <- function(
  migration,
  recent_window_years = MIGRATION_RECENT_WINDOW_YEARS
) {
  if (nrow(migration) == 0) {
    return(tibble::tibble(
      country_id = character(),
      abs_rate_mean = numeric(),
      abs_rate_recent_mean = numeric(),
      abs_rate_p90 = numeric(),
      abs_rate_max = numeric(),
      years_estimate = integer(),
      years_projection = integer()
    ))
  }

  estimates <- migration |>
    dplyr::filter(.data$data_type == "estimate", is.finite(.data$net_migration_rate))

  recent_floor <- if (nrow(estimates) > 0) {
    max(estimates$year, na.rm = TRUE) - recent_window_years + 1L
  } else {
    NA_integer_
  }

  overall <- estimates |>
    dplyr::summarise(
      abs_rate_mean = mean(abs(.data$net_migration_rate)),
      abs_rate_p90 = stats::quantile(abs(.data$net_migration_rate), 0.9, names = FALSE),
      abs_rate_max = max(abs(.data$net_migration_rate)),
      .by = "country_id"
    )

  recent <- estimates |>
    dplyr::filter(is.na(.env$recent_floor) | .data$year >= .env$recent_floor) |>
    dplyr::summarise(
      abs_rate_recent_mean = mean(abs(.data$net_migration_rate)),
      .by = "country_id"
    )

  coverage <- migration |>
    dplyr::summarise(
      years_estimate = sum(.data$data_type == "estimate"),
      years_projection = sum(.data$data_type == "projection"),
      .by = "country_id"
    )

  overall |>
    dplyr::left_join(recent, by = "country_id") |>
    dplyr::left_join(coverage, by = "country_id") |>
    dplyr::mutate(
      abs_rate_recent_mean = dplyr::coalesce(.data$abs_rate_recent_mean, .data$abs_rate_mean)
    ) |>
    dplyr::arrange(.data$country_id)
}

writePreparedMigrationManifest <- function(manifest, path = preparedMigrationManifestPath()) {
  lines <- c(
    "{",
    sprintf('  "schema_version": "%s",', escapeJsonString(manifest$schema_version)),
    sprintf('  "built_at": "%s",', escapeJsonString(manifest$built_at)),
    sprintf('  "output_path": "%s",', escapeJsonString(manifest$output_path)),
    sprintf('  "source_file": "%s",', escapeJsonString(manifest$source_file)),
    sprintf('  "countries_file": "%s",', escapeJsonString(manifest$countries_file)),
    sprintf('  "country_count": %s,', manifest$country_count),
    sprintf('  "row_count": %s,', manifest$row_count),
    sprintf('  "year_min": %s,', manifest$year_min),
    sprintf('  "year_max": %s,', manifest$year_max),
    sprintf(
      '  "data_types": [%s],',
      paste(sprintf('"%s"', escapeJsonString(manifest$data_types)), collapse = ", ")
    ),
    sprintf('  "file_size_bytes": %s', manifest$file_size_bytes),
    "}"
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

buildPreparedMigration <- function(
  data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data"),
  countries_path = countriesDataPath(data_dir),
  source_path = NULL,
  output_path = preparedMigrationPath(data_dir),
  compress = "gzip"
) {
  if (!file.exists(countries_path)) {
    stop(sprintf("countries file not found: %s", countries_path), call. = FALSE)
  }
  if (is.null(source_path)) {
    source_path <- resolveMigrationSourcePath(data_dir)
  }

  countries <- loadCountryDictionary(countries_path)
  country_ids <- unique(countries$country_id)

  message(sprintf("Loading migration indicators from %s ...", basename(source_path)))
  migration <- loadMigrationFromExcel(source_path)

  missing_in_source <- setdiff(country_ids, unique(migration$country_id))
  if (length(missing_in_source) > 0) {
    warning(
      sprintf(
        "These country_id values are absent in the migration source: %s",
        paste(missing_in_source, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  migration <- migration |>
    dplyr::filter(.data$country_id %in% country_ids)

  validateMigration(migration)

  country_features <- computeMigrationCountryFeatures(migration)

  prepared <- list(
    schema_version = PREPARED_MIGRATION_SCHEMA_VERSION,
    migration = migration,
    country_features = country_features
  )

  output_dir <- dirname(output_path)
  if (nzchar(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(prepared, output_path, compress = compress)

  manifest <- list(
    schema_version = PREPARED_MIGRATION_SCHEMA_VERSION,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    output_path = normalizePath(output_path, winslash = "/", mustWork = FALSE),
    source_file = basename(source_path),
    countries_file = basename(countries_path),
    country_count = length(unique(migration$country_id)),
    row_count = nrow(migration),
    year_min = if (nrow(migration) > 0) min(migration$year, na.rm = TRUE) else NA_integer_,
    year_max = if (nrow(migration) > 0) max(migration$year, na.rm = TRUE) else NA_integer_,
    data_types = sort(unique(migration$data_type)),
    file_size_bytes = file.info(output_path)$size
  )
  writePreparedMigrationManifest(manifest, preparedMigrationManifestPath(data_dir))

  message(sprintf(
    "Saved %s (%s rows, %s countries). Manifest: %s",
    output_path,
    format(manifest$row_count, big.mark = ","),
    manifest$country_count,
    preparedMigrationManifestPath(data_dir)
  ))

  invisible(prepared)
}

loadPreparedMigration <- function(
  path = preparedMigrationPath(),
  required = FALSE
) {
  if (!file.exists(path)) {
    if (required) {
      stop(sprintf("Prepared migration file not found: %s", path), call. = FALSE)
    }
    return(NULL)
  }

  prepared <- readRDS(path)
  if (is.null(prepared$migration)) {
    stop(sprintf("Prepared migration file %s has no 'migration' table.", path), call. = FALSE)
  }
  validateMigration(prepared$migration)
  if (is.null(prepared$country_features)) {
    prepared$country_features <- computeMigrationCountryFeatures(prepared$migration)
  }
  prepared
}

prepared_migration_path <- preparedMigrationPath
build_prepared_migration <- buildPreparedMigration
load_prepared_migration <- loadPreparedMigration
compute_migration_country_features <- computeMigrationCountryFeatures

populationCachePath <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  resolvePopulationCachePath(data_dir)
}

populationSourcesAreFresh <- function(sources, cache_path) {
  if (!file.exists(cache_path) || length(sources) == 0) {
    return(FALSE)
  }
  source_info <- file.info(sources)
  if (any(is.na(source_info$mtime))) {
    return(FALSE)
  }
  file.info(cache_path)$mtime >= max(source_info$mtime)
}

resolvePopulationPaths <- function(data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")) {
  env_paths <- Sys.getenv("GEN_TRACKER_POPULATION_PATHS", unset = "")
  if (nzchar(env_paths)) {
    parts <- trimws(strsplit(env_paths, ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) > 0) {
      cache_name <- basename(populationCachePath(data_dir))
      if (length(parts) == 1L && basename(parts) == cache_name) {
        warning(
          sprintf(
            "%s is the full WPP Excel import cache, not the prepared app artifact. ",
            "Use %s (from scripts/build_prepared_population.R) for normal app startup.",
            parts[[1]],
            preparedPopulationPath(data_dir)
          ),
          call. = FALSE
        )
      }
      return(parts)
    }
  }

  prepared_path <- preparedPopulationPath(data_dir)
  if (file.exists(prepared_path)) {
    return(prepared_path)
  }

  allow_excel <- isTRUE(as.logical(Sys.getenv("GEN_TRACKER_ALLOW_EXCEL_SOURCES", unset = "FALSE")))
  if (!allow_excel) {
    return(character())
  }

  message(
    "Prepared population.rds not found; loading WPP Excel sources ",
    "(set GEN_TRACKER_ALLOW_EXCEL_SOURCES=FALSE to disable)."
  )
  tryCatch(
    resolveWppSourcePaths(data_dir),
    error = function(e) {
      character()
    }
  )
}

fillEmptyHeaderCells <- function(header_vals) {
  header_vals[is.na(header_vals) | header_vals == ""] <- paste0(
    "unnamed_",
    seq_len(sum(is.na(header_vals) | header_vals == ""))
  )
  make.unique(header_vals)
}

resolveSingleAgeHeader <- function(raw_df, header_row) {
  header_vals <- trimws(as.character(unlist(raw_df[header_row, ], use.names = FALSE)))
  data_start <- header_row + 1L

  if (header_row < nrow(raw_df)) {
    next_vals <- trimws(as.character(unlist(raw_df[header_row + 1L, ], use.names = FALSE)))
    if (length(next_vals) < length(header_vals)) {
      next_vals <- c(next_vals, rep("", length(header_vals) - length(next_vals)))
    }

    type_idx <- match("Type", header_vals)
    age_slots <- if (!is.na(type_idx)) seq.int(type_idx + 1L, length(header_vals)) else integer()
    age_labels_in_next <- sum(grepl("^\\d{1,3}$", next_vals[age_slots]), na.rm = TRUE)

    if (length(age_slots) > 0 && age_labels_in_next >= 20L) {
      for (i in age_slots) {
        if ((is.na(header_vals[i]) || header_vals[i] == "") && grepl("^\\d{1,3}$", next_vals[i])) {
          header_vals[i] <- next_vals[i]
        }
      }
      data_start <- header_row + 2L
    }
  }

  list(header = fillEmptyHeaderCells(header_vals), data_start = data_start)
}

identifySingleAgeColumns <- function(column_names) {
  digit_cols <- column_names[grepl("^\\d{1,3}$", column_names)]
  if (length(digit_cols) >= 20L) {
    return(digit_cols)
  }

  ellipsis_cols <- column_names[grepl("^\\.\\.\\.\\d+$", column_names)]
  if (length(ellipsis_cols) >= 20L) {
    return(ellipsis_cols)
  }

  type_idx <- match("Type", column_names)
  if (!is.na(type_idx) && type_idx < length(column_names)) {
    tail_cols <- column_names[(type_idx + 1L):length(column_names)]
    tail_cols <- tail_cols[!grepl("^unnamed_", tail_cols)]
    tail_cols <- tail_cols[!tail_cols %in% c("SortOrder", "Variant", "Notes")]
    if (length(tail_cols) >= 20L) {
      return(tail_cols)
    }
  }

  character()
}

coerceSingleAgeFromLabel <- function(age_label, age_cols) {
  age_chr <- as.character(age_label)
  if (all(grepl("^\\d{1,3}$", age_chr))) {
    return(as.integer(age_chr))
  }

  idx <- match(age_chr, age_cols)
  if (any(is.na(idx))) {
    stop(
      "Could not map age column labels after pivot_longer. ",
      "Examples: ", paste(utils::head(unique(age_chr), 5), collapse = ", "),
      call. = FALSE
    )
  }
  as.integer(idx - 1L)
}

readUnSingleAgeSheet <- function(
  path,
  sheet,
  sex,
  data_type,
  scenario,
  source,
  source_version
) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  raw_df <- as.data.frame(raw, stringsAsFactors = FALSE)

  header_candidates <- which(
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

  if (length(header_candidates) == 0) {
    stop(sprintf("Could not detect header row in %s [%s].", basename(path), sheet))
  }

  header_row <- header_candidates[1]
  header_info <- resolveSingleAgeHeader(raw_df, header_row)
  data_df <- raw_df[header_info$data_start:nrow(raw_df), , drop = FALSE]
  names(data_df) <- header_info$header

  required_cols <- c(
    "Region, subregion, country or area *",
    "Location code",
    "Year",
    "Type"
  )
  missing_cols <- setdiff(required_cols, names(data_df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Sheet %s in %s misses required columns: %s",
      sheet, basename(path), paste(missing_cols, collapse = ", ")
    ))
  }

  age_cols <- identifySingleAgeColumns(names(data_df))
  if (length(age_cols) == 0) {
    stop(sprintf("No single-age columns found in %s [%s].", basename(path), sheet))
  }

  iso_col <- if ("ISO3 Alpha-code" %in% names(data_df)) "ISO3 Alpha-code" else "Location code"
  id_vars <- c("country_id", "country_name", "year")

  data_df |>
    dplyr::filter(!is.na(.data[["Location code"]]), !is.na(.data[["Year"]])) |>
    dplyr::filter(.data[["Type"]] == "Country/Area") |>
    dplyr::transmute(
      country_id = as.character(.data[[iso_col]]),
      country_name = as.character(.data[["Region, subregion, country or area *"]]),
      year = as.integer(.data[["Year"]]),
      dplyr::across(tidyselect::all_of(age_cols), as.numeric)
    ) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(id_vars),
      names_to = "age_label",
      values_to = "population"
    ) |>
    dplyr::mutate(
      age = coerceSingleAgeFromLabel(.data$age_label, age_cols),
      sex = sex,
      data_type = data_type,
      scenario = scenario,
      source = source,
      source_version = source_version
    ) |>
    dplyr::select(-"age_label") |>
    dplyr::filter(!is.na(.data$population), !is.na(.data$country_id), .data$country_id != "")
}

normalizePopulationData <- function(population) {
  required_cols <- c("country_id", "country_name", "year", "sex", "age", "population")
  missing_cols <- setdiff(required_cols, names(population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Population table misses columns: %s", paste(missing_cols, collapse = ", ")))
  }

  if (!"data_type" %in% names(population)) population$data_type <- "estimate"
  if (!"scenario" %in% names(population)) population$scenario <- "baseline"
  if (!"source" %in% names(population)) population$source <- "UN WPP"
  if (!"source_version" %in% names(population)) population$source_version <- "unknown"

  population |>
    dplyr::mutate(
      year = as.integer(year),
      age = as.integer(age),
      population = as.numeric(population),
      sex = as.character(sex),
      country_id = as.character(country_id),
      country_name = as.character(country_name),
      data_type = dplyr::coalesce(.data$data_type, "estimate"),
      scenario = dplyr::coalesce(.data$scenario, "baseline"),
      source = dplyr::coalesce(.data$source, "UN WPP"),
      source_version = dplyr::coalesce(.data$source_version, "unknown")
    ) |>
    dplyr::arrange(.data$country_id, .data$year, .data$sex, .data$age)
}

savePopulationCache <- function(population, cache_path = populationCachePath()) {
  cache_dir <- dirname(cache_path)
  if (nzchar(cache_dir) && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(population, cache_path)
  message("Population cache saved: ", cache_path)
  invisible(cache_path)
}

validateLoadedPopulation <- function(population, path, strict = NULL) {
  if (!exists("validatePopulation", mode = "function")) {
    return(invisible(population))
  }
  use_strict <- populationValidationStrictForPath(path, strict = strict)
  validatePopulation(population, strict = use_strict)
  invisible(population)
}

loadPopulationData <- function(path, write_cache = NA, strict = NULL) {
  if (length(path) == 0) {
    stop(
      "No population data sources found. ",
      missingPreparedPopulationHint(),
      call. = FALSE
    )
  }

  if (is.na(write_cache)) {
    write_cache <- length(path) == 1 &&
      !(length(path) == 1 && tolower(tools::file_ext(path)) == "rds")
  }

  if (length(path) == 1 && tolower(tools::file_ext(path)) == "rds") {
    message("Reading prepared population ", basename(path), " ...")
    population <- readRDS(path)
    validateLoadedPopulation(population, path, strict = strict)
    return(population)
  }

  cache_path <- populationCachePath()
  if (populationSourcesAreFresh(path, cache_path)) {
    return(loadPopulationData(cache_path, write_cache = FALSE, strict = strict))
  }

  if (length(path) > 1) {
    message(
      sprintf(
        "Loading %d population file(s) from Excel (first run can take several minutes)...",
        length(path)
      )
    )
    started <- Sys.time()
    parts <- lapply(path, function(p) loadPopulationData(p, write_cache = FALSE))
    population <- suppressMessages(normalizePopulationData(dplyr::bind_rows(parts)))
    validateLoadedPopulation(population, cache_path, strict = strict)
    savePopulationCache(population, cache_path)
    message(sprintf(
      "Population table ready (%s).",
      format(difftime(Sys.time(), started, units = "secs"), digits = 1)
    ))
    return(population)
  }

  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    csv <- readr::read_csv(path, show_col_types = FALSE)

    if (all(c("Location", "Time", "AgeGrpStart", "PopMale", "PopFemale") %in% names(csv))) {
      out <- csv |>
        dplyr::transmute(
          country_id = as.character(dplyr::coalesce(.data$ISO3_code, .data$LocID, .data$Location)),
          country_name = as.character(.data$Location),
          year = as.integer(.data$Time),
          age = as.integer(.data$AgeGrpStart),
          male = as.numeric(.data$PopMale),
          female = as.numeric(.data$PopFemale)
        ) |>
        tidyr::pivot_longer(
          cols = c("male", "female"),
          names_to = "sex",
          values_to = "population"
        ) |>
        dplyr::mutate(
          data_type = "estimate",
          scenario = "legacy_csv",
          source = "UN WPP",
          source_version = "2019"
        ) |>
        normalizePopulationData()
    } else {
      out <- csv |>
        normalizePopulationData()
    }

    validateLoadedPopulation(out, path, strict = strict)
    if (write_cache) {
      savePopulationCache(out, populationCachePath())
    }
    return(out)
  }

  if (ext %in% c("xlsx", "xls")) {
    base <- basename(path)
    is_wpp <- grepl("^WPP2024_", base)
    is_ppp <- grepl("^UN_PPP2024_", base)
    if (!is_wpp && !is_ppp) {
      stop(sprintf("Unsupported Excel population source: %s", base))
    }

    sex <- dplyr::case_when(
      grepl("Female", base, ignore.case = TRUE) ~ "female",
      grepl("Male", base, ignore.case = TRUE) ~ "male",
      TRUE ~ "all"
    )

    if (is_wpp) {
      estimates <- readUnSingleAgeSheet(
        path = path,
        sheet = "Estimates",
        sex = sex,
        data_type = "estimate",
        scenario = "wpp_estimates",
        source = "UN WPP",
        source_version = "2024"
      )
      medium <- readUnSingleAgeSheet(
        path = path,
        sheet = "Medium variant",
        sex = sex,
        data_type = "projection",
        scenario = "wpp_medium_variant",
        source = "UN WPP",
        source_version = "2024"
      )
      out <- suppressMessages(normalizePopulationData(dplyr::bind_rows(estimates, medium)))
      validateLoadedPopulation(out, path, strict = strict)
      if (write_cache) {
        savePopulationCache(out, populationCachePath())
      }
      return(out)
    }

    median <- readUnSingleAgeSheet(
      path = path,
      sheet = "Median",
      sex = sex,
      data_type = "projection",
      scenario = "ppp_median",
      source = "UN PPP",
      source_version = "2024"
    )
    out <- normalizePopulationData(median)
    validateLoadedPopulation(out, path, strict = strict)
    if (write_cache) {
      savePopulationCache(out, populationCachePath())
    }
    return(out)
  }

  stop(sprintf("Unsupported file extension for population data: %s", ext))
}

normalize_population_data <- normalizePopulationData
load_population_data <- loadPopulationData
resolve_population_paths <- resolvePopulationPaths
population_cache_path <- populationCachePath

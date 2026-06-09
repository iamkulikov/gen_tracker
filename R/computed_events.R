parseCriterionOperator <- function(operator, threshold_value) {
  op <- trimws(as.character(operator))
  threshold <- as.numeric(threshold_value)
  if (grepl("^relative_drop\\s*>=", op)) {
    return(list(kind = "relative_drop", threshold = threshold))
  }
  if (grepl("^relative_increase\\s*>=", op)) {
    return(list(kind = "relative_increase", threshold = threshold))
  }
  if (grepl("^value\\s*>=", op)) {
    return(list(kind = "value_gte", threshold = threshold))
  }
  if (grepl("^flag\\s*==", op)) {
    return(list(kind = "flag_eq", threshold = 1))
  }
  stop(sprintf("Unsupported criterion operator: %s", op), call. = FALSE)
}

applyCriterionToIndicator <- function(indicator, parsed) {
  indicator <- indicator |>
    dplyr::arrange(.data$country_id, .data$year)

  if (parsed$kind == "relative_drop") {
    return(indicator |>
      dplyr::group_by(.data$country_id) |>
      dplyr::arrange(.data$year, .by_group = TRUE) |>
      dplyr::mutate(
        prev_value = dplyr::lag(.data$value),
        relative_drop = (.data$prev_value - .data$value) / .data$prev_value
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(
        is.finite(.data$relative_drop),
        .data$relative_drop >= .env$parsed$threshold
      ))
  }

  if (parsed$kind == "relative_increase") {
    return(indicator |>
      dplyr::group_by(.data$country_id) |>
      dplyr::arrange(.data$year, .by_group = TRUE) |>
      dplyr::mutate(
        prev_value = dplyr::lag(.data$value),
        relative_increase = (.data$value - .data$prev_value) / .data$prev_value
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(
        is.finite(.data$relative_increase),
        .data$relative_increase >= .env$parsed$threshold
      ))
  }

  if (parsed$kind == "value_gte") {
    return(indicator |>
      dplyr::filter(
        is.finite(.data$value),
        .data$value >= .env$parsed$threshold
      ))
  }

  indicator |>
    dplyr::filter(!is.na(.data$flag), .data$flag == 1L)
}

formatComputedEventName <- function(template, country_id, year) {
  out <- as.character(template)
  out <- gsub("{country_id}", country_id, out, fixed = TRUE)
  out <- gsub("{year}", as.character(year), out, fixed = TRUE)
  out
}

formatCriterionTemplateAsTagLabel <- function(template) {
  out <- as.character(template)
  out <- gsub("{country_id}", "", out, fixed = TRUE)
  out <- gsub("\\s*\\(\\{year\\}\\)", "", out, perl = TRUE)
  out <- gsub("\\s+in\\s*$", "", out, perl = TRUE)
  trimws(gsub("\\s{2,}", " ", out, perl = TRUE))
}

tagDisplayLabelFromCriteria <- function(tag, criteria = NULL) {
  if (is.null(criteria) || nrow(criteria) == 0L || !"default_tags" %in% names(criteria)) {
    return(gsub("_", " ", tag))
  }
  for (i in seq_len(nrow(criteria))) {
    raw_tags <- criteria$default_tags[[i]]
    if (is.na(raw_tags) || !nzchar(raw_tags)) {
      next
    }
    tags <- strsplit(as.character(raw_tags), ",", fixed = TRUE)[[1]] |> trimws()
    if (tag %in% tags && "name_template" %in% names(criteria)) {
      label <- formatCriterionTemplateAsTagLabel(criteria$name_template[[i]])
      if (nzchar(label)) {
        return(label)
      }
    }
  }
  gsub("_", " ", tag)
}

computedEventId <- function(criterion_id, country_id, year) {
  sprintf("CMP_%s_%s_%s", criterion_id, country_id, year)
}

buildComputedEventsFromCriteria <- function(indicators_by_name, criteria, countries = NULL) {
  if (nrow(criteria) == 0) {
    return(list(
      events = emptyComputedEventsFrame(),
      event_countries = emptyComputedEventCountriesFrame(),
      event_tags = emptyComputedEventTagsFrame()
    ))
  }

  required <- c(
    "criterion_id", "indicator", "operator", "threshold_value",
    "event_type", "default_event_mode", "name_template"
  )
  if (!"show_in_picker" %in% names(criteria)) {
    criteria$show_in_picker <- FALSE
  }
  missing <- setdiff(required, names(criteria))
  if (length(missing) > 0) {
    stop(
      sprintf("event_criteria misses columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  event_rows <- list()
  link_rows <- list()
  tag_rows <- list()

  for (i in seq_len(nrow(criteria))) {
    row <- criteria[i, ]
    indicator_name <- as.character(row$indicator)
    if (!indicator_name %in% names(indicators_by_name)) {
      stop(sprintf("Criterion %s references unknown indicator: %s", row$criterion_id, indicator_name))
    }
    parsed <- parseCriterionOperator(row$operator, row$threshold_value)
    hits <- applyCriterionToIndicator(indicators_by_name[[indicator_name]], parsed)
    if (nrow(hits) == 0) {
      next
    }

    for (j in seq_len(nrow(hits))) {
      hit <- hits[j, ]
      event_id <- computedEventId(row$criterion_id, hit$country_id, hit$year)
      event_name <- formatComputedEventName(row$name_template, hit$country_id, hit$year)
      show_in_picker <- isTRUE(as.logical(row$show_in_picker[[1]]))
      event_rows[[length(event_rows) + 1L]] <- tibble::tibble(
        event_id = event_id,
        event_name = event_name,
        event_type = as.character(row$event_type),
        event_scope = "national",
        start_year = as.integer(hit$year),
        end_year = as.integer(hit$year),
        peak_year = as.integer(hit$year),
        event_origin = "computed",
        cross_country_allowed = FALSE,
        show_in_picker = show_in_picker,
        criterion_id = as.character(row$criterion_id),
        source = as.character(hit$source),
        source_version = as.character(hit$source_version)
      )
      link_rows[[length(link_rows) + 1L]] <- tibble::tibble(
        event_id = event_id,
        country_id = as.character(hit$country_id),
        country_role = "affected"
      )
      if ("default_tags" %in% names(row) && !is.na(row$default_tags) && nzchar(row$default_tags)) {
        tags <- strsplit(as.character(row$default_tags), ",", fixed = TRUE)[[1]] |>
          trimws()
        tags <- tags[nzchar(tags)]
        for (tag in tags) {
          tag_rows[[length(tag_rows) + 1L]] <- tibble::tibble(
            event_id = event_id,
            tag = tag
          )
        }
      }
    }
  }

  events <- if (length(event_rows) == 0) {
    emptyComputedEventsFrame()
  } else {
    dplyr::bind_rows(event_rows)
  }

  event_countries <- if (length(link_rows) == 0) {
    emptyComputedEventCountriesFrame()
  } else {
    dplyr::bind_rows(link_rows) |> dplyr::distinct()
  }

  if (!is.null(countries) && nrow(event_countries) > 0) {
    orphan <- setdiff(event_countries$country_id, countries$country_id)
    if (length(orphan) > 0) {
      stop(
        "Computed event countries missing from countries dictionary: ",
        paste(utils::head(orphan, 5), collapse = ", "),
        call. = FALSE
      )
    }
  }

  event_tags <- if (length(tag_rows) == 0) {
    emptyComputedEventTagsFrame()
  } else {
    dplyr::bind_rows(tag_rows) |> dplyr::distinct()
  }

  list(events = events, event_countries = event_countries, event_tags = event_tags)
}

emptyComputedEventsFrame <- function() {
  tibble::tibble(
    event_id = character(),
    event_name = character(),
    event_type = character(),
    event_scope = character(),
    start_year = integer(),
    end_year = integer(),
    peak_year = integer(),
    event_origin = character(),
    cross_country_allowed = logical(),
    show_in_picker = logical(),
    criterion_id = character(),
    source = character(),
    source_version = character()
  )
}

emptyComputedEventCountriesFrame <- function() {
  tibble::tibble(
    event_id = character(),
    country_id = character(),
    country_role = character()
  )
}

emptyComputedEventTagsFrame <- function() {
  tibble::tibble(
    event_id = character(),
    tag = character()
  )
}

loadEventCriteria <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

writeComputedEventsManifest <- function(path, criteria, indicators_by_name, events, generated_at = Sys.time()) {
  lines <- c(
    "{",
    sprintf('  "generated_at": "%s",', format(generated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    sprintf('  "criteria_rows": %d,', nrow(criteria)),
    sprintf('  "computed_event_rows": %d,', nrow(events)),
    '  "criteria_ids": [',
    paste(
      sprintf('    "%s"', unique(criteria$criterion_id)),
      collapse = ",\n"
    ),
    "  ],",
    '  "indicators": ['
  )
  indicator_lines <- lapply(names(indicators_by_name), function(name) {
    ind <- indicators_by_name[[name]]
    sprintf(
      '    {"indicator": "%s", "rows": %d}',
      name,
      nrow(ind)
    )
  })
  lines <- c(
    lines,
    paste(indicator_lines, collapse = ",\n"),
    "  ]",
    "}"
  )
  writeLines(lines, path, useBytes = TRUE)
}

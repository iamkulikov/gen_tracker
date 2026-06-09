parseClassifierSpec <- function(spec) {
  spec <- trimws(as.character(spec))
  m <- regexec('^tag\\s*==\\s*"([^"]+)"\\s*$', spec, perl = TRUE)
  hit <- regmatches(spec, m)[[1]]
  if (length(hit) < 2) {
    stop(sprintf("Unsupported classifier_spec: %s", spec), call. = FALSE)
  }
  list(kind = "tag", tag = hit[[2]])
}

classifierTagSlug <- function(tag) {
  slug <- gsub("[^A-Za-z0-9]+", "_", tag)
  slug <- gsub("^_+|_+$", "", slug)
  toupper(slug)
}

compositeEventId <- function(tag, country_id) {
  sprintf("MERGE_%s_%s", classifierTagSlug(tag), country_id)
}

countryIdFromComputedEventId <- function(event_id) {
  sub("^CMP_[^_]+_([A-Z]{3})_[0-9]+$", "\\1", event_id)
}

resolveCountryDisplayName <- function(country_id, countries = NULL) {
  if (!is.null(countries) && nrow(countries) > 0L) {
    row <- countries |>
      dplyr::filter(.data$country_id == .env$country_id) |>
      dplyr::slice(1)
    if (nrow(row) > 0L && nzchar(row$country_name[[1]])) {
      return(row$country_name[[1]])
    }
  }
  country_id
}

buildCompositeEventsFromTags <- function(
  events,
  event_tags,
  classifier_spec,
  event_countries = NULL,
  countries = NULL,
  criteria = NULL,
  composite_name = NULL
) {
  parsed <- parseClassifierSpec(classifier_spec)
  tagged <- event_tags |>
    dplyr::filter(.data$tag == .env$parsed$tag)

  if (nrow(tagged) == 0) {
    return(list(
      composite_events = emptyCompositeEventsFrame(),
      composite_members = emptyCompositeMembersFrame(),
      event_countries = emptyComputedEventCountriesFrame()
    ))
  }

  members <- tagged |>
    dplyr::inner_join(
      events |> dplyr::select("event_id", "start_year", "end_year", "peak_year", "event_type"),
      by = "event_id"
    )

  if (!is.null(event_countries) && nrow(event_countries) > 0) {
    member_countries <- members |>
      dplyr::inner_join(
        event_countries |> dplyr::filter(.data$country_role == "affected"),
        by = "event_id"
      )
  } else {
    member_countries <- members |>
      dplyr::mutate(country_id = countryIdFromComputedEventId(.data$event_id))
  }

  member_countries <- member_countries |>
    dplyr::filter(!is.na(.data$country_id), nzchar(.data$country_id))

  if (!is.null(countries) && nrow(countries) > 0) {
    member_countries <- member_countries |>
      dplyr::filter(.data$country_id %in% countries$country_id)
  }

  composite_headers <- list()
  composite_member_rows <- list()
  link_rows <- list()

  country_ids <- unique(member_countries$country_id)
  for (country_id in country_ids) {
    group_members <- member_countries |>
      dplyr::filter(.data$country_id == .env$country_id) |>
      dplyr::distinct(.data$event_id, .keep_all = TRUE)

    if (nrow(group_members) < 2L) {
      next
    }

    composite_id <- compositeEventId(parsed$tag, country_id)
    tag_label <- tagDisplayLabelFromCriteria(parsed$tag, criteria = criteria)
    country_label <- resolveCountryDisplayName(country_id, countries = countries)
    composite_label <- composite_name %||% tag_label
    composite_label <- trimws(as.character(composite_label))
    if (!nzchar(composite_label)) {
      composite_label <- tag_label
    }
    if (!is.null(composite_name) && nzchar(trimws(as.character(composite_name)))) {
      event_name <- sprintf(
        "%s в %s (%d эпизодов)",
        composite_label,
        country_label,
        nrow(group_members)
      )
    } else {
      event_name <- sprintf(
        "%s in %s (%d episodes)",
        composite_label,
        country_label,
        nrow(group_members)
      )
    }

    composite_headers[[length(composite_headers) + 1L]] <- tibble::tibble(
      composite_event_id = composite_id,
      event_name = event_name,
      event_scope = "national",
      event_type = dplyr::first(group_members$event_type),
      country_id = country_id,
      default_event_mode = "start",
      classifier_spec = classifier_spec,
      description = sprintf("Per-country composite for tag %s.", parsed$tag),
      event_origin = "composite",
      start_year = min(group_members$start_year, na.rm = TRUE),
      end_year = max(group_members$end_year, na.rm = TRUE),
      peak_year = max(group_members$peak_year, na.rm = TRUE)
    )

    for (member_id in group_members$event_id) {
      composite_member_rows[[length(composite_member_rows) + 1L]] <- tibble::tibble(
        composite_event_id = composite_id,
        member_event_id = member_id
      )
    }

    link_rows[[length(link_rows) + 1L]] <- tibble::tibble(
      event_id = composite_id,
      country_id = country_id,
      country_role = "affected"
    )
  }

  list(
    composite_events = if (length(composite_headers) == 0) {
      emptyCompositeEventsFrame()
    } else {
      dplyr::bind_rows(composite_headers)
    },
    composite_members = if (length(composite_member_rows) == 0) {
      emptyCompositeMembersFrame()
    } else {
      dplyr::bind_rows(composite_member_rows) |> dplyr::distinct()
    },
    event_countries = if (length(link_rows) == 0) {
      emptyComputedEventCountriesFrame()
    } else {
      dplyr::bind_rows(link_rows) |> dplyr::distinct()
    }
  )
}

emptyCompositeEventsFrame <- function() {
  tibble::tibble(
    composite_event_id = character(),
    event_name = character(),
    event_scope = character(),
    event_type = character(),
    country_id = character(),
    default_event_mode = character(),
    classifier_spec = character(),
    description = character(),
    event_origin = character(),
    start_year = integer(),
    end_year = integer(),
    peak_year = integer()
  )
}

emptyCompositeMembersFrame <- function() {
  tibble::tibble(
    composite_event_id = character(),
    member_event_id = character()
  )
}

loadCompositeEvents <- function(path) {
  if (!file.exists(path)) {
    return(emptyCompositeEventsFrame())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

loadCompositeMembers <- function(path) {
  if (!file.exists(path)) {
    return(emptyCompositeMembersFrame())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

loadEventTags <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) {
    return(emptyComputedEventTagsFrame())
  }
  tag_tables <- lapply(paths, readr::read_csv, show_col_types = FALSE)
  dplyr::bind_rows(tag_tables) |>
    dplyr::mutate(
      event_id = as.character(.data$event_id),
      tag = as.character(.data$tag)
    ) |>
    dplyr::distinct()
}

compositeEventsAsCatalog <- function(composite_events) {
  if (nrow(composite_events) == 0) {
    return(tibble::tibble())
  }
  composite_events |>
    dplyr::transmute(
      event_id = .data$composite_event_id,
      event_name = .data$event_name,
      event_type = .data$event_type,
      event_scope = .data$event_scope,
      start_year = as.integer(.data$start_year),
      end_year = as.integer(.data$end_year),
      peak_year = as.integer(.data$peak_year),
      event_origin = "composite",
      cross_country_allowed = FALSE,
      show_in_picker = TRUE,
      country_id = as.character(.data$country_id)
    )
}

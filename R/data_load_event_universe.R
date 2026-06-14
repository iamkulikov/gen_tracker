normalizeEventsCatalog <- function(events) {
  if (!"event_origin" %in% names(events)) {
    events$event_origin <- "manual"
  }
  if (!"cross_country_allowed" %in% names(events)) {
    events$cross_country_allowed <- FALSE
  }
  if (!"country_id" %in% names(events)) {
    events$country_id <- NA_character_
  }
  if (!"show_in_picker" %in% names(events)) {
    events$show_in_picker <- NA
  }

  events |>
    dplyr::mutate(
      event_id = as.character(.data$event_id),
      event_name = as.character(.data$event_name),
      event_type = as.character(.data$event_type),
      event_scope = as.character(.data$event_scope),
      start_year = as.integer(.data$start_year),
      end_year = as.integer(.data$end_year),
      peak_year = as.integer(.data$peak_year),
      event_origin = dplyr::coalesce(.data$event_origin, "manual"),
      cross_country_allowed = dplyr::coalesce(as.logical(.data$cross_country_allowed), FALSE),
      country_id = as.character(.data$country_id),
      show_in_picker = dplyr::case_when(
        .data$event_origin == "manual" ~ TRUE,
        .data$event_origin == "composite" ~ TRUE,
        .data$event_origin == "computed" ~ dplyr::coalesce(as.logical(.data$show_in_picker), FALSE),
        TRUE ~ TRUE
      )
    ) |>
    normalizeEventCurationFields()
}

eventsForEventPicker <- function(events) {
  if (is.null(events) || nrow(events) == 0) {
    return(events)
  }
  events |> dplyr::filter(.data$show_in_picker)
}

assertNoEventIdCollisions <- function(manual_events, computed_events) {
  if (nrow(computed_events) == 0) {
    return(invisible(TRUE))
  }
  bad_manual <- manual_events$event_id[grepl("^CMP_", manual_events$event_id)]
  if (length(bad_manual) > 0) {
    stop(
      "Manual events must not use CMP_ prefix: ",
      paste(utils::head(bad_manual, 5), collapse = ", "),
      call. = FALSE
    )
  }
  overlap <- intersect(manual_events$event_id, computed_events$event_id)
  if (length(overlap) > 0) {
    stop(
      "event_id collision between manual and computed events: ",
      paste(utils::head(overlap, 5), collapse = ", "),
      call. = FALSE
    )
  }
  non_prefixed <- computed_events$event_id[!grepl("^CMP_", computed_events$event_id)]
  if (length(non_prefixed) > 0) {
    stop(
      "Computed events must use CMP_ prefix: ",
      paste(utils::head(non_prefixed, 5), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

loadComputedEventsCatalog <- function(path) {
  if (!file.exists(path)) {
    return(emptyComputedEventsFrame())
  }
  raw <- readr::read_csv(path, show_col_types = FALSE)
  normalizeEventsCatalog(raw)
}

loadEventCountryLayer <- function(path) {
  if (!file.exists(path)) {
    return(emptyComputedEventCountriesFrame())
  }
  loadEventCountries(path)
}

loadEventTagLayer <- function(path) {
  if (!file.exists(path)) {
    return(emptyComputedEventTagsFrame())
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      event_id = as.character(.data$event_id),
      tag = as.character(.data$tag)
    )
}

tagLayerWithOrigin <- function(tags, origin) {
  if (nrow(tags) == 0) {
    return(emptyComputedEventTagsFrame())
  }
  tags |>
    dplyr::mutate(origin = origin) |>
    dplyr::select("event_id", "tag", "origin")
}

linkLayerWithOrigin <- function(links, origin) {
  if (nrow(links) == 0) {
    return(tibble::tibble(
      event_id = character(),
      country_id = character(),
      country_role = character(),
      origin = character()
    ))
  }
  links |>
    dplyr::mutate(origin = origin) |>
    dplyr::select("event_id", "country_id", "country_role", "origin")
}

mergeEventCountriesWithOrigin <- function(manual_links, computed_links, composite_links) {
  dplyr::bind_rows(
    linkLayerWithOrigin(manual_links, "manual"),
    linkLayerWithOrigin(computed_links, "computed"),
    linkLayerWithOrigin(composite_links, "composite")
  ) |>
    dplyr::distinct(.data$event_id, .data$country_id, .keep_all = TRUE)
}

mergeEventTagsWithOrigin <- function(manual_tags, computed_tags) {
  dplyr::bind_rows(
    tagLayerWithOrigin(manual_tags, "manual"),
    tagLayerWithOrigin(computed_tags, "computed")
  ) |>
    dplyr::distinct(.data$event_id, .data$tag, .keep_all = TRUE)
}

stripEventCountriesOrigin <- function(links) {
  if (!"origin" %in% names(links)) {
    return(links)
  }
  links |> dplyr::select(-dplyr::any_of("origin"))
}

stripEventTagsOrigin <- function(tags) {
  if (!"origin" %in% names(tags)) {
    return(tags)
  }
  tags |> dplyr::select(-dplyr::any_of("origin"))
}

writeEventCountryLayer <- function(links, path, origin = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  out <- links
  if (!is.null(origin) && nrow(out) > 0 && !"origin" %in% names(out)) {
    out <- linkLayerWithOrigin(out, origin)
  }
  if ("origin" %in% names(out)) {
    out <- stripEventCountriesOrigin(out)
  }
  readr::write_csv(out, path)
  invisible(path)
}

writeEventTagLayer <- function(tags, path, origin = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  out <- tags
  if (!is.null(origin) && nrow(out) > 0 && !"origin" %in% names(out)) {
    out <- tagLayerWithOrigin(out, origin)
  }
  if ("origin" %in% names(out)) {
    out <- stripEventTagsOrigin(out)
  }
  readr::write_csv(out, path)
  invisible(path)
}

mergeDeployEventLinks <- function(
  data_dir = resolveDataDir(),
  manual_links = NULL,
  computed_links = NULL,
  composite_links = NULL,
  manual_path = NULL,
  computed_path = NULL,
  composite_path = NULL,
  deploy_path = NULL
) {
  manual_path <- manual_path %||% eventCountriesManualLayerPath(data_dir)
  computed_path <- computed_path %||% eventCountriesComputedLayerPath(data_dir)
  composite_path <- composite_path %||% eventCountriesCompositeLayerPath(data_dir)
  deploy_path <- deploy_path %||% resolveEventDataPath("event_countries.csv", data_dir)

  manual_links <- manual_links %||% loadEventCountryLayer(manual_path)
  computed_links <- computed_links %||% loadEventCountryLayer(computed_path)
  composite_links <- composite_links %||% loadEventCountryLayer(composite_path)

  merged <- mergeEventCountriesWithOrigin(manual_links, computed_links, composite_links)
  dir.create(dirname(deploy_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(merged, deploy_path)

  list(
    deploy_path = deploy_path,
    links = merged,
    manual_rows = nrow(manual_links),
    computed_rows = nrow(computed_links),
    composite_rows = nrow(composite_links)
  )
}

mergeDeployEventTags <- function(
  data_dir = resolveDataDir(),
  manual_tags = NULL,
  computed_tags = NULL,
  manual_path = NULL,
  computed_path = NULL,
  deploy_path = NULL
) {
  manual_path <- manual_path %||% eventTagsManualLayerPath(data_dir)
  computed_path <- computed_path %||% eventTagsComputedLayerPath(data_dir)
  deploy_path <- deploy_path %||% resolveEventDataPath("event_tags.csv", data_dir)

  manual_tags <- manual_tags %||% loadEventTagLayer(manual_path)
  computed_tags <- computed_tags %||% loadEventTagLayer(computed_path)

  merged <- mergeEventTagsWithOrigin(manual_tags, computed_tags)
  dir.create(dirname(deploy_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(merged, deploy_path)

  list(
    deploy_path = deploy_path,
    tags = merged,
    manual_rows = nrow(manual_tags),
    computed_rows = nrow(computed_tags)
  )
}

mergeEventCountriesUniverse <- function(manual_links, computed_links) {
  dplyr::bind_rows(manual_links, computed_links) |>
    dplyr::distinct(.data$event_id, .data$country_id, .keep_all = TRUE)
}

loadEventsUniverse <- function(
  manual_path,
  computed_path = NULL,
  composite_events_path = NULL,
  composite_members_path = NULL,
  data_dir = NULL
) {
  data_dir <- data_dir %||% dirname(manual_path)
  manual <- loadEvents(manual_path)
  computed_path <- computed_path %||% resolveEventDataPath("events_computed.csv", data_dir)
  computed <- loadComputedEventsCatalog(computed_path)
  assertNoEventIdCollisions(manual, computed)

  composite_events_path <- composite_events_path %||%
    resolveEventDataPath("composite_events.csv", data_dir)
  composite_members_path <- composite_members_path %||%
    resolveEventDataPath("composite_members.csv", data_dir)

  composite_events <- loadCompositeEvents(composite_events_path)
  composite_members <- loadCompositeMembers(composite_members_path)
  composite_catalog <- compositeEventsAsCatalog(composite_events)

  composite_ids <- if (nrow(composite_catalog) > 0) composite_catalog$event_id else character(0)
  overlap_composite <- intersect(c(manual$event_id, computed$event_id), composite_ids)
  if (length(overlap_composite) > 0) {
    stop(
      "Composite event_id collision: ",
      paste(utils::head(overlap_composite, 5), collapse = ", "),
      call. = FALSE
    )
  }

  elementary <- dplyr::bind_rows(manual, computed) |>
    normalizeEventsCatalog()

  events <- dplyr::bind_rows(elementary, composite_catalog) |>
    normalizeEventsCatalog()

  list(
    events = events,
    composite_members = composite_members,
    elementary_events = elementary
  )
}

loadEventCountriesUniverse <- function(
  manual_path,
  computed_path = NULL,
  composite_links_path = NULL,
  data_dir = NULL
) {
  data_dir <- data_dir %||% dirname(manual_path)
  deploy_path <- resolveEventDataPath("event_countries.csv", data_dir)

  if (file.exists(deploy_path)) {
    deploy_links <- loadEventCountries(deploy_path)
    if ("origin" %in% names(deploy_links)) {
      return(stripEventCountriesOrigin(deploy_links))
    }
  }

  manual <- if (file.exists(manual_path)) loadEventCountries(manual_path) else {
    emptyComputedEventCountriesFrame()
  }
  computed_path <- computed_path %||% eventCountriesComputedLayerPath(data_dir)
  if (!file.exists(computed_path)) {
    computed_path <- resolveEventDataPath("event_countries_computed.csv", data_dir)
  }
  computed <- loadEventCountryLayer(computed_path)

  composite_links_path <- composite_links_path %||% eventCountriesCompositeLayerPath(data_dir)
  if (!file.exists(composite_links_path)) {
    composite_links_path <- resolveEventDataPath("event_countries_composite.csv", data_dir)
  }
  composite_links <- loadEventCountryLayer(composite_links_path)

  using_legacy_split <- nrow(computed) > 0 || nrow(composite_links) > 0
  if (using_legacy_split) {
    warning(
      "Loading split event_countries layers; run build scripts to write merged deploy file.",
      call. = FALSE
    )
  }

  mergeEventCountriesUniverse(
    mergeEventCountriesUniverse(manual, computed),
    composite_links
  )
}

loadEventTagsUniverse <- function(data_dir = resolveDataDir()) {
  deploy_path <- resolveEventDataPath("event_tags.csv", data_dir)
  if (file.exists(deploy_path)) {
    tags <- loadEventTagLayer(deploy_path)
    if ("origin" %in% names(tags)) {
      return(stripEventTagsOrigin(tags))
    }
    return(tags)
  }

  manual <- loadEventTagLayer(eventTagsManualLayerPath(data_dir))
  if (nrow(manual) == 0) {
    legacy_manual <- resolveEventDataPath("event_tags.csv", data_dir)
    if (file.exists(legacy_manual)) {
      manual <- loadEventTagLayer(legacy_manual)
    }
  }

  computed_path <- eventTagsComputedLayerPath(data_dir)
  if (!file.exists(computed_path)) {
    computed_path <- resolveEventDataPath("event_tags_computed.csv", data_dir)
  }
  computed <- loadEventTagLayer(computed_path)

  if (nrow(computed) > 0) {
    warning(
      "Loading split event_tags layers; run build scripts to write merged deploy file.",
      call. = FALSE
    )
  }

  dplyr::bind_rows(manual, computed) |>
    dplyr::distinct(.data$event_id, .data$tag, .keep_all = TRUE)
}

validateCompositeMembers <- function(events, composite_members) {
  if (nrow(composite_members) == 0) {
    return(invisible(TRUE))
  }
  composite_ids <- events$event_id[events$event_origin == "composite"]
  orphan_composites <- setdiff(composite_members$composite_event_id, composite_ids)
  if (length(orphan_composites) > 0) {
    stop("composite_members reference unknown composite_event_id values.", call. = FALSE)
  }

  elementary_ids <- events$event_id[events$event_origin != "composite"]
  orphan_members <- setdiff(composite_members$member_event_id, elementary_ids)
  if (length(orphan_members) > 0) {
    stop(
      "composite_members reference unknown elementary event_id values: ",
      paste(utils::head(orphan_members, 5), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

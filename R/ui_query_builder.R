queryBuilderDefaultCountryId <- function(countries, preferred = "RUS") {
  if (is.null(countries) || nrow(countries) == 0) {
    return(preferred)
  }
  if (preferred %in% countries$country_id) {
    return(preferred)
  }
  countries$country_id[[1]]
}

resolveQueryBuilderCountryId <- function(selected, countries, preferred = "RUS") {
  if (!is.null(selected) && length(selected) == 1L && !is.na(selected) && nzchar(selected)) {
    if (selected %in% countries$country_id) {
      return(as.character(selected))
    }
  }
  queryBuilderDefaultCountryId(countries, preferred = preferred)
}

lookupQueryBuilderEvent <- function(events_df, event_id) {
  if (
    is.null(event_id) ||
      length(event_id) != 1L ||
      is.na(event_id) ||
      !nzchar(event_id) ||
      is.null(events_df) ||
      nrow(events_df) == 0
  ) {
    return(NULL)
  }

  event_row <- events_df |>
    dplyr::filter(.data$event_id == .env$event_id) |>
    dplyr::slice(1)
  if (nrow(event_row) == 0) {
    return(NULL)
  }
  event_row
}

queryBuilderEventInfoUi <- function(event_row) {
  if (is.null(event_row) || nrow(event_row) == 0) {
    return(NULL)
  }

  short_desc <- NA_character_
  source_url <- NA_character_
  if ("short_description" %in% names(event_row)) {
    short_desc <- event_row$short_description[[1]] %||% NA_character_
  }
  if ("source_url" %in% names(event_row)) {
    source_url <- event_row$source_url[[1]] %||% NA_character_
  }

  has_desc <- !is.na(short_desc) && nzchar(trimws(short_desc))
  has_source <- !is.na(source_url) && nzchar(trimws(source_url))
  if (!has_desc && !has_source) {
    return(NULL)
  }

  popover_children <- list()
  if (has_desc) {
    popover_children <- c(popover_children, list(
      htmltools::tags$p(class = "query-event-info-desc", short_desc)
    ))
  }
  if (has_source) {
    popover_children <- c(popover_children, list(
      htmltools::tags$p(
        class = "query-event-info-meta",
        htmltools::tags$a(
          class = "query-event-info-link",
          href = source_url,
          target = "_blank",
          rel = "noopener noreferrer",
          "Source"
        )
      )
    ))
  }

  htmltools::tags$span(
    class = "query-event-info",
    htmltools::tags$span(
      class = "query-event-info-trigger",
      tabindex = "0",
      role = "button",
      `aria-label` = "Event details",
      shiny::icon("info-circle")
    ),
    htmltools::tags$span(
      class = "query-event-info-popover",
      role = "tooltip",
      popover_children
    )
  )
}

queryBuilderInlineSelectInput <- function(
  inputId,
  choices,
  selected = NULL
) {
  shiny::selectInput(
    inputId = inputId,
    label = NULL,
    choices = choices,
    selected = selected,
    selectize = FALSE,
    width = "100%"
  )
}

queryBuilderEventSelectInput <- function(
  inputId,
  events_df,
  country_id,
  event_countries = NULL,
  country_name = NULL,
  selected = NULL
) {
  event_select_class <- "shiny-input-select form-control query-inline-select query-event-select"

  if (nrow(events_df) == 0) {
    return(htmltools::tags$select(
      id = inputId,
      class = event_select_class
    ))
  }

  groups <- partitionEventPickerIndices(
    events_df = events_df,
    country_id = country_id,
    event_countries = event_countries
  )

  build_option <- function(i, group) {
    label <- formatEventPickerOptionLabel(
      event_name = events_df$event_name[[i]],
      start_year = events_df$start_year[[i]],
      end_year = events_df$end_year[[i]],
      event_id = events_df$event_id[[i]],
      group = group
    )
    title_attr <- NULL
    if ("short_description" %in% names(events_df)) {
      desc <- events_df$short_description[[i]]
      if (!is.na(desc) && nzchar(desc)) {
        title_attr <- desc
      }
    }

    is_primary <- group == "primary"
    opt <- htmltools::tags$option(
      value = events_df$event_id[[i]],
      label,
      title = title_attr,
      style = if (is_primary) "font-weight:700" else NULL
    )
    if (!is.null(selected) && identical(events_df$event_id[[i]], selected)) {
      opt <- htmltools::tagAppendAttributes(opt, selected = "selected")
    }
    opt
  }

  build_group <- function(label, indices, group) {
    if (length(indices) == 0L) {
      return(NULL)
    }
    do.call(
      htmltools::tags$optgroup,
      c(
        list(label = label),
        lapply(indices, build_option, group = group)
      )
    )
  }

  country_label <- country_name %||% country_id
  children <- list(
    build_group(sprintf("Events for %s", country_label), groups$primary, "primary"),
    build_group("Global events", groups$global, "global"),
    build_group("Other events", groups$other, "other"),
    build_group("Years", groups$years, "years")
  )
  children <- children[!vapply(children, is.null, logical(1))]

  if (length(children) == 0) {
    children <- lapply(seq_len(nrow(events_df)), function(i) {
      build_option(i, "primary")
    })
  }

  do.call(
    htmltools::tags$select,
    c(
      list(id = inputId, class = event_select_class),
      children
    )
  )
}

queryBuilderUi <- function(id, line_color = NULL) {
  ns <- shiny::NS(id)
  card_style <- if (!is.null(line_color) && length(line_color) == 1L && nzchar(line_color)) {
    sprintf("--gt-query-color: %s;", line_color)
  } else {
    NULL
  }

  shiny::tagList(
    shiny::div(
      class = "query-builder-card",
      style = card_style,
      shiny::div(
        class = "query-sentence",
        shiny::span(class = "query-chunk query-chunk-sex", queryBuilderInlineSelectInput(
          ns("sex"),
          choices = queryBuilderSexChoices(),
          selected = "all"
        )),
        shiny::span(class = "query-plain", "in"),
        shiny::span(class = "query-chunk query-chunk-country", shiny::uiOutput(ns("country_ui"), inline = TRUE)),
        shiny::span(class = "query-plain", "who were"),
        shiny::span(class = "query-chunk query-chunk-complement", queryBuilderInlineSelectInput(
          ns("age_modifier"),
          choices = queryBuilderComplementChoices(),
          selected = "none"
        )),
        shiny::span(
          class = "query-chunk query-chunk-age",
          shiny::uiOutput(ns("age_status_ui"), inline = TRUE),
          shiny::uiOutput(ns("custom_age_ui"), inline = TRUE)
        ),
        shiny::span(class = "query-chunk query-chunk-mode", queryBuilderInlineSelectInput(
          ns("event_mode"),
          choices = queryBuilderEventModeChoices(),
          selected = "start"
        )),
        shiny::span(
          class = "query-chunk-event-row",
          shiny::span(class = "query-chunk query-chunk-event", shiny::uiOutput(ns("event_ui"), inline = TRUE)),
          shiny::uiOutput(ns("event_info_ui"), inline = TRUE)
        ),
        shiny::span(class = "query-plain query-sentence-end", ".")
      ),
      shiny::uiOutput(ns("validity_ui"))
    )
  )
}

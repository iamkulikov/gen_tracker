queryBuilderDefaultCountryId <- function(countries, preferred = "RUS") {
  if (is.null(countries) || nrow(countries) == 0) {
    return(preferred)
  }
  if (preferred %in% countries$country_id) {
    return(preferred)
  }
  countries$country_id[[1]]
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
    width = "auto"
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
  if (nrow(events_df) == 0) {
    return(htmltools::tags$select(
      id = inputId,
      class = "query-inline-select query-event-select"
    ))
  }

  is_primary <- vapply(seq_len(nrow(events_df)), function(i) {
    isEventPrimaryForCountry(
      event_id = events_df$event_id[[i]],
      event_scope = events_df$event_scope[[i]],
      country_id = country_id,
      event_countries = event_countries
    )
  }, logical(1))

  build_option <- function(i) {
    label <- formatEventChoiceLabel(
      event_name = events_df$event_name[[i]],
      start_year = events_df$start_year[[i]],
      end_year = events_df$end_year[[i]]
    )
    opt <- htmltools::tags$option(
      value = events_df$event_id[[i]],
      label
    )
    if (is_primary[[i]]) {
      opt <- htmltools::tagAppendAttributes(opt, style = "font-weight:700")
    }
    if (!is.null(selected) && identical(events_df$event_id[[i]], selected)) {
      opt <- htmltools::tagAppendAttributes(opt, selected = "selected")
    }
    opt
  }

  is_primary <- vapply(seq_len(nrow(events_df)), function(i) {
    isEventPrimaryForCountry(
      event_id = events_df$event_id[[i]],
      event_scope = events_df$event_scope[[i]],
      country_id = country_id,
      event_countries = event_countries
    )
  }, logical(1))

  is_global <- vapply(events_df$event_scope, function(scope) {
    identical(scope, "global")
  }, logical(1))

  primary_idx <- which(is_primary)
  global_idx <- which(!is_primary & is_global)
  other_idx <- which(!is_primary & !is_global)
  country_label <- country_name %||% country_id

  children <- list()
  if (length(primary_idx) > 0) {
    children <- c(
      children,
      list(htmltools::tags$optgroup(
        label = sprintf("Events for %s", country_label),
        lapply(primary_idx, build_option)
      ))
    )
  }
  if (length(global_idx) > 0) {
    children <- c(
      children,
      list(htmltools::tags$optgroup(
        label = "Global events",
        lapply(global_idx, build_option)
      ))
    )
  }
  if (length(other_idx) > 0) {
    children <- c(
      children,
      list(htmltools::tags$optgroup(
        label = "Other events",
        lapply(other_idx, build_option)
      ))
    )
  }
  if (length(children) == 0) {
    children <- lapply(seq_len(nrow(events_df)), build_option)
  }

  htmltools::tags$select(
    id = inputId,
    class = "query-inline-select query-event-select",
    children
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
        shiny::span(class = "query-chunk", queryBuilderInlineSelectInput(
          ns("sex"),
          choices = queryBuilderSexChoices(),
          selected = "all"
        )),
        shiny::span(class = "query-plain", "in"),
        shiny::span(class = "query-chunk query-chunk-country", shiny::uiOutput(ns("country_ui"), inline = TRUE)),
        shiny::span(class = "query-plain", "who were"),
        shiny::span(
          class = "query-chunk query-chunk-age",
          shiny::uiOutput(ns("age_group_ui"), inline = TRUE),
          shiny::uiOutput(ns("custom_age_ui"), inline = TRUE)
        ),
        shiny::span(class = "query-plain", "and"),
        shiny::span(class = "query-chunk", queryBuilderInlineSelectInput(
          ns("operator"),
          choices = queryBuilderOperatorChoices(),
          selected = "experienced"
        )),
        shiny::span(class = "query-chunk query-chunk-event", shiny::uiOutput(ns("event_ui"), inline = TRUE)),
        shiny::span(class = "query-chunk", queryBuilderInlineSelectInput(
          ns("event_mode"),
          choices = queryBuilderEventModeChoices(),
          selected = "start"
        )),
        shiny::span(class = "query-plain query-sentence-end", ".")
      ),
      shiny::uiOutput(ns("validity_ui"))
    )
  )
}

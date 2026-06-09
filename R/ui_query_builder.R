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

  is_primary <- eventPrimaryFlagsForCountry(
    events = events_df,
    country_id = country_id,
    event_countries = event_countries
  )

  build_option <- function(i) {
    label <- formatEventChoiceLabel(
      event_name = events_df$event_name[[i]],
      start_year = events_df$start_year[[i]],
      end_year = events_df$end_year[[i]]
    )
    opt <- htmltools::tags$option(
      value = events_df$event_id[[i]],
      label,
      style = if (is_primary[[i]]) "font-weight:700" else NULL
    )
    if (!is.null(selected) && identical(events_df$event_id[[i]], selected)) {
      opt <- htmltools::tagAppendAttributes(opt, selected = "selected")
    }
    opt
  }

  is_global <- events_df$event_scope == "global"

  primary_idx <- which(is_primary)
  global_idx <- which(!is_primary & is_global)
  other_idx <- which(!is_primary & !is_global)
  country_label <- country_name %||% country_id

  children <- list()
  if (length(primary_idx) > 0) {
    children <- c(
      children,
      list(do.call(
        htmltools::tags$optgroup,
        c(
          list(label = sprintf("Events for %s", country_label)),
          lapply(primary_idx, build_option)
        )
      ))
    )
  }
  if (length(global_idx) > 0) {
    children <- c(
      children,
      list(do.call(
        htmltools::tags$optgroup,
        c(
          list(label = "Global events"),
          lapply(global_idx, build_option)
        )
      ))
    )
  }
  if (length(other_idx) > 0) {
    children <- c(
      children,
      list(do.call(
        htmltools::tags$optgroup,
        c(
          list(label = "Other events"),
          lapply(other_idx, build_option)
        )
      ))
    )
  }
  if (length(children) == 0) {
    children <- lapply(seq_len(nrow(events_df)), build_option)
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
        shiny::span(class = "query-chunk query-chunk-event", shiny::uiOutput(ns("event_ui"), inline = TRUE)),
        shiny::span(class = "query-plain query-sentence-end", ".")
      ),
      shiny::uiOutput(ns("validity_ui"))
    )
  )
}

assembleQueryBuilderRecipe <- function(input, query_id, metric = "count") {
  age_status_id <- input$age_status_id
  custom_min <- if (identical(age_status_id, "custom")) {
    as.integer(input$custom_age_min)
  } else {
    NA_integer_
  }
  custom_max <- if (identical(age_status_id, "custom")) {
    as.integer(input$custom_age_max)
  } else {
    NA_integer_
  }
  age_modifier <- as.character(input$age_modifier %||% input$is_complement %||% "none")
  if (age_modifier %in% c("FALSE", "TRUE")) {
    age_modifier <- if (identical(age_modifier, "TRUE")) "not" else "none"
  }
  is_complement <- identical(age_modifier, "not")

  list(
    query_id = query_id,
    country_id = input$country_id,
    sex = input$sex,
    age_status_id = age_status_id,
    age_modifier = age_modifier,
    is_complement = is_complement,
    custom_age_min = custom_min,
    custom_age_max = custom_max,
    event_id = input$event_id,
    event_mode = input$event_mode,
    metric = metric
  )
}

queryBuilderServer <- function(
  id,
  countries,
  events,
  age_groups,
  event_countries,
  query_id,
  metric = NULL,
  custom_age_seed = NULL,
  render_outputs = NULL,
  events_catalog = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    metric_reactive <- metric
    if (is.null(metric_reactive)) {
      metric_reactive <- shiny::reactive("count")
    }
    custom_age_seed_reactive <- custom_age_seed
    if (is.null(custom_age_seed_reactive)) {
      custom_age_seed_reactive <- shiny::reactive(NULL)
    }
    render_outputs_reactive <- render_outputs
    if (is.null(render_outputs_reactive)) {
      render_outputs_reactive <- shiny::reactive(TRUE)
    }
    events_catalog_reactive <- events_catalog
    if (is.null(events_catalog_reactive)) {
      events_catalog_reactive <- events
    }

    inputs_ready <- shiny::reactive({
      queryBuilderInputsReady(input)
    })

    selected_country <- shiny::reactive({
      input$country_id
    })

    effective_country_id <- shiny::reactive({
      resolveQueryBuilderCountryId(selected_country(), countries())
    })

    selected_event <- shiny::reactive({
      input$event_id
    })

    compatible_events <- shiny::reactive({
      filterCompatibleEvents(
        events = events(),
        country_id = effective_country_id(),
        event_countries = event_countries()
      )
    })

    compatible_countries <- shiny::reactive({
      filterCompatibleCountries(
        countries = countries(),
        event_id = selected_event(),
        events = events(),
        event_countries = event_countries()
      )
    })

    output$country_ui <- shiny::renderUI({
      if (!render_outputs_reactive()) {
        return(queryBuilderInlineSelectInput(
          inputId = session$ns("country_id"),
          choices = stats::setNames(countries()$country_id, countries()$country_name),
          selected = queryBuilderDefaultCountryId(countries(), "RUS")
        ))
      }
      choices_df <- compatible_countries()
      if (nrow(choices_df) == 0) {
        choices_df <- countries()
      }
      selected <- selected_country()
      default_country <- queryBuilderDefaultCountryId(choices_df, "RUS")
      if (!is.null(selected) && selected %in% choices_df$country_id) {
        selected_value <- selected
      } else {
        selected_value <- default_country
      }

      queryBuilderInlineSelectInput(
        inputId = session$ns("country_id"),
        choices = stats::setNames(choices_df$country_id, choices_df$country_name),
        selected = selected_value
      )
    })

    output$event_ui <- shiny::renderUI({
      if (!render_outputs_reactive()) {
        return(htmltools::tags$select(
          id = session$ns("event_id"),
          class = "shiny-input-select form-control query-inline-select query-event-select"
        ))
      }
      countries_df <- countries()
      country_id <- effective_country_id()
      events_df <- events()

      choices_df <- compatible_events()
      if (nrow(choices_df) == 0) {
        choices_df <- events_df
      }
      if (nrow(choices_df) == 0) {
        return(htmltools::tags$select(
          id = session$ns("event_id"),
          class = "shiny-input-select form-control query-inline-select query-event-select"
        ))
      }

      selected <- selected_event()
      if (!is.null(selected) && length(selected) == 1L && selected %in% choices_df$event_id) {
        selected_value <- selected
      } else {
        preset_event <- resolvePresetEventId(
          candidates = c("RUS_AFGHAN_WAR", "AFG_WAR"),
          country_id = country_id,
          events = events_df
        )
        if (!is.null(preset_event) && preset_event %in% choices_df$event_id) {
          selected_value <- preset_event
        } else {
          selected_value <- choices_df$event_id[[1]]
        }
      }

      country_row <- countries_df |>
        dplyr::filter(.data$country_id == .env$country_id) |>
        dplyr::slice(1)
      country_name <- if (nrow(country_row) > 0) {
        country_row$country_name[[1]]
      } else {
        country_id
      }

      queryBuilderEventSelectInput(
        inputId = session$ns("event_id"),
        events_df = choices_df,
        country_id = country_id,
        event_countries = event_countries(),
        country_name = country_name,
        selected = selected_value
      )
    })

    output$age_status_ui <- shiny::renderUI({
      selected <- input$age_status_id
      if (is.null(selected) || !selected %in% queryBuilderAgeStatusValues(age_groups())) {
        selected <- "adults"
      }
      queryBuilderInlineSelectInput(
        inputId = session$ns("age_status_id"),
        choices = queryBuilderAgeStatusChoices(age_groups()),
        selected = selected
      )
    })

    output$custom_age_ui <- shiny::renderUI({
      selected_age <- input$age_status_id
      if (is.null(selected_age) || !identical(selected_age, "custom")) {
        return(NULL)
      }

      seed <- custom_age_seed_reactive()
      min_val <- input$custom_age_min
      max_val <- input$custom_age_max
      if (!is.null(seed) && length(seed) == 2L) {
        min_val <- seed[[1]]
        max_val <- seed[[2]]
      }
      if (is.null(min_val) || is.na(min_val)) {
        min_val <- 18L
      }
      if (is.null(max_val) || is.na(max_val)) {
        max_val <- 65L
      }

      shiny::tagList(
        shiny::span(class = "query-plain query-custom-age-lead", " ages"),
        shiny::span(
          class = "query-chunk query-chunk-numeric",
          shiny::numericInput(
            session$ns("custom_age_min"),
            label = NULL,
            value = min_val,
            min = 0,
            max = 120,
            width = "56px"
          )
        ),
        shiny::span(class = "query-plain", "–"),
        shiny::span(
          class = "query-chunk query-chunk-numeric",
          shiny::numericInput(
            session$ns("custom_age_max"),
            label = NULL,
            value = max_val,
            min = 0,
            max = 120,
            width = "56px"
          )
        )
      )
    })

    shiny::outputOptions(output, "country_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "event_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "age_status_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "custom_age_ui", suspendWhenHidden = FALSE)

    recipe <- shiny::reactive({
      assembleQueryBuilderRecipe(
        input = input,
        query_id = query_id,
        metric = metric_reactive()
      )
    })

    assessment <- shiny::reactive({
      if (!inputs_ready()) {
        return(list(
          valid = FALSE,
          errors = character(0),
          warnings = character(0),
          pending = TRUE
        ))
      }

      state <- assessRecipe(
        recipe = recipe(),
        countries = countries(),
        events = events_catalog_reactive(),
        age_groups = age_groups(),
        event_countries = event_countries()
      )
      state$pending <- FALSE
      state
    })

    output$validity_ui <- shiny::renderUI({
      state <- assessment()
      if (isTRUE(state$pending)) {
        return(NULL)
      }
      if (state$valid && length(state$warnings) == 0) {
        return(NULL)
      }

      items <- list()
      if (!state$valid) {
        items <- c(
          items,
          lapply(state$errors, function(msg) {
            shiny::tags$div(
              class = "query-validity-error",
              shiny::icon("times-circle"),
              " ",
              msg
            )
          })
        )
      }
      if (length(state$warnings) > 0) {
        items <- c(
          items,
          lapply(state$warnings, function(msg) {
            shiny::tags$div(
              class = "query-validity-warning",
              shiny::icon("exclamation-triangle"),
              " ",
              msg
            )
          })
        )
      }

      if (length(items) == 0) {
        return(NULL)
      }
      shiny::tagList(items)
    })

    shiny::reactive({
      state <- assessment()
      list(
        recipe = recipe(),
        valid = state$valid,
        errors = state$errors,
        warnings = state$warnings,
        pending = isTRUE(state$pending)
      )
    })
  })
}

queryBuilderInputsReady <- function(input) {
  required_ids <- c(
    "country_id", "sex", "age_status_id", "age_modifier", "event_id",
    "event_mode"
  )

  if (identical(input$age_status_id, "custom")) {
    required_ids <- c(required_ids, "custom_age_min", "custom_age_max")
  }

  all(vapply(required_ids, function(id) {
    value <- input[[id]]
    if (id %in% c("custom_age_min", "custom_age_max")) {
      return(!is.null(value) && length(value) >= 1L && !is.na(value))
    }
    !is.null(value) && length(value) >= 1L && !is.na(value) && nzchar(as.character(value))
  }, logical(1)))
}

assembleQueryBuilderRecipe <- function(input, query_id, metric = "count") {
  age_group_id <- input$age_group_id
  custom_min <- if (identical(age_group_id, "custom")) {
    as.integer(input$custom_age_min)
  } else {
    NA_integer_
  }
  custom_max <- if (identical(age_group_id, "custom")) {
    as.integer(input$custom_age_max)
  } else {
    NA_integer_
  }

  list(
    query_id = query_id,
    country_id = input$country_id,
    sex = input$sex,
    age_group_id = age_group_id,
    custom_age_min = custom_min,
    custom_age_max = custom_max,
    event_id = input$event_id,
    event_mode = input$event_mode,
    operator = input$operator,
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
  metric = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    metric_reactive <- metric
    if (is.null(metric_reactive)) {
      metric_reactive <- shiny::reactive("count")
    }

    inputs_ready <- shiny::reactive({
      queryBuilderInputsReady(input)
    })

    selected_country <- shiny::reactive({
      input$country_id
    })

    selected_event <- shiny::reactive({
      input$event_id
    })

    compatible_events <- shiny::reactive({
      filterCompatibleEvents(
        events = events(),
        country_id = selected_country(),
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
      choices_df <- compatible_events()
      if (nrow(choices_df) == 0) {
        choices_df <- events()
      }
      selected <- selected_event()
      if (!is.null(selected) && selected %in% choices_df$event_id) {
        selected_value <- selected
      } else {
        selected_value <- choices_df$event_id[[1]]
      }

      country_row <- countries() |>
        dplyr::filter(.data$country_id == selected_country()) |>
        dplyr::slice(1)
      country_name <- if (nrow(country_row) > 0) {
        country_row$country_name[[1]]
      } else {
        selected_country()
      }

      queryBuilderEventSelectInput(
        inputId = session$ns("event_id"),
        events_df = choices_df,
        country_id = selected_country(),
        event_countries = event_countries(),
        country_name = country_name,
        selected = selected_value
      )
    })

    output$age_group_ui <- shiny::renderUI({
      selected <- input$age_group_id
      if (is.null(selected) || !selected %in% age_groups()$age_group_id) {
        selected <- "adults"
      }

      queryBuilderInlineSelectInput(
        inputId = session$ns("age_group_id"),
        choices = stats::setNames(age_groups()$age_group_id, age_groups()$age_label),
        selected = selected
      )
    })

    output$custom_age_ui <- shiny::renderUI({
      selected_age <- input$age_group_id
      if (is.null(selected_age) || !identical(selected_age, "custom")) {
        return(NULL)
      }

      min_val <- input$custom_age_min
      max_val <- input$custom_age_max
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
        events = events(),
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
    "country_id", "sex", "age_group_id", "event_id",
    "event_mode", "operator"
  )

  if (identical(input$age_group_id, "custom")) {
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

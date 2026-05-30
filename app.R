suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(tidyr)
})

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))

resolveOptionalPath <- function(env_name, fallback_path) {
  env_value <- Sys.getenv(env_name, unset = "")
  if (nzchar(env_value)) {
    return(env_value)
  }
  fallback_path
}

bootstrapAppData <- function() {
  population_paths <- resolvePopulationPaths()
  events_path <- resolveOptionalPath("GEN_TRACKER_EVENTS_PATH", file.path("data", "events.csv"))
  countries_path <- resolveOptionalPath("GEN_TRACKER_COUNTRIES_PATH", file.path("data", "countries.csv"))
  event_countries_path <- resolveOptionalPath(
    "GEN_TRACKER_EVENT_COUNTRIES_PATH",
    file.path("data", "event_countries.csv")
  )

  missing <- c()
  if (length(population_paths) == 0) {
    missing <- c(missing, missingPreparedPopulationHint())
  }
  if (!file.exists(events_path)) {
    missing <- c(missing, sprintf("events file at %s", events_path))
  }
  if (!file.exists(countries_path)) {
    missing <- c(missing, sprintf("countries file at %s", countries_path))
  }
  if (length(missing) > 0) {
    stop(sprintf("Failed to bootstrap data sources: missing %s.", paste(missing, collapse = "; ")))
  }

  message("Generation Tracker: loading population...")
  population <- loadPopulationData(population_paths)
  message("Generation Tracker: loading events and country dictionaries...")
  events <- loadEvents(events_path)
  countries <- loadCountryDictionary(countries_path)

  event_countries_loaded <- file.exists(event_countries_path)
  if (event_countries_loaded) {
    event_countries <- loadEventCountries(event_countries_path)
    message(
      "Generation Tracker: loaded ",
      nrow(event_countries),
      " event–country link(s) from ",
      event_countries_path
    )
  } else {
    event_countries <- tibble::tibble(
      event_id = character(),
      country_id = character(),
      country_role = character()
    )
    warning(
      sprintf(
        paste(
          "Event–country links not found at %s.",
          "National events will not be blocked until you run scripts/build_event_countries.R",
          "or set GEN_TRACKER_EVENT_COUNTRIES_PATH."
        ),
        event_countries_path
      ),
      call. = FALSE
    )
  }

  validateEvents(events, countries = countries, event_countries = event_countries)

  year_bounds <- populationYearBounds(population)

  list(
    population = population,
    events = events,
    countries = countries,
    event_countries = event_countries,
    meta = list(
      population_paths = population_paths,
      events_path = events_path,
      countries_path = countries_path,
      event_countries_path = event_countries_path,
      event_countries_loaded = event_countries_loaded,
      event_countries_rows = nrow(event_countries),
      year_bounds = year_bounds
    )
  )
}

message("Generation Tracker: loading application data...")
app_data <- bootstrapAppData()
message("Generation Tracker: data ready. Starting Shiny.")

metricInputChoices <- function() {
  c(
    "Population count" = "count",
    "Share of population" = "share_total_population"
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "query_inline.js")
  ),
  div(
    class = "gt-header",
    h2("Generation Tracker"),
    p("Compare cohorts shaped by historical events across time.")
  ),
  div(
    class = "gt-page",
    div(
      class = "gt-card gt-queries-panel",
      uiOutput("query_ui"),
      p(
        class = "gt-queries-hint",
        "Tap any highlighted phrase to change it — the sentence is the query."
      )
    ),
    uiOutput("plot_status"),
    div(
      class = "gt-card gt-plot-card",
      div(
        class = "gt-plot-header",
        h3(class = "gt-plot-title", "Population strata by historical experience"),
        div(
          class = "gt-plot-header-metric",
          selectInput(
            "metric",
            label = NULL,
            choices = metricInputChoices(),
            selected = "count",
            width = "220px"
          )
        )
      ),
      plotOutput("series_plot", height = "520px"),
      div(
        class = "gt-plot-controls",
        div(
          class = "gt-plot-controls-row",
          div(
            class = "gt-plot-year-range",
            sliderInput(
              "year_range",
              label = NULL,
              min = app_data$meta$year_bounds$min,
              max = app_data$meta$year_bounds$max,
              value = app_data$meta$year_bounds$default_range,
              step = 1,
              sep = "",
              width = "100%"
            )
          ),
          div(
            class = "gt-plot-projection-toggle",
            checkboxInput("show_projection", "Show projection", value = TRUE)
          )
        )
      )
    ),
    div(
      class = "gt-card gt-download-card",
      div(class = "gt-card-title", "Download"),
      div(
        class = "gt-export-row",
        downloadButton("download_xlsx", "Export XLSX", class = "btn-default"),
        downloadButton("download_png", "Export PNG", class = "btn-default"),
        downloadButton("download_jpg", "Export JPEG", class = "btn-default")
      )
    ),
    div(
      class = "gt-card",
      div(class = "gt-card-title", "What this chart shows"),
      uiOutput("query_details_ui")
    ),
    div(
      class = "gt-card",
      div(class = "gt-card-title", "Notes and warnings"),
      uiOutput("warnings_ui")
    ),
    div(
      class = "gt-card gt-footer-card",
      div(class = "gt-card-title", "Data sources"),
      uiOutput("data_sources_ui")
    )
  )
)

server <- function(input, output, session) {
  population <- reactiveVal(app_data$population)
  events <- reactiveVal(app_data$events)
  countries <- reactiveVal(app_data$countries)
  event_countries <- reactiveVal(app_data$event_countries)
  age_groups <- reactiveVal(defaultAgeGroups())
  query_count <- reactiveVal(1L)
  plot_context <- reactive({
    buildPlotCalculationContext(population())
  })
  metric <- reactive(input$metric)
  bootstrap_meta <- app_data$meta

  output$data_sources_ui <- renderUI({
    pop_label <- if (length(bootstrap_meta$population_paths) == 1L) {
      basename(bootstrap_meta$population_paths[[1]])
    } else {
      sprintf("%d population sources", length(bootstrap_meta$population_paths))
    }

    links_status <- if (isTRUE(bootstrap_meta$event_countries_loaded)) {
      sprintf(
        "%s event–country links loaded",
        format(bootstrap_meta$event_countries_rows, big.mark = ",")
      )
    } else {
      tagList(
        span(class = "gt-data-warning", "Event links not loaded"),
        tags$small(
          class = "gt-data-muted",
          " — run ",
          tags$code("Rscript scripts/build_event_countries.R"),
          " or set ",
          tags$code("GEN_TRACKER_EVENT_COUNTRIES_PATH")
        )
      )
    }

    year_min <- as.integer(input$year_range[[1]])
    year_max <- as.integer(input$year_range[[2]])
    projection_label <- if (isTRUE(input$show_projection)) {
      "projection shown"
    } else {
      "estimates only"
    }

    tagList(
      div(
        class = "gt-data-grid",
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Population"),
          div(class = "gt-data-grid-value", pop_label)
        ),
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Events / countries"),
          div(
            class = "gt-data-grid-value",
            sprintf(
              "%s / %s rows",
              format(nrow(events()), big.mark = ","),
              format(nrow(countries()), big.mark = ",")
            )
          )
        ),
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Event links"),
          div(class = "gt-data-grid-value", links_status)
        ),
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Catalogues"),
          div(
            class = "gt-data-grid-value",
            sprintf(
              "%s · %s",
              basename(bootstrap_meta$events_path),
              basename(bootstrap_meta$countries_path)
            )
          )
        ),
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Available years"),
          div(
            class = "gt-data-grid-value",
            sprintf(
              "%s–%s (estimates through %s)",
              bootstrap_meta$year_bounds$min,
              bootstrap_meta$year_bounds$max,
              bootstrap_meta$year_bounds$estimate_max
            )
          )
        ),
        div(
          class = "gt-data-grid-item",
          div(class = "gt-data-grid-label", "Chart window"),
          div(
            class = "gt-data-grid-value",
            sprintf("%s–%s · %s", year_min, year_max, projection_label)
          )
        )
      )
    )
  })

  query_reactives <- list(
    queryBuilderServer("qb1", countries, events, age_groups, event_countries, "q1", metric = metric),
    queryBuilderServer("qb2", countries, events, age_groups, event_countries, "q2", metric = metric),
    queryBuilderServer("qb3", countries, events, age_groups, event_countries, "q3", metric = metric),
    queryBuilderServer("qb4", countries, events, age_groups, event_countries, "q4", metric = metric)
  )

  observeEvent(input$add_query, {
    if (query_count() < 4L) {
      query_count(query_count() + 1L)
    }
  })

  output$query_ui <- renderUI({
    n <- query_count()
    line_colors <- trackerPlotPalette(4L)
    tagList(
      lapply(seq_len(n), function(i) {
        queryBuilderUi(paste0("qb", i), line_color = line_colors[[i]])
      }),
      if (n < 4L) {
        div(
          class = "gt-add-query-row",
          actionButton(
            "add_query",
            label = "+",
            class = "gt-add-query-btn",
            title = "Add another query"
          )
        )
      }
    )
  })

  query_states <- reactive({
    n <- query_count()
    purrr::map(seq_len(n), function(i) query_reactives[[i]]())
  })

  recipes <- reactive({
    states <- query_states()
    purrr::map_dfr(states, function(state) {
      tibble::as_tibble(state$recipe)
    })
  })

  valid_recipes <- reactive({
    states <- query_states()
    valid <- purrr::keep(states, function(state) isTRUE(state$valid))
    if (length(valid) == 0) {
      return(tibble::tibble())
    }
    purrr::map_dfr(valid, function(state) tibble::as_tibble(state$recipe))
  })

  plot_data <- reactive({
    req(nrow(valid_recipes()) > 0)
    out <- buildPlotData(
      recipes = valid_recipes(),
      population = population(),
      events = events(),
      age_groups = age_groups(),
      countries = countries(),
      event_countries = event_countries(),
      plot_context = plot_context()
    )
    applyPlotViewFilters(
      plot_data = out,
      year_range = input$year_range,
      show_projection = isTRUE(input$show_projection)
    )
  })

  plot_view_state <- reactive({
    list(
      metric = input$metric,
      year_range = input$year_range,
      show_projection = isTRUE(input$show_projection)
    )
  })

  plot_obj <- reactive({
    buildTrackerPlot(
      plot_data = plot_data(),
      events = events(),
      metric = input$metric,
      title = NULL,
      subtitle = buildPlotViewSubtitle(
        metric = input$metric,
        year_range = input$year_range,
        show_projection = isTRUE(input$show_projection)
      )
    )
  })

  output$plot_status <- renderUI({
    states <- query_states()
    invalid <- purrr::keep(states, function(state) {
      !isTRUE(state$valid) && !isTRUE(state$pending)
    })
    if (length(invalid) == 0) {
      return(NULL)
    }
    msgs <- purrr::map_chr(invalid, function(state) {
      paste(state$recipe$query_id, paste(state$errors, collapse = " "), sep = ": ")
    })
    div(
      class = "alert alert-warning gt-status-alert",
      tags$strong("Some queries are incomplete or incompatible:"),
      tags$ul(lapply(msgs, tags$li))
    )
  })

  output$series_plot <- renderPlot({
    req(nrow(valid_recipes()) > 0)
    plot_obj()
  })

  output$query_details_ui <- renderUI({
    if (nrow(valid_recipes()) == 0) {
      return(div(class = "gt-empty-state", "Complete at least one sentence above to see the breakdown."))
    }

    rows <- buildQueryDetailsRows(plot_data())
    query_nums <- sub("^q", "", rows$query_id)

    tagList(lapply(seq_len(nrow(rows)), function(i) {
      div(
        class = "gt-query-detail",
        div(class = "gt-query-detail-title", sprintf("Line %s", query_nums[[i]])),
        div(class = "gt-query-detail-sentence", rows$query_description[[i]]),
        tags$details(
          class = "query-advanced-block",
          tags$summary("Recipe code"),
          div(class = "gt-recipe-code", rows$recipe_code[[i]])
        )
      )
    }))
  })

  output$warnings_ui <- renderUI({
    req(nrow(recipes()) > 0)
    warning_lines <- purrr::map_chr(seq_len(nrow(recipes())), function(i) {
      recipe <- as.list(recipes()[i, ])
      event <- events() |> filter(event_id == recipe$event_id) |> slice(1)
      ctry <- countries() |> filter(country_id == recipe$country_id) |> slice(1)
      w <- collectQueryWarnings(recipe, event, ctry, event_countries = event_countries())
      paste(sprintf("%s: %s", recipe$query_id, paste(w, collapse = " | ")), collapse = "\n")
    })
    div(class = "gt-warnings-box", paste(warning_lines, collapse = "\n\n"))
  })

  output$download_xlsx <- downloadHandler(
    filename = function() sprintf("generation_tracker_%s.xlsx", Sys.Date()),
    content = function(file) {
      export_tables <- buildExportMetadataTables(
        plot_data = plot_data(),
        recipes = valid_recipes(),
        events = events(),
        countries = countries(),
        age_groups = age_groups(),
        view_state = plot_view_state(),
        population = population(),
        population_paths = bootstrap_meta$population_paths,
        events_path = bootstrap_meta$events_path,
        countries_path = bootstrap_meta$countries_path,
        event_countries_path = bootstrap_meta$event_countries_path,
        event_countries = event_countries(),
        event_countries_loaded = bootstrap_meta$event_countries_loaded,
        event_countries_rows = bootstrap_meta$event_countries_rows
      )
      exportXlsx(
        plot_data = plot_data(),
        recipes = valid_recipes(),
        events = events(),
        file_path = file,
        export_tables = export_tables
      )
    }
  )

  output$download_png <- downloadHandler(
    filename = function() sprintf("generation_tracker_%s.png", Sys.Date()),
    content = function(file) exportPlot(plot_obj(), file)
  )

  output$download_jpg <- downloadHandler(
    filename = function() sprintf("generation_tracker_%s.jpg", Sys.Date()),
    content = function(file) exportPlot(plot_obj(), file)
  )
}

shinyApp(ui, server, options = list(launch.browser = TRUE))

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(tidyr)
})

r_files <- unique(normalizePath(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  winslash = "/",
  mustWork = TRUE
))
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
  events_path <- resolveOptionalPath("GEN_TRACKER_EVENTS_PATH", eventsDataPath())
  countries_path <- resolveOptionalPath("GEN_TRACKER_COUNTRIES_PATH", countriesDataPath())
  event_countries_path <- resolveOptionalPath(
    "GEN_TRACKER_EVENT_COUNTRIES_PATH",
    eventCountriesDeployPath()
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
  data_dir <- dirname(events_path)
  universe <- loadEventsUniverse(manual_path = events_path, data_dir = data_dir)
  events <- universe$events
  composite_members <- universe$composite_members
  countries <- loadCountryDictionary(countries_path)

  event_countries_loaded <- file.exists(event_countries_path)
  if (event_countries_loaded) {
    event_countries <- loadEventCountriesUniverse(
      manual_path = event_countries_path,
      data_dir = data_dir
    )
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
  validateCompositeMembers(events, composite_members)

  migration_path <- resolveOptionalPath("GEN_TRACKER_MIGRATION_PATH", preparedMigrationPath(data_dir))
  migration <- loadPreparedMigration(migration_path, required = FALSE)
  if (is.null(migration)) {
    message(
      "Generation Tracker: prepared migration not found at ", migration_path,
      "; reliability scoring disabled (build offline: Rscript scripts/build_prepared_migration.R)."
    )
  } else {
    message(
      "Generation Tracker: loaded migration series for ",
      length(unique(migration$migration$country_id)), " countries."
    )
  }

  year_bounds <- populationYearBounds(population)
  events <- appendYearMarkerEvents(events, year_bounds)

  list(
    population = population,
    events = events,
    composite_members = composite_members,
    countries = countries,
    event_countries = event_countries,
    migration = migration,
    meta = list(
      population_paths = population_paths,
      events_path = events_path,
      countries_path = countries_path,
      event_countries_path = event_countries_path,
      event_countries_loaded = event_countries_loaded,
      event_countries_rows = nrow(event_countries),
      migration_path = migration_path,
      migration_loaded = !is.null(migration),
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
    "Share of population" = "share_total_population",
    "Share of WAP" = "share_working_age_population"
  )
}

staticAssetUrl <- function(path, base_dir = "www") {
  full_path <- file.path(base_dir, path)
  version <- if (file.exists(full_path)) {
    as.integer(as.numeric(file.mtime(full_path)))
  } else {
    as.integer(as.numeric(Sys.time()))
  }
  sprintf("%s?v=%d", path, version)
}

ui <- fluidPage(
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = staticAssetUrl("styles.css")),
    htmltools::tags$script(src = staticAssetUrl("query_inline.js"))
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
      uiOutput("query_layout_css"),
      uiOutput("query_ui"),
      recipePackPanelUi(),
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
        downloadButton("download_macro_xlsx", "Export all countries (XLSX)", class = "btn-default"),
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
  events_picker <- reactiveVal(eventsForEventPicker(app_data$events))
  composite_members <- reactiveVal(app_data$composite_members)
  countries <- reactiveVal(app_data$countries)
  event_countries <- reactiveVal(app_data$event_countries)
  migration <- reactiveVal(app_data$migration)
  age_groups <- reactiveVal(defaultAgeGroups())
  active_query_slots <- reactiveVal(1L)
  plot_context <- reactive({
    buildPlotCalculationContext(population())
  })
  metric <- reactive(input$metric)
  bootstrap_meta <- app_data$meta

  output$data_sources_ui <- renderUI({
    buildDataSourcesUi(
      year_bounds = bootstrap_meta$year_bounds,
      n_countries = nrow(countries()),
      n_events = nrow(events())
    )
  })

  import_custom_ages <- reactiveVal(NULL)

  query_reactives <- lapply(seq_len(4L), function(slot) {
    queryBuilderServer(
      paste0("qb", slot),
      countries,
      events_picker,
      age_groups,
      event_countries,
      events_catalog = events,
      paste0("q", slot),
      metric = metric,
      custom_age_seed = reactive({
        ca <- import_custom_ages()
        if (is.null(ca)) {
          return(NULL)
        }
        ca[[as.character(slot)]]
      }),
      render_outputs = reactive({
        slot %in% active_query_slots()
      })
    )
  })

  query_defaults_seeded <- FALSE
  session$onFlushed(function() {
    if (isTRUE(query_defaults_seeded)) {
      return()
    }
    query_defaults_seeded <<- TRUE
    seedSessionDefaultQueries(
      session = session,
      events = app_data$events,
      countries = app_data$countries
    )
  })

  observeEvent(input$add_query, {
    active <- active_query_slots()
    if (length(active) >= 4L) {
      return()
    }
    next_slot <- setdiff(seq_len(4L), active)[1]
    active_query_slots(c(active, next_slot))
  })

  remove_query_slot <- function(slot) {
    active <- active_query_slots()
    if (length(active) <= 1L || !slot %in% active) {
      return()
    }
    active_query_slots(setdiff(active, slot))
  }

  for (slot in seq_len(4L)) {
    local({
      slot_id <- slot
      observeEvent(
        input[[paste0("remove_query_", slot_id)]],
        remove_query_slot(slot_id),
        ignoreInit = TRUE
      )
    })
  }

  recipe_pack_status <- reactiveVal("")

  output$recipe_pack_status <- renderText({
    recipe_pack_status()
  })

  observeEvent(input$export_recipe_pack, {
    view_state <- list(
      metric = metric(),
      year_range = input$year_range,
      show_projection = isTRUE(input$show_projection)
    )
    code <- tryCatch(
      buildRecipePackCodeFromStates(query_states_raw(), view_state),
      error = function(e) {
        recipe_pack_status(conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(code)) {
      return()
    }
    updateTextAreaInput(session, "recipe_pack_code", value = code)
    recipe_pack_status("Recipe pack copied to the field above.")
  })

  observeEvent(input$import_recipe_pack, {
    code <- trimws(input$recipe_pack_code %||% "")
    if (!nzchar(code)) {
      recipe_pack_status("Paste a GENPACK1 code before importing.")
      return()
    }

    assessment <- validateRecipePack(
      code = code,
      countries = countries(),
      events = events(),
      age_groups = age_groups(),
      event_countries = event_countries(),
      year_bounds = bootstrap_meta$year_bounds
    )

    if (!assessment$valid) {
      recipe_pack_status(paste(assessment$errors, collapse = " "))
      return()
    }

    import_custom_ages(recipePackCustomAgeSeeds(assessment$pack$recipes))

    imported <- tryCatch(
      {
        applyRecipePackToSession(
          session = session,
          pack = assessment$pack,
          set_active_slots = active_query_slots,
          year_bounds = bootstrap_meta$year_bounds
        )
        TRUE
      },
      error = function(e) {
        recipe_pack_status(conditionMessage(e))
        FALSE
      }
    )
    if (!isTRUE(imported)) {
      return()
    }

    status <- "Recipe pack imported into the query lines above."
    if (length(assessment$warnings) > 0) {
      status <- paste(c(status, assessment$warnings), collapse = " ")
    }
    recipe_pack_status(status)
  })

  output$query_layout_css <- renderUI({
    active <- active_query_slots()
    hidden <- setdiff(seq_len(4L), active)
    rules <- character(0)
    if (length(hidden) > 0L) {
      rules <- c(rules, sprintf(
        "%s { display: none; }",
        paste(sprintf("#gt-query-row-%d", hidden), collapse = ", ")
      ))
    }
    if (length(active) <= 1L) {
      rules <- c(rules, ".gt-query-row .gt-query-row-actions { display: none; }")
    }
    if (length(active) >= 4L) {
      rules <- c(rules, "#gt-add-query-row { display: none; }")
    }
    if (length(rules) == 0L) {
      return(NULL)
    }
    htmltools::tags$style(htmltools::HTML(paste(rules, collapse = "\n")))
  })

  output$query_ui <- renderUI({
    line_colors <- trackerPlotPalette(4L)
    tagList(
      lapply(seq_len(4L), function(slot) {
        row_style <- sprintf("--gt-query-color: %s;", line_colors[[slot]])
        div(
          id = paste0("gt-query-row-", slot),
          class = "gt-query-row",
          style = row_style,
          queryBuilderUi(paste0("qb", slot), line_color = line_colors[[slot]]),
          div(
            class = "gt-query-row-actions",
            actionButton(
              paste0("remove_query_", slot),
              label = "\u00d7",
              class = "gt-remove-query-btn",
              title = "Remove this query"
            )
          )
        )
      }),
      div(
        id = "gt-add-query-row",
        class = "gt-add-query-row",
        actionButton(
          "add_query",
          label = "+",
          class = "gt-add-query-btn",
          title = "Add another query"
        )
      )
    )
  })

  query_states_raw <- reactive({
    purrr::map(active_query_slots(), function(i) query_reactives[[i]]())
  })
  query_states <- shiny::debounce(query_states_raw, millis = 220)

  recipes <- reactive({
    states <- query_states()
    ready <- purrr::keep(states, function(state) {
      !isTRUE(state$pending) && !is.null(state$recipe$event_id)
    })
    if (length(ready) == 0) {
      return(tibble::tibble())
    }
    purrr::map_dfr(ready, function(state) tibble::as_tibble(state$recipe))
  })

  valid_recipes <- reactive({
    states <- query_states()
    valid <- purrr::keep(states, function(state) isTRUE(state$valid))
    if (length(valid) == 0) {
      return(tibble::tibble())
    }
    purrr::map_dfr(valid, function(state) tibble::as_tibble(state$recipe))
  })

  event_window_start <- reactive({
    recipes <- valid_recipes()
    if (nrow(recipes) == 0) {
      return(NULL)
    }
    ev <- events()
    start_years <- ev$start_year[ev$event_id %in% recipes$event_id]
    if (length(start_years) == 0) {
      return(NULL)
    }
    eventWindowDefaultStart(
      event_start_years = start_years,
      year_min = bootstrap_meta$year_bounds$min,
      year_max = bootstrap_meta$year_bounds$max
    )
  })

  observeEvent(event_window_start(), {
    new_start <- event_window_start()
    req(!is.null(new_start))
    current <- input$year_range
    upper <- if (!is.null(current)) current[[2]] else bootstrap_meta$year_bounds$max
    if (is.null(current) || current[[1]] != new_start) {
      updateSliderInput(session, "year_range", value = c(new_start, upper))
    }
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
      plot_context = plot_context(),
      composite_members = composite_members(),
      migration = migration()
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
      composite_members = composite_members(),
      metric = input$metric,
      title = NULL,
      subtitle = NULL
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
      htmltools::tags$strong("Some queries are incomplete or incompatible:"),
      htmltools::tags$ul(lapply(msgs, htmltools::tags$li))
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
      narrative <- rows$chart_narrative[[i]]
      if (is.na(narrative) || !nzchar(narrative)) {
        narrative <- rows$query_description[[i]]
      }
      narrative_paragraphs <- strsplit(narrative, "\n\n", fixed = TRUE)[[1]]

      event_description <- if ("event_short_description" %in% names(rows)) {
        rows$event_short_description[[i]]
      } else {
        NA_character_
      }
      event_source <- if ("event_source_url" %in% names(rows)) {
        rows$event_source_url[[i]]
      } else {
        NA_character_
      }
      event_family <- if ("event_family" %in% names(rows)) {
        rows$event_family[[i]]
      } else {
        NA_character_
      }
      event_context_tags <- list()
      if (!is.na(event_description) && nzchar(event_description)) {
        event_context_tags <- c(event_context_tags, list(
          htmltools::tags$p(class = "gt-event-description", event_description)
        ))
      }
      event_meta_children <- list()
      if (!is.na(event_family) && nzchar(event_family)) {
        event_meta_children <- c(event_meta_children, list(
          htmltools::tags$span(class = "gt-event-family", event_family)
        ))
      }
      if (!is.na(event_source) && nzchar(event_source)) {
        event_meta_children <- c(event_meta_children, list(
          htmltools::tags$a(
            class = "gt-event-source",
            href = event_source,
            target = "_blank",
            rel = "noopener noreferrer",
            "Source"
          )
        ))
      }
      if (length(event_meta_children) > 0) {
        event_context_tags <- c(event_context_tags, list(
          htmltools::tags$p(class = "gt-event-meta", event_meta_children)
        ))
      }

      div(
        class = "gt-query-detail",
        div(class = "gt-query-detail-title", sprintf("Line %s", query_nums[[i]])),
      div(
        class = "gt-query-detail-sentence",
        lapply(narrative_paragraphs, function(paragraph) {
          htmltools::tags$p(paragraph)
        }),
        event_context_tags,
        htmltools::tags$p(
          class = "gt-reliability-summary",
          formatReliabilitySummary(list(
            reliability_score = rows$reliability_score[[i]] %||% NA_real_,
            migration_exposure = rows$migration_exposure[[i]] %||% NA_real_,
            reliability_warning = rows$reliability_warning[[i]] %||% NA_character_
          ))
        )
      ),
        htmltools::tags$details(
          class = "query-advanced-block",
          htmltools::tags$summary("Recipe code"),
          div(class = "gt-recipe-code", rows$recipe_code[[i]])
        )
      )
    }))
  })

  output$warnings_ui <- renderUI({
    recipes_tbl <- valid_recipes()
    req(nrow(recipes_tbl) > 0)
    pdata <- plot_data()
    warning_lines <- purrr::map_chr(seq_len(nrow(recipes_tbl)), function(i) {
      recipe <- as.list(recipes_tbl[i, ])
      event <- events() |> filter(event_id == recipe$event_id) |> slice(1)
      ctry <- countries() |> filter(country_id == recipe$country_id) |> slice(1)
      series_rows <- pdata |> dplyr::filter(.data$query_id == recipe$query_id)
      w <- collectQueryWarnings(
        recipe,
        event,
        ctry,
        event_countries = event_countries(),
        stratum_series = series_rows,
        composite_members = composite_members(),
        events = events(),
        migration = migration()
      )
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
        event_countries_rows = bootstrap_meta$event_countries_rows,
        composite_members = composite_members(),
        migration = migration()
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

  output$download_macro_xlsx <- downloadHandler(
    filename = function() sprintf("generation_tracker_macro_%s.xlsx", Sys.Date()),
    content = function(file) {
      recipes_tbl <- valid_recipes()
      req(nrow(recipes_tbl) > 0)
      exportMacroLongTable(
        file_path = file,
        template_recipes = recipes_tbl,
        countries = countries(),
        population = population(),
        events = events(),
        age_groups = age_groups(),
        plot_context = plot_context(),
        composite_members = composite_members(),
        event_countries = event_countries(),
        migration = migration(),
        year_range = input$year_range,
        show_projection = isTRUE(input$show_projection),
        population_paths = bootstrap_meta$population_paths
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

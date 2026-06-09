GEN_TRACKER_WPP_URL <- "https://population.un.org/wpp/"

dataSourcesPopulationBlurb <- function() {
  "Single-age estimates by sex and year; medium-variant projections where shown."
}

dataSourcesEventsBlurb <- function() {
  "Curated episodes linked to countries and years; not a complete world history."
}

formatDataSourcesYearSpan <- function(year_bounds) {
  sprintf(
    "%s–%s (estimates through %s)",
    year_bounds$min,
    year_bounds$max,
    year_bounds$estimate_max
  )
}

buildDataSourcesUi <- function(
  year_bounds,
  n_countries,
  n_events
) {
  htmltools::tagList(
    htmltools::tags$div(
      class = "gt-data-grid",
      htmltools::tags$div(
        class = "gt-data-grid-item gt-data-grid-item-wide",
        htmltools::tags$div(class = "gt-data-grid-label", "Population source"),
        htmltools::tags$div(
          class = "gt-data-grid-value",
          htmltools::tags$a(
            href = GEN_TRACKER_WPP_URL,
            target = "_blank",
            rel = "noopener noreferrer",
            "UN World Population Prospects"
          ),
          " — ",
          dataSourcesPopulationBlurb()
        )
      ),
      htmltools::tags$div(
        class = "gt-data-grid-item gt-data-grid-item-wide",
        htmltools::tags$div(class = "gt-data-grid-label", "Event catalogue"),
        htmltools::tags$div(
          class = "gt-data-grid-value",
          dataSourcesEventsBlurb()
        )
      ),
      htmltools::tags$div(
        class = "gt-data-grid-item",
        htmltools::tags$div(class = "gt-data-grid-label", "Countries"),
        htmltools::tags$div(
          class = "gt-data-grid-value",
          format(n_countries, big.mark = ",")
        )
      ),
      htmltools::tags$div(
        class = "gt-data-grid-item",
        htmltools::tags$div(class = "gt-data-grid-label", "Events"),
        htmltools::tags$div(
          class = "gt-data-grid-value",
          format(n_events, big.mark = ",")
        )
      ),
      htmltools::tags$div(
        class = "gt-data-grid-item",
        htmltools::tags$div(class = "gt-data-grid-label", "Years"),
        htmltools::tags$div(
          class = "gt-data-grid-value",
          formatDataSourcesYearSpan(year_bounds)
        )
      )
    )
  )
}

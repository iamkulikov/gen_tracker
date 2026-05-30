exportXlsx <- function(
  plot_data,
  recipes,
  events,
  file_path,
  export_tables = NULL
) {
  recipes_tbl <- tibble::as_tibble(recipes)

  if (is.null(export_tables)) {
    export_tables <- buildExportMetadataTables(
      plot_data = plot_data,
      recipes = recipes_tbl,
      events = events,
      countries = tibble::tibble(),
      age_groups = defaultAgeGroups(),
      view_state = list(
        metric = recipes_tbl$metric[[1]] %||% "count",
        year_range = if (nrow(plot_data) > 0) {
          range(plot_data$year, na.rm = TRUE)
        } else {
          c(NA_integer_, NA_integer_)
        },
        show_projection = if (nrow(plot_data) > 0) {
          any(plot_data$is_projection)
        } else {
          TRUE
        }
      )
    )
  }

  used_events <- export_tables$used_events
  if (is.null(used_events) || nrow(used_events) == 0) {
    used_events <- events |>
      dplyr::filter(.data$event_id %in% unique(recipes_tbl$event_id))
  }

  writexl::write_xlsx(
    list(
      metadata = export_tables$metadata,
      data = plot_data,
      query_descriptions = export_tables$query_descriptions,
      queries = recipes_tbl,
      events = used_events,
      event_disclaimers = export_tables$event_disclaimers,
      notes = export_tables$notes
    ),
    path = file_path
  )

  invisible(file_path)
}

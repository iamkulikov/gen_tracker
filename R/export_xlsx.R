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

  queries <- export_tables$queries
  if (is.null(queries) || nrow(queries) == 0) {
    queries <- buildExportQueriesSheet(
      recipes = recipes_tbl,
      query_descriptions = export_tables$query_descriptions,
      events = used_events
    )
  }

  metric <- recipes_tbl$metric[[1]] %||% "count"
  if (!is.null(export_tables$metadata) && nrow(export_tables$metadata) > 0) {
    metric_code <- export_tables$metadata$value[
      export_tables$metadata$field == "metric_code"
    ]
    if (length(metric_code) == 1L && nzchar(metric_code)) {
      metric <- metric_code
    }
  }

  data_wide <- buildExportDataWide(plot_data)

  writeExportXlsxWorkbook(
    sheets = list(
      data = data_wide,
      metadata = export_tables$metadata,
      queries = transposeExportQueriesSheet(queries)
    ),
    file_path = file_path,
    metric = metric,
    plot_data = plot_data,
    data_wide = data_wide
  )

  invisible(file_path)
}

writeExportXlsxWorkbook <- function(
  sheets,
  file_path,
  metric = "count",
  plot_data = NULL,
  data_wide = NULL
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required for styled XLSX export. Install it with install.packages('openxlsx').",
      call. = FALSE
    )
  }

  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold")
  row_label_style <- openxlsx::createStyle(textDecoration = "bold", valign = "top")
  notes_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  value_style <- exportXlsxValueStyle(metric)
  projection_style <- exportXlsxProjectionValueStyle(metric)

  for (sheet_name in names(sheets)) {
    sheet_df <- sheets[[sheet_name]]
    if (is.null(sheet_df)) {
      sheet_df <- tibble::tibble()
    } else {
      sheet_df <- tibble::as_tibble(sheet_df)
    }

    openxlsx::addWorksheet(wb, sheet_name)
    if (ncol(sheet_df) == 0L) {
      next
    }

    openxlsx::writeData(wb, sheet_name, sheet_df, colNames = TRUE)
    applyExportXlsxSheetStyle(
      wb = wb,
      sheet_name = sheet_name,
      sheet_df = sheet_df,
      header_style = header_style,
      row_label_style = row_label_style,
      notes_style = notes_style,
      wrap_style = wrap_style,
      value_style = value_style,
      projection_style = projection_style,
      metric = metric,
      plot_data = plot_data,
      data_wide = data_wide
    )
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  invisible(file_path)
}

applyExportXlsxProjectionStyles <- function(
  wb,
  sheet_name,
  projection_style,
  proj_cells
) {
  if (nrow(proj_cells) == 0L) {
    return(invisible(NULL))
  }

  for (col_idx in unique(proj_cells$col)) {
    rows_for_col <- unique(proj_cells$row[proj_cells$col == col_idx])
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = projection_style,
      rows = rows_for_col,
      cols = col_idx,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  invisible(NULL)
}

exportXlsxValueStyle <- function(metric) {
  if (isShareMetric(metric)) {
    openxlsx::createStyle(numFmt = "0.00%")
  } else {
    openxlsx::createStyle(numFmt = "#,##0")
  }
}

exportXlsxProjectionValueStyle <- function(metric) {
  projection_font <- "#C00000"
  if (isShareMetric(metric)) {
    openxlsx::createStyle(
      numFmt = "0.00%",
      textDecoration = "italic",
      fontColour = projection_font
    )
  } else {
    openxlsx::createStyle(
      numFmt = "#,##0",
      textDecoration = "italic",
      fontColour = projection_font
    )
  }
}

applyExportXlsxSheetStyle <- function(
  wb,
  sheet_name,
  sheet_df,
  header_style,
  row_label_style,
  notes_style,
  wrap_style,
  value_style,
  projection_style,
  metric,
  plot_data = NULL,
  data_wide = NULL
) {
  n_col <- ncol(sheet_df)
  n_row <- nrow(sheet_df)
  if (n_col == 0L) {
    return(invisible(NULL))
  }

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = header_style,
    rows = 1,
    cols = seq_len(n_col),
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = 2)

  if (sheet_name == "metadata") {
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = notes_style,
      rows = 2:max(2, n_row + 1),
      cols = seq_len(n_col),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = c(1, 2), widths = c(28, 96))
    return(invisible(NULL))
  }

  if (sheet_name == "queries") {
    openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = 2, firstActiveCol = 2)
    if (n_row > 0L) {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = row_label_style,
        rows = 2:(n_row + 1),
        cols = 1,
        gridExpand = TRUE,
        stack = TRUE
      )
      wrap_rows <- which(sheet_df$field %in% c(
        "line_label", "recipe_code", "reliability_summary",
        "short_description", "source_url", "curation_scores"
      ))
      if (length(wrap_rows) > 0L) {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = wrap_style,
          rows = wrap_rows + 1L,
          cols = 2:n_col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
    openxlsx::setColWidths(
      wb,
      sheet = sheet_name,
      cols = 1,
      widths = 22
    )
    if (n_col > 1L) {
      openxlsx::setColWidths(
        wb,
        sheet = sheet_name,
        cols = 2:n_col,
        widths = exportXlsxAutoColumnWidths(sheet_df[, -1, drop = FALSE])
      )
    }
    return(invisible(NULL))
  }

  openxlsx::addFilter(wb, sheet = sheet_name, rows = 1, cols = seq_len(n_col))

  if (n_row > 0L && sheet_name == "data") {
    numeric_cols <- exportXlsxNumericColumnIndexes(sheet_name, sheet_df, metric)
    if (length(numeric_cols) > 0L) {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = value_style,
        rows = 2:(n_row + 1),
        cols = numeric_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
      if (!is.null(plot_data) && !is.null(data_wide)) {
        proj_cells <- if ("country_id" %in% names(data_wide)) {
          buildMacroExportDataProjectionCells(plot_data, data_wide)
        } else {
          buildExportDataProjectionCells(plot_data, data_wide)
        }
        if (nrow(proj_cells) > 0L) {
          applyExportXlsxProjectionStyles(
            wb = wb,
            sheet_name = sheet_name,
            projection_style = projection_style,
            proj_cells = proj_cells
          )
        }
      }
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(n_col),
    widths = exportXlsxAutoColumnWidths(sheet_df)
  )

  invisible(NULL)
}

exportXlsxNumericColumnIndexes <- function(sheet_name, sheet_df, metric) {
  if (sheet_name != "data") {
    return(integer())
  }
  non_numeric <- match(c("year", "country_id"), names(sheet_df))
  non_numeric <- non_numeric[!is.na(non_numeric)]
  if (length(non_numeric) == 0L) {
    year_idx <- match("year", names(sheet_df))
    if (is.na(year_idx)) {
      return(integer())
    }
    return(seq_len(ncol(sheet_df))[-year_idx])
  }
  setdiff(seq_len(ncol(sheet_df)), non_numeric)
}

exportXlsxAutoColumnWidths <- function(sheet_df) {
  vapply(
    seq_len(ncol(sheet_df)),
    function(j) {
      chars <- c(names(sheet_df)[j], as.character(sheet_df[[j]]))
      chars <- chars[!is.na(chars)]
      if (length(chars) == 0L) {
        return(12)
      }
      width <- max(nchar(chars, type = "width"), na.rm = TRUE) + 2L
      min(48L, max(10L, width))
    },
    numeric(1)
  )
}

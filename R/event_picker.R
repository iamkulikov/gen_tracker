countryCodeFromEventId <- function(event_id) {
  event_id <- as.character(event_id)
  m <- regexec("^([A-Z]{3})_", event_id, perl = TRUE)
  hit <- regmatches(event_id, m)[[1]]
  if (length(hit) < 2L) {
    return(NA_character_)
  }
  code <- hit[[2]]
  if (code %in% c("GLB", "MER", "CMP", "YEA", "CUS")) {
    return(NA_character_)
  }
  code
}

isCountryScopedGlobalEvent <- function(event_id, event_scope) {
  if (!identical(as.character(event_scope), "global")) {
    return(FALSE)
  }
  code <- countryCodeFromEventId(event_id)
  !is.na(code) && nzchar(code)
}

formatEventPickerOptionLabel <- function(
  event_name,
  start_year,
  end_year,
  event_id,
  group
) {
  if (identical(group, "years")) {
    return(as.character(event_name))
  }

  label <- formatEventChoiceLabel(event_name, start_year, end_year)
  if (identical(group, "other")) {
    code <- countryCodeFromEventId(event_id)
    if (!is.na(code) && nzchar(code)) {
      label <- sprintf("%s: %s", code, label)
    }
  }
  label
}

sortEventPickerGroupIndices <- function(indices, events_df, decreasing = TRUE) {
  if (length(indices) <= 1L) {
    return(indices)
  }

  order_vec <- order(
    events_df$start_year[indices],
    events_df$event_name[indices],
    events_df$event_id[indices],
    decreasing = decreasing,
    na.last = TRUE
  )
  indices[order_vec]
}

partitionEventPickerIndices <- function(events_df, country_id, event_countries = NULL) {
  n <- nrow(events_df)
  if (n == 0L) {
    return(list(
      primary = integer(0),
      global = integer(0),
      other = integer(0),
      years = integer(0)
    ))
  }

  is_year <- events_df$event_origin == "year_marker"
  is_primary <- eventPrimaryFlagsForCountry(
    events = events_df,
    country_id = country_id,
    event_countries = event_countries
  )
  is_country_global <- vapply(
    seq_len(n),
    function(i) {
      isCountryScopedGlobalEvent(
        events_df$event_id[[i]],
        events_df$event_scope[[i]]
      )
    },
    logical(1)
  )

  is_global <- !is_primary &
    !is_year &
    events_df$event_scope == "global" &
    !is_country_global
  is_other <- !is_primary & !is_year & !is_global

  list(
    primary = which(is_primary),
    global = sortEventPickerGroupIndices(which(is_global), events_df),
    other = sortEventPickerGroupIndices(which(is_other), events_df),
    years = sortEventPickerGroupIndices(which(is_year), events_df, decreasing = FALSE)
  )
}

collectEventPickerOptionLabels <- function(events_df, country_id, event_countries = NULL) {
  groups <- partitionEventPickerIndices(events_df, country_id, event_countries)
  labels <- list()

  append_labels <- function(indices, group) {
    if (length(indices) == 0L) {
      return(invisible(NULL))
    }
    labels[[length(labels) + 1L]] <<- vapply(
      indices,
      function(i) {
        formatEventPickerOptionLabel(
          event_name = events_df$event_name[[i]],
          start_year = events_df$start_year[[i]],
          end_year = events_df$end_year[[i]],
          event_id = events_df$event_id[[i]],
          group = group
        )
      },
      character(1)
    )
  }

  append_labels(groups$primary, "primary")
  append_labels(groups$global, "global")
  append_labels(groups$other, "other")
  append_labels(groups$years, "years")

  list(
    groups = groups,
    labels_by_group = list(
      primary = if (length(groups$primary) > 0L) {
        vapply(groups$primary, function(i) {
          formatEventPickerOptionLabel(
            events_df$event_name[[i]],
            events_df$start_year[[i]],
            events_df$end_year[[i]],
            events_df$event_id[[i]],
            "primary"
          )
        }, character(1))
      } else {
        character(0)
      },
      global = if (length(groups$global) > 0L) {
        vapply(groups$global, function(i) {
          formatEventPickerOptionLabel(
            events_df$event_name[[i]],
            events_df$start_year[[i]],
            events_df$end_year[[i]],
            events_df$event_id[[i]],
            "global"
          )
        }, character(1))
      } else {
        character(0)
      },
      other = if (length(groups$other) > 0L) {
        vapply(groups$other, function(i) {
          formatEventPickerOptionLabel(
            events_df$event_name[[i]],
            events_df$start_year[[i]],
            events_df$end_year[[i]],
            events_df$event_id[[i]],
            "other"
          )
        }, character(1))
      } else {
        character(0)
      },
      years = if (length(groups$years) > 0L) {
        vapply(groups$years, function(i) {
          formatEventPickerOptionLabel(
            events_df$event_name[[i]],
            events_df$start_year[[i]],
            events_df$end_year[[i]],
            events_df$event_id[[i]],
            "years"
          )
        }, character(1))
      } else {
        character(0)
      }
    )
  )
}

assertNoDuplicateEventPickerOptions <- function(events_df, country_id, event_countries = NULL) {
  collected <- collectEventPickerOptionLabels(events_df, country_id, event_countries)
  labels_by_group <- collected$labels_by_group
  groups <- collected$groups

  for (group_name in names(labels_by_group)) {
    labels <- labels_by_group[[group_name]]
    indices <- groups[[group_name]]
    if (length(labels) == 0L) {
      next
    }
    dup_label <- labels[duplicated(labels)]
    if (length(dup_label) > 0L) {
      stop(
        sprintf(
          "Duplicate event picker labels in '%s' group: %s",
          group_name,
          paste(unique(dup_label), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    ids <- events_df$event_id[indices]
    dup_id <- ids[duplicated(ids)]
    if (length(dup_id) > 0L) {
      stop(
        sprintf(
          "Duplicate event picker ids in '%s' group: %s",
          group_name,
          paste(unique(dup_id), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

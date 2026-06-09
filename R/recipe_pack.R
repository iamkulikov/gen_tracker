RECIPE_PACK_PREFIX <- "GENPACK1:"

isRecipePackCode <- function(code) {
  length(code) == 1L && nzchar(code) && startsWith(trimws(code), RECIPE_PACK_PREFIX)
}

coerceRecipesTable <- function(recipes) {
  if (is.data.frame(recipes)) {
    return(tibble::as_tibble(recipes))
  }
  if (is.list(recipes) && length(recipes) > 0L && is.list(recipes[[1L]])) {
    return(dplyr::bind_rows(recipes))
  }
  tibble::as_tibble(recipes)
}

encodeRecipePack <- function(recipes, view_state) {
  recipes_tbl <- coerceRecipesTable(recipes)
  if (nrow(recipes_tbl) == 0L) {
    stop("Cannot encode an empty recipe pack.")
  }
  if (nrow(recipes_tbl) > 4L) {
    stop("Recipe pack supports at most 4 queries.")
  }

  metric <- view_state$metric
  year_range <- view_state$year_range
  if (length(year_range) != 2L) {
    stop("view_state$year_range must contain min and max year.")
  }

  header <- paste(
    sprintf("metric=%s", metric),
    sprintf("year_min=%d", as.integer(year_range[[1]])),
    sprintf("year_max=%d", as.integer(year_range[[2]])),
    sprintf(
      "show_projection=%s",
      if (isTRUE(view_state$show_projection)) "TRUE" else "FALSE"
    ),
    sep = ";"
  )

  recipe_codes <- vapply(
    seq_len(nrow(recipes_tbl)),
    function(i) {
      row <- as.list(recipes_tbl[i, ])
      row$metric <- metric
      encodeRecipe(row)
    },
    character(1)
  )

  paste0(RECIPE_PACK_PREFIX, header, "||", paste(recipe_codes, collapse = "||"))
}

parseRecipePackHeader <- function(header) {
  kv <- strsplit(header, ";", fixed = TRUE)[[1]]
  parsed <- stats::setNames(
    object = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[2], character(1)),
    nm = vapply(strsplit(kv, "=", fixed = TRUE), function(x) x[1], character(1))
  )

  metric <- parsed[["metric"]]
  year_min <- suppressWarnings(as.integer(parsed[["year_min"]]))
  year_max <- suppressWarnings(as.integer(parsed[["year_max"]]))
  show_projection <- identical(parsed[["show_projection"]], "TRUE")

  if (is.null(metric) || !nzchar(metric)) {
    stop("Recipe pack header is missing metric.")
  }
  if (!metric %in% c("count", "share_total_population", "share_working_age_population")) {
    stop(
      "Recipe pack metric must be count, share_total_population, or share_working_age_population."
    )
  }
  if (!is.finite(year_min) || !is.finite(year_max)) {
    stop("Recipe pack header must include numeric year_min and year_max.")
  }
  if (year_min > year_max) {
    stop("Recipe pack year_min must not exceed year_max.")
  }

  list(
    metric = metric,
    year_range = c(year_min, year_max),
    show_projection = show_projection
  )
}

decodeRecipePack <- function(code) {
  code <- trimws(code)
  if (!isRecipePackCode(code)) {
    stop("Unsupported recipe pack version. Expected GENPACK1.")
  }

  payload <- sub(paste0("^", RECIPE_PACK_PREFIX), "", code)
  parts <- strsplit(payload, "||", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    stop("Recipe pack must include a header and at least one GEN2 recipe code.")
  }

  header <- parseRecipePackHeader(parts[[1]])
  recipe_codes <- parts[-1]
  if (length(recipe_codes) > 4L) {
    stop("Recipe pack supports at most 4 queries.")
  }
  if (!all(startsWith(recipe_codes, "GEN2:"))) {
    stop("Each recipe in a pack must use the GEN2 format.")
  }

  recipes <- lapply(recipe_codes, decodeRecipe)
  recipes <- assignRecipePackQueryIds(recipes)

  list(
    metric = header$metric,
    year_range = header$year_range,
    show_projection = header$show_projection,
    recipes = recipes
  )
}

assignRecipePackQueryIds <- function(recipes) {
  purrr::imap(
    recipes,
    function(recipe, i) {
      recipe$query_id <- sprintf("q%d", i)
      recipe
    }
  )
}

normalizeRecipePackViewState <- function(pack, year_bounds = NULL) {
  year_range <- pack$year_range
  if (!is.null(year_bounds)) {
    year_range[[1]] <- max(as.integer(year_bounds$min), year_range[[1]])
    year_range[[2]] <- min(as.integer(year_bounds$max), year_range[[2]])
    if (year_range[[1]] > year_range[[2]]) {
      year_range <- c(
        as.integer(year_bounds$min),
        as.integer(year_bounds$max)
      )
    }
  }

  list(
    metric = pack$metric,
    year_range = year_range,
    show_projection = isTRUE(pack$show_projection)
  )
}

validateRecipePack <- function(
  code,
  countries,
  events,
  age_groups,
  event_countries = NULL,
  year_bounds = NULL
) {
  errors <- character(0)
  warnings <- character(0)

  pack <- tryCatch(
    decodeRecipePack(code),
    error = function(e) {
      return(list(error = conditionMessage(e)))
    }
  )
  if (!is.null(pack$error)) {
    return(list(
      valid = FALSE,
      errors = pack$error,
      warnings = warnings,
      pack = NULL
    ))
  }

  if (length(pack$recipes) == 0L) {
    errors <- c(errors, "Recipe pack contains no queries.")
  }

  purrr::iwalk(pack$recipes, function(recipe, i) {
    state <- assessRecipe(
      recipe = recipe,
      countries = countries,
      events = events,
      age_groups = age_groups,
      event_countries = event_countries,
      max_queries = length(pack$recipes)
    )
    if (!state$valid) {
      prefix <- sprintf("Query %d", i)
      errors <<- c(errors, paste(prefix, state$errors, sep = ": "))
    }
    if (length(state$warnings) > 0) {
      warnings <<- c(warnings, paste(sprintf("Query %d", i), state$warnings, sep = ": "))
    }
  })

  if (!is.null(year_bounds)) {
    yr_min <- pack$year_range[[1]]
    yr_max <- pack$year_range[[2]]
    if (yr_min < year_bounds$min || yr_max > year_bounds$max) {
      warnings <- c(
        warnings,
        sprintf(
          "Pack year range %s–%s was outside available data (%s–%s) and will be clamped on import.",
          yr_min,
          yr_max,
          year_bounds$min,
          year_bounds$max
        )
      )
    }
  }

  list(
    valid = length(errors) == 0L,
    errors = unique(errors),
    warnings = unique(warnings),
    pack = if (length(errors) == 0L) {
      list(
        metric = pack$metric,
        year_range = pack$year_range,
        show_projection = pack$show_projection,
        recipes = pack$recipes
      )
    } else {
      NULL
    }
  )
}

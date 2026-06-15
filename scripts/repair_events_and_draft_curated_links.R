#!/usr/bin/env Rscript
# Rebuild draft 2_event_countries_curated.csv from 1_events.csv.
# Optional: repair malformed whole-row quoting in 1_events.csv (--repair-events).
#
# By default only events with include_in_core_catalogue=TRUE are linked
# (multi_country + global). Pass --all-events to include non-core candidates.

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
repair_events <- "--repair-events" %in% args
core_only <- !("--all-events" %in% args)

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")
events_path <- eventsDataPath(data_dir)
curated_path <- eventCountriesCuratedPath(data_dir)
countries_path <- countriesDataPath(data_dir)

eventPrefix <- function(event_id) {
  sub("_.*", "", event_id)
}

tokenCountryMap <- c(
  AFG = "AFG",
  ALGERIA = "DZA",
  ALGERIAN = "DZA",
  ARGENTINA = "ARG",
  BANGLADESH = "BGD",
  BIAFRA = "NGA",
  CHINA = "CHN",
  CHINESE = "CHN",
  CONGO = "COD",
  CUBA = "CUB",
  CYPRUS = "CYP",
  EAST_TIMOR = "TLS",
  ERITREA = "ERI",
  ETHIOPIA = "ETH",
  FALKLANDS = "FLK",
  FRANCE = "FRA",
  GERMANY = "DEU",
  INDIA = "IND",
  INDO = "IND",
  INDONESIA = "IDN",
  INDOPAK = "IND",
  IRAN = "IRN",
  IRAQ = "IRQ",
  ISRAEL = "ISR",
  JAPAN = "JPN",
  KASHMIR = "PAK",
  KOREA = "KOR",
  KOREAN = "KOR",
  KUWAIT = "KWT",
  LIBYA = "LBY",
  MALVINAS = "FLK",
  MEXICO = "MEX",
  MOROCCO = "MAR",
  MYANMAR = "MMR",
  NORTH = "PRK",
  PAK = "PAK",
  PAKISTAN = "PAK",
  PALESTINE = "PSE",
  POLAND = "POL",
  ROHINGYA = "MMR",
  RUSSIA = "RUS",
  RUSSIAN = "RUS",
  SAHARA = "ESH",
  SINO = "CHN",
  SOUTH = "SSD",
  SOVIET = "RUS",
  SUDAN = "SDN",
  SYRIA = "SYR",
  TAIWAN = "TWN",
  TANZANIA = "TZA",
  THAILAND = "THA",
  TURKEY = "TUR",
  UKRAINE = "UKR",
  USA = "USA",
  UGANDA = "UGA",
  UNITED = "USA",
  VIET = "VNM",
  VIETNAM = "VNM",
  YEMEN = "YEM"
)

resolveTokensFromText <- function(text) {
  if (length(text) == 0L || is.na(text) || !nzchar(text)) {
    return(character(0))
  }
  upper <- toupper(text)
  hits <- character(0)
  for (token in names(tokenCountryMap)) {
    if (grepl(token, upper, fixed = TRUE)) {
      hits <- c(hits, tokenCountryMap[[token]])
    }
  }
  unique(hits)
}

eventNameFromId <- function(event_id) {
  slug <- sub("^[^_]+_", "", event_id)
  gsub("_", " ", slug)
}

repairInvalidEventFamilies <- function(events) {
  if (!"event_family" %in% names(events)) {
    return(events)
  }
  energy_as_family <- events$event_family == "energy" & isManualCuratedV2Event(events)
  if (any(energy_as_family)) {
    events$event_family[energy_as_family & events$event_type == "economy"] <- "macro_financial_crisis"
    still <- energy_as_family & events$event_family == "energy"
    events$event_family[still] <- "development_model_policy"
  }
  events
}

injectMissingEventName <- function(line) {
  pattern <- "^([A-Z0-9_]+),(war|economy|politics|policy|society|health|disaster|technology|energy|culture|sport),(national|multi_country|global),"
  if (!grepl(pattern, line, perl = TRUE)) {
    return(line)
  }
  id <- sub(",.*$", "", line)
  name <- eventNameFromId(id)
  sub(paste0("^", id, ","), paste0(id, ",", name, ","), line, perl = TRUE)
}

unwrapMalformedCsvLine <- function(line, header_line) {
  line <- trimws(line)
  if (!nzchar(line)) {
    return(NULL)
  }
  if (grepl("^\"", line) && grepl("\"$", line)) {
    inner <- substr(line, 2L, nchar(line) - 1L)
    inner <- gsub("\"\"", "\"", inner, fixed = TRUE)
    line <- inner
  }
  line <- injectMissingEventName(line)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(header_line, line), tmp, useBytes = TRUE)
  parsed <- readr::read_csv(tmp, show_col_types = FALSE, na = c("", "NA"))
  if (nrow(parsed) != 1L) {
    stop(
      "Expected one row after parse, got ", nrow(parsed), ": ",
      substr(line, 1L, min(120L, nchar(line))),
      call. = FALSE
    )
  }
  parsed
}

fixEventsCsv <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(lines) < 1L) {
    stop("Empty events file: ", path, call. = FALSE)
  }
  header_line <- lines[[1L]]
  header <- strsplit(header_line, ",", fixed = TRUE)[[1L]]
  expected_cols <- c(
    "event_id", "event_name", "event_type", "event_scope",
    "start_year", "end_year", "peak_year", "event_origin", "cross_country_allowed",
    "short_description", "source_url", "event_family", "selection_channel",
    "population_reach_score", "intensity_score", "institutional_discontinuity_score",
    "memory_salience_score", "cohort_relevance_score", "source_confidence_score",
    "include_in_core_catalogue"
  )
  if (!identical(header, expected_cols)) {
    stop(
      "Unexpected header in ", path, ". Expected 20 v2 columns.",
      call. = FALSE
    )
  }

  rows <- lapply(lines[-1L], function(line) {
    tryCatch(
      unwrapMalformedCsvLine(line, header_line),
      error = function(e) {
        stop("Failed to parse line: ", conditionMessage(e), call. = FALSE)
      }
    )
  })
  out <- bind_rows(rows)
  out <- normalizeEventCurationFields(out)
  out <- repairInvalidEventFamilies(out)
  if (nrow(out) != length(lines) - 1L) {
    stop("Row count mismatch after fix.", call. = FALSE)
  }
  readr::write_csv(out, path)
  message("Fixed ", nrow(out), " event rows in ", path)
  out
}

buildDraftCuratedLinks <- function(events, valid_country_ids, core_only = TRUE) {
  cross_scope <- events |>
    filter(.data$event_scope %in% c("multi_country", "global"))

  if (isTRUE(core_only)) {
    if (!"include_in_core_catalogue" %in% names(cross_scope)) {
      stop(
        "include_in_core_catalogue column required for core-only curated links.",
        call. = FALSE
      )
    }
    cross_scope <- cross_scope |>
      filter(.data$include_in_core_catalogue == TRUE)
  }

  if (nrow(cross_scope) == 0L) {
    return(tibble(
      event_id = character(),
      country_id = character(),
      country_role = character()
    ))
  }

  multi_links <- cross_scope |>
    filter(.data$event_scope == "multi_country") |>
    mutate(prefix = vapply(.data$event_id, eventPrefix, character(1))) |>
    rowwise() |>
    mutate(
      country_ids = list(unique(c(
        prefix,
        resolveTokensFromText(.data$event_name),
        resolveTokensFromText(.data$event_id)
      )))
    ) |>
    ungroup() |>
    tidyr::unnest(country_ids) |>
    transmute(
      event_id = .data$event_id,
      country_id = .data$country_ids,
      country_role = "affected"
    ) |>
    filter(.data$country_id %in% valid_country_ids)

  global_ids <- cross_scope |>
    filter(.data$event_scope == "global") |>
    pull(.data$event_id)

  if (length(global_ids) > 0L) {
    global_links <- tidyr::crossing(
      event_id = global_ids,
      country_id = valid_country_ids
    ) |>
      mutate(country_role = "affected")
    multi_links <- bind_rows(multi_links, global_links)
  }

  multi_links |>
    distinct(.data$event_id, .data$country_id, .keep_all = TRUE) |>
    arrange(.data$event_id, .data$country_id)
}

if (isTRUE(repair_events)) {
  events <- fixEventsCsv(events_path)
} else {
  events <- loadEvents(events_path)
}
events <- normalizeEventCurationFields(events)
countries <- readr::read_csv(countries_path, show_col_types = FALSE)
valid_country_ids <- countries$country_id

draft_links <- buildDraftCuratedLinks(events, valid_country_ids, core_only = core_only)
readr::write_csv(draft_links, curated_path)

message(
  "Wrote curated links (core_only=", core_only, "): ", nrow(draft_links), " rows to ",
  curated_path, " (", n_distinct(draft_links$event_id), " cross-country/global events)"
)

withCallingHandlers(
  validateEvents(
    events,
    countries = countries,
    event_countries = draft_links
  ),
  warning = function(w) {
    message("WARN: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
message("Validation OK after fix.")

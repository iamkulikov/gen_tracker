#!/usr/bin/env Rscript
# Build manual event-country links and merge deploy event_countries.csv (variant C).

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

data_dir <- Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data")
events_path <- resolveEventDataPath("events.csv", data_dir)
countries_path <- resolveEventDataPath("countries.csv", data_dir)
manual_layer_path <- eventCountriesManualLayerPath(data_dir)

if (!file.exists(events_path)) {
  stop("Missing ", events_path, ". Place events.csv in data/ before running this script.")
}
if (!file.exists(countries_path)) {
  stop("Missing ", countries_path, ". Place countries.csv in data/ before running this script.")
}

events <- read_csv(events_path, show_col_types = FALSE)
countries <- read_csv(countries_path, show_col_types = FALSE)
valid_country_ids <- countries$country_id

eventPrefix <- function(event_id) {
  sub("_.*", "", event_id)
}

# Token -> ISO3 country_id (must exist in countries.csv)
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
  upper <- toupper(text)
  hits <- character(0)
  for (token in names(tokenCountryMap)) {
    if (grepl(token, upper, fixed = TRUE)) {
      hits <- c(hits, tokenCountryMap[[token]])
    }
  }
  unique(hits)
}

# Curated cross-country links: source of truth is data/event_countries_manual.csv.
# This file is hand-editable and authoritative for manually curated
# origin/affected/culturally_relevant links. The in-code explicitMultiLinks
# table was removed in v2; token parsing below remains only as a fallback for
# legacy multi-country events not yet present in the curated file.
manual_links_source_path <- resolveEventDataPath("event_countries_manual.csv", data_dir)
loadCuratedManualLinks <- function(path) {
  if (!file.exists(path)) {
    return(tibble(
      event_id = character(),
      country_id = character(),
      country_role = character()
    ))
  }
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      event_id = as.character(event_id),
      country_id = as.character(country_id),
      country_role = as.character(country_role)
    )
}

curated_multi_links <- loadCuratedManualLinks(manual_links_source_path)

allowed_roles <- c("affected", "origin", "culturally_relevant")
bad_roles <- setdiff(unique(curated_multi_links$country_role), allowed_roles)
if (length(bad_roles) > 0) {
  stop(
    "Unsupported country_role in ", manual_links_source_path, ": ",
    paste(bad_roles, collapse = ", ")
  )
}

buildNationalLinks <- function(events) {
  events |>
    filter(event_scope == "national") |>
    mutate(
      country_id = vapply(event_id, eventPrefix, character(1)),
      country_role = "affected"
    ) |>
    select(event_id, country_id, country_role)
}

# Token fallback for legacy multi-country events not covered by the curated
# manual links file. Curated links remain authoritative.
buildTokenFallbackLinks <- function(events, curated_ids) {
  events |>
    filter(event_scope == "multi_country", !event_id %in% curated_ids) |>
    mutate(prefix = vapply(event_id, eventPrefix, character(1))) |>
    rowwise() |>
    mutate(
      country_ids = list({
        from_name <- resolveTokensFromText(event_name)
        from_id <- resolveTokensFromText(event_id)
        unique(c(prefix, from_name, from_id))
      })
    ) |>
    ungroup() |>
    tidyr::unnest(country_ids) |>
    transmute(
      event_id = event_id,
      country_id = country_ids,
      country_role = "affected"
    )
}

national_links <- buildNationalLinks(events)
token_links <- buildTokenFallbackLinks(events, curated_multi_links$event_id)

# Curated links first so their explicit country_role wins over the default
# "affected" role assigned by national/token derivation in distinct().
event_countries <- bind_rows(curated_multi_links, national_links, token_links) |>
  distinct(event_id, country_id, .keep_all = TRUE) |>
  filter(country_id %in% valid_country_ids) |>
  arrange(event_id, country_id)

prefixes <- unique(vapply(events$event_id, eventPrefix, character(1)))
invalid_prefixes <- setdiff(prefixes, c(valid_country_ids, "GLB"))
if (length(invalid_prefixes) > 0) {
  warning(
    "Some event prefixes are not country IDs and were skipped in links: ",
    paste(invalid_prefixes, collapse = ", ")
  )
}

national_events <- events |> filter(event_scope == "national")
missing_national <- national_events$event_id[
  !national_events$event_id %in% event_countries$event_id
]
if (length(missing_national) > 0) {
  stop(
    "National events without country links: ",
    paste(head(missing_national, 10), collapse = ", "),
    if (length(missing_national) > 10) " ..." else ""
  )
}

orphan_links <- setdiff(event_countries$event_id, events$event_id)
if (length(orphan_links) > 0) {
  stop("Links reference unknown event_id values.")
}

writeEventCountryLayer(event_countries, manual_layer_path)
merged <- mergeDeployEventLinks(data_dir = data_dir, manual_links = event_countries)

message(sprintf(
  "Wrote %s manual rows to %s; merged deploy file %s (%d rows total).",
  nrow(event_countries),
  manual_layer_path,
  merged$deploy_path,
  nrow(merged$links)
))

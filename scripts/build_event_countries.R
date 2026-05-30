#!/usr/bin/env Rscript
# Build data/event_countries.csv from data/events.csv and data/countries.csv.
#
# Rules (see docs/dev_plan.md §4.3):
# - national: at least one link to the country implied by the event_id prefix
# - multi_country: primary prefix country plus known counterpart countries
# - global: no links required

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())

events_path <- file.path("data", "events.csv")
countries_path <- file.path("data", "countries.csv")
output_path <- file.path("data", "event_countries.csv")

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

# Explicit multi-country links where token parsing is insufficient.
explicitMultiLinks <- tribble(
  ~event_id, ~country_id, ~country_role,
  "IND_PARTITION_INDEPENDENCE", "IND", "affected",
  "IND_PARTITION_INDEPENDENCE", "PAK", "affected",
  "IND_PARTITION_INDEPENDENCE", "BGD", "origin",
  "IND_INDO_PAK_WAR_1965", "IND", "affected",
  "IND_INDO_PAK_WAR_1965", "PAK", "affected",
  "BGD_PARTITION_EAST_PAKISTAN", "BGD", "affected",
  "BGD_PARTITION_EAST_PAKISTAN", "PAK", "origin",
  "BGD_LIBERATION_WAR", "BGD", "affected",
  "BGD_LIBERATION_WAR", "PAK", "origin",
  "BGD_LIBERATION_WAR", "IND", "culturally_relevant",
  "BGD_ROHINGYA_INFLUX", "BGD", "affected",
  "BGD_ROHINGYA_INFLUX", "MMR", "origin",
  "PAK_PARTITION_INDEPENDENCE", "PAK", "affected",
  "PAK_PARTITION_INDEPENDENCE", "IND", "origin",
  "PAK_BANGLADESH_WAR_1971", "PAK", "affected",
  "PAK_BANGLADESH_WAR_1971", "BGD", "affected",
  "PAK_BANGLADESH_WAR_1971", "IND", "culturally_relevant",
  "PAK_AFGHAN_JIHAD_REFUGEES", "PAK", "affected",
  "PAK_AFGHAN_JIHAD_REFUGEES", "AFG", "origin",
  "PAK_KASHMIR_EARTHQUAKE", "PAK", "affected",
  "PAK_KASHMIR_EARTHQUAKE", "IND", "affected",
  "PAK_WAR_ON_TERROR", "PAK", "affected",
  "PAK_WAR_ON_TERROR", "AFG", "origin",
  "IDN_NATIONAL_REVOLUTION", "IDN", "affected",
  "IDN_NATIONAL_REVOLUTION", "NLD", "origin",
  "IDN_ASIAN_CRISIS_REFORMASI", "IDN", "affected",
  "IDN_EAST_TIMOR_CRISIS", "IDN", "affected",
  "IDN_EAST_TIMOR_CRISIS", "TLS", "affected",
  "IDN_ACEH_TSUNAMI", "IDN", "affected",
  "IDN_ACEH_TSUNAMI", "THA", "affected",
  "IDN_ACEH_TSUNAMI", "IND", "affected",
  "NGA_BOKO_HARAM_INSURGENCY", "NGA", "affected",
  "NGA_BOKO_HARAM_INSURGENCY", "NER", "affected",
  "NGA_BOKO_HARAM_INSURGENCY", "CMR", "affected",
  "NGA_EBOLA_CONTAINMENT", "NGA", "affected",
  "NGA_EBOLA_CONTAINMENT", "GIN", "origin",
  "NGA_EBOLA_CONTAINMENT", "LBR", "origin",
  "NGA_EBOLA_CONTAINMENT", "SLE", "origin",
  "RUS_AFGHAN_WAR", "RUS", "affected",
  "RUS_AFGHAN_WAR", "AFG", "affected",
  "RUS_SOVIET_DISSOLUTION_SHOCK", "RUS", "affected",
  "RUS_SOVIET_DISSOLUTION_SHOCK", "UKR", "affected",
  "RUS_SOVIET_DISSOLUTION_SHOCK", "BLR", "affected",
  "RUS_SOVIET_DISSOLUTION_SHOCK", "KAZ", "affected",
  "RUS_UKRAINE_WAR", "RUS", "affected",
  "RUS_UKRAINE_WAR", "UKR", "affected",
  "USA_VIETNAM_WAR", "USA", "affected",
  "USA_VIETNAM_WAR", "VNM", "affected",
  "USA_STAGFLATION_OIL_SHOCK", "USA", "affected",
  "USA_AFGHANISTAN_IRAQ_WARS", "USA", "affected",
  "USA_AFGHANISTAN_IRAQ_WARS", "AFG", "affected",
  "USA_AFGHANISTAN_IRAQ_WARS", "IRQ", "affected",
  "CHN_WTO_ACCESSION", "CHN", "affected",
  "CHN_SARS_OUTBREAK", "CHN", "origin",
  "CHN_SARS_OUTBREAK", "HKG", "affected",
  "CHN_SARS_OUTBREAK", "VNM", "affected",
  "KOR_KOREAN_WAR", "KOR", "affected",
  "KOR_KOREAN_WAR", "PRK", "affected",
  "KOR_KOREAN_WAR", "USA", "affected",
  "KOR_KOREAN_WAR", "CHN", "affected",
  "KOR_DIVISION_AFTERSHOCK", "KOR", "affected",
  "KOR_DIVISION_AFTERSHOCK", "PRK", "affected",
  "KOR_WORLD_CUP_2002", "KOR", "affected",
  "KOR_WORLD_CUP_2002", "JPN", "affected",
  "KOR_ASIAN_FINANCIAL_CRISIS", "KOR", "affected",
  "KOR_KOREAN_WAVE", "KOR", "origin",
  "VNM_VIETNAM_WAR", "VNM", "affected",
  "VNM_VIETNAM_WAR", "USA", "affected",
  "VNM_FIRST_INDOCHINA_FINAL", "VNM", "affected",
  "VNM_FIRST_INDOCHINA_FINAL", "FRA", "origin",
  "VNM_BOAT_PEOPLE_EXODUS", "VNM", "origin",
  "VNM_SINO_VIETNAMESE_WAR", "VNM", "affected",
  "VNM_SINO_VIETNAMESE_WAR", "CHN", "affected",
  "VNM_US_NORMALIZATION", "VNM", "affected",
  "VNM_US_NORMALIZATION", "USA", "affected",
  "VNM_WTO_EXPORT_BOOM", "VNM", "affected",
  "VNM_SEA_PROTESTS_2014", "VNM", "affected",
  "VNM_SEA_PROTESTS_2014", "CHN", "origin",
  "UKR_INDEPENDENCE", "UKR", "affected",
  "UKR_INDEPENDENCE", "RUS", "origin",
  "UKR_CHERNOBYL_DISASTER", "UKR", "affected",
  "UKR_CHERNOBYL_DISASTER", "BLR", "affected",
  "UKR_GAS_DISPUTES", "UKR", "affected",
  "UKR_GAS_DISPUTES", "RUS", "origin",
  "UKR_CRIMEA_DONBAS_WAR", "UKR", "affected",
  "UKR_CRIMEA_DONBAS_WAR", "RUS", "affected",
  "UKR_FULL_SCALE_INVASION", "UKR", "affected",
  "UKR_FULL_SCALE_INVASION", "RUS", "origin",
  "UKR_EU_CANDIDATE_STATUS", "UKR", "affected",
  "ETH_ERITREA_FEDERATION_ANNEXATION", "ETH", "affected",
  "ETH_ERITREA_FEDERATION_ANNEXATION", "ERI", "affected",
  "ETH_ERITREAN_WAR", "ETH", "affected",
  "ETH_ERITREAN_WAR", "ERI", "affected",
  "ETH_GERD_PROJECT", "ETH", "affected",
  "ETH_GERD_PROJECT", "EGY", "affected",
  "ETH_GERD_PROJECT", "SDN", "affected",
  "COD_CONGO_CRISIS", "COD", "affected",
  "COD_CONGO_CRISIS", "FRA", "origin",
  "COD_FIRST_CONGO_WAR", "COD", "affected",
  "COD_FIRST_CONGO_WAR", "RWA", "origin",
  "COD_FIRST_CONGO_WAR", "UGA", "origin",
  "COD_SECOND_CONGO_WAR", "COD", "affected",
  "COD_SECOND_CONGO_WAR", "RWA", "affected",
  "COD_SECOND_CONGO_WAR", "UGA", "affected",
  "COD_KIVU_CONFLICT", "COD", "affected",
  "COD_KIVU_CONFLICT", "RWA", "affected",
  "TZA_INDEPENDENCE_UNION", "TZA", "affected",
  "TZA_INDEPENDENCE_UNION", "ZMB", "origin",
  "TZA_UGANDA_WAR", "TZA", "affected",
  "TZA_UGANDA_WAR", "UGA", "affected",
  "UGA_TANZANIA_WAR", "UGA", "affected",
  "UGA_TANZANIA_WAR", "TZA", "affected",
  "UGA_LRA_CONFLICT", "UGA", "affected",
  "UGA_LRA_CONFLICT", "COD", "affected",
  "UGA_LRA_CONFLICT", "SSD", "affected",
  "SDN_SOUTH_SUDAN_SECESSION", "SDN", "affected",
  "SDN_SOUTH_SUDAN_SECESSION", "SSD", "affected",
  "IRQ_IRAN_IRAQ_WAR", "IRQ", "affected",
  "IRQ_IRAN_IRAQ_WAR", "IRN", "affected",
  "IRQ_GULF_WAR", "IRQ", "affected",
  "IRQ_GULF_WAR", "KWT", "affected",
  "IRQ_GULF_WAR", "USA", "affected",
  "IRQ_SANCTIONS_DECADE", "IRQ", "affected",
  "IRQ_2003_INVASION", "IRQ", "affected",
  "IRQ_2003_INVASION", "USA", "origin",
  "IRQ_2003_INVASION", "GBR", "origin",
  "IRQ_ISIS_WAR", "IRQ", "affected",
  "IRQ_ISIS_WAR", "SYR", "affected",
  "IRN_1953_COUP", "IRN", "affected",
  "IRN_1953_COUP", "GBR", "origin",
  "IRN_1953_COUP", "USA", "origin",
  "IRN_IRAQ_WAR", "IRN", "affected",
  "IRN_IRAQ_WAR", "IRQ", "affected",
  "IRN_NUCLEAR_DEAL_SANCTIONS", "IRN", "affected",
  "IRN_NUCLEAR_DEAL_SANCTIONS", "USA", "origin",
  "EGY_SUEZ_CRISIS", "EGY", "affected",
  "EGY_SUEZ_CRISIS", "GBR", "origin",
  "EGY_SUEZ_CRISIS", "FRA", "origin",
  "EGY_SUEZ_CRISIS", "ISR", "affected",
  "EGY_SIX_DAY_WAR", "EGY", "affected",
  "EGY_SIX_DAY_WAR", "ISR", "affected",
  "EGY_SIX_DAY_WAR", "JOR", "affected",
  "EGY_OCTOBER_WAR", "EGY", "affected",
  "EGY_OCTOBER_WAR", "ISR", "affected",
  "EGY_CAMP_DAVID_PEACE", "EGY", "affected",
  "EGY_CAMP_DAVID_PEACE", "ISR", "affected",
  "DZA_ALGERIAN_WAR", "DZA", "affected",
  "DZA_ALGERIAN_WAR", "FRA", "origin",
  "DZA_ARAB_SPRING_PROTESTS", "DZA", "affected",
  "MAR_GREEN_MARCH", "MAR", "affected",
  "MAR_GREEN_MARCH", "ESH", "affected",
  "MAR_WESTERN_SAHARA_WAR", "MAR", "affected",
  "MAR_WESTERN_SAHARA_WAR", "ESH", "affected",
  "SAU_GULF_SECURITY_ERA", "SAU", "affected",
  "SAU_GULF_SECURITY_ERA", "IRQ", "origin",
  "SAU_GULF_WAR_TROOPS", "SAU", "affected",
  "SAU_GULF_WAR_TROOPS", "IRQ", "origin",
  "SAU_GULF_WAR_TROOPS", "USA", "affected",
  "SAU_ARAB_SPRING_INTERVENTIONS", "SAU", "origin",
  "SAU_ARAB_SPRING_INTERVENTIONS", "YEM", "affected",
  "SAU_YEMEN_INTERVENTION", "SAU", "origin",
  "SAU_YEMEN_INTERVENTION", "YEM", "affected",
  "MMR_ROHINGYA_CRISIS", "MMR", "origin",
  "MMR_ROHINGYA_CRISIS", "BGD", "affected",
  "PHL_INDEPENDENCE", "PHL", "affected",
  "PHL_INDEPENDENCE", "USA", "origin",
  "PHL_ASIAN_FINANCIAL_CRISIS", "PHL", "affected",
  "MEX_NAFTA_LAUNCH", "MEX", "affected",
  "MEX_NAFTA_LAUNCH", "USA", "affected",
  "MEX_NAFTA_LAUNCH", "CAN", "affected",
  "COL_PLAN_COLOMBIA", "COL", "affected",
  "COL_PLAN_COLOMBIA", "USA", "origin",
  "COL_VENEZUELAN_MIGRATION", "COL", "affected",
  "COL_VENEZUELAN_MIGRATION", "VEN", "origin",
  "ARG_FALKLANDS_WAR", "ARG", "affected",
  "ARG_FALKLANDS_WAR", "GBR", "affected",
  "KEN_EMBASSY_BOMBING", "KEN", "affected",
  "KEN_EMBASSY_BOMBING", "TZA", "affected",
  "KEN_EMBASSY_BOMBING", "USA", "origin",
  "DEU_OSTPOLITIK", "DEU", "affected",
  "DEU_OSTPOLITIK", "POL", "affected",
  "DEU_OIL_SHOCK_RECESSION", "DEU", "affected",
  "DEU_EUROZONE_CRISIS_RESPONSE", "DEU", "affected",
  "DEU_EUROZONE_CRISIS_RESPONSE", "GRC", "affected",
  "GBR_SUEZ_CRISIS", "GBR", "affected",
  "GBR_SUEZ_CRISIS", "EGY", "affected",
  "GBR_TROUBLES", "GBR", "affected",
  "GBR_TROUBLES", "IRL", "affected",
  "GBR_EEC_ENTRY", "GBR", "affected",
  "GBR_BLACK_WEDNESDAY", "GBR", "affected",
  "GBR_IRAQ_WAR", "GBR", "affected",
  "GBR_IRAQ_WAR", "IRQ", "affected",
  "GBR_2008_FINANCIAL_CRISIS", "GBR", "affected",
  "GBR_BREXIT", "GBR", "affected",
  "FRA_ALGERIAN_WAR", "FRA", "origin",
  "FRA_ALGERIAN_WAR", "DZA", "affected",
  "FRA_TRENTE_GLORIEUSES_END", "FRA", "affected",
  "FRA_EURO_ADOPTION", "FRA", "affected",
  "FRA_EURO_ADOPTION", "DEU", "affected",
  "ITA_LIRA_CRISIS", "ITA", "affected",
  "ITA_EURO_ADOPTION", "ITA", "affected",
  "ITA_SOVEREIGN_DEBT_CRISIS", "ITA", "affected",
  "ITA_COVID_FIRST_WAVE", "ITA", "affected",
  "ESP_EU_ACCESSION", "ESP", "affected",
  "ESP_2008_HOUSING_CRISIS", "ESP", "affected",
  "POL_NATO_EU_ACCESSION", "POL", "affected",
  "POL_SMOLENSK_DISASTER", "POL", "affected",
  "POL_SMOLENSK_DISASTER", "RUS", "origin",
  "CAN_FREE_TRADE_NAFTA", "CAN", "affected",
  "CAN_FREE_TRADE_NAFTA", "USA", "affected",
  "CAN_FREE_TRADE_NAFTA", "MEX", "affected",
  "TUR_CYPRUS_INTERVENTION", "TUR", "affected",
  "TUR_CYPRUS_INTERVENTION", "CYP", "affected",
  "THA_ASIAN_FINANCIAL_CRISIS", "THA", "affected",
  "THA_2004_TSUNAMI", "THA", "affected",
  "THA_2004_TSUNAMI", "IDN", "affected",
  "THA_2004_TSUNAMI", "IND", "affected",
  "JPN_OIL_SHOCK_INDUSTRIAL_SHIFT", "JPN", "affected"
)

buildNationalLinks <- function(events) {
  events |>
    filter(event_scope == "national") |>
    mutate(
      country_id = vapply(event_id, eventPrefix, character(1)),
      country_role = "affected"
    ) |>
    select(event_id, country_id, country_role)
}

buildMultiLinksFromTokens <- function(events) {
  multi <- events |> filter(event_scope == "multi_country")
  explicit_ids <- explicitMultiLinks$event_id

  token_rows <- multi |>
    filter(!event_id %in% explicit_ids) |>
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
      country_role = if_else(country_id == prefix, "affected", "affected")
    )

  bind_rows(
    explicitMultiLinks,
    token_rows
  )
}

national_links <- buildNationalLinks(events)
multi_links <- buildMultiLinksFromTokens(events)

event_countries <- bind_rows(national_links, multi_links) |>
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

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(event_countries, output_path)

message(sprintf(
  "Wrote %s rows to %s (%d national, %d multi_country links).",
  nrow(event_countries),
  output_path,
  nrow(national_links),
  nrow(multi_links)
))

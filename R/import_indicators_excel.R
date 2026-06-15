normalizeCountryNameKey <- function(x) {
  x |>
    as.character() |>
    tolower() |>
    gsub("[^a-z0-9]+", " ", x = _, perl = TRUE) |>
    gsub("\\s+", " ", x = _, perl = TRUE) |>
    trimws()
}

erCountryIdOverrides <- function() {
  c(
    RU = "RUS",
    UK = "GBR",
    KO = "KOR",
    KS = "KOR"
  )
}

defaultsDbCountryNameAliases <- function() {
  c(
    "cote d ivoire" = "CIV",
    "turkey ottoman empire" = "TUR",
    "cape verde" = "CPV",
    "vietnam" = "VNM",
    "czech republic" = "CZE",
    "yugoslavia" = "SRB",
    "st kitts nevis" = "KNA",
    "congo brazzaville" = "COG",
    "congo kinshasa" = "COD",
    "democratic republic of the congo" = "COD",
    "korea" = "KOR",
    "russia" = "RUS",
    "bolivia" = "BOL",
    "venezuela" = "VEN",
    "united states" = "USA",
    "ivory coast" = "CIV"
  )
}

defaultsDbCountryIdOverrides <- function() {
  c(
    BB = "BRB",
    BY = "BLR",
    CD = "COD",
    CI = "CIV",
    CS = "SRB",
    CV = "CPV",
    CY = "CYP",
    CZ = "CZE",
    KN = "KNA",
    LB = "LBN",
    ME = "MNE",
    RS = "SRB",
    SB = "SLB",
    SR = "SUR",
    TR = "TUR",
    VN = "VNM",
    YU = "SRB"
  )
}

matchCountryNameToIso3 <- function(country_name, countries) {
  if (is.na(country_name) || !nzchar(country_name) || is.null(countries) || nrow(countries) == 0L) {
    return(NA_character_)
  }

  key <- normalizeCountryNameKey(country_name)
  aliases <- defaultsDbCountryNameAliases()
  if (key %in% names(aliases)) {
    return(unname(aliases[[key]]))
  }

  exact <- countries |>
    dplyr::mutate(name_key = normalizeCountryNameKey(.data$country_name)) |>
    dplyr::filter(.data$name_key == .env$key) |>
    dplyr::slice(1)
  if (nrow(exact) > 0L) {
    return(exact$country_id[[1]])
  }

  prefix <- countries |>
    dplyr::mutate(name_key = normalizeCountryNameKey(.data$country_name)) |>
    dplyr::filter(
      startsWith(.data$name_key, .env$key) | startsWith(.env$key, .data$name_key)
    ) |>
    dplyr::arrange(nchar(.data$name_key)) |>
    dplyr::slice(1)
  if (nrow(prefix) > 0L) {
    return(prefix$country_id[[1]])
  }

  token <- strsplit(key, " ", fixed = TRUE)[[1]][[1]]
  if (nzchar(token)) {
    token_hit <- countries |>
      dplyr::mutate(name_key = normalizeCountryNameKey(.data$country_name)) |>
      dplyr::filter(grepl(paste0("\\b", token, "\\b"), .data$name_key, perl = TRUE)) |>
      dplyr::arrange(nchar(.data$name_key)) |>
      dplyr::slice(1)
    if (nrow(token_hit) > 0L) {
      return(token_hit$country_id[[1]])
    }
  }

  NA_character_
}

loadDefaultsDbCountryDictionary <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(country_id = character(), country_name = character()))
  }
  raw <- readxl::read_excel(path, sheet = "RR")
  if (!all(c("country_id", "country_name") %in% names(raw))) {
    stop("Defaults_DB sheet 'RR' must contain country_id and country_name.", call. = FALSE)
  }
  raw |>
    dplyr::transmute(
      country_id = as.character(.data$country_id),
      country_name = as.character(.data$country_name)
    ) |>
    dplyr::filter(!is.na(.data$country_id), nzchar(.data$country_id)) |>
    dplyr::distinct(.data$country_id, .keep_all = TRUE)
}

resolveDefaultsIso3 <- function(defaults_country_id, country_name, countries) {
  code <- trimws(as.character(defaults_country_id))
  if (length(code) != 1L || !nzchar(code)) {
    return(NA_character_)
  }

  if (nchar(code) == 3L && !is.null(countries) && nrow(countries) > 0L) {
    hit <- countries |>
      dplyr::filter(.data$country_id == .env$code) |>
      dplyr::slice(1)
    if (nrow(hit) > 0L) {
      return(hit$country_id[[1]])
    }
  }

  overrides <- defaultsDbCountryIdOverrides()
  if (code %in% names(overrides)) {
    return(unname(overrides[[code]]))
  }

  if (!is.na(country_name) && nzchar(country_name)) {
    hit <- matchCountryNameToIso3(country_name, countries)
    if (!is.na(hit) && nzchar(hit)) {
      return(hit)
    }
  }

  NA_character_
}

defaultsDbSourceVersion <- function(path) {
  info <- tryCatch(
    readxl::read_excel(path, sheet = "info"),
    error = function(e) NULL
  )
  if (!is.null(info) && nrow(info) > 0L) {
    first_col <- names(info)[[1]]
    src <- as.character(info[[first_col]][[1]])
    if (!is.na(src) && nzchar(src)) {
      return(trimws(src))
    }
  }
  "Defaults_DB explicit"
}

importDefaultsExplicitFromExcel <- function(path, countries = NULL, country_dictionary = NULL) {
  if (!file.exists(path)) {
    stop(sprintf("Defaults_DB file not found: %s", path), call. = FALSE)
  }

  raw <- readxl::read_excel(path, sheet = "explicit")
  required <- c("country_id", "year", "default_fitch", "default_moodys", "default_sp")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(
      sprintf("Defaults_DB sheet 'explicit' misses columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (is.null(country_dictionary)) {
    country_dictionary <- loadDefaultsDbCountryDictionary(path)
  }

  name_lookup <- country_dictionary |>
    dplyr::transmute(
      defaults_country_id = .data$country_id,
      country_name = .data$country_name
    )

  out <- raw |>
    dplyr::transmute(
      defaults_country_id = as.character(.data$country_id),
      year = as.integer(.data$year),
      default_fitch = as.integer(.data$default_fitch),
      default_moodys = as.integer(.data$default_moodys),
      default_sp = as.integer(.data$default_sp)
    ) |>
    dplyr::left_join(name_lookup, by = "defaults_country_id") |>
    dplyr::mutate(
      iso3 = mapply(
        resolveDefaultsIso3,
        defaults_country_id = .data$defaults_country_id,
        country_name = .data$country_name,
        MoreArgs = list(countries = countries %||% tibble::tibble()),
        USE.NAMES = FALSE
      ),
      flag = as.integer(pmax(
        .data$default_fitch,
        .data$default_moodys,
        .data$default_sp,
        na.rm = TRUE
      ) >= 1L),
      country_id = .data$iso3,
      source = "Defaults_DB",
      source_version = defaultsDbSourceVersion(path)
    )

  unmapped <- out |>
    dplyr::filter(is.na(.data$country_id) | !nzchar(.data$country_id)) |>
    dplyr::distinct(.data$defaults_country_id)
  if (nrow(unmapped) > 0) {
    stop(
      "Defaults_DB explicit rows reference unmapped country_id codes: ",
      paste(unmapped$defaults_country_id, collapse = ", "),
      call. = FALSE
    )
  }

  out <- out |>
    dplyr::filter(!is.na(.data$country_id), nzchar(.data$country_id)) |>
    dplyr::group_by(.data$country_id, .data$year) |>
    dplyr::summarise(
      flag = as.integer(max(.data$flag, na.rm = TRUE)),
      source = dplyr::first(.data$source),
      source_version = dplyr::first(.data$source_version),
      .groups = "drop"
    )

  if (!is.null(countries) && nrow(countries) > 0L) {
    out <- out |> dplyr::filter(.data$country_id %in% countries$country_id)
  }

  out
}

resolveErIso3 <- function(er_country_id, country_name, countries) {
  overrides <- erCountryIdOverrides()
  if (er_country_id %in% names(overrides)) {
    return(unname(overrides[[er_country_id]]))
  }

  key <- normalizeCountryNameKey(country_name)
  hit <- countries |>
    dplyr::mutate(name_key = normalizeCountryNameKey(.data$country_name)) |>
    dplyr::filter(.data$name_key == .env$key) |>
    dplyr::slice(1)
  if (nrow(hit) == 0) {
    return(NA_character_)
  }
  hit$country_id[[1]]
}

importCpiFromWdiExcel <- function(
  path,
  countries = NULL,
  indicator_code = "FP.CPI.TOTL.ZG"
) {
  if (!file.exists(path)) {
    stop(sprintf("CPI file not found: %s", path), call. = FALSE)
  }

  raw <- readxl::read_excel(path, sheet = "Data", skip = 3)
  year_cols <- names(raw)[grepl("^[0-9]{4}$", names(raw))]
  if (length(year_cols) == 0) {
    stop("CPI sheet 'Data' has no year columns (expected 1960, 1961, ...).", call. = FALSE)
  }

  out <- raw |>
    dplyr::filter(.data[["Indicator Code"]] == .env$indicator_code) |>
    dplyr::transmute(
      country_id = as.character(.data[["Country Code"]]),
      dplyr::across(dplyr::all_of(year_cols), as.numeric)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      source = "World Bank WDI",
      source_version = "FP.CPI.TOTL.ZG"
    ) |>
    dplyr::filter(is.finite(.data$value))

  if (!is.null(countries) && nrow(countries) > 0) {
    out <- out |> dplyr::filter(.data$country_id %in% countries$country_id)
  }

  out
}

importErFromExcel <- function(path, countries = NULL, indicator_code = "usdlc_av") {
  if (!file.exists(path)) {
    stop(sprintf("ER file not found: %s", path), call. = FALSE)
  }

  raw <- readxl::read_excel(path, sheet = "y")
  year_cols <- names(raw)[grepl("^[0-9]{4}$", names(raw))]
  if (length(year_cols) == 0) {
    stop("ER sheet 'y' has no year columns.", call. = FALSE)
  }

  dict <- tryCatch(
    readxl::read_excel(path, sheet = "dict"),
    error = function(e) NULL
  )
  source_name <- "IMF/BIS exchange rate"
  if (!is.null(dict) && "source_name" %in% names(dict)) {
    src <- dict$source_name[dict$indicator_code == indicator_code]
    if (length(src) == 1 && !is.na(src) && nzchar(src)) {
      source_name <- as.character(src)
    }
  }

  rows <- raw |>
    dplyr::filter(.data$indicator_code == .env$indicator_code)

  if (nrow(rows) == 0) {
    return(tibble::tibble(
      country_id = character(),
      year = integer(),
      value = numeric(),
      source = character(),
      source_version = character()
    ))
  }

  long <- rows |>
    dplyr::mutate(
      iso3 = mapply(
        resolveErIso3,
        er_country_id = .data$country_id,
        country_name = .data$country,
        MoreArgs = list(countries = countries %||% tibble::tibble()),
        USE.NAMES = FALSE
      )
    ) |>
    dplyr::filter(!is.na(.data$iso3), nzchar(.data$iso3)) |>
    dplyr::select("iso3", dplyr::all_of(year_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(
      country_id = .data$iso3,
      year = as.integer(.data$year),
      source = source_name,
      source_version = indicator_code
    ) |>
    dplyr::filter(is.finite(.data$value), .data$value > 0) |>
    dplyr::select("country_id", "year", "value", "source", "source_version")

  if (!is.null(countries) && nrow(countries) > 0) {
    long <- long |> dplyr::filter(.data$country_id %in% countries$country_id)
  }

  long
}

writeIndicatorCsv <- function(indicator, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(indicator, path)
  invisible(path)
}

prepareIndicatorsFromExcel <- function(
  data_dir = Sys.getenv("GEN_TRACKER_DATA_DIR", unset = "data"),
  countries_path = NULL,
  cpi_path = NULL,
  er_path = NULL,
  defaults_db_path = NULL,
  indicators_dir = NULL
) {
  countries_path <- countries_path %||% countriesDataPath(data_dir = data_dir)
  cpi_path <- cpi_path %||% resolveIndicatorExcelPath("CPI.xlsx", data_dir = data_dir)
  er_path <- er_path %||% resolveIndicatorExcelPath("ER.xlsx", data_dir = data_dir)
  defaults_db_path <- defaults_db_path %||% resolveIndicatorExcelPath("Defaults_DB.xlsx", data_dir = data_dir)
  indicators_dir <- indicators_dir %||% resolveIndicatorsCsvDir(data_dir = data_dir)

  if (!file.exists(countries_path)) {
    stop("Missing countries.csv for indicator import.", call. = FALSE)
  }

  countries <- loadCountryDictionary(countries_path)
  written <- character(0)

  if (file.exists(cpi_path)) {
    cpi <- importCpiFromWdiExcel(cpi_path, countries = countries)
    validateIndicators(cpi, indicator_name = "cpi_inflation")
    out_path <- file.path(indicators_dir, indicatorCsvFilename("cpi_inflation"))
    writeIndicatorCsv(cpi, out_path)
    written <- c(written, out_path)
  }

  if (file.exists(er_path)) {
    er <- importErFromExcel(er_path, countries = countries)
    validateIndicators(er, indicator_name = "exchange_rate")
    out_path <- file.path(indicators_dir, indicatorCsvFilename("exchange_rate"))
    writeIndicatorCsv(er, out_path)
    written <- c(written, out_path)
  }

  if (file.exists(defaults_db_path)) {
    defaults <- importDefaultsExplicitFromExcel(defaults_db_path, countries = countries)
    validateIndicators(defaults, indicator_name = "sovereign_defaults")
    out_path <- file.path(indicators_dir, indicatorCsvFilename("sovereign_defaults"))
    writeIndicatorCsv(defaults, out_path)
    written <- c(written, out_path)
  }

  list(
    written = written,
    indicators_dir = indicators_dir
  )
}

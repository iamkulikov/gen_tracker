formatMetricLabel <- function(metric) {
  if (metric == "count") {
    if (isTRUE(populationCountStoredInThousands())) {
      return("People, thousands")
    }
    return("Population count")
  }
  if (metric == "share_total_population") {
    return("Share of total population")
  }
  if (metric == "share_working_age_population") {
    return("Share of working-age population")
  }
  metric
}

isShareMetric <- function(metric) {
  metric %in% c("share_total_population", "share_working_age_population")
}

populationCountStoredInThousands <- function() {
  TRUE
}

formatEventYearRange <- function(start_year, end_year) {
  start_year <- suppressWarnings(as.integer(start_year))
  end_year <- suppressWarnings(as.integer(end_year))

  if (length(start_year) == 0L && length(end_year) == 0L) {
    return("")
  }
  if (length(start_year) == 0L || is.na(start_year)) {
    return(as.character(end_year[[1]]))
  }
  if (length(end_year) == 0L || is.na(end_year) || identical(start_year, end_year)) {
    return(as.character(start_year))
  }
  sprintf("%s–%s", start_year, end_year)
}

formatEventChoiceLabel <- function(event_name, start_year, end_year) {
  years <- formatEventYearRange(start_year, end_year)
  if (!nzchar(years)) {
    return(as.character(event_name))
  }
  sprintf("%s (%s)", event_name, years)
}

formatBirthYearRange <- function(birth_years) {
  birth_years <- sort(unique(as.integer(birth_years)))
  birth_years <- birth_years[!is.na(birth_years)]
  if (length(birth_years) == 0L) {
    return("none")
  }
  if (length(birth_years) == 1L) {
    return(as.character(birth_years))
  }
  sprintf("%s–%s", min(birth_years), max(birth_years))
}

formatAgeRangeDetail <- function(age_range) {
  if (is.na(age_range$age_min) && is.na(age_range$age_max)) {
    return(age_range$age_label)
  }
  if (is.na(age_range$age_max)) {
    return(sprintf("%s (%s+)", age_range$age_label, age_range$age_min))
  }
  sprintf("%s (%s–%s)", age_range$age_label, age_range$age_min, age_range$age_max)
}

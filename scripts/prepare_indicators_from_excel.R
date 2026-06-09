#!/usr/bin/env Rscript
# Convert CPI.xlsx, ER.xlsx, and Defaults_DB.xlsx into data/generated/indicators/*.csv

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
result <- prepareIndicatorsFromExcel(data_dir = data_dir)

if (length(result$written) == 0) {
  stop(
    "No indicators written. Place CPI.xlsx, ER.xlsx, and/or Defaults_DB.xlsx under ",
    "data/sources/indicators/ (or legacy data/indicators/) and ensure countries.csv exists."
  )
}

for (path in result$written) {
  message("Wrote ", path)
}

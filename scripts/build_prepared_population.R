# Build data/population.rds from WPP2024 Male/Female Excel files in data/.
#
# Prerequisites (in data/):
#   - countries.csv
#   - WPP2024_*_SINGLE_AGE_*_Male.xlsx
#   - WPP2024_*_SINGLE_AGE_*_Female.xlsx
#
# From project root:
#   source("scripts/build_prepared_population.R")
#
# Non-interactive:
#   Rscript scripts/build_prepared_population.R

locateGenTrackerRoot <- function() {
  env_root <- Sys.getenv("GEN_TRACKER_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/"))
  }

  if (file.exists(file.path("R", "project_root.R")) && file.exists("app.R")) {
    return(normalizePath(getwd(), winslash = "/"))
  }

  if (file.exists(file.path("..", "R", "project_root.R")) && file.exists(file.path("..", "app.R"))) {
    return(normalizePath("..", winslash = "/"))
  }

  stop(
    "Cannot find gen_tracker project root. ",
    "setwd() to the project folder or set GEN_TRACKER_PROJECT_ROOT.",
    call. = FALSE
  )
}

root_dir <- locateGenTrackerRoot()
source(file.path(root_dir, "R", "project_root.R"), local = FALSE)
loadProjectSources(root_dir)
setwd(root_dir)

buildPreparedPopulation()

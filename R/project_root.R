resolveProjectRoot <- function() {
  env_root <- Sys.getenv("GEN_TRACKER_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/"))
  }

  wd <- normalizePath(getwd(), winslash = "/")
  if (dir.exists(file.path(wd, "R")) && file.exists(file.path(wd, "app.R"))) {
    return(wd)
  }

  parent <- normalizePath(file.path(wd, ".."), winslash = "/")
  if (dir.exists(file.path(parent, "R")) && file.exists(file.path(parent, "app.R"))) {
    return(parent)
  }

  stop(
    "Cannot resolve project root. ",
    "setwd() to the gen_tracker folder or set GEN_TRACKER_PROJECT_ROOT.",
    call. = FALSE
  )
}

loadProjectSources <- function(root_dir = resolveProjectRoot()) {
  r_dir <- file.path(root_dir, "R")
  if (!dir.exists(r_dir)) {
    stop(sprintf("R/ directory not found under %s", root_dir), call. = FALSE)
  }
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(r_files, source, local = FALSE))
  invisible(root_dir)
}

resolve_project_root <- resolveProjectRoot
load_project_sources <- loadProjectSources

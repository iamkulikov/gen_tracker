library(testthat)

if (file.exists("R/project_root.R")) {
  source("R/project_root.R")
  root_dir <- resolveProjectRoot()
} else if (file.exists("../R/project_root.R")) {
  source("../R/project_root.R")
  root_dir <- resolveProjectRoot()
} else {
  stop("Cannot locate gen_tracker R/ directory for tests.")
}

loadProjectSources(root_dir)

test_dir(if (dir.exists("tests/testthat")) "tests/testthat" else "testthat")

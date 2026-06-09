# Build data/migration.rds from the WPP2024 GEN/F01 demographic indicators file.
#
# Prerequisites (in data/):
#   - countries.csv
#   - WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx
#
# From project root:
#   Rscript scripts/build_prepared_migration.R
#
# The net migration rate (per 1,000 population) and net number of migrants are
# extracted offline so the app's reliability scoring stays cheap at runtime.

source(file.path("R", "project_root.R"))
setwd(resolveProjectRoot())
loadProjectSources()

buildPreparedMigration()

library("dplyr")
library("reshape2")
library("countrycode")
library("readxl")
library("readr")
library("tidyr")
library("data.table")
library("writexl")
library("stringr")
library("gsubfn")


setwd("C:/Projects/generation_tracker")

#### Import data
fname <- "WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
fullbase <- read_csv(fname)
#summary(fullbase)

#### Downscale
fullbase <- fullbase %>% select(-c(LocID, VarID, Variant, PopTotal, MidPeriod, AgeGrp, AgeGrpSpan))

#### Export tidy data
fname_new <- "Age_quest.csv"
fullbase <- write_csv(fullbase, fname_new)


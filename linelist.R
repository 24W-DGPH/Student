#
#
#    LINELIST DGPH R4EPI EXAMPLE
#
#
# Version 0.1
# 2024-11-29


# Load Packages -------------------------
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  epikit, # age_categories() function
  tidyverse, # data management and visualization
  #
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  #
  skimr, # preview tibbles (aka data frames)
  todor # add TODO comments to your project
)


# Import Data ---------------------------
# TODO: check wrong file on import
linelist_raw <- import("linelist_raw.xlsx")
skimr::skim(linelist_raw)



# Clean Data ----------------------------

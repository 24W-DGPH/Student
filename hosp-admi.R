#
#
#    Hospital Admission
#
#
# Version 0.1
# 2025-01-17


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
  todor, # add TODO comments to your project)
  
  
  # Working with Dates
  lubridate, # general package for handling and converting dates
  parsedate, # has function to "guess" messy dates
  aweek, # another option for converting dates to weeks, and weeks to dates
  zoo # additional date/time functions)
)

# Import Data ---------------------------
# TODO: check wrong file on import
hosp_admi_raw <- import("hosp_admi_raw.xlsx", setclass = "tibble")

# Clean Data 1 ----------------------------
hosp_admi <- hosp_admi_raw %>%
  
  janitor::clean_names()

colnames(hosp_admi) <- as.character(hosp_admi_raw[9, ]) # Set the ninth row as column names

hosp_admi <- hosp_admi[-(1:10), ] # Remove the first 10 rows 

colnames(hosp_admi)[2] <- colnames(hosp_admi)[1] # Make the column 2 header equal to the 1

hosp_admi <- hosp_admi[-1, -1] # Remove the first column and the first row

colnames(hosp_admi)[1] <- "Primary Diagnosis" # Change column 1 to 'Primary Diagnosis'

hosp_admi <- hosp_admi[grepl("neoplasm(s)?", hosp_admi$'Primary Diagnosis', ignore.case = TRUE),]
# Filter rows where "Primary Diagnosis" contains "neoplasm" or "neoplasms"


# Clean Data 2 ----------------------------
hosp_admi <- hosp_admi %>%
  
  janitor::clean_names()

# Convert the columns to numeric and sum them
hosp_admi <- hosp_admi %>%
  mutate(across(16:39, ~ as.numeric(as.character(.)))) %>% # Ensure numeric for columns 16 to 39
  mutate(
    Age_0_9 = rowSums(across(16:18), na.rm = TRUE),
    Age_10_19 = rowSums(across(19:24), na.rm = TRUE),
    Age_20_29 = rowSums(across(25:26), na.rm = TRUE),
    Age_30_39 = rowSums(across(27:28), na.rm = TRUE),
    Age_40_49 = rowSums(across(29:30), na.rm = TRUE),
    Age_50_59 = rowSums(across(31:32), na.rm = TRUE),
    Age_60_69 = rowSums(across(33:34), na.rm = TRUE),
    Age_70_79 = rowSums(across(35:36), na.rm = TRUE),
    Age_80_90 = rowSums(across(37:39), na.rm = TRUE)
  ) %>%
  select(-c(16:39)) # Remove original columns

hosp_admi <- hosp_admi[, !grepl("media|mean", colnames(hosp_admi), ignore.case = TRUE)]
# Remove columns containing "media" or "mean" in their names
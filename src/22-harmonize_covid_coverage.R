# Harmonize C19 cases and deaths

# Init ------------------------------------------------------------

library(yaml)
library(tidyr); library(stringr); library(dplyr); library(readr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  covid_coverage = './dat/coverage.rds'
)
paths$output <- list(
  covid_coverage_harmonized = './tmp/22-covid_coverage_harmonized.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# list containers for analysis artifacts
dat <- list()

# Harmonize death -------------------------------------------------

dat$covid_coverage_raw <- read_rds(paths$input$covid_coverage)

dat$covid_coverage_harmonized <-
  dat$covid_coverage_raw %>%
  select(Code, Date, Sex, Age, CasesFix, DeathsFix) %>%
  mutate(
    Age = ifelse(Age >= 85, 85, Age),
    year = lubridate::year(Date)
  ) %>%
  group_by(Code, year, Age, Sex) %>%
  summarise(c19cases_coverage = sum(CasesFix), c19deaths_coverage = sum(DeathsFix)) %>%
  ungroup() %>%
  mutate(
    Sex = case_when(Sex == 'b' ~ 'Total', Sex == 'm' ~ 'Male',
                    Sex == 'f' ~ 'Female'),
    id = GenerateRowID(region_iso = Code, sex = Sex, year = year,
                       age_start = Age)
  ) %>%
  select(id, c19cases_coverage, c19deaths_coverage)

# Export ----------------------------------------------------------

saveRDS(dat$covid_coverage_harmonized, file = paths$output$covid_coverage_harmonized)

# Harmonize data on death counts

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
  death_wpp = './dat/11-death_wpp.rds'
)
paths$output <- list(
  death_wpp_harmonized = './tmp/20-death_wpp_harmonized.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# list containers for analysis artifacts
dat <- list()

# Harmonize death -------------------------------------------------

dat$death_raw <- read_rds(paths$input$death_wpp)

dat$death_wpp_harmonized <-
  dat$death_raw %>%
  filter(variant_name == 'Medium') %>%
  select(region_code_iso2, year, age_start, age_width, death_male, death_female, death_total) %>%
  pivot_longer(cols = c(death_male, death_female, death_total), names_to = 'sex', values_to = 'death_observed') %>%
  mutate(
    agegroup = age_start%/%5*5,
    agegroup = ifelse(agegroup >= 85, 85, agegroup)
  ) %>%
  group_by(region_code_iso2, year, sex, agegroup) %>%
  summarise(death_observed = sum(death_observed)) %>%
  ungroup() %>%
  mutate(
    death_actual_wpp = death_observed*1e3,
    sex = case_when(
      sex == 'death_male' ~ 'Male',
      sex == 'death_female' ~ 'Female',
      sex == 'death_total' ~ 'Total'
    ),
    id = GenerateRowID(region_iso = region_code_iso2, sex = sex, year = year, age_start = agegroup)
  ) %>%
  select(id, death_actual_wpp)

# Export ----------------------------------------------------------

saveRDS(dat$death_wpp_harmonized, file = paths$output$death_wpp_harmonized)

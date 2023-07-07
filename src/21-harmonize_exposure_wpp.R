# Harmonize data on person-year exposure

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
  exposure_wpp = './dat/12-exposure_wpp.rds'
)
paths$output <- list(
  exposure_wpp_harmonized = './tmp/21-exposure_wpp_harmonized.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# list containers for analysis artifacts
dat <- list()

# Harmonize death -------------------------------------------------

dat$exposure_raw <- read_rds(paths$input$exposure_wpp)

dat$exposure_wpp_harmonized <-
  dat$exposure_raw %>%
  filter(variant_name == 'Medium') %>%
  select(region_code_iso2, year, age_start, age_width, exposure_male, exposure_female, exposure_total) %>%
  pivot_longer(cols = c(exposure_male, exposure_female, exposure_total), names_to = 'sex', values_to = 'exposure_observed') %>%
  mutate(
    agegroup = age_start%/%5*5,
    agegroup = ifelse(agegroup >= 85, 85, agegroup)
  ) %>%
  group_by(region_code_iso2, year, sex, agegroup) %>%
  summarise(exposure_observed = sum(exposure_observed)) %>%
  ungroup() %>%
  mutate(
    exposure_actual_wpp = exposure_observed*1e3,
    sex = case_when(
      sex == 'exposure_male' ~ 'Male',
      sex == 'exposure_female' ~ 'Female',
      sex == 'exposure_total' ~ 'Total'
    ),
    id = GenerateRowID(region_iso = region_code_iso2, sex = sex, year = year, age_start = agegroup)
  ) %>%
  select(id, exposure_actual_wpp)

# Export ----------------------------------------------------------

saveRDS(dat$exposure_wpp_harmonized, file = paths$output$exposure_wpp_harmonized)

# Download WPP data on death counts
#
# Download annual death count data by age and sex from WPP.

# Init ------------------------------------------------------------

library(yaml)
library(httr)
library(purrr); library(dplyr); library(readr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  url_wpp_death = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.zip'
)
paths$output <- list(
  wpp_death = './dat/11-death_wpp.rds',
  wpp_death_zip = './tmp/11-death_wpp.zip'
)

# global configuration
config <- read_yaml(paths$input$config)

# list containers for analysis artifacts
dat <- list()

# Download WPP deaths ---------------------------------------------

dat$wpp_death_zip <-
  GET(url = paths$input$url_wpp_death, progress())

# Preliminary format WPP death ------------------------------------

# save downloaded zip to file
writeBin(
  object = content(dat$wpp_death_zip, 'raw'),
  con = paths$output$wpp_death_zip
)

# list all files in archive
dat$wpp_death_filenames <- unzip(
  paths$output$wpp_death_zip,
  list = TRUE
)[['Name']]

# unzip data
dat$wpp_death <-
    unz(paths$output$wpp_death_zip, filename = dat$wpp_death_filenames) %>%
      read_csv(
        col_names = c('order', 'region_code_who', 'notes', 'region_code_iso3', 'region_code_iso2',
                      'region_code_sdmx', 'region_type_id', 'region_type_name',
                      'region_parent_id', 'region_name', 'variant_id',
                      'variant_name', 'year', 'midyear', 'agegroup', 'age_start', 'age_width',
                      'death_male', 'death_female', 'death_total'),
        col_types = 'iiccciicicicinciinnn',
        skip = 1,
        na = ''
      )

# Export ----------------------------------------------------------

saveRDS(dat$wpp_death, file = paths$output$wpp_death)

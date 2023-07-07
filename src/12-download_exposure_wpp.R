# Download data on population exposures
#
# (1) Download annual exposure data from WPP

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
  url_wpp_exposure = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationExposureBySingleAgeSex_Medium_1950-2021.zip'
)
paths$output <- list(
  wpp_exposure = './dat/12-exposure_wpp.rds',
  wpp_exposure_zip = './tmp/12-exposure_wpp.zip'
)

# global configuration
config <- read_yaml(paths$input$config)

# list containers for analysis artifacts
dat <- list()

# Download WPP exposure -------------------------------------------

dat$wpp_exposure_zip <-
  GET(url = paths$input$url_wpp_exposure, progress())

# Preliminary format WPP exposure ---------------------------------

# save downloaded zip to file
writeBin(
  object = content(dat$wpp_exposure_zip, 'raw'),
  con = paths$output$wpp_exposure_zip
)

# list all files in archive
dat$wpp_exposure_filenames <- unzip(
  paths$output$wpp_exposure_zip,
  list = TRUE
)[['Name']]

# unzip data
dat$wpp_exposure <-
  unz(paths$output$wpp_exposure_zip, filename = dat$wpp_exposure_filenames) %>%
  read_csv(
    col_names = c('order', 'region_code_who', 'notes', 'region_code_iso3', 'region_code_iso2',
                  'region_code_sdmx', 'region_type_id', 'region_type_name',
                  'region_parent_id', 'region_name', 'variant_id',
                  'variant_name', 'year', 'midyear', 'agegroup', 'age_start', 'age_width',
                  'exposure_male', 'exposure_female', 'exposure_total'),
    col_types = 'iiccciicicicinciinnn',
    skip = 1,
    na = ''
  )

# Export ----------------------------------------------------------

saveRDS(dat$wpp_exposure, file = paths$output$wpp_exposure)

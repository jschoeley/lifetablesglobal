# Assemble data basis for pop projection

# Init ------------------------------------------------------------

library(dplyr); library(readr); library(openxlsx)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  skeleton = './tmp/10-skeleton.rds',
  global = './src/00-global.R',
  exposure_wpp = './tmp/21-exposure_wpp_harmonized.rds',
  death_wpp = './tmp/20-death_wpp_harmonized.rds',
  covid_coverage = './tmp/22-covid_coverage_harmonized.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  out_rds = './out/30-modelinput.rds',
  out_csv = './out/30-modelinput.csv',
  out_xlsx = './out/30-modelinput.xlsx'
)

# list containers for analysis artifacts
dat <- list()

# Functions -------------------------------------------------------

source(paths$input$global)

# Data ------------------------------------------------------------

dat$skeleton <- readRDS(paths$input$skeleton)
dat$exposure_wpp <- readRDS(paths$input$exposure)
dat$death_wpp <- readRDS(paths$input$death)
dat$covid_coverage <- readRDS(paths$input$covid)

# Join ------------------------------------------------------------

dat$modelinput <-
  dat$skeleton %>%
  left_join(dat$death_wpp, by = 'id') %>%
  left_join(dat$exposure_wpp, by = 'id') %>%
  left_join(dat$covid_coverage, by = 'id')

# Export ----------------------------------------------------------

saveRDS(dat$modelinput, file = paths$output$out_rds)

write_csv(dat$modelinput, file = paths$output$out_csv)

write.xlsx(dat$modelinput, file = paths$output$out_xlsx,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)

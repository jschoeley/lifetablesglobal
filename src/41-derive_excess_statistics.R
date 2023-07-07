# Derive life table statistics

# Init ------------------------------------------------------------

library(yaml)
library(tidyverse)
library(openxlsx)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  forecasts = './tmp/40-forecast.rds',
  modelinput = './out/30-modelinput.rds'
)
paths$output <- list(
  lifetablesglobal_rds = './out/41-lifetablesglobal.rds',
  lifetablesglobal_csv = './out/41-lifetablesglobal.csv',
  lifetablesglobal_xlsx = './out/41-lifetablesglobal.xlsx'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    country_code_iso2 %in% config$skeleton$regions
  )
})

# Function --------------------------------------------------------

# return a vector of mean and quantiles
QuantileWithMean <- function (x, prob = cnst$quantiles) {
  x <- x[!(is.na(x)|is.nan(x)|is.infinite(x))]
  q <- quantile(x, prob = prob, names = FALSE, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  result <- c(m, q)
  names(result) <- c('mean', paste0('q', prob))
  return(result)
}

# Input -----------------------------------------------------------

forecast <- readRDS(paths$input$forecasts)
modelinput <- readRDS(paths$input$modelinput)

# mx actual wpp ---------------------------------------------------

mx_actual_wpp <- list()

mx_actual_wpp$ci <- apply(
  forecast[,,,,'nmx','actual',],
  c('region', 'sex', 'year', 'age'),
  QuantileWithMean,
  c(0.05, 0.95),
  simplify = TRUE
)
names(dimnames(mx_actual_wpp$ci))[1] <- 'quantile'

mx_actual_wpp$df <-
  as.data.frame.table(mx_actual_wpp$ci, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = quantile, values_from = Freq) %>%
  transmute(
    id = GenerateRowID(region, sex, as.integer(year), as.integer(age)),
    mx_actual_wpp_mean = mean
  )

# mx expected wpp -------------------------------------------------

mx_expected_wpp <- list()

mx_expected_wpp$ci <- apply(
  forecast[,,,,'nmx','expected',],
  c('region', 'sex', 'year', 'age'),
  QuantileWithMean,
  c(0.05, 0.95),
  simplify = TRUE
)
names(dimnames(mx_expected_wpp$ci))[1] <- 'quantile'

mx_expected_wpp$df <-
  as.data.frame.table(mx_expected_wpp$ci, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = quantile, values_from = Freq) %>%
  transmute(
    id = GenerateRowID(region, sex, as.integer(year), as.integer(age)),
    mx_expected_wpp_mean = ifelse(is.nan(mean), NA, mean),
    mx_expected_wpp_q05 = q0.05,
    mx_expected_wpp_q95 = q0.95
  )


# ex actual wpp ---------------------------------------------------

ex_actual_wpp <- list()

ex_actual_wpp$ci <- apply(
  forecast[,,,,'ex','actual',],
  c('region', 'sex', 'year', 'age'),
  QuantileWithMean,
  c(0.05, 0.95),
  simplify = TRUE
)
names(dimnames(ex_actual_wpp$ci))[1] <- 'quantile'

ex_actual_wpp$df <-
  as.data.frame.table(ex_actual_wpp$ci, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = quantile, values_from = Freq) %>%
  transmute(
    id = GenerateRowID(region, sex, as.integer(year), as.integer(age)),
    ex_actual_wpp = mean
  )

# ex expected wpp -------------------------------------------------

ex_expected_wpp <- list()

ex_expected_wpp$ci <- apply(
  forecast[,,,,'ex','expected',],
  c('region', 'sex', 'year', 'age'),
  QuantileWithMean,
  c(0.05, 0.95),
  simplify = TRUE
)
names(dimnames(ex_expected_wpp$ci))[1] <- 'quantile'

ex_expected_wpp$df <-
  as.data.frame.table(ex_expected_wpp$ci, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = quantile, values_from = Freq) %>%
  transmute(
    id = GenerateRowID(region, sex, as.integer(year), as.integer(age)),
    ex_expected_wpp_mean = ifelse(is.nan(mean), NA, mean),
    ex_expected_wpp_q05 = q0.05,
    ex_expected_wpp_q95 = q0.95
  )

# Join with input data --------------------------------------------

lifetablesglobal <-
  modelinput %>%
  left_join(mx_actual_wpp$df, by = 'id') %>%
  left_join(mx_expected_wpp$df, by = 'id') %>%
  left_join(ex_actual_wpp$df, by = 'id') %>%
  left_join(ex_expected_wpp$df, by = 'id')

# Export ----------------------------------------------------------

saveRDS(lifeexpectancyglobal, file = paths$output$lifetablesglobal_rds)

write_csv(lifeexpectancyglobal, file = paths$output$lifetablesglobal_csv)

write.xlsx(lifeexpectancyglobal, file = paths$output$lifetablesglobal_xlsx,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)

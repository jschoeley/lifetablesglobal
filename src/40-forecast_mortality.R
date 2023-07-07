# Forecast mortality by region, age, and sex

# Init ------------------------------------------------------------

#remotes::install_github('coolbutuseless/ggpattern')
library(yaml)
library(tidyverse)
library(StMoMo)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  region = './cfg/who_regions.csv',
  projectioninput = './out/30-modelinput.rds'
)
paths$output <- list(
  forecast = './out/40-forecast.rds',
  log = './tmp/40-log.txt'
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
  lee_carter_fitting_period = 1990:2019
  jumpoff_year = max(lee_carter_fitting_period)
  forecast_horizon = 2
  forecast_years = jumpoff_year+(1:forecast_horizon)
  nsim = 50
  seed = 1987
  age_group_width = 5
})

# Functions -------------------------------------------------------

# this function returns TRUE wherever elements are the same,
# including NA's, and FALSE everywhere else
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# return a vector of mean and quantiles
QuantileWithMean <- function (x, prob = cnst$quantiles) {
  x <- x[!(is.na(x)|is.nan(x)|is.infinite(x))]
  q <- quantile(x, prob = prob, names = FALSE, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  result <- c(m, q)
  names(result) <- c('mean', paste0('q', prob))
  return(result)
}

# array structure for population data
MakePopArray <- function (
    x, age, year, sex, region, measure, scenario, nsim
) {
  X <- array(
    x,
    dim = c(
      length(age), length(year), length(sex),
      length(region), length(measure), length(scenario), length(1:nsim)
    ),
    dimnames = list(
      age = age, year = year, sex = sex,
      region = region, measure = measure, scenario = scenario, nsim = 1:nsim
    )
  )
  return(X) 
}

# Lee-Carter Forecast
ForecastLeeCarter <- function (Dxt, Ext, ages, years, h, nsim, seed) {

  constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    c1 <- mean(kt[1, ], na.rm = TRUE)
    c2 <- sum(bx[, 1], na.rm = TRUE)
    list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
  }
  LC_def <- StMoMo(link = 'log', staticAgeFun = TRUE,
                   periodAgeFun = 'NP', constFun = constLC)
  LC_fit <- fit(LC_def, Dxt = Dxt, Ext = Ext, ages = ages, years = years)
  LC_sim <-
    simulate(LC_fit, nsim = nsim, h = h,
             jumpchoice = 'actual',
             seed = seed,
             kt.method = 'mrwd'#, kt.order = c(1,1,2)
             )[['rates']]
  
  return(list(
    sim = LC_sim,
    deviance = LC_fit[['deviance']],
    fail = LC_fit[['fail']],
    converged = LC_fit[['conv']]
  ))
}

# Load population data --------------------------------------------

projectioninput <- readRDS(paths$input$projectioninput)

# Result array ----------------------------------------------------

strata <-
  list(
    age = as.character(seq(config$skeleton$age$start, config$skeleton$age$end, 5)),
    year = as.character(config$skeleton$year$start:config$skeleton$year$end),
    sex = as.character(unlist(config$skeleton$sex)),
    region = as.character(config$skeleton$regions),
    measure = c(
      'nDx', 'nEx', 'nmx', 'lx', 'ex'),
    scenario = c('actual', 'expected'),
    nsim = as.character(1:cnst$nsim)
  )

# specify aggregates
aggregates <-
  list(
    age = c('0-30', '30-60', '60+', 'Total'),
    year = c('20/21'),
    sex = 'Total',
    region = 'World'
  )

# initialize result array
forecast <- MakePopArray(
  NA,
  age = c(strata$age, aggregates$age),
  year = c(strata$year, aggregates$year),
  sex = c(strata$sex, aggregates$sex),
  region = c(strata$region, aggregates$region),
  measure = strata$measure,
  scenario = strata$scenario,
  nsim = cnst$nsim
)

# add actual deaths and exposures
forecast[
  strata$age,strata$year,strata$sex,strata$region,'nDx','actual',
] <- projectioninput$death_actual_wpp
forecast[
  strata$age,strata$year,strata$sex,strata$region,'nEx','actual',
] <- projectioninput$exposure_actual_wpp

# Forecast mortality ----------------------------------------------

iteration_strata <- expand_grid(
  region = config$skeleton$regions,
  sex = unlist(config$skeleton$sex)
) %>%
  mutate(
    convergence = NA,
    deviance = NA
  )

# stochastic forecast of mortality rates 2020-21
n <- 1
for (i in 1:nrow(iteration_strata)) {
  
  region <- iteration_strata[i,][['region']]
  sex <- iteration_strata[i,][['sex']]
  
  cat('Forecast:', region, sex, n, 'of', nrow(iteration_strata), '\n')
  
  n <- n+1
  
  suppressMessages({
    
    # subset to single population by period and age
    leecarterinput <-
      forecast[strata$age,
               as.character(cnst$lee_carter_fitting_period),
               sex,region,,'actual',1]
    Dxt <- leecarterinput[,,'nDx']
    Ext <- leecarterinput[,,'nEx']
    # if NAs in input, skip forecast
    if (anyNA(c(Dxt, Ext))) { next }
    
    skip_to_next <- FALSE
    tryCatch({
      fit <- ForecastLeeCarter(
        Dxt, Ext, as.integer(strata$age),
        as.integer(cnst$lee_carter_fitting_period),
        h = cnst$forecast_horizon,
        nsim = cnst$nsim, seed = cnst$seed
      )
      # write to result matrix
      forecast[strata$age,
               as.character(cnst$forecast_years),
               sex,region,'nmx','expected',] <- fit[['sim']]
      iteration_strata[i,][['convergence']] <- fit[['converged']]
      iteration_strata[i,][['deviance']] <- fit[['deviance']]
    },
    error = function(e) { skip_to_next <<- TRUE }
    )
    if(skip_to_next) { next }     
    
  })
}

rm(projectioninput)
saveRDS(forecast, './tmp/40-forecast.rds')
saveRDS(iteration_strata, './tmp/40-forecast_log.rds')

# Expected deaths -------------------------------------------------

# set divergent mortality predictions NA
forecast[,,,,'nmx','expected',][
  forecast[,,,,'nmx','expected',]>10] <- NA

# derive expected deaths
forecast[,,,,'nDx','expected',] <-
  forecast[,,,,'nEx','actual',]*forecast[,,,,'nmx','expected',]
# this allows to get back the expected mortality rates
# by going nDx/nEx
forecast[,,,,'nEx','expected',] <- forecast[,,,,'nEx','actual',]

# Add aggregates --------------------------------------------------

# sex
forecast[,,'Total',,c('nDx','nEx'),,] <-
  forecast[,,'Female',,c('nDx','nEx'),,] +
  forecast[,,'Male',,c('nDx','nEx'),,]

# year
forecast[,'20/21',,,c('nDx','nEx'),,] <-
  forecast[,'2020',,,c('nDx','nEx'),,] +
  forecast[,'2021',,,c('nDx','nEx'),,]

# region
forecast[,,,'World',c('nDx','nEx'),,] <-
  apply(
    forecast[,,,strata$region,c('nDx','nEx'),,],
    c('age', 'year', 'sex', 'measure', 'scenario', 'nsim'), # sum over region
    sum, na.rm = TRUE
  )

# Calculate life expectancy ---------------------------------------

{
  forecast[strata$age,,,,'nmx',,] <-
    forecast[strata$age,,,,'nDx',,]/
    forecast[strata$age,,,,'nEx',,]
  #px <- exp(-mortality)
  forecast[strata$age,,,,'lx',,] <- apply(
    forecast[strata$age,,,,'nmx',,],
    c('year', 'sex', 'region', 'scenario', 'nsim'),
    function (nmx) c(1, exp(cumsum(head(-nmx*cnst$age_group_width,-1))))
  )
  ndx <- apply(
    forecast[strata$age,,,,'lx',,],
    # apply function to vector of data by age
    c('year', 'sex', 'region', 'scenario', 'nsim'),
    function (lx) c(-diff(lx), tail(lx, 1))
  )
  nLx <- ndx/forecast[strata$age,,,,'nmx',,]
  I <- compareNA(forecast[strata$age,,,,'nmx',,],0)
  nLx[I] <- forecast[strata$age,,,,'lx',,][I]
  Tx <- apply(
    nLx,
    # apply function to vector of data by age
    c('year', 'sex', 'region', 'scenario', 'nsim'),
    function (nLx) rev(cumsum(rev(nLx)))
  )
  # ex = Tx/lx
  forecast[strata$age,,,,'ex',,] <-
    Tx/forecast[strata$age,,,,'lx',,]
  rm(nLx, I, Tx, ndx)
}

saveRDS(forecast, './tmp/40-forecast.rds')
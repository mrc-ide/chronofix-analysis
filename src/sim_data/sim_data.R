# Simulate data for each simulation scenario

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

library(orderly)
library(chronofix)

## Number of data sets to simulate for each scenario
pars <- orderly_parameters(scenario = "baseline",
                           nsims = NULL)
orderly_dependency("sim_params", "latest", 
                   files = c("date_params.rds",
                             "error_params.rds",
                             "scenarios.rds"))

orderly_resource("support.R")
source("support.R")

orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.4")

dir.create("outputs")

# Load all simulation parameters
date_params <- readRDS("date_params.rds")
error_params <- readRDS("error_params.rds")
scenarios <- readRDS("scenarios.rds")

set.seed(1)

date_params <- date_params[[scenarios[[scenario]]$date_model]]
error_params <- error_params[[scenarios[[scenario]]$error_model]]
  
## simulate true data
true_data <- simulate_true_data(date_params, nsims = pars$nsims)
  
## simulate errors
for (i in seq_len(pars$nsims)) {
  filename <- paste0("outputs/sim_data_", i, ".rds")
  orderly_artefact(description = "Simulated Data", 
                   files = filename)
  
  res <- chronofix_simulate_observation_errors(
    true_data[[i]], error_params, date_params$date_range
  )
  saveRDS(res, filename)
}

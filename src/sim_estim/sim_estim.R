library(orderly)
library(chronofix)

orderly_resource("support.R")
orderly_shared_resource("util.R")
source("support.R")
source("util.R")

version_check("chronofix", "0.0.5")

pars <- orderly_parameters(scenario = "baseline",
                           dataset = 1)

scenario <- pars$scenario
dataset <- pars$dataset

n_steps <- 10000
burnin <- 5000
thinning_factor <- 20

orderly_dependency("sim_params", "latest", 
                   c("date_params.rds",
                     "scenarios.rds"))
data_filename <- paste0("outputs/sim_data_", scenario, "_", dataset, ".rds")
orderly_dependency("sim_data", "latest", 
                   c("sim_data.rds" = data_filename))

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = c("sim_estim.rds",
                           "pars_summary.rds"))





# Read in dependencies --------------------------------------------------------

date_params <- readRDS("date_params.rds")
scenarios <- readRDS("scenarios.rds")
sim_data <- readRDS("sim_data.rds")

# MCMC settings ---------------------------------------------------------------

control <- chronofix_mcmc_control(n_steps = n_steps,
                                  burnin = burnin,
                                  thinning_factor = thinning_factor,
                                  n_chains = 4,
                                  parallel = TRUE,
                                  n_workers = 4,
                                  earliest_possible_date = "2014-01-01",
                                  latest_possible_date = "2015-01-01",
                                  cascade_sampling = TRUE,
                                  prob_update_estimated_dates = 0.1,
                                  prob_update_error_indicators = 0.1)
sampler <- chronofix_sampler(control)
hyperparameters <- chronofix_hyperparameters(
  gamma_shape_prior_shape = 1,
  gamma_shape_prior_rate = 0.1,
  gamma_mean_prior_shape = 2,
  gamma_mean_prior_scale = 10
)

# Run MCMC -------------------------------------------------------------------

date_model <- scenarios[[scenario]]$date_model
delay_info <- date_params[[date_model]]$delay_info

model <- chronofix_model(sim_data$observed_data, delay_info,
                         hyperparameters, control)
samples <- chronofix_mcmc_run(model, sampler, control = control)

pars_summary <- summarise_pars(samples, delay_info)

saveRDS(samples, "sim_estim.rds")

saveRDS(pars_summary, "pars_summary.rds")


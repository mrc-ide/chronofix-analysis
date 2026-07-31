library(orderly)
library(chronofix)
library(monty)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(posterior)
library(bayesplot)

pars <- orderly_parameters(scenario = "baseline",
                           dataset = 1)

orderly_resource("support.R")
orderly_resource("plot.R")
orderly_shared_resource("util.R")
source("support.R")
source("plot.R")
source("util.R")

version_check("chronofix", "0.0.6")

scenario <- pars$scenario
dataset <- pars$dataset

n_steps <- 10000
burnin <- 5000
thinning_factor <- 20

orderly_dependency("sim_params", "latest", 
                   c("date_params.rds",
                     "error_params.rds",
                     "scenarios.rds"))
data_filename <- paste0("outputs/sim_data_", scenario, "_", dataset, ".rds")
orderly_dependency("sim_data", "latest", 
                   c("sim_data.rds" = data_filename))

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = c("sim_estim.rds",
                           "pars_summary.rds",
                           "errors_summary.rds",
                           "figures/rankplots.pdf",
                           "figures/traceplots.pdf"))


# Read in dependencies --------------------------------------------------------

date_params <- readRDS("date_params.rds")
error_params <- readRDS("error_params.rds")
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
error_model <- scenarios[[scenario]]$error_model
delay_info <- date_params[[date_model]]$delay_info

model <- chronofix_model(sim_data$observed_data, delay_info,
                         hyperparameters, control)
samples <- chronofix_mcmc_run(model, sampler, control = control)
saveRDS(samples, "sim_estim.rds")


# Summarise results and produce diagnostic plots-------------------------------

pars_summary <- summarise_pars(samples, delay_info, 
                               error_params[[error_model]]$prob_error)

errors_summary <- summarise_errors(samples, sim_data)
saveRDS(errors_summary, "errors_summary.rds")

dir.create("figures", showWarnings = FALSE)

ggsave("figures/traceplots.pdf", traceplots(samples, burnin, pars_summary),
       width = 20, height = 12)

ggsave("figures/rankplots.pdf", rankplots(samples, burnin, pars_summary),
       width = 20, height = 12)

pars_summary <- pars_summary %>% select(!variable)
saveRDS(pars_summary, "pars_summary.rds")

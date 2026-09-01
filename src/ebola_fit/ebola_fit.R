library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(monty)
library(chronofix)

orderly::orderly_resource("support.R")
source("support.R")
orderly::orderly_resource("plot.R")
source("plot.R")
orderly::orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.8")

orderly::orderly_artefact(description = "MCMC outputs", 
                          files = "samples.rds")

n_steps <- 6000
burnin <- 1000
thinning_factor <- 20

raw_data <- read.csv("rstb20160308supp1.csv")
data <- prepare_data(raw_data)

delay_map <- delay_info <- data.frame(
  from = c("onset", "onset", "onset", "onset", "onset", "onset",
           "hospitalisation", "onset", "hospitalisation"),
  to = c("report", "report", "report", "report", "death", "hospitalisation",
         "discharge", "hospitalisation", "death"),
  group = c("community-alive", "community-dead", "hospitalised-alive",
            "hospitalised-dead", "community-dead", "hospitalised-alive",
            "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
  distribution = "gamma"
)

# MCMC settings ---------------------------------------------------------------

control <- chronofix_mcmc_control(n_steps = n_steps,
                                  burnin = burnin,
                                  thinning_factor = thinning_factor,
                                  n_chains = 4,
                                  parallel = TRUE,
                                  n_workers = 4,
                                  date_buffer = 15,
                                  cascade_sampling = TRUE,
                                  prob_update_estimated_dates = 1,
                                  prob_update_error_indicators = 1)
sampler <- chronofix_sampler(control)
hyperparameters <- chronofix_hyperparameters(
  gamma_shape_prior_shape = 1,
  gamma_shape_prior_rate = 0.1,
  gamma_mean_prior_shape = 2,
  gamma_mean_prior_scale = 10
)

# Run MCMC -------------------------------------------------------------------

model <- chronofix_model(data, delay_map, hyperparameters, control)
samples <- chronofix_mcmc_run(model, sampler, control = control)
saveRDS(samples, "samples.rds")

pars_summary <- summarise_pars(samples, delay_map)

dir.create("figures", showWarnings = FALSE)

ggsave("figures/traceplots.pdf", traceplots(samples, burnin, pars_summary),
       width = 20, height = 12)

ggsave("figures/rankplots.pdf", rankplots(samples, burnin, pars_summary),
       width = 20, height = 12)

pars_summary <- pars_summary %>% select(!variable)
saveRDS(pars_summary, "pars_summary.rds")

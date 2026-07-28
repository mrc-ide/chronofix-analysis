library(orderly)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(abind)

pars <- orderly_parameters(scenario = NULL)

nsims <- 100

scenario <- pars$scenario

orderly_dependency("sim_params", "latest", 
                   c("date_params.rds",
                     "error_params.rds",
                     "scenarios.rds"))


for (i in seq_len(nsims)) {
  orderly_dependency("sim_estim",
                     quote(latest(parameter:scenario == this:scenario && 
                                    parameter:dataset == environment:i)),
                     c("inputs/pars_summary/pars_summary_${i}.rds" = "pars_summary.rds"))
}

orderly_artefact(files = c("outputs/pars_summary.rds"),
                 description = "MCMC Summary outputs for a single scenario")

orderly_resource("support.R")
source("support.R")

dir.create("outputs", showWarnings = FALSE)

pars_summary <- load_df("pars_summary")
saveRDS(pars_summary, "outputs/pars_summary.rds")

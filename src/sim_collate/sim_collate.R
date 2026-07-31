library(orderly)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(stringr)
library(abind)

pars <- orderly_parameters(scenario = "baseline")

nsims <- 100

scenario <- pars$scenario

orderly_dependency("sim_params", "latest", 
                   c("date_params.rds",
                     "error_params.rds",
                     "scenarios.rds"))


for (i in seq_len(nsims)) {
  dep_files <- c(
    "pars_summary",
    "errors_summary"
  )
  
  dep_names <- as.character(glue("{dep_files}.rds"))
  local_names <- glue("inputs/{dep_files}/{dep_files}_{i}.rds")
  deps_mapping <- setNames(dep_names, local_names)
  
  orderly_dependency("sim_estim",
                     quote(latest(parameter:scenario == this:scenario && 
                                    parameter:dataset == environment:i)),
                     deps_mapping)
}

orderly_artefact(files = c("outputs/pars_summary.rds",
                           "outputs/errors_summary.rds"),
                 description = "MCMC Summary outputs for a single scenario")

orderly_resource("support.R")
source("support.R")

dir.create("outputs", showWarnings = FALSE)

pars_summary <- load_df(nsims, "pars_summary")
saveRDS(pars_summary, "outputs/pars_summary.rds")

errors_summary <- load_df(nsims, "errors_summary")
saveRDS(errors_summary, "outputs/errors_summary.rds")

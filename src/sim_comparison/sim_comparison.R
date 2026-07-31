library(orderly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(glue)
library(stringr)
library(ggrastr)
library(forcats)

pars <- orderly_parameters(comparison = "sanity")

orderly_resource("support.R")
orderly_resource("plot.R")
source("support.R")
source("plot.R")

scenarios <- comparison_scenarios[[pars$comparison]]

# Loop through scenarios and fetch the individual summaries
for (s in scenarios) {
  remote_files <- c(
    "outputs/pars_summary.rds",
    "outputs/errors_summary.rds"
  )
  
  local_names <- 
    glue("inputs/{str_remove(basename(remote_files), '\\\\.rds')}_{s}.rds")
  deps_mapping <- setNames(remote_files, local_names)
  
  orderly_dependency(
    "sim_collate", 
    "latest(parameter:scenario == environment:s)",
    deps_mapping
  )
}



orderly_artefact(files = c("figures/ess_plot.pdf",
                          "figures/coverage_plot.pdf",
                          "figures/bias_plot.pdf",
                          "figures/posterior_delays.pdf",
                           "figures/posterior_prob_error.pdf",
                           "figures/sensitivity_events.pdf",
                           "figures/sensitivity_individuals.pdf"),
                 description = "Diagnostic figures")

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# Bind all the individual scenarios together -----------------------------------
pars_summary <- combine_df(scenarios, "pars_summary")
errors_summary <- combine_df(scenarios, "errors_summary")


# Plots ----------------------------------------------------------------------

ess_threshold <- 400
rhat_threshold <- 1.05

# Get any problem traces
problem_traces <- pars_summary %>%
  filter(ess_bulk < ess_threshold | rhat > rhat_threshold) %>%
  select(scenario, dataset) %>%
  distinct(scenario, dataset)

n_problem_traces <- nrow(problem_traces)
if (n_problem_traces > 0) {
  for (i in seq_len(n_problem_traces)) {
    scenario <- 
      names(scenario_labels)[scenario_labels == problem_traces$scenario[i]]
    dataset <- as.integer(problem_traces$dataset[i])
    orderly_dependency(
      "sim_estim",
      quote(usedby(latest(name == "sim_collate" && 
                            parameter:scenario == environment:scenario)) &&
              parameter:dataset == environment:dataset),
      c("figures/problem_traces/${scenario}_${dataset}_traceplots.pdf" = 
          "figures/traceplots.pdf",
        "figures/problem_traces/${scenario}_${dataset}_rankplots.pdf" = 
          "figures/rankplots.pdf")
    )
  }
}


# ESS plot
ggsave("figures/ess_plot.pdf", plot_ess(pars_summary, ess_threshold),
       width = 21, height = 14)


# Coverage plot
ggsave("figures/coverage_plot.pdf", plot_coverage(pars_summary),
       width = 21, height = 14)


# Bias plot
ggsave("figures/bias_plot.pdf", plot_bias(pars_summary),
       width = 21, height = 14)


# Posterior delays plot
ggsave("figures/posterior_delays.pdf", plot_posterior_delays(pars_summary),
       width = 21, height = 14)

# Posterior probability of error plot
ggsave("figures/posterior_prob_error.pdf",
       plot_posterior_prob_error(pars_summary),
       width = 14, height = 4)


# Event level sensitivity
ggsave("figures/sensitivity_events.pdf",
       plot_event_sensitivity(errors_summary), width = 14, height = 6)


# Individual level sensitivity
ggsave("figures/sensitivity_individuals.pdf",
       plot_indiv_sensitivity(errors_summary), 
       width = 14, height = 8)

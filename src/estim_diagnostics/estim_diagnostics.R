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

pars <- orderly_parameters(scenarios = NULL)

n_steps <- 10000
burnin <- 5000
scenarios <- strsplit(scenarios, ",")[[1]]

# Loop through scenarios and fetch the individual summaries
for (s in scenarios) {
  remote_files <- c(
    "results/true_params.rds",
    "results/all_draws.rds",
    "results/sim_summaries.rds",
    "results/agg_summaries.rds",
    "results/observed_patterns.rds",
    "results/obs_pattern_summary.rds",
    "results/indiv_event_status.rds",
    "results/event_confusion.rds",
    "results/indiv_performance_summary.rds",
    "results/convergence_issues_by_individual.rds"
  )
  
  local_names <- glue("{str_remove(basename(remote_files), '\\\\.rds')}_{s}.rds")
  deps_mapping <- setNames(remote_files, local_names)
  
  orderly_dependency(
    "estim_summary", 
    "latest(parameter:scenario == environment:s)",
    deps_mapping
  )
}

orderly_resource("support.R")
source("support.R")

orderly_artefact(files = c("figures/bias_plot_delays_gt.pdf",
                           "figures/bias_plot_error_gt.pdf",
                           "figures/bias_plot_cv_gt.pdf",
                           "figures/coverage_plot.pdf",
                           "figures/posterior_delay_mean.pdf",
                           "figures/posterior_cv.pdf",
                           "figures/posterior_prob_error.pdf",
                           "figures/observed_patterns.pdf",
                           "figures/ess_plot.pdf",
                           "figures/problem_traces",
                           "figures/rhat_vs_ess.pdf",
                           "figures/sensitivity_events.pdf",
                           "figures/sensitivity_individuals.pdf"),
                 description = "Diagnostic figures")

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# Bind all the individual scenarios together -----------------------------------
true_params <- combine_df(scenarios, "true_params")
all_draws <- combine_df(scenarios, "all_draws")
sim_summaries <- combine_df(scenarios, "sim_summaries")
agg_summaries <- combine_df(scenarios, "agg_summaries")
observed_patterns <- combine_df(scenarios, "observed_patterns")
obs_pattern_summary <- combine_df(scenarios, "obs_pattern_summary")
indiv_event_status <- combine_df(scenarios, "indiv_event_status")
event_confusion <- combine_df(scenarios, "event_confusion")
indiv_performance <- combine_df(scenarios, "indiv_performance_summary")
problem_sims <- combine_df(scenarios, "convergence_issues_by_individual")

delay_mapping <- tibble::tribble(
  ~param_idx, ~delay_from,       ~delay_to,         ~group,
  1,          "onset",           "report",          "community-alive",
  2,          "onset",           "report",          "community-dead",
  3,          "onset",           "report",          "hospitalised-alive",
  4,          "onset",           "report",          "hospitalised-dead",
  5,          "onset",           "death",           "community-dead",
  6,          "onset",           "hospitalisation", "hospitalised-alive",
  7,          "hospitalisation", "discharge",       "hospitalised-alive",
  8,          "onset",           "hospitalisation", "hospitalised-dead",
  9,          "hospitalisation", "death",           "hospitalised-dead"
) %>%
  mutate(param_label = paste(delay_from, "to", delay_to),
         group       = as.character(group))

scenario_labels <- c(
  "low_missingness" = "Low missingness (0.05)",
  "very_small_sample" = "Very small groups (n = 10)",
  "small_sample" = "Small groups (n = 20)",
  "moderate_sample" = "Moderate groups (n = 50)",
  "high_error" = "High error (0.2)",
  "short_delays" = "Short delays (0.5x baseline)",
  "low_variability" = "Low variability (0.5x baseline cv)",
  "baseline" = "Baseline", 
  "low_error" = "Low error (0.02)",
  "no_error" = "Missing dates only (0.2)",
  "no_missing" = "Errors only (0.05)",
  "no_error_no_missing" = "No errors or missing dates",
  "very_large_sample" = "Very large groups (n = 500)",
  "long_delays" = "Long delays (2x baseline)",
  "high_variability" = "High variability (2x baseline cv)",
  "lognormal_delays" = "Lognormal delays"
)

param_order <- c("prob_error",
                 paste0("delay_mean", 1:9),
                 paste0("delay_cv", 1:9))


# Plots ----------------------------------------------------------------------

# Event level sensitivity
ggsave("figures/sensitivity_events.pdf",
       plot_event_sensitivity(event_confusion), width = 14, height = 6)

# Individual level sensitivity
ggsave("figures/sensitivity_individuals.pdf",
       plot_indiv_sensitivity, width = 14, height = 8)


# Bias plots
make_bias_plot <- function(data, bias_avg_col, bias_sd_col, title, subtitle) {
  ggplot(data, aes(x = param_label, y = {{bias_avg_col}}, colour = param_label)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    geom_errorbar(aes(ymin = {{bias_avg_col}} - {{bias_sd_col}}, 
                      ymax = {{bias_avg_col}} + {{bias_sd_col}}), 
                  width = 0.3) +
    facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
    labs(title = title,
         subtitle = subtitle,
         y = "Median Bias",
         x = "",
         colour = "Parameter") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}

make_error_bias_plot <- function(data, bias_col, sd_col, title, subtitle) {
  ggplot(data, aes(x = scenario, y = {{bias_col}})) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = {{bias_col}} - {{sd_col}}, 
                      ymax = {{bias_col}} + {{sd_col}}), 
                  width = 0.15, colour = "midnightblue", alpha = 0.5) +
    geom_point(size = 2.5, colour = "midnightblue") +
    labs(title = title, subtitle = subtitle, y = "Median Bias", x = "") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}

ggsave("figures/bias_plot_delays_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(bias_gt_avg,  bias_gt_sd,
                        "Median Bias of Delay Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

ggsave("figures/bias_plot_error_gt.pdf",
       agg_summaries %>% filter(param_label %in% "probability of error") %>%
         make_error_bias_plot(bias_gt_avg,  bias_gt_sd,
                              "Median Bias: Probability of Error",
                              "Compared to Ground Truth"),
       width = 14, height = 4)

ggsave("figures/bias_plot_cv_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(cv_bias_gt_avg,  cv_bias_gt_sd,
                        "Median Bias of CV Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

# Coverage plots
make_coverage_plot <- function(data, cov95_col, cov50_col, subtitle) {
  coverage_data <- data %>%
    select(scenario, group, param_label, n_sims, 
           cov95 = {{cov95_col}}, cov50 = {{cov50_col}}) %>%
    pivot_longer(cols = c(cov95, cov50),
                 names_to = "metric",
                 values_to = "coverage") %>%
    mutate(interval = ifelse(metric == "cov95", "95% CrI", "50% CrI"),
           n_success = round(coverage * n_sims)) %>%
    rowwise() %>%
    mutate(
      binom_ci = list(binom.test(n_success, n_sims, conf.level = 0.95)$conf.int),
      ci_lower = binom_ci[1],
      ci_upper = binom_ci[2]
    ) %>%
    ungroup() %>%
    select(-binom_ci, -n_success, -metric)
  
  ggplot(coverage_data, 
         aes(x = param_label, y = coverage, 
             colour = param_label, shape = interval)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, alpha = 0.6, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    facet_grid(rows = vars(group), cols = vars(scenario)) +
    labs(title = "Coverage of Credible Intervals",
         subtitle = subtitle,
         y = "Coverage Probability",
         x = "",
         shape = "Interval") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10))) +
    guides(colour = "none")
}

ggsave(
  "figures/coverage_plot.pdf",
  agg_summaries %>% filter(!param_label %in% "probability of error") %>%
    make_coverage_plot(
      cov95_gt_pct, cov50_gt_pct,
      "True parameters (ground truth). Error bars: 95% binomial confidence intervals"
    ),
  width = 10, height = 8)

# Posterior density plots
posterior_data <- all_draws %>%
  filter(param_label != "probability of error")

posterior_data_trimmed <- posterior_data %>%
  group_by(param_label, group) %>%
  mutate(lower_bound_delay = quantile(post_mean, 0.01, na.rm = TRUE),
         upper_bound_delay = quantile(post_mean, 0.99, na.rm = TRUE),
         lower_bound_cv = quantile(post_cv, 0.01, na.rm = TRUE),
         upper_bound_cv = quantile(post_cv, 0.99, na.rm = TRUE)) %>%
  filter(post_mean >= lower_bound_delay & post_mean <= upper_bound_delay &
           post_cv >= lower_bound_cv & post_cv <= upper_bound_cv) %>%
  ungroup()

ref_lines <- true_params %>%
  filter(param_idx > 0) %>%
  group_by(param_label, group) %>%
  mutate(mean_shared = n_distinct(true_mean) == 1,
         cv_shared = n_distinct(true_cv) == 1) %>%
  ungroup()

ggsave("figures/posterior_delay_mean.pdf",
       plot_posterior_delay_mean(posterior_data_trimmed, ref_lines),
       width = 14, height = 10)

ggsave("figures/posterior_cv.pdf",
       plot_posterior_cv(posterior_data_trimmed, ref_lines),
       width = 14, height = 10)

ggsave("figures/posterior_prob_error.pdf",
       plot_posterior_prob_error(all_draws, true_params),
       width = 14, height = 4)

# Observed patterns plot
ggsave("figures/observed_patterns.pdf",
       plot_observed_patterns(obs_pattern_summary),
       width = 14, height = 7)

# ESS plot
ggsave("figures/ess_plot.pdf", plot_ess(sim_summaries), width = 14, height = 7)

# Problem traces
plot_problem_traces(problem_sims)

# see if low ess correlates with poor rhat
ggsave("figures/rhat_vs_ess.pdf", plot_rhat_vs_ess(sim_summaries), 
       width = 12, height = 8)

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
true_params <- map_dfr(scenarios, ~readRDS(glue("true_params_{.x}.rds")))
all_draws <- map_dfr(scenarios, ~readRDS(glue("all_draws_{.x}.rds")))
sim_summaries <- map_dfr(scenarios, ~readRDS(glue("sim_summaries_{.x}.rds")))
agg_summaries <- map_dfr(scenarios, ~readRDS(glue("agg_summaries_{.x}.rds")))
observed_patterns <- map_dfr(scenarios, ~readRDS(glue("observed_patterns_{.x}.rds")))
obs_pattern_summary <- map_dfr(scenarios, ~readRDS(glue("obs_pattern_summary_{.x}.rds")))
indiv_event_status <- map_dfr(scenarios, ~readRDS(glue("indiv_event_status_{.x}.rds")))
event_confusion <- map_dfr(scenarios, ~readRDS(glue("event_confusion_{.x}.rds")))
indiv_performance <- map_dfr(scenarios, ~readRDS(glue("indiv_performance_summary_{.x}.rds")))
problem_sims <- map_dfr(scenarios, ~readRDS(glue("convergence_issues_by_individual_{.x}.rds")))

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

scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenarios)]
scenario_labels <- factor(scenario_labels, levels = scenario_labels)

# Re-apply correct factor orderings after the bind_rows ----------------------
apply_factor_levels <- function(df) {
  if("scenario" %in% names(df)) {
    df$scenario <- factor(df$scenario, levels = unname(scenario_labels))
  }
  if("group" %in% names(df)) {
    df$group <- factor(df$group, levels = c("community-alive", "community-dead", 
                                            "hospitalised-alive", "hospitalised-dead"))
  }
  if("param_label" %in% names(df)) {
    df$param_label <- factor(df$param_label, levels = c(
      "probability of error", "onset to report", "onset to death", 
      "onset to hospitalisation", "hospitalisation to discharge", "hospitalisation to death"
    ))
  }
  df
}

true_params <- apply_factor_levels(true_params)
all_draws <- apply_factor_levels(all_draws)
sim_summaries <- apply_factor_levels(sim_summaries)
agg_summaries <- apply_factor_levels(agg_summaries)
observed_patterns <- apply_factor_levels(observed_patterns)
obs_pattern_summary <- apply_factor_levels(obs_pattern_summary)
indiv_event_status <- apply_factor_levels(indiv_event_status)
event_confusion <- apply_factor_levels(event_confusion)
indiv_performance <- apply_factor_levels(indiv_performance)
problem_sims <- apply_factor_levels(problem_sims)

param_order <- c("prob_error",
                 paste0("delay_mean", 1:9),
                 paste0("delay_cv", 1:9))


# Plots ----------------------------------------------------------------------

# Event level sensitivity
plot_event_sensitivity <- event_confusion %>%
  filter(metric_type == "Sensitivity") %>%
  mutate(event = factor(event, levels = c("onset", "report",
                                          "hospitalisation",
                                          "discharge", "death"))) %>%
  ggplot(aes(x = event, y = group, fill = pct_accuracy)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f", pct_accuracy)),
            size = 2.8, colour = "grey20") +
  facet_grid(threshold ~ scenario) +
  scale_fill_gradient2(midpoint = 50, low = "firebrick",
                       mid = "white", high = "steelblue",
                       limits = c(0, 100), na.value = "grey95") +
  scale_y_discrete(drop = FALSE, limits = rev) +
  labs(title    = "Sensitivity in identifying erroneous dates",
       subtitle = "Percentage of true errors correctly flagged",
       x = "Event", y = "Group", fill = "Sensitivity (%)") +
  theme_bw() +
  theme(strip.text   = element_text(size = 9, face = "bold"),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "darkgrey",
                                    fill = NA, linewidth = 1),
        panel.grid   = element_blank())

ggsave("figures/sensitivity_events.pdf",
       plot_event_sensitivity, width = 14, height = 6)

# Filter out scenarios that have no true errors simulated
plot_data <- indiv_performance %>%
  filter(!scenario %in% c("Missing dates only (0.2)", "No errors or missing dates"))

# Calculate the averages for sensitivity and specificity per threshold and scenario
avg_metrics <- plot_data %>%
  filter(!is.na(accuracy), metric_type %in% c("Sensitivity", "Specificity")) %>%
  group_by(scenario, threshold, metric_type) %>%
  summarise(mean_val = mean(accuracy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric_type, values_from = mean_val) %>%
  mutate(label_text = sprintf("Avg Sens: %.2f\nAvg Spec: %.2f", Sensitivity, Specificity))

# Individual level sensitivity
plot_indiv_sensitivity <- plot_data %>%
  filter(metric_type == "Sensitivity") %>%
  filter(!is.na(accuracy)) %>%
  ggplot(aes(x = accuracy, y = group)) +
  geom_boxplot(fill = "dodgerblue", alpha = 0.5, width = 0.7, outlier.size = 1) +
  geom_text(data = avg_metrics, aes(x = -Inf, y = Inf, label = label_text), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, size = 3,
            fontface = "bold", colour = "navy") +
  facet_grid(threshold ~ scenario) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_discrete(drop = FALSE, limits = rev) +
  labs(title = "Individual-level Sensitivity",
       subtitle = "Distribution across simulations (individuals with >= 2 recorded dates)",
       y = "Group",
       x = "Sensitivity") +
  theme_bw() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
        legend.position = "none")

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

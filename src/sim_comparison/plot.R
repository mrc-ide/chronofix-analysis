plot_ess <- function(pars_summary, ess_threshold) {
  ymax <- max(pars_summary$ess_bulk)
  pars_summary %>%
    ggplot(aes(x = scenario, y = ess_bulk, fill = scenario,
               ymin = 0, ymax = ymax)) +
    geom_violin(alpha = 0.3, scale = "width") +
    geom_jitter(aes(colour = scenario), width = 0.2, alpha = 0.5, size = 1) +
    geom_hline(yintercept = ess_threshold, linetype = "dashed", 
               colour = "black", linewidth = 0.8) +
    facet_wrap(~par_label, scales = "free_y") +
    labs(
      title = "Distribution of effective sample size across simulations",
      subtitle = paste0("Black line = threshold of ", ess_threshold),
      y = "ESS",
      x = "") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


plot_event_sensitivity <- function(errors_summary) {
  errors_summary %>%
    filter(event != "individual") %>%
    group_by(scenario, group, event, threshold) %>%
    mutate(threshold = paste0(as.character(threshold * 100), "% Threshold")) %>%
    summarise(pct_accuracy = 
                sum(n_true_errors_flagged) / sum(n_true_errors) * 100) %>%
    filter(!is.na(pct_accuracy)) %>%
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
}


plot_indiv_sensitivity <- function(errors_summary) {
  
  errors_summary %>%
    filter(event == "individual") %>%
    # Filter out scenarios that have no true errors simulated
    filter(!scenario %in% c("Missing dates only (0.2)", 
                            "No errors or missing dates")) %>%
    mutate(threshold = paste0(as.character(threshold * 100), "% Threshold")) %>%
    mutate(accuracy = n_true_errors_flagged / n_true_errors) %>%
    filter(!is.na(accuracy)) %>%
    ggplot(aes(x = accuracy, y = group)) +
    geom_boxplot(fill = "dodgerblue", alpha = 0.5, 
                 width = 0.7, outlier.size = 1) +
    facet_grid(threshold ~ scenario) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_discrete(drop = FALSE, limits = rev) +
    labs(
      title = "Individual-level Sensitivity",
      subtitle = 
        "Distribution across simulations (individuals with >= 2 recorded dates)",
      y = "Group",
      x = "Sensitivity") +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          panel.border = element_rect(colour = "darkgrey", 
                                      fill = NA, linewidth = 1),
          legend.position = "none")
}


plot_coverage <- function(pars_summary) {
  coverage_data <- pars_summary %>%
    group_by(scenario, par, delay, group, par_label) %>%
    summarise(cov50 = sum(q25 <= true_value & true_value <= q75),
              cov95 = sum(q2.5 <= true_value & true_value <= q97.5),
              n_sims = n()) %>%
    pivot_longer(cols = c(cov95, cov50),
                 names_to = "metric",
                 values_to = "n_success") %>%
    mutate(interval = ifelse(metric == "cov95", "95% CrI", "50% CrI"),
           coverage = n_success / n_sims) %>%
    rowwise() %>%
    mutate(
      binom_ci = list(binom.test(n_success, n_sims, 
                                 conf.level = 0.95)$conf.int),
      ci_lower = binom_ci[1],
      ci_upper = binom_ci[2]
    ) %>%
    ungroup() %>%
    select(-binom_ci, -n_success, -metric)
  
  ggplot(coverage_data, 
         aes(x = scenario, y = coverage, 
             colour = scenario, shape = interval)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, alpha = 0.6, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    facet_wrap(~par_label) +
    labs(title = "Coverage of Credible Intervals",
         subtitle = "True parameters (ground truth). Error bars: 95% binomial confidence intervals",
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


plot_bias <- function(pars_summary) {
  bias_data <- pars_summary %>%
    mutate(bias = mean - true_value) %>%
    group_by(scenario, par, delay, group, par_label) %>%
    summarise(bias_avg = median(bias),
              bias_sd = sd(bias))
  
  ggplot(bias_data, aes(x = scenario, y = bias_avg, colour = scenario)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    geom_errorbar(aes(ymin = bias_avg - bias_sd, 
                      ymax = bias_avg + bias_sd), 
                  width = 0.3) +
    facet_wrap(~par_label, scales = "free_y") +
    labs(title = "Median Bias of Delay Parameters (+/- SD)",
         subtitle = "Compared to Ground Truth",
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


plot_posterior_delays <- function(pars_summary) {
  pars_summary <- pars_summary %>%
    filter(par_label != "probability of error")
  
  true_values <- pars_summary %>%
    distinct(scenario, par, delay, group, par_label, true_value)
  
  true_values_shared <- true_values %>%
    distinct(par_label, true_value) %>%
    group_by(par_label) %>%
    summarise(shared = n() == 1)
  
  true_values <- true_values %>% right_join(true_values_shared)
  
  ggplot(pars_summary,
         aes(x = mean, colour = scenario)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = true_values %>% filter(shared),
               aes(xintercept = true_value),
               colour = "black", linetype = "dashed", linewidth = 0.8) +
    geom_vline(data = true_values %>% filter(!shared),
               aes(xintercept = true_value, colour = scenario),
               linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~par_label, scales = "free") +
    labs(title = "Posterior Distributions: mean",
         subtitle = "Dashed line = true value. Densities across all simulations.",
         x = "Mean", y = "Density", colour = "Scenario") +
    theme_minimal() +
    theme(strip.text = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}


plot_posterior_prob_error <- function(pars_summary) {
  pars_summary %>%
    filter(par_label == "probability of error") %>%
    ggplot(aes(x = mean, fill = scenario, colour = scenario)) +
    geom_density(alpha = 0.3) +
    geom_vline(aes(xintercept = true_value), linetype = "dashed", linewidth = 0.8) +
    facet_wrap(~scenario, scales = "free_y", nrow = 1) +
    scale_x_continuous(expand = c(0.005, 0), limits = c(0, NA)) +
    scale_y_continuous(expand = c(0, 0.05)) +
    labs(title = "Posterior Distributions: Probability of Error",
         subtitle = "Dashed line = true value. Densities across all simulations.",
         x = "Probability of Error", y = "Density",
         fill = "Scenario", colour = "Scenario") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}

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
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
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
    mutate(
      event = factor(event, levels = global_event_levels, labels = global_event_labels)
    ) %>%
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
    mutate(scenario = factor(scenario, 
                             levels = setdiff(levels(scenario), 
                                              c("Missing dates only (0.2)", 
                                                "No errors or missing dates")))) %>%
    mutate(threshold = paste0(as.character(threshold * 100), "% Threshold")) %>%
    mutate(accuracy = n_true_errors_flagged / n_true_errors) %>%
    filter(!is.na(accuracy)) %>%
    ggplot(aes(x = accuracy, y = group, fill = scenario, colour = scenario)) +
    geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1,
                 position = position_dodge2(reverse = TRUE, padding = 0.1)) +
    facet_grid(threshold ~ .) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_discrete(drop = FALSE, limits = rev) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(
      title = "Individual-level Sensitivity",
      subtitle = 
        "Distribution across simulations (individuals with >= 2 recorded dates)",
      y = "",
      x = "Sensitivity (True Positive Rate)",
      fill = "Scenario") +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          panel.border = element_rect(colour = "darkgrey", 
                                      fill = NA, linewidth = 1),
          legend.position = "right") +
    guides(fill = guide_legend(ncol = 1), colour = "none")
}


plot_coverage <- function(pars_summary) {
  coverage_data <- pars_summary %>%
    filter(!is.na(true_value)) %>%
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
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
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
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
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
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
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
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
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


plot_coverage_by_group <- function(pars_summary) {
  
  coverage_data <- pars_summary %>%
    add_par_roles() %>%
    filter(!is.na(true_value), !is.na(role)) %>%
    group_by(scenario, group, delay, role) %>%
    summarise(
      n_sims = sum(!is.na(q2.5) & !is.na(q97.5)),
      `50% CrI` = sum(q25  <= true_value & true_value <= q75,   na.rm = TRUE),
      `95% CrI` = sum(q2.5 <= true_value & true_value <= q97.5, na.rm = TRUE),
      .groups = "drop") %>%
    filter(n_sims > 0) %>%
    pivot_longer(c(`50% CrI`, `95% CrI`),
                 names_to = "interval", values_to = "n_success") %>%
    rowwise() %>%
    mutate(coverage = n_success / n_sims,
           ci_lower = binom.test(n_success, n_sims)$conf.int[1],
           ci_upper = binom.test(n_success, n_sims)$conf.int[2]) %>%
    ungroup() %>%
    mutate(
      delay = factor(as.character(delay), levels = global_delay_levels, labels = global_delay_labels),
      panel_title = sprintf("<span style='color: #1F77B4;'>%s</span><br>%s", group, role)
    ) %>%
    arrange(group, role) %>%
    mutate(panel_title = factor(panel_title, levels = unique(panel_title)))
  
  ggplot(coverage_data,
         aes(x = delay, y = coverage, colour = scenario)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = 0.75),
                    size = 0.3, fatten = 2.2, alpha = 0.85) +
    facet_wrap(group ~ role, ncol = 2, scales = "free_x") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_discrete(labels = scales::label_wrap(18),
                     expand = expansion(add = 0.4)) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = "Coverage of Credible Intervals",
         subtitle = paste("True parameters (ground truth).",
                          "Error bars: 95% binomial confidence intervals"),
         x = NULL, y = "Coverage Probability",
         colour = "Scenario") +
    theme_minimal() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          legend.position = "right",
          legend.key.height = unit(0.8, "lines"),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10))) +
    guides(colour = guide_legend(ncol = 1, order = 1))
}

plot_performance_figure <- function(pars_summary, target_role = "Mean") {

  plot_data_raw <- pars_summary %>%
    add_par_roles() %>%
    filter(!is.na(true_value), !is.na(role), role == target_role) %>%
    mutate(sim_rel_bias = (median - true_value) / true_value) %>%
    group_by(scenario, group, delay, role) %>%
    summarise(
      n_sims = sum(!is.na(q2.5) & !is.na(q97.5)),
      cov_50_n = sum(q25 <= true_value & true_value <= q75, na.rm = TRUE),
      cov_95_n = sum(q2.5 <= true_value & true_value <= q97.5, na.rm = TRUE),
      bias_median = median(sim_rel_bias, na.rm = TRUE),
      bias_low = quantile(sim_rel_bias, 0.025, na.rm = TRUE),
      bias_high = quantile(sim_rel_bias, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_sims > 0) %>%
    rowwise() %>%
    mutate(
      cov_50 = cov_50_n / n_sims,
      cov_95 = cov_95_n / n_sims,
      cov_50_lower = binom.test(cov_50_n, n_sims)$conf.int[1],
      cov_50_upper = binom.test(cov_50_n, n_sims)$conf.int[2],
      cov_95_lower = binom.test(cov_95_n, n_sims)$conf.int[1],
      cov_95_upper = binom.test(cov_95_n, n_sims)$conf.int[2]
    ) %>%
    ungroup()

  # maximum number of delays in any single group
  delay_counts <- plot_data_raw %>% distinct(group, delay) %>% count(group)
  max_delays <- max(delay_counts$n, na.rm = TRUE)

  # groups that need padding to match max_delays
  dummy_groups <- delay_counts %>% filter(n < max_delays)

  if (nrow(dummy_groups) > 0) {
    dummy_rows <- dummy_groups %>%
      rowwise() %>%
      mutate(delay = list(strrep(" ", 1:(max_delays - n)))) %>%
      unnest(delay) %>%
      mutate(scenario = plot_data_raw$scenario[1]) %>%
      select(group, delay, scenario) %>%
      ungroup()

    plot_data <- bind_rows(plot_data_raw, dummy_rows)
  } else {
    plot_data <- plot_data_raw
  }

  dummy_levels <- strrep(" ", 1:max_delays)

  plot_data <- plot_data %>%
    mutate(
      delay = factor(as.character(delay),
                     levels = c(global_delay_levels, dummy_levels),
                     labels = c(global_delay_labels, dummy_levels))
    )

  cov_data <- plot_data %>%
    select(scenario, group, delay, cov_50, cov_95,
           cov_50_lower, cov_50_upper, cov_95_lower, cov_95_upper) %>%
    pivot_longer(c(cov_50, cov_95),
                 names_to = "interval",
                 values_to = "coverage") %>%
    mutate(
      ci_lower = ifelse(interval == "cov_50", cov_50_lower, cov_95_lower),
      ci_upper = ifelse(interval == "cov_50", cov_50_upper, cov_95_upper),
      interval = factor(ifelse(interval == "cov_50", "50% CrI", "95% CrI"),
                        levels = c("50% CrI", "95% CrI"))
    )

  # coverage
  p_cov <- ggplot(cov_data, aes(x = delay, y = coverage, colour = scenario)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = 0.85),
                    size = 0.25, fatten = 1.8, alpha = 0.85, na.rm = TRUE) +
    facet_wrap(~ group, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_discrete(labels = scales::label_wrap(18),
                     expand = expansion(add = 0.4)) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = sprintf("Coverage of the %s", target_role),
         x = NULL, y = "Coverage Probability", colour = "Scenario") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10, face = "bold", hjust = 0,
                                margin = margin(t = 5, b = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.spacing = unit(1.5, "lines"),
      panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 1))

  # relative bias
  p_bias <- ggplot(plot_data, aes(x = delay, y = bias_median, colour = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black", alpha = 0.5) +
    geom_pointrange(aes(ymin = bias_low, ymax = bias_high),
                    position = position_dodge(width = 0.85),
                    size = 0.25, fatten = 1.8, alpha = 0.85, na.rm = TRUE) +
    facet_wrap(~ group, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = scales::percent, limits = c(-1, 1)) +
    scale_x_discrete(labels = scales::label_wrap(18), expand = expansion(add = 0.4)) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = sprintf("Relative Bias of the %s", target_role),
         x = NULL, y = "Relative Bias (Median vs True)", colour = "Scenario") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10, face = "bold", colour = "transparent",
                                margin = margin(t = 5, b = 10)),
      axis.title.y = element_text(margin = margin(r = 10, l = 10)),
      panel.spacing = unit(1.5, "lines"),
      panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 1))

  combined_plot <- p_cov + p_bias +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "right",
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 11, hjust = 0.5))

  return(combined_plot)
}


plot_sensitivity_figure <- function(errors_summary, target_threshold = 0.5) {
  
  sens_data <- errors_summary %>%
    filter(threshold == target_threshold) %>%
    filter(!scenario %in% c("Missing dates only (0.2)",
                            "No errors or missing dates")) %>%
    filter(trimws(scenario) != "") %>%
    mutate(scenario = factor(scenario,
                             levels = setdiff(levels(scenario),
                                              c("Missing dates only (0.2)",
                                                "No errors or missing dates"))))
  
  indiv_data <- sens_data %>% 
    filter(event == "individual") %>%
    mutate(accuracy = n_true_errors_flagged / n_true_errors) %>%
    filter(!is.na(accuracy))
  
  event_data <- sens_data %>% 
    filter(event != "individual") %>%
    group_by(scenario, group, event) %>%
    summarise(pct_accuracy = sum(n_true_errors_flagged) / sum(n_true_errors),
              .groups = "drop") %>%
    filter(!is.na(pct_accuracy)) %>%
    mutate(event = factor(event,
                          levels = global_event_levels,
                          labels = global_event_labels))
  
  # individual plot
  p_indiv <- ggplot(indiv_data, aes(x = scenario, y = accuracy,
                                    fill = scenario, colour = scenario)) +
    geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1,
                 position = position_dodge2(reverse = TRUE, padding = 0.1)) +
    facet_grid(group ~ .) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = "Individual-Level Sensitivity",
         y = sprintf("Sensitivity (%.0f%% Threshold)", target_threshold * 100),
         x = "") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 10, l = 10)),
          legend.position = "none")
  
  # event plot grid
  p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                    fill = pct_accuracy)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f", pct_accuracy * 100)),
              size = 2.8, colour = "grey20") +
    facet_grid(group ~ .) +
    scale_fill_gradient2(midpoint = 0.5,
                         low = "firebrick", mid = "white", high = "steelblue", 
                         limits = c(0, 1), labels = scales::percent,
                         na.value = "grey95") +
    scale_y_discrete(drop = FALSE, limits = rev) +
    labs(title = "Event-Level Sensitivity",
         y = "",
         x = "",
         fill = sprintf("Sensitivity\n(%.0f%% Threshold)",
                        target_threshold * 100)) +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          panel.grid = element_blank(),
          legend.title = element_text(margin = margin(b = 15)))
  
  combined_plot <- p_indiv + p_event + 
    plot_layout(ncol = 2, widths = c(1, 1))
  
  return(combined_plot)
}

plot_specificity_figure <- function(errors_summary, target_threshold = 0.5) {

  spec_data <- errors_summary %>%
    filter(threshold == target_threshold) %>%
    filter(trimws(scenario) != "")

  indiv_data <- spec_data %>%
    filter(event == "individual") %>%
    filter(n_true_non_errors > 0) %>%
    mutate(specificity =
             (n_true_non_errors - n_false_positives) / n_true_non_errors) %>%
    filter(!is.na(specificity))

  event_data <- spec_data %>%
    filter(event != "individual") %>%
    group_by(scenario, group, event) %>%
    summarise(pct_specificity =
                (sum(n_true_non_errors) - sum(n_false_positives)) /
                sum(n_true_non_errors),
              .groups = "drop") %>%
    filter(!is.na(pct_specificity)) %>%
    mutate(event = factor(event,
                          levels = global_event_levels,
                          labels = global_event_labels))

  p_indiv <- ggplot(indiv_data, aes(x = scenario, y = specificity,
                                    fill = scenario, colour = scenario)) +
    geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1,
                 position = position_dodge2(reverse = TRUE, padding = 0.1)) +
    facet_grid(group ~ .) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_fill_manual(values = scenario_colours, drop = FALSE) +
    scale_colour_manual(values = scenario_colours, drop = FALSE) +
    labs(title = "Individual-Level Specificity",
         y = sprintf("Specificity (%.0f%% Threshold)", target_threshold * 100),
         x = "") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(margin = margin(r = 10, l = 10)),
          legend.position = "none")

  p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                    fill = pct_specificity)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f", pct_specificity * 100)),
              size = 2.8, colour = "grey20") +
    facet_grid(group ~ .) +
    scale_fill_gradient2(midpoint = 0.5,
                         low = "firebrick", mid = "white", high = "steelblue",
                         limits = c(0, 1), labels = scales::percent,
                         na.value = "grey95") +
    scale_y_discrete(drop = FALSE, limits = rev) +
    labs(title = "Event-Level Specificity",
         y = "",
         x = "",
         fill = sprintf("Specificity\n(%.0f%% Threshold)",
                        target_threshold * 100)) +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold", angle = 270),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1),
          panel.grid = element_blank(),
          legend.title = element_text(margin = margin(b = 15)))

  combined_plot <- p_indiv + p_event +
    plot_layout(ncol = 2, widths = c(1, 1))

  return(combined_plot)
}


plot_all_specificity <- function(errors_summary) {

  target_thresholds <- sort(unique(errors_summary$threshold))
  plot_list <- list()
  
  for (i in seq_along(target_thresholds)) {
    thr <- target_thresholds[i]
    is_bottom_row <- (i == length(target_thresholds))
    
    spec_data <- errors_summary %>%
      filter(threshold == thr) %>%
      filter(trimws(scenario) != "")
    
    indiv_data <- spec_data %>% 
      filter(event == "individual") %>%
      filter(n_true_non_errors > 0) %>%
      mutate(specificity = (n_true_non_errors - n_false_positives) /
               n_true_non_errors) %>%
      filter(!is.na(specificity))
    
    event_data <- spec_data %>% 
      filter(event != "individual") %>%
      group_by(scenario, group, event) %>%
      summarise(
        pct_specificity = (sum(n_true_non_errors) - sum(n_false_positives)) /
          sum(n_true_non_errors), 
        .groups = "drop"
      ) %>%
      filter(!is.na(pct_specificity)) %>%
      mutate(event = factor(event,
                            levels = global_event_levels,
                            labels = global_event_labels))
    
    p_indiv <- ggplot(indiv_data, aes(x = scenario, y = specificity,
                                      fill = scenario, colour = scenario)) +
      geom_boxplot(alpha = 0.8, width = 0.7, outlier.size = 1) +
      facet_grid(group ~ .) +
      scale_x_discrete(drop = TRUE) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_fill_manual(values = scenario_colours, drop = FALSE) +
      scale_colour_manual(values = scenario_colours, drop = FALSE) +
      labs(title = sprintf("Individual-Level Specificity (%.0f%% Threshold)", thr * 100),
           y = "Specificity",
           x = "") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "darkgrey",
                                        fill = NA, linewidth = 1),
            strip.text.y = element_text(size = 9, face = "bold", angle = 270),
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            legend.position = "none")
    
    p_event <- ggplot(event_data, aes(x = scenario, y = event,
                                      fill = pct_specificity)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f", pct_specificity * 100)),
                size = 2.8, colour = "grey20") +
      facet_grid(group ~ .) +
      scale_fill_gradient2(midpoint = 0.5,
                           low = "firebrick", mid = "white", high = "steelblue", 
                           limits = c(0, 1), labels = scales::percent,
                           na.value = "grey95") +
      scale_y_discrete(drop = FALSE, limits = rev, expand = c(0, 0)) +
      labs(title = sprintf("Event-Level Specificity (%.0f%% Threshold)", thr * 100),
           y = "",
           x = "",
           fill = "Specificity") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "darkgrey",
                                        fill = NA, linewidth = 1),
            strip.text.y = element_text(size = 9, face = "bold", angle = 270),
            panel.grid = element_blank(),
            legend.title = element_text(margin = margin(b = 15)))
    
    # only show x-axis labels for bottom plot in the stack
    if (is_bottom_row) {
      p_indiv <- p_indiv +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      p_event <- p_event +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # remove x-axis text and ticks for top plots
      p_indiv <- p_indiv +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      p_event <- p_event +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }

    plot_list[[length(plot_list) + 1]] <- p_indiv
    plot_list[[length(plot_list) + 1]] <- p_event
  }
  
  combined_plot <- wrap_plots(plot_list, ncol = 2, widths = c(1, 1)) + 
    plot_layout(guides = "collect")
  
  return(combined_plot)
}
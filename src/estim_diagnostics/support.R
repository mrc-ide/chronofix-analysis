combine_df <- function(scenarios, type) {
  scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenarios)]
  scenario_labels <- factor(scenario_labels, levels = scenario_labels)
  
  df <- map_dfr(scenarios, ~readRDS(glue("{type}_{.x}.rds")))
  apply_factor_levels(df, scenario_labels)
}


# Re-apply correct factor orderings after the bind_rows ----------------------
apply_factor_levels <- function(df, scenario_labels) {
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


plot_event_sensitivity <- function(event_confusion) {
  event_confusion %>%
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
}


plot_indiv_sensitivity <- function(indiv_performance) {
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
  
  
  plot_data %>%
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
}


plot_posterior_delay_mean <- function(posterior_data_trimmed, ref_lines) {
  ggplot(posterior_data_trimmed,
         aes(x = post_mean, colour = scenario)) +
    geom_density() +
    geom_vline(data = ref_lines %>% filter(mean_shared) %>% distinct(param_label, group, true_mean),
               aes(xintercept = true_mean),
               colour = "black", linetype = "dashed", linewidth = 0.8) +
    geom_vline(data = ref_lines %>% filter(!mean_shared),
               aes(xintercept = true_mean, colour = scenario),
               linetype = "dashed", linewidth = 0.8) +
    facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
    labs(title = "Posterior Distributions: Mean Delay",
         subtitle = "Dashed line = true value. Densities across all simulations.",
         x = "Mean Delay (days)", y = "Density", colour = "Scenario") +
    theme_minimal() +
    theme(strip.text = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}


plot_posterior_cv <- function(posterior_data_trimmed, ref_lines) {
  ggplot(posterior_data_trimmed,
         aes(x = post_cv, colour = scenario)) +
    geom_density() +
    geom_vline(data = ref_lines %>% filter(cv_shared) %>% distinct(param_label, group, true_cv),
               aes(xintercept = true_cv),
               colour = "black", linetype = "dashed", linewidth = 0.8) +
    geom_vline(data = ref_lines %>% filter(!cv_shared),
               aes(xintercept = true_cv, colour = scenario),
               linetype = "dashed", linewidth = 0.8) +
    facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
    labs(title    = "Posterior Distributions: CV",
         subtitle = "Dashed line = true value. Densities across all simulations.",
         x = "Coefficient of Variation", y = "Density", colour = "Scenario") +
    theme_minimal() +
    theme(strip.text  = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}


plot_posterior_prob_error <- function(all_draws, true_params) {
  all_draws %>%
    filter(param_label %in% "probability of error") %>%
    left_join(true_params %>%
                filter(param_idx == 0) %>%
                select(scenario, true_mean), by = "scenario") %>%
    ggplot(aes(x = post_mean, fill = scenario, colour = scenario)) +
    geom_density(alpha = 0.3) +
    geom_vline(aes(xintercept = true_mean), linetype = "dashed", linewidth = 0.8) +
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


plot_observed_patterns <- function(obs_pattern_summary) {
  obs_pattern_summary %>%
    ggplot(aes(x = reorder(pattern, n_individuals), y = pct, fill = pattern)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 2.5) +
    coord_flip() +
    facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
    labs(title = "Check simulated error/missingness patterns",
         x = "", y = "Percentage of Individuals (%)", fill = "Pattern") +
    theme_minimal() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}


plot_ess <- function(sim_summaries) {
  sim_summaries %>%
    ggplot(aes(x = scenario, y = ess_bulk_est, fill = scenario)) +
    geom_violin(alpha = 0.3, scale = "width") +
    geom_jitter(aes(colour = scenario), width = 0.2, alpha = 0.5, size = 1) +
    geom_hline(yintercept = 200, linetype = "dashed", colour = "black", linewidth = 0.8) +
    facet_wrap(~param_label, scales = "free_y") +
    labs(
      title = "Distribution of effective sample size across simulations",
      subtitle = "Black line = threshold of 200",
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


plot_problem_traces <- function(problem_sims) {
  dir.create("figures/problem_traces", recursive = TRUE, showWarnings = FALSE)
  
  problem_combos <- problem_sims %>%
    distinct(scenario, simulation)
  
  true_params_plot <- true_params %>%
    select(scenario, param_label, group, true_mean) %>%
    mutate(group = forcats::fct_na_value_to_level(group, "Global"))
  
  if (nrow(problem_combos) > 0) {
    for(i in 1:nrow(problem_combos)) {
      scen <- as.character(problem_combos$scenario[i])
      sim <- problem_combos$simulation[i]
      
      plot_data <- all_draws %>%
        filter(scenario == scen, simulation == sim) %>%
        mutate(group = forcats::fct_na_value_to_level(group, "Global"))
      
      failed_params <- problem_sims %>%
        filter(scenario == scen, simulation == sim) %>%
        pull(param_label) %>%
        unique() %>%
        paste(collapse = ", ")
      
      p <- ggplot(plot_data, aes(x = iteration, y = post_mean, colour = factor(chain))) +
        rasterise(geom_line(alpha = 0.6, linewidth = 0.5), dpi = 300) +
        geom_hline(data = true_params_plot %>% filter(scenario == scen),
                   aes(yintercept = true_mean), linetype = "dashed", colour = "black", linewidth = 0.8) +
        facet_grid(rows = vars(group), cols = vars(param_label), scales = "free_y") +
        labs(title = glue("Diagnostic Trace: {scen} (Sim {sim})"),
             subtitle = glue("Failed params: {failed_params}\nDashed line = True Value"),
             x = "Iteration", y = "Estimate", colour = "Chain") +
        theme_bw() +
        theme(strip.text = element_text(size = 7, face = "bold"),
              legend.position = "bottom",
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)))
      
      clean_scen <- str_replace_all(scen, "[^[:alnum:]]", "_")
      file_name <- file.path("figures/problem_traces", glue("trace_{clean_scen}_sim{sim}.pdf"))
      
      ggsave(file_name, plot = p, width = 16, height = 10)
    }
  }
}


plot_rhat_vs_ess <- function(sim_summaries) {
  sim_summaries %>%
    ggplot(aes(x = ess_bulk_est, y = rhat_est)) +
    geom_point(aes(colour = scenario), alpha = 0.4) +
    geom_vline(xintercept = 200, linetype = "dotted") +
    geom_hline(yintercept = 1.05, linetype = "dotted") +
    facet_grid(rows = vars(scenario), cols = vars(param_label),
               scales = "free_y") +
    labs(title = "Rhat vs ESS",
         subtitle = "Top-left quadrant = above rhat and below bulk ess thresholds",
         y = "Rhat", x = "Bulk ESS") +
    theme_bw() +
    theme(strip.text = element_text(size = 8, face = "bold"),
          legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}
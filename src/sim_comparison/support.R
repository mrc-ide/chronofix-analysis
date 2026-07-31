comparison_scenarios <- list(
  sanity = c("baseline", "no_error", "no_missing", "no_error_no_missing"),
  variable_error = c("baseline", "low_error", "high_error"),
  variable_sample_size = c("baseline", "very_small_sample", "small_sample",
                           "moderate_sample", "very_large_sample"),
  variable_delay_length = c("baseline", "short_delays", "long_delays"),
  variable_delay_cv = c("baseline", "low_variability", "high_variability"),
  variable_delay_distribution = c("baseline", "lognormal_delays")
)


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


combine_df <- function(scenarios, type) {
  scenario_labels <- 
    scenario_labels[intersect(names(scenario_labels), scenarios)]
  scenario_labels <- factor(scenario_labels, levels = scenario_labels)
  names(scenarios) <- scenarios
  df <- purrr::map_dfr(scenarios, ~readRDS(glue("inputs/{type}_{.x}.rds")), 
                       .id = "scenario")
  apply_factor_levels(df, scenario_labels)
}


# Re-apply correct factor orderings after the bind_rows
apply_factor_levels <- function(df, scenario_labels) {
  if("scenario" %in% names(df)) {
    df$scenario <- factor(df$scenario, levels = names(scenario_labels),
                          labels = unname(scenario_labels))
  }
  
  if("group" %in% names(df)) {
    df$group <- factor(df$group, 
                       levels = c("community-alive", "community-dead", 
                                  "hospitalised-alive", "hospitalised-dead"))
  }
  
  if ("par_label" %in% names(df)) {
    levels <- unique(df$par_label)
    df$par_label <- factor(df$par_label, levels = unique(df$par_label))
  }
  df
}

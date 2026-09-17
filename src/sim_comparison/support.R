comparison_scenarios <- list(
  sanity = c("baseline", "no_error", "no_missing", "no_error_no_missing"),
  variable_error = c("baseline", "low_error", "high_error"),
  variable_sample_size = c("baseline", "very_small_sample", "small_sample",
                           "moderate_sample", "very_large_sample"),
  variable_delay_length = c("baseline", "short_delays", "long_delays"),
  variable_delay_cv = c("baseline", "low_variability", "high_variability"),
  variable_delay_distribution = c("baseline", "lognormal_delays"),
  same_delay_means = c("baseline", "same_means"),
  all_scenarios = c(
    "baseline", 
    "gap1", "low_missingness", "no_missing", "no_error", "no_error_no_missing",
    "gap2", "low_error", "high_error",
    "gap3", "very_small_sample", "small_sample", "moderate_sample", "very_large_sample",
    "gap4", "long_delays", "short_delays",
    "gap5", "high_variability", "low_variability",
    "gap6", "lognormal_delays", "same_means"
  )
)


scenario_labels <- c(
  "baseline" = "Baseline",
  
  # workaround to add some whitespace between scenario groups in the legend
  "gap1" = " ",
  
  "low_missingness" = "Low missingness (0.05)",
  "no_missing" = "Errors only (0.05)",
  "no_error" = "Missing dates only (0.2)",
  "no_error_no_missing" = "No errors or missing dates",
  
  "gap2" = "  ",
  
  "low_error" = "Low error (0.02)",
  "high_error" = "High error (0.2)",
  
  "gap3" = "   ",
  
  "very_small_sample" = "Very small groups (n = 10)",
  "small_sample" = "Small groups (n = 20)",
  "moderate_sample" = "Moderate groups (n = 50)",
  "very_large_sample" = "Very large groups (n = 500)",
  
  "gap4" = "    ",
  
  "short_delays" = "Short delays (0.5x baseline)",
  "long_delays" = "Long delays (2x baseline)",
  
  "gap5" = "     ",
  
  "low_variability" = "Low variability (0.5x baseline cv)",
  "high_variability" = "High variability (2x baseline cv)",
  
  "gap6" = "      ",
  
  "lognormal_delays" = "Lognormal delays",
  "same_means" = "Same mean delay (7 days)"
)

scenario_colours <- c(
  "Baseline" = "#000000",
  
  # blank for the gaps in legend
  " " = "transparent",
  "  " = "transparent",
  "   " = "transparent",
  "    " = "transparent",
  "     " = "transparent",
  "      " = "transparent",
  
  "Low missingness (0.05)" = "#6BAED6",
  "Errors only (0.05)" = "#3182BD",
  "Missing dates only (0.2)" = "#08519C",
  "No errors or missing dates" = "#08306B",
  
  "Low error (0.02)" = "#E0A21F",
  "High error (0.2)" = "#7F4F0A",
  
  "Very small groups (n = 10)" = "#FB7B5B",
  "Small groups (n = 20)" = "#EF3B2C",
  "Moderate groups (n = 50)" = "#CB181D",
  "Very large groups (n = 500)" = "#67000D",
  
  "Short delays (0.5x baseline)" = "#74C476",
  "Long delays (2x baseline)" = "#00441B",
  
  "Low variability (0.5x baseline cv)" = "#9E9AC8",
  "High variability (2x baseline cv)" = "#3F007D",
  
  "Lognormal delays" = "#F768A1",
  "Same mean delay (7 days)" = "#7A0177"
)

global_delay_levels <- c("onset to report", "onset to hospitalisation",
                         "hospitalisation to discharge", "hospitalisation to death",
                         "onset to death")
global_delay_labels <- tools::toTitleCase(global_delay_levels)

global_group_levels <- c("community-alive", "community-dead",
                         "hospitalised-alive", "hospitalised-dead")
global_group_labels <- tools::toTitleCase(gsub("-", " ", global_group_levels))

global_event_levels <- c("onset", "report", "hospitalisation", "discharge", "death")
global_event_labels <- tools::toTitleCase(global_event_levels)


combine_df <- function(scenarios, type) {
  scenario_labels <- 
    scenario_labels[intersect(names(scenario_labels), scenarios)]
  scenario_labels <- factor(scenario_labels, levels = scenario_labels)
  actual_scenarios <- scenarios[!grepl("^gap", scenarios)]
  names(actual_scenarios) <- actual_scenarios
  df <- purrr::map_dfr(actual_scenarios, ~readRDS(glue("inputs/{type}_{.x}.rds")), 
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
                       levels = global_group_levels, 
                       labels = global_group_labels)
  }
  
  if ("par_label" %in% names(df)) {
    levels <- unique(df$par_label)
    df$par_label <- factor(df$par_label, levels = unique(df$par_label))
  }
  df
}


# Combine gamma and lognormal into same panels
add_par_roles <- function(df) {
  roles <- c("Mean", "CV", "95th Quantile")
  df %>%
    filter(par != "probability of error") %>%
    mutate(role = factor(case_when(
      par == "mean" ~ roles[1],
      par == "cv" ~ roles[2],
      par == "q95" ~ roles[3],
      TRUE ~ NA
    ), levels = roles))
}

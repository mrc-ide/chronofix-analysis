summarise_pars <- function(samples, delay_info, true_prob_error) {
  samples$data <- NULL
  samples_df <- posterior::as_draws_df(samples)
  p_quantile <- c(0.025, 0.25, 0.75, 0.975)
  summary <- 
    posterior::summarise_draws(samples_df, mean, median, sd, 
                               ~posterior::quantile2(.x, p_quantile),
                               posterior::default_convergence_measures())
  
  prob_error <- data.frame(par = "probability of error",
                           delay = NA,
                           group = NA,
                           variable = "prob_error",
                           true_value = true_prob_error)
  
  get_delay_info_i <- function(i) {
    true_mean <- delay_info$mean[i]
    true_cv <- delay_info$cv[i]
    if (delay_info$distribution[i] == "gamma") {
      par <- c("mean", "shape")
      true_shape <- 1 / true_cv^2
      true_value <- c(true_mean, true_shape)
    } else if (delay_info$distribution[i] == "log-normal") {
      par <- c("meanlog", "precisionlog")
      true_precisionlog <- 1 / log(true_cv^2 + 1)
      true_meanlog <- log(true_mean) - 1 / (2 * true_precisionlog)
      true_value <- c(true_meanlog, true_precisionlog)
    } 
    delay <- paste0(delay_info$from[i], " to ", delay_info$to[i])
    group <- delay_info$group[i]
    
    data.frame(par = par,
               delay = delay,
               group = group,
               variable = paste0("delay", i, "_", par),
               true_value = true_value)
  }
  
  delays <- lapply(seq_len(nrow(delay_info)),
                   get_delay_info_i) %>%
    dplyr::bind_rows() %>% 
    arrange(group)
  
  rbind(prob_error, delays) %>%
    mutate(
      par_label = dplyr::case_when(par == "probability of error" ~ par,
                                   .default = paste0(delay, " ", par,
                                                     " (", group, ")"))) %>%
    right_join(summary)
}


summarise_errors <- function(samples, data) {
  sample_errors <- samples$data$error_indicators
  data_errors <- data$error_indicators
  n_samples <- dim(sample_errors)[3]
  
  date_cols <- c("onset", "hospitalisation", "report", "death", "discharge")
  ## we will only include individuals with at least two non-missing dates
  eligible <- rowSums(!is.na(data_errors[, date_cols])) > 1
  data_errors <- data_errors[eligible, , drop = FALSE]
  sample_errors <- sample_errors[eligible, , , drop = FALSE]
  
  ## vector indicating whether or not the individual has at least one true error
  data_errors$individual <- 
    apply(data_errors[, date_cols], 1, function(x) sum(x, na.rm = TRUE)) > 0
  
  
  ## n_individuals by n_dates array of proportion of samples where the date is
  ## an  error 
  prop_is_sample_errors <- apply(sample_errors, c(1, 2), sum) / 
    n_samples
  
  ## n_individuals by n_samples array of whether there is at least one error
  individual_has_sample_errors <- 
    apply(sample_errors, c(1, 3), function(x) sum(x, na.rm = TRUE)) > 0
  ## n_individuals vector of proportion of samples where the individual has
  ## as least one error
  prop_individual_has_sample_errors <- 
    apply(individual_has_sample_errors, 1, sum) / n_samples
  prop_is_sample_errors <- cbind(prop_is_sample_errors,
                                 prop_individual_has_sample_errors)
  colnames(prop_is_sample_errors) <- c(date_cols, "individual")
  
  calc_error_summary <- function(threshold) {
    df_sample_errors <- 
      data.frame(id = data_errors$id,
                 prop_is_sample_errors > threshold)
    
    df_sample_errors <- df_sample_errors %>%
      pivot_longer(!id, names_to = "event", values_to = "sample_error")
    
    data_errors %>%
      pivot_longer(!c(id, group), names_to = "event", 
                   values_to = "data_error") %>%
      right_join(df_sample_errors) %>%
      group_by(group, event) %>%
      summarise(n_true_errors_flagged = sum(sample_error & data_error, 
                                            na.rm = TRUE),
                n_true_errors = sum(data_error, na.rm = TRUE)) %>%
      mutate(threshold = threshold) %>%
      relocate(threshold, .after = event)
  }
  
  rbind(calc_error_summary(0.5), calc_error_summary(0.95))
  
}

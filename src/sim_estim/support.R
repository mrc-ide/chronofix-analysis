summarise_pars <- function(samples, delay_map) {
  samples$data <- NULL
  samples_df <- posterior::as_draws_df(samples)
  summary <- posterior::summarise_draws(samples_df)
  
  prob_error <- data.frame(par = "probability of error",
                           delay = NA,
                           group = NA,
                           variable = "prob_error")
  
  get_delay_info_i <- function(i) {
    if (delay_map$distribution[i] == "gamma") {
      par <- c("mean", "shape")
    } else if (delay_map$distribution[i] == "log-normal") {
      par <- c("meanlog", "precisionlog")
    } 
    data.frame(par = par,
               delay = paste0(delay_map$from[i], " to ", delay_map$to[i]),
               group = delay_map$group[i],
               variable = paste0("delay", i, "_", par))
  }
  
  delays <- lapply(seq_len(nrow(delay_map)),
                   get_delay_info_i) %>%
    dplyr::bind_rows() %>% 
    arrange(group)
  
  rbind(prob_error, delays) %>%
    right_join(summary) %>%
    select(!variable)

}

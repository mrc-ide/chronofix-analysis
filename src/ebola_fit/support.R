prepare_data <- function(data) {
  data$id <- seq_len(nrow(data))
  data %>% 
    filter(EpiCaseDef == "confirmed") %>%
    filter(!is.na(FinalStatus)) %>%
    filter(!retrospective) %>%
    filter(!is.na(HospitalizedEver)) %>%
    mutate(group = case_when(
      FinalStatus == "Alive" & HospitalizedEver == "Yes" ~ "hospitalised-alive",
      FinalStatus == "Dead" & HospitalizedEver == "Yes" ~ "hospitalised-dead",
      FinalStatus == "Alive" & HospitalizedEver == "No" ~ "community-alive",
      FinalStatus == "Dead" & HospitalizedEver == "No" ~ "community-dead")) %>%
    mutate(onset = DateOnset,
           report = DateReport,
           hospitalisation = DateHospitalCurrentAdmit,
           discharge = case_when(
             group == "hospitalised-alive" ~ DateOutcomeComp,
             group != "hospitalised-alive" ~ NA),
           death = case_when(FinalStatus == "Dead" ~ DateOutcomeComp,
                             FinalStatus != "Dead" ~ NA)) %>%
    filter(!(is.na(onset) & is.na(report) & is.na(hospitalisation) & 
               is.na(discharge) & is.na(death))) %>%
    select(id, group, onset, report, hospitalisation, discharge, death)
}


summarise_pars <- function(samples, delay_info) {
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
                           variable = "prob_error")
  
  get_delay_info_i <- function(i) {
    true_mean <- delay_info$mean[i]
    true_cv <- delay_info$cv[i]
    if (delay_info$distribution[i] == "gamma") {
      par <- c("mean", "shape")
    } else if (delay_info$distribution[i] == "log-normal") {
      par <- c("meanlog", "precisionlog")
    } 
    delay <- paste0(delay_info$from[i], " to ", delay_info$to[i])
    group <- delay_info$group[i]
    
    data.frame(par = par,
               delay = delay,
               group = group,
               variable = paste0("delay", i, "_", par))
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

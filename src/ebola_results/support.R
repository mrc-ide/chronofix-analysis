calc_credible_set <- function(samples, i, j, cred_level) {
  estimated_dates <- 
    as.Date(floor(samples$augmented_data$estimated_dates[i, j, ]))
  freq <- sort(table(estimated_dates), decreasing = TRUE)
  prob <- freq / sum(freq)
  cum_prob <- cumsum(prob)
  k <- which(cum_prob >= cred_level)[1]
  names(cum_prob[seq_len(k)])
}

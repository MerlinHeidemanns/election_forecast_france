#' ppc_obs_xi_past
#' @param fit cmdstanr fit containing xi_past
#' @param xi_past True data with election_id and candidate id
ppc_plt_xi_past <- function(fit, xi_past = NULL){
  xi_past_hat <- fit$summary("xi_past", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(xi_past_hat) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  xi_past_hat <- xi_past_hat %>%
    mutate(
      election_id = as.integer(str_match(variable, ",([\\d]+)")[,2]),
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(q50 > -100)
  plt <- ggplot(xi_past_hat, aes(x = interaction(candidate_id, election_id))) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(y = "Polling error",
         x = "Interaction (candidate, election",
         caption = "logodds, 0.5, 0.8, 0.9")
  if (!is.null(xi_past)){
    xi_past_hat <- xi_past_hat %>%
      left_join(xi_past)
    plt <- plt +
      geom_point(aes(x = interaction(candidate_id, election_id),
                     y = xi_logodds), color = "red", shape = 2,
                 data = xi_past_hat)
  }
  return(plt)
}
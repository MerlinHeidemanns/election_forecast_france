#' ppc_obs_prob_sigma_cov
#' @param fit cmdstanr fit with prob_sigma_cov
#' @param supvec_names Names of candidates
ppc_obs_prob_sigma_cov <- function(fit, supvec_names = supvec_names){
  prob_sigma_cov <- fit$summary("prob_sigma_cov", ~ quantile(. * 100, c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(prob_sigma_cov) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  prob_sigma_cov <- prob_sigma_cov %>%
    mutate(
      candidate = supvec_names[as.integer(str_match(variable, "([\\d]+)")[,1])]
    ) %>%
    dplyr::select(-variable)

  plt <- ggplot(prob_sigma_cov, aes(x = candidate, y = q50)) +
    geom_point() +
    coord_flip() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Day-to-day change (percentage points)")
  return(plt)
}
#' ppc_obs_sigma
#' @param fit Cmdstan fit
ppc_obs_sigma <- function(fit){
  plt <- fit$draws(c("sigma_xi", "sigma_alpha", "sigma_tau")) %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "draws") %>%
    filter(!grepl("\\.", variable)) %>%
    ggplot(aes(x = draws)) +
    geom_histogram(position = 'identity', bins = 50) +
    facet_grid(.~variable) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = "Estimate")
  return(plt)
}
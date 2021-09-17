#' ppc_obs_xi
#' @param fit cmdstanr fit
#' @param supvec_names vector with candidate names in correct order
ppc_obs_xi <- function(fit, supvec_names){
  xi <- fit$summary("invlogit_xi", ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9))) %>%
    pivot_longer(c(-variable),
                 names_to = "q",
                 values_to = "val",
                 names_pattern = "(\\d+)") %>%
    mutate(candidate = supvec_names[as.integer(str_match(variable, "(\\d+)")[, 2])]) %>%
    pivot_wider(id_cols = candidate,
                names_from = q,
                values_from = val,
                names_prefix = "q")
  plt <- ggplot(xi, aes(x = candidate, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    coord_flip() +
    theme_light() +
    theme(axis.title.y = element_blank()) +
    labs(y = "Polling error (percentage points)")
  return(plt)
}
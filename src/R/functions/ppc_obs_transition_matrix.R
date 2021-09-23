#' ppc_obs_transition_matrix
#' @param fit cmdstanr fit
#' @param supvec_names Vector of candidate names
ppc_obs_transition_matrix <- function(fit, supvec_names = supvec_names){
  ## Take median
  transition_matrix_hat <- fit$summary("transition_matrix", ~ quantile(., 0.5))
  colnames(transition_matrix_hat) <- c("variable", "q50")
  transition_matrix_hat <- transition_matrix_hat %>%
    mutate(
      from_candidate = supvec_names[as.integer(str_match(variable, "([\\d+]+),")[,2])],
      to_candidate = supvec_names[as.integer(str_match(variable, ",([\\d+]+)")[,2])],
    ) %>%
    dplyr::select(-variable) %>%
    mutate(q50 = abs(q50))

  ## Categorize
  transition_matrix_hat <- transition_matrix_hat %>%
    mutate(q50_cat = round(q50, 1))

  ## Plot
  plt <- ggplot(data = transition_matrix_hat,
                aes(x = to_candidate, y = from_candidate, fill = as.factor(q50_cat))) +
    geom_tile(alpha = 0.5) +
    labs(fill = "Transition probability")+
    theme_light() +
    ggplot2::theme(axis.text.x =
                     ggplot2::element_text(angle = 45,
                                           vjust = 1,
                                           size = 12,
                                           hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 12)) +
    scale_fill_brewer(palette = "Greens") +
    ggplot2::coord_fixed() +
    labs(y = "From", x = "To")
  return(plt)
}
#' ppc_obs_xi_past
#' @param fit cmdstanr fit containing xi_past
#' @param xi_past True data with election_id and candidate id
ppc_plt_xi_past <- function(fit, xi_past = NULL){
  xi_past_hat <- fit$summary("xi_past", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(xi_past_hat) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  xi_past_hat <- xi_past_hat %>%
    mutate(
      election_id = as.integer(str_match(variable, ",([\\d]+)")[,2]),
      bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(q50 > -100) %>%
    mutate(
      bloc_election = paste(bloc_id, election_id, sep = ".")
    ) %>%
    left_join(xi_past) %>%
    arrange(xi_logodds) %>%
    mutate(bloc_election = factor(bloc_election, levels =bloc_election))
  plt1 <- ggplot(xi_past_hat, aes(x = bloc_election)) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(y = "Polling error",
         x = "Interaction (bloc, election)",
         caption = "logodds, 0.5, 0.8, 0.9") +
    coord_flip() +
    geom_point(aes(x = interaction(bloc_id, election_id),
                               y = xi_logodds), color = "red", shape = 2,
                           data = xi_past_hat)

  ## Second plot
  xi_past_hat <- fit$draws("xi_past") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(
      election_id = as.integer(str_match(var, ",([\\d]+)")[,2]),
      bloc_id = as.integer(str_match(var, "([\\d]+),")[,2])
    ) %>%
    filter(val > -100, !is.na(bloc_id)) %>%
    left_join(xi_past) %>%
    group_by(election_id, bloc_id) %>%
    summarize(
      q50 = quantile(xi_logodds - val, 0.5),
      q25 = quantile(xi_logodds - val, 0.25),
      q75 = quantile(xi_logodds - val, 0.75),
      q10 = quantile(xi_logodds - val, 0.1),
      q90 = quantile(xi_logodds - val, 0.9)
    ) %>%
    mutate(bloc_election = paste(bloc_id, election_id, sep = ".")) %>%
    ungroup() %>%
    arrange(q50) %>%
    mutate(bloc_election = factor(bloc_election, levels = bloc_election))

  plt2 <- ggplot(xi_past_hat, aes(x = bloc_election)) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(y = "True polling error - estimate",
         x = "Interaction (bloc, election)",
         caption = "logodds difference, 0.5, 0.8, 0.9") +
    coord_flip()

  ## Join on columns
  plt <- gridExtra::grid.arrange(plt1, plt2, layout_matrix= rbind(c(1, 2),
                                                          c(1, 2)))

  ## Return
  return(plt)
}
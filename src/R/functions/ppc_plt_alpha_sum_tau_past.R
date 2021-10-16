

ppc_plt_alpha_sum_tau_past <- function(fit, data_list){
  tau_pollster <- fit$draws("tau_past") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      poll_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(poll_id)) %>%
    left_join(data.frame(poll_id = 1:sum(data_list$NPolls_Pollster_past),
                         pollster_election_id = data_list$id_P_pollster_past )) %>%
    rename(draws_tau = draws) %>%
    group_by(pollster_election_id, bloc_id, iter) %>%
    summarize(sum_draws_tau = sum(draws_tau)) %>%
    ungroup()

  alpha <- fit$draws("alpha_past") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      pollster_election_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(pollster_election_id)) %>%
    rename(draws_alpha = draws)

  plt <- left_join(tau_pollster, alpha) %>%
    ggplot(aes(x = draws_alpha, y = sum_draws_tau,
               color = as.factor(pollster_election_id))) +
    geom_point(alpha = 0.3) +
    theme_light() +
    facet_wrap(bloc_id ~ .) +
    geom_smooth(method = 'lm', se = 0, size = 0.5) +
    labs(x = "Alpha", y = "Sum tau", color = "Pollster x Election")
  return(plt)
}



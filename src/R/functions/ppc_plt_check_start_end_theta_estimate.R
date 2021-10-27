#' ppc_plt_check_start_end_theta_estimate
#' Function checks whether the connection between blocs and candidates is correct
ppc_plt_check_start_end_theta_estimate <- function(fit, data_list, data_true){
  theta_blocs <- fit$draws("theta_blocs") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter <= 300) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      bloc_id = as.integer(str_match(variable, "([\\d+]+),")[,2]),
      time_id = as.integer(str_match(variable, ",([\\d+]+)")[,2])
    ) %>%
    filter(!is.na(time_id)) %>%
    filter(time_id == max(time_id)) %>%
    group_by(iter, time_id) %>%
    mutate(draws = exp(draws)/sum(exp(draws))) %>%
    ungroup() %>%
    dplyr::select(iter, draws, bloc_id)


  prob_theta_matrix_candidate_start <-
    data.frame(percentage = data_true$prob_theta_matrix_candidates[1, ],
               bloc_id = data_list$id_C_blocs) %>%
    group_by(bloc_id) %>%
    summarize(percentage = sum(percentage))

  prob_theta_matrix_bloc_end <-
    data.frame(percentage_bloc = data_true$prob_theta_matrix_blocs[500, ],
               bloc_id = 1:6)


  plt <- theta_blocs %>%
    left_join(prob_theta_matrix_candidate_start) %>%
    left_join(prob_theta_matrix_bloc_end) %>%
    ggplot(aes(x = draws)) +
    geom_histogram() +
    geom_vline(aes(xintercept = percentage)) +
    geom_vline(aes(xintercept = percentage_bloc), color = "red") +
    facet_wrap(bloc_id ~.)

  return(plt)
}
#' ppc_plt_alpha_sum_tau
#' @param fit cmdstan fit
#' @param data_list Data list

ppc_plt_alpha_sum_tau <- function(fit, data_list){
  tau_pollster <- fit$draws("tau") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      survey_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(survey_id)) %>%
    left_join(data.frame(survey_id = 1:data_list$NSurveys,
                         pollster_id = data_list$id_S_pollster)) %>%
    rename(draws_tau = draws) %>%
    group_by(pollster_id, candidate_id, iter) %>%
    summarize(sum_draws_tau = sum(draws_tau)) %>%
    ungroup()

  alpha <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d]+),")[,2]),
      pollster_id = as.integer(str_match(variable, ",([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable) %>%
    filter(!is.na(pollster_id)) %>%
    rename(draws_alpha = draws)

  plt <- left_join(tau_pollster, alpha) %>%
    ggplot(aes(x = draws_alpha, y = sum_draws_tau, color = as.factor(pollster_id))) +
    geom_point(alpha = 0.3) +
    theme_light() +
    facet_wrap(candidate_id ~ .) +
    geom_smooth(method = 'lm', se = 0, size = 0.5)
  return(plt)
}
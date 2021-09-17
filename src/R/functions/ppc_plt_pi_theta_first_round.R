#' ppc_pi_theta
#' @param fit Cmdstan fit object
#' @return Dataframe with quantiles of th voteshares
ppc_plt_pi_theta_first_round <- function(fit, polls, df_skip, true_data = NULL){
  pi_theta_first_round <- fit$draws("prob_theta") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "ct",
      values_to = "draws"
    ) %>%
    mutate(
      candidate_id = as.integer(str_match(ct, "(\\d+),")[, 2]),
      t_unit = as.integer(str_match(ct, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(t_unit)) %>%
    left_join(df_skip) %>%
    dplyr::select(-ct) %>%
    group_by(candidate_id, time_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  plt <- ggplot(pi_theta_first_round, aes(x = time_id, y = q50)) +
    geom_line() +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
    theme_light() +
    geom_point(data = polls, aes(x = time_id, y = y/n)) +
    facet_wrap(candidate_id ~ .)

  if (!is.null(true_data)){
    plt <- plt +
      geom_line(data = true_data, aes(x = time_id, y = share), linetype = 2,
              color = "red")
  }
  return(plt)
}
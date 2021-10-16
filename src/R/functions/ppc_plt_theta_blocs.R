#' ppc_plt_theta_blocs
#' @param fit cmdstanr fit
#' @param polls past polls
#' @param df_skip the df containing the crosswalk from unit scale to actual time points
#' @param true_date

ppc_plt_theta_blocs <- function(fit, polls, df_skip, true_data = NULL){
  prob_theta_bloc <- fit$draws("prob_theta_blocs") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "bt",
      values_to = "draws"
    ) %>%
    mutate(
      bloc_id = as.integer(str_match(bt, "(\\d+),")[, 2]),
      t_unit = as.integer(str_match(bt, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(t_unit)) %>%
    left_join(df_skip) %>%
    dplyr::select(-bt) %>%
    group_by(bloc_id, time_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  plt <- ggplot(prob_theta_bloc, aes(x = time_id, y = q50)) +
    geom_line() +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
    theme_light() +
    geom_point(data = data_polls$polls_past, aes(x = time_id, y = y/1000)) +
    facet_wrap(bloc_id ~ .)

  if (!is.null(true_data)){
    plt <- plt +
      geom_line(data = data_true$df_blocs, aes(x = time_id, y = share), linetype = 2,
                color = "red")
  }
  return(plt)
}
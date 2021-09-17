#' ppc_plt_alpha
#' @param fit Cmdstan object
#' @param true_alpha If available, dataframe of p, r, alpha (alpha being the true
#' value)
#' @return plt
ppc_plt_alpha <- function(fit, true_alpha = NULL){
  alpha_hat_true <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "cr",
      values_to = "draws"
    ) %>%
    mutate(
      candidate_id = str_match(cr, "(\\d+),")[, 2],
      pollster_id = as.integer(str_match(cr, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(pollster_id)) %>%
    dplyr::select(-cr) %>%
    group_by(candidate_id, pollster_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )

  #' Plot
  plt <- ggplot(alpha_hat_true) +
    geom_point(aes(x = interaction(candidate_id, pollster_id), y = q50)) +
    geom_errorbar(aes(x = interaction(candidate_id, pollster_id),
                      ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(x = interaction(candidate_id, pollster_id),
                      ymin = q10, ymax = q90), size = 0.5,
                  width = 0) +
    theme_light() +
    labs(
      x = "Party - Pollster",
      y = "Polling house deviation",
      caption = "Log-odds scale"
    )
  if (!is.null(true_alpha)){
    alpha_hat_true <- alpha_hat_true %>%
      left_join(true_alpha)
    plt <- plt +
      geom_point(data = alpha_hat_true,
                 aes(x = interaction(candidate_id, pollster_id), y = alpha), color = "red", shape = 3)
  }
  return(plt)
}
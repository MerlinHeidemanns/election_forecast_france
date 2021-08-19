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
      names_to = "pr",
      values_to = "draws"
    ) %>%
    mutate(
      p = str_match(pr, "(\\d+),")[, 2],
      r = as.integer(str_match(pr, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(r)) %>%
    dplyr::select(-pr) %>%
    group_by(p, r) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )

  #' Plot
  plt <- ggplot(alpha_hat_true) +
    geom_point(aes(x = interaction(p, r), y = q50)) +
    geom_errorbar(aes(x = interaction(p, r),
                      ymin = q25, ymax = q75), size = 0.75,
                  width = 0) +
    geom_errorbar(aes(x = interaction(p, r),
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
                 aes(x = interaction(p, r), y = alpha), color = "red", shape = 3)
  }
  return(plt)
}
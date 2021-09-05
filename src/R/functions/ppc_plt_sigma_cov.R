#' ppc_plt_sigma_cov
#' @param fit CmdstanR fit
#' @param transition_matrix If on simulated data, then covariance matrix of random walk.
ppc_plt_sigma_cov <- function(fit, transition_matrix = NULL){
  df <- fit$draws("sigma_cov") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "p",
                 values_to = "val",
                 names_pattern = "(\\d+)") %>%
    filter(!is.na(p)) %>%
    group_by(p) %>%
    summarize(
      q50 = quantile(val, 0.5),
      q25 = quantile(val, 0.25),
      q75 = quantile(val, 0.75),
      q10 = quantile(val, 0.10),
      q90 = quantile(val, 0.90)
    )
  plt <- ggplot(df, aes(x = p, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
    theme_light() +
    labs(x = "Party", y = "Standard deviation random walk",
         caption = "Log-Odds, median, 0.5, 0.8")
  if (!is.null(transition_matrix)){
    sigma <- data$transition_matrix_old %>% diag() %>% sqrt()
    df <- df %>%
      add_column(true = sigma)
    plt <- plt +
      geom_point(data = df, aes(x = p, y = true), color = "red", shape = 2)
  }
  return(plt)
}
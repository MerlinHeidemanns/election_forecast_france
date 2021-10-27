#' ppc_plt_cov_theta_blocs

ppc_plt_cov_theta_blocs <- function(fit, transition_matrix = NULL){

  sigma <- transition_matrix %>% diag() %>% sqrt()

  ## Plot 1
  df <- fit$draws("sigma_cov_blocs") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "bloc_id",
                 values_to = "val",
                 names_pattern = "(\\d+)") %>%
    filter(!is.na(bloc_id)) %>%
    group_by(bloc_id) %>%
    summarize(
      q50 = quantile(val, 0.5),
      q25 = quantile(val, 0.25),
      q75 = quantile(val, 0.75),
      q10 = quantile(val, 0.10),
      q90 = quantile(val, 0.90)
    ) %>%
    add_column(true = sigma)

  plt1 <- ggplot(df, aes(x = bloc_id, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
    theme_light() +
    labs(x = "Party", y = "Standard deviation random walk",
         caption = "Log-Odds, median, 0.5, 0.8") +
    geom_point(data = df, aes(x = bloc_id, y = true), color = "red", shape = 2)

  ## Plot 2
  sigma_cov <- fit$summary("sigma_cov_blocs", ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9)))
  colnames(sigma_cov) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  plt2 <- sigma_cov %>%
    mutate(candidate_id = 2:(n() + 1)) %>%
    add_column(true_value = sigma) %>%
    ggplot(aes(x = true_value, y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.5) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size = 0.25) +
    theme_light() +
    geom_abline(aes(intercept = 0, slope = 1)) +
    coord_cartesian() +
    labs(x = "True", y = "Estimate")

  plt <- gridExtra::grid.arrange(plt1, plt2)

  return(plt)
}
#' ppc_plt_cov_theta
#' @param fit Cmdstan fit
#' @param transition_matrix True transition matrix if running on
#' simulated data
ppc_plt_cov_theta <- function(fit, transition_matrix = NULL){
  cov_theta <- fit$draws("cov_theta") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "pp",
      values_to = "draws"
    ) %>%
    mutate(
      p1 = as.integer(str_match(pp, "(\\d+),")[, 2]),
      p2 = as.integer(str_match(pp, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(p1)) %>%
    group_by(p1, p2) %>%
    summarise(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )

  plt <- ggplot(data = cov_theta, aes(x = interaction(p1, p2))) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(y = "Covariance",
         x = "Party - Party")

  if (!is.null(transition_matrix)){
    cov_theta_true <- transition_matrix %>%
      as.data.frame() %>%
      mutate(p1 = 1:n()) %>%
      pivot_longer(
        c(-p1),
        names_to = "p2",
        values_to = "cov",
        names_prefix = "V"
      ) %>%
      mutate(p2 = as.integer(p2))

    plt <- plt +
      geom_point(data = cov_theta_true,
                 aes(x = interaction(p1, p2),
                     y = cov), color = "red", shape = 3)
  }
  return(plt)
}
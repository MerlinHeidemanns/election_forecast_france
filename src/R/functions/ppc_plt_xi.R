#' ppc_plt_xi
#' @param fit Cmdstan fit
#' @param true_xi data.frame with xi and p
ppc_plt_xi <- function(fit, true_xi = NULL){
  xi_hat <- fit$draws("xi") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "p",
      values_to = "draws",
      names_pattern = "(\\d+)"
    ) %>%
    mutate(p = as.integer(p)) %>%
    filter(!is.na(p)) %>%
    group_by(p) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  plt <- ggplot(data = xi_hat, aes(x = p)) +
    geom_point(aes(y = q50)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.2, width = 0) +
    theme_light() +
    labs(x = "Party",
         y = "Polling error")
  if (!is.null(true_xi)){
    xi_hat <- xi_hat %>%
      left_join(true_xi)
    plt <- plt +
      geom_point(data = xi_hat, aes(y = xi), color = "red", shape = 3)
  }
  return(plt)
}
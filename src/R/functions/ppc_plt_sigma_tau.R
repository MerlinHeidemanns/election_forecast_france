#' ppc_plt_sigma_tau
#' @param fit Cmdstanr object
#' @param sigma_tau_true Defaults to Null, if on simulated
#' value input true value
ppc_plt_sigma_tau <- function(fit, sigma_tau_true = NULL){
  plt <- fit$draws("sigma_tau") %>%
    posterior::as_draws_df() %>%
    ggplot(aes(x = sigma_tau)) +
    geom_histogram(bins = 50) +
    theme_light() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(
      caption = "Log-odds scale"
    )
  if (!is.null(sigma_tau_true)){
    plt <- plt +
      geom_vline(aes(xintercept = sigma_tau_true))
  }
  return(plt)
}
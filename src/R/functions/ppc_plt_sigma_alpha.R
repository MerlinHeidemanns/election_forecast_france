#' ppc_plt_sigma_alpha
#' @param fit Cmdstanr object
#' @param sigma_alpha_true Defaults to Null, if on simulated
#' value input true value
ppc_plt_sigma_alpha <- function(fit, sigma_alpha_true = NULL){
  plt <- fit$draws("sigma_alpha") %>%
    posterior::as_draws_df() %>%
    ggplot(aes(x = sigma_alpha)) +
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
  if (!is.null(sigma_alpha_true)){
    plt <- plt +
      geom_vline(aes(xintercept = sigma_alpha_true))
  }
  return(plt)
}

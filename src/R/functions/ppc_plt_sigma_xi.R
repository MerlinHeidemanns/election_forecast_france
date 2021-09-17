#' ppc_plt_sigma_xi

ppc_plt_sigma_xi <- function(fit, sigma_xi_true = NULL){
  plt <- fit$draws("sigma_xi") %>%
    posterior::as_draws_df() %>%
    ggplot(aes(x = sigma_xi)) +
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
  if (!is.null(sigma_xi_true)){
    plt <- plt +
      geom_vline(aes(xintercept = sigma_xi_true))
  }
  return(plt)
}
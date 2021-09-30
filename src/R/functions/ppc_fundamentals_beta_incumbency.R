#' ppc_fundamentals_beta_incumbency
#' @param fit
#' @param true_beta true beta value
ppc_fundamentals_beta_incumbency <- function(fit, true_beta = NULL){
  plt <- fit$draws("beta") %>%
    posterior::as_draws_df() %>%
    ggplot(aes(x = beta)) +
    geom_histogram(bins = 50) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(x = "Beta (incumbency)")

  if (!is.null(true_beta)){
    plt <- plt +
      geom_vline(aes(xintercept = true_beta),
                 color = "red",
                 linetype = 2)
  }

  return(plt)
}
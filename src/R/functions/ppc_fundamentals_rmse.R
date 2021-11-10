ppc_fundamentals_rmse <- function(fit){
  rmse <- fit$draws("rmse") %>%
    posterior::as_draws_df()
  plt <- ggplot(rmse, aes(x = rmse)) +
    geom_histogram(bins = 100) +
    theme_light() +
    labs(x = "RMSE") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank()) +
    geom_vline(aes(xintercept = mean(rmse)), color = "red", linetype = 2)
  return(plt)
}
#' ppc_fundamentals_beta
#' @param fit
#' @param true_beta dataframe with name, value (true name, true value)
ppc_fundamentals_beta <- function(fit, true_beta){
  beta_fundamentals <- fit$summary("beta_fundamentals", ~ quantile(., c(0.1, 0.25, 0.5, 0.75, 0.9)))
  colnames(beta_fundamentals) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  beta_fundamentals <- beta_fundamentals %>%
    mutate(
      name = true_beta$name[as.integer(str_match(variable, "([\\d]+)")[,2])]
    ) %>%
    dplyr::select(-variable) %>%
    left_join(true_beta)
  plt <- ggplot(data = beta_fundamentals, aes(x = name)) +
    geom_point(aes(y = q50)) +
    geom_point(aes(y = value), color = "red", shape = 2) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0) +
    theme_light() +
    labs(y = "Effect size") +
    theme(axis.title.x = element_blank())
  return(plt)
}
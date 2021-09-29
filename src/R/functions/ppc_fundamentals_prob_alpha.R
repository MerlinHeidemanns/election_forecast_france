#' ppc_fundamentals_prob_alpha
#' @param fit cmdstan fit including prob alpha
#' @param true_data optional if this is run on fake data and true values are known

ppc_fundamentals_prob_alpha <- function(fit, true_data = NULL){
  prob_alpha <- fit$summary("prob_alpha", ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9)))
  colnames(prob_alpha) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  prob_alpha <- prob_alpha %>%
    mutate(
      election_id = as.integer(str_match(variable, ",([\\d]+)")[,2]),
      bloc_id = as.integer(str_match(variable, "([\\d]+),")[,2])
    ) %>%
    dplyr::select(-variable)

  plt <- ggplot(prob_alpha, aes(x = election_id, y = q50)) +
    geom_line() +
    geom_ribbon(aes(ymin = q25, ymax = q75), color = "blue", alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), color = "blue", alpha = 0.5) +
    theme_light() +
    facet_wrap(bloc_id ~ .) +
    labs(x = "Election", y = "Share")
  if (!is.null(true_data)){
    plt <- plt +
      geom_line(data = true_data, aes(x = election_id, y = percentage),
                linetype = 2, color = "red")
  }
  return(plt)
}

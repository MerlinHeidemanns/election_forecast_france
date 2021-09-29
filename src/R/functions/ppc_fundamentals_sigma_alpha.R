#' ppc_fundamentals_sigma_alpha
#' @param fit cmdstan fit
#' @param true_data if sim data then this is the true data
#' @return plt with errorbar plot and quantiles

ppc_fundamentals_sigma_alpha <- function(fit, true_data){

  ## Clean sigma_cov_fundamentals
  sigma_cov_fundamentals <- fit$summary("sigma_cov_fundamentals",
                                        ~ quantile(., c(0.10, 0.25, 0.5, 0.75, 0.9)))
  colnames(sigma_cov_fundamentals) <- c("variable", "q10", "q25", "q50", "q75", "q90")
  sigma_cov_fundamentals <- sigma_cov_fundamentals %>%
    mutate(
      bloc_id = 1 + as.integer(str_match(variable, "([\\d]+)")[,2])
    ) %>%
    dplyr::select(-variable)

  ## Plot
  plt <- ggplot(sigma_cov_fundamentals, aes(x = as.factor(bloc_id), y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.5) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0, size = 0.25) +
    theme_light() +
    labs(x = "Bloc", y = "sigma_alpha (logodds)",
         caption = "Median, 50, 80, true")

  ## If true data supplied, add
  if (!is.null(true_data)){
    plt <- plt +
      geom_point(data = true_data, aes(x = as.factor(bloc_id), y = sigma_alpha),
                 color = "red", shape = 2)
  }

  ## Return plot
  return(plt)
}
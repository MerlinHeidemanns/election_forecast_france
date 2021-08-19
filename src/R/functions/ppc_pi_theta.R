#' ppc_pi_theta
#' @param fit Cmdstan fit object
#' @return Dataframe with quantiles of th voteshares
ppc_pi_theta <- function(fit){
  pi_theta <- fit$draws("pi_theta") %>%
    posterior::as_draws_df() %>%
    pivot_longer(
      everything(),
      names_to = "pt",
      values_to = "draws"
    ) %>%
    mutate(
      p = str_match(pt, "(\\d+),")[, 2],
      t = as.integer(str_match(pt, ",(\\d+)")[, 2])
    ) %>%
    filter(!is.na(t)) %>%
    dplyr::select(-pt) %>%
    group_by(p, t) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  return(pi_theta)
}
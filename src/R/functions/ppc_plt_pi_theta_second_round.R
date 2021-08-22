#' ppc_pi_beta
#' @param fit Cmdstan fit object
ppc_plt_pi_theta_second_round <- function(fit, polls){
  pi_theta_second_round <- fit$draws("pi_theta_second_round") %>%
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
    ) %>%
    filter(p == 1)
  ggplot(pi_theta_second_round, aes(x = t, y = q50)) +
    geom_line() +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.25) +
    geom_point(data = polls, aes(x = t, y = y/n)) +
    theme_light()
}
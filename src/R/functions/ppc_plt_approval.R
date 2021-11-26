ppc_plt_approval <- function(fit, df_approval){
  XApproval <- fit$draws("XApproval") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "draws") %>%
    mutate(
      time_id = as.integer(str_match(var, ",([\\d]+)")[,2]),
      election_id = as.integer(str_match(var, "([\\d]+),")[,2]),
      draws = boot::inv.logit(draws)
    ) %>%
    filter(!is.na(time_id)) %>%
    left_join(
      df_approval %>%
        distinct(election_year, president) %>%
        arrange(election_year) %>%
        mutate(election_id = 1:n())
    ) %>%
    group_by(time_id, election_year, president, election_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  plt <- ggplot(XApproval, aes(x = time_id, y = q50)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "blue", alpha = 0.3) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "blue", alpha = 0.15) +
    geom_point(data = df_approval, aes(x = time_id, y = floor(p_approve * N)/N)) +
    theme_light() +
    facet_wrap(president ~ .)
  return(plt)
}
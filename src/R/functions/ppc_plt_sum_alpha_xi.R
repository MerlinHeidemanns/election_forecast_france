#' ppc_plt_sum_alpha_xi
#' @param fit Cmdstan fit with alpha as polling house deviation
#' and xi as polling error
#' @return Plot of sum of alpha of iteration and polling house against polling
#' error

ppc_plt_sum_alpha_xi <- function(fit){
  ## Extract alpha and sum over polling houses by iteration and party
  alpha_by_p <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "var",
                 values_to = "alpha") %>%
    mutate(p = as.integer(str_match(var, "(\\d+),")[, 2]),
           r = str_match(var, ",(\\d+)")[, 2]) %>%
    filter(!is.na(p)) %>%
    group_by(iter, p) %>%
    summarize(sum_alpha = sum(alpha))

  ## Extract xi
  xi <- fit$draws("xi") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "p",
                 values_to = "xi",
                 names_pattern = "(\\d+)") %>%
    mutate(p = as.integer(p)) %>%
    filter(!is.na(p))

  ## Join alpha and xi
  alpha_xi <- left_join(alpha_by_p, xi, by = c("p" = "p", "iter" = "iter"))

  ## Plt
  ggplot(alpha_xi, aes(x = sum_alpha, y = xi)) +
    geom_point(alpha = 0.3) +
    facet_wrap(p ~ .) +
    theme_light() +
    labs(x = "Sum of polling house deviation",
         y = "Polling error")
}
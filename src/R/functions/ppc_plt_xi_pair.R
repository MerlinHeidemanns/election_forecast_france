#' ppc_plt_xi_pair
#' @param fit cmdnstan fit
#' @param NIter Number of iterations to use
#'
ppc_plt_xi_pair <- function(fit, NIter = 100){
  xi_hat <- fit$draws("xi") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter <= NIter) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws_a") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d+])")[,2])
    ) %>%
    filter(draws_a > - 100,
           !is.na(candidate_id)) %>%
    dplyr::select(-variable)

  full_join(xi_hat, xi_hat %>%
              rename(draws_b = draws_a), by = "iter") %>%
    filter(candidate_id.x != candidate_id.y) %>%
    ggplot(aes(x = draws_a, y = draws_b)) +
    geom_point(alpha = 0.3) +
    facet_grid(candidate_id.x ~ candidate_id.y, scales = "free") +
    theme_light() +
    labs(x = "Polling error", y = "Polling error")
}




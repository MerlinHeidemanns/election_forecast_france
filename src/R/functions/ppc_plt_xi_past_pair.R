#' ppc_plt_xi_past_pair
#' @param fit cmdnstan fit
#' @param past_election Which past election
#' @param NIter Number of iterations to use
#'
ppc_plt_xi_past_pair <- function(fit, past_election = 1, NIter = 100){
  xi_past_hat <- fit$draws("xi_past") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter <= NIter) %>%
    pivot_longer(c(-iter),
                 names_to = "variable",
                 values_to = "draws_a") %>%
    mutate(
      candidate_id = as.integer(str_match(variable, "([\\d+]),")[,2]),
      election_id = as.integer(str_match(variable, ",([\\d+])")[,2])
    ) %>%
    filter(election_id == past_election, draws_a > - 100) %>%
    dplyr::select(-variable, -election_id)

  full_join(xi_past_hat, xi_past_hat %>%
              rename(draws_b = draws_a), by = "iter") %>%
    filter(candidate_id.x != candidate_id.y) %>%
    ggplot(aes(x = draws_a, y = draws_b)) +
    geom_point(alpha = 0.3) +
    facet_grid(candidate_id.x ~ candidate_id.y, scales = "free") +
    theme_light() +
    labs(x = "Polling error", y = "Polling error")
}
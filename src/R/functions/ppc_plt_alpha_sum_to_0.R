#' ppc_plt_alpha_sum_to_0
#' @param fit Cmdstan fit with alpha as P, R
ppc_plt_alpha_sum_to_0 <- function(fit){
  alpha_by_r <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "var",
                 values_to = "alpha") %>%
    mutate(p = str_match(var, "(\\d+),")[, 2],
           r = str_match(var, ",(\\d+)")[, 2]) %>%
    filter(!is.na(r)) %>%
    group_by(iter, r) %>%
    summarize(sum_alpha = sum(alpha)) %>%
    group_by(r) %>%
    summarize(mean = mean(sum_alpha),
              sd = sd(sum_alpha))
  alpha_by_p <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(c(-iter),
                 names_to = "var",
                 values_to = "alpha") %>%
    mutate(p = str_match(var, "(\\d+),")[, 2],
           r = str_match(var, ",(\\d+)")[, 2]) %>%
    filter(!is.na(p)) %>%
    group_by(iter, p) %>%
    summarize(sum_alpha = sum(alpha)) %>%
    group_by(p) %>%
    summarize(mean = mean(sum_alpha),
              sd = sd(sum_alpha))
  return(list(alpha_by_r = alpha_by_r,
              alpha_by_p = alpha_by_p))
}
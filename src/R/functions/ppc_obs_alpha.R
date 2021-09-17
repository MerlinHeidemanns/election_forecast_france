#' ppc_obs_alpha
#' @param fit cmdstanr fit
#' @param supvec_names vector with candidate names in correct order
#' @param supvec_pollster vector with pollster names in correct order
ppc_obs_alpha <- function(fit, supvec_names, supvec_pollster){
  alpha <- fit$draws("invlogit_alpha") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "cr",
                 values_to = "val") %>%
    mutate(
      candidate = supvec_names[as.integer(str_match(cr, "(\\d+),")[,2])],
      pollster = supvec_pollster[as.integer(str_match(cr, ",(\\d+)")[,2])]
    ) %>%
    dplyr::select(-cr) %>%
    filter(!is.na(candidate)) %>%
    group_by(candidate, pollster) %>%
    mutate(val = val * 100) %>%
    summarize(
      q50 = quantile(val, 0.5),
      q25 = quantile(val, 0.25),
      q75 = quantile(val, 0.75),
      q10 = quantile(val, 0.10),
      q90 = quantile(val, 0.90)
    )
  plt <- ggplot(alpha, aes(x = candidate, y = q50, color = pollster)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.5, width = 0,
                  position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.25, width = 0,
                  position = position_dodge(width = 0.5)) +
    coord_flip() +
    theme_light() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank()) +
    labs(y = "Pollster deviations (percentage points)")
  return(plt)
}

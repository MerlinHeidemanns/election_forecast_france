#' ppc_plt_y_yhat
#' @param fit cmdstanr fit
#' @param data_list list containing y and NPolls_Candidates
ppc_plt_y_yhat <- function(fit, data_list){
  yhat <- fit$draws("yhat") %>%
    posterior::as_draws_df() %>%
    pivot_longer(everything(),
                 names_to = "candidate_id_question_id",
                 values_to = "draws",
                 names_pattern = "(\\d+)") %>%
    filter(!is.na(candidate_id_question_id)) %>%
    mutate(candidate_id_question_id = as.integer(candidate_id_question_id)) %>%
    group_by(candidate_id_question_id) %>%
    summarize(
      q50 = quantile(draws, 0.5),
      q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75),
      q10 = quantile(draws, 0.10),
      q90 = quantile(draws, 0.90)
    )
  y <- data.frame(y = data_list$y,
                  candidate_id_question_id = 1:data_list$NPolls_Candidates)
  plt <- left_join(y, yhat) %>%
    ggplot(aes(x = boot::inv.logit(y), y = q50)) +
    geom_point() +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0) +
    geom_abline(aes(intercept = 0, slope = 1), linetype = 2, color = "red") +
    labs(x = "y", y = "yhat") +
    theme_light()
  return(plt)
}
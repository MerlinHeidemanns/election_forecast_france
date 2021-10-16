#' functionality_check_tau
#' @param fit
#' @param data_list
#' @return The function checks whether or not the sum to zero within pollsters for tau works
#' as intended.

functionality_check_tau <- function(fit, data_list){
  tau <- fit$draws("tau") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter == 1) %>%
    select(-iter) %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(
      candidate_id = str_match(var, "([\\d]+),")[,2],
      order_id = as.integer(str_match(var, ",([\\d]+)")[,2])
    ) %>%
    filter(!is.na(order_id)) %>%
    select(-var) %>%
    pivot_wider(id_cols = order_id,
                names_from = candidate_id,
                values_from = val) %>%
    mutate(pollster_id = data_list$id_S_pollster,
           survey_id = 1:data_list$NSurveys)

  out <- matrix(NA, nrow = max(tau$pollster_id), ncol = 2)
  for (jj in 1:max(tau$pollster_id)){
    tau_mat <- tau %>%
      filter(pollster_id == 2) %>%
      select(-pollster_id, -survey_id, -order_id) %>%
      as.matrix()
    out[jj, 1] <- sum(round(apply(tau_mat, 1, sum), 5))
    out[jj, 2] <- sum(round(apply(tau_mat, 2, sum), 5))
  }
  eval <- all(out == 0)
  if (eval){
    print("tau sum to zero within pollster: WORKS")
  } else {
    print("tau sum to zero within pollster: DOES NOT WORK")
  }
}

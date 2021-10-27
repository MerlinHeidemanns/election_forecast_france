#' functionality_check_tau_past

functionality_check_tau_past <- function(fit, data_list){
  tau <- fit$draws("tau_past") %>%
    posterior::as_draws_df() %>%
    mutate(iter = 1:n()) %>%
    filter(iter == 1) %>%
    select(-iter) %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(
      bloc_id = str_match(var, "([\\d]+),")[,2],
      order_id = as.integer(str_match(var, ",([\\d]+)")[,2])
    ) %>%
    filter(!is.na(order_id)) %>%
    select(-var) %>%
    pivot_wider(id_cols = order_id,
                names_from = bloc_id,
                values_from = val) %>%
    mutate(pollster_id = data_list$id_P_pollster_past,
           survey_id = 1:sum(data_list$NPolls_Pollster_past))

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
    print("tau sum to zero within pollster past: WORKS")
  } else {
    print("tau sum to zero within pollster past: DOES NOT WORK")
  }
}

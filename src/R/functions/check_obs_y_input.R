check_obs_y_input <- function(p_id, p_1r_included, df, P_N_combinations){
  input_df <- lapply(1:length(p_id), function(j){
    out <- data.frame(
      question_id = j,
      y = y_1r[j, 1:P_N_combinations[p_id[j]]],
      candidate_id = p_1r_included[p_id[j], 1:P_N_combinations[p_id[j]]]
    )
    return(out)
  }) %>%
    do.call("bind_rows", .)
  input_df <- input_df %>%
    group_by(question_id) %>%
    mutate(percentage = y / sum(y)) %>%
    left_join(
      df %>%
        distinct(candidate, candidate_id),
      by = c("candidate_id")
    ) %>%
    mutate(n = n()) %>%
    filter(n > 2) %>%
    group_by(candidate) %>%
    summarize(
      q10 = quantile(percentage, 0.1),
      q25 = quantile(percentage, 0.25),
      q50 = quantile(percentage, 0.50),
      q75 = quantile(percentage, 0.75),
      q90 = quantile(percentage, 0.90),
      min = min(percentage),
      max = max(percentage)
    )
  return(input_df)
}

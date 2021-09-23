#' obs_create_variable_inclusion_input
#' @param df df including survey_id, candidate_id, question_id
obs_create_variable_inclusion_input <- function(df){
  source("src/R/functions/create_y_first_round.R")

  ## How many parties did the poll ask for?
  #' Group by id and find number of observation as number of parties
  includes_abstention <- df %>%
    distinct(question_id, includes_abstention) %>%
    arrange(question_id) %>%
    pull(includes_abstention)
  P_first_round <- df %>%
    group_by(question_id) %>%
    summarize(n = n()) %>%
    pull(n)
  P_first_round <- P_first_round + c(1, 0)[includes_abstention + 1]

  ## How many combinations exist?
  #' Sort the df by number of parties
  #' select id and party,
  #' long to wide to get combinations
  #' Pass combinations to list
  #' Call unique
  df_wide <- df %>%
    group_by(question_id) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    arrange(-n, question_id, candidate_id) %>%
    dplyr::select(question_id, candidate_id) %>%
    mutate(val = 1) %>%
    pivot_wider(id_cols = question_id,
                names_from = candidate_id,
                names_prefix = "candidate_id",
                values_from = val,
                names_sort = TRUE,
                values_fill = 0) %>%
    mutate(candidate_id1 = 1) %>%
    as.matrix()
  combinations <- list()
  for (ii in 1:nrow(df_wide)){
    tmp <- seq(1, ncol(df_wide) - 1)[1 == c(df_wide[ii, 2:ncol(df_wide)])]
    tmp <- unique(c(1, tmp))
    combinations[[ii]] <- tmp
  }
  combinations <- combinations %>% unique()
  N_combinations <- length(combinations)

  ## Which poll belongs to which combinations? // p_id
  #' Create local sequence
  #' Find position in list of combinations
  df_wide <- cbind(df_wide, NA)
  for (ii in 1:nrow(df_wide)){
    local_sequence <- seq(1, ncol(df_wide) - 2)[1 == c(df_wide[ii, 2:(ncol(df_wide) - 1)])]
    for (jj in 1:length(combinations)){
      if (paste0(combinations[[jj]], collapse = "") == paste0(local_sequence, collapse = "")){
        df_wide[ii, ncol(df_wide)] <- jj
      }
    }
  }
  df_wide <- df_wide %>%
    as.data.frame() %>%
    arrange(question_id)
  combination_id <- df_wide[,ncol(df_wide)][1:max(df$question_id)]
  ## How long is each combination?
  P_N_combinations <- lapply(combinations, length) %>% unlist()

  ## What are the combinations
  #' Create matrizes
  #' Fill by row with combinations
  P <- df %>% pull(candidate_id) %>% max()
  P_max_included <- lapply(combinations, length) %>% unlist() %>% max()
  P_min_included <- lapply(combinations, length) %>% unlist() %>% min()
  p_first_round_included <- matrix(-99,
                                   nrow = length(combinations),
                                   ncol = P)
  p_first_round_excluded <- matrix(-99,
                                   nrow = length(combinations),
                                   ncol = P)
  for (ii in 1:length(combinations)){
    tmp_included <- combinations[[ii]]
    tmp_excluded <- seq(1:P)[!seq(1:P) %in% tmp_included]
    p_first_round_included[ii, 1:length(tmp_included)] <- tmp_included
    if (length(tmp_excluded) > 0){
      p_first_round_excluded[ii, 1:length(tmp_excluded)] <- tmp_excluded
    }
  }
  #' Return output
  return(list(
    P_first_round = P_first_round,
    N_combinations = N_combinations,
    P_N_combinations = P_N_combinations,
    p_first_round_excluded = p_first_round_excluded,
    p_first_round_included = p_first_round_included,
    p_id = combination_id,
    y_first_round = create_y_first_round(df),
    abstention_omitted = c(1, 0)[includes_abstention + 1]
  ))
}
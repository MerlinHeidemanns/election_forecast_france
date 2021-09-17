#' create_variable_inclusion_input
#' @param df df including survey id, p,
create_variable_inclusion_input <- function(df){
  ## How many parties did the poll ask for?
  #' Group by id and find number of observation as number of parties
  NCandidates_Poll <- df %>%
    group_by(question_id) %>%
    summarize(n = n()) %>%
    pull(n)

  ## How combinations exist?
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
                values_fill = 0) %>%
    as.matrix()
  combinations <- list()
  for (ii in 1:nrow(df_wide)){
    combinations[[ii]] <- seq(1, ncol(df_wide) - 1)[1 == c(df_wide[ii, 2:ncol(df_wide)])]
  }
  combinations <- combinations %>% unique()
  NCombinations <- length(combinations)

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
  NCandidate_Combinations <- lapply(combinations, length) %>% unlist()

  ## What are the combinations
  #' Create matrizes
  #' Fill by row with combinations
  NCandidates <- df %>% pull(candidate_id) %>% unique() %>% length()
  NCandidates_included_max <- lapply(combinations, length) %>% unlist() %>% max()
  NCandidates_included_max <- lapply(combinations, length) %>% unlist() %>% min()
  candidates_included <- matrix(-99,
                                   nrow = length(combinations),
                                   ncol = NCandidates)
  candidates_excluded <- matrix(-99,
                                   nrow = length(combinations),
                                   ncol = NCandidates)
  for (ii in 1:length(combinations)){
    tmp_included <- combinations[[ii]]
    tmp_excluded <- seq(1:NCandidates)[!seq(1:NCandidates) %in% tmp_included]
    candidates_included[ii, 1:length(tmp_included)] <- tmp_included
    if (length(tmp_excluded) > 0){
      candidates_excluded[ii, 1:length(tmp_excluded)] <- tmp_excluded
    }
  }
  #' Return output
  return(list(
    NCandidates_Poll = NCandidates_Poll,
    NCombinations = NCombinations,
    NCandidate_Combinations = NCandidate_Combinations,
    candidates_excluded = candidates_excluded,
    candidates_included = candidates_included,
    combination_id = combination_id,
    y = create_y_first_round(df)
  ))
}
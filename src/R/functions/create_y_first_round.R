#' create_y_first_round
#' @param df Dataframe with first round polls with id and sorted by id, p
create_y_first_round <- function(df){
  y_first_round <- matrix(-99,
                          nrow = max(df$question_id),
                          ncol = max(df$candidate_id))
  question_id_vector <- unique(df$question_id)
  for (ii in 1:length(question_id_vector)){
    candidates <- df %>%
      filter(question_id == question_id_vector[ii]) %>%
      pull(candidate_id)
    if (1 %in% candidates){
      tmp <- df %>%
        filter(question_id == question_id_vector[ii]) %>%
        arrange(candidate_id) %>%
        pull(y)
      y_first_round[ii, 1:length(tmp)] <- tmp
    } else {
      tmp <- df %>%
        filter(question_id == question_id_vector[ii]) %>%
        arrange(candidate_id) %>%
        pull(y)
      y_first_round[ii, 2:(length(tmp) + 1)] <- tmp
    }
  }
  return(y_first_round)
}
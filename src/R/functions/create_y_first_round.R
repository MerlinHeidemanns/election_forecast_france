#' create_y_first_round
#' @param df Dataframe with first round polls with id and sorted by id, p
create_y_first_round <- function(df){
  y_first_round <- matrix(-99,
                          nrow = max(df$question_id),
                          ncol = max(df$candidate_id))
  for (ii in 1:max(df$question_id)){
    candidates <- df %>%
      filter(question_id == ii) %>%
      pull(candidate_id)
    if (1 %in% candidates){
      tmp <- df %>%
        filter(question_id == ii) %>%
        arrange(candidate_id) %>%
        pull(y)
      y_first_round[ii, 1:length(tmp)] <- tmp
    } else {
      tmp <- df %>%
        filter(question_id == ii) %>%
        arrange(candidate_id) %>%
        pull(y)
      y_first_round[ii, 2:(length(tmp) + 1)] <- tmp
    }
  }
  return(y_first_round)
}
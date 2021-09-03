#' create_y_first_round
#' @param df Dataframe with first round polls with id and sorted by id, p
create_y_first_round <- function(df){
  y_first_round <- matrix(-99,
                          nrow = max(df$question_id),
                          ncol = 6)
  for (ii in 1:max(df$question_id)){
    tmp <- df %>%
      filter(question_id == ii) %>%
      pull(y)
    y_first_round[ii, 1:length(tmp)] <- tmp
  }
  return(y_first_round)
}
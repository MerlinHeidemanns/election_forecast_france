#' data_list_check_y_1r
#' @param data_list input to model
#' Assesses whether the model could grab the wrong element from the outcome
#' matrix
data_list_check_y_1r <- function(data_list){
  N_1r <- data_list$N_1r
  p_id <- data_list$p_id
  P_N_combinations <- data_list$P_N_combinations
  y <- data_list$y_1r

  for (ii in 1:data_list$N_1r){
    if (any(y[1:P_N_combinations[p_id[ii]],ii] < 0)){
      stop("One of the y's is smaller than 0.")
    }
  }
}
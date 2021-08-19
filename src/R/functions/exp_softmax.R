#' exp_softmax
#' @param x Vector
exp_softmax <- function(x){
  exp(x)/sum(exp(x))
}

#' exp_softmax_by_row
#' #' @param x Matrix
exp_softmax_by_row <- function(x){
  for (j in 1:nrow(x)){
    x[j, ] <- exp(x[j, ])/sum(exp(x[j, ]))
  }
  return(x)
}

#' demean_by_row
#' #' @param x Matrix
demean_by_row <- function(x){
  for (j in 1:nrow(x)){
    x[j, ] <- x[j, ] - mean(x[j, ])
  }
  return(x)
}
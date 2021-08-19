## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
sim_random_walk <- function(P, T, rho, sigma){
  ## Simulate a multinomial random walk
  #' Set N of parties and time points;
  #' Initial shares and log odds transformation
  #' Transition matrix for the process
  pi <- runif(P, 0, 1); pi <- pi/sum(pi)
  eta <- c(log(pi[1:length(pi) - 1]/pi[length(pi)]), 0)
  transition_matrix <- matrix(sigma^2 * rho, nrow = 4, ncol = 4)
  diag(transition_matrix) <- sigma^2

  ## Random walk
  #' Setup matrizes
  #' Random walk
  #' Transformation back into shares
  eta_matrix <- matrix(NA, nrow = T, ncol = P)
  pi_matrix <- matrix(NA, nrow = T, ncol = P)
  eta_matrix[1,] <- eta
  for (t in 2:T){
    eta_matrix[t, ] <- eta_matrix[t - 1, ] + MASS::mvrnorm(1, rep(0, P), transition_matrix)
  }
  for (t in 1:T){
    pi_matrix[t, ] <- exp(eta_matrix[t, ])/sum(exp(eta_matrix[t, ]))
  }
  ## Plot
  data <- pi_matrix %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "p",
      values_to = "share",
      names_prefix = "V"
    )
  return(list(df = data,
              pi_matrix = pi_matrix,
              eta_matrix = eta_matrix,
              transition_matrix = transition_matrix))
}




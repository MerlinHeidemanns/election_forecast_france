## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
sim_random_walk <- function(P, T, T_prior = 0, rho, sigma){
  ## Simulate a multinomial random walk
  #' Set N of parties and time points;
  #' Initial shares and log odds transformation
  #' Transition matrix for the process
  pi <- runif(P, 0.3, 1); pi <- pi/sum(pi)
  eta <- c(log(pi[1:length(pi) - 1]/pi[length(pi)]), 0)
  transition_matrix <- matrix(sigma^2 * rho, nrow = P, ncol = P)
  diag(transition_matrix) <- sigma^2

  ## Random walk
  #' Setup matrizes
  #' Transformation back into shares
  eta_matrix <- matrix(NA, nrow = T, ncol = P)
  pi_matrix <- matrix(NA, nrow = T, ncol = P)

  #' Determine start of election season
  eta_matrix[1,] <- eta + MASS::mvrnorm(1, rep(0, P), T_prior * transition_matrix)

  #' Random walk
  for (t in 2:T){
    eta_matrix[t, ] <- eta_matrix[t - 1, ] + MASS::mvrnorm(1, rep(0, P), transition_matrix)
  }
  for (t in 1:T){
    pi_matrix[t, ] <- exp(eta_matrix[t, ])/sum(exp(eta_matrix[t, ]))
  }

  #' Create collapsed data frame
  eta_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  pi_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  for (t in 1:T){
    eta_matrix_coll[t, ] <- eta_matrix[t, 1:2] +
      transition_matrix[1:2, 3:P] %*% solve(transition_matrix[3:P, 3:P]) %*% (rep(-10, P - 2) - eta_matrix[t, 3:P])
  }
  for (t in 1:T){
    pi_matrix_coll[t, ] <- exp(eta_matrix_coll[t, ])/sum(exp(eta_matrix_coll[t, ]))
  }

  ## Create data frame
  data <- pi_matrix %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "p",
      values_to = "share",
      names_prefix = "V"
    )
  data_coll <- pi_matrix_coll %>%
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
              transition_matrix = transition_matrix,
              eta_start = eta,
              pi_start = pi,
              df_coll = data_coll))
}




## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
NCandidate = 6
T = 20
T_prior = 10
rho <- 0.5
sigma <- 0.1
sim_random_walk <- function(N_past_election,
                            NCandidate,
                            T,
                            T_prior = 0,
                            rho,
                            sigma,
                            K_VAR){

  P_past_elections <- rpois(N_past_election, 4)
  while (min(P_past_elections) <= 2){
    P_past_elections[P_past_elections <= 2] <- rpois(sum(P_past_elections <= 2), 5)
  }
  max_P_past_elections <- max(P_past_elections)
  pi_past <- matrix(0, nrow = N_past_election, ncol = max_P_past_elections)
  for (ii in 1:N_past_election){
    tmp <- runif(P_past_elections[ii], 0.3, 1)
    tmp <- tmp/sum(tmp)
    pi_past[ii, 1:P_past_elections[ii]] <- tmp
  }


  ## Simulate a multinomial random walk
  #' Set N of parties and time points;
  #' Initial shares and log odds transformation
  #' Transition matrix for the process
  pi <- runif(NCandidate, 0.3, 1)
  pi <- pi/sum(pi)
  eta <- c(log(pi[1:length(pi) - 1]/pi[length(pi)]), 0)
  trans_matr <- matrix(sigma^2 * rho,
                                  nrow = NCandidate,
                                  ncol = NCandidate)
  diag(trans_matr) <- sigma^2

  ## Random walk / Autoregressive process
  #' Draw parameters
  beta <- matrix(0,
         nrow = NCandidate,
         ncol = NCandidate)
  diag(beta) <- 1

  #' Setup matrizes
  #' Transformation back into shares
  eta_matrix <- matrix(NA, nrow = T, ncol = NCandidate)
  pi_matrix <- matrix(NA, nrow = T, ncol = NCandidate)

  #' Determine start of election season
  #' Project forward
  #' Set disappearing parties to negative value
  eta_matrix[1, ] = eta
  #' Random walk
  for (t in 2:T){
    eta_matrix[t, ] <- eta_matrix[t - 1, ] %*% beta + MASS::mvrnorm(1, rep(0, NCandidate), trans_matr)
  }
  for (t in 1:T){
    pi_matrix[t, ] <- exp(eta_matrix[t, ])/sum(exp(eta_matrix[t, ]))
  }

  #' Create collapsed data frame
  eta_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  pi_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
  for (t in 1:T){
    eta_matrix_coll[t, ] <- eta_matrix[t, 1:2] +
      trans_matr[1:2, 3:NCandidate] %*%
      solve(trans_matr[3:NCandidate, 3:NCandidate]) %*%
      (rep(-10, NCandidate - 2) - eta_matrix[t, 3:NCandidate])
  }
  for (t in 1:T){
    pi_matrix_coll[t, ] <- exp(eta_matrix_coll[t, ])/sum(exp(eta_matrix_coll[t, ]))
  }

  ## Create data frames
  #' true data current all parties
  data <- pi_matrix %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "candidate_id",
      values_to = "share",
      names_prefix = "V"
    )
  #' true data current run off
  data_coll <- pi_matrix_coll %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(
      c(-t),
      names_to = "candidate_id",
      values_to = "share",
      names_prefix = "V"
    )
  #' past results
  pi_past_dataframe <- pi_past %>%
    as.data.frame() %>%
    mutate(t = 1:n()) %>%
    pivot_longer(c(-t),
                 names_to = "candidate_id",
                 values_to = "share",
                 names_prefix = "V") %>%
    mutate(candidate_id = as.integer(candidate_id)) %>%
    filter(share > 0)
  return(list(
              NCandidate = NCandidate,
              df = data,
              pi_matrix = pi_matrix,
              eta_matrix = eta_matrix,
              transition_matrix = trans_matr,
              eta_start = eta,
              pi_start = pi,
              df_coll = data_coll,
              pi_past = pi_past,
              pi_past_dataframe = pi_past_dataframe))
}




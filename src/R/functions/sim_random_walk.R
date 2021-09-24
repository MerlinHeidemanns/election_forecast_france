## Functions
#' Simulate a multivariate random walk
#' @param P Number of parties
#' @param T Number of time points
#' @param rho correlation of transition matrix
#' @param sigma standard deviation of transition matrix
P_both = 4
P_past = 2
P_new = 3
T = 20
T_prior = 10
rho <- 0.5
sigma <- 0.1
sim_random_walk <- function(NElections_past,
                            NCandidates,
                            NTime,
                            rho,
                            sigma,
                            K_VAR){
  NCandidates <- NCandidates + 1

  NCandidates_Elections_past <- rpois(NElections_past, 5)
  while (min(NCandidates_Elections_past) <= 2){
    NCandidates_Elections_past[NCandidates_Elections_past <= 2] <- rpois(sum(NCandidates_Elections_past <= 2), 5)
  }
  NCandidates_Elections_past_max <- max(NCandidates_Elections_past)
  prob_theta_past <- matrix(0, nrow = NElections_past, ncol = NCandidates_Elections_past_max)
  for (ii in 1:NElections_past){
    tmp <- runif(NCandidates_Elections_past[ii], 0.3, 1)
    tmp <- tmp/sum(tmp)
    tmp[1] <- runif(1, 0.15, 0.25)
    tmp <- tmp/sum(tmp)
    prob_theta_past[ii, 1:NCandidates_Elections_past[ii]] <- tmp
  }


  ## Simulate a multinomial random walk
  #' Set N of parties and time points;
  #' Initial shares and log odds transformation
  #' Transition matrix for the process
  prob_theta <- runif(NCandidates, 0.3, 1)
  prob_theta <- prob_theta/sum(prob_theta)
  theta <- c(log(prob_theta[1:length(prob_theta) - 1]/prob_theta[length(prob_theta)]), 0)
  trans_matrix_rw <- matrix(sigma^2 * rho,
                                  nrow = NCandidates - 1,
                                  ncol = NCandidates - 1)
  diag(trans_matrix_rw) <- sigma^2

  ## Random walk / Autoregressive process
  #' Draw parameters
  beta <- matrix(0,
         nrow = NCandidates - 1,
         ncol = NCandidates - 1)
  diag(beta) <- 1

  #' Setup matrizes
  #' Transformation back into shares
  theta_matrix <- matrix(NA, nrow = NTime, ncol = NCandidates)
  prob_theta_matrix <- matrix(NA, nrow = NTime, ncol = NCandidates)

  #' Determine start of election season
  #' Project forward
  #' Set disappearing parties to negative value
  theta_matrix[1, ] = theta
  theta_matrix[, NCandidates] = 0
  #' Random walk
  for (t in 2:NTime){
    theta_matrix[t, 1:NCandidates - 1] <- theta_matrix[t - 1, 1:NCandidates - 1] %*% beta +
      MASS::mvrnorm(1, rep(0, NCandidates - 1), trans_matrix_rw)
  }
  for (t in 1:NTime){
    prob_theta_matrix[t, ] <- exp(theta_matrix[t, ])/sum(exp(theta_matrix[t, ]))
  }

  #' -- Create collapsed data frame
  #' * Create transition matrix
  trans_matrix_pref <- matrix(0, nrow = NCandidates, ncol = NCandidates)
  for (jj in 1:NCandidates){
    included <- seq(1, NCandidates)
    included <- included[!included %in% jj]
    trans_matrix_pref[included, jj] <- - DirichletReg::rdirichlet(1, rep(5, NCandidates - 1))
  }
  diag(trans_matrix_pref) <- 1

  prob_theta_matrix_coll <- matrix(NA, nrow = NTime, ncol = 2)
  for (t in 1:NTime){
    prob_theta_matrix_coll[t, ] <- prob_theta_matrix[t, 1:2] +
      trans_matrix_pref[1:2, 3:NCandidates] %*%
      solve(trans_matrix_pref[3:NCandidates, 3:NCandidates]) %*%
      (rep(0, NCandidates - 2) - prob_theta_matrix[t, 3:NCandidates])
  }

  ## Create data frames
  #' true data current all parties
  data <- prob_theta_matrix %>%
    as.data.frame() %>%
    mutate(time_id = 1:n()) %>%
    pivot_longer(
      c(-time_id),
      names_to = "candidate_id",
      values_to = "share",
      names_prefix = "V"
    )
  #' true data current run off
  data_coll <- prob_theta_matrix_coll %>%
    as.data.frame() %>%
    mutate(time_id = 1:n()) %>%
    pivot_longer(
      c(-time_id),
      names_to = "candidate_id",
      values_to = "share",
      names_prefix = "V"
    ) %>%
    mutate(candidate_id = as.integer(candidate_id))
  #' past results
  prob_theta_past_df <- prob_theta_past %>%
    as.data.frame() %>%
    mutate(time_id = 1:n()) %>%
    pivot_longer(c(-time_id),
                 names_to = "candidate_id",
                 values_to = "share",
                 names_prefix = "V") %>%
    mutate(candidate_id = as.integer(candidate_id)) %>%
    filter(share > 0)
  return(list(
              NCandidates = NCandidates,
              df = data,
              prob_theta_matrix = prob_theta_matrix,
              theta_matrix = theta_matrix,
              transition_matrix_random_walk = trans_matrix_rw,
              transition_matrix_preferences = trans_matrix_pref,
              theta_start = theta,
              prob_theta_start = prob_theta,
              df_coll = data_coll,
              prob_theta_past = prob_theta_past,
              prob_theta_past_df = prob_theta_past_df))
}




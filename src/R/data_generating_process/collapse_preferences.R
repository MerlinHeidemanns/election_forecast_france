

N <- 4
Mu <- seq(1, N, 1)
sigma <- seq(1, N)
Sigma <- matrix(0.5, nrow = N, ncol = N)
for (i in 1:N){
  for (j in 1:N){
    if (i == j){
      Sigma[i, j] = sigma[i]^2
    } else {
      Sigma[i, j] = sigma[i] * sigma[j] * 0.5
    }
  }
}

Mu[1:2] + Sigma[3:4, 1:2] %*% Sigma[3:4, 3:4] %*% (c(0, 0) - Mu[3:4])


P <- 4
T <- 20
T_prior <- 1
rho <- -0.5
sigma <- 0.1
## Simulate a multinomial random walk
#' Set N of parties and time points;
#' Initial shares and log odds transformation
#' Transition matrix for the process
pi <- runif(P, 0.3, 1); pi <- pi/sum(pi)
eta <- c(log(pi[1:length(pi) - 1]/pi[length(pi)]), 0)
transition_matrix <- matrix(sigma^2 * rho, nrow = 4, ncol = 4)
diag(transition_matrix) <- sigma^2
transition_matrix <- lqmm::make.positive.definite(transition_matrix)

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

eta_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
pi_matrix_coll <- matrix(NA, nrow = T, ncol = 2)
for (t in 1:T){
  eta_matrix_coll[t, ] <- eta_matrix[t, 1:2] +
    transition_matrix[3:4, 1:2] %*% Sigma[3:4, 3:4] %*% (c(-1000, -1000) - eta_matrix[t, 3:4])
}
for (t in 1:T){
  pi_matrix_coll[t, ] <- exp(eta_matrix_coll[t, ])/sum(exp(eta_matrix_coll[t, ]))
}

pi_matrix %>%
  as.data.frame() %>%
  mutate(t = 1:n()) %>%
  pivot_longer(
    c(-t),
    names_to = "p",
    values_to = "share",
    names_prefix = "V"
  ) %>%
  ggplot(aes(x = t, y = share, color = p)) +
    geom_line()

pi_matrix_coll %>%
  as.data.frame() %>%
  mutate(t = 1:n()) %>%
  pivot_longer(
    c(-t),
    names_to = "p",
    values_to = "share",
    names_prefix = "V"
  ) %>%
  ggplot(aes(x = t, y = share, color = p)) +
  geom_line()


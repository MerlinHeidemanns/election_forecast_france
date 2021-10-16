
source("src/R/functions/exp_softmax.R")

## Correlation matrix
A <- matrix(0.5, nrow = N, ncol = N)
diag(A) <- 1
## Standard deviations on the odds_scale
sigma <- c(1.1, 1.1, 1.1, 1.1)
## Covariance matrix on the odds scale
diag_one <- matrix(0, nrow = N, ncol = N)
diag(diag_one) <- 1
ones_34 <- rbind(c(1,0,0),
                 c(0,1,0),
                 c(0,0,1),
                 c(0,0,1))

A_odds <- (sigma * diag_one) %*% A %*%
  t(diag_one * sigma)
## Collapsed covariance matrix on the odds scale
A_odds_34 <- t(ones_34) %*% (sigma * diag_one) %*% A %*%
       t(diag_one * sigma) %*% ones_34
sqrt(diag(A_odds_34))

chol_A_odds_34 <- t(ones_34) %*% (sigma * diag_one) %*% t(chol(A))
A_odds_34 <- chol_A_odds_34 %*% t(chol_A_odds_34)
## Correlation matrix of the collapsed matrix
corr_34 <- 1/sqrt(diag(A_odds_34)) * diag_one[1:3, 1:3] %*% A_odds_34 %*%
  t(diag_one[1:3, 1:3] * 1/sqrt(diag(A_odds_34)))
## Correlation matrix with log taken of diagonal elements
A_fromodds <- log(sqrt(diag(A_odds_34))) * diag_one[1:3, 1:3] %*% corr_34 %*%
  t(diag_one[1:3, 1:3] * log(sqrt(diag(A_odds_34))))
## Correlation matrix with sigma logged
A_logodds <- (log(sigma) * diag_one) %*% A %*%
  t(diag_one * log(sigma))
A_fromlogodds <- t(ones_34) %*% A_logodds %*% ones_34
## Difference
A_fromlogodds - A_fromodds

## Draw from logodds
draws <- exp(MASS::mvrnorm(20000, rep(10, 4), A_logodds))
## Log sum exp for 3 and 4
## Correlation matrix is correct
round(cor(cbind(
  draws[,1:2],
  apply(draws[,3:4], 1, function(x){sum(x)})
)), 4)
## Covariance matrix
draws <- MASS::mvrnorm(20000, rep(0, 4), A_logodds)
cov_mat <- round(cov(cbind(
  draws[,1:2],
  apply(draws[,3:4], 1, function(x){sum(x)})
)), 4)
cov_mat
exp(sqrt(diag(cov_mat)))
sqrt(diag(cov_mat))
## variance parameters
var_parameters <- diag(round(cov(cbind(
  draws[,1:2],
  apply(draws[,3:4], 1, function(x){log(sum(exp(x)))})
)), 4))
exp(sqrt(var_parameters))^2


exp(sqrt(var_empirical))^2



################################################################################
## Test implementation in Stan, scale of priors

N <- 4
## Correlation matrix
A <- matrix(0.5, nrow = N, ncol = N)
diag(A) <- 1
## Standard deviations on the odds_scale
sigma_log <- round(abs(rnorm(N, 0, 0.1)), 3)
## Covariance matrix on the odds scale
diag_one <- matrix(0, nrow = N, ncol = N)
diag(diag_one) <- 1
ones_34 <- rbind(c(1,0,0),
                 c(0,1,0),
                 c(0,0,1),
                 c(0,0,1))

A_odds <- (exp(sigma_log) * diag_one) %*% A %*%
  t(diag_one * exp(sigma_log))
## Collapsed covariance matrix on the odds scale
A_odds_34 <- t(ones_34) %*% (sigma * diag_one) %*% A %*%
  t(diag_one * sigma) %*% ones_34
sqrt(diag(A_odds_34))

chol_A_odds_34 <- t(ones_34) %*% (sigma * diag_one) %*% t(chol(A))
A_odds_34 <- chol_A_odds_34 %*% t(chol_A_odds_34)
## Correlation matrix of the collapsed matrix
corr_34 <- 1/sqrt(diag(A_odds_34)) * diag_one[1:3, 1:3] %*% A_odds_34 %*%
  t(diag_one[1:3, 1:3] * 1/sqrt(diag(A_odds_34)))
## Correlation matrix with log taken of diagonal elements
A_fromodds <- log(sqrt(diag(A_odds_34))) * diag_one[1:3, 1:3] %*% corr_34 %*%
  t(diag_one[1:3, 1:3] * log(sqrt(diag(A_odds_34))))
## Correlation matrix with sigma logged
A_logodds <- (log(sigma) * diag_one) %*% A %*%
  t(diag_one * log(sigma))
A_fromlogodds <- t(ones_34) %*% A_logodds %*% ones_34
## Difference
A_fromlogodds - A_fromodds

## Test empirically difference between change on logodds and odds scale

N <- 4
sigma <- round(abs(rnorm(N, 0, 0.1)), 3)
## Covariance matrix on the odds scale
diag_one <- matrix(0, nrow = N, ncol = N)
diag(diag_one) <- 1
A_odds <- (sigma * diag_one) %*% A %*%
  t(diag_one * sigma)


draws <- MASS::mvrnorm(2000, rep(1, 4), A_odds)

round(cov(draws - 1), 5) - round(cov(log(draws) - 0), 5)


cov_mat <- round(cov(cbind(
  draws[,1:2] - 1,
  apply(draws[,3:4] - 1, 1, function(x){sum(x)})
)), 4)

cov_mat_log <- round(cov(cbind(
  log(draws[,1:2]),
  apply(log(draws[,3:4]), 1, function(x){sum(x)})
)), 4)












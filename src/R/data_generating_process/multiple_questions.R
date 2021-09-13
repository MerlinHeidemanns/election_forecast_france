#' Objective: Is the subsetting behavior as desired?


p <- c(0.1, 0.2, 0.3, 0.4)

rho <- -0.1
sigma <- 0.01
covmat <- matrix(rho * sigma^2, nrow = 4, ncol = 4)
diag(covmat) <- sigma^2


phat <- p[1:3]
covmathat <- covmat[1:3, 1:3]

lo_phat <- log(phat/p[4])

lo_phat10 <- lo_phat + MASS::mvrnorm(1, rep(0, 3), covmathat * 100)

phat10 <- exp(lo_phat10) * p[4]
p10 <- c(phat10, 1 - sum(phat10))


p <- c(0.1, 0.2, 0.3, 0.4)
rho <- -0.1
sigma <- 0.01
covmat <- matrix(rho * sigma^2, nrow = 4, ncol = 4)
diag(covmat) <- sigma^2

apply(MASS::mvrnorm(1000, p, covmat) * 1000, 2, mean)

#' Subsetting
p <- c(0.3, 0.3, 0.1, 0.1, 0.2)
covmat <- p %*% t(p)
for (j in 1:length(p)){
  for (i in 1:length(p)){
    if (i != j){
      covmat[i, j] = -covmat[i, j]
    }
  }
}
p = p * 200
covmat = covmat * 200

p[1:4] + (covmat[1:4, 5] %*% solve(covmat[5, 5])) %*% (0 - p[5])










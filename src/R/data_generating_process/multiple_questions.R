#' Objective: Is the subsetting behavior as desired?
#' If I softmax the first three elements I get a different result
p <- c(0.1, 0.2, 0.3, 0.4)
p_logodds <- log(p/p[4])
exp_softmax(p_logodds[1:4])
#' Create a covariance matrix 4x4
#' Can I condition this covariance matrix on one of the parameters being 0
#' and then use it for the random walk of the other parameters on the logodds scale
rho <- -0.1
sigma <- 0.01
covmat <- matrix(rho * sigma^2, nrow = 4, ncol = 4)
diag(covmat) <- sigma^2
condcovmat
#' This does not work because the fourth element is fixed at 0 not conditioned on.






















p <- c(0.1, 0.2, 0.3, 0.4)




phat <- p[1:3]
covmathat <- covmat[1:3, 1:3]

lo_phat <- log(phat/p[4])

mat <- matrix(NA, nrow = 20, 4)
mat[, 4] = 0
mat[1, 1:3] = lo_phat
for (t in 2:20){
  mat[t, 1:3] = mat[t - 1, 1:3] + MASS::mvrnorm(1, rep(0, 3), covmathat * 100)
}
for (t in 1:20){
  mat[t, ] = exp_softmax(mat[t,])
}
phat10 <- exp(lo_phat10) * p[4]
p10 <- c(phat10, 1 - sum(phat10))
exp_softmax(c(lo_phat10, 0)





p <- c(0.1, 0.2, 0.3, 0.4)
rho <- -0.1
sigma <- 0.01
covmat <- matrix(rho * sigma^2, nrow = 4, ncol = 4)
diag(covmat) <- sigma^2

apply(MASS::mvrnorm(1000, p, covmat) * 1000, 2, mean)

#' Subsetting
p <- c(0.3, 0.3, 0.1, 0.1, 0.2)
p <- runif(5, 0.2, 0.4)
p <- p/sum(p)
covmat <- p %*% t(p)
n <- 200
for (j in 1:length(p)){
  for (i in 1:length(p)){
    if (i != j){
      covmat[i, j] = -p[i] * p[j]
    } else {
      covmat[i, j] = p[i] * (1 - p[j])
    }
  }
}
p = p

p[1:3]
pcond <- p[1:3] + covmat[1:3, 4:5] %*% solve(covmat[4:5, 4:5]) %*% (0 - p[4:5])
round(pcond, 4) == round(p[1:3]/sum(p[1:3]), 4)

covmatcondderived <- covmat[1:3, 1:3] - covmat[1:3, 4:5] %*% solve(covmat[4:5, 4:5]) %*% covmat[4:5, 1:3]

apply(MASS::mvrnorm(10000000, pcond, covmatcondderived) * 1000000, 2, mean)/1000000
pcond

covmatcond <- pcond %*% t(pcond)
n <- 200
for (j in 1:length(pcond)){
  for (i in 1:length(pcond)){
    if (i != j){
      covmatcond[i, j] = -pcond[i] * pcond[j]
    } else {
      covmatcond[i, j] = pcond[i] * (1 - pcond[j])
    }
  }
}

### transition matrix approach

transmat <- matrix(abs(rnorm(6^6)), nrow = 6, ncol = 6)
diag(transmat) <- 0
for (j in 1:6) transmat[j,] = transmat[j,]/sum(transmat[j,])

p <- c(0.1, 0.2, 0.3, 0.2, 0.1, 0.1)

for (j in 2:3){
  tt <- rep(1, 6)
  tt[j] <- 0
  p = p * tt + transmat[j,] * p
  for (i in 1:6){
    tmp <- transmat[j, ]
    transmat[j, ] = transmat[j, ] * tmp
  }
}




transmat[1:3, 1:3] + transmat[1:3, 4:6] %*% solve(transmat[4:6, 4:6]) %*% transmat[4:6, 1:3]




p <- c(0.1, 0.2, 0.3, 0.3, 0.1)

phat <- log(p/p[length(p)])

library(Dirichlet)


mat <- DirichletReg::rdirichlet(2, rep(1, 3))

mat %*% c(0.1,0.2, 0.3, 0.3, 0.1)
c(0.1, 0.2, 0.3) + t(mat) %*% c(0.3, 0.1)































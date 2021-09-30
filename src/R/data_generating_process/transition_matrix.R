

p <- DirichletReg::rdirichlet(1, rep(1, 5))
mat <- matrix(0, nrow = 5, ncol = 5)
diag(mat) = 1
mat[1, 5] = mat[5, 1] = mat[1, 4] = mat[4, 1] = -0.5
mat[2, 5] = mat[5, 2] = mat[2, 4] = mat[4, 2] = -0.3
mat[3, 5] = mat[5, 3] = mat[3, 4] = mat[4, 3] = -0.1
mat[4, 5] = mat[5, 4] = -0.1
#mat[2, 3] = mat[3, 2] = mat[3, 1] = mat[1, 3] = -0
mat
pcond <- p[1:3] + mat[1:3, 4:5] %*% solve(mat[4:5,4:5]) %*% (c(0.2, 0.2) - p[4:5])
sum(pcond)
mat
mat[1:4, 1:4] - mat[1:4, 5] %*% solve(mat[5,5]) %*% mat[5, 1:4]

p <- DirichletReg::rdirichlet(1, rep(1, 5))
mat <- abs(rnorm(5, 0, 1))
#mat <- mat/sum(mat)
mat <- mat %*% t(mat)
#mat <- t(p) %*% p
diag(mat) <- 0
for (j in 1:nrow(mat)){
  mat[,j] = mat[,j] / sum(mat[, j])
}
mat = mat * - 1
diag(mat) <- 1
sigma <- seq(1, 5)
sigma <- sigma %*% t(sigma)
pcond = p[1:3] + mat[1:3, 4:5] %*% solve(mat[4:5,4:5]) %*% (c(0.3, 0.3) - p[4:5])
sum(pcond)
pcond
p




P <- 5
corr_mat_positions <- matrix(0, nrow = P, ncol = P - 1)
for (ii in 1:P){
  for (jj in 1:P - 1){
    if (jj >= ii){
      corr_mat_positions[ii, jj] = jj + 1
    } else {
      corr_mat_positions[ii, jj] = jj
    }
  }
}
corr_mat_positions

p[1:4] + MASS::mvrnorm(1, rep(0, 4), (mat * sigma)[1:4, 1:4])





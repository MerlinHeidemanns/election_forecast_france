



A <- matrix(0.5, nrow = 5, ncol = 5)
diag(A) <- c(1,1,1,1, 1)
A[4,3] = A[3,4] = 0
A[5,3] = A[3,5] = 0

draws <- MASS::mvrnorm(1e5, rep(0, 5), A)

round(cov(cbind(draws[,1:2],
                apply(draws[,3:4], 1, sum),
                draws[,5])),3)


t(c(0,0,1,1,0)) %*% A %*% c(0,0,1,1,0)

ones <- rbind(c(1,0,0,0),
      c(0,1,0,0),
      c(0,0,1,0),
      c(0,0,1,0),
      c(0,0,0,1))

t(ones) %*% A %*% ones


2 * 2 * 2
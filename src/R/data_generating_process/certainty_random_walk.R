


N <- 3
T <- 100
p <- rep(1/N, N)
logodds_p <- log(p/p[length(p)])

sigma <- 0.1
corrmat <- matrix(0.1, nrow = N - 1, ncol = N - 1)
diag(cov_mat) <- 1
diag_one <- matrix(0, nrow = N - 1, ncol = N - 1)
diag(diag_one) <- 1

logodds <- matrix(NA, nrow = T, ncol = N - 1)
prob <- matrix(NA, nrow = T, ncol = N)

certainty_start <- rep(0.5, N - 1)
logitcertainty <- matrix(NA, nrow = T, ncol = N - 1)
logitcertainty[1,] <- boot::logit(certainty_start)


logodds[1, ] <- logodds_p[1:(N - 1)]
prob[1, ] <- exp_softmax(c(logodds[1,], 0))



for (t in 2:T){
  logitcertainty[t, ] <- logitcertainty[t - 1, ] + rnorm(N - 1, 0, 0.1)

  sigma <- rep(0.001, N - 1)
  covmat <- sigma * diag_one %*% corrmat %*% diag_one * sigma
  beta <- boot::inv.logit(logitcertainty[t, ])/0.5
  logodds[t,] <- beta * logodds[t - 1, ] + MASS::mvrnorm(1, rep(0, N - 1), cov_mat)
  prob[t,] <- exp_softmax(c(logodds[t,], 0))
}

df <- prob %>%
  as.data.frame() %>%
  mutate(t = 1:n()) %>%
  pivot_longer(c(-t),
               names_to = "candidate",
               values_to = "share")
plt <- ggplot(df, aes(x = t, y = share, color = candidate)) +
  geom_line()
plt
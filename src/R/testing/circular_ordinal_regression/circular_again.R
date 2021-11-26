
###############################################################################
## Thresholds on the circle
thresholds <- sort(2 * atan(rnorm(6, 0, 4))) + pi
df <-
  data.frame(
    x = as.numeric(rvonmises(10000, 2, 0.5))
    #x = (rnorm(100000, 1.5, 2) + 10 * pi) %% (2 * pi)
  ) %>%
  mutate(
    g = ifelse(x < thresholds[1], 1,
        ifelse(x > thresholds[1] & x < thresholds[2], 2,
        ifelse(x > thresholds[2] & x < thresholds[3], 3,
        ifelse(x > thresholds[3] & x < thresholds[4], 4,
        ifelse(x > thresholds[4] & x < thresholds[5], 5, 6)))))
  )
ggplot(df, aes(x = x, fill = as.factor(g))) +
  geom_histogram(bins = 200)
###############################################################################
## Model test
library(cmdstanr)
thresholds <- sort(seq(0.5, 2 * pi - 0.5, length.out = 4))
df <- data.frame(
  x = as.numeric(rvonmises(200, 0, 1))
) %>%
  mutate(
    g = ifelse(x < thresholds[1] | x > thresholds[4], 1,
        ifelse(x > thresholds[1] & x < thresholds[2], 2,
        ifelse(x > thresholds[2] & x < thresholds[3], 3,
        ifelse(x > thresholds[3] & x < thresholds[4], 4, NA))))
  )
hist(df$x)
table(df$g)
mod <- cmdstan_model("src/stan/testing/circle_sampling_cheat.stan")
fit <- mod$sample(
  data = list(N = nrow(df), C = 4, y = data.frame(table(df$g))$Freq/200),
  chains = 6,
  iter_sampling = 1000,
  iter_warmup = 300,
  parallel_chains = 6,
  refresh = 100,
  init = 0.2
)
fit$draws("t") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "t",
               values_to = "draws",
               names_pattern = "([\\d])") %>%
  filter(!is.na(t)) %>%
  ggplot(aes(x = draws, fill = t)) +
    geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
    facet_wrap(t ~.) +
    geom_vline(data = data.frame(h = thresholds, t = as.character(1:4)),
               aes(xintercept = h))
bayesplot::mcmc_pairs(fit$draws("t"))
#' bad output
###############################################################################
## Map mu to circle but wrap around
library(circular)
plot(seq(1, 10, 0.1), sapply(seq(1, 10, 0.1), function(x){
  return(mean(rvonmises(100000, x, 0.5)))
}))



mu <- 2
thres <- thresholds <- sort(rnorm(6, 0, 4))
thres <- thresholds <- sort(runif(6, -pi, pi))
compute_theta(mu, thres)
sum(compute_theta_circle(mu, thres))
compute_theta_circle <- function(mu, thres){
  len_theta <- length(thres)
  out_theta <- rep(NA, len_theta)
  thres_circ <- tan(thres/2)
  m_circ <- tan(mu/2)
  out_theta[1] = pnorm(m_circ - thres_circ[len_theta]) +
    (1 - pnorm(m_circ - thres_circ[1]))
  for (k in 2:len_theta){
    out_theta[k] = pnorm(m_circ - thres_circ[k - 1]) - pnorm(m_circ - thres_circ[k])
  }
  return(out_theta)
}

thres <- thresholds <- sort(runif(6, -pi + 0.2, pi - 0.2))
thres <- seq(-2, 1, length.out = 3)
df0 <- lapply(seq(-pi,pi, length.out = 100), function(x){
  df1 <- data.frame(prob = compute_theta_circle(x, thres)) %>%
    add_column(mu = x) %>%
    mutate(cat = 1:n())
  return(df1)
}) %>%
  do.call("bind_rows", .)

ggplot(df0, aes(x = mu, y = prob, color = as.factor(cat))) +
  geom_line()





compute_theta <- function(mu, thres){
  len_theta <- length(thres)
  out_theta <- rep(NA, len_theta)
  out_theta[1] = (2 * atan(mu - thres[len_theta]) + pi)/(2 * pi) +
    (1 - (2 * atan(mu - thres[1]) + pi)/(2 * pi))
  for (k in 2:len_theta){
    out_theta[k] = ((2.0 * atan((mu - thres[k - 1])) -
                       2.0 * atan(mu - thres[k])))/(2.0 * pi);
  }
  return(out_theta)
}




thres <- thresholds <- sort(rnorm(6, 0, 4))
mu <- 2
compute_theta_circle_2 <- function(mu, thres){
  len_theta <- length(thres)
  out_theta <- rep(NA, len_theta)
  thres_circ <- 2 * atan(thres) + pi
  m_circ <- 2 * atan(mu) + pi
  out_theta[1] = m_circ - thres_circ[len_theta] +
    1 - m_circ - thres_circ[1])
  for (k in 2:len_theta){
    out_theta[k] = pnorm(m_circ - thres_circ[k - 1]) - pnorm(m_circ - thres_circ[k])
  }
  return(out_theta)
}

thres <- thresholds <- sort(runif(6, -pi + 0.2, pi - 0.2))
thres <- seq(-2, 1, length.out = 3)
df0 <- lapply(seq(-pi,pi, length.out = 100), function(x){
  df1 <- data.frame(prob = compute_theta_circle(x, thres)) %>%
    add_column(mu = x) %>%
    mutate(cat = 1:n())
  return(df1)
}) %>%
  do.call("bind_rows", .)

ggplot(df0, aes(x = mu, y = prob, color = as.factor(cat))) +
  geom_line()
























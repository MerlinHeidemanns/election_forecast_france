###############################################################################
## Title: Circular ordinal model
###############################################################################
## Libraries
library(circular)
library(cmdstanr)
library(tidyverse)
###############################################################################
## What does the distribution look like?
ggplot(data.frame(x = rvonmises(10000, 0, 1)),
       aes(x)) +
  geom_histogram(bins = 100) +
  coord_polar(theta = "x", start = -.13, direction = 1)
## Cutpoints
C <- 5
c <- sort(runif(C, min = 0, max = 2 * pi))
df <- data.frame(
  x = rvonmises(10000, 0, 1)
) %>%
  mutate(
    class = ifelse(x < c[1], 1,
                   ifelse(x < c[2], 2,
                          ifelse(x < c[3], 3,
                                 ifelse(x < c[4], 4,
                                        ifelse(x < c[5], 5,
                                                      6
                                               )
                                        )
                                 )
                          )
                   )
    )
ggplot(df,
       aes(x, fill = as.factor(class))) +
  geom_histogram(bins = 100) +
  coord_polar(theta = "x", start = -.13, direction = 1)
###############################################################################
## Testing the circle cutpoints
N <- 10
mu <- 3
x <- runif(N, -1, 1)
cutpoints <- sort(rnorm(3, 0, 5))

y_star <- mu + 2 * runif(1, -2, 2)
cat1 <- 1 - (2 * atan((y_star - cutpoints[1])) + pi)/(2 * pi)
cat2 <- ((2 * atan((y_star - cutpoints[1])) - 2 * atan(y_star - cutpoints[2])) + 0)/(2 * pi)
cat3 <- ((2 * atan(y_star - cutpoints[2]) - 2 * atan(y_star - cutpoints[3])) + 0)/(2 * pi)
cat4 <- ((2 * atan(y_star - cutpoints[3])) + pi)/(2 * pi)
sum(c(cat1, cat2, cat3, cat4))
c(cat1, cat2, cat3, cat4)


cutpoints <- sort(rnorm(4, 0, 5))
y_star <- mu + 2 * runif(1, -2, 2)
cat1 <- 1 - (2 * atan((y_star - cutpoints[1])) + pi)/(2 * pi) +
  ((2 * atan(y_star - cutpoints[4])) + pi)/(2 * pi)
cat2 <- ((2 * atan((y_star - cutpoints[1])) - 2 * atan(y_star - cutpoints[2])) + 0)/(2 * pi)
cat3 <- ((2 * atan(y_star - cutpoints[2]) - 2 * atan(y_star - cutpoints[3])) + 0)/(2 * pi)
cat4 <- ((2 * atan(y_star - cutpoints[3]) - 2 * atan(y_star - cutpoints[4])) + 0)/(2 * pi)
sum(c(cat1, cat2, cat3, cat4))
c(cat1, cat2, cat3, cat4)





###############################################################################
## Basic Stan model
N <- 500
mu <- 0
kappa <- 1
x <- rnorm(N, 0, 1)
C <- 6
c <- 2 * atan(sort(runif(C, -40, 40)))
c <- sort(runif(C - 1, -pi, pi))
beta <- 1
df <- data.frame(y_star = 2 * atan(mu + beta * x),
                 x = x) %>%
  mutate(
    y = ifelse((y_star < c[1]), 1,
        ifelse((c[1] < y_star) & (y_star < c[2]), 2,
        ifelse((c[2] < y_star) & (y_star < c[3]), 3,
        ifelse((c[3] < y_star) & (y_star < c[4]), 4,
        ifelse((c[4] < y_star) & (y_star < c[5]), 5,
        ifelse((c[5] < y_star), 6,
        NA))))))
  )
ggplot(df,
       aes(y_star, fill = as.factor(y))) +
  geom_histogram(bins = 100) +
  coord_polar(theta = "x", start = -.13, direction = 1)
hist(df$y_star)
## Data list
data_list <- list(
  N = N,
  y = df %>% pull(y),
  x = df %>% pull(x),
  C = 6
)
## Model
mod <- cmdstan_model("src/stan/testing/circular_ordinal.stan")
mod <- cmdstan_model("src/stan/testing/circular_ordinal_overlap.stan")
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 250,
  init = 0.2
)
fit$summary()
table(data_list$y)/data_list$N

yhat <- fit$draws("yhat") %>%
  posterior::as_draws_df() %>%
  pivot_longer(everything(),
               names_to = "yhat",
               values_to = "draws",
               names_pattern = "([\\d]+)") %>%
  mutate(yhat = as.integer(yhat)) %>%
  filter(!is.na(yhat)) %>%
  arrange(yhat) %>%
  group_by(yhat, draws) %>%
  summarize(N = n()) %>%
  group_by(yhat) %>%
  mutate(freq = N/sum(N)) %>%
  left_join(data.frame(
    y = data_list$y,
    yhat = 1:data_list$N
  ))

ggplot(yhat, aes(x = draws, y = freq)) +
  geom_jitter(width = 0.3) +
  facet_wrap(y ~ .)

###############################################################################
## Overlap model
N <- 1000
kappa <- 1
x <- rnorm(N, 0, 1)
C <- 6
c <- 2 * atan(sort(runif(C, -40, 40)))
c <- sort(runif(C, -2.9, pi))
beta <- 1
df <- data.frame(y_star = 2 * atan(beta * x),
                 x = x) %>%
  mutate(
    y = ifelse((c[6] < y_star) | (y_star < c[1]), 1,
        ifelse((c[1] < y_star) & (y_star < c[2]), 2,
        ifelse((c[2] < y_star) & (y_star < c[3]), 3,
        ifelse((c[3] < y_star) & (y_star < c[4]), 4,
        ifelse((c[4] < y_star) & (y_star < c[5]), 5,
        ifelse((c[5] < y_star) & (y_star < c[6]), 6, NA))))))
  )
ggplot(df[1:500,],
       aes(y_star, fill = as.factor(y))) +
  geom_histogram(bins = 100) +
  coord_polar(theta = "x", start = -.13, direction = 1)
## Data list
df <- df %>%
  mutate(i = 1:n())
y <- df %>% filter(i <= 500) %>%
  select(i, y) %>%
  mutate(z = 1) %>%
  pivot_wider(id_cols = i,
              names_from = y,
              values_from = z,
              values_fill = 0) %>%
  select(-i)
y <- y[, order(colnames(y))] * 40 + matrix(rbinom(6 * 500, , 0.5), nrow = 500, ncol = 6)
data_list <- list(
  N = 500,
  N2 = 500,
  y = y,
  x = df %>% filter(i <= 500) %>% pull(x),
  x_new = df %>% filter(i > 500) %>% pull(x),
  C = 6
)
## Model
mod <- cmdstan_model("src/stan/testing/circular_ordinal_overlap_multi.stan")
## Fit
fit <- mod$sample(
  data = data_list,
  chains = 6,
  iter_sampling = 500,
  iter_warmup = 500,
  parallel_chains = 6,
  refresh = 250
)
y_long <- data_list$y %>%
  as.data.frame() %>%
  mutate(i = 1:n()) %>%
  pivot_longer(c(-i),
               names_to = "c",
               values_to = "true") %>%
  mutate(c = as.integer(c))
yhat <- fit$draws("yhat") %>%
  posterior::as_draws_df() %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(c(-iter),
               names_to = "variable",
               values_to = "draws") %>%
  mutate(
    c = as.integer(str_match(variable, "([\\d]+)\\]")[,2]),
    i = as.integer(str_match(variable, "\\[([\\d]+)")[,2])
  ) %>%
  filter(!is.na(c)) %>%
  left_join(y_long)
yhat <- yhat %>%
  arrange(i) %>%
  group_by(iter, i) %>%
  mutate(draws_share = draws/sum(draws),
         true_share = true/sum(true)) %>%
  group_by(i, c) %>%
  summarize(
    q50 = quantile(draws_share - true_share, 0.5),
    q25 = quantile(draws_share - true_share, 0.25),
    q75 = quantile(draws_share - true_share, 0.75),
    true = mean(true)
  )
ggplot(yhat, aes(x = c, y = q50)) +
  geom_point() +
  facet_wrap(true ~ .)


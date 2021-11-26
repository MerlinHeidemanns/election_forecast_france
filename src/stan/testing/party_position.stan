functions {
  real unnormalized_normal_density(real x, real mu, real sigma){
    return(1/sigma * e()^(-0.5 * (x - mu)^2/sigma));
  }
  vector normal_density_prob(real x, vector mu, vector sigma){
    int len_prob = size(mu);
    vector[len_prob] dens;
    for (j in 1:len_prob) dens[j] = unnormalized_normal_density(x, mu[j], sigma[j]);
    return(dens/sum(dens));
  }
}
data {
  int<lower=0> N;
  int K;
  matrix[N, K] x;
  int P;
  matrix[N, P] y;
}
parameters {
  ordered[P] mu;
  vector<lower = 0>[P] sigma;
  vector[K] beta;
  real<lower = 0> sigma_y;
}
model {
  vector[N] y_star = x * beta;
  matrix[N, P] y_hat;
  mu ~ normal(0, 3);
  sigma ~ normal(0, 3);
  beta ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  for (j in 1:N){
    y_hat[j] = compute_prob(normal_density_prob(y_star[j], mu, sigma))';
  }
  to_vector(y) ~ normal(to_vector(y_hat), sigma_y);
}


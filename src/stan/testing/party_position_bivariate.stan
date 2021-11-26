functions {
  real unnormalized_normal_density(vector x, vector mu, vector sigma, real rho){
    return(1/(2 * pi() * sigma[1] * sigma[1] * sqrt(1 - square(rho))) *
      e()^(square((x[1] - mu[1])/sigma[1]) - 2 * rho * ((x[1] - mu[1])/sigma[1]) * ((x[2] - mu[2])/sigma[1]) + square((x[2] - mu[2])/sigma[1]))
    );
  }
  vector normal_density_prob(vector x, matrix mu, matrix sigma, vector rho){
    int len_prob = size(rho);
    vector[len_prob] dens;
    for (j in 1:len_prob) dens[j] = unnormalized_normal_density(x, mu[j]', sigma[j]', rho[j]);
    return(dens/sum(dens));
  }
}
data {
  int<lower=0> N;
  int<lower =0> NDepartments;
  int K;
  matrix[N, K] x;
  int P;
  int id_Obs_departments[N];
  matrix[N, P] y;
}
parameters {
  real<lower = 0> sigma_alpha;
  matrix[NDepartments, 2] raw_alpha;
  positive_ordered[P - 3] mu_one_left;
  positive_ordered[P - 3] mu_one_right;
  vector[P] mu_two;
  matrix<lower = 0>[P, 2] sigma;
  matrix[K, 2] beta;
  real<lower = 0> sigma_y;
  vector<lower = - 1, upper = 1>[P] rho;
}
transformed parameters {
  matrix[NDepartments, 2] alpha = raw_alpha * sigma_alpha;
}
model {
  matrix[P, 2] mu;
  matrix[N, 2] y_star = alpha[id_Obs_departments] + x * beta;
  matrix[N, P] y_hat;
  mu[1:2,1] = -reverse(mu_one_left);
  mu[4:5,1] = mu_one_right;
  mu[,2] = mu_two;
  mu[3] = rep_vector(0.0, 2)';

  to_vector(raw_alpha) ~ std_normal();
  sigma_alpha ~ normal(0, 1);
  mu_one_left ~ normal(0, 3);
  mu_one_right ~ normal(0, 3);
  mu_two ~ normal(0, 3);
  to_vector(sigma) ~ normal(0, 3);
  to_vector(beta) ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  rho ~ normal(0, 0.5);
  for (j in 1:N){
    y_hat[j] = normal_density_prob(y_star[j]', mu, sigma, rep_vector(0.0, P))';
  }
  to_vector(y) ~ normal(to_vector(y_hat), sigma_y);
}


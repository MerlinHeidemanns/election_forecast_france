functions {
  real unnormalized_normal_density(real x, real mu, real sigma){
    return(1/sigma * e()^(-0.5 * (x - mu)^2/sigma));
  }
  vector normal_density_prob(int Nstd,
                             vector xstd,
                             real mu_department,
                             real sigma_department,
                             vector mu_party,
                             vector sigma_party){
    int len_prob = size(mu_party);
    vector[len_prob] out;
    matrix[Nstd, len_prob] dens;
    for (i in 1:Nstd){
      for (j in 1:len_prob){
        dens[i, j] = unnormalized_normal_density(
          mu_department + xstd[i] * sigma_department,
          mu_party[j],
          sigma_party[j]);
      }
      dens[i] = dens[i]/sum(dens[i]);
    }
    for (j in 1:len_prob) out[j] = mean(dens[,j]);
    return(out);
  }
}
data {
  int<lower=0> N;
  int<lower = 0> NDepartments;
  int id_Obs_departments[N];
  int K;
  matrix[N, K] x;
  int P;
  matrix[N, P] y;
  int Nstd;
  vector[Nstd] xstd;
}
parameters {
  ordered[P] mu;
  vector<lower = 0>[P] sigma;
  vector[K] beta;
  real<lower = 0> sigma_y;
  real<lower = 0> sigma_sigma_departments;
  vector[NDepartments] raw_sigma_departments;
}
transformed parameters {
  vector[NDepartments] sigma_departments = exp(raw_sigma_departments * sigma_sigma_departments);
}
model {
  vector[N] y_star = x * beta;
  matrix[N, P] y_hat;
  raw_sigma_departments ~ std_normal();
  sigma_sigma_departments ~ normal(0, 1);
  mu ~ normal(0, 3);
  sigma ~ normal(0, 3);
  beta ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  for (j in 1:N){
    y_hat[j] = normal_density_prob(
      Nstd,
      xstd,
      y_star[j],
      0.5,
      mu,
      sigma)';
  }
  to_vector(y) ~ normal(to_vector(y_hat), sigma_y);
}


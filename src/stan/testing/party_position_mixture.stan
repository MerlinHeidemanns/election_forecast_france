functions {
  real normalized_normal_density(real x, real mu, real sigma){
    return(1/(sigma * sqrt(2 * pi())) * e()^(-0.5 * (x - mu)^2/sigma));
  }
  vector normal_density_prob(real x, matrix mu, vector sigma, vector lambda){
    int len_prob = size(lambda);
    vector[len_prob] dens;
    for (j in 1:len_prob){
      dens[j] =
        lambda[j] * normalized_normal_density(x, mu[j, 1], sigma[j]) +
        (1 - lambda[j]) * normalized_normal_density(x, mu[j, 2], sigma[j]);
    }
    return(dens/sum(dens));
  }
}
data {
  int<lower=0> N;
  int NElections;
  int id_Obs_elections[N];
  int NDepartments;
  int K;
  matrix[N, K] x;
  int P;
  matrix[N, P] y;
  matrix[N, P] y_lag;

  int NBlocs_Elections[NElections];
  array[NElections, P] int included_blocs;
  int NParticipated;
  int participated[NParticipated];

}
parameters {
  ordered[2] raw_mu[P];
  vector<lower = 0>[P] sigma;
  vector<lower =0, upper = 1>[P] lambda;
  vector[K] beta;
  real<lower = 0.0001> sigma_y;
}
transformed parameters {
  vector[N] y_star;
  matrix[P, 2] mu;
  for (j in 1:P){
    mu[j] = raw_mu[j]';
  }
  y_star = x * beta;
}
model {
  matrix[N, P] y_hat;
  for (j in 1:P){
    raw_mu[j] ~ normal(0, 3);
    sigma[j] ~ normal(0, 3);
  }
  beta ~ normal(0, 1);
  lambda ~ beta(2, 2);
  sigma_y ~ normal(0, 0.01);
  for (j in 1:N){
    {
      int included[NBlocs_Elections[id_Obs_elections[j]]] = included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]];
      y_hat[j, included] = normal_density_prob(
        y_star[j],
        mu[included],
        sigma[included],
        lambda[included])';
    }
  }
  to_vector(y)[participated] ~ normal(to_vector(y_hat)[participated], sigma_y);
}
generated quantities {
  matrix[N, P] y_hat;
  for (j in 1:N){
    {
      int included[NBlocs_Elections[id_Obs_elections[j]]] = included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]];
      y_hat[j, included] = normal_density_prob(
        y_star[j],
        mu[included],
        sigma[included],
        lambda[included])';
    }
  }
}

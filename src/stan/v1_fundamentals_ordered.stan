data {
  int NElections_past_fundamentals;
  int NBlocs;
  int y_fundamentals[NElections_past_fundamentals, NBlocs];
  matrix[NElections_past_fundamentals, NBlocs] incumbency;
  int K;
  matrix[NElections_past_fundamentals, K] x_fundamentals;
  int incumbency_distance[NBlocs - 1, NBlocs];
  int incumbency_bloc[NElections_past_fundamentals];
}
transformed data {
  real lsigma = 0.00001;
}
parameters {
  matrix[NBlocs - 1, NElections_past_fundamentals] raw_alpha;
  real<lower = lsigma> sigma_sigma_cov_fundamentals;
  vector<lower = lsigma>[NBlocs - 1] sigma_cov_fundamentals;
  cholesky_factor_corr[NBlocs - 1] chol_corr_alpha;
  simplex[NBlocs] alpha_prior;
  real beta_incumbency;

  vector[2] raw_beta_incumbency;
  ordered[NBlocs - 2] raw_beta_fundamentals[2];

}
transformed parameters {
  matrix[NBlocs, NElections_past_fundamentals] alpha;
  cholesky_factor_cov[NBlocs - 1] chol_cov_alpha;
  matrix[K, NBlocs] beta_fundamentals;

  // alpha walk
  chol_cov_alpha = diag_pre_multiply(sigma_cov_fundamentals, chol_corr_alpha);
  alpha[,1] = log(alpha_prior/alpha_prior[1]);
  alpha[1] = rep_vector(0.0, NElections_past_fundamentals)';
  for (tt in 2:NElections_past_fundamentals){
    alpha[2:NBlocs, tt] = chol_cov_alpha * raw_alpha[:,tt] +
        alpha[2:NBlocs, tt - 1];
  }

  // beta fundamentals
  beta_fundamentals[,1] = raw_beta_incumbency;
  beta_fundamentals[1, 2:5] = raw_beta_fundamentals[1]';
  beta_fundamentals[2, 5:2] = raw_beta_fundamentals[2]';
  beta_fundamentals[,6] = rep_vector(0.0, 2);
}
model {
  matrix[NElections_past_fundamentals, NBlocs] mu_beta;
  for (nn in 1:NElections_past_fundamentals){
    mu_beta[nn] = (x_fundamentals[nn] * beta_fundamentals)[incumbency_distance[incumbency_bloc[nn]]];
  }
  mu_beta = x_fundamentals * beta_fundamentals;
  beta_incumbency ~ normal(0, 0.1);
  for (k in 1:K)
    raw_beta_fundamentals[k] ~ normal(0, 0.1);
  raw_beta_incumbency ~ normal(0, 0.1);
  to_vector(raw_alpha) ~ std_normal();
  sigma_sigma_cov_fundamentals ~ normal(0, 0.1);
  sigma_cov_fundamentals ~ normal(0, sigma_sigma_cov_fundamentals);
  chol_corr_alpha ~ lkj_corr_cholesky(1.0);
  alpha_prior ~ dirichlet(rep_vector(2, NBlocs));
  for (nn in 1:NElections_past_fundamentals){
    {
      vector[NBlocs] mu;
      mu = softmax(alpha[,nn] +
          beta_incumbency * incumbency[nn, ]' +
          mu_beta[nn]');
      target += multinomial_lpmf(y_fundamentals[nn] | mu);
    }
  }
}
generated quantities {
  matrix[NBlocs, NElections_past_fundamentals] prob_alpha;
  matrix[NBlocs, NElections_past_fundamentals - 1] epsilon;
  for (nn in 1:NElections_past_fundamentals)
    prob_alpha[,nn] = softmax(alpha[,nn]);

  // for (tt in 2:NElections_past_fundamentals){
  //   {
  //     vector[NBlocs] prob_y = to_vector(y_fundamentals[tt])/sum(y_fundamentals[tt]);
  //     vector[NBlocs] prob_y_hat = softmax(alpha[,tt - 1] + append_col(0.0, to_vector(normal_rng(rep_vector(0.0, NBlocs - 1), 1))' * chol_cov_alpha)' +
  //     incumbency[tt]' * beta_incumbency +
  //     ((x_fundamentals[tt] * beta_fundamentals)[incumbency_distance[incumbency_bloc[tt]]])');
  //     epsilon[, tt - 1] = prob_y - prob_y_hat;
  //   }
  // }
}




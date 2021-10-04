data {
  int NElections_past_fundamentals;
  int NBlocs;
  int y_fundamentals[NElections_past_fundamentals, NBlocs];
  matrix[NElections_past_fundamentals, NBlocs] incumbency;
  int K;
  matrix[NElections_past_fundamentals, K] x_fundamentals;
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
  vector[K] beta_fundamentals;
  matrix[NBlocs - 1, NElections_past_fundamentals] raw_eta;
}
transformed parameters {
  matrix[NBlocs, NElections_past_fundamentals] alpha;
  matrix[NBlocs, NElections_past_fundamentals] eta;
  cholesky_factor_cov[NBlocs - 1] chol_cov_alpha;

  chol_cov_alpha = diag_pre_multiply(sigma_cov_fundamentals, chol_corr_alpha);
  alpha[,1] = log(alpha_prior/alpha_prior[1]);
  alpha[1] = rep_vector(0.0, NElections_past_fundamentals)';
  for (tt in 2:NElections_past_fundamentals){
    alpha[2:NBlocs, tt] = chol_cov_alpha * raw_alpha[:,tt] +
        alpha[2:NBlocs, tt - 1];
  }
  for (nn in 1:NElections_past_fundamentals){
    eta[,nn] = append_row(raw_eta[,nn], - sum(raw_eta[,nn]));
  }

}
model {
  vector[NElections_past_fundamentals] mu_beta;
  mu_beta = x_fundamentals * beta_fundamentals;
  beta_incumbency ~ normal(0, 0.1);
  beta_fundamentals ~ normal(0, 0.1);
  to_vector(raw_alpha) ~ std_normal();
  to_vector(eta) ~ normal(0, 0.4);
  sigma_sigma_cov_fundamentals ~ normal(0, 0.01);
  sigma_cov_fundamentals ~ normal(0, sigma_sigma_cov_fundamentals);
  chol_corr_alpha ~ lkj_corr_cholesky(10.0);
  alpha_prior ~ dirichlet(rep_vector(2, NBlocs));
  for (nn in 1:NElections_past_fundamentals){
    {
      vector[NBlocs] mu;
      mu = softmax(alpha[,nn] +
          beta_incumbency * incumbency[nn, ]' +
          mu_beta[nn] * incumbency[nn, ]');
      target += multinomial_lpmf(y_fundamentals[nn] | mu);
    }
  }
}
generated quantities {
  matrix[NBlocs, NElections_past_fundamentals] prob_alpha;
  matrix[NBlocs, NElections_past_fundamentals - 1] epsilon;
  for (nn in 1:NElections_past_fundamentals)
    prob_alpha[,nn] = softmax(alpha[,nn]);

  for (tt in 2:NElections_past_fundamentals){
    {
      vector[NBlocs] prob_y = to_vector(y_fundamentals[tt])/sum(y_fundamentals[tt]);
      vector[NBlocs] prob_y_hat = softmax(alpha[,tt - 1] + append_col(0.0, to_vector(normal_rng(rep_vector(0.0, NBlocs - 1), 1))' * chol_cov_alpha)' +
      incumbency[tt]' * beta_incumbency +
      incumbency[tt]' * (x_fundamentals[tt] * beta_fundamentals));
      epsilon[, tt - 1] = prob_y - prob_y_hat;
    }
  }
}




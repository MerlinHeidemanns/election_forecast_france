data {
  int NElections_past_fundamentals;
  int NBlocs;
  int y_fundamentals[NElections_past_fundamentals, NBlocs];
}
transformed data {
  real lsigma = 0.00001;
}
parameters {
  matrix[NBlocs - 1, NElections_past_fundamentals] raw_alpha;
  vector<lower = lsigma>[NBlocs - 1] sigma_cov_fundamentals;
  cholesky_factor_corr[NBlocs - 1] chol_corr_alpha;
  simplex[NBlocs] alpha_prior;
}
transformed parameters {
  matrix[NBlocs, NElections_past_fundamentals] alpha;
  cholesky_factor_cov[NBlocs - 1] chol_cov_alpha;
  chol_cov_alpha = diag_pre_multiply(sigma_cov_fundamentals, chol_corr_alpha);

  alpha[,1] = log(alpha_prior/alpha_prior[1]);
  alpha[1] = rep_vector(0.0, NElections_past_fundamentals)';

  for (tt in 2:NElections_past_fundamentals){
    alpha[2:NBlocs, tt] = chol_cov_alpha * raw_alpha[:,tt] +
        alpha[2:NBlocs, tt - 1];
  }
}
model {
  to_vector(raw_alpha) ~ std_normal();
  sigma_cov_fundamentals ~ normal(0, 0.1);
  chol_corr_alpha ~ lkj_corr_cholesky(1.0);
  alpha_prior ~ dirichlet(rep_vector(2, NBlocs));
  for (nn in 1:NElections_past_fundamentals){
    {
      vector[NBlocs] mu;
      mu = softmax(alpha[,nn]);
      target += multinomial_lpmf(y_fundamentals[nn] | mu);
    }
  }
}
generated quantities {
  matrix[NBlocs, NElections_past_fundamentals + 1] prob_alpha;
  for (nn in 1:NElections_past_fundamentals)
    prob_alpha[,nn] = softmax(alpha[,nn]);
}




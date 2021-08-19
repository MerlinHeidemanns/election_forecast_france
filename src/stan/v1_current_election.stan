data {
  int N;
  int P;
  int R;
  int T;
  int T_prior;
  vector[P] theta_prior;
  int t[N];
  int r[N];
  int<lower = 0> y[P, N];
}
transformed data {
  real lsigma = 0.0001;
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  vector<lower = lsigma>[P] sigma_cov;
  matrix[P, N] raw_tau;
  matrix[P, R] raw_alpha;
  matrix[P, T] raw_theta;
  vector[P] raw_xi;
  cholesky_factor_corr[P] cholesky_corr_theta;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, N] tau;
  matrix[P, R] alpha;
  vector[P] xi;
  cholesky_factor_cov[P] cholesky_cov_theta;
  cholesky_cov_theta = diag_pre_multiply(sigma_cov, cholesky_corr_theta);

  for (ii in 1:N){
    tau[, ii] = raw_tau[, ii] - mean(raw_tau[, ii]);
  }
  for (ii in 1:R){
    alpha[, ii] = raw_alpha[, ii] - mean(raw_alpha[, ii]);
  }
  xi = raw_xi - mean(raw_xi);
  theta[, 1] = sqrt(T_prior) * cholesky_cov_theta * raw_theta[:, 1] + theta_prior;
  for (tt in 2:T){
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  }
}
model {
  sigma_xi ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  sigma_cov ~ normal(0, 0.1);
  to_vector(raw_xi) ~ normal(0, sigma_xi);
  to_vector(raw_alpha) ~ normal(0, sigma_alpha);
  to_vector(raw_tau) ~ normal(0, sigma_tau);
  cholesky_corr_theta ~ lkj_corr_cholesky(2.0);
  to_vector(raw_theta) ~ std_normal();
  for (ii in 1:N){
    target += multinomial_lpmf(y[, ii] |
      softmax(theta[, t[ii]] +
        tau[, ii] +
        alpha[, r[ii]] +
        xi));
  }
}
generated quantities {
  matrix[P, T] pi_theta;
  matrix[P, P] cov_theta;
  cov_theta = cholesky_cov_theta * cholesky_cov_theta';
  for (tt in 1:T){
    pi_theta[, tt] = softmax(theta[, tt]);
  }
}


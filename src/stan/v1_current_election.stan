data {
  int N;
  int P;
  int R;
  int T;
  int t[N];
  int r[N];
  int<lower = 0> y[P, N];
}
transformed data {
  real lsigma = 0.0001;
}
parameters {
  real<lower = lsigma> sigma_tau;
  matrix[P, N] raw_tau;
  matrix[P, T] raw_theta;
  cholesky_factor_corr[P] cholesky_cov_theta;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, N] tau;
  for (ii in 1:N){
    tau[, ii] = raw_tau[, ii] - mean(raw_tau[, ii]);
  }
  theta[, 1] = cholesky_cov_theta * raw_theta[:, 1];
  for (tt in 2:T){
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  }
}
model {
  sigma_tau ~ normal(0, 0.1);
  to_vector(raw_tau) ~ normal(0, sigma_tau);
  cholesky_cov_theta ~ lkj_corr_cholesky(1.0);
  to_vector(raw_theta) ~ std_normal();
  for (ii in 1:N){
    target += multinomial_lpmf(y[, ii] | softmax(theta[, t[ii]] + tau[, ii]));
  }
}
generated quantities {
  matrix[P, T] pi_theta;
  for (tt in 1:T){
    pi_theta[, tt] = softmax(theta[, tt]);
  }
}


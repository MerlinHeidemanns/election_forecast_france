data {
  int N;
  int P;
  int R;
  int T;
  int t[N];
  int r[N];
  int<lower = 0> y[P, N];
}
parameters {
  matrix[P, T] raw_theta;
  cholesky_factor_corr[P] cholesky_cov_theta;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, T] pi_theta;
  theta[, 1] = cholesky_cov_theta * raw_theta[:, 1];
  for (tt in 2:T){
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  }
  for (tt in 1:T){
    pi_theta[, tt] = softmax(theta[, tt]);
  }
}
model {
  cholesky_cov_theta ~ lkj_corr_cholesky(1.0);
  to_vector(raw_theta) ~ std_normal();
  for (ii in 1:N){
    target += multinomial_lpmf(y[, ii] | pi_theta[, t[ii]]);
  }
}



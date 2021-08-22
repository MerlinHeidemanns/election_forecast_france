data {
  int N_first_round;
  int N_second_round;
  int P;
  int R;
  int T;
  int T_prior;
  vector[P] theta_prior;
  int t_first_round[N_first_round];
  int t_second_round[N_second_round];
  int r_first_round[N_first_round];
  int r_second_round[N_second_round];
  int<lower = 0> y_first_round[P, N_first_round];
  int<lower = 0> y_second_round[N_second_round];
  int<lower = 0> n_second_round[N_second_round];

  int N_past;
  int N_first_round_past;
  int N_second_round_past;
  int P_past[N_past];
  int R_past;
  matrix[N_past, max(P_past)] pi_theta_first_round_past;
  vector[N_past] pi_theta_second_round_past;
  int t_first_round_past[N_first_round_past];
  int t_second_round_past[N_second_round_past];
  int r_first_round_past[N_first_round_past];
  int r_second_round_past[N_second_round_past];
  int<lower = 0> y_first_round_past[max(P), N_first_round_past];
  int<lower = 0> y_second_round_past[N_second_round_past];
  int<lower = 0> n_second_round_past[N_second_round_past];
}
transformed data {
  real lsigma = 0.0001;
  vector[P - 2] conditional_values = rep_vector(0, P - 2);
  matrix[N_past, max(P_past)] theta_first_round_past;
  for (ii in 1:N_past){
    theta_first_round_past[ii, 1:(P_past[N_past] - 1)] =
      log(pi_theta_first_round_past[1:(P_past[N_past] - 1)]/pi_theta_first_round_past[P_past[N_past]]);
    theta_first_round_past[ii, P_past[N_past]] = 0;
  }
  vector[N_past] theta_second_round_past = logit(pi_theta_second_round_past);

}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  vector<lower = lsigma>[P] sigma_cov;
  matrix[P, N_first_round] raw_tau_first_round;
  matrix[P, N_second_round] raw_tau_second_round;
  matrix[P, R] raw_alpha;
  matrix[P, T] raw_theta;
  vector[P] raw_xi;
  cholesky_factor_corr[P] cholesky_corr_theta;
  // Past election
  matrix[max(P_past), N_first_round_past] raw_tau_first_round_past;
  vector[N_second_round_past] raw_tau_second_round_past;
  matrix[N_past, max(P_past)] raw_xi_past;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, N_first_round] tau_first_round;
  matrix[P, N_second_round] tau_second_round;
  matrix[P, R] alpha;
  vector[P] xi;
  cholesky_factor_cov[P] cholesky_cov_theta;
  matrix[P, P] cov_theta;
  matrix[2, P - 2] cov_beta;
  vector[N_second_round] pi_beta;
  cholesky_cov_theta = diag_pre_multiply(sigma_cov, cholesky_corr_theta);

  for (ii in 1:N_first_round)
    tau_first_round[, ii] = raw_tau_first_round[, ii] - mean(raw_tau_first_round[, ii]);
  for (ii in 1:N_second_round)
    tau_second_round[, ii] = raw_tau_second_round[, ii] - mean(raw_tau_second_round[, ii]);
  for (ii in 1:R)
    alpha[, ii] = raw_alpha[, ii] - mean(raw_alpha[, ii]);
  xi = raw_xi - mean(raw_xi);

  theta[, 1] = sqrt(T_prior) * cholesky_cov_theta * raw_theta[:, 1] + theta_prior;
  for (tt in 2:T){
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  }

  cov_theta = cholesky_cov_theta * cholesky_cov_theta';
  cov_beta = cov_theta[1:2, 3:P] * inverse(cov_theta[3:P, 3:P]);

  for (ii in 1:N_second_round){
    {
      vector[P] tmp;
      vector[2] beta;
      tmp = theta[, t_second_round[ii]] +
        tau_second_round[, ii] +
          alpha[, r_second_round[ii]] +
          xi;
      beta = tmp[1:2] + cov_beta * (conditional_values - tmp[3:P]);
      pi_beta[ii] = softmax(pi_beta)[1];
    }
  }
}
model {
  sigma_xi ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  sigma_cov ~ normal(0, 0.1);
  to_vector(raw_xi) ~ normal(0, sigma_xi);
  to_vector(raw_alpha) ~ normal(0, sigma_alpha);
  to_vector(raw_tau_first_round) ~ normal(0, sigma_tau);
  to_vector(raw_tau_second_round) ~ normal(0, sigma_tau);
  cholesky_corr_theta ~ lkj_corr_cholesky(2.0);
  to_vector(raw_theta) ~ std_normal();
  for (ii in 1:N_first_round){
    target += multinomial_lpmf(y_first_round[, ii] |
      softmax(theta[, t_first_round[ii]] +
        tau_first_round[, ii] +
        alpha[, r_first_round[ii]] +
        xi));
  }
}
generated quantities {
  matrix[P, T] pi_theta_first_round;
  matrix[2, T] theta_second_round;
  matrix[2, T] pi_theta_second_round;
  for (tt in 1:T){
    pi_theta_first_round[, tt] = softmax(theta[, tt]);
  }
  for (tt in 1:T){
    theta_second_round[:, tt] = theta[1:2, tt] + cov_beta * (conditional_values - theta[3:P, tt]);
    pi_theta_second_round[:, tt] = softmax(theta_second_round[:, tt]);
  }
}


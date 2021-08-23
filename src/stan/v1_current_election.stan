data {
  int N_first_round;
  int N_second_round;
  int P;
  int P_past_present;
  int R;
  int T;
  int T_prior;
  vector[P_past_present] theta_prior;
  int t_first_round[N_first_round];
  int t_second_round[N_second_round];
  int r_first_round[N_first_round];
  int r_second_round[N_second_round];
  int<lower = 0> y_first_round[P, N_first_round];
  int<lower = 0> y_second_round[N_second_round];
  int<lower = 0> n_second_round[N_second_round];
}
transformed data {
  real lsigma = 0.0001;
  vector[P_past_present - P] conditional_values_one =
    rep_vector(0, P_past_present - P);
  vector[P - 2] conditional_values_two = rep_vector(0, P - 2);
}
parameters {
  vector[P_past_present] std_theta_prior;
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  vector<lower = lsigma>[P_past_present] sigma_cov;
  matrix[P, N_first_round] raw_tau_first_round;
  matrix[P, N_second_round] raw_tau_second_round;
  matrix[P, R] raw_alpha;
  matrix[P, T] raw_theta;
  vector[P] raw_xi;
  cholesky_factor_corr[P_past_present] cholesky_corr_theta;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, N_first_round] tau_first_round;
  matrix[P, N_second_round] tau_second_round;
  matrix[P, R] alpha;
  vector[P] xi;
  cholesky_factor_cov[P_past_present] cholesky_cov_theta_past;
  matrix[P_past_present, P_past_present] cov_theta_past;
  matrix[P, P] cholesky_cov_theta;
  matrix[P, P] cov_theta;
  vector[N_second_round] pi_beta;
  cholesky_cov_theta_past = diag_pre_multiply(sigma_cov, cholesky_corr_theta);
  cov_theta_past = cholesky_cov_theta_past * cholesky_cov_theta_past';
  {
    vector[P_past_present] tmp;
    int cut;
    matrix[P_past_present - P, P_past_present - P] inv_cov_theta_past;
    cut = P + 1;
    inv_cov_theta_past = inverse_spd(cov_theta_past[cut:P_past_present, cut:P_past_present]);
    tmp = sqrt(T_prior) * cholesky_cov_theta_past * std_theta_prior + theta_prior;
    theta[, 1] = tmp[1:P] + cov_theta_past[1:P, cut:P_past_present] *
      inv_cov_theta_past * (conditional_values_one - tmp[cut:P_past_present]);
    cov_theta = cov_theta_past[1:P, 1:P] - cov_theta_past[1:P, cut:P_past_present] *
      inv_cov_theta_past * cov_theta_past[cut:P_past_present, 1:P];
    cholesky_cov_theta = cholesky_decompose(cov_theta);
  }

  for (ii in 1:N_first_round)
    tau_first_round[, ii] = raw_tau_first_round[, ii] - mean(raw_tau_first_round[, ii]);
  for (ii in 1:N_second_round)
    tau_second_round[, ii] = raw_tau_second_round[, ii] - mean(raw_tau_second_round[, ii]);
  for (ii in 1:R)
    alpha[, ii] = raw_alpha[, ii] - mean(raw_alpha[, ii]);
  xi = raw_xi - mean(raw_xi);
  for (tt in 2:T){
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  }

  for (ii in 1:N_second_round){
    {
      vector[P] tmp;
      vector[2] beta;
      matrix[2, P - 2] cov_beta;
      cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
      tmp = theta[, t_second_round[ii]] +
        tau_second_round[, ii] +
          alpha[, r_second_round[ii]] +
          xi;
      beta = tmp[1:2] + cov_beta * (conditional_values_two - tmp[3:P]);
      pi_beta[ii] = softmax(beta)[1];
    }
  }
}
model {
  sigma_xi ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  sigma_cov ~ normal(0, 0.1);
  std_theta_prior ~ std_normal();
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
  y_second_round ~ binomial(n_second_round, pi_beta);
}
generated quantities {
  matrix[P, T] pi_theta_first_round;
  matrix[2, T] theta_second_round;
  matrix[2, T] pi_theta_second_round;
  for (tt in 1:T){
    pi_theta_first_round[, tt] = softmax(theta[, tt]);
  }
  for (tt in 1:T){
    {
      matrix[2, P - 2] cov_beta;
      cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
        theta_second_round[:, tt] = theta[1:2, tt] + cov_beta * (conditional_values_two - theta[3:P, tt]);
        pi_theta_second_round[:, tt] = softmax(theta_second_round[:, tt]);
    }
  }
}


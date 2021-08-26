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
  // past
  int N_elections_past;
  int N_first_round_past;
  int P_past[N_elections_past];
  int R_past;
  int<lower = 1, upper = R_past> r_past[N_first_round_past];
  int<lower = 1, upper = N_elections_past> rt_past[R_past];
  int<lower = 1, upper = N_elections_past> t_past[N_first_round_past];
  matrix[max(P_past), N_elections_past] results;
  int<lower = 0> y_first_round_past[max(P_past), N_first_round_past];
}
transformed data {
  real lsigma = 0.0001;
  vector[P_past_present - P] conditional_values_one =
    rep_vector(0, P_past_present - P);
  vector[P - 2] conditional_values_two = rep_vector(0, P - 2);
  matrix[max(P_past), N_elections_past] theta_results = rep_matrix(0.0, max(P_past), N_elections_past);
  int rt_past_count = sum(P_past[rt_past]);
  for (ii in 1:N_elections_past){
    theta_results[1:P_past[ii], ii] = log(results[1:P_past[ii], ii]/results[P_past[ii], ii]);
  }

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
  matrix[max(P_past), R_past] raw_alpha_past;
  matrix[max(P_past), N_first_round_past] raw_tau_first_round_past;
  matrix[P, T] raw_theta;
  vector[P] raw_xi;
  matrix[max(P_past), N_elections_past] raw_xi_past;
  cholesky_factor_corr[P_past_present] cholesky_corr_theta;
}
transformed parameters {
  matrix[P, T] theta;
  matrix[P, N_first_round] tau_first_round;
  matrix[P, N_second_round] tau_second_round;
  matrix[P, R] alpha;
  matrix[max(P_past), R_past] alpha_past = rep_matrix(0.0, max(P_past), R_past);
  matrix[max(P_past), N_first_round_past] tau_first_round_past;
  vector[P] xi;
  matrix[max(P_past), N_elections_past] xi_past;
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
    matrix[P, P_past_present - P] left_inv_cov_theta_past;
    cut = P + 1;
    left_inv_cov_theta_past = cov_theta_past[1:P, cut:P_past_present] /
      cov_theta_past[cut:P_past_present, cut:P_past_present];
    tmp = sqrt(T_prior) * cholesky_cov_theta_past * std_theta_prior + theta_prior;
    theta[, 1] = tmp[1:P] + left_inv_cov_theta_past *
      (conditional_values_one - tmp[cut:P_past_present]);
    cov_theta = cov_theta_past[1:P, 1:P] - left_inv_cov_theta_past * cov_theta_past[cut:P_past_present, 1:P];
    cholesky_cov_theta = cholesky_decompose(cov_theta);
  }

  // -- Current polling data
  // Demean parameters
  for (ii in 1:N_first_round)
    tau_first_round[, ii] = raw_tau_first_round[, ii] - mean(raw_tau_first_round[, ii]);
  for (ii in 1:N_second_round)
    tau_second_round[, ii] = raw_tau_second_round[, ii] - mean(raw_tau_second_round[, ii]);
  for (ii in 1:R)
    alpha[, ii] = raw_alpha[, ii] - mean(raw_alpha[, ii]);
  xi = raw_xi - mean(raw_xi);
  // Random walk
  for (tt in 2:T)
    theta[, tt] = cholesky_cov_theta * raw_theta[:, tt] + theta[:, tt - 1];
  // -- Past polling data
  // Demean parameters
  for (ii in 1:N_first_round_past){
    tau_first_round_past[1:P_past[t_past[ii]]] = raw_tau_first_round_past[1:P_past[t_past[ii]]] -
      mean(raw_tau_first_round_past[1:P_past[t_past[ii]]]);
  }
  for (ii in 1:R_past)
      alpha_past[1:P_past[rt_past[ii]], ii] = raw_alpha_past[1:P_past[rt_past[ii]], ii] -
        mean(raw_alpha_past[1:P_past[rt_past[ii]], ii]);
  for (ii in 1:N_elections_past)
    xi_past[1:P_past[ii], ii] = raw_xi_past[1:P_past[ii], ii] - mean(raw_xi_past[1:P_past[ii], ii]);




  {
    matrix[2, P - 2] cov_beta;
    cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
    for (ii in 1:N_second_round){
      {
        vector[P] tmp;
        vector[2] beta;
        tmp = theta[, t_second_round[ii]] +
          tau_second_round[, ii] +
            alpha[, r_second_round[ii]] +
            xi;
        beta = tmp[1:2] + cov_beta * (conditional_values_two - tmp[3:P]);
        pi_beta[ii] = softmax(beta)[1];
      }
    }
  }


}
model {
  // -- Current polling data
  sigma_xi ~ normal(0, 0.3);
  sigma_alpha ~ normal(0, 0.3);
  sigma_tau ~ normal(0, 0.3);
  sigma_cov ~ normal(0, 0.3);
  to_vector(raw_xi) ~ normal(0, sigma_xi);
  to_vector(raw_alpha) ~ normal(0, sigma_alpha);
  to_vector(raw_tau_first_round) ~ normal(0, sigma_tau);
  to_vector(raw_tau_second_round) ~ normal(0, sigma_tau);
  // Random walk
  std_theta_prior ~ std_normal();
  to_vector(raw_theta) ~ std_normal();
  cholesky_corr_theta ~ lkj_corr_cholesky(2.0);
  // Likelihood (first round)
  for (ii in 1:N_first_round){
    target += multinomial_lpmf(y_first_round[, ii] |
      softmax(theta[, t_first_round[ii]] +
        tau_first_round[, ii] +
        alpha[, r_first_round[ii]] +
        xi));
  }
  // Likelihood (second round)
  y_second_round ~ binomial(n_second_round, pi_beta);
  // -- Past polling data
  // Priors
  to_vector(raw_alpha_past) ~ normal(0, sigma_alpha);
  to_vector(raw_xi_past) ~ normal(0, sigma_xi);
  to_vector(raw_tau_first_round_past) ~ normal(0, sigma_tau);
  // Likelihood
  for (ii in 1:N_first_round_past){
    target += multinomial_lpmf(y_first_round_past[1:P_past[t_past[ii]], ii] |
      softmax(theta_results[1:P_past[t_past[ii]], t_past[ii]] +
        alpha_past[1:P_past[rt_past[r_past[ii]]], r_past[ii]] +
        xi_past[1:P_past[t_past[ii]], t_past[ii]] +
        tau_first_round_past[1:P_past[t_past[ii]], ii]));
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
    {
      matrix[2, P - 2] cov_beta;
      cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
        theta_second_round[:, tt] = theta[1:2, tt] + cov_beta * (conditional_values_two - theta[3:P, tt]);
        pi_theta_second_round[:, tt] = softmax(theta_second_round[:, tt]);
    }
  }
}


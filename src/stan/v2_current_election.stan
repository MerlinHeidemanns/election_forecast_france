data {
  int S_1r_surveys; // # first round surveys
  int<lower = S_1r_surveys> N_1r; // # first round polls
  int N_2r; // # second round polls
  int P;
  int R;
  int T_unit;
  vector[T_unit - 1] t_unit_skip;

  // First round
  int<lower = 1, upper = S_1r_surveys> s_1r[N_1r];
  int r_1r[S_1r_surveys];
  int t_unit_1r[S_1r_surveys];

  // Second round
  int t_unit_2r[N_2r];
  int r_2r[N_2r];
  int<lower = 0> y_2r[N_2r];
  int<lower = 0> n_2r[N_2r];

  // variable inclusion
  int P_1r[N_1r];
  int N_combinations;
  int P_N_combinations[N_combinations];
  int p_1r_included[N_combinations, P];
  int p_1r_excluded[N_combinations, P];
  int<lower = 1, upper = N_combinations> p_id[N_1r];
  int y_1r[P, N_1r];

  // past
  int N_elections_past;
  int N_1r_past;
  int P_past[N_elections_past];
  int R_past;
  int<lower = 1, upper = R_past> r_past[N_1r_past];
  int<lower = 1, upper = N_elections_past> rt_past[R_past];
  int<lower = 1, upper = N_elections_past> t_past[N_1r_past];
  matrix[max(P_past), N_elections_past] results;
  int<lower = 0> y_1r_past[max(P_past), N_1r_past];
}
transformed data {
  real lsigma = 0.0001;
  // Conditional values for subsetting:
  // * Previous election to current one
  // * Runoff
  // * Subsets currently
  vector[P] conditional_values = rep_vector(0.0, P);
  matrix[max(P_past), N_elections_past] theta_results = rep_matrix(0.0, max(P_past), N_elections_past);
  int not_P_N_combinations[N_combinations];
  vector[T_unit] t_unit_skip_sqrt = sqrt(t_unit_skip);
  for (ii in 1:N_combinations)
    not_P_N_combinations[ii] = P - P_N_combinations[ii];
  for (ii in 1:N_elections_past){
    theta_results[1:P_past[ii], ii] = log(results[1:P_past[ii], ii]/results[P_past[ii], ii]);
  }
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  matrix[P - 1, S_1r_surveys] raw_tau_1r;
  matrix[P - 1, N_2r] raw_tau_2r;
  matrix[P - 1, R] raw_alpha;
  matrix[max(P_past) - 1, R_past] raw_alpha_past;
  matrix[max(P_past), N_1r_past] raw_tau_1r_past;
  vector[P - 1] raw_xi;
  matrix[max(P_past) - 1, N_elections_past] raw_xi_past;
  // Random walk
  simplex[P] theta_prior;
  vector<lower = lsigma>[P] sigma_cov;
  matrix[P, T_unit] raw_theta;
  cholesky_factor_corr[P] cholesky_corr_theta;
}
transformed parameters {
  matrix[P, T_unit] theta;
  matrix[P, S_1r_surveys] tau_1r;
  matrix[P, N_2r] tau_2r;
  matrix[P, R] alpha;
  matrix[max(P_past), R_past] alpha_past = rep_matrix(0.0, max(P_past), R_past);
  matrix[max(P_past), N_1r_past] tau_1r_past;
  vector[P] xi;
  matrix[max(P_past), N_elections_past] xi_past;

  // Covariance matrizes
  cholesky_factor_cov[P] cholesky_cov_theta;
  matrix[P, P] cov_theta;
  matrix[max(P_N_combinations), max(P_N_combinations)] left_inv_cov_theta_comb[N_combinations];

  // Containers
  vector[N_2r] pi_beta;

  // Determine current covariance matrix
  cholesky_cov_theta = diag_pre_multiply(sigma_cov, cholesky_corr_theta);
  cov_theta = cholesky_cov_theta * cholesky_cov_theta';

  for (ii in 1:N_combinations){
    {
      matrix[P_N_combinations[ii], P - P_N_combinations[ii]] mat1;
      matrix[P - P_N_combinations[ii], P - P_N_combinations[ii]] mat2;
      mat1 = cov_theta[p_1r_included[ii, 1:P_N_combinations[ii]], p_1r_excluded[ii, 1:not_P_N_combinations[ii]]];
      mat2 = cov_theta[p_1r_excluded[ii, 1:not_P_N_combinations[ii]], p_1r_excluded[ii, 1:not_P_N_combinations[ii]]];
      left_inv_cov_theta_comb[ii, 1:P_N_combinations[ii], 1:not_P_N_combinations[ii]] =  mat1 / mat2;
    }
  }

  // -- Current polling data
  // Sum to 0 constraints
  for (ii in 1:S_1r_surveys)
    tau_1r[, ii] = append_row(
      raw_tau_1r[, ii],- sum(raw_tau_1r[, ii]));

  for (ii in 1:N_2r)
    tau_2r[, ii] = append_row(
      raw_tau_2r[, ii], -sum(raw_tau_2r[, ii]));
  // for (ii in 1:R)
  //   alpha[, ii] = append_row(raw_alpha[, ii], -sum(raw_alpha[, ii]));
  //
  alpha[1:(P - 1), 1:(R - 1)] = raw_alpha[1:(P - 1), 1:(R - 1)];
  for (ii in 1:(P - 1)) alpha[ii, R] = -sum(alpha[ii,1:(R - 1)]);
  for (ii in 1:(R)) alpha[P, ii] = -sum(alpha[1:(P - 1), ii]);

  xi = append_row(raw_xi, -sum(raw_xi));

  // Random walk
  theta[, 1] = theta_prior;
  for (tt in 2:T_unit)
    theta[, tt] = t_unit_skip_sqrt[tt - 1] * cholesky_cov_theta * raw_theta[:, tt] + theta[, tt - 1];

  // -- Past polling data
  // Sum to 0 constraints
  for (ii in 1:N_1r_past){
    tau_1r_past[1:P_past[t_past[ii]], ii] =
      append_row(raw_tau_1r_past[1:(P_past[t_past[ii]] - 1), ii],
            -sum(raw_tau_1r_past[1:(P_past[t_past[ii]] - 1), ii]));
  }
  for (ii in 1:R_past)
      alpha_past[1:P_past[rt_past[ii]], ii] = append_row(
        raw_alpha_past[1:(P_past[rt_past[ii]] - 1), ii],
        - sum(raw_alpha_past[1:(P_past[rt_past[ii]] - 1), ii]));
  for (ii in 1:N_elections_past)
    xi_past[1:P_past[ii], ii] =
      append_row(raw_xi_past[1:(P_past[ii] - 1), ii],
            -sum(raw_xi_past[1:(P_past[ii] - 1), ii]));

  {
    matrix[2, P - 2] cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
    for (ii in 1:N_2r){
      {
        vector[P] tmp;
        vector[2] beta;
        tmp = theta[, t_unit_2r[ii]] +
          tau_2r[, ii] +
          alpha[, r_2r[ii]] +
          xi;
        beta = tmp[1:2] + cov_beta * (conditional_values[3:P] - tmp[3:P]);
        pi_beta[ii] = beta[1];
      }
    }
  }


}
model {
  // -- Current polling data
  sigma_xi ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  sigma_cov ~ normal(0, 0.1);
  to_vector(raw_xi) ~ normal(0, sigma_xi);
  to_vector(raw_alpha) ~ normal(0, sigma_alpha);
  to_vector(raw_tau_1r) ~ normal(0, sigma_tau);
  to_vector(raw_tau_2r) ~ normal(0, sigma_tau);

  // Random walk
  theta_prior ~ dirichlet(rep_vector(1, P)) ;
  to_vector(raw_theta) ~ std_normal();
  cholesky_corr_theta ~ lkj_corr_cholesky(1.0);

  // Likelihood (first round)
  // * Get the indexes for the included and excluded candidates
  // * Create a container for the complete vector
  // * Create a subset of those observed for the specific poll using the
  // * correct left_inv of the covariance matrix cov_theta
  for (ii in 1:N_1r){
    {
      vector[P] pi_theta_complete;
      vector[P_1r[ii]] pi_theta_subset;
      int p_id_ii = p_id[ii];
      int P_1r_ii = P_1r[ii];
      int index_included[P_1r_ii] = p_1r_included[p_id_ii, 1:P_1r_ii];
      int index_excluded[P - P_1r_ii] = p_1r_excluded[p_id_ii, 1:(P - P_1r_ii)];
      pi_theta_complete = theta[, t_unit_1r[s_1r[ii]]] +
        tau_1r[, s_1r[ii]] +
        alpha[, r_1r[s_1r[ii]]] +
        xi;
      pi_theta_subset = pi_theta_complete[index_included] -
        left_inv_cov_theta_comb[p_id_ii, 1:P_N_combinations[p_id_ii], 1:not_P_N_combinations[p_id_ii]] *
        (conditional_values[1:not_P_N_combinations[p_id_ii]] - pi_theta_complete[index_excluded]);
      target += multinomial_lpmf(y_1r[1:P_N_combinations[p_id_ii], ii] |
                                 pi_theta_subset);
    }
  }
  // Likelihood (second round)
  y_2r ~ binomial(n_2r, pi_beta);
  // -- Past polling data
  // Priors
  to_vector(raw_alpha_past) ~ normal(0, sigma_alpha);
  to_vector(raw_xi_past) ~ normal(0, sigma_xi);
  to_vector(raw_tau_1r_past) ~ normal(0, sigma_tau);
  // Likelihood
  for (ii in 1:N_1r_past){
    target += multinomial_lpmf(y_1r_past[1:P_past[t_past[ii]], ii] |
      theta_results[1:P_past[t_past[ii]], t_past[ii]] +
        alpha_past[1:P_past[rt_past[r_past[ii]]], r_past[ii]] +
        xi_past[1:P_past[t_past[ii]], t_past[ii]] +
        tau_1r_past[1:P_past[t_past[ii]], ii]);
  }
}
generated quantities {
  matrix[P, T_unit] pi_theta_1r;
  matrix[2, T_unit] theta_2r;
  matrix[2, T_unit] pi_theta_2r;
  for (tt in 1:T_unit){
    pi_theta_1r[, tt] = theta[, tt];
  }
  for (tt in 1:T_unit){
    {
      matrix[2, P - 2] cov_beta;
      cov_beta = cov_theta[1:2, 3:P] / cov_theta[3:P, 3:P];
        theta_2r[:, tt] = theta[1:2, tt] + cov_beta * (conditional_values[3:P] - theta[3:P, tt]);
        pi_theta_2r[:, tt] = theta_2r[:, tt];
    }
  }
}

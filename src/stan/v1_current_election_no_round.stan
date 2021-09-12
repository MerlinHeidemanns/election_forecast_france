data {
  int S_1r_surveys; // # first round surveys
  int<lower = S_1r_surveys> N_1r; // # first round polls
  int P;
  int R;
  int T_unit;
  vector[T_unit - 1] t_unit_skip;

  // First round
  int<lower = 1, upper = S_1r_surveys> s_1r[N_1r];
  int r_1r[S_1r_surveys];
  int t_unit_1r[S_1r_surveys];

  // variable inclusion
  int P_1r[N_1r];
  int N_combinations;
  int P_N_combinations[N_combinations];
  int p_1r_included[N_combinations, P];
  int p_1r_excluded[N_combinations, P];
  int<lower = 1, upper = N_combinations> p_id[N_1r];
  int y_1r[P, N_1r];
}
transformed data {
  real lsigma = 0.0001;
  // Conditional values for subsetting:
  // * Previous election to current one
  // * Runoff
  // * Subsets currently
  vector[P] conditional_values = rep_vector(-2, P);
  int not_P_N_combinations[N_combinations];
  vector[T_unit] t_unit_skip_sqrt = sqrt(t_unit_skip);
  for (ii in 1:N_combinations)
    not_P_N_combinations[ii] = P - P_N_combinations[ii];
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  matrix[P - 1, S_1r_surveys] raw_tau_1r;
  matrix[P - 1, R] raw_alpha;
  vector[P - 1] raw_xi;
  // Random walk
  simplex[P] theta_prior;
  vector<lower = lsigma>[P] sigma_cov;
  matrix[P, T_unit] raw_theta;
  cholesky_factor_corr[P] cholesky_corr_theta;
}
transformed parameters {
  matrix[P, T_unit] theta;
  matrix[P, S_1r_surveys] tau_1r;
  matrix[P, R] alpha;
  vector[P] xi;

  // Covariance matrizes
  cholesky_factor_cov[P] cholesky_cov_theta;
  matrix[P, P] cov_theta;
  matrix[P, P] left_inv_cov_theta_comb[N_combinations];

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
    tau_1r[, ii] = append_row(-sum(raw_tau_1r[, ii]),
      raw_tau_1r[, ii]);

  alpha[1:(P - 1), 1:(R - 1)] = raw_alpha[1:(P - 1), 1:(R - 1)];
  for (ii in 1:(P - 1)) alpha[ii, R] = -sum(alpha[ii,1:(R - 1)]);
  for (ii in 1:(R)) alpha[P, ii] = -sum(alpha[1:(P - 1), ii]);

  xi = append_row(-sum(raw_xi), raw_xi);

  // Random walk
  theta[, 1] = log(theta_prior/theta_prior[1]);
  for (tt in 2:T_unit)
    theta[, tt] = t_unit_skip_sqrt[tt - 1] * cholesky_cov_theta * raw_theta[:, tt] + theta[, tt - 1];
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
                                 softmax(pi_theta_subset));
    }
  }
}
generated quantities {
  matrix[P, T_unit] pi_theta_1r;
  for (tt in 1:T_unit){
    pi_theta_1r[, tt] = softmax(theta[, tt]);
  }
}


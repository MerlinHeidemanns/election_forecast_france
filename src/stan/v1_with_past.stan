data {
  int NSurveys;
  int<lower = NSurveys> NPolls;
  int NCandidates;
  int NPollsters;
  int NTime;
  vector[NTime - 1] t_unit;

  // First round
  int<lower = 1, upper = NSurveys> id_P_survey[NPolls];
  int id_S_pollster[NSurveys];
  int id_S_time[NSurveys];

  // variable inclusion
  int NCandidates_Poll[NPolls];
  int NCombinations;
  int NCandidate_Combinations[NCombinations];
  int candidates_included[NCombinations, NCandidates];
  int candidates_excluded[NCombinations, NCandidates];
  int<lower = 1, upper = NCombinations> id_P_combinations[NPolls];
  int y[NCandidates, NPolls];

    // past
  int NElections_past;
  int NPolls_past;
  int NCandidates_past[NElections_past];
  int NPollsters_past;
  int<lower = 1, upper = NPollsters_past> id_r_past[NPolls_past]; // which pollster
  int<lower = 1, upper = NElections_past> id_rt_past[NPollsters_past]; // which election the pollster belongs to
  int<lower = 1, upper = NElections_past> id_t_past[NPolls_past];
  matrix[max(NCandidates_past), NElections_past] results;
  int<lower = 0> y_past[max(NCandidates_past), NPolls_past];
}
transformed data {
  real lsigma = 0.0001;
  // Conditional values for subsetting:
  // * Previous election to current one
  // * Runoff
  // * Subsets currently
  vector[NCandidates] zeros = rep_vector(0, NCandidates);
  row_vector[NTime] zeros_theta = rep_vector(0.0, NTime)';
  matrix[max(NCandidates_past), NElections_past] theta_results = rep_matrix(0.0, max(NCandidates_past), NElections_past);
  int NCandidate_Combinations_neg[NCombinations];
  int corr_mat_positions[NCandidates, NCandidates - 1];

  int NAlpha_past = 0;
  int NTau_past = 0;
  int NXi_past = 0;

  vector[NTime] t_unit_sqrt = sqrt(t_unit);
  for (ii in 1:NCombinations)
    NCandidate_Combinations_neg[ii] = NCandidates - NCandidate_Combinations[ii];
  for (ii in 1:NCandidates){
    for (jj in 1:NCandidates - 1){
      if (jj >= ii){
        corr_mat_positions[ii, jj] = jj + 1;
      } else {
        corr_mat_positions[ii, jj] = jj;
      }
    }
  }

  for (ii in 1:NElections_past){
    theta_results[1:NCandidates_past[ii], ii] = log(results[1:NCandidates_past[ii], ii]/results[NCandidates_past[ii], ii]);
  }
  for (jj in 1:NElections_past) NXi_past += NCandidates_past[jj];
  for (jj in 1:NPollsters_past) NAlpha_past += NCandidates_past[id_rt_past[jj]];
  for (jj in 1:NPolls_past) NTau_past += NCandidates_past[id_t_past[jj]];
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  matrix[NCandidates, NSurveys] raw_tau;
  matrix[NCandidates, NPollsters] raw_alpha;
  vector[NCandidates] raw_xi;
  // Random walk
  vector<lower = lsigma>[NCandidates - 1] sigma_cov;
  cholesky_factor_corr[NCandidates - 1] chol_corr_theta;

  simplex[NCandidates] theta_prior;
  matrix[NCandidates - 1, NTime] raw_theta;
  simplex[NCandidates - 1] trans_prob[NCandidates];

  // -- Past parameters
  vector[NAlpha_past] raw_alpha_past;
  vector[NTau_past] raw_tau_past;
  vector[NXi_past] raw_xi_past;
}
transformed parameters {
  matrix[NCandidates, NTime] theta;
  matrix[NCandidates, NSurveys] tau;
  matrix[NCandidates, NPollsters] alpha;
  vector[NCandidates] xi;

  // Covariance matrizes
  cholesky_factor_cov[NCandidates - 1] chol_cov_theta;

  // transition matrix
  matrix[NCandidates, NCandidates] transition_matrix;
  matrix[NCandidates, NCandidates] left_inv_trans_comb[NCombinations];

  // Past parameters
  matrix[max(NCandidates_past), NPollsters_past] alpha_past = rep_matrix(-999, max(NCandidates_past), NPollsters_past);
  matrix[max(NCandidates_past), NPolls_past] tau_past = rep_matrix(-999, max(NCandidates_past), NPolls_past);
  matrix[max(NCandidates_past), NElections_past] xi_past = rep_matrix(-999, max(NCandidates_past), NElections_past);

  // -- Transition matrix
  // * Fill
  for (ii in 1:NCandidates){
    transition_matrix[ii, ii] = 1;
    transition_matrix[corr_mat_positions[ii], ii] = -trans_prob[ii];
  }

  // * Determine and store the left inverse
  for (ii in 1:NCombinations){
    {
      matrix[NCandidate_Combinations[ii],
             NCandidates - NCandidate_Combinations[ii]] mat1 =
          transition_matrix[
             candidates_included[ii, 1:NCandidate_Combinations[ii]],
             candidates_excluded[ii, 1:NCandidate_Combinations_neg[ii]]];
      matrix[NCandidates - NCandidate_Combinations[ii],
             NCandidates - NCandidate_Combinations[ii]] mat2 =
          transition_matrix[
             candidates_excluded[ii, 1:NCandidate_Combinations_neg[ii]],
             candidates_excluded[ii, 1:NCandidate_Combinations_neg[ii]]];
      left_inv_trans_comb[ii,
        1:NCandidate_Combinations[ii],
        1:NCandidate_Combinations_neg[ii]] = mat1 / mat2;
    }
  }

  // -- Current polling data
  // * Scale parameters
  tau = raw_tau * sigma_tau;
  alpha = raw_alpha * sigma_alpha;
  xi = raw_xi * sigma_xi;

  // -- Random walk
  // Determine current covariance matrix
  // * Premultiply
  // * log-odds ratio
  // * random walk with skip mechanic
  // * fill last row with 0s
  chol_cov_theta = diag_pre_multiply(sigma_cov, chol_corr_theta);
  theta[1:(NCandidates - 1), 1] = log(theta_prior[1:NCandidates - 1]/theta_prior[NCandidates]);
  for (tt in 2:NTime)
    theta[1:(NCandidates - 1), tt] = t_unit_sqrt[tt - 1] * chol_cov_theta * raw_theta[:, tt] + theta[1:(NCandidates - 1), tt - 1];
  theta[NCandidates] = zeros_theta;

  // -- Past polling data
  // * Scale parameters
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NPollsters_past){
      end += NCandidates_past[id_rt_past[jj]];
      alpha_past[1:NCandidates_past[id_rt_past[jj]], jj] = raw_alpha_past[start:end] * sigma_alpha;
      start = end + 1;
    }
  }
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NElections_past){
      end += NCandidates_past[jj];
      xi_past[1:NCandidates_past[jj], jj] =
        raw_xi_past[start:end] * sigma_xi;
      start = end + 1;
    }
  }
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NPolls_past){
      end += NCandidates_past[id_t_past[jj]];
      tau_past[1:NCandidates_past[id_t_past[jj]], jj] =
        raw_tau_past[start:end] * sigma_tau;
      start = end + 1;
    }
  }
}
model {
  // -- Current polling data
  // Standard deviations
  sigma_xi ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  sigma_cov ~ normal(0, 0.1);
  // Adjustment parameters
  // * Implement sum to zero constraints
  raw_xi ~ normal(0, inv(sqrt(1 - inv(NCandidates))));
  for (ii in 1:NPollsters) raw_alpha[,ii] ~ normal(0, inv(sqrt(1 - inv(NCandidates))));
  for (ii in 1:NSurveys) raw_tau[,ii] ~ normal(0, inv(sqrt(1 - inv(NCandidates))));

  // -- Random walk
  theta_prior ~ dirichlet(rep_vector(1, NCandidates)) ;
  to_vector(raw_theta) ~ std_normal();
  chol_corr_theta ~ lkj_corr_cholesky(1.0);

  // -- Transition probabilities
  for (ii in 1:NCandidates) trans_prob[ii] ~ dirichlet(rep_vector(1.0, NCandidates - 1));

  // -- Likelihood (first round)
  // * Get the indexes for the included and excluded candidates
  // * Create a container for the complete vector
  // * Create a subset of those observed for the specific poll using the
  // * correct left_inv of the transition matrix
  for (ii in 1:NPolls){
    {
      vector[NCandidates] prob_theta_complete;
      vector[NCandidates_Poll[ii]] prob_theta_subset;
      int id_P_combinations_ii = id_P_combinations[ii];
      int NCandidates_ii = NCandidates_Poll[ii];
      int index_included[NCandidates_ii] =
        candidates_included[
          id_P_combinations_ii,
          1:NCandidates_ii];
      int index_excluded[NCandidates - NCandidates_ii] =
        candidates_excluded[
          id_P_combinations_ii,
          1:(NCandidates - NCandidates_ii)];
      prob_theta_complete = softmax(theta[, id_S_time[id_P_survey[ii]]] +
        tau[, id_P_survey[ii]] +
        alpha[, id_S_pollster[id_P_survey[ii]]] +
        xi);
      prob_theta_subset = prob_theta_complete[index_included] +
        left_inv_trans_comb[id_P_combinations_ii,
          1:NCandidate_Combinations[id_P_combinations_ii],
          1:NCandidate_Combinations_neg[id_P_combinations_ii]] *
          (zeros[1:NCandidate_Combinations_neg[id_P_combinations_ii]] -
          prob_theta_complete[index_excluded]);
      target += multinomial_lpmf(y[1:NCandidate_Combinations[id_P_combinations_ii], ii] |
          prob_theta_subset);
    }
  }

  // Past elections
  // * Scale parameters
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NPollsters_past){
      end += NCandidates_past[id_rt_past[jj]];
      raw_alpha_past[start:end] ~ normal(0, inv(sqrt(1 - inv(NCandidates_past[id_rt_past[jj]]))));
      start = end + 1;
    }
  }
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NElections_past){
      end += NCandidates_past[jj];
      raw_xi_past[start:end] ~ normal(0, inv(sqrt(1 - inv(NCandidates_past[jj]))));
      start = end + 1;
    }
  }
  {
    int start = 1;
    int end = 0;
    for (jj in 1:NPolls_past){
      end += NCandidates_past[id_t_past[jj]];
      raw_tau_past[start:end] ~ normal(0,
        inv(sqrt(1 - inv(NCandidates_past[id_t_past[jj]]))));
      start = end + 1;
    }
  }
  // * Likelihood
  for (ii in 1:NPolls_past){
    target += multinomial_lpmf(y_past[1:NCandidates_past[id_t_past[ii]], ii] |
      softmax(theta_results[1:NCandidates_past[id_t_past[ii]], id_t_past[ii]] +
        alpha_past[1:NCandidates_past[id_rt_past[id_r_past[ii]]], id_r_past[ii]] +
         xi_past[1:NCandidates_past[id_t_past[ii]], id_t_past[ii]] +
         tau_past[1:NCandidates_past[id_t_past[ii]], ii]));
  }
}
generated quantities {
  matrix[NCandidates, NTime] prob_theta;
  vector[NCandidates] prob_xi;
  matrix[NCandidates, NPollsters] prob_alpha;
  vector[NCandidates] prob_sigma_cov;
  for (tt in 1:NTime){
    prob_theta[, tt] = softmax(theta[, tt]);
  }
  prob_xi = softmax(theta[,NTime] + xi) - softmax(theta[,NTime]);
  for (jj in 1:NPollsters)
    prob_alpha[,jj] = softmax(theta[, NTime] + alpha[,jj]) - softmax(theta[, NTime]);
  {
    vector[NCandidates] tmp = append_row(theta[1:NCandidates - 1,NTime] +
      chol_cov_theta * to_vector(normal_rng(
        rep_vector(0.0, NCandidates - 1), 1)), 0);
    prob_sigma_cov = softmax(tmp) - softmax(theta[, NTime]);
  }

}



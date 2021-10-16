data {
  int NSurveys;
  int NBlocs;
  int<lower = NSurveys> NPolls;
  int NCandidates;
  int NPollsters;
  int NTime;
  vector[NTime - 1] t_unit;

  // Current round
  int<lower = 1, upper = NCandidates> NBlocs_Candidates[NBlocs];
  int<lower = 1, upper = NBlocs> id_C_blocs[NCandidates];
  int<lower = 1, upper = NSurveys> id_P_survey[NPolls];
  int id_S_pollster[NSurveys];
  int id_S_time[NSurveys];
  int abstention_omitted[NPolls]; //0-1 absentee treated as last candidate;
  matrix[NCandidates, NCandidates - 1] transition_probability_prior;

  // variable inclusion
  int NCandidates_Poll[NPolls];
  int NCombinations;
  int NCandidate_Combinations[NCombinations];
  int candidates_included[NCombinations, NCandidates];
  int candidates_excluded[NCombinations, NCandidates];
  int<lower = 1, upper = NCombinations> id_P_combinations[NPolls];
  int y[NCandidates, NPolls];

  // past
  int NElections;
  int NPolls_past[NElections - 1];
  int NPollsters_past[NElections - 1];
  int NTime_past;
  real t_bloc_unit_prior; // time since last election
  vector[NTime_past - 1] t_unit_past;
  int<lower = 1, upper = NTime_past> id_P_time_past[sum(NPolls_past)];
  int<lower = 1, upper = NTime_past> id_P_pollster_past[sum(NPolls_past)];
  int<lower = 1, upper = NElections - 1> id_P_elections_past[sum(NPolls_past)];

  matrix[NBlocs, NElections] elections_results;
  int id_E_time[NElections];
  int y_past[NBlocs, sum(NPolls_past)];
  int abstention_omitted_past[sum(NPolls_past)];

  // priors
  real<lower = 0> prior_sigma_xi;
  real<lower = 0> prior_sigma_alpha;
  real<lower = 0> prior_sigma_tau;
  real<lower = 0> prior_sigma_cov;
}
transformed data {
  real lsigma = 0.0001;
  // Conditional values for subsetting:
  // * Previous election to current one
  // * Runoff
  // * Subsets currently
  vector[NCandidates] zeros = rep_vector(0, NCandidates);
  row_vector[NTime] zeros_theta = rep_vector(0.0, NTime)';
  int NCandidate_Combinations_neg[NCombinations];
  int corr_mat_positions[NCandidates, NCandidates - 1];

  int supvec_NPollsters_past[NElections - 1];
  int supvec_NPolls_past[NElections - 1];
  int supvec_NBlocs[NBlocs + 1];
  matrix[NBlocs - 1, NCandidates - 1] identity_bloc = rep_matrix(0.0, NBlocs - 1, NCandidates - 1);

  matrix[NBlocs, NElections] election_results_prob;



  vector[NTime - 1] t_unit_sqrt = sqrt(t_unit);
  vector[NTime_past - 1] t_unit_past_sqrt = sqrt(t_unit_past);
  vector[NTime_past] t_unit_past_zero = append_row(0, t_unit_past);
  real t_bloc_unit_sqrt_prior = sqrt(t_bloc_unit_prior);
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
  for (nn in 2:NCandidates){
    identity_bloc[id_C_blocs[nn] - 1, nn - 1] = 1;
  }

  supvec_NPollsters_past[1] = 0;
  for (jj in 2:(NElections - 1)) supvec_NPollsters_past[jj] = sum(NPollsters_past[1:(jj - 1)]);
  supvec_NPolls_past[1] = 0;
  for (jj in 2:(NElections - 1)) supvec_NPolls_past[jj] = sum(NPolls_past[1:(jj - 1)]);

  supvec_NBlocs[1] = 0;
  for (jj in 1:NBlocs) supvec_NBlocs[jj + 1] = sum(NBlocs_Candidates[1:jj]);

}
parameters {
  cholesky_factor_corr[NBlocs - 1] chol_corr_theta_blocs;
  vector<lower = lsigma>[NBlocs - 1] sigma_cov_blocs;
  real<lower = lsigma> mu_sigma_cov_blocs;
  real<lower = lsigma> sigma_sigma_cov_blocs;
  matrix[NBlocs - 1, NTime_past] raw_theta_blocs[2];
}
transformed parameters {
  matrix[NBlocs, NTime_past] theta_blocs;
  // Covariance matrizes
  cholesky_factor_cov[NBlocs - 1] chol_cov_theta_blocs;
  // -- Random walk
  chol_cov_theta_blocs = diag_pre_multiply(sigma_cov_blocs, chol_corr_theta_blocs);
  // // * Random walk over blocs
  theta_blocs[1] = rep_vector(0.0, NTime_past)';
  for (jj in 1:NElections){
    theta_blocs[, id_E_time[jj]] = log(elections_results[, jj]/elections_results[1,jj]);
  }
  ## Fill with election results
  for (jj in 2:NElections){
    {
      // Create containers
      int interval_length = id_E_time[jj] - id_E_time[jj - 1]; // id_E_time[1] = 1; this works
      matrix[interval_length, NBlocs - 1] past_interpolation;
      matrix[interval_length, NBlocs - 1] future_interpolation;
      // Place starting values in containers (logodds election results)
      past_interpolation[1] = theta_blocs[2:NBlocs,id_E_time[jj - 1]]';
      future_interpolation[1] = theta_blocs[2:NBlocs,id_E_time[jj]]';
      // Then elements one jump at a time
      for (qq in 2:interval_length){
        past_interpolation[qq] =   to_row_vector(sqrt(sum(t_unit_past[id_E_time[jj - 1]:id_E_time[jj - 1] + qq - 2])) * chol_cov_theta_blocs * raw_theta_blocs[1,, id_E_time[jj - 1] + qq - 1]);
        future_interpolation[qq] = to_row_vector(sqrt(sum(t_unit_past[1:id_E_time[jj] - 1]) - sum(t_unit_past[1:id_E_time[jj] - qq])) * chol_cov_theta_blocs * raw_theta_blocs[2,, id_E_time[jj - 1] + qq - 1]);
      }
      for (qq in 1:(NBlocs - 1)){
        past_interpolation[,qq] = cumulative_sum(past_interpolation[,qq]);
        future_interpolation[,qq] = reverse(cumulative_sum(future_interpolation[,qq]));
      }
      // Weighted average
      for (tt in (id_E_time[jj - 1] + 1):(id_E_time[jj] - 1)){
        theta_blocs[2:NBlocs,tt] =
          to_vector(
            fabs(sum(t_unit_past_zero[1:tt]) -
                sum(t_unit_past_zero[1:id_E_time[jj]])) * // full
              past_interpolation[tt - id_E_time[jj - 1] + 1] +
            fabs(sum(t_unit_past_zero[1:tt]) -
                sum(t_unit_past_zero[1:id_E_time[jj - 1]])) *
              future_interpolation[tt - id_E_time[jj - 1] + 1]
            /
            sum(t_unit_past_zero[1:id_E_time[jj]]) - sum(t_unit_past_zero[1:id_E_time[jj - 1]])
          );
      }
    }
  }

}
model {
  // // -- Current polling data
  // // Standard deviations
  mu_sigma_cov_blocs ~ normal(0, 0.1);
  sigma_sigma_cov_blocs ~ normal(0, 0.1);
  sigma_cov_blocs ~ normal(mu_sigma_cov_blocs, sigma_sigma_cov_blocs);

  chol_corr_theta_blocs ~ lkj_corr_cholesky(2.0);
  for (j in 1:2) to_vector(raw_theta_blocs[j]) ~ std_normal();
  // -- Likelihood (first round)
  // * Get the indexes for the included and excluded candidates
  // * Create a container for the complete vector
  // * Create a subset of those observed for the specific poll using the
  // * correct left_inv of the transition matrix
  // // Past elections
  // // * Scale parameters

  // // * Likelihood
  profile("past_likelihood"){
    for (ii in 1:sum(NPolls_past)){
      {
        int start = 1 + abstention_omitted_past[ii];
        vector[NBlocs] theta_past =
          softmax(
            theta_blocs[, id_P_time_past[ii]]);
        target += multinomial_lpmf(y_past[start:NBlocs, ii] |
          theta_past[start:NBlocs] / sum(theta_past[start:NBlocs])
        );
      }
    }
  }
}
generated quantities {
  matrix[NBlocs, NTime_past] prob_theta_blocs;
  for (tt in 1:NTime_past){
    prob_theta_blocs[, tt] = softmax(theta_blocs[, tt]);
  }
}



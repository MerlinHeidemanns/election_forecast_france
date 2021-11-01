data {
  int NPolls;
  int NBlocs;
  int NSeasons;
  int NCandidates[NSeasons];
  int NPollsters;
  int NTime[NSeasons];

  // Current election
  int<lower = 1, upper = NPolls> NPolls_Pollster[NPollsters];
  int<lower = 1, upper = NPollsters> NPollsters_Season[NSeasons];
  int id_C_blocs[NSeasons, max(NCandidates)];
  int<lower = 1, upper = NSeasons> id_Polls_season[NPolls];
  int<lower = 1, upper = NSeasons> id_Pollster_season[NPollsters];
  int id_P_pollster[NPolls];
  int id_P_time[NPolls];
  int abstention_omitted[NPolls]; //0-1 absentee treated as last candidate;

  // variable inclusion
  int y[max(NCandidates), NPolls];

  // priors
  real<lower = 0> prior_sigma_xi;
  real<lower = 0> prior_sigma_alpha;
  real<lower = 0> prior_sigma_tau;
  real<lower = 0> prior_sigma_cov;
}
transformed data {
  real lsigma = 0.0001;
  vector[max(NCandidates)] zeros = rep_vector(0, max(NCandidates));
  row_vector[max(NTime)] zeros_theta = rep_vector(0.0, max(NTime))';
  int supvec_NSurveys_Pollsters[NPollsters];
  int supvec_NPollsters_Seasons[NSeasons];

  // Support vectors to correctly implement sum to zero constraints within the subgroups
  // * Surveys current
  supvec_NSurveys_Pollsters[1] = 0;
  for (jj in 2:(NPollsters)) supvec_NSurveys_Pollsters[jj] = sum(NSurveys_Pollster[1:(jj - 1)]);

  supvec_NPollsters_Seasons[1] = 0;
  for (jj in 2:NSeasons) supvec_NPollsters_Seasons[jj] = sum(NPollsters_Season[1:(jj - 1)]);
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha[2];
  real<lower = lsigma> sigma_xi;

  matrix[max(NCandidates), NSurveys] raw_tau;
  matrix[max(NCandidates), NPollsters] raw_alpha;
  // Random walk
  matrix<lower = lsigma>[max(NCandidates) - 1, NSeasons] sigma_cov;
  cholesky_factor_corr[max(NCandidates) - 1] chol_corr_theta_candidates[NSeasons];

  simplex[max(NCandidates)] prior_theta_candidates[NSeasons]; // because a subset of the elements of a dirichlet is also dirichlet
  matrix[max(NCandidates) - 1, max(NTime)] raw_theta_candidates[NSeasons];

}
transformed parameters {
  matrix[max(NCandidates), max(NTime)] theta_candidates[NSeasons];
  matrix[max(NCandidates), NSurveys] tau;
  matrix[max(NCandidates), NPollsters] alpha;

  // Covariance matrizes
  matrix[max(NCandidates) - 1, max(NCandidates) - 1] chol_cov_theta_candidates[NSeasons];

  // -- Current polling data
  // * Scale parameters
  // * Polling house deviation
  profile("sum to zero constraints"){
    alpha[1] = raw_alpha[1] * sigma_alpha[1];
    alpha[2:max(NCandidates)] = raw_alpha[2:max(NCandidates)] * sigma_alpha[2];
    // By columns
    for (ii in 1:NPollsters - 1) alpha[NCandidates[id_Pollster_season[ii]], ii]  = -sum(alpha[1:NCandidates[id_Pollster_season[ii]] - 1, ii]);
    // By rows
    for (jj in 1:NSeasons){
      for (ii in 1:NCandidates[jj]) alpha[ii, supvec_NPollsters_Seasons[jj] + NPollsters_Season[jj]] = -sum(alpha[ii, (supvec_NPollsters_Seasons[jj] + 1):(supvec_NPollsters_Seasons[jj] + NPollsters_Season[jj] - 1)]);
    }
    // * Nonsampling error
    tau = raw_tau * sigma_tau;
    for (jj in 1:NPollsters){
      // By rows
      for (ii in 1:(NCandidates[id_Pollster_season[jj]] - 1)){
        tau[ii, 1 + supvec_NSurveys_Pollsters[jj]] =
          - sum(tau[ii, (2 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])]);
      }
      // By columns
      for (ii in (1 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])){
        tau[NCandidates[id_Pollster_season[jj]], ii] = -sum(tau[1:NCandidates[id_Pollster_season[jj]] - 1, ii]);
      }
    }
  }

  // -- Random walk
  // * Determine big covariance matrix for candidates
  profile("premultiply correlation matrizes"){
    for (jj in 1:NSeasons){
      chol_cov_theta_candidates[jj, 1:NCandidates[jj] - 1, 1:NCandidates[jj] - 1] =
        diag_pre_multiply(sigma_cov[1:NCandidates[jj] - 1, jj],
          chol_corr_theta_candidates[jj, 1:NCandidates[jj] - 1, 1:NCandidates[jj] - 1]);
    }
  }


  // Current random walk
  // * log-odds ratio
  // * random walk with skip mechanic
  // * fill last row with 0s
  profile("random walk"){
    for (jj in 1:NSeasons){
      theta_candidates[jj, 1:NCandidates[jj], 1] = log(prior_theta_candidates[jj, 1:NCandidates[jj]]/prior_theta_candidates[jj, 1]);
      for (tt in 2:NTime[jj])
        theta_candidates[jj, 2:NCandidates[jj], tt] = chol_cov_theta_candidates[jj, 1:NCandidates[jj] - 1, 1:NCandidates[jj] - 1] *
          raw_theta_candidates[jj, 1:NCandidates[jj] - 1, tt - 1] +
          theta_candidates[jj, 2:NCandidates[jj], tt - 1];
      theta_candidates[jj, 1, 1:NTime[jj]] = zeros_theta[1:NTime[jj]];
    }
  }
}
model {
  // -- Current polling data
  // Standard deviations
  sigma_alpha ~ normal(0, prior_sigma_alpha);
  sigma_tau ~ normal(0, prior_sigma_tau);
  sigma_xi ~ normal(0, prior_sigma_xi);
  to_vector(sigma_cov) ~ normal(0, prior_sigma_cov);

  // Adjustment parameters
  // * Implement sum to zero constraints
  to_vector(raw_alpha) ~ std_normal();
  to_vector(raw_tau) ~ std_normal();

  // -- Random walk (candidates)
  for (jj in 1:NSeasons)
    prior_theta_candidates[jj] ~ dirichlet(rep_vector(10, max(NCandidates)));
  for (jj in 1:NSeasons){
    to_vector(raw_theta_candidates[jj]) ~ std_normal();
    chol_corr_theta_candidates[jj] ~ lkj_corr_cholesky(10.0);
  }


  // -- Likelihood (first round)
  // * Get the indexes for the included and excluded candidates
  // * Create a container for the complete vector
  // * Create a subset of those observed for the specific poll using the
  // * correct left_inv of the transition matrix
  profile("current_polls"){
    for (ii in 1:NPolls){
      {
        int start = 1 + abstention_omitted[ii];
        vector[NCandidates[id_Polls_season[ii]]] prob_theta_complete;
        prob_theta_complete = softmax(
          theta_candidates[id_Polls_season[ii],1:NCandidates[id_Polls_season[ii]],id_S_time[id_P_survey[ii]]] +
          tau[1:NCandidates[id_Polls_season[ii]], id_P_survey[ii]] +
          alpha[1:NCandidates[id_Polls_season[ii]], id_S_pollster[id_P_survey[ii]]]
          );

        target += multinomial_lpmf(
            y[(1 + abstention_omitted[ii]):NCandidates[id_Polls_season[ii]], ii] |
            prob_theta_complete[(1 + abstention_omitted[ii]):NCandidates[id_Polls_season[ii]]]/
            sum(prob_theta_complete[(1 + abstention_omitted[ii]):NCandidates[id_Polls_season[ii]]]));
      }
    }
  }
}

data {
  int NSurveys;
  int NBlocs;
  int<lower = NSurveys> NPolls;
  int NSeasons;
  int NCandidates[NSeasons];
  int NPollsters;
  int NTime[NSeasons];
  matrix[NSeasons, max(NTime) - 1] t_unit;

  // Current election
  int<lower = 1, upper = NSurveys> NSurveys_Pollster[NPollsters];
  int<lower = 1, upper = NPollsters> NPollsters_Season[NSeasons];
  int id_C_blocs[NSeasons, max(NCandidates)];
  int<lower = 1, upper = NSurveys> id_P_survey[NPolls];
  int<lower = 1, upper = NSeasons> id_S_season[NSurveys];
  int<lower = 1, upper = NSeasons> id_P_season[NPollsters];
  int id_S_pollster[NSurveys];
  int id_S_time[NSurveys];
  int abstention_omitted[NPolls]; //0-1 absentee treated as last candidate;

  // variable inclusion
  int NCandidates_Poll[NPolls];
  int NCombinations[NSeasons];
  int NCandidate_Combinations[NSeasons, max(NCombinations)];
  int candidates_included[max(NCombinations), NSeasons, max(NCandidates)];
  int candidates_excluded[max(NCombinations), NSeasons, max(NCandidates)];
  int<lower = 1, upper = max(NCombinations)> id_P_combinations[NPolls];
  int y[max(NCandidates), NPolls];

  // priors
  real<lower = 0> prior_sigma_alpha;
  real<lower = 0> prior_sigma_tau;
  real<lower = 0> prior_sigma_cov;
}
transformed data {
  real lsigma = 0.0001;
  vector[max(NCandidates)] zeros = rep_vector(0, max(NCandidates));
  row_vector[max(NTime)] zeros_theta = rep_vector(0.0, max(NTime))';
  int NCandidate_Combinations_neg[NSeasons, max(NCombinations)];
  int corr_mat_positions[NSeasons, max(NCandidates), max(NCandidates) - 1];
  int supvec_NSurveys_Pollsters[NPollsters];
  int supvec_NPollsters_Seasons[NSeasons];
  matrix[NBlocs - 1, max(NCandidates) - 1] identity_bloc[NSeasons];

  matrix[NSeasons, max(NTime) - 1] t_unit_sqrt = sqrt(t_unit);
  for (jj in 1:NSeasons){
    for (ii in 1:NCombinations[jj]){
      NCandidate_Combinations_neg[jj, ii] = NCandidates[jj] - NCandidate_Combinations[jj, ii];
    }
  }
  for (rr in 1:NSeasons){
    for (ii in 1:NCandidates[rr]){
      for (jj in 1:NCandidates[rr] - 1){
        if (jj >= ii){
          corr_mat_positions[rr, ii, jj] = jj + 1;
        } else {
          corr_mat_positions[rr, ii, jj] = jj;
        }
      }
    }
  }
  for (jj in 1:NSeasons){
    identity_bloc[jj] = rep_matrix(0.0, NBlocs - 1, max(NCandidates) - 1);
    for (nn in 2:NCandidates[jj]){
      identity_bloc[jj, id_C_blocs[jj, nn] - 1, nn - 1] = 1;
    }
  }

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


  matrix[max(NCandidates), NSurveys] raw_tau;
  matrix[max(NCandidates), NPollsters] raw_alpha;
  // Random walk
  matrix<lower = lsigma>[max(NCandidates) - 1, NSeasons] sigma_cov;
  cholesky_factor_corr[max(NCandidates) - 1] chol_corr_theta_candidates[NSeasons];

  simplex[max(NCandidates)] prior_theta_candidates[NSeasons]; // because a subset of the elements of a dirichlet is also dirichlet
  matrix[max(NCandidates) - 1, max(NTime)] raw_theta_candidates[NSeasons];
  simplex[max(NCandidates) - 1] trans_prob[NSeasons, max(NCandidates)];
  simplex[NBlocs] sum_bloc_prob[NBlocs];

}
transformed parameters {
  matrix[max(NCandidates), max(NTime)] theta_candidates[NSeasons];
  matrix[max(NCandidates), NSurveys] tau;
  matrix[max(NCandidates), NPollsters] alpha;

  // Covariance matrizes
  matrix[max(NCandidates) - 1, max(NCandidates) - 1] chol_cov_theta_candidates[NSeasons];

  // transition matrix
  matrix[max(NCandidates), max(NCandidates)] transition_matrix[NSeasons];
  matrix[max(NCandidates), max(NCandidates)] left_inv_trans_comb[max(NCombinations), NSeasons];

  // -- Transition matrix
  // * Fill
  profile("fill transition matrix"){
    for (j in 1:NSeasons){
      for (ii in 1:NCandidates[j]){
        transition_matrix[j, ii, ii] = 1;
        // Since we sample on max(NCandidates we have to select the first NCandidates[j] - 1 elements)
        transition_matrix[j, corr_mat_positions[j, ii, 1:NCandidates[j] - 1], ii] = -(trans_prob[j, ii][1:NCandidates[j] - 1])/sum(trans_prob[j, ii][1:NCandidates[j] - 1]);
      }
    }
  }

  // * Determine and store the left inverse
  profile ("left_inverse"){
    for (jj in 1:NSeasons){
      for (ii in 1:NCombinations[jj]){
        {
          matrix[NCandidate_Combinations[jj, ii],
                 NCandidates[jj] - NCandidate_Combinations[jj, ii]] mat1 =
              transition_matrix[jj,
                 candidates_included[ii, jj, 1:NCandidate_Combinations[jj, ii]],
                 candidates_excluded[ii, jj, 1:NCandidate_Combinations_neg[jj, ii]]];
          matrix[NCandidates[jj] - NCandidate_Combinations[jj, ii],
                 NCandidates[jj] - NCandidate_Combinations[jj, ii]] mat2 =
              transition_matrix[jj,
                 candidates_excluded[ii, jj, 1:NCandidate_Combinations_neg[jj, ii]],
                 candidates_excluded[ii, jj, 1:NCandidate_Combinations_neg[jj, ii]]];
          left_inv_trans_comb[
            ii,
            jj,
            1:NCandidate_Combinations[jj, ii],
            1:NCandidate_Combinations_neg[jj, ii]
          ] = mat1 / mat2;
        }
      }
    }
  }

  // -- Current polling data
  // * Scale parameters
  // * Polling house deviation
  profile("sum to zero constraints"){
    alpha[1] = raw_alpha[1] * sigma_alpha[1];
    alpha[2:max(NCandidates)] = raw_alpha[2:max(NCandidates)] * sigma_alpha[2];
    // By columns
    for (ii in 1:NPollsters - 1) alpha[NCandidates[id_P_season[ii]], ii]  = -sum(alpha[1:NCandidates[id_P_season[ii]] - 1, ii]);
    // By rows
    for (jj in 1:NSeasons){
      for (ii in 1:NCandidates[jj]) alpha[ii, supvec_NPollsters_Seasons[jj] + NPollsters_Season[jj]] = -sum(alpha[ii, (supvec_NPollsters_Seasons[jj] + 1):(supvec_NPollsters_Seasons[jj] + NPollsters_Season[jj] - 1)]);
    }
    // * Nonsampling error
    tau = raw_tau * sigma_tau;
    for (jj in 1:NPollsters){
      // By rows
      for (ii in 1:(NCandidates[id_P_season[jj]] - 1)){
        tau[ii, 1 + supvec_NSurveys_Pollsters[jj]] =
          - sum(tau[ii, (2 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])]);
      }
      // By columns
      for (ii in (1 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])){
        tau[NCandidates[id_P_season[jj]], ii] = -sum(tau[1:NCandidates[id_P_season[jj]] - 1, ii]);
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
        theta_candidates[jj, 2:NCandidates[jj], tt] = t_unit_sqrt[jj, tt - 1] * chol_cov_theta_candidates[jj, 1:NCandidates[jj] - 1, 1:NCandidates[jj] - 1] *
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


  // -- Transition probabilities
  profile("transition_probabilities_model"){
    for (jj in 1:NSeasons){
      for (ii in 1:NCandidates[jj]) trans_prob[jj, ii] ~ dirichlet(rep_vector(1, max(NCandidates) - 1));
    }

    for (jj in 1:NSeasons){
      for (ii in 1:NCandidates[jj]){
        {
          vector[NBlocs] tmp = rep_vector(1, NBlocs);
          for (rr in 1:ii - 1){
            //id_C_blocs
            //simplex[max(NCandidates) - 1] trans_prob[NSeasons, max(NCandidates)];

            tmp[id_C_blocs[jj, rr]] = tmp[id_C_blocs[jj, rr]] + trans_prob[jj, ii, rr]/sum(trans_prob[jj, ii, 1:NCandidates[jj] - 1]);
          }
          for (rr in (ii + 1):NCandidates[jj]){
            tmp[id_C_blocs[jj, rr]] = tmp[id_C_blocs[jj, rr]] + trans_prob[jj, ii, rr - 1]/sum(trans_prob[jj, ii, 1:NCandidates[jj] -1]);
          }
          // row = blocs from, col = blocs to
          sum_bloc_prob[id_C_blocs[jj, ii]] ~ dirichlet(tmp * 10);
        }
      }
    }
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
        int id_S_season_ii = id_S_season[id_P_survey[ii]];
        vector[NCandidates[id_S_season_ii]] prob_theta_complete;
        int NCandidates_ii = NCandidates_Poll[ii];
        vector[NCandidates_ii] prob_theta_subset;
        int id_P_combinations_ii = id_P_combinations[ii];
        int index_included[NCandidates_ii] =
          candidates_included[
            id_P_combinations_ii,
            id_S_season_ii,
            1:NCandidates_ii];
        int index_excluded[NCandidates[id_S_season_ii] - NCandidates_ii] =
          candidates_excluded[
            id_P_combinations_ii,
            id_S_season_ii,
            1:(NCandidates[id_S_season_ii] - NCandidates_ii)];

        int NCandidatesC_ii = NCandidate_Combinations[id_S_season_ii, id_P_combinations_ii];
        int NCandidatesC_neg_ii = NCandidate_Combinations_neg[id_S_season_ii, id_P_combinations_ii];
        prob_theta_complete = softmax(
          theta_candidates[id_S_season_ii,1:NCandidates[id_S_season_ii],id_S_time[id_P_survey[ii]]] +
          tau[1:NCandidates[id_S_season_ii], id_P_survey[ii]] +
          alpha[1:NCandidates[id_S_season_ii], id_S_pollster[id_P_survey[ii]]]
          );
        prob_theta_subset = prob_theta_complete[index_included] +
            left_inv_trans_comb[id_P_combinations_ii, id_S_season_ii,
            1:NCandidatesC_ii,
            1:NCandidatesC_neg_ii] *
            (zeros[1:NCandidatesC_neg_ii] -
            prob_theta_complete[index_excluded]);

        target += multinomial_lpmf(
            y[(1 + abstention_omitted[ii]):NCandidatesC_ii, ii] |
            prob_theta_subset[(1 + abstention_omitted[ii]):NCandidatesC_ii]/
            sum(prob_theta_subset[(1 + abstention_omitted[ii]):NCandidatesC_ii]));
      }
    }
  }
}

data {
  int NSurveys;
  int NBlocs;
  int<lower = NSurveys> NPolls;
  int NCandidates;
  int NPollsters;
  int NTime;
  vector[NTime - 1] t_unit;

  // Current election
  int<lower = 1, upper = NSurveys> NSurveys_Pollster[NPollsters];
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
  int NPollsters_past[NElections - 1];
  int NPolls_Pollster_past[sum(NPollsters_past)];
  int NTime_past;
  real t_bloc_unit_prior; // time since last election
  vector[NTime_past - 1] t_unit_past;
  int<lower = 1, upper = NTime_past> id_P_time_past[sum(NPolls_Pollster_past)];
  int<lower = 1, upper = NTime_past> id_P_pollster_past[sum(NPolls_Pollster_past)];
  int<lower = 1, upper = NElections - 1> id_P_elections_past[sum(NPolls_Pollster_past)];

  int elections_results[NBlocs, NElections];
  int id_E_time[NElections];
  int y_past[NBlocs, sum(NPolls_Pollster_past)];
  int abstention_omitted_past[sum(NPolls_Pollster_past)];
  int abstention_omitted_pollster[NPollsters];
  int abstention_omitted_pollster_past[sum(NPollsters_past)];

  // priors
  real<lower = 0> prior_sigma_xi;
  real<lower = 0> prior_sigma_alpha;
  real<lower = 0> prior_sigma_tau;
  real<lower = 0> prior_sigma_cov;
}
transformed data {
  real lsigma = 0.0001;
  vector[NCandidates] zeros = rep_vector(0, NCandidates);
  row_vector[NTime] zeros_theta = rep_vector(0.0, NTime)';
  int NCandidate_Combinations_neg[NCombinations];
  int corr_mat_positions[NCandidates, NCandidates - 1];
  int supvec_NSurveys_Pollsters[NPollsters];
  int supvec_NPollsters_past[NElections - 1];
  int supvec_NPolls_Pollster_past[sum(NPollsters_past)];
  matrix[NBlocs - 1, NCandidates - 1] identity_bloc = rep_matrix(0.0, NBlocs - 1, NCandidates - 1);
  matrix[NBlocs, NElections] election_results_logodds;
  vector[NTime - 1] t_unit_sqrt = sqrt(t_unit);
  vector[NTime_past - 1] t_unit_past_sqrt = sqrt(t_unit_past);
  real t_bloc_unit_sqrt_prior = sqrt(t_bloc_unit_prior);

  // Support vectors to correctly implement sum to zero constraints within the subgroups
  // * Pollster past
  supvec_NPollsters_past[1] = 0;
  for (jj in 2:(NElections - 1)) supvec_NPollsters_past[jj] = sum(NPollsters_past[1:(jj - 1)]);
  // * Polls past
  supvec_NPolls_Pollster_past[1] = 0;
  for (jj in 2:(sum(NPollsters_past))) supvec_NPolls_Pollster_past[jj] = sum(NPolls_Pollster_past[1:(jj - 1)]);

  // Past election results to shares
  for (nn in 1:NElections){
    {
      vector[NBlocs] prob = to_vector(elections_results[,nn])/sum(to_vector(elections_results[,nn]));
      election_results_logodds[,nn] = log(prob/prob[1]);
    }
  }
}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  // Random walk
  vector<lower = lsigma>[NCandidates - 1] sigma_cov;
  cholesky_factor_corr[NCandidates - 1] chol_corr_theta_candidates;

  simplex[NCandidates] prior_theta_candidates; // because a subset of the elements of a dirichlet is also dirichlet
  matrix[NCandidates - 1, NTime] raw_theta_candidates;
  matrix[NBlocs - 1, NTime_past] raw_theta_blocs;
  simplex[NCandidates - 1] trans_prob[NCandidates];

  // -- Past parameters
  matrix[NBlocs, sum(NPollsters_past)] raw_alpha_past;
  matrix[NBlocs, sum(NPolls_Pollster_past)] raw_tau_past;
  matrix[NBlocs - 1, NElections] raw_xi_past;
  simplex[NBlocs] theta_blocs_prior;
}
transformed parameters {
  matrix[NCandidates, NTime] theta_candidates;
  matrix[NBlocs, NTime_past] theta_blocs;
  matrix[NCandidates, NSurveys] tau;
  matrix[NCandidates, NPollsters] alpha;
  vector[NCandidates] xi;

  // Covariance matrizes
  cholesky_factor_cov[NCandidates - 1] chol_cov_theta_candidates;
  cholesky_factor_cov[NBlocs - 1] chol_cov_theta_blocs;

  // transition matrix
  matrix[NCandidates, NCandidates] transition_matrix;
  matrix[NCandidates, NCandidates] left_inv_trans_comb[NCombinations];

  // Past parameters
  matrix[NBlocs, sum(NPollsters_past)] alpha_past = rep_matrix(-999, NBlocs, sum(NPollsters_past));
  matrix[NBlocs, sum(NPolls_Pollster_past)] tau_past = rep_matrix(-999, NBlocs, sum(NPolls_Pollster_past));
  matrix[NBlocs, NElections - 1] xi_past = rep_matrix(-999, NBlocs, NElections - 1);

  // Prior connecting bloc and candidates
  vector[NCandidates] theta_candidates_prior;

  // -- Transition matrix
  // * Fill
  profile ("filling the transition matrix"){
    for (ii in 1:NCandidates){
      transition_matrix[ii, ii] = 1;
      transition_matrix[corr_mat_positions[ii], ii] = -trans_prob[ii];
    }
  }

  profile("left_inverse"){
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
  }

  // -- Current polling data
  // * Scale parameters
  profile("sum to zero current") {
    // * Polling error
    xi = append_row(raw_xi * sigma_xi, -sum(raw_xi * sigma_xi));
    // * Polling house deviation
    alpha = raw_alpha * sigma_alpha;
    // By columns
    for (ii in 1:NPollsters - 1) alpha[NCandidates, ii]  = -sum(alpha[1:NCandidates - 1, ii]);
    // By rows
    for (ii in 1:NCandidates) alpha[ii, NPollsters] = -sum(alpha[ii, 1:NPollsters - 1]);
    // * Nonsampling error
    tau = raw_tau * sigma_tau;
    for (jj in 1:NPollsters){
      // By rows
      for (ii in 1:(NCandidates - 1)){
        tau[ii, 1 + supvec_NSurveys_Pollsters[jj]] =
          - sum(tau[ii, (2 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])]);
      }
      // By columns
      for (ii in (1 + supvec_NSurveys_Pollsters[jj]):(NSurveys_Pollster[jj] + supvec_NSurveys_Pollsters[jj])){
        tau[NCandidates, ii] = -sum(tau[1:NCandidates - 1, ii]);
      }

    }
  }


  // -- Past polling data
  // * Sum to zero constraints
  profile ("sum to zero constraints past"){
    alpha_past = raw_alpha_past * sigma_alpha;
    for (jj in 1:(NElections - 1)){
      for (ii in (1 + supvec_NPollsters_past[jj]):(NPollsters_past[jj] + supvec_NPollsters_past[jj])){
        alpha_past[NBlocs, ii] = -sum(alpha_past[1:(NBlocs - 1), ii]);
      }
      for (ii in 1:(NBlocs - 1)){
        alpha_past[ii, 1 + supvec_NPollsters_past[jj]] =
          - sum(alpha_past[ii, (2 + supvec_NPollsters_past[jj]):(NPollsters_past[jj] + supvec_NPollsters_past[jj])]);
      }
    }

    for (jj in 1:(NElections - 1)){
      vector[NBlocs - 1] tmp = raw_xi_past[, jj] * sigma_xi;
      xi_past[, jj] = append_row(tmp, -sum(tmp));
    }
   tau_past = raw_tau_past * sigma_tau;
   for (jj in 1:sum(NPollsters_past)){
      for (ii in 1:(NBlocs - 1)){
        tau_past[ii, 1 + supvec_NPolls_Pollster_past[jj]] =
          - sum(tau_past[ii, (2 + supvec_NPolls_Pollster_past[jj]):(NPolls_Pollster_past[jj] + supvec_NPolls_Pollster_past[jj])]);
      }
      for (ii in (1 + supvec_NPolls_Pollster_past[jj]):(NPolls_Pollster_past[jj] + supvec_NPolls_Pollster_past[jj])){
        tau_past[NBlocs, ii] =  -sum(tau_past[1:(NBlocs - 1), ii]);
      }
    }
  }

  // -- Random walk
  // * Determine big covariance matrix for candidates
  profile ("premultiply matrices") {
    chol_cov_theta_candidates = diag_pre_multiply(sigma_cov, chol_corr_theta_candidates);
  }
  // * Determine smaller covariance matrix
  profile("Determine smaller covariance matrix") {
    chol_cov_theta_blocs = cholesky_decompose(identity_bloc * chol_cov_theta_candidates * chol_cov_theta_candidates' * identity_bloc');
  }
  // * Random walk over blocs
  profile ("random walk blocs"){
    theta_blocs[1] = rep_vector(0.0, NTime_past)';
    theta_blocs[,id_E_time[1]] = election_results_logodds[, 1];
    for (jj in 2:NElections){
      theta_blocs[,id_E_time[jj]] = election_results_logodds[, jj];
      for (tt in 2:(id_E_time[jj] - id_E_time[jj - 1])){
        theta_blocs[2:NBlocs, id_E_time[jj] - tt + 1] = theta_blocs[2:NBlocs,id_E_time[jj] - tt + 2] + t_unit_past_sqrt[id_E_time[jj] - tt + 1] * chol_cov_theta_blocs * raw_theta_blocs[, id_E_time[jj] - tt + 1];
      }
    }
  }

  // Current random walk
  // * log-odds ratio
  // * random walk with skip mechanic
  // * fill last row with 0s
  profile ("random walk candidates"){
    theta_candidates[, 1] = log(prior_theta_candidates/prior_theta_candidates[1]);
    for (tt in 2:NTime)
      theta_candidates[2:NCandidates, tt] = t_unit_sqrt[tt - 1] * chol_cov_theta_candidates * raw_theta_candidates[:, tt] + theta_candidates[2:NCandidates, tt - 1];
    theta_candidates[1] = zeros_theta;
  }
}
model {
  prior_theta_candidates ~ dirichlet(rep_vector(2, NCandidates));
  {
    vector[NBlocs - 1] sum_prior_theta_candidates;
    vector[NBlocs] prob_prior_theta_candidates = rep_vector(0.0, NBlocs);
    for (ii in 1:NCandidates){
      prob_prior_theta_candidates[id_C_blocs[ii]] += prior_theta_candidates[ii];
    }
    sum_prior_theta_candidates = log(prob_prior_theta_candidates[2:NBlocs]/prob_prior_theta_candidates[1]);
    sum_prior_theta_candidates ~ multi_normal_cholesky(election_results_logodds[2:NBlocs, NElections], sqrt(t_bloc_unit_sqrt_prior) * chol_cov_theta_blocs);
  }

  for (jj in 1:(NElections - 1)){
    theta_blocs[2:NBlocs, id_E_time[jj] + 1] ~ multi_normal_cholesky(election_results_logodds[2:NBlocs, jj], t_unit_past_sqrt[id_E_time[jj]] * chol_cov_theta_blocs);
  }

  // -- Current polling data
  // Standard deviations
  sigma_xi ~ normal(0, prior_sigma_xi);
  sigma_alpha ~ normal(0, prior_sigma_alpha);
  sigma_tau ~ normal(0, prior_sigma_tau);
  sigma_cov ~ normal(0, prior_sigma_cov);

  // Adjustment parameters
  // * Implement sum to zero constraints
  raw_xi ~ std_normal();
  to_vector(raw_alpha) ~ std_normal();
  to_vector(raw_tau) ~ std_normal();

  // -- Random walk (candidates)
  to_vector(raw_theta_candidates) ~ std_normal();
  chol_corr_theta_candidates ~ lkj_corr_cholesky(10.0);

  // -- Transition probabilities
  for (ii in 1:NCandidates) trans_prob[ii] ~ dirichlet(transition_probability_prior[ii]);

  // // Past elections
  theta_blocs_prior ~ dirichlet(rep_vector(2, NBlocs));
  to_vector(raw_theta_blocs) ~ std_normal();
  to_vector(raw_alpha_past) ~ std_normal();
  to_vector(raw_xi_past) ~ std_normal();
  to_vector(raw_tau_past) ~ std_normal();

  // -- Likelihood (first round)
  // * Get the indexes for the included and excluded candidates
  // * Create a container for the complete vector
  // * Create a subset of those observed for the specific poll using the
  // * correct left_inv of the transition matrix
  profile("current_polls"){
    for (ii in 1:NPolls){
      {
        int start = 1 + abstention_omitted[ii];
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
        int NCandidatesC_ii = NCandidate_Combinations[id_P_combinations_ii];
        int NCandidatesC_neg_ii = NCandidate_Combinations_neg[id_P_combinations_ii];
        prob_theta_complete = softmax(
          theta_candidates[, id_S_time[id_P_survey[ii]]] +
          tau[, id_P_survey[ii]] +
          alpha[, id_S_pollster[id_P_survey[ii]]] +
          xi
          );

        prob_theta_subset = prob_theta_complete[index_included] +
            left_inv_trans_comb[id_P_combinations_ii,
            1:NCandidate_Combinations[id_P_combinations_ii],
            1:NCandidate_Combinations_neg[id_P_combinations_ii]] *
            (zeros[1:NCandidate_Combinations_neg[id_P_combinations_ii]] -
            prob_theta_complete[index_excluded]);


        target += multinomial_lpmf(
            y[(1 + abstention_omitted[ii]):NCandidatesC_ii, ii] |
            prob_theta_subset[(1 + abstention_omitted[ii]):NCandidatesC_ii]/
            sum(prob_theta_subset[(1 + abstention_omitted[ii]):NCandidatesC_ii]));
      }
    }
  }


  // // * Likelihood
  profile("past_likelihood"){
    for (ii in 1:sum(NPolls_Pollster_past)){
      {
        int start = 1 + abstention_omitted_past[ii];
        vector[NBlocs] theta_past =
          softmax(
            theta_blocs[, id_P_time_past[ii]] +    // trend
            alpha_past[, id_P_pollster_past[ii]] + // pollster
            tau_past[, ii] +                    // nonsampling error
            xi_past[, id_P_elections_past[ii]]//   // polling error
                  );
        profile("multinomial"){
          target += multinomial_lpmf(y_past[start:NBlocs, ii] |
           theta_past[start:NBlocs] / sum(theta_past[start:NBlocs])
          );
        }
        // vector[NBlocs - abstention_omitted_past[ii]] theta_past =
        //   softmax(
        //     theta_blocs[1 + abstention_omitted_past[ii]:NBlocs, id_P_time_past[ii]] +    // trend
        //     alpha_past[1 + abstention_omitted_past[ii]:NBlocs, id_P_pollster_past[ii]] + // pollster
        //     tau_past[1 + abstention_omitted_past[ii]:NBlocs, ii] +                    // nonsampling error
        //     xi_past[1 + abstention_omitted_past[ii]:NBlocs, id_P_elections_past[ii]]//   // polling error
        //           );
        // profile("multinomial"){
        //   target += multinomial_lpmf(y_past[1 + abstention_omitted_past[ii]:NBlocs, ii] |
        //     theta_past);
        // }
        //
      }
    }
  }
}
generated quantities {
  vector[NBlocs - 1] sigma_cov_blocs = sqrt(diagonal(chol_cov_theta_blocs * chol_cov_theta_blocs'));
  matrix[NBlocs, NTime_past] prob_theta_blocs;
  matrix[NCandidates, NTime] prob_theta_candidates;
  vector[NCandidates] prob_xi;
  matrix[NCandidates, NPollsters] prob_alpha;
  vector[NCandidates] prob_sigma_cov;
  matrix[NBlocs, sum(NPolls_Pollster_past)] epsilon_past = rep_matrix(-99, NBlocs, sum(NPolls_Pollster_past));
  for (ii in 1:sum(NPolls_Pollster_past)){
    {
      int start = 1 + abstention_omitted_past[ii];
      vector[NBlocs] y_past_sample;
      vector[NBlocs] tau_sample;
      vector[NBlocs] theta_past;
      tau_sample = to_vector(normal_rng(rep_vector(0, NBlocs), sigma_tau));
      tau_sample = tau_sample - mean(tau_sample);
      theta_past =
        softmax(
          theta_blocs[, id_P_time_past[ii]] + // trend
          alpha_past[, id_P_pollster_past[ii]] + // pollster
          xi_past[, id_P_elections_past[ii]]+     // polling error
          tau_sample);
      y_past_sample[start:NBlocs] = to_vector(multinomial_rng(theta_past[start:NBlocs] / sum(theta_past[start:NBlocs]),
      sum(y_past[start:NBlocs, ii])));
      epsilon_past[start:NBlocs, ii] = to_vector(y_past[start:NBlocs, ii])/sum(to_vector(y_past[start:NBlocs, ii])) - y_past_sample[start:NBlocs]/sum(y_past_sample[start:NBlocs]);
    }
  }


  for (tt in 1:NTime){
    prob_theta_candidates[, tt] = softmax(theta_candidates[, tt]);
  }
  for (tt in 1:NTime_past){
    prob_theta_blocs[, tt] = softmax(theta_blocs[, tt]);
  }
  prob_xi = softmax(theta_candidates[,NTime] + xi) - softmax(theta_candidates[,NTime]);
  for (jj in 1:NPollsters)
    prob_alpha[,jj] = softmax(theta_candidates[, NTime] + alpha[,jj]) - softmax(theta_candidates[, NTime]);
  {
    vector[NCandidates] tmp = append_row(0, theta_candidates[2:NCandidates,NTime] +
      chol_cov_theta_candidates * to_vector(normal_rng(
        rep_vector(0.0, NCandidates - 1), 1)));
    prob_sigma_cov = softmax(tmp) - softmax(theta_candidates[, NTime]);
  }
}



data {
  int NSurveys; // first round surveys
  int<lower = NSurveys> NPolls; // first round polls
  int NCandidates; // Number of candidates
  int NPolls_Candidates; // Numbers of individual poll results
  int NPollsters;         // Number o
  int TUnit;
  vector[TUnit - 1] t_unit;

  vector[NPolls_Candidates] y; // logit of poll proportion
  vector[NPolls_Candidates] n;          // observations

  int Sid_t[NSurveys]; // id timepoint of survey
  int Sid_r[NSurveys]; // id pollster  of survey

  int NCandidates_p[NPolls]; // N of candidates by poll
  int Pid_s[NPolls]; // id of survey by poll

  // variable inclusion
  int Candidates_Polls[NPolls];
  int NCombinations;
  int NCandidates_Combinations[NCombinations]; // How many candidates
  int Candidates_included[NCombinations, NCandidates]; // Who included
  int Candidates_excluded[NCombinations, NCandidates]; // Who excluded
  int<lower = 1, upper = NCombinations> id_c[NPolls];

  // past
  int NElections_past;
  int NPolls_past;
  int NCandidates_past[NElections_past];
  int NPollsters_past;
  int<lower = 1, upper = NPollsters_past> id_p_past[NPolls_past];
  int<lower = 1, upper = NElections_past> id_rt_past[NPollsters_past];
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
  vector[NPollsters] conditional_values = rep_vector(-10, NPollsters);
  // Previous election results
  matrix[max(NCandidates_past), NElections_past] theta_results =
    rep_matrix(0.0, max(NCandidates_past), NElections_past);
  int NCandidates_Combinations_neg[NCombinations];
  vector[TUnit] t_unit_sqrt = sqrt(t_unit);
  vector[NPolls_Candidates] sigma = sqrt(rep_vector(0.25, NPolls_Candidates) ./ n) * sqrt(4);

  for (ii in 1:NCombinations)
    NCandidates_Combinations_neg[ii] = NCandidates - NCandidates_Combinations[ii];
  for (ii in 1:NElections_past){
    theta_results[1:NCandidates_past[ii], ii] = log(results[1:NCandidates_past[ii], ii]/results[NCandidates_past[ii], ii]);
  }

}
parameters {
  real<lower = lsigma> sigma_tau;
  real<lower = lsigma> sigma_alpha;
  real<lower = lsigma> sigma_xi;
  matrix[NCandidates, NSurveys] raw_tau;
  matrix[NCandidates - 1, NPollsters] raw_alpha;
  //matrix[max(NCandidates_past) - 1, NPollsters_past] raw_alpha_past;
  //matrix[max(NCandidates_past), NPolls_past] raw_tau_past;
  vector[NCandidates] raw_xi;
  //matrix[max(NCandidates_past) - 1, NElections_past] raw_xi_past;
  // Random walk
  simplex[NCandidates] theta_prior;
  vector<lower = lsigma>[NCandidates] sigma_cov;
  matrix[NCandidates, TUnit] raw_theta;
  cholesky_factor_corr[NCandidates] cholesky_corr_theta;
}
transformed parameters {
  matrix[NCandidates, TUnit] theta;
  matrix[NCandidates, NSurveys] tau;
  matrix[NCandidates, NPollsters] alpha;
  matrix[max(NCandidates_past), NPollsters_past] alpha_past =
    rep_matrix(0.0, max(NCandidates_past), NPollsters_past);
  matrix[max(NCandidates_past), NPolls_past] tau_past;
  vector[NCandidates] xi;
  matrix[max(NCandidates_past), NElections_past] xi_past;

  // Covariance matrizes
  cholesky_factor_cov[NPollsters] cholesky_cov_theta;
  matrix[NCandidates, NCandidates] cov_theta;
  matrix[NCandidates, NCandidates] left_inv_cov_theta_comb[NCombinations];

  // Containers
  matrix[NSurveys, NCandidates] mu_sc;
  vector[NPolls_Candidates] mu_pc;// = rep_vector(0.0, NPolls * NCandidates);

  // Determine current covariance matrix
  cholesky_cov_theta = diag_pre_multiply(sigma_cov, cholesky_corr_theta);
  cov_theta = cholesky_cov_theta * cholesky_cov_theta';

  // Create left_inv
  for (ii in 1:NCombinations){
    left_inv_cov_theta_comb[ii] = rep_matrix(0.0, NCandidates, NCandidates);
    {
      matrix[NCandidates_Combinations[ii], NCandidates_Combinations_neg[ii]] mat1;
      matrix[NCandidates_Combinations_neg[ii], NCandidates_Combinations_neg[ii]] mat2;
      mat1 = cov_theta[
        Candidates_included[ii, 1:NCandidates_Combinations[ii]],
        Candidates_excluded[ii, 1:NCandidates_Combinations_neg[ii]]];
      mat2 = cov_theta[
        Candidates_excluded[ii, 1:NCandidates_Combinations_neg[ii]],
        Candidates_excluded[ii, 1:NCandidates_Combinations_neg[ii]]];
      left_inv_cov_theta_comb[ii,
        1:NCandidates_Combinations[ii],
        1:NCandidates_Combinations_neg[ii]] = mat1 / mat2;
    }
  }

  // -- Current polling data
  // Create a matrix of survey level results
  // Sum to 0 constraints
  tau = raw_tau;

  for (ii in 1:NPollsters)
    alpha[, ii] = append_row(raw_alpha[, ii], -sum(raw_alpha[, ii]));

  // alpha[1:(NCandidates - 1), 1:(NPollsters - 1)] =
  //   raw_alpha[1:(NCandidates - 1), 1:(NPollsters - 1)];
  // for (ii in 1:(NCandidates - 1))
  //   alpha[ii, NPollsters] = -sum(alpha[ii,1:(NPollsters - 1)])/sqrt(NCandidates - 1);
  // for (ii in 1:NPollsters)
  //   alpha[NCandidates, ii] = -sum(alpha[1:(NCandidates - 1), ii])/sqrt(NPollsters - 1);
  xi = raw_xi;

  // Random walk
  theta[, 1] = logit(theta_prior);
  for (tt in 2:TUnit)
    theta[, tt] = t_unit_sqrt[tt - 1] * cholesky_cov_theta * raw_theta[:, tt] + theta[, tt - 1];

  // Fill containers
  for (ii in 1:NSurveys){
    mu_sc[ii] = theta[, Sid_t[ii]]';
  }
  {
    int start = 1;
    int end = 0;
    for (ii in 1:NPolls){
      vector[NCandidates_p[ii]] pi_theta_subset;
      int id_c_ii = id_c[ii];
      int NCandidates_p_ii = NCandidates_p[ii];
      int id_included[NCandidates_p_ii] = Candidates_included[id_c_ii, 1:NCandidates_p_ii];
      int id_excluded[NCandidates - NCandidates_p_ii] = Candidates_excluded[id_c_ii, 1:(NCandidates - NCandidates_p_ii)];
      pi_theta_subset = (mu_sc[Pid_s[ii]][id_included])' -
        left_inv_cov_theta_comb[
          id_c[ii],
          1:NCandidates_Combinations[id_c_ii],
          1:NCandidates_Combinations_neg[id_c_ii]] *
        (conditional_values[1:NCandidates_Combinations_neg[id_c_ii]] -
        (mu_sc[Pid_s[ii]][id_excluded])');
      end += NCandidates_p_ii;
      mu_pc[start:end] = pi_theta_subset;// +
        xi[id_included] +
        tau[, Pid_s[ii]][id_included] +
        alpha[, Sid_r[Pid_s[ii]]][id_included];
      start = end + 1;
    }
  }


//
//
//   // -- Past polling data
//   // Sum to 0 constraints
//   for (ii in 1:NElections_past){
//     tau_past[1:NCandidates_past[id_t_past[ii]], ii] =
//       append_row(raw_tau_past[1:(NCandidates_past[id_t_past[ii]] - 1), ii],
//             -sum(raw_tau_past[1:(NCandidates_past[id_t_past[ii]] - 1), ii]));
//   }
//   for (ii in 1:NPollsters_past)
//       alpha_past[1:NCandidates_past[id_rt_past[ii]], ii] = append_row(
//         raw_alpha_past[1:(NCandidates_past[id_rt_past[ii]] - 1), ii],
//         - sum(raw_alpha_past[1:(NCandidates_past[id_rt_past[ii]] - 1), ii]));
//   for (ii in 1:NElections_past)
//     xi_past[1:NCandidates_past[ii], ii] =
//       append_row(raw_xi_past[1:(NCandidates_past[ii] - 1), ii],
//             -sum(raw_xi_past[1:(NCandidates_past[ii] - 1), ii]));
}
model {
  // -- Current polling data
  sigma_xi ~ normal(0, 0.01);
  sigma_alpha ~ normal(0, 0.01);
  sigma_tau ~ normal(0, 0.01);
  sigma_cov ~ normal(0, 0.01);
  to_vector(raw_xi) ~ normal(0, sigma_xi);
  to_vector(raw_alpha) ~ normal(0, sigma_alpha);
  to_vector(raw_tau) ~ normal(0, sigma_tau);

  // Random walk
  theta_prior ~ dirichlet(rep_vector(1, NCandidates)) ;
  to_vector(raw_theta) ~ std_normal();
  cholesky_corr_theta ~ lkj_corr_cholesky(20.0);

  // Likelihood
  // + sqrt((mu_pc .* (1 - mu_pc)) ./n)
  //target += normal_lpdf(y | mu_pc, sigma);
  target += normal_lpdf(y | mu_pc, 0.02);

  // -- Past polling data
  // Priors
  //to_vector(raw_alpha_past) ~ normal(0, sigma_alpha);
  //to_vector(raw_xi_past) ~ normal(0, sigma_xi);
  //to_vector(raw_tau_past) ~ normal(0, sigma_tau);
}
generated quantities {
  vector[NPolls_Candidates] yhat;
  vector[NPolls_Candidates] eps;
  yhat = to_vector(inv_logit(normal_rng(mu_pc, sigma)));
  eps = inv_logit(y) - yhat;
}

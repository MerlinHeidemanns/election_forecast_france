functions {
  matrix sum_to_zero_pollster(int N_A, matrix A, int[] sv1, int[] sv2, real sigma, int[] N, int [,] ix){
    matrix[rows(A) + 1, cols(A)] B;
    for (j in 1:N_A){
      B[ix[j, 1:N[j] - 1], sv1[j]:sv2[j] - 1] =
        sigma * A[1:N[j] - 1,sv1[j]:sv2[j] - 1];
      for (i in sv1[j]:sv2[j] - 1){
        B[ix[j, N[j]], i] = -sum(B[ix[j, 1:N[j] - 1], i]);
      }
      for (i in 1:N[j]){
        B[ix[j, i], sv2[j]] = -sum(B[ix[j, i], sv1[j]:sv2[j] - 1]);
      }
    }
    return(B);
  }

  matrix sum_to_zero_xi(int N_A, matrix A, real sigma, int [] N, int [,] ix){
    matrix[rows(A), cols(A)] B;
    for (j in 1:N_A){
      vector[N[j] - 1] tmp = A[1:N[j] - 1, j] * sigma;
      B[ix[j, 1:N[j]], j] = append_row(tmp, - sum(tmp));
    }
    return(B);
  }
}
data {
  int<lower=1> m1_N;
  int<lower = 1> m1_NLimit;
  int<lower=1> m1_NBlocs; // Number of groupings excluding abstentions
  int<lower=1> m1_T1;
  int<lower=1> m1_T2;
  real<lower = 1> m1_t1[m1_T1];
  int m1_count_weeks;

  int m1_NPollsters[m1_T2];
  int m1_NPolls_Pollster[sum(m1_NPollsters)];
  int m1_ix_pollster[m1_N];
  int<lower = 1, upper = m1_T2> m1_ix_election[m1_N];
  int<lower = 1, upper = m1_T1> m1_ix_week[m1_N];
  vector[m1_N] m1_abstention_share;
  vector[m1_N] m1_abstention_share_included;

  int m1_ix_time[m1_N];
  int m1_y[m1_NBlocs, m1_N];

  int m1_y_results[m1_NBlocs, m1_T2 - 1];
  int m1_ix_results[m1_T2];
  int m1_NMiss_results[m1_T2];
  int m1_miss_results[m1_T2, m1_NBlocs];

  int m1_NMiss[m1_N];
  int m1_miss[m1_N, m1_NBlocs];

  int m1_NMiss_pred[m1_T1 * m1_T2];
  int m1_miss_pred[m1_T1 * m1_T2, m1_NBlocs];
  int<lower = 1, upper = m1_T2> m1_election[m1_T1 * m1_T2];

  // M2
  int<lower=1> m2_N1;
  int<lower=1> m2_N2;
  real m2_x1[m2_N1];
  real m2_x2[m2_N2];
  matrix[m2_N1, m1_NBlocs] m2_y;
  int m2_NMiss[m2_N1 + m2_N2];
  int m2_miss[m2_N1 + m2_N2, m1_NBlocs];
  real m2_prior_sigma_quality;

  int m3_NObs;
  int m3_NBlocs;
  int m3_NElections;
  int m3_NDepartments;
  int m3_id_Obs_elections[m3_NObs];
  int m3_id_Obs_departments[m3_NObs];
  matrix[m3_NObs, m3_NBlocs] m3_YVoteshare;
  vector[m3_NDepartments] m3_w;

  int m3_NMiss[m3_NObs];
  int m3_miss[m3_NObs, m3_NBlocs];

  // National level predictors
  int m3_K;
  matrix[m3_NElections, m3_K] m3_XNation;
  matrix[m3_NElections, m3_NBlocs - 1] m3_incumbency;
  matrix[m3_NElections, m3_NBlocs - 1] m3_main_contender;
  // -- Approval
  int m3_NInclusion;
  int m3_NPolls;
  int m3_NPollsters;
  int m3_NPresidents;
  int m3_NPollsters_Presidents[m3_NPresidents];
  int m3_NPolls_Presidents[m3_NPresidents];
  int m3_NTime;
  int m3_id_Polls_time[m3_NPolls];
  int<lower = 1, upper = m3_NPresidents> m3_id_Polls_president[m3_NPolls];
  int<lower = 1, upper = m3_NPollsters> m3_id_Polls_pollster[m3_NPolls];
  int<lower = 1, upper = sum(m3_NPollsters_Presidents)> m3_id_Polls_pollster_president[m3_NPolls];
  int m3_y_approval[m3_NPolls];
  int m3_n_approval[m3_NPolls];
  // Department level predictors
  int m3_M;
  matrix[m3_NObs, m3_M] m3_XDepartment;
  int m3_NMiss_X;
  int m3_id_X_miss[m3_NMiss_X];
  int m3_NTime_max;

}
transformed data {
  real m1_delta = 1e-9;
  int m1_NPollsters_iter   = 0;
  int m1_NPollsters_       = sum(m1_NPollsters);

  int m3_supvec_NPollsters_Presidents[m3_NPresidents];
  int m3_supvec_NPolls_Presidents[m3_NPresidents];

  int m1_T = m1_T1 * m1_T2;
  int m1_supvec_A[m1_T2];
  int m1_supvec_B[m1_T2];

  int m2_N = m2_N1 + m2_N2;
  real m2_x[m2_N];
  row_vector[m2_N] m2_zeros = rep_row_vector(0.0, m2_N);
  row_vector[m1_T] m1_zerosT = rep_row_vector(0.0, m1_T);
  for (n1 in 1:m2_N1) m2_x[n1] = m2_x1[n1];
  for (n2 in 1:m2_N2) m2_x[m2_N1 + n2] = m2_x2[n2];
  m1_supvec_A[1] = 1;
  m1_supvec_B[1] = m1_NPollsters[1];
  for (j in 2:m1_T2){
    m1_supvec_A[j] = sum(m1_NPollsters[1:j - 1]) + 1;
    m1_supvec_B[j] = sum(m1_NPollsters[1:j]);
  }

  for (j in 1:m1_T2){
    if (m1_NPollsters[j] != 0){
      m1_NPollsters_iter = m1_NPollsters_iter + 1;
    }
  }
  // M3
  m3_supvec_NPollsters_Presidents[1] = 0;
  m3_supvec_NPolls_Presidents[1] = 0;
  for (jj in 2:m3_NPresidents){
    m3_supvec_NPollsters_Presidents[jj] = sum(m3_NPollsters_Presidents[1:jj - 1]);
    m3_supvec_NPolls_Presidents[jj] = sum(m3_NPolls_Presidents[1:jj - 1]);
  }

}
parameters {
  // M1
  real<lower=0>                          m1_rho;
  vector<lower=0>[m1_NBlocs - 1]         m1_alpha;
  cholesky_factor_corr[m1_NBlocs - 1]    m1_L_Omega;
  matrix[m1_NBlocs - 1, m1_T1]           m1_eta[m1_T2];
  matrix[m1_NBlocs, m1_T2]               m1_raw_xi;
  real<lower = 0>                        m1_sigma_xi;
  matrix[m1_NBlocs - 1, m1_NPollsters_]  m1_raw_mu_pollster;
  real<lower = 0>                        m1_sigma_mu_pollster;
  real<lower = 0>                        m1_mu_abstention;
  vector[m1_NBlocs - 1]                  m1_raw_abstention_adjustment;
  real<lower = 0>                        m1_sigma_abstention_adjustment;
  real<lower = 0>                        m1_beta;

  // M2
  real<lower=0>                          m2_rho;
  matrix[m1_NBlocs - 1, m2_N]            m2_eta;
  vector<lower = 0>[m1_NBlocs - 1]       m2_sigma_quality;
  row_vector[m1_NBlocs]                  m2_prediction;
  matrix[m1_NBlocs - 1,m2_N]             m2_raw_quality;
  real<lower = 0>                        m2_sigma;

  // M3
  vector<lower = 0.0001>[m3_NBlocs - 1]  m3_sigma_alpha;
  real                                   m3_mu_alpha[m3_NBlocs - 1];
  matrix[m3_NElections, m3_NBlocs - 1]   m3_raw_alpha;
  // Department level predictors
  vector[m3_NMiss_X]                     m3_XMiss;
  // Department level predictors
  matrix[m3_M, m3_NBlocs - 1]            m3_raw_beta;
  real<lower = 0.0>                      m3_sigma_beta;
  real                                   m3_gamma_incumbency;
  real                                   m3_gamma;
  real<lower = 0>                        m3_sigma;
  real                                   m3_beta_incumbency;
  // Approval
  matrix[m3_NElections, m3_NTime]        m3_raw_XApproval;
  vector<lower = 0>[m3_NElections]       m3_sigma_XApproval;
  vector[m3_NElections]                  m3_mu_XApproval;
  vector<lower = 0>[m3_NPollsters]       m3_sigma_pollster;
  real<lower = 0>                        m3_sigma_mu_pollster;
  vector[sum(m3_NPollsters_Presidents)]  m3_raw_mu_pollster_president;
  vector[m3_NPolls]                      m3_raw_tau;
  real                                   m3_delta;
  // m3_prediction
  matrix[m3_NDepartments, m3_NBlocs]     m3_prediction;


  // Prediction
  simplex[m1_NMiss_results[m1_T2]]       prediction;
}
transformed parameters {
  matrix[m1_NBlocs, m2_N]      m2_f;
  matrix[m1_NBlocs - 1,
         m1_NBlocs - 1]        m1_L_Sigma = diag_pre_multiply(m1_alpha, m1_L_Omega);
  matrix[m1_NBlocs, m1_T]         m1_f;
  matrix[m1_NBlocs,
         m1_NPollsters_]       m1_mu_pollster;
  matrix[m1_NBlocs, m1_T2]     m1_xi;
  vector[m1_NBlocs]            m1_abstention_adjustment;
  // M2
  matrix[m1_NBlocs,m2_N]     m2_quality;
  matrix[m2_N, m1_NBlocs]     m2_voteshare_prediction;
  // M3
  matrix[m3_NObs, m3_NBlocs - 1] m3_y_star;
  matrix[m3_NObs, m3_M] m3_XDepartment_miss;
  matrix[m3_NElections, m3_K + 1] m3_XNation_;
  matrix[m3_NElections, m3_NBlocs - 1] m3_alpha;
  // Department level predictors
  matrix[m3_NBlocs, m3_NBlocs] m3_epsilon;
  matrix[m3_NObs, m3_NBlocs] m3_voteshare_prediction;
  matrix[m3_NElections, m3_NTime] m3_XApproval;
  vector[sum(m3_NPollsters_Presidents)] m3_mu_pollster_president;
  vector[m3_NPolls] m3_tau;


  {
    matrix[m1_T1, m1_T1]     m1_K = gp_exp_quad_cov(m1_t1, 1.0, m1_rho);
    matrix[m2_N, m2_N] m2_K = gp_exp_quad_cov(m2_x, 1.0, m2_rho);
    matrix[m1_T1, m1_T1] m1_chol_K;

    // Add nudge
    for (i in 1:m1_T1)   m1_K[i, i]    = m1_K[i, i] + m1_delta;
    for (i in 1:m2_N) m2_K[i, i] = m2_K[i, i] + m1_delta;
    // Decompose because multiple use in loop
    m1_chol_K = cholesky_decompose(m1_K)';
    // M1
    for (i in 1:m1_T2){
      m1_f[1:m1_NBlocs - 1, 1 + (i - 1) * m1_count_weeks:i * m1_count_weeks] = m1_L_Sigma * m1_eta[i] * m1_chol_K;
    }
    m1_f[m1_NBlocs] = m1_zerosT;
    // M2
    m2_f[1:m1_NBlocs - 1] = m1_L_Sigma * m2_eta * cholesky_decompose(m2_K)';
    m2_f[m1_NBlocs] = m2_zeros;
  }
  // M1
  m1_mu_pollster = sum_to_zero_pollster(m1_NPollsters_iter,
                                     m1_raw_mu_pollster,
                                     m1_supvec_A,
                                     m1_supvec_B,
                                     m1_sigma_mu_pollster,
                                     m1_NMiss_results,
                                     m1_miss_results);

  m1_xi = sum_to_zero_xi(m1_T2, m1_raw_xi, m1_sigma_xi, m1_NMiss_results, m1_miss_results);

  m1_abstention_adjustment[1:m1_NBlocs - 1] = m1_raw_abstention_adjustment * m1_sigma_abstention_adjustment;
  m1_abstention_adjustment[m1_NBlocs]       = -sum(m1_abstention_adjustment[1:m1_NBlocs - 1]);

  // M2
  for (j in 1:m2_N){
    m2_quality[2:m1_NBlocs, j] = m2_raw_quality[,j] .* m2_sigma_quality;
    m2_quality[1, j] = -sum(m2_quality[2:m1_NBlocs, j]);
  }
  m2_voteshare_prediction[1:m2_N1] = m2_y;
  m2_voteshare_prediction[m2_N]    = m2_prediction;



  // M3
  m3_XDepartment_miss[, 1] = m3_XDepartment[, 1];
  m3_XDepartment_miss[m3_id_X_miss, 1] = m3_XMiss;
  // Approval
  // -- Random walk
  for (jj in 1:m3_NElections){
    m3_XApproval[jj] = m3_mu_XApproval[jj] + cumulative_sum(m3_raw_XApproval[jj] * m3_sigma_XApproval[jj]);
  }
  m3_mu_pollster_president = m3_raw_mu_pollster_president * m3_sigma_mu_pollster;
  m3_tau = m3_raw_tau .* m3_sigma_pollster[m3_id_Polls_pollster];
  // Sum to zero constraints mu_pollster_president
  // Pollsters are ordered by president
  for (jj in 1:m3_NPresidents){
    m3_mu_pollster_president[m3_supvec_NPollsters_Presidents[jj] + 1] =
      - sum(m3_mu_pollster_president[(m3_supvec_NPollsters_Presidents[jj] + 2):(m3_supvec_NPollsters_Presidents[jj] + m3_NPollsters_Presidents[jj])]);
    m3_tau[m3_supvec_NPolls_Presidents[jj] + 1] =
      - sum(m3_tau[(m3_supvec_NPolls_Presidents[jj] + 2):(m3_supvec_NPolls_Presidents[jj] + m3_NPolls_Presidents[jj])]);
  }
  //
  for (j in 1:m3_NBlocs - 1){

    m3_alpha[,j] = m3_mu_alpha[j] + cumulative_sum(m3_raw_alpha[,j] * m3_sigma_alpha[j]);
  }

  profile("m3_y_star"){
    {
    matrix[m3_NElections, m3_NBlocs - 1] tmp1 = m3_incumbency;
    matrix[m3_NObs, m3_NBlocs - 1] tmp2;
    for (j in 1:m3_NElections){
      tmp1[j] = tmp1[j] * inv_logit(m3_XApproval[j, m3_NTime_max]);
    }
    for (j in 1:m3_NElections){
      tmp2[1 + (j - 1) * m3_NDepartments:j * m3_NDepartments] = to_vector(m3_XDepartment_miss[1 + (j - 1) * m3_NDepartments:j * m3_NDepartments]) * m3_incumbency[j];
    }
    m3_y_star = m3_alpha[m3_id_Obs_elections] +
                m3_gamma_incumbency * tmp1[m3_id_Obs_elections] +
                inv_logit(m3_XApproval[m3_id_Obs_elections, m3_NTime_max]) * m3_gamma * rep_row_vector(1.0, m3_NBlocs - 1) +
                m3_XDepartment_miss * m3_sigma_beta * m3_raw_beta +
                tmp2 * m3_beta_incumbency +
                m3_delta * m3_main_contender[m3_id_Obs_elections];
    }
  }
  m3_voteshare_prediction[1:m3_NObs - m3_NDepartments] = m3_YVoteshare[1:m3_NObs - m3_NDepartments];
  m3_voteshare_prediction[(m3_NObs - m3_NDepartments + 1):m3_NObs] = m3_prediction;
}
model {
  matrix[m3_NObs, m3_NBlocs] m3_theta;

  // M1 ------------------------------------------------------------------------
  // Priors
  m1_rho                        ~ normal(2, 1);
  m1_alpha                      ~ student_t(10, 0, 0.5);
  m1_L_Omega                    ~ lkj_corr_cholesky(5);
  for (j in 1:m1_T2){
    to_vector(m1_eta[j])        ~ std_normal();
  }
  m1_sigma_mu_pollster          ~ normal(0, 0.08);
  to_vector(m1_raw_mu_pollster) ~ std_normal();
  to_vector(m1_raw_xi)          ~ std_normal();
  m1_sigma_xi                   ~ normal(0, 0.5);
  m1_mu_abstention              ~ normal(0.22, 0.03);
  m1_raw_abstention_adjustment  ~ std_normal();
  m1_sigma_abstention_adjustment ~ normal(0, 1);
  m1_beta                       ~ normal(0.01, 0.02);

  // Likelihood
  for (j in 1:m1_NLimit){
    {
      int m1_ix[m1_NMiss[j]] = m1_miss[j, 1:m1_NMiss[j]];
      vector[m1_NMiss[j]] m1_theta = softmax(
        m1_f[m1_ix,m1_ix_time[j]] +  // Polling trend
        m1_mu_pollster[m1_ix,m1_ix_pollster[j]]  + // Pollster adjustment
        (m1_abstention_adjustment[m1_ix] * (m1_abstention_share[j] - 0.22 + m1_beta * (m1_count_weeks + 1 - m1_ix_week[j]))) *
        m1_abstention_share_included[j]
      );
      m1_y[m1_ix, j] ~ multinomial(m1_theta[1:m1_NMiss[j]]);
    }
  }
  // Polling error likelihood
  for (j in 1:(m1_T2 - 1)){
    {
      int m1_ix[m1_NMiss_results[j]] = m1_miss_results[j, 1:m1_NMiss_results[j]];
      vector[m1_NMiss[j]] m1_theta = softmax(m1_f[,m1_ix_results[j]][m1_ix] +
                                       m1_xi[m1_ix,j]);
      m1_y_results[m1_miss_results[j, 1:m1_NMiss_results[j]], j] ~ multinomial(m1_theta);
    }
  }

  // M2 ------------------------------------------------------------------------
  // Priors
  m2_rho ~ normal(12, 3);
  to_vector(m2_eta) ~ std_normal();
  to_vector(m2_raw_quality) ~ std_normal();
  m2_sigma_quality ~ normal(0, m2_prior_sigma_quality);
  m2_sigma ~ normal(0, 0.01);
  // Likelihood
  for (j in 1:m2_N){
    {
      vector[m2_NMiss[j]] theta = softmax(m2_f[m2_miss[j, 1:m2_NMiss[j]], j] + m2_quality[m2_miss[j, 1:m2_NMiss[j]], j]);
      m2_voteshare_prediction[j, m2_miss[j, 1:m2_NMiss[j]]] ~ normal(theta, m2_sigma);
    }
  }

  // M3 ------------------------------------------------------------------------
  // Approval
  to_vector(m3_raw_XApproval) ~ std_normal();
  m3_mu_XApproval ~ normal(0, 2);
  m3_sigma_XApproval ~ normal(0, 0.05);
  m3_sigma_pollster ~ normal(0, 0.01);
  m3_sigma_mu_pollster ~ normal(0, 0.05);
  m3_raw_mu_pollster_president ~ std_normal();
  m3_raw_tau ~ std_normal();
  // Likelihood
  {
    vector[m3_NTime * m3_NElections] m3_XApproval_vector;
    for (j in 1:m3_NElections){
      m3_XApproval_vector[(1 + (j - 1) * m3_NTime):(m3_NTime * j)] = m3_XApproval[j]';
    }
    m3_y_approval[1:m3_NInclusion] ~ binomial_logit(m3_n_approval[1:m3_NInclusion],
      m3_XApproval_vector[m3_id_Polls_time[1:m3_NInclusion]] +
      m3_mu_pollster_president[m3_id_Polls_pollster_president[1:m3_NInclusion]] +
      m3_tau[1:m3_NInclusion]);
  }
  // Department predictors
  m3_XMiss ~ normal(0, 1);
  m3_beta_incumbency ~ normal(0, 0.3);
  // // National trend
  m3_mu_alpha ~ normal(0, 0.4);
  m3_sigma_alpha ~ normal(0, 0.1);
  to_vector(m3_raw_alpha) ~ std_normal();
  to_vector(m3_raw_beta) ~ std_normal();
  m3_sigma_beta ~ normal(0, 0.3);
  m3_gamma_incumbency ~ normal(0, 0.5);
  m3_gamma ~ normal(0, 0.5);
  m3_sigma ~ normal(0, 0.1);
  {
    profile("m3_softmax"){
      for (j in 1:m3_NObs){
        m3_theta[j, 1:m3_NMiss[j]] = softmax(append_row(0.0, m3_y_star[j]')[m3_miss[j, 1:m3_NMiss[j]]])';
      }
    }
    profile("m3_llh"){
      for (j in 1:m3_NElections){
          to_vector(
            m3_voteshare_prediction[1 + (j - 1) * 100:j * 100, m3_miss[j * 100, 1:m3_NMiss[j * 100]]]
          ) ~ normal(
              to_vector(m3_theta[1 + (j - 1) * 100:j * 100, 1:m3_NMiss[j * 100]]),
              m3_sigma);
      }
    }
  }


  // Partial pooling -----------------------------------------------------------
  {
    vector[m1_NMiss_results[m1_T2]] m1_pred =
        softmax(m1_f[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]],m1_T2 * m1_T1] +
                m1_xi[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]],m1_T2]);
    vector[m1_NMiss_results[m1_T2]] m3_pred = rep_vector(0.0, m1_NMiss_results[m1_T2]);
    for (j in m3_NObs - m3_NDepartments + 1:m3_NObs){
      m3_pred = m3_pred + m3_w[j - (m3_NObs - m3_NDepartments)] * m3_voteshare_prediction[j,m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]]]';
    }
    prediction ~ dirichlet(m1_pred * 100);
    prediction ~ dirichlet((m2_voteshare_prediction[m2_N, m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]]]' + 0.01) * 100);
    prediction ~ dirichlet((m3_pred + 0.01) * 100);
  }
}
generated quantities {
  matrix[m1_T, m1_NBlocs] m1_y2 = rep_matrix(0.0, m1_T, m1_NBlocs);
  matrix[m2_N, m1_NBlocs] m2_y2 = rep_matrix(0.0, m2_N, m1_NBlocs);
  matrix[m1_NBlocs, m1_N] m1_y_rep;
  vector[m1_NBlocs + 1] prediction_adjusted;
  vector[m1_NMiss_results[m1_T2]] m1_prediction_new;
  vector[m1_NMiss_results[m1_T2]] m2_prediction_new;
  vector[m1_NMiss_results[m1_T2]] m3_prediction_new = rep_vector(0.0, m1_NMiss_results[m1_T2]);
  matrix[m1_NBlocs - 1,m1_NBlocs - 1] m1_Omega;
  m1_Omega = multiply_lower_tri_self_transpose(m1_L_Omega);

  for (j in 1:m1_T){
    m1_y2[j, m1_miss_pred[j, 1:m1_NMiss_pred[j]]] = softmax(m1_f[,j][m1_miss_pred[j, 1:m1_NMiss_pred[j]]])';
  }
  for (j in 1:m2_N1 + m2_N2)
    m2_y2[j] = softmax(m2_f[,j])';
  for (j in 1:m1_N){
    {
      int m1_ix[m1_NMiss[j]] = m1_miss[j, 1:m1_NMiss[j]];
      vector[m1_NMiss[j]] m1_theta = softmax(
        m1_f[m1_ix,m1_ix_time[j]] +  // Polling trend
        m1_mu_pollster[m1_ix,m1_ix_pollster[j]]  + // Pollster adjustment
        m1_abstention_adjustment[m1_ix] * m1_abstention_share[j]
      );
      m1_y_rep[m1_ix, j] =
        to_vector(multinomial_rng(m1_theta[1:m1_NMiss[j]], sum(m1_y[m1_ix, j])))/
        (1.0 * sum(m1_y[m1_ix, j]));
    }
  }

  // Individual predictions by model
  m1_prediction_new = softmax(m1_f[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]],m1_T2 * m1_T1] +
                m1_xi[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]],m1_T2]);

  {
    vector[m1_NBlocs] tmp;
    tmp[2:m1_NBlocs] = to_vector(normal_rng(rep_vector(0.0, m1_NBlocs - 1), m2_sigma_quality));
    tmp[1] = -sum(tmp[2:m1_NBlocs]);
    m2_prediction_new = to_vector(normal_rng(
      softmax(m2_f[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]],m2_N1 + 1] +
              tmp[m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]]]),
                m2_sigma));
  }
  for (j in m3_NObs - m3_NDepartments + 1:m3_NObs){
    m3_prediction_new = m3_prediction_new + m3_w[j - (m3_NObs - m3_NDepartments)] * m3_voteshare_prediction[j,m1_miss_results[m1_T2, 1:m1_NMiss_results[m1_T2]]]';
  }
}

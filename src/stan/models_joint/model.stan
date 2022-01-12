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
  int<lower=1> m1_NBlocs; // Number of groupings excluding abstentions
  int<lower=1> T1;
  int<lower=1> T2;
  real<lower = 1> t1[T1];

  int NPollsters[T2];
  int NPolls_Pollster[sum(NPollsters)];
  int ix_pollster[m1_N];
  int<lower = 1, upper = T2> ix_election[m1_N];
  int<lower = 1, upper = T1> ix_week[m1_N];
  vector[m1_N] abstention_share;
  vector[m1_N] abstention_share_included;

  int ix_time[m1_N];
  int y[m1_NBlocs, m1_N];

  int y_results[m1_NBlocs, T2 - 1];
  int ix_results[T2];
  int NMiss_results[T2];
  int miss_results[T2, m1_NBlocs];

  int NMiss[m1_N];
  int miss[m1_N, m1_NBlocs];

  int NMiss_pred[T1 * T2];
  int miss_pred[T1 * T2, m1_NBlocs];
  int<lower = 1, upper = T2> election[T1 * T2];

  int<lower=1> m2_N1;
  int<lower=1> m2_N2;
  real m2_x1[m2_N1];
  real m2_x2[m2_N2];
  int m2_y[m2_N1, m1_NBlocs];
  int m2_NMiss[m2_N1 + m2_N2];
  int m2_miss[m2_N1 + m2_N2, m1_NBlocs];
  real m2_prior_sigma_quality;
}
transformed data {
  real delta = 1e-9;
  int NPollsters_iter   = 0;
  int NPollsters_       = sum(NPollsters);

  int T = T1 * T2;
  int supvec_A[T2];
  int supvec_B[T2];

  vector[T2] xi_raw_sigma;
  int m2_N = m2_N1 + m2_N2;
  real m2_x[m2_N];
  row_vector[m2_N] zeros = rep_row_vector(0.0, m2_N);
  row_vector[T] zerosT = rep_row_vector(0.0, T);
  for (n1 in 1:m2_N1) m2_x[n1] = m2_x1[n1];
  for (n2 in 1:m2_N2) m2_x[m2_N1 + n2] = m2_x2[n2];
  supvec_A[1] = 1;
  supvec_B[1] = NPollsters[1];
  for (j in 2:T2){
    supvec_A[j] = sum(NPollsters[1:j - 1]) + 1;
    supvec_B[j] = sum(NPollsters[1:j]);
  }

  for (j in 1:T2){
    if (NPollsters[j] != 0){
      NPollsters_iter = NPollsters_iter + 1;
    }
  }

}
parameters {
  // M1
  real<lower=0>                       m1_rho;
  vector<lower=0>[m1_NBlocs - 1]      alpha;
  cholesky_factor_corr[m1_NBlocs - 1] L_Omega;
  matrix[m1_NBlocs - 1, T1]           m1_eta[T2];
  matrix[m1_NBlocs, T2]               raw_xi;
  real<lower = 0>                     sigma_xi;
  matrix[m1_NBlocs - 1, NPollsters_]  raw_mu_pollster;
  real<lower = 0>                     sigma_mu_pollster;
  real<lower = 0>                     mu_abstention;
  vector[m1_NBlocs - 1]               raw_abstention_adjustment;
  real<lower = 0>                     sigma_abstention_adjustment;
  real<lower = 0>                     beta;

  // M2
  real<lower=0>                        m2_rho;
  matrix[m1_NBlocs - 1, m2_N]          m2_eta;
  vector<lower = 0>[m1_NBlocs]         m2_sigma_quality;
  matrix[m1_NBlocs, m2_N]              m2_raw_quality;

  // Prediction
  simplex[NMiss_results[T2]] prediction;
}
transformed parameters {
  matrix[m1_NBlocs, m2_N]      m2_f;
  matrix[m1_NBlocs, m2_N]      m2_quality;
  matrix[m1_NBlocs - 1,
         m1_NBlocs - 1]         L_Sigma = diag_pre_multiply(alpha, L_Omega);
  matrix[m1_NBlocs, T]         m1_f;
  matrix[m1_NBlocs,
         NPollsters_]          mu_pollster;
  matrix[m1_NBlocs, T2]        xi;
  vector[m1_NBlocs]            abstention_adjustment;

  {
    matrix[T1, T1]        K = gp_exp_quad_cov(t1, 1.0,   m1_rho);
    matrix[m2_N, m2_N] m2_K = gp_exp_quad_cov(m2_x, 1.0, m2_rho);
    matrix[T1, T1] chol_K;

    // Add nudge
    for (i in 1:T1)   K[i, i]    = K[i, i] + delta;
    for (i in 1:m2_N) m2_K[i, i] = m2_K[i, i] + delta;
    // Decompose because multiple use in loop
    chol_K = cholesky_decompose(K)';
    // M1
    for (i in 1:T2){
      m1_f[1:m1_NBlocs - 1, 1 + (i - 1) * 16:i * 16] = L_Sigma * m1_eta[i] * chol_K;
    }
    m1_f[m1_NBlocs] = zerosT;
    // M2
    m2_f[1:m1_NBlocs - 1] = L_Sigma * m2_eta * cholesky_decompose(m2_K)';
    m2_f[m1_NBlocs] = zeros;
  }
  // M1
  mu_pollster = sum_to_zero_pollster(NPollsters_iter,
                                     raw_mu_pollster,
                                     supvec_A,
                                     supvec_B,
                                     sigma_mu_pollster,
                                     NMiss_results,
                                     miss_results);

  xi = sum_to_zero_xi(T2, raw_xi, sigma_xi, NMiss_results, miss_results);

  abstention_adjustment[1:m1_NBlocs - 1] = raw_abstention_adjustment * sigma_abstention_adjustment;
  abstention_adjustment[m1_NBlocs]       = -sum(abstention_adjustment[1:m1_NBlocs - 1]);


  // M2 ------------------------------------------------------------------------
  for (j in 1:m1_NBlocs){
    m2_quality[j] = m2_raw_quality[j] * m2_sigma_quality[j];
  }
}
model {
  // M1 ------------------------------------------------------------------------
  // Priors
  m1_rho                     ~ normal(50, 10);
  alpha                      ~ student_t(10, 0, 0.5);
  L_Omega                    ~ lkj_corr_cholesky(5);
  for (j in 1:T2){
    to_vector(m1_eta[j])     ~ std_normal();
  }
  sigma_mu_pollster          ~ normal(0, 0.08);
  to_vector(raw_mu_pollster) ~ std_normal();
  to_vector(raw_xi)          ~ std_normal();
  sigma_xi                   ~ normal(0, 0.5);
  mu_abstention              ~ normal(0.22, 0.03);
  raw_abstention_adjustment  ~ std_normal();
  sigma_abstention_adjustment ~ normal(0, 1);
  beta ~ normal(0.01, 0.002);
  // Likelihood
  for (j in 1:m1_N){
    {
      int ix[NMiss[j]] = miss[j, 1:NMiss[j]];
      vector[NMiss[j]] theta = softmax(
        m1_f[ix,ix_time[j]] +  // Polling trend
        mu_pollster[ix,ix_pollster[j]]  + // Pollster adjustment
        (abstention_adjustment[ix] * (abstention_share[j] - 0.22 + beta * (17 - ix_week[j]))) *
        abstention_share_included[j]
      );
      y[ix, j] ~ multinomial(theta[1:NMiss[j]]);
    }
  }
  // Polling error likelihood
  for (j in 1:(T2 - 1)){
    {
      int ix[NMiss_results[j]] = miss_results[j, 1:NMiss_results[j]];
      vector[NMiss[j]] theta = softmax(m1_f[,ix_results[j]][ix] +
                                       xi[ix,j]);
      y_results[miss_results[j, 1:NMiss_results[j]], j] ~ multinomial(theta);
    }
  }
  // M2 ------------------------------------------------------------------------
  // Priors
  m2_rho ~ normal(12, 1);
  to_vector(m2_eta) ~ std_normal();
  to_vector(m2_raw_quality) ~ std_normal();
  m2_sigma_quality ~ normal(0, m2_prior_sigma_quality);
  // Likelihood
  for (j in 1:m2_N1){
    {
      vector[m2_NMiss[j]] theta = softmax(m2_f[m2_miss[j, 1:m2_NMiss[j]], j] +
                                    m2_quality[m2_miss[j, 1:m2_NMiss[j]],j]);
      m2_y[j, m2_miss[j, 1:m2_NMiss[j]]] ~ multinomial(theta);
    }
  }
  // Partial pooling -----------------------------------------------------------
  prediction ~ dirichlet(rep_vector(1, NMiss_results[T2]));
  {
    vector[NMiss_results[T2]] pred =
        softmax(m1_f[miss_results[T2, 1:NMiss_results[T2]],T2 * T1] +
                xi[miss_results[T2, 1:NMiss_results[T2]],T2]);
    vector[NMiss_results[T2]] theta =
        softmax(m2_f[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1] +
                m2_quality[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1]);
    prediction ~ dirichlet(1000 * theta + 1000 * pred);
  }
}
generated quantities {
  matrix[T, m1_NBlocs] m1_y2 = rep_matrix(0.0, T, m1_NBlocs);
  matrix[m2_N, m1_NBlocs] m2_y2 = rep_matrix(0.0, m2_N, m1_NBlocs);
  matrix[m1_NBlocs, m1_N] y_rep;

  for (j in 1:T){
    m1_y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(m1_f[,j][miss_pred[j, 1:NMiss_pred[j]]])';
  }
  for (j in 1:m2_N1 + m2_N2)
    m2_y2[j] = softmax(m2_f[,j])';
  for (j in 1:m1_N){
    {
      int ix[NMiss[j]] = miss[j, 1:NMiss[j]];
      vector[NMiss[j]] theta = softmax(
        m1_f[ix,ix_time[j]] +  // Polling trend
        mu_pollster[ix,ix_pollster[j]]  + // Pollster adjustment
        abstention_adjustment[ix] * abstention_share[j]
      );
      y_rep[ix, j] =
        to_vector(multinomial_rng(theta[1:NMiss[j]], sum(y[ix, j])))/
        (1.0 * sum(y[ix, j]));
    }
  }
}

functions {
  vector Q_sum_to_zero_QR(int N) {
    vector [2*N] Q_r;

    for(i in 1:N) {
      Q_r[i] = -sqrt((N-i)/(N-i+1.0));
      Q_r[i+N] = inv_sqrt((N-i) * (N-i+1));
    }
    return Q_r;
  }

  vector sum_to_zero_QR(vector x_raw, vector Q_r) {
    int N = num_elements(x_raw) + 1;
    vector [N] x;
    real x_aux = 0;

    for(i in 1:N-1){
      x[i] = x_aux + x_raw[i] * Q_r[i];
      x_aux = x_aux + x_raw[i] * Q_r[i+N];
    }
    x[N] = x_aux;
    return x;
  }
}
data {
  int<lower=1> N1;
  int<lower=1> D; // Number of groupings excluding abstentions
  int<lower=1> T1;
  int<lower=1> T2;
  real<lower = 1> t1[T1];

  int P[T2];
  int NPolls_Pollster[sum(P)];
  int p[N1];
  int<lower = 1, upper = T2> index_election[N1];
  int<lower = 1, upper = T1> index_week[N1];
  vector[N1] abstention_share;

  int indicator[N1];
  int y[D, N1];


  int y_results[D, T2 - 1];
  int result_indicator[T2];
  int NMiss_results[T2];
  int miss_results[T2, D];

  int NMiss[N1];
  int miss[N1, D];

  int NMiss_pred[T1 * T2];
  int miss_pred[T1 * T2, D];
  int<lower = 1, upper = T2> election[T1 * T2];

  int<lower=1> m2_N1;
  int<lower=1> m2_N2;
  real m2_x1[m2_N1];
  real m2_x2[m2_N2];
  int m2_y[m2_N1, D];
  int m2_NMiss[m2_N1 + m2_N2];
  int m2_miss[m2_N1 + m2_N2, D];
  real m2_prior_sigma_quality;
}
transformed data {
  real delta = 1e-9;
  int T = T1 * T2;
  int supvec_A[T2];
  int supvec_B[T2];
  int supvec_C[sum(P)];
  int supvec_D[sum(P)];

  int locations[D - 1];
  matrix[2*D, T2] Q_r;
  vector[T2] xi_raw_sigma;
  int m2_N = m2_N1 + m2_N2;
  real m2_x[m2_N];
  row_vector[m2_N] zeros = rep_row_vector(0.0, m2_N);
  row_vector[T] zerosT = rep_row_vector(0.0, T);
  for (n1 in 1:m2_N1) m2_x[n1] = m2_x1[n1];
  for (n2 in 1:m2_N2) m2_x[m2_N1 + n2] = m2_x2[n2];
  supvec_A[1] = 1;
  supvec_B[1] = P[1];
  for (j in 2:T2){
    supvec_A[j] = sum(P[1:j - 1]) + 1;
    supvec_B[j] = sum(P[1:j]);
  }

  supvec_C[1] = 1;
  supvec_D[1] = NPolls_Pollster[1];
  for (j in 2:sum(P)){
    supvec_C[j] = sum(NPolls_Pollster[1:j - 1]) + 1;
    supvec_D[j] = sum(NPolls_Pollster[1:j]);
  }

  {
    int count = 0;
    for (j in 1:D - 1){
      count = count + 1;
      if (count == 5){
        count = count + 1;
      }
      locations[j] = count;
    }
  }
}
parameters {
  // M1
  real<lower=0>                m1_rho;
  vector<lower=0>[D - 1]       alpha;
  cholesky_factor_corr[D - 1]  L_Omega;
  matrix[D - 1, T1]            eta[T2];
  matrix[D, T2]                raw_xi;
  real<lower = 0>              sigma_xi;
  matrix[D - 1, sum(P)]        raw_mu_pollster;
  real<lower = 0>              sigma_mu_pollster;
  vector[D - 1]                raw_abstention_adjustment;
  real<lower = 0>              sigma_abstention_adjustment;

  // M2
  real<lower=0>                m2_rho;
  matrix[D - 1, m2_N]          m2_eta;
  vector<lower = 0>[D]         m2_sigma_quality;
  matrix[D, m2_N]              m2_raw_quality;

  // Prediction
  simplex[NMiss_results[T2]] prediction;
}
transformed parameters {
  matrix[D, m2_N]      m2_f;
  matrix[D, m2_N]      m2_quality;
  matrix[D - 1, D - 1] L_Sigma = diag_pre_multiply(alpha, L_Omega);
  matrix[D, T]         m1_f;
  matrix[D, sum(P)]    mu_pollster;
  matrix[D, T2]        xi;
  vector[D]            abstention_adjustment;

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
      m1_f[1:D - 1, 1 + (i - 1) * 16:i * 16] = L_Sigma * eta[i] * chol_K;
    }
    m1_f[D] = zerosT;
    // M2
    m2_f[1:D - 1] = L_Sigma * m2_eta * cholesky_decompose(m2_K)';
    m2_f[D] = zeros;
  }
  // M1
  for (j in 1:T2){
    mu_pollster[miss_results[j, 1:NMiss_results[j] - 1], supvec_A[j]:supvec_B[j] - 1] =
      sigma_mu_pollster * raw_mu_pollster[1:NMiss_results[j] - 1,supvec_A[j]:supvec_B[j] - 1];
    for (i in supvec_A[j]:supvec_B[j] - 1){
      mu_pollster[miss_results[j, NMiss_results[j]], i] = -sum(mu_pollster[miss_results[j, 1:NMiss_results[j] - 1], i]);
    }
    for (i in 1:NMiss_results[j]){
      mu_pollster[miss_results[j, i], supvec_B[j]] = -sum(mu_pollster[miss_results[j, i], supvec_A[j]:supvec_B[j] - 1]);
    }
  }

  for (j in 1:T2){
    vector[NMiss_results[j] - 1] tmp = raw_xi[1:NMiss_results[j] - 1, j] * sigma_xi;
    xi[miss_results[j, 1:NMiss_results[j]], j] = append_row(tmp, - sum(tmp));
  }
  abstention_adjustment[1:D - 1] = raw_abstention_adjustment * sigma_abstention_adjustment;
  abstention_adjustment[D]       = -sum(abstention_adjustment[1:D - 1]);


  // M2 ------------------------------------------------------------------------
  for (j in 1:D){
    m2_quality[j] = m2_raw_quality[j] * m2_sigma_quality[j];
  }
}
model {
  // M1 ------------------------------------------------------------------------
  // Priors
  m1_rho ~ normal(50, 10);
  alpha ~ student_t(10, 0, 0.5);
  L_Omega ~ lkj_corr_cholesky(5);
  for (j in 1:T2) to_vector(eta[j]) ~ std_normal();
  sigma_mu_pollster ~ normal(0, 0.08);
  to_vector(raw_mu_pollster) ~ std_normal();
  to_vector(raw_xi) ~ std_normal();
  sigma_xi ~ normal(0, 0.5);
  raw_abstention_adjustment ~ std_normal();
  sigma_abstention_adjustment ~ normal(0, 1);
  // Likelihood
  for (j in 1:N1){
    {
      int index[NMiss[j]] = miss[j, 1:NMiss[j]];
      vector[NMiss[j]] theta = softmax(
        m1_f[index,indicator[j]] +  // Polling trend
        mu_pollster[index,p[j]]  + // Pollster adjustment
        abstention_adjustment[index] * abstention_share[j]
      );
      y[index, j] ~ multinomial(theta[1:NMiss[j]]);
    }
  }
  // Polling error likelihood
  for (j in 1:(T2 - 1)){
    {
      int index[NMiss_results[j]] = miss_results[j, 1:NMiss_results[j]];
      vector[NMiss[j]] theta = softmax(m1_f[,result_indicator[j]][index] +
                                       xi[index,j]);
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
  matrix[T, D] m1_y2 = rep_matrix(0.0, T, D);
  matrix[m2_N, D] m2_y2 = rep_matrix(0.0, m2_N, D);
  matrix[D, N1] y_rep;

  for (j in 1:T){
    m1_y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(m1_f[,j][miss_pred[j, 1:NMiss_pred[j]]])';
  }
  for (j in 1:m2_N1 + m2_N2)
    m2_y2[j] = softmax(m2_f[,j])';
  for (j in 1:N1){
    {
      int index[NMiss[j]] = miss[j, 1:NMiss[j]];
      vector[NMiss[j]] theta = softmax(
        m1_f[index,indicator[j]] +  // Polling trend
        mu_pollster[index,p[j]]  + // Pollster adjustment
        abstention_adjustment[index] * abstention_share[j]
      );
      y_rep[index, j] =
        to_vector(multinomial_rng(theta[1:NMiss[j]], sum(y[index, j])))/
        (1.0 * sum(y[index, j]));
    }
  }
}

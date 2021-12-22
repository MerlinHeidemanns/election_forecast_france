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
  int<lower=1> D;
  int<lower=1> T1;
  int<lower=1> T2;
  real<lower = 1, upper = T1> t1[T1 * T2];
  real<lower = 1, upper = T2> t2[T1 * T2];

  int P[T2];
  int NPolls_Pollster[sum(P) - 3];
  int p[N1];
  int<lower = 1, upper = T2> index_election[N1];

  int indicator[N1];
  int y[D, N1];
  int abs_inc[N1];

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
  int supvec_C[sum(P) - 3];
  int supvec_D[sum(P) - 3];

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
  for (j in 2:sum(P) - 3){
    supvec_C[j] = sum(NPolls_Pollster[1:j - 1]) + 1;
    supvec_D[j] = sum(NPolls_Pollster[1:j]);
  }


  for (j in 1:T2){
    Q_r[1:NMiss_results[j] * 2, j] = Q_sum_to_zero_QR(NMiss_results[j]);
    xi_raw_sigma[j] = inv_sqrt(1 - inv(NMiss_results[j]));
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
  real<lower=0>                rho_x1;
  real<lower=0>                rho_x2;
  vector<lower=0>[D - 1]       alpha;
  cholesky_factor_corr[D - 1]  L_Omega;
  matrix[D - 1, T]             eta;
  matrix[D, T2]                raw_xi;
  real<lower = 0>              sigma_xi;
  matrix[D - 1, sum(P)]        raw_mu_pollster;
  real<lower = 0>              sigma_mu_pollster;
  matrix[D - 1, N1]            raw_tau;
  real<lower = 0>              sigma_tau;



  // M2
  real<lower=0>                m2_rho_long;
  matrix[D - 1, m2_N]          m2_eta;
  vector<lower = 0>[D - 1]     m2_sigma_quality;
  matrix[D - 1, m2_N]          m2_raw_quality;

  // Prediction
  //simplex[NMiss_results[T2]] prediction;

}
transformed parameters {
  matrix[D, m2_N]      m2_f;
  matrix[D, m2_N]      m2_quality;
  matrix[D - 1, D - 1] L_Sigma = diag_pre_multiply(alpha, L_Omega);
  matrix[D, T]         f;
  matrix[D, sum(P)]    mu_pollster;
  matrix[D, T2]        xi;
  matrix[D, N1]        tau;


  {
    profile("gp_exp_quad_cov"){
      matrix[T, T] K = gp_exp_quad_cov(t1, 1.0, rho_x1) .* gp_exp_quad_cov(t2, 1.0, rho_x2);
      //matrix[m2_N, m2_N] m2_K = gp_exp_quad_cov(m2_x, 1.0, m2_rho_long);

      // diagonal elements
      // M1
      for (i in 1:T)
        K[i, i] = K[i, i] + delta;
      // f = (cholesky_decompose(K) * eta
      //     * L_Sigma)';
      f[locations] = L_Sigma * eta * cholesky_decompose(K)';
      f[5] = zerosT;
      // M2
      //for (i in 1:m2_N) m2_K[i, i] = m2_K[i, i] + delta;

      // m2_f = (cholesky_decompose(m2_K) * m2_eta
      //     * L_Sigma)';
      //m2_f[2:D] = L_Sigma * m2_eta * cholesky_decompose(m2_K)';
      m2_f[1] = zeros;
    }
  }
  profile("sum to zero"){
    // M1
    for (j in 1:T2){
      mu_pollster[miss_results[j, 1:NMiss_results[j] - 1], supvec_A[j]:supvec_B[j] - 1] =
        sigma_mu_pollster * raw_mu_pollster[1:NMiss_results[j] - 1,supvec_A[j]:supvec_B[j] - 1];
      for (i in supvec_A[j]:supvec_B[j] - 1) mu_pollster[NMiss_results[j], i] = -sum(mu_pollster[miss_results[j, 1:NMiss_results[j] - 1], i]);
      for (i in 1:NMiss_results[j]){
        mu_pollster[miss_results[j, i], supvec_B[j]] = -sum(mu_pollster[miss_results[j, i], supvec_A[j]:supvec_B[j] - 1]);
      }
    }

    for (j in 1:sum(P) - 3){
      tau[miss[j, 1:NMiss[j] - 1], supvec_C[j]:supvec_D[j] - 1] =
        sigma_tau * raw_tau[1:NMiss[j] - 1,supvec_C[j]:supvec_D[j] - 1];
      for (i in supvec_C[j]:supvec_D[j] - 1){
        tau[1,i] = {0, tau[1, i]}[abs_inc[i] + 1];
      }
      for (i in supvec_C[j]:supvec_D[j] - 1) tau[NMiss[j], i] = -sum(tau[miss[j, 1:NMiss[j] - 1], i]);
      for (i in 1:NMiss[j]){
        tau[miss[j, i], supvec_D[j]] = -sum(tau[miss[j, i], supvec_C[j]:supvec_D[j] - 1]);
      }
    }

    for (j in 1:T2){
      vector[NMiss_results[j] - 1] tmp = raw_xi[1:NMiss_results[j] - 1, j] * sigma_xi;
      xi[miss_results[j, 1:NMiss_results[j]], j] = sum_to_zero_QR(tmp, Q_r[1:NMiss_results[j] * 2,j]);
    }
  }
  // M2
  for (j in 2:D) m2_quality[j] = m2_raw_quality[j - 1] * m2_sigma_quality[j - 1];
  m2_quality[1] = zeros;

}
model {
  // M1
  rho_x1 ~ normal(50, 10);
  rho_x2 ~ gamma(20,10);
  alpha ~ student_t(10, 0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(eta) ~ std_normal();

  sigma_mu_pollster ~ normal(0, 0.8);
  to_vector(raw_mu_pollster) ~ std_normal();
  for (j in 1:T2) raw_xi[,j] ~ normal(0, xi_raw_sigma[j]);
  sigma_xi ~ normal(0, 0.5);

  to_vector(raw_tau) ~ std_normal();
  sigma_tau ~ normal(0, 0.3);

  profile("llh"){
      for (j in 1:50){
        {
          int index[NMiss[j]] = miss[j, 1:NMiss[j]];
          vector[NMiss[j]] theta = softmax(
            f[,indicator[j]][index] +
            mu_pollster[,p[j]][index] +
            tau[,j][index]);
          y[miss[j, 2 - abs_inc[j]:NMiss[j]], j] ~ multinomial(
            theta[2 - abs_inc[j]:NMiss[j]]/(1 - {theta[1], 0.0}[abs_inc[j] + 1])
          );
        }
      }
  }

  for (j in 1:(T2 - 3)){
    {
      int index[NMiss_results[j]] = miss_results[j, 1:NMiss_results[j]];
      vector[NMiss[j]] theta = softmax(f[,result_indicator[j] - 15][index]);
      theta[1] ~ normal(0.25, 0.05);
    }
  }

  // Polling error distribution
  for (j in 1:(T2 - 1)){
    {
      int index[NMiss_results[j]] = miss_results[j, 1:NMiss_results[j]];
      vector[NMiss[j]] theta = softmax(f[,result_indicator[j]][index] +
                                       xi[index,j]);
      y_results[miss_results[j, 1:NMiss_results[j]], j] ~ multinomial(theta);
    }
  }



  // Make polling error year specific

  // M2
    m2_rho_long ~ normal(12, 1);
    to_vector(m2_eta) ~ std_normal();
    to_vector(m2_raw_quality) ~ std_normal();
    m2_sigma_quality ~ normal(0, m2_prior_sigma_quality);

//   for (j in 1:m2_N1){
//     vector[m2_NMiss[j]] m2_theta = softmax(m2_f[, j] +
//                                            m2_quality[,j])[m2_miss[j, 1:m2_NMiss[j]]];
//     m2_y[j, m2_miss[j, 1:m2_NMiss[j]]] ~ multinomial(m2_theta/sum(m2_theta));
//   }
//
//   // Partial pooling
//   {
//     vector[NMiss_results[T2]] theta = softmax(m2_f[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1] + m2_quality[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1]);
//     prediction ~ dirichlet(10000000 * theta/sum(theta));
//     prediction ~ dirichlet(10000000 * softmax(f[,T2 * T1][miss_results[T2, 1:NMiss_results[T2]]] + xi[miss_results[T2, 1:NMiss_results[T2]],T2]));
//   }
}
generated quantities {
  matrix[T, D] m1_y2 = rep_matrix(0.0, T, D);
  matrix[m2_N, D] m2_y2 = rep_matrix(0.0, m2_N, D);
  matrix[D, N1] y_rep;

  for (j in 1:T){
    m1_y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(f[,j][miss_pred[j, 1:NMiss_pred[j]]])';
  }
  // for (j in 1:m2_N1 + m2_N2)
  //   m2_y2[j] = softmax(m2_f[,j])';
  for (j in 1:N1){
    {
      int index[NMiss[j]] = miss[j, 1:NMiss[j]];
      vector[NMiss[j]] theta = softmax(
        f[,indicator[j]][index] + mu_pollster[,p[j]][index]);
      y_rep[miss[j, 2 - abs_inc[j]:NMiss[j]], j] = to_vector(multinomial_rng(
        theta[2 - abs_inc[j]:NMiss[j]]/(sum(theta[2 - abs_inc[j]:NMiss[j]])),
          sum(y[miss[j, 2 - abs_inc[j]:NMiss[j]], j])))/(1.0 * sum(y[miss[j, 2 - abs_inc[j]:NMiss[j]], j]));
        }
      }
}

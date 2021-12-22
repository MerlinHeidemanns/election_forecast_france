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
  real<lower = 1> t1[T1];

  int P[T2];
  int NPolls_Pollster[sum(P) - 3];
  int p[N1];
  int<lower = 1, upper = T2> index_election[N1];
  int<lower = 1, upper = T1> index_week[N1];

  int indicator[N1];
  int y[D, N1];
  int N1_abstention;
  int y_abstention[N1_abstention];
  int n_abstention[N1_abstention];
  int index_abstention[N1_abstention];

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
  int m2_y[m2_N1, D + 1];
  int m2_NMiss[m2_N1 + m2_N2];
  int m2_miss[m2_N1 + m2_N2, D + 1];
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
  real<lower=0>                rho_short;
  real<lower=0>                rho_long;
  vector<lower=0>[D]       alpha;
  cholesky_factor_corr[D]  L_Omega;
  matrix[D - 1, T1]            eta[T2];
  matrix[D, T2]                raw_xi;
  real<lower = 0>              sigma_xi;
  matrix[D - 1, sum(P)]        raw_mu_pollster;
  real<lower = 0>              sigma_mu_pollster;
  // matrix[D - 1, N1]            raw_tau;
  // real<lower = 0>              sigma_tau;
  real                         phi;
  real                         sigma_gamma;
  vector[D - 1]                    raw_gamma;
  vector[N1]                   raw_omnicron;
  real<lower = 0>              sigma_omnicron;

  // M2
  real<lower=0>                m2_rho_long;
  matrix[D, m2_N]              m2_eta;
  real<lower = 0>              m2_sigma_quality;
  matrix[D, m2_N]              m2_raw_quality;

  // Prediction
  simplex[NMiss_results[T2]] prediction;

}
transformed parameters {
  matrix[D + 1, m2_N]      m2_f;
  matrix[D + 1, m2_N]  m2_quality;
  matrix[D - 1, D - 1] L_Sigma1 = diag_pre_multiply(alpha[2:D], L_Omega[2:D, 2:D]);
  matrix[D, D]         L_Sigma2 = diag_pre_multiply(alpha, L_Omega);
  matrix[D, T]         f;
  matrix[D, sum(P)]    mu_pollster;
  matrix[D, T2]        xi;
  vector[N1]           omnicron;
  matrix[D + 1, m2_N1]     m2_theta;
  vector[D]            gamma;

  {
    profile("gp_exp_quad_cov"){
      matrix[T1, T1] K = gp_exp_quad_cov(t1, 1.0, rho_short);
      matrix[m2_N, m2_N] m2_K = gp_exp_quad_cov(m2_x, 1.0, m2_rho_long);
      for (i in 1:T1)
        K[i, i] = K[i, i] + delta;
      for (i in 1:m2_N) m2_K[i, i] = m2_K[i, i] + delta;

      matrix[T1, T1] chol_K = cholesky_decompose(K)';
      // M1


      for (i in 1:T2){
        f[1:D - 1, 1 + (i - 1) * 16:i * 16] = L_Sigma1 * eta[i] * chol_K;
      }
      f[D] = zerosT;
      // M2
      m2_f[1:D] = L_Sigma2 * m2_eta * cholesky_decompose(m2_K)';
      m2_f[D + 1] = zeros;
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
    //
    // for (j in 1:sum(P) - 3){
    //   tau[miss[j, 1:NMiss[j] - 1], supvec_C[j]:supvec_D[j] - 1] =
    //     sigma_tau * raw_tau[1:NMiss[j] - 1,supvec_C[j]:supvec_D[j] - 1];
    //   for (i in supvec_C[j]:supvec_D[j] - 1){
    //     tau[1,i] = {0, tau[1, i]}[abs_inc[i] + 1];
    //   }
    //   for (i in supvec_C[j]:supvec_D[j] - 1) tau[NMiss[j], i] = -sum(tau[miss[j, 1:NMiss[j] - 1], i]);
    //   for (i in 1:NMiss[j]){
    //     tau[miss[j, i], supvec_D[j]] = -sum(tau[miss[j, i], supvec_C[j]:supvec_D[j] - 1]);
    //   }
    // }

    for (j in 1:T2){
      vector[NMiss_results[j] - 1] tmp = raw_xi[1:NMiss_results[j] - 1, j] * sigma_xi;
      xi[miss_results[j, 1:NMiss_results[j]], j] = append_row(tmp, - sum(tmp));
    }
  }

  // M2
  for (j in 2:D + 1) m2_quality[j] = m2_raw_quality[j - 1] * m2_sigma_quality;
  m2_quality[1] = zeros;

  for (j in 1:m2_N1){
    m2_theta[m2_miss[j, 1:m2_NMiss[j]], j] = softmax(m2_f[m2_miss[j, 1:m2_NMiss[j]], j] +
                                                     m2_quality[m2_miss[j, 1:m2_NMiss[j]],j]);
  }


  omnicron = sigma_omnicron * raw_omnicron .* sqrt(to_vector(index_week));
  gamma[1:D - 1] = raw_gamma * sigma_gamma;
  gamma[D] = -sum(gamma[1:D - 1]);

}
model {
  // M1
  rho_short ~ normal(50, 10);
  rho_long ~ gamma(20,10);
  alpha ~ student_t(10, 0, 0.5);
  L_Omega ~ lkj_corr_cholesky(5);
  for (j in 1:T2) to_vector(eta[j]) ~ std_normal();

  sigma_mu_pollster ~ normal(0, 0.08);
  to_vector(raw_mu_pollster) ~ std_normal();
  to_vector(raw_xi) ~ std_normal();
  sigma_xi ~ normal(0, 0.1);
  to_vector(raw_omnicron) ~ std_normal();
  sigma_omnicron ~ normal(0, 0.01);
  raw_gamma ~ std_normal();
  sigma_gamma ~ normal(0, 0.01);

  y_abstention ~ binomial_logit(n_abstention,
                phi * (16 - to_vector(index_week[index_abstention])) +
                logit(m2_theta[1]'[index_election[index_abstention]]) +
                omnicron[index_abstention]);


  profile("llh"){
    {
      for (j in 1:N1){
        {
          int index[NMiss[j]] = miss[j, 1:NMiss[j]];
          vector[NMiss[j]] theta = softmax(
            f[index,indicator[j]] +
            mu_pollster[index,p[j]] +
            omnicron[j] * gamma[index]
          );
          y[miss[j, 1:NMiss[j]], j] ~ multinomial(theta[1:NMiss[j]]);
        }
      }
    }
  }

  // // // Polling error distribution
  // for (j in 1:(T2 - 1)){
  //   {
  //     int index[NMiss_results[j]] = miss_results[j, 1:NMiss_results[j]];
  //     vector[NMiss[j]] theta = softmax(f[,result_indicator[j]][index] +
  //                                      xi[index,j]);
  //     y_results[miss_results[j, 1:NMiss_results[j]], j] ~ multinomial(theta);
  //   }
  // }



  // Make polling error year specific

  // M2
    m2_rho_long ~ normal(12, 1);
    to_vector(m2_eta) ~ std_normal();
    to_vector(m2_raw_quality) ~ std_normal();
    m2_sigma_quality ~ normal(0, m2_prior_sigma_quality);

  for (j in 1:m2_N1){
    m2_y[j, m2_miss[j, 1:m2_NMiss[j]]] ~ multinomial(m2_theta[m2_miss[j, 1:m2_NMiss[j]], j]);
  }
//
// //   // Partial pooling
  {
    vector[NMiss_results[T2]] pred =
        softmax(f[miss_results[T2, 1:NMiss_results[T2]],T2 * T1] +
                xi[miss_results[T2, 1:NMiss_results[T2]],T2]);
      //   vector[NMiss_results[T2]] theta =
      // softmax(m2_f[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1] +
      //         m2_quality[miss_results[T2, 1:NMiss_results[T2]],m2_N1 + 1]);
       // prediction ~ dirichlet(10 * theta);
       prediction ~ dirichlet(10 * pred);
  }
}
generated quantities {
  matrix[T, D] m1_y2 = rep_matrix(0.0, T, D);
  // matrix[m2_N, D] m2_y2 = rep_matrix(0.0, m2_N, D);
  matrix[D, N1] y_rep;

  for (j in 1:T){
    m1_y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(f[,j][miss_pred[j, 1:NMiss_pred[j]]])';
  }
  // for (j in 1:m2_N1 + m2_N2)
  //   m2_y2[j] = softmax(m2_f[,j])';
  // for (j in 1:N1){
  //   {
  //     int index[NMiss[j]] = miss[j, 1:NMiss[j]];
  //     vector[NMiss[j]] theta = softmax(
  //       f[,indicator[j]][index]);
  //     y_rep[miss[j, 2:NMiss[j]], j] = to_vector(multinomial_rng(
  //       theta[2:NMiss[j]] + (lambda * theta[1]),
  //         sum(y[miss[j, 2 - abs_inc[j]:NMiss[j]], j])))/(1.0 * sum(y[miss[j, 2 - abs_inc[j]:NMiss[j]], j]));
  //       }
  //     }
}

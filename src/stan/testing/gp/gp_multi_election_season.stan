data {
  int<lower=1> N1;
  int<lower=1> D;
  int<lower=1> T1;
  int<lower=1> T2;
  real<lower = 1, upper = T1> t1[T1 * T2];
  real<lower = 1, upper = T2> t2[T1 * T2];

  int P;
  int p[N1];

  int indicator[N1];
  int y[D, N1];
  int abs_inc[N1];

  int y_results[D, T2 - 1];
  int result_indicator[T2 - 1];
  int NMiss_results[T2 - 1];
  int miss_results[T2 - 1, D];

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
  int m2_N = m2_N1 + m2_N2;
  real m2_x[m2_N];
  for (n1 in 1:m2_N1) m2_x[n1] = m2_x1[n1];
  for (n2 in 1:m2_N2) m2_x[N1 + n2] = m2_x2[n2];

}
parameters {
  real<lower=0> rho_x1;
  real<lower=0> rho_x2;
  vector<lower=0>[D] alpha;
  cholesky_factor_corr[D] L_Omega;
  matrix[T, D] eta;
  //matrix[D, T2] xi;
  real<lower = 0> sigma_xi;
  matrix[D - 1, P - 1] raw_mu_pollster;
  real<lower = 0> sigma_mu_pollster;


  real<lower=0> m2_rho_short;
  real<lower=0> m2_rho_long;
  matrix[m2_N, D] m2_eta;
  vector<lower = 0>[D - 1] m2_sigma_quality;
  matrix[D - 1, m2_N] m2_raw_quality;
}
transformed parameters {
  matrix[m2_N, D] m2_f;
  matrix[D, m2_N] m2_quality;
  cholesky_factor_cov[D] L_Sigma = diag_pre_multiply(alpha, L_Omega);
  matrix[D, T] f;
  matrix[D, P] mu_pollster;
  {
    //profile("gp_exp_quad_cov"){
      matrix[T, T] K = gp_exp_quad_cov(t1, 1.0, rho_x1) .* gp_exp_quad_cov(t2, 1.0, rho_x2);
      matrix[m2_N, m2_N] m2_K = gp_exp_quad_cov(m2_x, 1.0, m2_rho_long);

      // diagonal elements
      // M1
      for (i in 1:T)
        K[i, i] = K[i, i] + delta;
      f = (cholesky_decompose(K) * eta
          * L_Sigma')';
      // M2
      for (i in 1:m2_N) m2_K[i, i] = K[i, i] + delta;

      m2_f = cholesky_decompose(m2_K) * m2_eta
          * L_Sigma';
  }

  }
  // M1
  mu_pollster[1:D - 1, 1:P - 1] = raw_mu_pollster * sigma_mu_pollster;;
  for (j in 1:D - 1) mu_pollster[j, P] = -sum(mu_pollster[j, 1:P - 1]);
  for (j in 1:P) mu_pollster[D, j] = -sum(mu_pollster[1:D - 1, j]);

  // M2
  for (j in 2:D) m2_quality[j] = m2_raw_quality[j - 1] * m2_sigma_quality[j - 1];
  m2_quality[1] = rep_row_vector(0.0, m2_N);

}
model {
  rho_x1 ~ normal(50, 10);
  rho_x2 ~ gamma(20,10);
  alpha ~ normal(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(eta) ~ std_normal();

  sigma_mu_pollster ~ normal(0, 0.1);
  to_vector(raw_mu_pollster) ~ std_normal();
  sigma_xi ~ normal(0, 0.1);


  profile("llh"){
      for (j in 1:N1){
        {
          int index[NMiss[j]] = miss[j, 1:NMiss[j]];
          vector[NMiss[j]] theta = softmax(
            f[,indicator[j]][index] + mu_pollster[,p[j]][index]
          );
          y[miss[j, 2 - abs_inc[j]:NMiss[j]], j] ~ multinomial(
            theta[2 - abs_inc[j]:NMiss[j]]/(sum(theta[2 - abs_inc[j]:NMiss[j]]))
          );
        }
      }
    for (j in 1:(T2 - 1)){
      y_results_transpose[miss_results[j, 1:NMiss_results[j]], j] ~ multinomial(
          softmax(
            f[,result_indicator[j]][miss_results[j, 1:NMiss_results[j]]] +
          )
        0);
    }
  }
}
generated quantities {
  matrix[T, D] y2 = rep_matrix(0.0, T, D);
  for (j in 1:T){
    y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(f[,j][miss_pred[j, 1:NMiss_pred[j]]])';
  }
}

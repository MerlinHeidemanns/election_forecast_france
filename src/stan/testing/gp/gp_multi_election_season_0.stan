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
  matrix[N1, D] data_mu;
  matrix[N1, D] data_sigma;
  int abs_inc[N1];

  matrix[T2, D] results;
  int result_indicator[T2];
  int NMiss_results[T2];
  int miss_results[T2, D];

  int NMiss[N1];
  int miss[N1, D];

  int NMiss_pred[T1 * T2];
  int miss_pred[T1 * T2, D];
}
transformed data {
  real delta = 1e-9;
  int T = T1 * T2;
  matrix[N1, D] data_var = square(data_sigma);
}
parameters {
  real<lower=0> rho_x1;
  real<lower=0> rho_x2;
  vector<lower=0>[D - 1] alpha;
  cholesky_factor_corr[D - 1] L_Omega;
  matrix[T, D - 1] eta;
  vector<lower = 0>[P] sigma_pollster;
  matrix[T2, D - 1] raw_xi;
  real<lower = 0> sigma_xi;
}
transformed parameters {
  matrix[T, D - 1] f;
  matrix[N1, D] sigma_sum;
  matrix[T2, D] xi;
  {
    matrix[T, T] K = gp_exp_quad_cov(t1, 1.0, rho_x1) .* gp_exp_quad_cov(t2, 1.0, rho_x2);
    matrix[T, T] L_K;

    // diagonal elements
    for (i in 1:T)
      K[i, i] = K[i, i] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta
        * diag_pre_multiply(alpha, L_Omega)';
  }
  sigma_sum = sqrt(data_var + rep_matrix(sigma_pollster[p], D));

  for (j in 1:T2){
    xi[j,miss_results[j, 2:NMiss_results[j]]] = raw_xi[j, 1:NMiss_results[j] - 1] * sigma_xi;
    xi[j,1] = -sum(xi[j,miss_results[j, 2:NMiss_results[j]]]);
  }
}
model {
  rho_x1 ~ normal(3, 0.5);
  rho_x2 ~ normal(1, 0.5);
  alpha ~ normal(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(3);
  to_vector(eta) ~ std_normal();
  sqrt(sigma_pollster) ~ normal(0, 0.02);
  to_vector(raw_xi) ~ std_normal();
  sigma_xi ~ normal(0, 0.3);
  for (j in 1:N1){
    {
      vector[NMiss[j]] theta = softmax(append_row(0, f[indicator[j]]')[miss[j, 1:NMiss[j]]]);
      data_mu[j, miss[j, 2 - abs_inc[j]:NMiss[j]]] ~ normal(
        theta[2 - abs_inc[j]:NMiss[j]]/(sum(theta[2 - abs_inc[j]:NMiss[j]])),
        sigma_sum[j, miss[j, (2 - abs_inc[j]):NMiss[j]]]);
    }
  }
  for (j in 1:T2){
    results[j, miss_results[j, 1:NMiss_results[j]]] ~ normal(
      softmax(
        append_row(0, f[result_indicator[j]]')[miss_results[j, 1:NMiss_results[j]]] +
        xi[j,miss_results[j, 1:NMiss_results[j]]]'),
        0.001);
  }
}
generated quantities {
  matrix[T, D] y2 = rep_matrix(0.0, T, D);
  for (j in 1:T)
    y2[j, miss_pred[j, 1:NMiss_pred[j]]] = softmax(append_row(0, f[j]'[miss_pred[j, 1:NMiss_pred[j]]]))';
}

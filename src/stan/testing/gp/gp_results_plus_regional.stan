data {
  int<lower=1> N1[2];
  int<lower=1> N2;
  int<lower=1> D;
  real x1[sum(N1)];
  real x2[N2];
  int type1[sum(N1)];
  int type2[N2];
  int y[sum(N1), D];
  int NMiss[sum(N1) + N2];
  int miss[sum(N1) + N2, D];
  real prior_sigma_quality;
}
transformed data {
  real delta = 1e-9;
  int N = sum(N1) + N2;
  real x[N];
  for (n1 in 1:sum(N1)) x[n1] = x1[n1];
  for (n2 in 1:N2) x[sum(N1) + n2] = x2[n2];
}
parameters {
  real<lower=0> rho_short;
  real<lower=0> rho_long;
  vector<lower=0>[D] alpha;
  cholesky_factor_corr[D] L_Omega;
  matrix[N, D] eta;
  vector<lower = 0>[D - 1] sigma_quality;
  vector<lower = 0>[D - 1] sigma_swing;
  matrix[D - 1, N1[1]] raw_quality;
  matrix[D - 1, N1[2]] raw_swing;
  real<lower = 0> sigma_aggregation;
  vector[D - 1] raw_aggregation;
}
transformed parameters {
  matrix[N, D] f;
  matrix[D, N1[2]] swing;
  matrix[D, N1[1]] quality;
  vector[D] aggregation;

  {
    matrix[N, N] K;
    matrix[N, N] L_K;

    K = gp_exp_quad_cov(x, 1.0, rho_long);//gp_exp_quad_cov(x, 1.0, rho_short) +
        //gp_exp_quad_cov(x, 1.0, rho_long);

    // diagonal elements
    for (i in 1:N)
      K[i, i] = K[i, i] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta
        * diag_pre_multiply(alpha, L_Omega)';
  }
  for (j in 2:D) quality[j] = raw_quality[j - 1] * sigma_quality[j - 1];
  for (j in 2:D) swing[j]   = raw_swing[j - 1] * sigma_swing[j - 1];
  for (j in 1:N1[2]) swing[1,j] = -sum(swing[2:D, j]);
  for (j in 1:N1[1]) quality[1,j] = -sum(quality[2:D, j]);
  aggregation[1:D - 1] = raw_aggregation * sigma_aggregation;
  aggregation[D] = -sum(aggregation[1:D - 1]);
}
model {
  rho_short ~ normal(6, 3);
  rho_long ~ normal(12, 1);
  alpha ~ normal(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(4);
  to_vector(eta) ~ std_normal();
  to_vector(raw_quality) ~ std_normal();
  sigma_quality ~ normal(0, prior_sigma_quality);

  to_vector(raw_swing) ~ std_normal();
  sigma_swing ~ normal(0, 0.4);

  raw_aggregation ~ std_normal();
  sigma_aggregation ~ normal(0, 0.4);


  for (j in 1:N1[1]){
    vector[NMiss[j]] theta = softmax(f[j, miss[j, 1:NMiss[j]]]' + quality[miss[j, 1:NMiss[j]],j]);
    y[j, miss[j, 1:NMiss[j]]] ~ multinomial(theta);
  }
  for (j in N1[1] + 1:sum(N1)){
    vector[NMiss[j]] theta = softmax(f[j, miss[j, 1:NMiss[j]]]' +
                                      swing[miss[j, 1:NMiss[j]], j - N1[1]] +
                                      aggregation[miss[j, 1:NMiss[j]]]);
    y[j, miss[j, 1:NMiss[j]]] ~ multinomial(theta);
  }
}
generated quantities {
  matrix[N, D] y2 = rep_matrix(0.0, N, D);
  for (j in 1:sum(N1)){
    y2[j] = softmax(f[j]')';
  }
  for (j in sum(N1) + 1:N)
    y2[j] = softmax(f[j]')';
}

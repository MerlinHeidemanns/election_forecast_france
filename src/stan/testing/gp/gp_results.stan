data {
  int<lower=1> N1;
  int<lower=1> N2;
  int<lower=1> D;
  real x1[N1];
  real x2[N2];
  int y[N1, D];
  int NMiss[N1 + N2];
  int miss[N1 + N2, D];
  real prior_sigma_quality;
}
transformed data {
  real delta = 1e-9;
  int N = N1 + N2;

  real x[N];
  for (n1 in 1:N1) x[n1] = x1[n1];
  for (n2 in 1:N2) x[N1 + n2] = x2[n2];
}
parameters {
  real<lower=0> rho_short;
  real<lower=0> rho_long;
  vector<lower=0>[D] alpha;
  cholesky_factor_corr[D] L_Omega;
  matrix[N, D] eta;
  vector<lower = 0>[D - 1] sigma_quality;
  matrix[D - 1, N] raw_quality;
}
transformed parameters {
  matrix[N, D] f;
  matrix[D, N] quality;

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
  quality[1] = rep_row_vector(0.0, N);
}
model {
  rho_short ~ normal(6, 3);
  rho_long ~ normal(12, 1);
  alpha ~ normal(0, 0.5);
  L_Omega ~ lkj_corr_cholesky(4);
  to_vector(eta) ~ std_normal();
  to_vector(raw_quality) ~ std_normal();
  sigma_quality ~ normal(0, prior_sigma_quality);


  for (j in 1:N1){
    vector[NMiss[j]] theta = softmax(f[j]' + quality[,j])[miss[j, 1:NMiss[j]]];
    y[j, miss[j, 1:NMiss[j]]] ~ multinomial(theta/sum(theta));
  }
}
generated quantities {
  matrix[N, D] y2 = rep_matrix(0.0, N, D);
  for (j in 1:N1)
    y2[j] = softmax(f[j]')';
  for (j in N1 + 1:N)
    y2[j] = softmax(f[j]')';
}

data {
  int<lower=1> N1;
  int<lower=1> N2;
  int<lower=1> D;
  real x1_old[N1];
  real x2_old[N1];
  real x1_new[N2];
  real x2_new[N2];
  int y[N1, D];
  int n[N1];
}
transformed data {
  real delta = 1e-9;
  int N = N1 + N2;
  matrix[N1, D] mu_binom;
  matrix[N1, D] sigma_binom;
  vector[N1 * D] mu_binom_vec;
  vector[N1 * D] sigma_binom_vec;
  real x1[N];
  real x2[N];
  for (i in 1:D){
    mu_binom[,i] = to_vector(y[, i])./to_vector(n);
    sigma_binom[,i] = 4 * sqrt((mu_binom[,i] .* (rep_vector(1.0, N1) - mu_binom[, i]))./to_vector(n));
  }
  sigma_binom_vec = to_vector(sigma_binom);
  mu_binom_vec = to_vector(mu_binom);
  for (n1 in 1:N1){
    x1[n1] = x1_old[n1];
    x2[n1] = x2_old[n1];
  }
  for (n2 in 1:N2){
    x1[N1 + n2] = x1_new[n2];
    x2[N1 + n2] = x2_new[n2];
  }
}
parameters {
  real<lower=0> rho_x1;
  real<lower=0> rho_x2;
  vector<lower=0>[D] alpha;
  cholesky_factor_corr[D] L_Omega;
  matrix[N, D] eta;
}
transformed parameters {
  matrix[N, D] f;
  {
    matrix[N, N] K = gp_exp_quad_cov(x1, 1.0, rho_x1) .* gp_exp_quad_cov(x2, 1.0, rho_x2);
    matrix[N, N] L_K;

    // diagonal elements
    for (i in 1:N)
      K[i, i] = K[i, i] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta
        * diag_pre_multiply(alpha, L_Omega)';
  }
}
model {
  rho_x1 ~ normal(3, 0.5);
  rho_x2 ~ normal(1, 0.5);
  alpha ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(3);
  to_vector(eta) ~ std_normal();
  to_vector(f[1:N1]) ~ normal(logit(mu_binom_vec), sigma_binom_vec);
}
generated quantities {
  matrix[N2, D] y2;
  for (n2 in 1:N2)
    y2[n2] = inv_logit(f[N1 + n2]);
}

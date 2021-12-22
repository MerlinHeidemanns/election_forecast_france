data {
  int<lower=1> N;
  int<lower=1> D;
  int<lower = 1> T;
  real rho_short;
  real rho_long;
  vector[D] alpha;
  real sigma;
  real eta;
}
transformed data {
  real delta = 1e-9;
  row_vector[N] zeros[D];
  for (j in 1:D){
    zeros[j] = rep_vector(0.0, N)';
  }
}
model {}
generated quantities {
  real x[N];
  matrix[N, D] f;
  int y[N, D];
  int n[N];
  cholesky_factor_corr[D] L_Omega;

  for (j in 1:N)
    x[j] = uniform_rng(0,T);
  L_Omega = lkj_corr_cholesky_rng(D, eta);
  {
    matrix[N, N] cov;
    matrix[N, N] L_cov;

    cov = gp_exp_quad_cov(x, 1.0, rho_short) +
          gp_exp_quad_cov(x, 1.0, rho_long);
    for (j in 1:N)
      cov[j, j] = cov[j, j] + 1e-12;
    {
      matrix[N, D] z;
      for (j in 1:D) z[,j] = to_vector(normal_rng(rep_vector(0.0, N), 1));
      L_cov = cholesky_decompose(cov);
      f = L_cov * z * diag_pre_multiply(alpha, L_Omega);
    }
  }
  for (j in 1:N){
    n[j] = 1000;
    y[j] = binomial_rng(n[j], inv_logit(f[j]'));
  }
}
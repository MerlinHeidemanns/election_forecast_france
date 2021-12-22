data {
  int<lower=1> N;
  real<lower=0> length_scale;
  real<lower=0> alpha;
}
transformed data {
  vector[N] zeros;
  zeros = rep_vector(0, N);
}
model {}
generated quantities {
  real x1[N];
  real x2[N];
  vector[N] f;
  int y[N];
  int n[N];
  for (j in 1:N)
    x1[j] = uniform_rng(0,30);
    x2[j] = uniform_rng(0,30);
  {
    matrix[N, N] cov;
    matrix[N, N] L_cov;
    cov =
      gp_cov_exp_quad(x1, alpha_short, length_scale_short) +
      gp_cov_exp_quad(x1, alpha_long, length_scale_long);
      gp_matern32_cov()
    for (j in 1:N)
      cov[j, j] = cov[j, j] + 1e-12;
    L_cov = cholesky_decompose(cov);
    f = multi_normal_cholesky_rng(zeros, L_cov);
  }
  for (j in 1:N){
    n[j] = 1000;
    y[j] = binomial_rng(n[j], inv_logit(f[j]));
  }
}

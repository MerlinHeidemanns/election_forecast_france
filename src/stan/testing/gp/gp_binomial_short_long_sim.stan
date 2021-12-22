data {
  int<lower=1> N;
  real<lower = 1> T;
  real<lower=0> length_scale_short;
  real<lower=0> alpha_short;
  real<lower=0> length_scale_long;
  real<lower=0> alpha_long;
}
transformed data {
  vector[N] zeros;
  zeros = rep_vector(0, N);
}
model {}
generated quantities {
  real x[N];
  vector[N] f;
  int y[N];
  int n[N];
  for (j in 1:N)
    x[j] = uniform_rng(0,T);
  {
    matrix[N, N] cov;
    matrix[N, N] L_cov;
    cov =
      gp_exp_quad_cov(x, alpha_short, length_scale_short) +
      gp_exp_quad_cov(x, alpha_long, length_scale_long);
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

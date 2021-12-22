data {
  int<lower = 1> R;
  int<lower = 1> T;
  int<lower=1> N;
  int<lower = 1> G;
  real<lower = 0> alpha;
  real<lower = 0> mu_length_scaleR;
  real<lower = 0> mu_length_scaleG;
  real<lower = 0> sigma_length_scaleR;
  real<lower = 0> sigma_length_scaleG;
}
transformed data {
  vector[T] zeros;
  vector[G] probG;
  vector[T] probT;
  vector[R] probR;
  real sequenceX[T];
  int sequenceG[G];
  zeros = rep_vector(0.0, T);
  probG = softmax(rep_vector(1.0/G, G));
  probT = softmax(rep_vector(1.0/T, T));
  probR = softmax(rep_vector(1.0/R, R));
  {
    int count = 0;
    for (i in 1:T){
      count = count + 1;
      sequenceX[i] = count;
    }
  }
}
model {}
generated quantities {
  int x[N];
  int g[N];
  int r[N];
  vector[T] f[G, R];
  int y[N];
  int n[N];
  vector<lower = 0>[G] length_scaleG;
  vector<lower = 0>[R] length_scaleR;
  for (i in 1:G){
    length_scaleG[i] = normal_rng(mu_length_scaleG, sigma_length_scaleG);
  }
  for (i in 1:R){
    length_scaleR[i] = normal_rng(mu_length_scaleR, sigma_length_scaleR);
  }

  for (j in 1:N){
    x[j] = categorical_rng(probT);
    g[j] = categorical_rng(probG);
    r[j] = categorical_rng(probR);
  }
  {
    matrix[T, T] cov[G, R];
    matrix[T, T] L_cov[G, R];
    for (i in 1:G){
      for (j in 1:R)
      cov[i, j] =
        gp_exp_quad_cov(sequenceX, alpha, length_scaleG[i]) +
        gp_exp_quad_cov(sequenceX, alpha, length_scaleR[j]);
    }
    for (j in 1:T){
      for (i in 1:G){
        for (q in 1:R){
          cov[i, q, j, j] = cov[i, q, j, j] + 1e-12;
        }
      }
    }
    for (i in 1:G){
      for (j in 1:R){
        L_cov[i, j] = cholesky_decompose(cov[i, j]);
        f[i, j] = multi_normal_cholesky_rng(zeros, L_cov[i, j]);
      }
    }
  }
  for (j in 1:N){
    n[j] = 1000;
    y[j] = binomial_rng(n[j], inv_logit(f[g[j], r[j], x[j]]));
  }
}

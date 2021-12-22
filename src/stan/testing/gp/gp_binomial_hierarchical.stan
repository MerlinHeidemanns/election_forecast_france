functions {
  vector gp_pred_rng(real[] x_pred,
                     vector y_is,
                     real[] x_is,
                     real alpha,
                     real length_scale,
                     vector sigma) {
    vector[size(x_pred)] f_pred;
    int N_pred;
    int N;
    N_pred = size(x_pred);
    N = rows(y_is);
    {
      matrix[N, N] L_Sigma;
      vector[N] K_div_y_is;
      matrix[N, N_pred] k_x_is_x_pred;
      matrix[N, N_pred] v_pred;
      vector[N_pred] f_pred_mu;
      matrix[N_pred, N_pred] cov_f_pred;
      matrix[N_pred, N_pred] nug_pred;
      matrix[N, N] Sigma;
      Sigma = gp_exp_quad_cov(x_is, alpha, length_scale);
      for (j in 1:N){
        Sigma[j, j] = Sigma[j, j] + sigma[j];
      }
      L_Sigma = cholesky_decompose(Sigma);
      // This conditions on the observed data to make predictions.
      K_div_y_is = mdivide_left_tri_low(L_Sigma, y_is);
      K_div_y_is = mdivide_right_tri_low(K_div_y_is',L_Sigma)';
      k_x_is_x_pred = cov_exp_quad(x_is, x_pred, alpha, length_scale);
      f_pred_mu = (k_x_is_x_pred' * K_div_y_is);
      v_pred = mdivide_left_tri_low(L_Sigma, k_x_is_x_pred);
      cov_f_pred = gp_exp_quad_cov(x_pred, alpha, length_scale) - v_pred' * v_pred;
      nug_pred = diag_matrix(rep_vector(1e-12,N_pred));
      f_pred = multi_normal_rng(f_pred_mu, cov_f_pred + nug_pred);
    }
    return f_pred;
  }
}
data {
  int<lower=1> T;
  int<lower=1> R;
  int<lower=1> G;
  int<lower=1> N;
  int y[N];
  int n[N];
  real x[N];
  int g[N];
  int r[N];
}
transformed data {
  vector[N] mu = to_vector(y)./to_vector(n);
  vector[N] ones = rep_vector(1.0, N);
}
parameters {
  vector<lower = 0>[G] length_scaleG;
  vector<lower = 0>[R] length_scaleR;
  real<lower=0> alpha;
  vector[N] eta[G, R];
}
transformed parameters {
  vector[N] f[G, R];
  vector[N] logit_mu = logit(mu);
  vector[N] logit_sigma = sqrt(((mu .* (ones - mu)) ./to_vector(n))) * 4;
  {
    matrix[T, T] L[G, R];
    matrix[T, T] K[G, R];
    for (i in 1:R){
      for (j in 1:G){
        K[j, i] = gp_exp_quad_cov(x, alpha, length_scaleG[g[i]]) +
          gp_exp_quad_cov(x, alpha, length_scaleR[r[i]]);
      }
    }

    for (i in 1:R){
      for (j in 1:G){
        for (q in 1:T){
          K[j, i, q, q] = K[j, i, q, q] + 1e-12;
        }
        L[j, i] = cholesky_decompose(K[j, i]);
        f[j, i] = L[j, i] * eta[j, i];
      }
    }
  }
}
model {
  length_scaleG ~ normal(10, 1);
  length_scaleR ~ normal(5, 1);
  alpha ~ normal(0, 0.5);
  for (i in 1:R) for (j in 1:G) eta[j, i] ~ normal(0, 1);
  for (i in 1:N){
    logit_mu[i] ~ normal(f[g[i], r[i]], logit_sigma[i]);
  }
}
// generated quantities {
//   vector[N_pred] f_pred;
//   vector[N_pred] y_pred;
//   f_pred = gp_pred_rng(x_pred, logit_mu, x, alpha, length_scale, logit_sigma);
//   y_pred = inv_logit(f_pred);
// }


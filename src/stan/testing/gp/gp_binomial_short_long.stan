functions {
  vector gp_pred_rng(real[] x_pred,
                     vector y_is,
                     real[] x_is,
                     real alpha_short,
                     real alpha_long,
                     real length_scale_short,
                     real length_scale_long,
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
      Sigma = gp_exp_quad_cov(x_is, alpha_short, length_scale_short) +
              gp_exp_quad_cov(x_is, alpha_long, length_scale_long);
      for (j in 1:N){
        Sigma[j, j] = Sigma[j, j] + sigma[j];
      }
      L_Sigma = cholesky_decompose(Sigma);
      // This conditions on the observed data to make predictions.
      K_div_y_is = mdivide_left_tri_low(L_Sigma, y_is);
      K_div_y_is = mdivide_right_tri_low(K_div_y_is',L_Sigma)';
      k_x_is_x_pred = cov_exp_quad(x_is, x_pred, alpha_short, length_scale_short) +
                      cov_exp_quad(x_is, x_pred, alpha_long, length_scale_long);
      f_pred_mu = (k_x_is_x_pred' * K_div_y_is);
      v_pred = mdivide_left_tri_low(L_Sigma, k_x_is_x_pred);
      cov_f_pred = gp_exp_quad_cov(x_pred, alpha_short, length_scale_short) +
                   gp_exp_quad_cov(x_pred, alpha_long, length_scale_long) -
                   v_pred' * v_pred;
      nug_pred = diag_matrix(rep_vector(1e-12,N_pred));
      f_pred = multi_normal_rng(f_pred_mu, cov_f_pred + nug_pred);
    }
    return f_pred;
  }
}
data {
  int<lower=1> N;
  int<lower=1> N_pred;
  int y[N];
  int n[N];
  real x[N];
  real x_pred[N_pred];
}
transformed data {
  vector[N] mu = to_vector(y)./to_vector(n);
  vector[N] ones = rep_vector(1.0, N);
}
parameters {
  real<lower=0> length_scale_short;
  real<lower=0> length_scale_long;
  real<lower=0> alpha_short;
  real<lower=0> alpha_long;
  vector[N] eta;
}
transformed parameters {
  vector[N] f;
  vector[N] logit_mu = logit(mu);
  vector[N] logit_sigma = sqrt(((mu .* (ones - mu)) ./to_vector(n))) * 4;
  {
    matrix[N, N] L;
    matrix[N, N] K;
    K = gp_exp_quad_cov(x, alpha_short, length_scale_short) +
        gp_exp_quad_cov(x, alpha_long, length_scale_long);
    for (j in 1:N)
      K[j, j] = K[j, j] + 1e-12;
    L = cholesky_decompose(K);
    f = L * eta;
  }
}
model {
  length_scale_short ~ normal(10, 1);
  length_scale_long ~ normal(100, 5);
  alpha_short ~ normal(0, 0.5);
  alpha_long ~ normal(2, 0.5);
  eta ~ normal(0, 1);
  f ~ normal(logit_mu, logit_sigma);
}
generated quantities {
  vector[N_pred] f_pred;
  vector[N_pred] y_pred;
  f_pred = gp_pred_rng(x_pred, logit_mu, x, alpha_short, alpha_long, length_scale_short, length_scale_long, logit_sigma);
  y_pred = inv_logit(f_pred);
}


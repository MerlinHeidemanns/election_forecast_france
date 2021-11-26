functions {
  vector compute_prob(int P, vector party_positions, real department_position){
    vector[P] dist;
    for (j in 1:P) dist[j] = -fabs(party_positions[j] - department_position);
    return(softmax(dist));
  }
}
data {
  int<lower=0> N;
  int<lower =0> NDepartments;
  int K;
  matrix[N, K] x;
  int P;
  int id_Obs_departments[N];
  matrix[N, P] y;
}
parameters {
  real<lower = 0> sigma_positions;
  real<lower = 0> sigma_alpha;
  vector[NDepartments] raw_alpha;
  positive_ordered[P - 1] raw_positions;
  vector[K] beta;
  real<lower = 0> sigma_y;
}
transformed parameters {
  vector[NDepartments] alpha = raw_alpha * sigma_alpha;
}
model {
  matrix[P, 2] mu;
  matrix[N, P] y_hat;
  vector[N] y_star;
  vector[P] positions = append_row(-2, raw_positions - 2);
  y_star = alpha[id_Obs_departments] + x * beta;

  beta ~ normal(0, 1);
  to_vector(raw_alpha) ~ std_normal();
  sigma_alpha ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  raw_positions ~ normal(0, 3);
  for (j in 1:N){
    y_hat[j] = compute_prob(P, positions, y_star[j])';
  }
  to_vector(y) ~ normal(to_vector(y_hat), sigma_y);
}


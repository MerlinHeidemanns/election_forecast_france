functions {
  vector compute_theta(vector thres){
    int len_thres = size(thres);
    vector[len_thres] sorted_thres = sort_asc(thres) + pi();
    vector[len_thres] theta;
    theta[1] = sorted_thres[1]/(2 * pi()) + 1 - sorted_thres[len_thres]/(2 * pi());
    for (j in 2:len_thres){
      theta[j] = (sorted_thres[j] - sorted_thres[j - 1])/(2 * pi());
    }
    return(theta);
  }
}

data {
  int C;
  vector[C] y;
}
parameters {
  positive_ordered[C - 1] adj;
  unit_vector[2] mu_vec[C];
  real<lower = 0> sigma;
}
transformed parameters {
  vector<lower = -pi(), upper = pi()>[C - 1] t;
  for (j in 1:C - 1) t[j] = atan2(mu_vec[j, 2], mu_vec[j, 1]);
}
model {
  sigma ~ normal(0, 0.01);
  adj ~ normal(0, pi() * 2);
  t ~ von_mises(adj, 4);
  y ~ normal(compute_theta(append_row(-pi(), -pi() + t)), sigma);
}
generated quantities {
  vector[C] theta;
  theta = compute_theta(append_row(-pi(), -pi() + t));
}
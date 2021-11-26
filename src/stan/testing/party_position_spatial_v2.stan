functions {
  real compute_utility(row_vector pos_department, row_vector pos_party, real beta){
    // return(
    //   2 * (1 - 0.5) * pos_department * pos_party -
    //     0.5 * sqrt(square(pos_department - pos_party))
    // );
    return(
      2 * dot_product(pos_department, pos_party)
    );
  }
  vector compute_prob(int P, row_vector pos_department, matrix pos_party, real beta){
    vector[P] prob;
    for (j in 1:P){
      prob[j] = compute_utility(pos_department, pos_party[j], beta);
    }
    return(prob/sum(prob));
  }
}
data {
  int<lower=0> N;
  int<lower = 0> NDepartments;
  int<lower = 0> NElections;
  int id_Obs_departments[N];
  int id_Obs_elections[N];
  int D;
  int K;
  matrix[N, K] x;
  int P;
  matrix[N, P] y;
}
parameters {
  ordered[P] ord_party_position;
  matrix[P, D - 1] uno_party_position;
  matrix[K, D] beta;
  real<lower = 0> sigma_y;
  real<lower = 0, upper = 1> lambda;
  real<lower = 0> sigma_swing;
  matrix[NElections, D] raw_swing;
}
transformed parameters {
  matrix[NElections, D] swing;
  matrix[N, D] y_star;
  swing[1] = raw_swing[1] * sigma_swing;
  for (j in 2:NElections) swing[j] = swing[j - 1] + raw_swing[j] * sigma_swing;
  y_star = swing[id_Obs_elections] + x * beta;
}
model {
  matrix[N, P] y_hat;
  matrix[P, D] party_position;
  party_position[,1] = ord_party_position;
  party_position[,2:D] = uno_party_position;
  ord_party_position ~ normal(0, 1);
  to_vector(uno_party_position) ~ normal(0, 1);

  to_vector(beta) ~ normal(0, 0.5);
  sigma_y ~ normal(0, 0.05);
  lambda ~ beta(2, 2);
  to_vector(raw_swing) ~ std_normal();
  sigma_swing ~ normal(0, 0.25);
  for (j in 1:N){
    y_hat[j] = compute_prob(P, y_star[j],
                party_position, lambda)';
  }
  to_vector(y) ~ normal(to_vector(y_hat), sigma_y);
}


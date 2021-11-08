data {
  int NElections;
  int NPolls[NElections];
  int NPollsters;
  int NBloc_Presidents;
  int id_Polls_elections[sum(NPolls)];
  int NTime[NElections];
  int id_Polls_time[sum(NPolls)];
  int id_Polls_pollster[NElections];
  int<lower = 1, upper = NBloc_Presidents> id_Polls_bloc[sum(NPolls)];
  int y[sum(NPolls)];
  int n[sum(NPolls)];
  real beta_prior;
}
parameters {
  matrix[NElections, max(NTime)] raw_theta;
  vector<lower = 0>[NElections] sigma_theta;
  matrix[NPollsters, NBloc_Presidents] raw_alpha;
  real<lower = 0> sigma_alpha;

  vector[sum(NPolls)] raw_tau;
  real<lower = 0> sigma_tau;
}
transformed parameters {
  matrix[NElections, max(NTime)] theta;
  vector[sum(NPolls)] tau;
  matrix[NPollsters, NBloc_Presidents] alpha;
  theta[, 1] = logit(raw_theta[, 1] .* sigma_theta);
  for (tt in 2:max(NTime)){
    theta[,tt] = theta[, tt - 1] + raw_theta[,tt] .* sigma_theta;
  }

  alpha = raw_alpha * sigma_alpha;
  tau = raw_tau * sigma_tau;
}
model {
  ## Random walk
  to_vector(raw_theta) ~ std_normal();
  to_vector(raw_tau) ~ std_normal();
  sigma_theta ~ normal(0, 0.1);
  sigma_alpha ~ normal(0, 0.1);
  sigma_tau ~ normal(0, 0.1);
  to_vector(raw_alpha) ~ std_normal();

  y[1:NPolls[1]] ~ binomial_logit(n[1:NPolls[1]],
    theta[1][id_Polls_time[1:NPolls[1]]] +
    alpha[id_Polls_pollster[1:NPolls[1]],]' +
    tau[1:NPolls[1]]'
  );
  for (jj in 2:NElections){
    y[1 + sum(NPolls[1:jj - 1]):sum(NPolls[1:jj])] ~ binomial_logit(
      n[1 + sum(NPolls[1:jj - 1]):sum(NPolls[1:jj])],
      theta[jj][id_Polls_time[1 + sum(NPolls[1:jj - 1]):sum(NPolls[1:jj])]] +
      alpha[id_Polls_pollster[1 + sum(NPolls[1:jj - 1]):sum(NPolls[1:jj])]]' +
      tau[1 + sum(NPolls[1:jj - 1]):sum(NPolls[1:jj])]'
    );
  }
}
generated quantities {
  matrix[NElections, max(NTime)] prob_theta = inv_logit(theta);
}


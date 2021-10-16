data {
  int NPolls;
  int T;
  int tPolls[NPolls];
  int yPolls[NPolls];
  int nPolls[NPolls];

  int NElections;
  int tElections[NElections];
  int yElections[NElections];
  int nElections[NElections];
}
parameters {
  vector[T] raw_theta;
  real<lower = 0> sigma;
}
transformed parameters {
  vector[T] theta;
  theta[1] = raw_theta[1] * sigma;
  for (t in 2:T){
    theta[t] = theta[t - 1] + raw_theta[1] * sigma;
  }
}
model {
  raw_theta ~ std_normal();
  sigma ~ normal(0, 0.1);
  // Poll likelihood
  yPolls ~ binomial_logit(nPolls, theta[tPolls]);
  // Election likelihood
  yElections ~ binomial_logit(nElections, theta[tElections]);
}
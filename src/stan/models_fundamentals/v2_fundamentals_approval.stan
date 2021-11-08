data {
  int NPresidents;
  int NPolls;
  int NPollsters;
  int NPollsters_Presidents;
  int NTime;
  int election_points[NPresidents + 1];
  int id_Polls_time[NPolls];
  int<lower = 1, upper = NPresidents> id_Polls_president[NPolls];
  int<lower = 1, upper = NPollsters> id_Polls_pollster[NPolls];
  int<lower = 1, upper = NPollsters_Presidents> id_Polls_pollster_president[NPolls];
  vector[NPolls] id_Polls_time_president;
  vector[NPolls] y;
  vector[NPolls] n;
  real beta_prior;
}
parameters {
  vector[NTime] raw_theta;
  vector<lower = 0>[NPresidents] sigma_theta;
  vector[NPresidents] mean_shifts;
  vector<lower = 0>[NPollsters] sigma_pollster;
  real<lower = 0> sigma_mu_pollster;
  vector[NPollsters_Presidents] raw_mu_pollster_president;
  vector[NPresidents] beta;
  vector<lower = 0>[NPresidents] gamma;
}
transformed parameters {
  vector[NTime] theta;
  vector[NPollsters_Presidents] mu_pollster_president;
  for (j in 1:NPresidents){
      theta[election_points[j]] = mean_shifts[j] + raw_theta[election_points[j]] * sigma_theta[j];
      for (tt in (election_points[j] + 1):election_points[j + 1]){
      {
        real tmp_sigma = sqrt(sigma_theta[j]^2 + gamma[j]^2 * 1.0 * (election_points[j + 1] - tt)/(1.0 * election_points[j + 1]));
        theta[tt] = theta[tt - 1] + raw_theta[tt] * tmp_sigma;
      }
    }
  }
  mu_pollster_president = raw_mu_pollster_president * sigma_mu_pollster;
}
model {
  vector[NPolls] mu;
  vector[NPolls] sigma;
  raw_theta ~ std_normal();
  mean_shifts ~ normal(0, 0.2);
  sigma_theta ~ normal(0, 0.1);
  sigma_pollster ~ normal(0, 0.1);
  sigma_mu_pollster ~ normal(0, 0.05);
  raw_mu_pollster_president ~ std_normal();
  beta ~ normal(0, beta_prior);
  gamma ~ normal(0, 0.1);
  mu = inv_logit(mu_pollster_president[id_Polls_pollster_president] +
                 theta[id_Polls_time] +
                 beta[id_Polls_president] .* id_Polls_time_president);
  sigma = sigma_pollster[id_Polls_pollster];
  y ~ normal(mu, sigma);
}
generated quantities {
  vector[NTime] prob_theta = inv_logit(theta);
}

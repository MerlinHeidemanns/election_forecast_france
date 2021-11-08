data {
  int NElections;
  int NDepartements;
  int<upper = NElections * NDepartements> NObs;

  // Approval
  int NPolls;
  int NPollsters;
  int NPresidents;
  int NPollsters_Presidents;
  int NTime;
  int id_Polls_time[NPolls];
  int<lower = 1, upper = NPresidents> id_Polls_president[NPolls];
  int<lower = 1, upper = NPollsters> id_Polls_pollster[NPolls];
  int<lower = 1, upper = NPollsters_Presidents> id_Polls_pollster_president[NPolls];
  int y_approval[NPolls];
  int n_approval[NPolls];
  real beta_prior;

  int M;
  matrix[NObs, M] XDepartements;
  int NMiss_X[M];
  int id_X_miss[max(NMiss_X), M];

  int<lower = 1, upper = NDepartements> id_Obs_departements[NObs];
  int<lower = 1, upper = NElections> id_Obs_elections[NObs];
  int y_result[NObs];
  int n_result[NObs];
}
parameters {
  real alpha;
  real gamma;
  vector[M] beta_departments;
  matrix[max(NMiss_X), M] XMiss;

  // National predictors
  vector[2] beta_national[NDepartements];
  cholesky_factor_corr[2] omega_corr;
  vector[2] mu_beta_national;
  vector[2] sigma_beta_national;

  // Approval
  matrix[NElections, NTime] raw_theta;
  vector<lower = 0>[NElections] sigma_theta;
  cholesky_factor_corr[NElections] corr_Theta;
  vector[NElections] mu_theta;
  vector<lower = 0>[NPollsters] sigma_pollster;
  real<lower = 0> sigma_mu_pollster;
  vector[NPollsters_Presidents] raw_mu_pollster_president;
  vector[NPolls] raw_tau;
}
transformed parameters {
  // Approval
  //cholesky_factor_cov[NElections] cov_Theta = diag_pre_multiply(sigma_theta, corr_Theta);
  matrix[NElections, NTime] theta;
  vector[NPollsters_Presidents] mu_pollster_president;
  vector[NPolls] tau;
  matrix[NElections, 2] Z;

  vector[NObs] mu_departements;
  matrix[NObs, M] XAll;
  for (j in 1:M){
    XAll[, 1] = log(XDepartements[, 1]);
    XAll[id_X_miss[1:NMiss_X[j], j], j] = log(20 + XMiss[1:NMiss_X[j], j] * 4);
  }

  // Approval random walk
  theta[, 1] = mu_theta + sigma_theta .* raw_theta[, 1];
  for (tt in 2:NTime){
    theta[, tt] = theta[, tt - 1] + sigma_theta .* raw_theta[, tt];
  }
  mu_pollster_president = raw_mu_pollster_president * sigma_mu_pollster;
  tau = raw_tau .* sigma_pollster[id_Polls_pollster];

  // National predictors
  Z = append_col(rep_vector(1.0, NElections),
                 log(inv_logit(theta[,40])));
  for (j in 1:NObs){
    mu_departements[j] = alpha dot_product(beta_national[id_Obs_departements[j]], Z[id_Obs_elections[j]]) +
      XAll[j] * beta_departments;
  }
}
model {
  vector[NTime * NElections] theta_vector;
  for (j in 1:NElections){
    theta_vector[(1 + (j - 1) * NTime):(NTime * j)] = theta[j]';
  }
  to_vector(raw_theta) ~ std_normal();
  corr_Theta ~ lkj_corr_cholesky(10);
  mu_theta ~ normal(0, 2);
  sigma_theta ~ normal(0, 0.05);
  sigma_pollster ~ normal(0, 0.01);
  sigma_mu_pollster ~ normal(0, 0.05);
  raw_mu_pollster_president ~ std_normal();
  raw_tau ~ std_normal();
  gamma ~ normal(0, 5);

  // National predictors
  mu_beta_national ~ normal(0, 1);
  sigma_beta_national ~ normal(0, 1);
  omega_corr ~ lkj_corr_cholesky(10);
  beta_national ~ multi_normal_cholesky(mu_beta_national, diag_pre_multiply(sigma_beta_national, omega_corr));

  // Department predictors
  to_vector(XMiss) ~ normal(0, 1);
  beta_departments ~ normal(0, 0.1);

  // Election results
  y_result ~ binomial_logit(n_result, mu_departements);

  y_approval ~ binomial_logit(n_approval,
    theta_vector[id_Polls_time] +
    mu_pollster_president[id_Polls_pollster_president] +
    tau);
}

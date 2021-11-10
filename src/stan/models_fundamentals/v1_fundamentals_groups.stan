data {
  int NElections;
  int NDepartements;
  int NBlocs;
  int<upper = NElections * NDepartements> NObs;

  // Approval
  int NPolls;
  int NPollsters;
  int NPresidents;
  int NPollsters_Presidents[NPresidents]; // N of pollsters by president
  int NPolls_Presidents[NPresidents]; // N of polls by president
  int NTime;
  int id_Polls_time[NPolls];
  int<lower = 1, upper = NPresidents> id_Polls_president[NPolls];
  int<lower = 1, upper = NPollsters> id_Polls_pollster[NPolls];
  int<lower = 1, upper = sum(NPollsters_Presidents)> id_Polls_pollster_president[NPolls];
  int y_approval[NPolls];
  int n_approval[NPolls];
  real beta_prior;

  int M;
  matrix[NObs, M] XDepartements;
  int NMiss_X[M];
  int id_X_miss[max(NMiss_X), M];

  int<lower = 1, upper = NDepartements> id_Obs_departements[NObs];
  int<lower = 1, upper = NElections> id_Obs_elections[NObs];
  matrix[NObs, NBlocs] y_result;
  matrix[NObs, NBlocs] lag_y_result;
}
transformed data {
  int supvec_NPollsters_Presidents[NPresidents];
  int supvec_NPolls_Presidents[NPresidents];
  supvec_NPollsters_Presidents[1] = 0;
  supvec_NPolls_Presidents[1] = 0;
  for (jj in 2:NPresidents){
    supvec_NPollsters_Presidents[jj] = sum(NPollsters_Presidents[1:jj - 1]);
    supvec_NPolls_Presidents[jj] = sum(NPolls_Presidents[1:jj - 1]);
  }
}
parameters {
  real<lower = 0> sigma;
  real alpha;
  real gamma;
  vector[M] beta_departments;
  matrix[max(NMiss_X), M] XMiss;

  // National predictors
  real beta_national1;
  real<lower = 0> beta_national2;

  // Approval
  matrix[NElections, NTime] raw_theta;
  vector<lower = 0>[NElections] sigma_theta;
  cholesky_factor_corr[NElections] corr_Theta;
  vector[NElections] mu_theta;
  vector<lower = 0>[NPollsters] sigma_pollster;
  real<lower = 0> sigma_mu_pollster;
  vector[sum(NPollsters_Presidents)] raw_mu_pollster_president;
  vector[NPolls] raw_tau;
}
transformed parameters {
  // Approval
  //cholesky_factor_cov[NElections] cov_Theta = diag_pre_multiply(sigma_theta, corr_Theta);
  matrix[NElections, NTime] theta;
  vector[sum(NPollsters_Presidents)] mu_pollster_president;
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
  // Sum to zero constraints mu_pollster_president
  // Pollsters are ordered by president
  for (jj in 1:NPresidents){
    mu_pollster_president[supvec_NPollsters_Presidents[jj] + 1] =
      - sum(mu_pollster_president[(supvec_NPollsters_Presidents[jj] + 2):(supvec_NPollsters_Presidents[jj] + NPollsters_Presidents[jj])]);
    tau[supvec_NPolls_Presidents[jj] + 1] =
      - sum(tau[(supvec_NPolls_Presidents[jj] + 2):(supvec_NPolls_Presidents[jj] + NPolls_Presidents[jj])]);

  }

  // National predictors
  mu_departements = beta_national1 * logit(lag_y_result)  +
    beta_national2 * inv_logit(theta[,40])[id_Obs_elections] +
    XAll * beta_departments;
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

  // National predictors
  beta_national1 ~ normal(0, 1);
  beta_national2 ~ normal(0, 1);

  // Department predictors
  to_vector(XMiss) ~ normal(0, 1);
  beta_departments ~ normal(0, 0.1);

  sigma ~ normal(0, 0.1);

  // Election results

  logit(y_result) ~ multi_student_t(NElections,
                                    )
  student_t(NElections, mu_departements, sigma);

  y_approval ~ binomial_logit(n_approval,
    theta_vector[id_Polls_time] +
    mu_pollster_president[id_Polls_pollster_president] +
    tau);
}
generated quantities {
  vector[NObs] hat_y_result;
  hat_y_result = inv_logit(to_vector(student_t_rng(NElections,mu_departements, sigma)));
}








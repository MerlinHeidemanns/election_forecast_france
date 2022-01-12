functions {
  real induced_dirichlet_lpdf(vector c, vector alpha, real phi) {
    int K = num_elements(c) + 1;
    vector[K - 1] sigma = inv_logit(phi - c);
    vector[K] p;
    matrix[K, K] J = rep_matrix(0, K, K);

    // Induced ordinal probabilities
    p[1] = 1 - sigma[1];
    for (k in 2:(K - 1))
      p[k] = sigma[k - 1] - sigma[k];
    p[K] = sigma[K - 1];

    // Baseline column of Jacobian
    for (k in 1:K) J[k, 1] = 1;

    // Diagonal entries of Jacobian
    for (k in 2:K) {
      real rho = sigma[k - 1] * (1 - sigma[k - 1]);
      J[k, k] = - rho;
      J[k - 1, k] = rho;
    }

    return   dirichlet_lpdf(p | alpha)
           + log_determinant(J);
  }

  vector induced_dirichlet_rng(vector alpha, real phi) {
    int K = size(alpha);
    vector[K - 1] c;
    vector[K] p = dirichlet_rng(alpha);

    c[1] = phi - logit(1 - p[1]);
    for (k in 2:(K - 1))
      c[k] = phi - logit(inv_logit(phi - c[k - 1]) - p[k]);

    return c;
  }

  vector ordered_logit(real mu, vector thres){
    int K = size(thres);
    vector[K + 1] pi_i;
    pi_i[1] = 1 - inv_logit(mu - thres[1]);
    for (k in 2:K){
      pi_i[k] = inv_logit(mu - thres[k - 1]) - inv_logit(mu - thres[k]);
    }
    pi_i[K + 1] = inv_logit(mu - thres[K]);
    return(pi_i);
  }

  vector ordered_logit_discrimination(real mu, vector thres, real disc){
    int K = size(thres);
    vector[K + 1] pi_i;
    pi_i[1] = 1 - inv_logit(disc * (mu - thres[1]));
    for (k in 2:K){
      pi_i[k] = inv_logit(disc * (mu - thres[k - 1])) - inv_logit(disc * (mu - thres[k]));
    }
    pi_i[K + 1] = inv_logit(disc * (mu - thres[K]));
    return(pi_i);
  }
}
data {
  int NObs;
  int NBlocs;
  int NElections;
  int NDepartments;
  int id_Obs_elections[NObs];
  int id_Obs_departments[NObs];
  matrix[NObs, NBlocs] YVoteshare;
  matrix[NElections, NBlocs] lag_YVoteshare_national;

  // Bloc participation
  int NBlocs_Elections[NElections];
  array[NElections, NBlocs] int included_blocs;
  int NParticipated;
  int participated[NParticipated];

  // National level predictors
  int K;
  int incumbency[NElections];
  // -- Approval
  int NPolls;
  int NPollsters;
  int NPresidents;
  int NPollsters_Presidents[NPresidents];
  int NPolls_Presidents[NPresidents];
  int NTime;
  int id_Polls_time[NPolls];
  int<lower = 1, upper = NPresidents> id_Polls_president[NPolls];
  int<lower = 1, upper = NPollsters> id_Polls_pollster[NPolls];
  int<lower = 1, upper = sum(NPollsters_Presidents)> id_Polls_pollster_president[NPolls];
  int y_approval[NPolls];
  int n_approval[NPolls];
  // Department level predictors
  int M;
  matrix[NObs, M] XDepartment;
  int NMiss_X;
  int id_X_miss[NMiss_X];


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
  ordered[NBlocs - 1] c[NElections];
  real<lower = 0.0001> sigma_y;

  // Department level intercepts
  real<lower = 0.0001> sigma_alpha;
  vector[NDepartments] raw_alpha;
  // Department level predictors
  vector[NMiss_X] XMiss;
  // National coefficients
  ordered[3] beta1;
  real beta2;
  // National predictors
  matrix[NElections, NTime] raw_XApproval;
  vector<lower = 0>[NElections] sigma_XApproval;
  vector[NElections] mu_XApproval;
  vector<lower = 0>[NPollsters] sigma_pollster;
  real<lower = 0> sigma_mu_pollster;
  vector[sum(NPollsters_Presidents)] raw_mu_pollster_president;
  vector[NPolls] raw_tau;

  real zeta;
  // Department level coefficients
  vector[M] gamma;

  real disc;
}
transformed parameters {
  matrix[NObs, NBlocs] theta = rep_matrix(0.0, NObs, NBlocs);
  vector[NDepartments] alpha = raw_alpha * sigma_alpha;
  vector[NObs] y_star;
  matrix[NObs, M] XDepartment_miss;
  matrix[NElections, NTime] XApproval;
  vector[sum(NPollsters_Presidents)] mu_pollster_president;
  vector[NPolls] tau;
  vector[NElections] psi;
  // Department level predictors
  XDepartment_miss[, 1] = XDepartment[, 1];
  XDepartment_miss[id_X_miss, 1] = XMiss;
  XDepartment_miss[,2:M] = XDepartment[, 2:M];
  // National level predictors
  // Approval
  // -- Random walk
  for (jj in 1:NElections){
    XApproval[jj] = mu_XApproval[jj] + cumulative_sum(raw_XApproval[jj] * sigma_XApproval[jj]);
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
  // Add
  // Latent outcome
  psi = beta1[incumbency] + beta2 * inv_logit(XApproval[, 40]);
  y_star = alpha[id_Obs_departments] +
    psi[id_Obs_elections];
  for (j in 1:NObs){
    {
      theta[j,
            included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]] =
        ordered_logit_discrimination(y_star[j],
                      c[id_Obs_elections[j], 1:NBlocs_Elections[id_Obs_elections[j]] - 1],
                      exp(disc))';
    }
  }
}
// walk on latent preferences over the blocs
model {
  vector[NTime * NElections] XApproval_vector;
  // National predictors
  // Approval
  for (j in 1:NElections){
    XApproval_vector[(1 + (j - 1) * NTime):(NTime * j)] = XApproval[j]';
  }
  to_vector(raw_XApproval) ~ std_normal();
  mu_XApproval ~ normal(0, 2);
  sigma_XApproval ~ normal(0, 0.05);
  sigma_pollster ~ normal(0, 0.01);
  sigma_mu_pollster ~ normal(0, 0.05);
  raw_mu_pollster_president ~ std_normal();
  raw_tau ~ std_normal();
  // Likelihood
  y_approval ~ binomial_logit(n_approval,
    XApproval_vector[id_Polls_time] +
    mu_pollster_president[id_Polls_pollster_president] +
    tau);
  // National coefficients
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  // Department predictors
  XMiss ~ normal(0, 1);
  // National trend
  zeta ~ normal(0, 3);
  sigma_y ~ normal(0, 0.1);
  gamma ~ normal(0, 1);

  raw_alpha ~ std_normal();
  sigma_alpha ~ normal(0, 1);

  disc ~ normal(0, 1);

  for (j in 1:NElections){
    c[j, 1:(NBlocs_Elections[j] - 1)] ~ induced_dirichlet(
      lag_YVoteshare_national[j, included_blocs[j, 1:NBlocs_Elections[j]]]' * 2,0.0);
  }
  to_vector(YVoteshare)[participated] ~ normal(
    to_vector(theta)[participated],
    sigma_y);

}
generated quantities {
  matrix[NObs, NBlocs] y_ppc;
  matrix[NObs, NBlocs] y_pred = rep_matrix(0, NObs, NBlocs);
  matrix[NObs, NBlocs] error_y_ppc;
  matrix[NObs, NBlocs] error_pred;
  real bias_error_y_ppc;
  real bias_error_pred;
  vector[NBlocs] bias_error_pred_bloc;
  vector[NBlocs] bias_error_y_ppc_bloc;
  real variance_error_y_ppc;
  real variance_error_pred;
  vector[NBlocs - 1] c_rep[NElections];
  for (j in 1:NElections){
    c_rep[j, 1:(NBlocs_Elections[j] - 1)] = induced_dirichlet_rng(
      lag_YVoteshare_national[j, included_blocs[j, 1:NBlocs_Elections[j]]]' * 10,0.0);
  }
  y_ppc = theta;
  for (j in 1:NObs){
    y_pred[j,
            included_blocs[id_Obs_elections[j],1:NBlocs_Elections[id_Obs_elections[j]]]] =
        ordered_logit_discrimination(y_star[j],
                      c_rep[id_Obs_elections[j], 1:NBlocs_Elections[id_Obs_elections[j]] - 1],
                      exp(disc))';
  }
  error_y_ppc = YVoteshare - theta;
  error_pred = YVoteshare - y_pred;
  bias_error_y_ppc = mean(fabs(error_y_ppc));
  bias_error_pred = mean(fabs(error_pred));
  variance_error_y_ppc = sd(error_y_ppc);
  variance_error_pred = sd(error_pred);
  for (j in 1:NBlocs){
    bias_error_pred_bloc[j] = mean(fabs(error_pred[,j]));
    bias_error_y_ppc_bloc[j] = mean(fabs(error_y_ppc[,j]));
  }
}
functions {
  real unnormalize_normal_density(real x, real mu, real sigma){
    return(1/sigma * e()^(-0.5 * (x - mu)^2/sigma));
  }
  vector normal_density_prob(real x, vector mu, real[ ] sigma){
    int len_prob = size(mu);
    vector[len_prob] dens;
    for (j in 1:len_prob) dens[j] = unnormalize_normal_density(x, mu[j], sigma[j]);
    return(dens/sum(dens));
  }
}
data {
  int NObs;
  int NBlocs;
  int NElections;
  int NDepartments;
  int NBlocs_Elections[NElections];
  array[NElections, NBlocs] int included_blocs;
  int NParticipated;
  int participated[NParticipated];
  array[NObs] int id_Obs_elections;
  array[NObs] int id_Obs_departments;

  int K;
  matrix[NElections, K] XNation;
  int incumbency[NElections];

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

  int M;
  matrix[NObs, M] XDepartment;
  int NMiss_X;
  int id_X_miss[NMiss_X];
  matrix[NObs, NBlocs] YVoteshare;
}
transformed data {
  vector[NBlocs - 1] YVoteshare_logodds[NObs];
  int supvec_NPollsters_Presidents[NPresidents];
  int supvec_NPolls_Presidents[NPresidents];

  for (j in 1:NObs){
    YVoteshare_logodds[j] = log(YVoteshare[j, 1:NBlocs - 1]/YVoteshare[j, NBlocs])';
  }
  supvec_NPollsters_Presidents[1] = 0;
  supvec_NPolls_Presidents[1] = 0;
  for (jj in 2:NPresidents){
    supvec_NPollsters_Presidents[jj] = sum(NPollsters_Presidents[1:jj - 1]);
    supvec_NPolls_Presidents[jj] = sum(NPolls_Presidents[1:jj - 1]);
  }
}
parameters {
  vector[M] b_department;
  matrix[2, K + 1] b_national;
  vector[NMiss_X] XMiss;
  real<lower = 0> sigma_bloc;
  real<lower = 0> sigma_alpha_department;
  vector[NObs] raw_alpha_department;

  ordered[NBlocs] mu_parties;
  real<lower = 0> sigma_parties[NBlocs];


  //real<lower = 0> sigma_national_swing;
  //vector[NElections] raw_national_swing;

  // Approval
  // matrix[NElections, NTime] raw_XApproval;
  // vector<lower = 0>[NElections] sigma_XApproval;
  // vector[NElections] mu_XApproval;
  // vector<lower = 0>[NPollsters] sigma_pollster;
  // real<lower = 0> sigma_mu_pollster;
  // vector[sum(NPollsters_Presidents)] raw_mu_pollster_president;
  // vector[NPolls] raw_tau;
}
transformed parameters {
  vector[NObs] mu;
  matrix[NObs, NBlocs] theta = rep_matrix(0.0, NObs, NBlocs);

  vector[NObs] alpha_department = raw_alpha_department * sigma_alpha_department;
  //matrix[NElections, NBlocs] mu_parties;
  //vector[NBlocs] mu_parties = append_row(-2, -2 + raw_mu_parties);
  //vector[NElections] national_swing;
  // National predictors
  // --- Approval
  // matrix[NElections, NTime] XApproval;
  // vector[sum(NPollsters_Presidents)] mu_pollster_president;
  //vector[NPolls] tau;

  matrix[NElections, K + 1] XNation_;
  matrix[NObs, M] XDepartment_miss;
  XDepartment_miss[, 1] = XDepartment[, 1];
  XDepartment_miss[id_X_miss, 1] = XMiss;
  XDepartment_miss[,2:M] = XDepartment[, 2:M];

  //mu_parties[1] = raw_mu_parties';
  // for (j in 2:NElections){
  //   mu_parties[j] = mu_parties[1] + push[j - 1];
  // }

  // Approval random walk
  // for (jj in 1:NElections){
  //   XApproval[jj] = mu_XApproval[jj] + cumulative_sum(raw_XApproval[jj] * sigma_XApproval[jj]);
  // }
  // mu_pollster_president = raw_mu_pollster_president * sigma_mu_pollster;
  // tau = raw_tau .* sigma_pollster[id_Polls_pollster];
  // Sum to zero constraints mu_pollster_president
  // Pollsters are ordered by president
  // for (jj in 1:NPresidents){
  //   mu_pollster_president[supvec_NPollsters_Presidents[jj] + 1] =
  //     - sum(mu_pollster_president[(supvec_NPollsters_Presidents[jj] + 2):(supvec_NPollsters_Presidents[jj] + NPollsters_Presidents[jj])]);
  //   tau[supvec_NPolls_Presidents[jj] + 1] =
  //     - sum(tau[(supvec_NPolls_Presidents[jj] + 2):(supvec_NPolls_Presidents[jj] + NPolls_Presidents[jj])]);
  // }

  //national_swing = raw_national_swing * sigma_national_swing;

  XNation_[,1:K] = XNation;
  XNation_[,K + 1] = rep_vector(0.0, NElections);//inv_logit(XApproval[, 40]) * 100;

  mu = alpha_department[id_Obs_departments];// +
    // rows_dot_product(XNation_, b_national[incumbency,])[id_Obs_elections] +
    // XDepartment_miss * b_department;

  for (j in 1:NObs){
    theta[j, included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]] =
    normal_density_prob(
      mu[j],
      mu_parties[included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]],
      sigma_parties[included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]])';
  }
}
model {
  // vector[NTime * NElections] XApproval_vector;
  // for (j in 1:NElections){
  //   XApproval_vector[(1 + (j - 1) * NTime):(NTime * j)] = XApproval[j]';
  // }
  // to_vector(raw_XApproval) ~ std_normal();
  // mu_XApproval ~ normal(0, 2);
  // sigma_XApproval ~ normal(0, 0.05);
  // sigma_pollster ~ normal(0, 0.01);
  // sigma_mu_pollster ~ normal(0, 0.05);
  // raw_mu_pollster_president ~ std_normal();
  // raw_tau ~ std_normal();

  // Department intercept
  raw_alpha_department ~ std_normal();
  sigma_alpha_department ~ normal(0, 0.5);

  // bungee ~ normal(1, 0.1);
  // push ~ normal(0, 0.4);

  // National swing
  //sigma_national_swing ~ normal(0, 0.2);
  //raw_national_swing ~ std_normal();
  sigma_parties ~ normal(0, 2);
  //raw_mu_parties ~ normal(3, 2);
  mu_parties ~ normal(0, 2);

  b_department ~ normal(0, 1);
  to_vector(b_national) ~ std_normal();
  XMiss ~ normal(0, 1);
  sigma_bloc ~ normal(0, 0.05);
  to_vector(YVoteshare)[participated] ~ normal(to_vector(theta)[participated], sigma_bloc);

  // Approval
  // y_approval ~ binomial_logit(n_approval,
  //   XApproval_vector[id_Polls_time] +
  //   mu_pollster_president[id_Polls_pollster_president] +
  //   tau);
}
generated quantities {
  matrix[NObs, NBlocs] hat_YVoteshare;
  matrix[NObs, NBlocs] error;
  real rmse;
  for (j in 1:NObs){
    hat_YVoteshare[j] = theta[j];
  }
  error = YVoteshare - hat_YVoteshare;
  rmse = sqrt(sum(square(error)));
}







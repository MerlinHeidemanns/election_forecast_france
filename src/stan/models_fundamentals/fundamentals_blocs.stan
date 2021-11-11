data {
  int NObs;
  int NBlocs;
  int NElections;
  int NDepartments;
  array[NObs] int id_Obs_elections;
  array[NObs] int id_Obs_departments;

  int M;
  matrix[NObs, M] XDepartment;
  int NMiss_X[M];
  int id_X_miss[max(NMiss_X), M];

  matrix[NElections, NBlocs - 1] national_incumbency;
  matrix[NElections, NBlocs - 1] national_reelection;
  matrix[NObs, NBlocs] YVoteshare;
}
transformed data {
  vector[NBlocs - 1] YVoteshare_logodds[NObs];
  vector[NObs] NObs_ones;
  for (j in 1:NObs){
    YVoteshare_logodds[j] = log(YVoteshare[j, 1:NBlocs - 1]/YVoteshare[j, NBlocs])';
  }
  NObs_ones = rep_vector(1.0, NObs);
}
parameters {
  matrix[max(NMiss_X), M] XMiss;
  real<lower = 0> sigma_b;
  vector[2] raw_b;

  vector[M] mu_b_department;
  vector<lower = 0>[M] sigma_b_department;
  matrix[M, NBlocs - 1] raw_b_department;

  vector[NBlocs - 1] alpha;
  cholesky_factor_corr[NBlocs - 1] corr_bloc;
  vector<lower = 0>[NBlocs - 1] sigma_bloc;
}
transformed parameters {
  cholesky_factor_cov[NBlocs - 1] cov_bloc = diag_pre_multiply(sigma_bloc, corr_bloc);
  vector[2] b;
  matrix[M, NBlocs - 1] b_department;
  vector[NBlocs - 1] mu_national[NObs];

  matrix[NObs, M] XDepartment_miss;
  for (j in 1:M){
    XDepartment_miss[, 1] = log(XDepartment[, 1]);
    XDepartment_miss[id_X_miss[1:NMiss_X[j], j], j] = log(20 + XMiss[1:NMiss_X[j], j] * 4);
  }

  for (m in 1:M) b_department[m] = mu_b_department[m] + raw_b_department[m] * sigma_b_department[m];
  b = raw_b * sigma_b;

  for (j in 1:NObs){
    mu_national[j] = to_vector(
      alpha + b[1] * national_incumbency[id_Obs_elections[j]]' +
      b[2] * national_reelection[id_Obs_elections[j]]' +
      (XDepartment_miss[j] * b_department)'
    );
  }
}
model {
  mu_b_department ~ normal(0, 1);
  sigma_b_department ~ normal(0, 1);
  to_vector(raw_b_department) ~ std_normal();

  to_vector(XMiss) ~ normal(0, 1);

  alpha ~ normal(0, 1);
  sigma_b ~ normal(0, 0.5);
  raw_b ~ std_normal();
  corr_bloc ~ lkj_corr_cholesky(10.0);
  sigma_bloc ~ normal(0, 1);
  YVoteshare_logodds ~ multi_normal_cholesky(mu_national, cov_bloc);
}
generated quantities {
  matrix[NObs, NBlocs] hat_YVoteshare;
  matrix[NObs, NBlocs] error;
  real rmse;
  for (j in 1:NObs){
    hat_YVoteshare[j] = softmax(append_row(
      multi_normal_cholesky_rng(mu_national[j], cov_bloc),
      0))';
  }
  error = YVoteshare - hat_YVoteshare;
  rmse = sqrt(sum(square(error)));
}







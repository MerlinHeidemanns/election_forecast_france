data {
  int NObs;
  int NBlocs;
  int NElections;
  int NDepartments;
  array[NObs] int id_Obs_elections;
  array[NObs] int id_Obs_departments;

  matrix[NObs, NBlocs] YVoteshare;
}
transformed data {
  vector[NBlocs - 1] YVoteshare_logodds[NObs];
  for (j in 1:NObs){
    YVoteshare_logodds[j] = log(YVoteshare[j, 1:NBlocs - 1]/YVoteshare[j, NBlocs])';
  }
}
parameters {
  simplex[NBlocs] mu_prob;
  cholesky_factor_corr[NBlocs - 1] corr_bloc;
  vector<lower = 0>[NBlocs - 1] sigma_bloc;
}
transformed parameters {
  cholesky_factor_cov[NBlocs - 1] cov_bloc = diag_pre_multiply(sigma_bloc, corr_bloc);
  vector[NBlocs - 1] mu_logodds = mu_prob[1:NBlocs - 1]/mu_prob[NBlocs];
}
model {
  mu_prob ~ dirichlet(rep_vector(2, NBlocs));
  corr_bloc ~ lkj_corr_cholesky(10.0);
  sigma_bloc ~ normal(0, 1);
  YVoteshare_logodds ~ multi_normal_cholesky(mu_logodds, cov_bloc);
}
generated quantities {
  matrix[NObs, NBlocs] hat_YVoteshare;
  matrix[NObs, NBlocs] error;
  real rmse;
  for (j in 1:NObs){
    hat_YVoteshare[j] = softmax(append_row(
      multi_normal_cholesky_rng(mu_logodds, cov_bloc),
      0))';
  }
  error = YVoteshare - hat_YVoteshare;
  rmse = sqrt(sum(square(error)));
}







functions {
  vector compute_theta(real mu, vector thres){
    int len_theta = size(thres);
    vector[len_theta] out_theta;
    out_theta[1] = (2.0 * atan(mu - thres[len_theta]) + pi())/(2 * pi()) +
        (1 - (2.0 * atan(mu - thres[1]) + pi())/(2 * pi()));
    for (k in 2:len_theta){
      out_theta[k] = ((2.0 * atan((mu - thres[k - 1])) -
        2.0 * atan(mu - thres[k])))/(2.0 * pi());
    }
    return(out_theta);
  }
}
data {
  int NObs;
  int NBlocs;
  int NElections;
  int NDepartments;
  array[NObs] int id_Obs_elections;
  array[NObs] int id_Obs_departments;

  int K;
  matrix[NElections, K] XNation;
  int incumbency[NElections];
  int M;
  matrix[NObs, M] XDepartment;
  int NMiss_X[M];
  int id_X_miss[max(NMiss_X), M];
  matrix[NObs, NBlocs] YVoteshare;
}
transformed data {
  vector[NBlocs - 1] YVoteshare_logodds[NObs];
  for (j in 1:NObs){
    YVoteshare_logodds[j] = log(YVoteshare[j, 1:NBlocs - 1]/YVoteshare[j, NBlocs])';
  }
}
parameters {
  ordered[NBlocs] thresholds;
  vector[M] b_department;
  vector[2] b_national;
  matrix[max(NMiss_X), M] XMiss;
  cholesky_factor_corr[NBlocs - 1] corr_bloc;
  vector<lower = 0>[NBlocs - 1] sigma_bloc;
}
transformed parameters {
  cholesky_factor_cov[NBlocs - 1] cov_bloc = diag_pre_multiply(sigma_bloc, corr_bloc);
  vector[NObs] mu;
  vector[NBlocs - 1] theta[NObs];

  matrix[NObs, M] XDepartment_miss;
  for (j in 1:M){
    XDepartment_miss[, 1] = log(XDepartment[, 1]);
    XDepartment_miss[id_X_miss[1:NMiss_X[j], j], j] = log(20 + XMiss[1:NMiss_X[j], j] * 4);
  }


  mu = XDepartment_miss * b_department;
  for (j in 1:NObs){
    {
      vector[NBlocs] raw_theta = compute_theta(mu[j], thresholds);
      theta[j] = log(raw_theta[1:NBlocs - 1]/raw_theta[NBlocs]);
    }
  }
}
model {
  thresholds ~ normal(0, 2.5);
  b_department ~ std_normal();
  b_national ~ std_normal();
  to_vector(XMiss) ~ normal(0, 1);
  corr_bloc ~ lkj_corr_cholesky(10.0);
  sigma_bloc ~ normal(0, 1);
  YVoteshare_logodds ~ multi_normal_cholesky(theta, cov_bloc);
}
generated quantities {
  matrix[NObs, NBlocs] hat_YVoteshare;
  matrix[NObs, NBlocs] error;
  real rmse;
  for (j in 1:NObs){
    hat_YVoteshare[j] = softmax(append_row(
      multi_normal_cholesky_rng(theta[j], cov_bloc),
      0))';
  }
  error = YVoteshare - hat_YVoteshare;
  rmse = sqrt(sum(square(error)));
}







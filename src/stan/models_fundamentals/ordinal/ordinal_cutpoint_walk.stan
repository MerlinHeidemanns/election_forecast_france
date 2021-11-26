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


  vector ordered_probit(real mu, vector thres){
    int len_theta = size(thres);
    vector[len_theta + 1] out_theta;
    out_theta[1] = Phi(thres[1] - mu);
    for (k in 2:len_theta){
      out_theta[k] = Phi(thres[k] - mu) - Phi(thres[k - 1] - mu);
    }
    out_theta[len_theta + 1] = 1- Phi(thres[len_theta] - mu);
    return(out_theta);
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

}
data {
  int NObs;
  int NBlocs;
  int NElections;
  int id_Obs_elections[NObs];
  int id_Obs_departments[NObs];
  matrix[NObs, NBlocs] YVoteshare;
  matrix[NBlocs,NElections] lag_YVoteshare_national;

  // Bloc participation
  int NBlocs_Elections[NElections];
  array[NElections, NBlocs] int included_blocs;
  int NParticipated;
  int participated[NParticipated];

  // National level predictors
  int K;
  matrix[NElections, K] XNation;
  int incumbency[NElections];

  // Department level predictors
  int M;
  matrix[NObs, M] XDepartment;
  int NMiss_X;
  int id_X_miss[NMiss_X];

}
parameters {
  ordered[NBlocs - 1] c[NElections];
  vector[NElections] raw_gamma;
  real<lower = 0.0001> sigma_gamma;
  real<lower = 0.0001> sigma_y;
  real<lower = 0> phi;

}
transformed parameters {
  matrix[NObs, NBlocs] theta;
  vector[NElections] gamma = raw_gamma * sigma_gamma;
  vector[NObs] y_star = gamma[id_Obs_elections];



  for (j in 1:NObs){
    {
      theta[j,
            included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]] =
        ordered_logit(y_star[id_Obs_elections[j]],
                      c[id_Obs_elections[j],
                        included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]] - 1]])';
    }
  }
}
// walk on latent preferences over the blocs
model {
  sigma_y ~ normal(0, 0.1);
  sigma_gamma ~ normal(0, 1);
  raw_gamma ~ normal(0, 1);
  phi ~ normal(10, 1);
  for (j in 1:NElections){
     c[j] ~ induced_dirichlet(
       phi * lag_YVoteshare_national[,j]/sum(lag_YVoteshare_national[,j]), 0);
  }
  to_vector(YVoteshare)[participated] ~ normal(
    to_vector(theta)[participated],
    sigma_y);
  print(to_vector(YVoteshare)[participated]);

}
generated quantities {
  matrix[NObs, NBlocs] y_ppc;
  matrix[NObs, NBlocs] y_pred = rep_matrix(0.0, NObs, NBlocs);
  matrix[NObs, NBlocs] error_pred;
  real bias_error_pred;
  real variance_error_pred;
  y_ppc = theta;
  {
    vector[NBlocs - 1] c_rep[NElections];
    vector[NElections] gamma_rep;
    for (j in 1:NElections){
     c_rep[j] = induced_dirichlet_rng(
       phi * lag_YVoteshare_national[,j]/sum(lag_YVoteshare_national[,j]), 0);
    }
    gamma_rep = to_vector(normal_rng(rep_vector(0, NElections), sigma_gamma));
    for (j in 1:NObs){
      y_pred[j,
            included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]]] =
        ordered_logit(gamma_rep[id_Obs_elections[j]],
                      c_rep[id_Obs_elections[j],
                        included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]] - 1]])';
    }
  }
  error_pred = YVoteshare - y_pred;
  bias_error_pred = mean(fabs(error_pred));
  variance_error_pred = sd(error_pred);
}
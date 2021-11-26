functions {
  real normal_density(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    real[] theta,    // parameters
                    real[] x_r,      // data (real)
                    int[] x_i) {     // data (integer)
  real mu = theta[1];
  real sigma = theta[2];
  return 1 / (sqrt(2 * pi()) * sigma) * exp(-0.5 * ((x - mu) / sigma)^2);
  }
  vector utility(real mu_department, vector mu_bloc, vector prev_vote, real lambda){
    int NBlocs = size(mu_bloc);
    vector[NBlocs] u;
    for (j in 1:NBlocs){
      u[j] = log(prev_vote[j]) * (2 * (1 - lambda) * mu_department * mu_bloc[j] -
        lambda * sqrt(square(mu_department - mu_bloc[j])));
    }
    return(u);
  }
  vector compute_prob(vector u){
    int S = size(u);
    vector[S] prob;
    for (s in 1:S) prob
  }
}

data {
  int<lower=0> N;
  int NElections;
  int id_Obs_elections[N];
  int id_Obs_department[N];
  int NDepartments;
  int K;
  matrix[N, K] x;
  int P;
  matrix[N, P] y;
  matrix[NElections, P] y_lag;

  int NBlocs_Elections[NElections];
  array[NElections, P] int included_blocs;
  int NParticipated;
  int participated[NParticipated];

}
parameters {
  vector<upper = 0>[P - 3] mu_party_left;
  vector<lower = 0>[P - 3] mu_party_right;


  vector[NElections] raw_mu_nation;
  real<lower = 0> sigma_mu_nation;
  real<lower = 0.0001> sigma_y;
  real<lower = 0, upper = 1> lambda;
  vector[NDepartments] raw_mu_department;
  real<lower = 0> sigma_mu_department;
}
transformed parameters {
  vector[NElections] mu_nation;
  vector[NDepartments] mu_department;
  vector[P] mu_party = append_row(mu_party_left, mu_party_right);
  mu_nation[1] = raw_mu_nation[1] * sigma_mu_nation;
  for (j in 2:NElections){
    mu_nation[j] = mu_nation[j - 1] + raw_mu_nation[j] * sigma_mu_nation;
  }
  mu_department = raw_mu_department * sigma_mu_department;
}
model {
  matrix[N, P] y_hat;
  mu_party_left ~ normal(0, 1);
  mu_party_right ~ normal(0, 1);
  raw_mu_nation ~ normal(0, 1);
  sigma_mu_nation ~ normal(0, 0.5);
  sigma_y ~ normal(0, 0.01);
  lambda ~ beta(2, 2);
  raw_mu_department ~ std_normal();
  sigma_mu_department ~ normal(0, 0.1);
  for (j in 1:N){
    {
      int included[NBlocs_Elections[id_Obs_elections[j]]] = included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]];
      y_hat[j, included] = utility(
        mu_nation[id_Obs_elections[j]] + mu_department[id_Obs_department[j]],
        mu_party[included],
        y_lag[id_Obs_elections[j]]',
        lambda)';
    }
  }
  to_vector(y)[participated] ~ normal(to_vector(y_hat)[participated], sigma_y);
}
generated quantities {
  matrix[N, P] y_hat;
  for (j in 1:N){
    {
      int included[NBlocs_Elections[id_Obs_elections[j]]] = included_blocs[id_Obs_elections[j]][1:NBlocs_Elections[id_Obs_elections[j]]];
      y_hat[j, included] = utility(
        mu_nation[id_Obs_elections[j]],
        mu_party[included],
        y_lag[id_Obs_elections[j]]',
        lambda)';
    }
  }
}

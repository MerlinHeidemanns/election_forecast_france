functions {
  real normal_density(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    real[] theta,    // parameters
                    real[] x_r,      // data (real)
                    int[] x_i) {     // data (integer)
    int lenS = size(theta) - 1;
    real out;
    out = exp(std_normal_lpdf((x - theta[1])/theta[lenS + 1]));
    for (j in 2:lenS) out = out * Phi((x - theta[j])/theta[lenS + 1]);
  return out;
  }
  real[] utility(real mu_department, vector mu_bloc, real lambda){
    int NBlocs = size(mu_bloc);
    real u[NBlocs];
    for (j in 1:NBlocs){
      u[j] = (2 * (1 - lambda) * mu_department * mu_bloc[j] -
        lambda * sqrt(square(mu_department - mu_bloc[j])));
    }
    return(u);
  }
}
data {
  int N;
  int D;
  matrix[N, D] y;
}
transformed data {
  real x_r[0];
  int x_i[0];
  int slots[D, D];
  {
    int count;
    for (i in 1:D){
      count = i;
      for (j in 1:D){
        if (count > D) count = 1;
        slots[i, j] = count;
        count = count + 1;
      }
    }
  }
}
parameters {
  real mu;
  real<lower = 0, upper = 1> lambda;
  vector[D - 1] raw_positions;
  real<lower = 0.00001> sigma_y;
  vector[N] raw_tau;
  real<lower = 0> sigma_tau;
  real<lower = 0> sigma;
}
transformed parameters {
  vector[D] positions = append_row(raw_positions, -sum(raw_positions));
  matrix[N, D] prob;
  vector[N] tau = raw_tau * sigma_tau;
  real u[D] = utility(mu, positions, lambda);
  for (i in 1:N){
    for (j in 1:(D - 1)){
      prob[i, j] = integrate_1d(normal_density,
                          -2,
                          2,
                          {utility(mu, positions, lambda)[slots[, j]]},
                          x_r,
                          x_i,
                          1e-8);
    }
    prob[i, D] = 1 - sum(prob[i, 1:D - 1]);
  }
}
model {
  sigma ~ normal(0,1);
  raw_tau ~ normal(0, 1);
  sigma_tau ~ normal(0, 1);
  lambda ~ beta(2, 2);
  mu ~ normal(0, 1);
  raw_positions ~ normal(0, 3);
  sigma_y ~ normal(0, 0.01);
  //to_vector(y) ~ normal(to_vector(prob), sigma_y);
}




functions {
  real normal_density(real x,          // Function argument
                    real xc,         // Complement of function argument
                                     //  on the domain (defined later)
                    real[] theta,    // parameters
                    real[] x_r,      // data (real)
                    int[] x_i) {     // data (integer)
    int lenS = size(theta);
    real out;
    out = 1 / (sqrt(2 * pi())) * exp(-0.5 * ((x - theta[1])^2));
    for (j in 2:lenS) out = out * Phi(x - theta[j]);
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
  int D;
  vector[D] positions;
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
  real x;
  real<lower = 0, upper = 1> lambda;
}
transformed parameters {
  real prob[D];
  real u[D] = utility(0.0, positions, lambda);
  for (j in 1:D){
    prob[j] = integrate_1d(normal_density,
                        negative_infinity(),
                        positive_infinity(),
                        u[slots[,j]],
                        x_r,
                        x_i);
  }

}
model {
  lambda ~ beta(2, 2);
  x ~ normal(0, 1);
}




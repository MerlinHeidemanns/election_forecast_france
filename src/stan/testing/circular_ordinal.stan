data {
  int N;
  int C;
  vector[N] x;
  int y[N];
}
parameters {
  real mu;
  real beta;
  real<lower = 0> kappa;
  ordered[C - 1] c;
}
model {
  vector[C] theta;
  c ~ normal(2, 5);
  mu ~ normal(0, 3);
  beta ~ normal(0, 1);
  for (n in 1:N) {
    real eta = mu + beta * x[n];
    theta[1] = 1 - (2.0 * atan(eta - c[1]) + pi())/(2 * pi());
    for (k in 2:C - 1)
      theta[k] = ((2.0 * atan((eta - c[k - 1])) -
        2.0 * atan(eta - c[k])))/(2.0 * pi());
    theta[C] = (2.0 * atan(eta - c[C - 1]) + pi())/(2 * pi());
    y[n] ~ categorical(theta);
  }
}
generated quantities {
  vector[N] yhat;
  vector[N] eta;
  matrix[N, C] theta;
  for (n in 1:N) {
    eta[n] = mu + beta * x[n];
    theta[n,1] = 1 - (2.0 * atan(eta[n] - c[1]) + pi())/(2 * pi());
    for (k in 2:C - 1)
      theta[n,k] = ((2.0 * atan((eta[n] - c[k - 1])) -
        2.0 * atan(eta[n] - c[k])))/(2.0 * pi());
    theta[n,C] = (2.0 * atan(eta[n] - c[C - 1]) + pi())/(2 * pi());
    yhat[n] = categorical_rng(theta[n]');
  }
}
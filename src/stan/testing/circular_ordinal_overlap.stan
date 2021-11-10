data {
  int N;
  int N2;
  int C;
  vector[N] x;
  int y[N];
  vector[N2] x_new;
}
parameters {
  real mu;
  real beta;
  ordered[C - 1] c;
}
model {
  vector[C] theta;
  vector[C] rc;
  rc[1] = - 10;
  rc[2:C] = c;
  c ~ normal(2, 5);
  mu ~ normal(0, 3);
  beta ~ normal(0, 1);
  for (n in 1:N) {
    real eta = mu + beta * x[n];
    theta[1] = (2.0 * atan(eta - rc[C]) + pi())/(2 * pi()) +
        (1 - (2.0 * atan(eta - rc[1]) + pi())/(2 * pi()));
    for (k in 2:C)
      theta[k] = ((2.0 * atan((eta - rc[k - 1])) -
        2.0 * atan(eta - rc[k])))/(2.0 * pi());
    y[n] ~ categorical(theta);
  }
}
generated quantities {
  vector[N] yhat;
  vector[N2] yhat2;
  vector[N] eta;
  matrix[N, C] theta;
  vector[C] rc;
  rc[1] = - 10;
  rc[2:C] = c;
  for (n in 1:N) {
    eta[n] = mu + beta * x[n];
    theta[n,1] = (2.0 * atan(eta[n] - rc[C]) + pi())/(2 * pi()) +
        (1 - (2.0 * atan(eta[n] - rc[1]) + pi())/(2 * pi()));
    for (k in 2:C)
      theta[n,k] = ((2.0 * atan((eta[n] - rc[k - 1])) -
        2.0 * atan(eta[n] - rc[k])))/(2.0 * pi());
    yhat[n] = categorical_rng(theta[n]');
  }
  for (n in 1:N2) {
    {
      vector[C] theta_new;
      real eta_new = mu + beta * x_new[n];
      theta_new[1] = (2.0 * atan(eta_new - rc[C]) + pi())/(2 * pi()) +
          (1 - (2.0 * atan(eta_new - rc[1]) + pi())/(2 * pi()));
      for (k in 2:C)
        theta_new[k] = ((2.0 * atan((eta_new - rc[k - 1])) -
          2.0 * atan(eta_new - rc[k])))/(2.0 * pi());
      yhat2[n] = categorical_rng(theta_new);
    }
  }
}
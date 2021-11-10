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
  int N;
  int N2;
  int C;
  vector[N] x;
  array[N, C] int y;
}
parameters {
  real beta;
  ordered[C] c;
}
model {
  vector[C] theta;
  c ~ normal(0, 2.5);
  beta ~ normal(0, 10);
  for (n in 1:N) {
    y[n] ~ multinomial(compute_theta(beta * x[n], c));
  }
}
generated quantities {
  matrix[N, C] yhat;
  for (n in 1:N) {
    yhat[n] = to_row_vector(multinomial_rng(compute_theta(beta * x[n], c), sum(y[n])));
  }
}
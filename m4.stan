 
data {
  int<lower=1> N;
  vector[N] height;
  vector[N] weight;
}
parameters {
  real alpha;
  real beta;
  real<lower=0,upper=50> sigma; //implicitly defining
}
model {
  vector[N] mu = alpha + beta * weight;  
  target += normal_lpdf(height | mu, sigma);
  target += normal_lpdf(alpha | 155, 30);
  target += normal_lpdf(beta | 0, 10);
} 
generated quantities {
  vector[N] y_pred;
  for (n in 1:N){
    y_pred[n] = normal_rng( alpha + beta*weight[n], sigma );
  }
}


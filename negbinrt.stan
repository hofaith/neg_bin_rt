data {
  int T; //number of event time
  int S; //number of serial intervals
  int I[T];//observed counts at event time T
  vector[S] w;//serial interval for linked local case
  int tau;//time window
}

transformed data {
  real X[T];
  vector[S] Y;
  for (t in 1:T) {
    for (s in 1:S) {
      if ((t-s)>0)
      Y[s] = I[t-s] * w[s];
      else 
      Y[s] = 0;
}
    X[t] = sum(Y);
}
}

parameters {
  real<lower=0,upper=1> p[T];
  real<lower=0> r[T]; 
  real<lower=0> sigma;
}

model {
    sigma ~ gamma(0.001,0.001);
  for (t in 1:T){
    p[t] ~ uniform(0,1); 
    r[t] ~ gamma(0.001,0.001);
  }
  for (t in 7:T){
    for (s in (t-tau+1):t){
      if (X[s] > 0)
      I[s] ~ neg_binomial(X[s]*r[t],p[t]/(1-p[t])); 
}
(r[t]-r[t-1]) ~ normal(0,sigma);
}
}
generated quantities {
  real log_lik[T];
  real rw[T];
    for (t in 1:T){
    log_lik[t] = 0;
    rw[t] = 0;
    }
  for (t in 7:T){
    for (s in (t-tau+1):t){
      if (X[s] > 0)
     log_lik[t] = neg_binomial_lpmf(I[s]|X[s]*r[t],p[t]/(1-p[t])); 
}
   rw[t] = normal_lpdf(r[t]-r[t-1]|0,sigma);
}
}
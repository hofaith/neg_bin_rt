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
  real<lower=0> rt[T];
}

model {
  for (t in 1:T){
    rt[t] ~ gamma(1,0.2); //prior used in Cori et al.
  }
  for (t in 7:T){
    for (s in (t-tau+1):t){
      if (X[s] > 0)
      I[s] ~ poisson(rt[t]*X[s]);
}
}
}

generated quantities {
real log_lik[T];
    for (t in 1:T){
log_lik[t] = 0;
    }
    for (t in 7:T){
       for (s in (t-tau+1):t){
         if (X[s] > 0)
         log_lik[t] = poisson_lpmf(I[s]|rt[t]*X[s]);
}
}
}

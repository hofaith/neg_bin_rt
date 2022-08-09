library(tidyverse)

## p80 function
propresponsible <- function(R0, k, prop) {
  qm1 <- qnbinom(1-prop, k+1, mu = R0*(k+1)/k)
  remq <- 1-prop-pnbinom(qm1-1, k+1, mu = R0*(k+1)/k)
  remx <- remq/dnbinom(qm1, k+1, mu = R0*(k+1)/k)
  q <- qm1+1
  1-pnbinom(q-1, k, mu = R0)-dnbinom(q, k, mu = R0)*remx
}

p80.iter <- matrix(NA,nrow=10000,ncol=nrow(data4[141:407,]))
p80 <- p80.ci.l <- p80.ci.u <- NA

for (i in 1:10000) {
  for (y in 1:nrow(data4[141:407,])){
    p80.iter[i,y] = propresponsible(
      rt.iter[i,y], #rt.iter is generated from simulation_rt.R
      r.iter[i,y], #r.iter is generated from simulation_rt.R
      0.8
    )
  }
}

for (y in 1:nrow(data4[141:407,])){
  p80[y] <- mean(p80.iter[,y])
  p80.ci.l[y] <- quantile(p80.iter[,y],probs=0.025)
  p80.ci.u[y] <- quantile(p80.iter[,y],probs=0.975)
}

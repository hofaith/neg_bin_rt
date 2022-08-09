library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(cmdstanr)
library(loo)
library(rstansim)

##################Run simulation
pois.mod <- cmdstan_model("/rt.stan")
mod1 <- cmdstan_model("/negbinrt.stan")
cmdstan_path()

rt.data <- list(NA)
I <- X <- Y <- NA
w=c(0.000,0.233,0.359,0.198,0.103,0.053,0.027,0.014,0.007,0.003,0.002,0.001)
T=70
S=12
r=rep(2,70) #c(rep(10,18),rep(1,18),rep(0.1,17),rep(2,17)) #0.1, 0.2, 0.5, 1
p=c(rep(0.625,35),rep(0.625,35)) #c(rep(0.870,18),rep(0.4,18),rep(0.11,17),rep(0.714,17)) #0.45,0.0769(0.1), 0.14(0.2), 0.29(0.5)
set.seed(123)
for (t in 1:5){
  I[t] = 10;
}
for (i in 1:50){
  for (t in 6:T) {
    for (s in 1:S) {
      if ((t-s)>0) {
      Y[s] = I[t-s] * w[s]
    }
      else {
      Y[s] = 0
    }
  }
  X[t] = sum(Y)
  if (X[t]>0) {
    I[t] = rnbinom(1,X[t]*r[t],p[t])
  }
}
rt.data[[i]] <- list(T=70, I = I,w = c(0.000,0.233,0.359,0.198,0.103,0.053,0.027,0.014,0.007,0.003,0.002,0.001),tau = 7,S = 12)
}

for (i in 1:50){
assign(paste0("sim.pois.rt.fit",i),pois.mod$sample(data=rt.data[[i]],seed=123,chains=1,refresh=500)) 
assign(paste0("sim.pois.stanfit",i),rstan::read_stan_csv(get(paste0("sim.pois.rt.fit",i))$output_files()))
assign(paste0("sim.pois.rt.ci.l",i),summary(get(paste0("sim.pois.stanfit",i)), pars="rt",probs = c(0.025, 0.975))$summary[,4])
assign(paste0("sim.pois.rt.ci.u",i),summary(get(paste0("sim.pois.stanfit",i)), pars="rt",probs = c(0.025, 0.975))$summary[,5])
assign(paste0("sim.pois.rt",i),as.numeric(unlist(get(paste0("sim.pois.rt.fit",i))$summary("rt","mean")[,2])))

assign(paste0("sim.nb.rt.fit",i),mod1$sample(data=rt.data[[i]],seed=123,chains=1,refresh=500,iter_warmup=10000,iter_sampling=10000))
assign(paste0("sim.nb.stanfit",i),rstan::read_stan_csv(get(paste0("sim.nb.rt.fit",i))$output_files()))
assign(paste0("sim.r",i),as.numeric(unlist(get(paste0("sim.nb.rt.fit",i))$summary("r","mean")[,2])))
assign(paste0("sim.r.ci.l",i),summary(get(paste0("sim.nb.stanfit",i)), pars="r",probs = c(0.025, 0.975))$summary[,4])
assign(paste0("sim.r.ci.u",i),summary(get(paste0("sim.nb.stanfit",i)), pars="r",probs = c(0.025, 0.975))$summary[,5])
assign(paste0("sim.p",i),as.numeric(unlist(get(paste0("sim.nb.rt.fit",i))$summary("p","mean")[,2])))
assign(paste0("sim.rt",i),get(paste0("sim.r",i))*(1-get(paste0("sim.p",i)))/get(paste0("sim.p",i)))
assign(paste0("sim.rt.ci.l",i),summary(get(paste0("sim.nb.stanfit",i)), pars="r",probs = c(0.025, 0.975))$summary[,4]*(1-summary(get(paste0("sim.nb.stanfit",i)), pars="p",probs = c(0.025, 0.975))$summary[,4])/summary(get(paste0("sim.nb.stanfit",i)), pars="p",probs = c(0.025, 0.975))$summary[,4])
assign(paste0("sim.rt.ci.u",i),summary(get(paste0("sim.nb.stanfit",i)), pars="r",probs = c(0.025, 0.975))$summary[,5]*(1-summary(get(paste0("sim.nb.stanfit",i)), pars="p",probs = c(0.025, 0.975))$summary[,5])/summary(get(paste0("sim.nb.stanfit",i)), pars="p",probs = c(0.025, 0.975))$summary[,5])
}

sim.r.ciu <- mean(rowMeans(mapply(c,mget(paste0("sim.r.ci.u",1:50))))[10:70]);sim.r.ciu 
sim.r.cil <- mean(rowMeans(mapply(c,mget(paste0("sim.r.ci.l",1:50))))[10:70]);sim.r.cil

#WAIC; Likelihood ratio test
for (i in 1:50){
assign(paste0("sim_log_lik_nb",i),extract_log_lik(get(paste0("sim.nb.stanfit",i))))
assign(paste0("sim_log_lik_pois",i),extract_log_lik(get(paste0("sim.pois.stanfit",i)))) 
sim_waic_nb[i] <- waic(get(paste0("sim_log_lik_nb",i)))$estimate[3]
sim_waic_pois[i] <- waic(get(paste0("sim_log_lik_pois",i)))$estimate[3]
sim_waic_nb_mean <- mean(sim_waic_nb);sim_waic_pois_mean <- mean(sim_waic_pois)
sim_waic_diff <- sim_waic_pois_mean - sim_waic_nb_mean
mean_sim_log_lik_pois <- mean(rowSums(get(paste0("sim_log_lik_pois",i))))
mean_sim_log_lik_nb <- mean(rowSums(get(paste0("sim_log_lik_nb",i))))
p.sim[i] <- 1-pchisq(2*(mean(rowSums(get(paste0("sim_log_lik_nb",i))))-mean(rowSums(get(paste0("sim_log_lik_pois",i))))),df=64)
lrt.sim <- length(p.sim[p.sim<0.05])/50*100
}

#Proportion of rejection of k>1, k>0.5, k>p80=0.2
for (i in 1:50){
sim.prop.r[i] <- ifelse(sum(get(paste0("sim.r.ci.u",i))[10:70]<1)>=1,1,0) 
sim.prop.1[i] <- ifelse(sum(get(paste0("sim.r.ci.u",i))[10:70]<0.5)>=1,1,0) 
#sim.prop.2[i] <- ifelse(sum(get(paste0("sim.r.ci.u",i))[10:70]<0.335)>=1,1,0)
sim.prop.3[i] <- ifelse(sum(get(paste0("sim.r.ci.u",i))[10:35]<0.32)&sum(get(paste0("sim.r.ci.u",i))[36:70]<0.416)>=1,1,0)
sim.prop.r <- sum(sim.prop.r)/50*100; sim.prop.r.2 <- sum(sim.prop.2)/50*100; sim.prop.r.3 <- sum(sim.prop.3)/50*100
}


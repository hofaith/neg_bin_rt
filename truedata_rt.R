library(dplyr)
library(surveillance)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(cmdstanr)
library(lubridate)
library(loo)

##############################################
#############Fit incidence data to model
data <- read.csv("/hkcase_20210316.csv")
data$classification[1] <- "Local case"
data <- data[data$classification%in%c("Local case","Epidemiologically linked with local case"),]
data$onset.day <- as.Date(data$confirm.date,format="%d/%m/%Y") - as.Date(data$onset.date,format="%d/%m/%Y")
data$confirm.day <- as.Date(data$confirm.date,format="%d/%m/%Y") - as.Date("2020-01-18")

data3 <- data[!is.na(data$onset.day),] %>% group_by(confirm.date,.drop=FALSE) %>% summarise(count=n()) %>% arrange(as.Date(confirm.date,"%d/%m/%Y"))
data3$confirm.date <- as.Date(data3$confirm.date,"%d/%m/%Y")
data4<-merge(data.frame(confirm.date= as.Date(min(data3$confirm.date):max(data3$confirm.date),"1970-1-1")),
             data3, by = "confirm.date", all = TRUE)
data4[is.na(data4)] <- 0
data4[1,2] <- 0

ZZ <- sts(data4$count)
bpnp.control <- list(k=0,eps=rep(1,2),iter.max=rep(250,2),B=-1,verbose=TRUE)
set.seed(123)
temp3 <- backprojNP(ZZ,incu.pmf=c(0,dlnorm(1:14,log(5.4),(2*(log(6.3)-log(5.4)))^0.5)), #14,1.434065,0.661267)
                    control=modifyList(bpnp.control,list(eq3a.method="C")), ylim=c(0,max(X,Y)))

newI2 <- temp3@upperbound/sum(temp3@upperbound)*nrow(data)

rt.real.data <- list(T=nrow(data4[135:407,]),
                S=12,
                I=round(newI2[135:407]),
                w=c(0.000,0.233,0.359,0.198,0.103,0.053,0.027,0.014,0.007,0.003,0.002,0.001),
                tau=7)

#Poisson likelihood
pois.mod <- cmdstan_model("/poisrt.stan")
pois.rt.fit <- pois.mod$sample(data=rt.real.data,seed=123,chains=1,refresh=500) 
pois.stanfit <- rstan::read_stan_csv(pois.rt.fit2$output_files())
pois.rt.ci.l <- summary(pois.stanfit, pars="rt",probs = c(0.025, 0.975))$summary[,4]
pois.rt.ci.u <- summary(pois.stanfit, pars="rt",probs = c(0.025, 0.975))$summary[,5]

#Negative binomial likelihood
mod1 <- cmdstan_model("/negbinrt.stan")
rt.fit <- mod1$sample(data=rt.real.data,seed=123,chains=1,refresh=500,iter_warmup=10000,iter_sampling=10000)
stanfit <- rstan::read_stan_csv(rt.fit$output_files())
r.iter <- as.matrix(stanfit,"r")
rt.iter <- as.matrix(stanfit,"r")*(1-as.matrix(stanfit,"p"))/as.matrix(stanfit,"p")
rt.ci.l <- summary(stanfit, pars="r",probs = c(0.025, 0.975))$summary[,4]*(1-summary(stanfit, pars="p",probs = c(0.025, 0.975))$summary[,4])/summary(stanfit, pars="p",probs = c(0.025, 0.975))$summary[,4]
rt.ci.u <- summary(stanfit, pars="r",probs = c(0.025, 0.975))$summary[,5]*(1-summary(stanfit, pars="p",probs = c(0.025, 0.975))$summary[,5])/summary(stanfit, pars="p",probs = c(0.025, 0.975))$summary[,5]
r.ci.l <- summary(stanfit, pars="r",probs = c(0.025, 0.975))$summary[,4]
r.ci.u <- summary(stanfit, pars="r",probs = c(0.025, 0.975))$summary[,5]
r <- as.numeric(unlist(rt.fit$summary("r","mean")[,2]))
p <- as.numeric(unlist(rt.fit$summary("p","mean")[,2]))
rt <- r*(1-p)/p

log_lik_nb <- extract_log_lik(stanfit);log_lik_pois <- extract_log_lik(pois.stanfit) 
waic(log_lik_nb);waic(log_lik_pois)


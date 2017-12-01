# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

#setwd("C:/Users/u341138/Dropbox/Documents/Teaching/2017-2018/Bayesian statistics/Group exercises/Assignment03")

# Required packages for this exercise.
require(rjags)
require(coda)

### Helper functions

# Plotting of standard deviation interval
plot_sd <- function(meanvec, sdvec) {
  n = length(meanvec)
  # Compute the upper end of the interval, i.e. expectation+sigma
  upper = meanvec + sdvec
  # Compute the lower end of the interval, i.e. expectation-sigma
  lower = meanvec - sdvec
  # Plot the interval
  polygon(c(1:n, rev(1:n)), c(lower, rev(upper)), col=rgb(0.2, 0.2, 0.2, 0.1), border=NA)
  # Plot the upper and lower bounds of the interval as a dotted line
  lines(1:n, lower, lty=3, col=rgb(0.1, 0.1, 0.1))
  lines(1:n, upper, lty=3, col=rgb(0.1, 0.1, 0.1))
}

# Plotting of neuron spikes
plot_spikes <- function(s) {
  n = length(s)
  # Plot spikes
  for (i in 1:n) {
    if (s[i]>0) {
      abline(v=i, col=rgb(0.7, 0.2, 0.2, 0.5), lwd=3, lty=1)
    }
  }
}





load(file='spikes.Rdata')
T = length(s)

plot(m, type='l')
plot_spikes(s)


model_spikes_notemporaldependency = "
model {
    #Prior
    alpha ~ dbeta(10,10)
    nu ~ dgamma(1,2)
    fmax ~ dunif(0,1)
    theta ~ dnorm(2,2)
    
    #Likelihood
    for(t in 1:T){
        m[t] ~ dnorm(alpha, nu)
        p[t] <- fmax * (1/(1+exp(-(m[t]+theta))))
        s[t] ~ dbern(p[t])
    }
}
"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('T' = T, 's' = s) # to be passed on to JAGS
parameters <- c('m') # fill in
jagsmodel <- jags.model(textConnection(model_spikes_notemporaldependency), 
                        data = data, 
                        n.chains = nchains)
mcmcsamples = coda.samples(jagsmodel, parameters, n.iter = niter)
samples = as.matrix(mcmcsamples)

samples_mp = samples
mean_mp = apply(samples_mp, 2, mean)
  
# The Monte Carlo standard error gives an indication of how certain we are about the membrane potentials. Compute it using:
sd_mp = apply(samples_mp, 2, sd) # applies function sd to each column of samples_mp; gives a vector of the standard deviation for each membrane potential

# Plot the ground truth m, the posterior expectation of the potentials mean_mp, use plot_sd(mean_mp, sd_mp) to show the (un)certainty and finally use plot_spikes(s) to plot the actual observed spikes.
plot(m, type='l', col = 'blue')
lines(mean_mp, col = 'green')
plot_sd(mean_mp, sd_mp)
plot_spikes(s)

legend(x = 'topleft', 
       1.9, 
       c('Ground truth', 'Posterior expectation', 'One sd from the mean', 'Spikes'), 
       lty=c(1,1,3,1),
       col=c('blue', 'green', 'grey', 'red'))

# Compute the correlation between the actual and estimated membrane potentials:
cor(m, mean_mp)


#########

model_spikes_withtemporaldependency = "
model {
    #Prior
    alpha ~ dbeta(10,10)
    nu ~ dgamma(1,2)
    fmax ~ dunif(0,1)
    theta ~ dnorm(2,2)

    #Likelihood
    m[1] ~ dnorm(alpha, nu)
    p[1] <- fmax * (1/(1+ exp(-(m[1]+theta))))
    s[1] ~ dbern(p[1])

    for(t in 2:T){
        m[t] ~ dnorm(alpha*m[t-1], nu)
        p[t] <- fmax * (1/(1+ exp(-(m[t]+theta))))
        s[t] ~ dbern(p[t])
    }

}
"


niter=10000
nchains=4
nsamples = niter*nchains

data <- list('T' = T, 's' = s) # to be passed on to JAGS
parameters <- c('m') # fill in
jagsmodel <- jags.model(textConnection(model_spikes_withtemporaldependency), 
                        data = data, 
                        n.chains = nchains)
mcmcsamples = coda.samples(jagsmodel, parameters, n.iter = niter)
samples = as.matrix(mcmcsamples)

# The steps below are repetitions of the previous exercise. Copy-pasting should work if you implemented this correctly

samples_mp = samples
mean_mp = apply(samples_mp, 2, mean)

# The Monte Carlo standard error gives an indication of how certain we are about the membrane potentials. Compute it using:
sd_mp = apply(samples_mp, 2, sd) # applies function sd to each column of samples_mp; gives a vector of the standard deviation for each membrane potential

# Plot the ground truth m, the posterior expectation of the potentials mean_mp, use plot_sd(mean_mp, sd_mp) to show the (un)certainty and finally use plot_spikes(s) to plot the actual observed spikes.

plot(m, type='l', col = 'blue')
lines(mean_mp, col = 'green')
plot_sd(mean_mp, sd_mp)
plot_spikes(s)

legend(x = 'topleft', 
       1.9, 
       c('Ground truth', 'Posterior expectation', 'One sd from the mean', 'Spikes'), 
       lty=c(1,1,3,1),
       col=c('blue', 'green', 'grey', 'red'))

# Compute the correlation between the actual and estimated membrane potentials:
cor(m, mean_mp)



################### 

model_spikes_modelcomparison = "
model {
    #Prior
    m_prob[1] <- 0.5
    m_prob[2] <- 0.5
    mdl ~ dcat(m_prob[])

    alpha ~ dbeta(10,10)
    nu ~ dgamma(1,2)
    fmax ~ dunif(0,1)
    theta ~ dnorm(2,2)
    
    #Likelihood
        m[1] ~ dnorm( alpha, nu)
        p[1] <- fmax * (1/(1+ exp(-(m[1]+ theta))))
        s[1] ~ dbern(p[1])
    
        for(t in 2:T){
            m[t] ~ dnorm( ((mdl-1)*(alpha*m[t-1]) + (2-mdl)*alpha), nu)
            p[t] <- fmax * (1/(1+ exp(-(m[t]+ theta))))
            s[t] ~ dbern(p[t])
        }
}
"


niter=10000
nchains=4
nsamples = niter*nchains

data <- list('T' = T, 's' = s) # to be passed on to JAGS
parameters <- c('mdl', 'm') # fill in
jagsmodel <- jags.model(textConnection(model_spikes_modelcomparison), 
                        data = data, 
                        n.chains = nchains)
mcmcsamples = coda.samples(jagsmodel, parameters, n.iter = niter)
samples = as.matrix(mcmcsamples)

# Compute the Bayes factor 10 (i.e. temporal dependency / no dependency):

model = samples[,'mdl']

posterior_model1_mcmc = sum(model==1) / length(model)
posterior_model2_mcmc = 1 - posterior_model1_mcmc

bayesFactor = posterior_model2_mcmc/posterior_model1_mcmc
bayesFactor

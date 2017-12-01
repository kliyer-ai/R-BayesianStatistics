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



# Example usage of plot_sd()
n = 100
sigma = 1
x = rnorm(n, 0, sigma)
plot(x, type='l', xaxs='i')
plot_sd(x, rep(sigma, n))



load(file='timeseries.Rdata')
n = length(y)


# Time series exercise 1

plot(y, pch=19, xlab='Observation index', type='o')


model_notimeseries = "
model {
    #Prior
    tau ~ dgamma(0.1, 0.1)
    sigma <- 1.0/sqrt(tau)
    mean ~ dnorm(0,1)
    
    #Likelihood
    for(i in 1:n){
        y[i] ~ dnorm(mean, tau)
    }
}
"

model_timeseries = "
model {
    #Prior
    tau ~ dgamma(0.1, 0.1)
    sigma <-  1.0/sqrt(tau)
    mean ~ dnorm(0,1)
    
    #Likelihood
    y[1] ~ dnorm(mean, tau)

    for(i in 2:n){
        y[i] ~ dnorm(y[i-1], tau)
    }
}
"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) 
parameters <- c('mean', 'sigma') 

jagsmodel <- jags.model(textConnection(model_timeseries), # change to model_notimeseries if you want to plot the results for the simple model
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))
mean1 = samples[, 'mean']       
sigma1 = samples[, 'sigma']     

jagsmodel <- jags.model(textConnection(model_notimeseries), 
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))
mean0 = samples[, 'mean']      
sigma0 = samples[, 'sigma']    

# Compute posterior expectation of the vector y:
E0 = rep(0, n)
E0[1:n] = mean(mean0)
sigma0 = rep(mean(sigma0), n)

E1 = rep(0, n)
E1[1] = mean(mean1)
E1[2:100] = y[1:99]
sigma1 = rep(mean(sigma1), n)

#plot
par(mfrow=c(1,1))
plot(y, pch=19, xlab='Observation index', type='o')
plot_sd(E0, sigma0)
title('No Timeseries')

plot(y, pch=19, xlab='Observation index', type='o')
plot_sd(E1, sigma1)
title('Timeseries')



# Model comparison:
model_timeseries_or_notimeseres = "
model {
    # Prior 
        m_prob[1] <- 0.5
        m_prob[2] <- 0.5
        m ~ dcat(m_prob[])
        
        tau ~ dgamma(0.1, 0.1)
        sigma <-  1.0/sqrt(tau)
        mean ~ dnorm(0,1)
    

    #Likelihood
        y[1] ~ dnorm(mean, sigma)
    
        for(i in 2:n){
            y[i] ~ dnorm((m-1)*y[i-1] + (2-m)*mean, sigma)
        }
}
"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) # to be passed on to JAGS
parameters <- c('m') # fill in
jagsmodel <- jags.model(textConnection(model_timeseries_or_notimeseres), # change to model_notimeseries if you want to plot the results for the simple model
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))
m = samples[,'m']

# Compute the Bayes factor of the comparison:
posterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc

bayesFactor = posterior_model2_mcmc/posterior_model1_mcmc
bayesFactor

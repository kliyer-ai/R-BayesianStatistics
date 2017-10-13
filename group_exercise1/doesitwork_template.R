# doesitwork.R

#setwd("C:/Users/u341138/Dropbox/Documents/Teaching/2017-2018/Bayesian statistics/R/DBDA2Eprograms")

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise.
require(rjags)
require(coda)
source("DBDA2E-utilities.R")


plotGaussian <- function(data, mean, sd) {
  plot.new()
  par(mfrow=c(1,1))
  h = hist(data, breaks = 50)
  xrange = seq(min(data),max(data),length=50) 
  gaussian_distribution = dnorm(xrange, mean=mean, sd = sd)
  scaled_gaussian = gaussian_distribution * diff(h $ mids[1:2])*length(data) 
  lines(xrange, scaled_gaussian)
}

# This code provides an outline; additional code needs to be added by yourself!



#----------   Model 1: learning a mean   --------------

# Set the initial parameters

sigma = 2.0
N = 1000
mu = 7

# Generate observations here
x = rep(0, N)

# fill vector x here
x = rnorm(N, mu, sigma)

hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model1.string = "
  model {
    # Prior
    mu ~ dnorm(prior_mu, prior_tau)

    # Likelihood
    for(i in 1:N){
    x[i] ~ dnorm(mu, tau)
    }
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model1.spec = textConnection(model1.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model1.spec,
                         data = list('N' = N,                    # the number of data points
                                     'tau' = 1/sigma^2,            # the standard deviation of the likelihood
                                     'x' = x,                    # the observations
                                     'prior_mu' = 0,
                                     'prior_tau' = 1/(5^2)
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel1,
                       c('mu'), # which variables do you want to monitor?
                       n.iter=mcmciterations)

# plot/interpret the results
mcmcsummary = summary(samples)
mcmcsummary $ statistics

plotPost(samples[,'mu'], xlab = 'guessed mean')
diagMCMC(codaObject = samples, parName = 'mu')


#----------   Model 2: learning mean and variance   --------------

# Set the initial parameters
sigma = 2.0
N = 1000
mu = 7

# Generate observations here
x = rep(0, N)
x = rnorm(N, mu, sigma)


hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model1.string = "
  model {
    # Prior
    mu ~ dnorm(prior_mu, prior_tau)
    sigma ~ dgamma(a,b)

    # Likelihood
    for(i in 1:N){
    x[i] ~ dnorm(mu, 1/sigma^2)
    }
    
    
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model1.spec = textConnection(model1.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model1.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x,                     # the observations
                                     'prior_mu' = 0,
                                     'prior_tau' = 1/5^2,
                                     'a' = 1,
                                     'b' = 1
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel1,
                       c('mu', 'sigma'), # which parameters do you want to monitor?
                       n.iter=mcmciterations)



# plot/interpret the results
mcmcsummary = summary(samples)
mcmcsummary $ statistics

plotPost(samples[,'mu'], xlab = 'guessed mean')
diagMCMC(codaObject = samples, parName = 'mu')

plotPost(samples[,'sigma'], xlab = 'guessed sigma')
diagMCMC(codaObject = samples, parName = 'sigma')

plotGaussian(x , mcmcsummary$statistics[1], mcmcsummary$statistics[2])

#----------   Model 3: the wrong model   --------------


# Set the initial parameters
N = 1000
df = 2

# Generate observations here
x = rep(0, N)
x = rt(N,df)


hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model3.string = "
  model {
    # Prior
    mu ~ dnorm(prior_mu, prior_tau)
    sigma ~ dgamma(a,b)
    
    # Likelihood
    for(i in 1:N){
    x[i] ~ dnorm(mu, 1/sigma^2)
    }


}
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model3.spec = textConnection(model3.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(model3.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x,                    # the observations
                                     'prior_mu' = 5,
                                     'prior_tau' = 1/5^2,
                                     'a' = 1,
                                     'b' = 1
                         ),    
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel3,
                       c('mu', 'sigma'), # which variables do you want to monitor?
                       n.iter=mcmciterations)


# plot/interpret the results

mcmcsummary = summary(samples)
mcmcsummary $ statistics

plotPost(samples[,'mu'], xlab = 'guessed mean')
diagMCMC(codaObject = samples, parName = 'mu')

plotPost(samples[,'sigma'], xlab = 'guessed sigma')
diagMCMC(codaObject = samples, parName = 'sigma')

plotGaussian(x , mcmcsummary$statistics[1], mcmcsummary$statistics[2])

####################################################################################
N = 40
P = 15

Z = rep(0,P)

for(i in 1:P){
    Z[i] = rbinom(1,1, 0.5)
}
    




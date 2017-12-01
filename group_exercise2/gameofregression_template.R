# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)



rm(list=ls())
require(rjags)
require(coda)

load(file = file.choose())

n = dim(x)[1] # number of data points
p = dim(x)[2] # number of predictors


linear_regression ="
model {
  # Prior

    tau ~ dgamma(0.01, 0.01)
    w0 ~ dnorm(0, 1)

    for(j in 1:p){
        w[j] ~ dnorm(0, 1)
    }

  

  # Likelihood
    for(i in 1:n){
        mu[i] <- w0 + inprod(w, x[i, 1:p])
        y[i] ~ dnorm(mu[i], tau)
    }


}

"

niter = 10000
nchains = 4
data = list('n' = n,
            'p' = p,
            'x' = x,
            'y' = y)

jagsmodel <- jags.model(textConnection(linear_regression), 
                        data = data,
                        n.chains = nchains)

store_parameters = c('w0', 'w') # you chose which parameters to monitor
samples = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples)

means = rep(0,5)
par(mfrow=c(2,3))

hist(samplesMatrix[,6], breaks = 50)
bias = mean(samplesMatrix[,6])

for(i in 1:5){
    hist(samplesMatrix[,i], breaks = 50)
    means[i] = mean(samplesMatrix[,i])
}



newChar = c(1, 0, 5, 380, 20)
predAge = bias + newChar%*%means
predAge

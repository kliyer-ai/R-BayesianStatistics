# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)

# Data:
x = c(-15.79, 0.98, 3.71, -5.37, -10.23, -8.32, -7.80, 6.77, -8.81, -9.56, -2.06, -0.76, -6.30, 39.40, -10.79, -8.16, -2.82, -16.19, -11.00, -14.60, -17.96, 0.76, -10.77)
y = c(3.19, -3.45, 0.04, 6.62, 3.61, 2.67, -2.45, 9.31, 15.29, 3.68, 8.63, 10.82, -0.50, -11.00, 2.05, -11.80, -2.02, 4.42, -0.86, -0.92, 2.16, 1.58, -1.16)
n = length(y)

crime_model = "
model {      
  # Prior 
    tau ~ dgamma(0.01, 0.01)
        
    for(j in 1:2){
        w[j] ~ dnorm(0, 1)
    }


  # Likelihood
    
    for(i in 1:n){
        mu[i] <- w[1] + w[2]*x[i]
        y[i] ~dnorm(mu[i], tau)
    }
}
"


niter = 10000
nchains = 4

# Specify your data structure here
data = list('n' = n,
            'x' = x,
            'y' = y)

jagsmodel_crime <- jags.model(textConnection(crime_model), 
                              data = data,
                              n.chains = nchains)

# Specify which parameters you want the sampler to store
store_parameters = c('w')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_crime = coda.samples(jagsmodel_crime, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_crime)

w0 = samplesMatrix[,'w[1]']
w1 = samplesMatrix[,'w[2]']

plot(x,y, xlab = "Percentage change in manpower", ylab = "Percentage change in thefts", pch=20)

par(mfrow = c(1,2))
hist(w0, breaks = 50)
hist(w1, breaks = 50)

mean_w0 = mean(w0)
mean_w1 = mean(w1)

par(mfrow = c(1,1))
plot(x,y, xlab = "Percentage change in manpower", ylab = "Percentage change in thefts", pch=20)
abline(mean_w0, mean_w1)

lines = 500
wi = sample(nchains*niter, lines)

plot(x,y, xlab = "Percentage change in manpower", ylab = "Percentage change in thefts", pch=20)
for(i in 1:lines){
    abline(w0[wi[i]], w1[wi[i]],col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.1))
}


# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())
setwd("~/Radboud University/Bayesian Statistics/DBDA2Eprograms/DBDA2Eprograms")

# Required packages for this exercise.
require(rjags)
require(coda)


# Load data
load(file = file.choose())
n = length(y)

## -------- Linear regression --------


linreg_model1 ="
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

# Create your data structure here
data = list('n' = n,
            'x' = x,
            'y' = y)

jagsmodel_linreg1 <- jags.model(textConnection(linreg_model1), 
                              data = data,
                              n.chains = nchains)

# Specify which parameters you want the sampler to store
store_parameters = c('w')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_linreg1 = coda.samples(jagsmodel_linreg1, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_linreg1)

w0 = samplesMatrix[,'w[1]']
w1 = samplesMatrix[,'w[2]']

par(mfrow = c(1,2))
hist(w0, breaks = 50)
hist(w1, breaks = 50)

mean_w0 = mean(w0)
mean_w1 = mean(w1)

par(mfrow = c(1,1))
plot(x,y, pch=20)
abline(mean_w0, mean_w1)

lines = 500
wi = sample(niter*nchains, lines)

plot(x,y, pch=20)
for(i in 1:lines){
    abline(w0[wi[i]], w1[wi[i]], col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.1))
}

## -------- Linear regression mixture --------

linreg_model2 ="
model {      
    # Prior 
    l_prob[1] <- 0.5
    l_prob[2] <- 0.5
    
    for(i in 1:n){
        z[i] ~ dcat(l_prob[])
    }
    
    tau ~ dgamma(0.01, 0.01)
    
    for(l in 1:2){
        for(j in 1:2){
            w[l, j] ~ dnorm(0, 1)
        }
    }
    
    # Likelihood
    for(i in 1:n){
        mu[i] <- w[z[i], 1] + w[z[i] ,2]*x[i]
        y[i] ~ dnorm(mu[i], tau)
    }
}
"

niter = 10000
nchains = 4

# Create your data structure here
data = list('n' = n,
            'x' = x,
            'y' = y)

jagsmodel_linreg2 <- jags.model(textConnection(linreg_model2), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('w')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_twolines = coda.samples(jagsmodel_linreg2, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_twolines)
w = samplesMatrix

par(mfrow = c(2,2))
hist(w[,1], breaks = 500)
hist(w[,3], breaks = 500)
hist(w[,2], breaks = 500)
hist(w[,4], breaks = 500)

par(mfrow = c(1,1))
plot(x,y,pch=20)

sample = sample(niter*nchains, 500)
for(i in 1:500){
    abline(w[sample[i], 1], w[sample[i], 3], col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.01))
    abline(w[sample[i], 2], w[sample[i], 4], col=rgb(0.2, 0.8, 0.2, max = 1.0, alpha = 0.01))
}

par(mfrow = c(1,1))
plot(x,y,pch=20)
abline(mean(w[,1]), mean(w[,3]))
abline(mean(w[,2]), mean(w[,4]))

## -------- Model selection --------

linreg_model3 ="
model {      
# Prior 
m_prob[1] <- 0.5
m_prob[2] <- 0.5

m ~ dcat( m_prob[] )   


l_prob[1] <- 0.5
l_prob[2] <- 0.5

for(i in 1:n){
z[i] ~ dcat(l_prob[])
}

tau ~ dgamma(0.01, 0.01)

for(l in 1:2){
for(j in 1:2){
w_two[l, j] ~ dnorm(0, 1)
}
}

for(j in 1:2){
w_one[j] ~ dnorm(0, 1)
}

# Likelihood
for(i in 1:n){
mu[i] <- (m-1)*w_two[z[i],1] + (2-m)*w_one[1]           #chose w0
+ (m-1) * w_two[z[i],2] * x[i] + (2-m)*w_one[2]*x[i]    #chose w1
y[i] ~dnorm(mu[i], tau)
}
}
"

niter = 10000
nchains = 4
# Create your data structure here
data = list('n' = n,
            'x' = x,
            'y' = y)

jagsmodel_linreg3 <- jags.model(textConnection(linreg_model3), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('m', 'w_two', 'w_one')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneortwolines = coda.samples(jagsmodel_linreg3, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneortwolines)
m = samplesMatrix[,'m']
w_one = samplesMatrix[,2:3]
w_two = samplesMatrix[,4:7]

par(mfrow = c(1,1))
plot(x,y,pch=20)
samples_m1 = which(m==1)
samples_m2 = which(m==2)

samples_one = sample(samples_m1, 250)
samples_two = sample(samples_m2, 250)
    
for(i in 1:250){
    #one line
    abline(w_one[samples_one[i], 1], w_one[samples_one[i], 2],col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.1))
    #two lines
    abline(w_two[samples_two[i], 1], w_two[samples_two[i], 3],col=rgb(0.2, 0.8, 0.2, max = 1.0, alpha = 0.1))
    abline(w_two[samples_two[i], 2], w_two[samples_two[i], 4],col=rgb(0.2, 0.2, 0.8, max = 1.0, alpha = 0.1))
}


posterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc

bayesFactor = posterior_model2_mcmc/posterior_model1_mcmc
bayesFactor





# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)
source("DBDA2E-utilities.R")

#------------------------   Model 1: exam scores   ----------------------------


# THE DATA
n = 40
k = c(19, 20, 16, 23, 22, 30, 38, 29, 34, 35, 35, 32, 37, 36, 33)
p = length(k)


# THE MODEL
exammodel1.string = "
  model {
    ## Prior
        for(i in 1:p){
            z[i] ~ dbern(0.5)
        }
    
        psi <- 0.5
        phi ~ dbeta(4,1)
        
        

    ## Likelihood
    for(i in 1:p){
        k[i] ~ dbin(equals(z[i]*psi + equals(z[i],1)*phi, n)
    }
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel1.spec = textConnection(exammodel1.string)

# SAMPLING PARAMETERS
niter = 10000
nchains = 4

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(exammodel1.spec,
                   data = list('k' = k,
                               'n' = n,
                               'p' = p),
                   n.chains = nchains)

# Collect samples to approximate the posterior distribution.
model1samples = coda.samples(jagsmodel1,
                           c('z', 'phi'), # which variables do you want to model
                           n.iter = niter)


# Add your analyses based on the collected samples here:
mcmcsummary = summary(model1samples)
model1samples

#----------   Model 2: exam scores with individual differences   --------------

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)

# THE DATA
n = 40
k = c(19, 20, 16, 23, 22, 30, 38, 29, 34, 35, 35, 32, 37, 36, 33)
p = length(k)


# THE MODEL
exammodel2.string = "
  model {
    ## Prior

    

    ## Likelihood    

    
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel2.spec = textConnection(exammodel2.string)

# SAMPLING PARAMETERS
mcmciterations = 1000

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel2 <- jags.model(exammodel2.spec,
                         data = list('k' = k,
                                     'n' = n,
                                     'p' = p),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model2samples = coda.samples(jagsmodel2,
                           c(''), # which variables do you want to monitor?
                           n.iter = mcmciterations)


# Add your analyses on the collected samples here:



#----------   Model 3: easy and difficult questions   --------------

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

n = 10
m = 20

k1 = matrix(0L, nrow = n, ncol = m)

k1[1,] = c( 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
k1[2,] = c( 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[3,] = c( 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
k1[4,] = c( 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[5,] = c( 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
k1[6,] = c( 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0)
k1[7,] = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
k1[8,] = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[9,] = c( 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1)
k1[10,] = c( 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)


# THE MODEL
exammodel3.string = "
  model {
    ## Prior


    ## Likelihood  
  }
"


# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel3.spec = textConnection(exammodel3.string)

# SAMPLING PARAMETERS
mcmciterations = 1000

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(exammodel3.spec,
                         data = list('k' = k1, 
                                     'n' = n,
                                     'm' = m),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model3samples = coda.samples(jagsmodel3,
                             c(''), # which variables do you want to monitor
                             n.iter = mcmciterations)

# Add your analyses on the collected samples here:


#----------   Model 4: differences between groups   --------------

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

n1 = 50
n2 = 49
k1 = 37
k2 = 48



# THE MODEL
exammodel3.string = "
model {
  ## Prior 

  ## Likelihood
}
"


# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel3.spec = textConnection(exammodel3.string)

# SAMPLING PARAMETERS
mcmciterations = 1000

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(exammodel3.spec,
                         data = list('k1' = k1,
                                     'k2' = k2,
                                     'n1' = n1,
                                     'n2' = n2),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model3samples = coda.samples(jagsmodel3,
                             c(''), # which variables do you want to monitor
                             n.iter = mcmciterations)

# Add your analyses on the collected samples here:



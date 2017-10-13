# characters dying in Game of Thrones

nPeople = 100
families = c('Stork', 'Sinnister', 'Tardigrade', 'Tharabeon')
nFamilies = length(families) 

familyDist = rep(1/nFamilies, nFamilies) # uniform prior

pseudoLifeDeath = matrix(0, nFamilies, 2) # prior beliefs for question 1.3
pseudoLifeDeath[1,] = c(10,1) # Storks
pseudoLifeDeath[2,] = c(3,4) # Sinnisters
pseudoLifeDeath[3,] = c(1,5) # Tardigrades
pseudoLifeDeath[4,] = c(1,7) # Tharabeons

survivalProbs = rep(0L, nFamilies)

for (j in 1:nFamilies) {
  survivalProbs[j] = rbeta(1, pseudoLifeDeath[j,1], pseudoLifeDeath[j,2])
}

z = rep(0, nPeople)
x = rep(0, nPeople)

for (i in 1:nPeople) {
  z[i] = sample(1:nFamilies, size=1, replace=TRUE, familyDist)
  x[i] = runif(1, min=0, max=1) < survivalProbs[z[i]]
}


for (j in 1:nFamilies) {
  family = z==j
  print(sprintf('%s:  %0.2f', families[j], sum(x[family]) / sum(family)))
}

# ------------- infer this model --------------#

graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)
source("DBDA2E-utilities.R")



model.string = "
model {

  # prior

  for (k in 1:K) {
    survival_prob[k] ~ dbeta(a[k], b[k])
  }

  # likelihood

  for (i in 1:N) {
    x[i] ~ dbern(survival_prob[z[i]])
  }

}

"
model.spec = textConnection(model.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel <- jags.model(model.spec,
                         data = list('z' = z,
                                     'a' = c(1, 1, 1, 1),
                                     'b' = c(1, 1, 1, 1),
                                     'N' = nPeople,                    # the number of data points
                                     'K' = nFamilies,                    # number of families
                                     'x' = x                     # the observations
                         ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel,
                       c('survival_prob'),
                       n.iter=mcmciterations)


mcmcsummary = summary(samples)
mcmcsummary $ statistics

plotPost(samples[,'survival_prob[1]'], xlab = 'survival probability')
diagMCMC(codaObject = samples, parName = 'survival_prob[1]')

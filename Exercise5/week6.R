require(rjags)
require(coda)
source("DBDA2E-utilities.R")


# Bernoulli(z,N | theta)
bernoulli_pdf <- function(theta, z, N) {
    # Computes p(z,N|theta) = Bernoulli(theta|z,N)
    pdf = theta^z * (1 - theta)^(N - z)
    return(pdf)
}

# beta(theta | a, b)
beta_pdf <- function(theta, a, b) {
    # Computes p(theta|a,b) = beta(theta|a,b)
    pdf = theta^(a - 1) * (1 - theta)^(b - 1)
    return(pdf)
}

# B(a+z, N-z+b) / B(a,b)
beta_bernoulli_evidence <- function(a, b, N, z) {
    return( beta( z+a, N-z+b ) / beta( a, b ) ) # not numerically stable
}


# Define a range of theta's to compute the probability density for:
theta = seq(0, 1, by = 0.01)
N = 12
z = 8

omega1 = 0.25
omega2 = 0.75
kappa = 8

a1 = omega1*(kappa-2)+1
b1 = (1-omega1)*(kappa-2)+1
a2 = omega2*(kappa-2)+1
b2 = (1-omega2)*(kappa-2)+1

prior1 = beta_pdf(theta, a=a1, b=b1)
prior2 = beta_pdf(theta, a2, b2)

likelihood = bernoulli_pdf(theta, z, N)

posterior1 = beta_pdf(theta, a = z + a1, b = N - z + b1) 
posterior2 = beta_pdf(theta, a = z + a2, b = N - z + b2)


par(mfrow= c(1,3))
plot(theta, posterior1, type = 'l')
title('Posterior for the first factory')

plot(theta, posterior2, type = 'l')
title('Posterior for the second factory')

plot(theta, likelihood, type = 'l')
title('Likelihood of theta')

theta[posterior1 == max(posterior1)]
theta[posterior2 == max(posterior2)]

#model comparison
evidence1 = beta_bernoulli_evidence(a1, b1, N, z)
show(sprintf('Evidence p(D|m_1) = %0.5f', evidence1))
evidence2 = beta_bernoulli_evidence(a2, b2, N, z)
show(sprintf('Evidence p(D|m_2) = %0.5f', evidence2))

bayes_factor = evidence1 / evidence2
show(sprintf('Bayes factor = %0.5f', bayes_factor))
show(sprintf('>1 : in favor of m1, <1 : in favor of m2'))

#no model preference
prior_model_1 = 0.5
prior_model_2 = 0.5

prior_odds = prior_model_1 / prior_model_2 




posterior_odds = bayes_factor * prior_odds 

posterior_model1 = posterior_odds / (1.0 + posterior_odds)
posterior_model2 = 1 - posterior_model1

show(sprintf('Posterior model 1 p(m_1 | D) = %0.2f', posterior_model1))
show(sprintf('Posterior model 2 p(m_2 | D) = %0.2f', posterior_model2))



######################Exercise 2
spiky_pdf <- function(theta, spike){
    i = length(theta)*spike
    pdf = rep(0, length(theta))
    pdf[i] = 1
    return(pdf)
}

a = 2
b = 2

theta = seq(0, 1, by = 0.01)

prior1 = spiky_pdf(theta, 0.5)
prior2 = beta_pdf(theta, a, b)

par(mfrow=c(1,2))
plot(theta, prior1, type = 'l')
plot(theta, prior2, type = 'l')


# THE MODEL
model_comparison_model.string = "
model {
## Prior
a <- 2
b <- 2

m_prob[1] <- 0.5
m_prob[2] <- 0.5
m ~ dcat( m_prob[] )

theta_m1 <- 0.5
theta_m2 ~ dbeta(a, b)

theta <- equals(m,1) * theta_m1 + equals(m,2) * theta_m2
## Likelihood
for (i in 1:N) {    
x[i] ~ dbern(theta)
}
}
"


model_comparison_model.spec = textConnection(model_comparison_model.string)

# SAMPLING PARAMETERS
niter = 10000
nchains = 4

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model_comparison_model.spec,
                         data = list('N' = 30,
                                     'x' =  c(rep(1, 20), rep(0, 10))
                         ),
                         n.chains = nchains)

# Collect samples to approximate the posterior distribution.
model1samples = coda.samples(jagsmodel1,
                             c('m', 'theta'),
                             n.iter = niter)

mcmcMat = as.matrix( model1samples , chains=TRUE )
m = mcmcMat[,"m"]
theta = mcmcMat[,"theta"]

posterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc

theta_given_m1 = theta[m==1]
theta_given_m2 = theta[m==2]

par(mfrow=c(1,2))
#hist(x = m)
hist(x = theta_given_m1, breaks = seq(0, 1, 1/30))
hist(x = theta_given_m2, breaks = seq(0, 1, 1/30))



poisson = function(k, lambda){
  return((exp(-lambda)*lambda^k)/factorial(k))
}

gamma = function(lambda, alpha, beta){
  return(
    ((beta^alpha)/factorial(alpha-1))*lambda^(alpha-1)*exp(-beta*lambda)
    )
}

#1.1
lambdas = 0:50      #number of expected occurences
k = 30              #number of occurences
y = poisson(k, lambdas)
plot(lambdas,y)

#1.2
a = 5          #number of occurences   
b = 0.1         #number of intervals
y = gamma(lambdas, a, b)
plot(lambdas, y)


#1.3
a = 7.5
b = 1
k = 30

likelihood = poisson(k, lambdas)
prior = gamma(lambdas, a, b)
evidence = sum(likelihood*prior)
posterior = likelihood*prior/evidence
plot(lambdas, posterior)

#1.4
posterior_conjugate = gamma(lambdas, a + k, b + 1)
plot(lambdas, posterior_conjugate)


#2.1
prior = function(theta){
  return(2/3*(cos(4*pi*theta)+1)^2)
}

thetas = seq(0,1,0.01)
y = prior(thetas)
plot(thetas, y)

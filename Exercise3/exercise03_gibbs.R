
## Set parameters of the bivarate Gaussian distribution.
# Means
mu = c(0,0)
# Covariance \rho
rho = 0.9

# Number of sweeps (in one sweep, all parameters are updated once). Watch how your results become more accurate when you increase this value!
niter = 5000

# Create a matrix to store the results in. The matrix has size niter x 2, because we store a value for x_1 and for x_2 at each iteration.
samples = matrix(0L, nrow=niter, ncol=2)

# Set the initial conditions. The function 'runif' gives a random number from the uniform distribution between 'min' and 'max'.
samples[1,] = runif(2, min=-3, max=3)

# Loop over n-1 iterations.
for (iter in 2:niter) {
  # This is where you come in!
  
    # Find the *most recent* value for the *other* x (so x_{not i}).
    x1 = samples[iter-1,1]
    x2 = samples[iter-1,2]

    # Draw a new value for x_i, given x_{not i}, using Equation 4 in the exercise.
    x1 = rnorm(1, rho*x2, sqrt(1-rho^2))
    x2 = rnorm(1, rho*x1, sqrt(1-rho^2))
    
    # Store the new value in the samples matrix.
    samples[iter,] = c(x1,x2)
}


# The code below provides a scatter plot for your results. Include this in your solution.
x1 = samples[,1]
x2 = samples[,2]
plot(x1, x2, asp=1, xlab=expression('x'[1]), ylab=expression('x'[2]))
title('A bivariate Gaussian distribution approximated using Gibbs MCMC')
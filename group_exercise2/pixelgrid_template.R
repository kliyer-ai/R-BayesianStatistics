# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Helper functions
number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector = binary_vector[-(1:(length(binary_vector) - noBits))]
  binary_vector[binary_vector==0] = -1
  return(binary_vector)
}

plot_evidences = function(evidences, sortBy) {
  sorted = order(evidences[,sortBy], decreasing = T)
  matplot(evidences[sorted,], type = 'b', lty = rep(1,nmodels), pch=20, col = 1:4, xlab = 'Dataset number', ylab = 'Evidence') 
  legend("topright", legend = c('Model 0', 'Model 1', 'Model 2', 'Model 3'), col=1:4, pch=20) 
  return(sorted)
}

# Create all the possible data sets
npixels = 9
ndatasets = 2^npixels
nmodels = 4

datasets = matrix(0, nrow = ndatasets, ncol = npixels)

for (d in 1:ndatasets) {
  datasets[d,] = number2binary(d, npixels)
}


x = matrix(0, npixels, 2)
x[1,] = c(-1, 1)
x[2,] = c(0, 1)
x[3,] = c(1, 1)
x[4,] = c(-1, 0)
x[5,] = c(0, 0)
x[6,] = c(1, 0)
x[7,] = c(-1, -1)
x[8,] = c(0, -1)
x[9,] = c(1, -1)


# Compute the evidences for each data set
evidences = matrix(0, nrow = ndatasets, ncol = nmodels)
evidences[,1] = 1/512 # fill in

# Parameters for the prior weights
mu = 0
sigma = 10

# The number of samples to get
nsamples = 10000


for (d in 1:ndatasets) {
  y = datasets[d,]
  
  for (i in 2:nmodels) {
    evidence = 0
    w = rep(0, 3)
    
    for (s in 1:nsamples) {
      # get weights from prior
      for(j in 1:(i-1)){
          w[j] = rnorm(1, mu, sigma)
      }
        pDwm = 1
        
        for(j in 1:9){
            exponent = -y[j]*(w[3]+w[1]*x[j, 1] + w[2]*x[j,2])
            pDwm = pDwm * (1/(1+exp(exponent)))
        }
      
      evidence = evidence + pDwm 
    }
    evidences[d,i] = evidence/nsamples 
  }
}

sorted_order = plot_evidences(evidences, sortBy=4)

#the table reveals that these are datasets 511 and 512

evidences[511,1] / evidences[511,4]
evidences[512,1] / evidences[512,4]

bayesFactorMatrix = matrix(0, 4, 4)

for(i in 1:4){
    for(j in 1:4){
        for(d in 1:512){
            if(evidences[d,i] > evidences[d, j]){
                bayesFactorMatrix[i, j] = bayesFactorMatrix[i, j]+1
            }
        }
    }
}
bayesFactorMatrix

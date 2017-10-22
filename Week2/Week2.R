source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
a = 1    # Convert to beta shape parameter a.
b = 1  # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

#Data
N = 100                         
z = 61                         
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="firstPoll",type="png")


#UPDATE BELIEVES    
newPrior = posterior #c(a,b)       # Specify Prior as vector with the two shape parameters.

#Data
N = 100                        
z = 67                         
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=newPrior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="secondPoll",type="png")



#UPDATE BELIEVES
newPrior = posterior

#Data
N = 100                         
z = 7                        
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=newPrior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="thirdPoll",type="png")





#EXERCISE 2

#FIRST EXPERIMENT
a = 1
b = 1

prior = c(a,b)

#Data
N = 50                         # The total number of participants
z = 35                         # The number of people who chose F
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="firstExperiment",type="png")



###Second Experiment
a = 1
b = 1

prior = c(a,b)

#Data
N = 50                         # The total number of participants
z = 13                         # The number of people who chose F
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="secondExperiment",type="png")


# SECOND EXPERIMENT WITH PRIOR
a = 1
b = 1

prior = c(a,b)

#Data
N = 25                         # The total number of participants
z = 20                         # The number of people who chose F
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )

#UPDATE BELIVES
newPrior = posterior

#Data
N = 50
z = 13

Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=newPrior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="secondExperimentWithPrior",type="png")


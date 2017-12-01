N = 250 
z = 140
p = 0.5

cbin = function(z, N, theta){
    sum = 0
    for(i in 0:z){
        sum = sum + dbinom(i, N, theta)
    }
    return(sum)
}

p_value = cbin(110, N, p) + (1 - cbin(139, N, p))

####################1.3
a = 1
b = 1

evidence_m0 = dbinom(z, N, 0.5)
evidence_m1 = beta(z+a, N-z+b)/beta(a,b)*choose(N, z)

BF10 = evidence_m1/evidence_m0
BF10

##################1.4
n = 400
BFs = rep(0, n)


for(a in 1:n){
    evidence_m0 = dbinom(z, N, 0.5)
    evidence_m1 = beta(z+a, N-z+a)/beta(a,a)*choose(N, z)
    BFs[a] = evidence_m1/evidence_m0
}
plot(1:n, BFs, 'l', xlab = 'alphas')
abline(1, 0, col = 'red')

max(BFs)

########1.7
theta_MLE = z/N
evidence_m2 = dbinom(z, N, theta_MLE)
evidence_m2/evidence_m0

#########1.9
p_value_new = cbin(109, N, p) + (1 - cbin(140, N, p))
p_value_new

##########1.10
z_new = 141

n = 400
BFs = rep(0, n)


for(a in 1:n){
    evidence_m0 = dbinom(z_new, N, 0.5)
    evidence_m1 = beta(z_new+a, N-z_new+a)/beta(a,a)*choose(N, z_new)
    BFs[a] = evidence_m1/evidence_m0
}
plot(1:n, BFs, 'l', xlab = 'alphas')
abline(1, 0, col = 'red')
max(BFs)

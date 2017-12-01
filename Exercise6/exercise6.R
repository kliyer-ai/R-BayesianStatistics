binomial = function(z, N, theta) {
    prob = choose(N, z) * theta^z * (1-theta)^(N-z)
    return(prob)
}

cumulative_binomial = function(z, N, theta){
    sum = 0
    for(i in 0:z){
        sum = sum + binomial(i, N, theta)
    }
    return(sum)
}

N = 45
z = 3
theta = 1/6

cumulative_binomial(z, N, theta)
cumulative_binomial(2, N, theta)

1 - cumulative_binomial(13, N, theta)

sum = 0
for(z in 14:N){
    sum = sum + binomial(z, N, theta)
}
sum

###########1.2
neg_binomial = function(z, N, theta){
    prob = (z/N) * choose(N, z) * theta^z * (1-theta)^(N-z)
    return(prob)
}


cum_neg_binomial = function(z, N, theta) {
    sum = 0
    if(z > N){
        return(0)
    }
    
    for(i in z:N) {
        sum = sum + neg_binomial(z, i, theta)
    }
    return(sum)
}

cum_neg_binomial(z, 4, theta)
1 - cum_neg_binomial(z, 41, theta)


############2nd Exercise

z = 5
N = 6
theta = 1/2

1 - cumulative_binomial(4,6, theta)
binomial(5,6, theta)+binomial(6,6, theta)

################2.2
cum_neg_binomial(5, 6, theta)
neg_binomial(5,6, theta)+neg_binomial(5,5, theta)
#######################2.3
z = 5
N = 6
a = 1
b = 1

theta = seq(0, 1, 0.01)
posterior = dbeta(theta, a + z, N - z + b)
prior = dbeta(theta, a, b)

plot(theta, prior, 'l')
title('Prior Distribution')

plot(theta, posterior, 'l')
title('Posterior Distribution')

1 - pbeta(0.5, a + z, N - z + b)


sum(posterior[51:100])/100



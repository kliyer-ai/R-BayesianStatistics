a_K = c(20,0,34,22)
b_K = c(2,100,15,22)
K = 4
alpha = rep(1, K)*1/4
N = 100


Z_i = sample(x = K, size = N, replace = T, prob = alpha)

theta_k = 0
for(k in seq(K)){
    a_k = a_K[k]
    b_k = b_K[k]
    theta_k[k] = rbeta(1, a_k, b_k)
}


x_i = 0
for(i in seq(N)){
    x_i[i] = rbinom(1, 1, theta_k[Z_i[i]])
}

#first family --> should all be 1;check hyper parameters
x_i[Z_i == 1]

#second family --> should all be 0; check hyper parameters
x_i[Z_i == 2]

#third
x_i[Z_i == 3]

#fourth
x_i[Z_i == 4]
a_K = c(1,1,1,1)
b_K = c(10,10,10,10)
K = 4
alpha = rep(1, K)*1/K
N = 100


Z_i = sample(x = K, size = N, replace = T, prob = alpha)

theta_k = array()
for(k in seq(K)){
    a_k = a_K[k]
    b_k = b_K[k]
    theta_k[k] = rbeta(1, a_k, b_k)
}


x_i = array()
for(i in seq(N)){
    x_i[i] = rbinom(1, 1, theta_k[Z_i[i]])
}

x_1sum = c(0,0)
x_2sum = c(0,0)
x_3sum = c(0,0)
x_4sum = c(0,0)


#first family --> should all be 1;check hyper parameters
x_i[Z_i == 1]
x_1sum[1] = sum(x_i[Z_i == 1]==0)
x_1sum[2] = sum(x_i[Z_i == 1]==1)
#second family --> should all be 0; check hyper parameters
x_i[Z_i == 2]
x_2sum[1] = sum(x_i[Z_i == 2]==0)
x_2sum[2] = sum(x_i[Z_i == 2]==1)
#third
x_i[Z_i == 3]
x_3sum[1] = sum(x_i[Z_i == 3]==0)
x_3sum[2] = sum(x_i[Z_i == 3]==1)
#fourth
x_i[Z_i == 4]
x_4sum[1] = sum(x_i[Z_i == 4]==0)
x_4sum[2] = sum(x_i[Z_i == 4]==1)

#Plotting the results. 
barplot(x_1sum, main = paste(c(sum(x_1sum), ' total members'), collapse = " "), ylab = 'Members' ,xlab = "Storks", names.arg = c("Death","Survival"))
barplot(x_2sum, main = paste(c(sum(x_2sum), ' total members'), collapse = " "), ylab = 'Members' ,xlab = "Sinnisters", names.arg = c("Death","Survival"))
barplot(x_3sum, main = paste(c(sum(x_3sum), ' total members'), collapse = " "), ylab = 'Members' ,xlab = "Tardigrade", names.arg = c("Death","Survival"))
barplot(x_4sum, main = paste(c(sum(x_4sum), ' total members'), collapse = " "), ylab = 'Members' ,xlab = "Tharabeon", names.arg = c("Death","Survival"))
